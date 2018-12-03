(** Block *)

type ('init, 'state, 'action, 'event, 'model, 'value) t = {
    initialize: 'init -> ('state, 'action, 'event) ActionResult.t;
    handle: 'state -> 'action -> ('state, 'action, 'event) ActionResult.t;
    viewModel: 'state -> ('action -> unit) -> 'model;
    getValue: 'state -> 'value
}

let map_init (f: ('init1 -> 'init2)) (block: ('init2,_,_,_,_,_) t) = 
  let initialize (init: 'init1) = block.initialize (f init) in
  (* let initialize2 = f @@ block.initialize; *)
  { initialize = initialize; handle = block.handle; viewModel = block.viewModel; getValue = block.getValue }

let map_value (f: ('value1 -> 'value2)) (block: (_,'state,_,_,_,'value1) t) =
  let getValue (s: 'state) = block.getValue s |> f in
  { initialize = block.initialize; handle = block.handle; viewModel = block.viewModel; getValue =  getValue }

let map_model (f: ('model1 -> 'model2)) (block: (_,'state,_,_,'model1,_) t) =
  let viewModel (s: 'state) dispatch = block.viewModel s dispatch |> f in
  { initialize = block.initialize; handle = block.handle; viewModel = viewModel; getValue =  block.getValue }

let choose_event (f: 'Event1 -> 'Event2 option) 
                 (block: ('init, 'state, 'action, 'event1, 'model, 'value) t)
    : ('init, 'state, 'action, 'event2, 'model, 'value) t =
    { initialize = (fun x -> ActionResult.choose_event f (block.initialize x));
      handle = (fun state (action: 'Action) -> block.handle state action |> ActionResult.choose_event f);
      viewModel = block.viewModel;
      getValue = block.getValue }

let combine (b1: ('init1,'state1,'action1,'event1,'model1,'value1) t)
            (b2: ('init2,'state2,'action2,'event2,'model2,'value2) t) =
  let initialize (a,b) =
    let res1 = b1.initialize a in
    let res2 = b2.initialize b in
    ActionResult.combine res1 res2 in

  let handle ((state1: 'state1), (state2: 'state2)) (action: ('action1, 'action2) Either.t) =
    match action with
    | Either.Left a -> 
      let res = b1.handle (state1) a in
      { ActionResult.newState = (res.newState, state2);
        events = (res.events |> List.map (fun x -> Either.Left(x))) }
    | Either.Right a -> 
      let res = b2.handle (state2) a in
      { ActionResult.newState = (state1, res.newState);
        events = (res.events |> List.map (fun x -> Either.Right(x))) } in

  let viewModel ((state1: 'state1), (state2: 'state2)) (dispatch: ('action1, 'action2) Either.t -> unit) =
    let vm1 = b1.viewModel state1 (fun a -> dispatch (Left a)) in
    let vm2 = b2.viewModel state2 (fun a -> dispatch (Right a)) in
    (vm1,vm2) in

  let getValue ((state1: 'state1), (state2: 'state2)) =
    let v1 = b1.getValue state1 in
    let v2 = b2.getValue state2 in
    (v1,v2) in

  { initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue }

let either (b1: ('init1,'state1,'action1,'event1,'model1,'value1) t) 
           (b2: ('init2,'state2,'action2,'event2,'model2,'value2) t) =
  let initialize (init: ('init1, 'init2) Either.t) =
    match init with
    | Either.Left init -> 
      b1.initialize init
      |> ActionResult.map_action (fun x -> Either.Left x)
      |> ActionResult.map_state (fun x -> Either.Left x)
      |> ActionResult.map_event (fun x -> Either.Left x)
    | Either.Right init ->
      b2.initialize init
      |> ActionResult.map_action (fun x -> Either.Right x)
      |> ActionResult.map_state (fun x -> Either.Right x)
      |> ActionResult.map_event (fun x -> Either.Right x) in

  let handle (state: ('state1, 'state2) Either.t) (action: ('action1, 'action2) Either.t) =
    match action with
    | Either.Left a ->
      match state with
      | Either.Left s ->
        b1.handle s a
        |> ActionResult.map_action (fun x -> Either.Left x)
        |> ActionResult.map_state (fun x -> Either.Left x)
        |> ActionResult.map_event (fun x -> Either.Left x)
      | Either.Right _s ->
        { newState = state; events = [] }
    | Either.Right a -> 
      match state with
      | Either.Right s -> 
        b1.handle s a
        |> ActionResult.map_action (fun x -> Either.Right x)
        |> ActionResult.map_state (fun x -> Either.Right x)
        |> ActionResult.map_event (fun x -> Either.Right x)
      | Either.Left _s -> 
        { newState = state; events = [] } in

  let viewModel (state: ('state1, 'state2) Either.t) (dispatch: ('action1, 'action2) Either.t -> unit) =
    match state with
    | Left s -> 
      b1.viewModel s (fun a -> dispatch(Left(a)))
    | Right s -> 
      b2.viewModel s (fun a -> dispatch(Right(a))) in

  let getValue (state: ('state1, 'state2) Either.t) =
    match state with
    | Either.Left s -> 
      b1.getValue s |> (fun x -> Either.Left x)
    | Either.Right s -> 
      b2.getValue s |> (fun x -> Either.Right(x)) in

  { initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue }

let static: ('a, 'a, unit, unit, 'a, 'a) t = 
  { initialize = (fun x -> ActionResult.mk_simple x)
  ; handle = (fun s _ -> ActionResult.mk_simple s)
  ; viewModel = (fun s _ -> s)
  ; getValue = (fun x -> x) }

let (<&&>) b1 b2 = combine b1 b2
let (<||>) b1 b2 = either b1 b2
