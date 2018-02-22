open Util
open Result2

type ('init, 'state, 'action, 'event, 'model, 'value) t = {
    initialize: 'init -> ('state, 'action, 'event) Result2.t;
    handle: 'state -> 'action -> ('state, 'action, 'event) Result2.t;
    viewModel: 'state -> ('action -> unit) -> 'model;
    getValue: 'state -> 'value
}

let mapInit (f: ('init1 -> 'init2)) (block: ('init2,_,_,_,_,_) t) = 
  let initialize (init: 'init1) = block.initialize (f init) in
  (* let initialize2 = f @@ block.initialize; *)
  { initialize = initialize; handle = block.handle; viewModel = block.viewModel; getValue = block.getValue }

let mapValue (f: ('value1 -> 'value2)) (block: (_,'state,_,_,_,'value1) t) =
  let getValue (s: 'state) = block.getValue s |> f in
  { initialize = block.initialize; handle = block.handle; viewModel = block.viewModel; getValue =  getValue }

let mapModel (f: ('model1 -> 'model2)) (block: (_,'state,_,_,'model1,_) t) =
  let viewModel (s: 'state) dispatch = block.viewModel s dispatch |> f in
  { initialize = block.initialize; handle = block.handle; viewModel = viewModel; getValue =  block.getValue }

let combine (b1: ('init1,'state1,'action1,'event1,'model1,'value1) t)
            (b2: ('init2,'state2,'action2,'event2,'model2,'value2) t) =
  let initialize (a,b) =
    let res1 = b1.initialize a in
    let res2 = b2.initialize b in
    Result2.combine res1 res2 in

  let handle ((state1: 'state1), (state2: 'state2)) (action: ('action1, 'action2) LeftRight.t) =
    match action with
    | LeftRight.Left a -> 
      let res = b1.handle (state1) a in
      { Result2.newState = (res.newState, state2);
        events = (res.events |> List.map (fun x -> LeftRight.Left(x))) }
    | LeftRight.Right a -> 
      let res = b2.handle (state2) a in
      { Result2.newState = (state1, res.newState);
        events = (res.events |> List.map (fun x -> LeftRight.Right(x))) } in

  let viewModel ((state1: 'state1), (state2: 'state2)) (dispatch: ('action1, 'action2) LeftRight.t -> unit) =
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
  let initialize (init: ('init1, 'init2) LeftRight.t) =
    match init with
    | LeftRight.Left init -> 
      b1.initialize init
      |> Result2.mapAction (fun x -> LeftRight.Left x)
      |> Result2.mapState (fun x -> LeftRight.Left x)
      |> Result2.mapEvent (fun x -> LeftRight.Left x)
    | LeftRight.Right init ->
      b2.initialize init
      |> Result2.mapAction (fun x -> LeftRight.Right x)
      |> Result2.mapState (fun x -> LeftRight.Right x)
      |> Result2.mapEvent (fun x -> LeftRight.Right x) in

  let handle (state: ('state1, 'state2) LeftRight.t) (action: ('action1, 'action2) LeftRight.t) =
    match action with
    | LeftRight.Left a ->
      match state with
      | LeftRight.Left s ->
        b1.handle s a
        |> Result2.mapAction (fun x -> LeftRight.Left x)
        |> Result2.mapState (fun x -> LeftRight.Left x)
        |> Result2.mapEvent (fun x -> LeftRight.Left x)
      | LeftRight.Right _s ->
        { newState = state; events = [] }
    | LeftRight.Right a -> 
      match state with
      | LeftRight.Right s -> 
        b1.handle s a
        |> Result2.mapAction (fun x -> LeftRight.Right x)
        |> Result2.mapState (fun x -> LeftRight.Right x)
        |> Result2.mapEvent (fun x -> LeftRight.Right x)
      | LeftRight.Left _s -> 
        { newState = state; events = [] } in

  let viewModel (state: ('state1, 'state2) LeftRight.t) (dispatch: ('action1, 'action2) LeftRight.t -> unit) =
    match state with
    | Left s -> 
      b1.viewModel s (fun a -> dispatch(Left(a)))
    | Right s -> 
      b2.viewModel s (fun a -> dispatch(Right(a))) in

  let getValue (state: ('state1, 'state2) LeftRight.t) =
    match state with
    | LeftRight.Left s -> 
      b1.getValue s |> (fun x -> LeftRight.Left x)
    | LeftRight.Right s -> 
      b2.getValue s |> (fun x -> LeftRight.Right(x)) in

  { initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue }
