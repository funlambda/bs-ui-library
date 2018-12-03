(** Reactor *)

type ('init, 'state, 'action, 'event) t =
    | ReInit of 'init
    | ReplaceState of 'state
    | ApplyAction of 'action
    (* | DoExtra of Extra *)
    | FireEvent of 'event
    (* | RunFollowUp of TheFollowUp<'action> *)

type ('state, 'value, 'event) blockInfo = {
    state: 'state;
    value: 'value;
    events: 'event list
}

let getBlockInfo (block: (_,_,_,_,_,_) Block.t) (result: (_,_,_) ActionResult.t) =
    { state = result.newState; value = block.getValue result.newState; events = result.events }

let applyReaction reaction (block: (_,_,_,_,_,_) Block.t) result =
    match reaction with
    | ReInit i -> 
        block.initialize i
        (* let followUp = FollowUp.combine result.FollowUp result2.FollowUp *)
        (* let extras = Array.append result.Extras result2.Extras *)
        (* Result.mkRawWithExtasAndEvents result2.State followUp extras (List.append result.Events result2.Events) *)
    | ReplaceState s -> 
        result 
        |> ActionResult.map_state (fun _ -> s) 
    | ApplyAction a -> 
        let result2 = block.handle result.newState a in
        (* let followUp = FollowUp.combine result.FollowUp result2.FollowUp *)
        (* let extras = Array.append result.Extras result2.Extras *)
        (* Result.mkRawWithExtasAndEvents result2.State followUp extras (List.append result.Events result2.Events) *)
        { newState = result2.newState; events = List.append result.events result2.events }
    (* | DoExtra e -> 
        { result with Extras = Array.concat [ result.Extras; [| e |] ] } *)
    | FireEvent e ->
        { result with events = List.append result.events [ e ] }
    (* | RunFollowUp fu ->
        let followUp = FollowUp.combine result.FollowUp fu
        { result with FollowUp = followUp } *)

let mkBlock
        (getReactions: 'action * ('state, 'value, 'event) blockInfo -> ('init, 'state, 'action, 'event) t array)
        (block: ('init, 'state, 'action, 'model, 'value, 'event) Block.t):
        ('init, 'state, 'action, 'model, 'value, 'event) Block.t =
    let handle (state: 'State) (action: 'Action) =
        let rec applyReactions result reactions =
            match reactions with
            | r::tail ->
                let newResult = applyReaction r block result in
                applyReactions newResult tail
            | [] -> result in

        let result = block.handle state action in
        let blockInfo = getBlockInfo block result in
        
        getReactions (action, blockInfo)
        |> Array.to_list
        |> applyReactions result in

    { initialize = block.initialize; handle = handle; viewModel = block.viewModel; getValue = block.getValue }


let mkBlock2 
        (getReactions: ('action1, 'action2) Either.t 
                        * ('state1, 'value1, 'event1) blockInfo 
                        * ('state2, 'value2, 'event2) blockInfo 
                        -> (('init1, 'state1, 'action1, 'event1) t, ('init2, 'state2, 'action2, 'event2) t) Either.t list)
        ((block1: ('init1, 'state1, 'action1, 'event1, 'model1, 'value1) Block.t),
         (block2: ('init2, 'state2, 'action2, 'event2, 'model2, 'value2) Block.t))
        : (('init1 * 'init2), ('state1 * 'state2), ('action1, 'action2) Either.t, ('event1, 'event2) Either.t, ('model1 * 'model2), ('value1 * 'value2)) Block.t =

    let mergeResultsIntoOne ((result1: (_,_,_) ActionResult.t), 
                             (result2: (_,_,_) ActionResult.t)): (_,_,_) ActionResult.t =
        let newState = result1.newState, result2.newState in
        (* let followUp = 
            FollowUp.combine
                (result1.FollowUp |> FollowUp.map Choice1Of2)
                (result2.FollowUp |> FollowUp.map Choice2Of2)
        let extras = 
            Array.concat [ result1.Extras; result2.Extras ] *)
        let events = 
            List.concat 
                [
                    (result1.events |> List.map (fun x -> Either.Left x));
                    (result2.events |> List.map (fun x -> Either.Right x))
                ] in
        { newState = newState; events = events } in

    let initialize ((init1: 'init1), (init2: 'init2)) =
        let result1 = block1.initialize init1 in
        let result2 = block2.initialize init2 in
        mergeResultsIntoOne (result1, result2) in

    let handle ((state1: 'state1), (state2: 'state2)) (action: ('action1, 'action2) Either.t) =
        let (result1, result2) =
            match action with
            | Either.Left a ->
                let result1 = block1.handle state1 a in
                let result2 = { ActionResult.newState = state2; events = [] }  in
                result1,result2
            | Either.Right a -> 
                let result1 = { ActionResult.newState = state1; events = [] }  in
                let result2 = block2.handle state2 a in
                result1,result2 in

        let rec applyReactions (result1, result2) reactions =
            match reactions with
            | head::tail ->
                let newResults = 
                    match head with
                    | Either.Left r -> result1 |> applyReaction r block1, result2
                    | Either.Right r -> result1, result2 |> applyReaction r block2 in
                applyReactions newResults tail
            | [] -> (result1, result2) in

        let blockInfo1 = getBlockInfo block1 result1 in
        let blockInfo2 = getBlockInfo block2 result2 in

        getReactions (action, blockInfo1, blockInfo2)
        |> applyReactions (result1, result2)
        |> mergeResultsIntoOne in

    let viewModel ((state1: 'state1), (state2: 'state2)) (dispatch: ('action1, 'action2) Either.t -> unit) =
        let model1 = block1.viewModel state1 (fun x -> dispatch (Either.Left x)) in
        let model2 = block2.viewModel state2 (fun x -> dispatch (Either.Right x)) in
        (model1,model2) in

    let getValue (state: 'State1 * 'State2) =
        block1.getValue (fst state), block2.getValue (snd state) in

    { initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue }
