open Result2

type state = {
    isSliding: bool;
    position: float
}

type action = 
    | StartSliding
    | MoveSlider of float
    | StopSliding

type model = {
    isSliding: bool;
    position: float;
    onStart: unit -> unit;
    onMove: float -> unit;
    onStop: unit -> unit;
}

let block: (float, state, action, unit, model, float) Block2.t =
    let initialize position =
        { newState = { isSliding = false; position = position }; events = [] } in

    let handle (state: state) (action: action) =
        match action with
        | StartSliding ->
            { newState = { isSliding = true; position = state.position }; events = [] }
        | MoveSlider x ->
            { newState = { isSliding = true; position = x }; events = [] }
        | StopSliding ->
            { newState = { isSliding = false; position = state.position }; events = [] } in

    let viewModel (state: state) dispatch =
        {
            isSliding = state.isSliding;
            position = state.position;
            onStart = (fun () -> dispatch StartSliding);
            onMove = (fun p -> dispatch (MoveSlider p));
            onStop = (fun () -> dispatch StopSliding);
        } in

    let getValue (state: state) =
        state.position in

    { initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue }
