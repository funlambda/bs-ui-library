open BlockLibrary

let block: ('a, 'a, unit, unit, 'a, 'a) Block2.t = 
    let initialize (init: 'a) = 
        { Result2.newState = init; events = [] } in

    let handle (state: 'a) (_action: unit) =
        { Result2.newState = state; events = [] } in

    let viewModel (state: 'a) (_dispatch: unit -> unit)  = 
        state in

    { Block2.initialize = initialize; handle = handle; viewModel = viewModel; getValue = fun x -> x }
