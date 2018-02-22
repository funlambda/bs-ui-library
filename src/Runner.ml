let run (block: (unit,'state,'action,'event,'model,unit) Block2.t) (onModelChanged: 'model -> unit) =
  let start (initialState: 'state) =
    let state = ref initialState in

    let rec dispatch (action: 'action) =
      let result = block.handle !state action in
      state := result.newState;
      let newModel = block.viewModel !state dispatch in
      let x = newModel in
      onModelChanged x in
    
    dispatch in

  let initialResult = block.initialize () in
  let initialState = initialResult.newState in

  let dispatch = start initialResult.newState in
  let initialModel = block.viewModel initialState dispatch in

  onModelChanged initialModel;
  ()
