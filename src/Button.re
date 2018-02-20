open BlockLibrary;
open Result2;

type state = { isHovered: bool };
type action = | OnClick | OnMouseEnter | OnMouseLeave;
type event = | Clicked;
/* type model = { isHovered: bool, onClick: unit => unit, onMouseEnter: unit => unit, onMouseLeave: unit => unit }; */

let initialize = () => { newState: { isHovered: false }, events: [] };
let handle = (state: state) => (action: action) => {
  switch (action){
  | OnClick => { newState: state, events: [ Clicked ]}
  | OnMouseEnter => { newState: { isHovered: true }, events: []}
  | OnMouseLeave => { newState: { isHovered: false }, events: []}
  }
};

let viewModel = (state: state) => (dispatch: action => unit) => {
  [%bs.obj {
     isHovered: state.isHovered, 
     onClick: () => dispatch(OnClick), 
     onMouseEnter: () => dispatch(OnMouseEnter), 
     onMouseLeave: () => dispatch(OnMouseLeave)
  }]
};

let getValue = (_state: state) => ();

let block: Block2.t(_, _, _, _, _, _) = { initialize, handle, viewModel, getValue }
};

let run = (block: Block2.t(unit,'state,'action,'event,'model,unit)) => (onModelChanged: 'model => unit) => {
let start = (initialState: 'state) => {
  let state = ref(initialState);

  let rec dispatch = (action: 'action) => {
    let result = block.handle(state^, action);
    state := result.newState;
    let newModel = block.viewModel(state^, dispatch);
    let x = newModel;
    onModelChanged(x);
  };

  dispatch
};

let initialResult = block.initialize(());
let initialState = initialResult.newState;

let dispatch = start(initialResult.newState);
let initialModel = block.viewModel(initialState, dispatch);

initialModel