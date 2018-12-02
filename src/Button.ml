open Block2
open Result2

type state = { isHovered: bool }
type action = | OnClick | OnMouseEnter | OnMouseLeave
type event = | Clicked
(* type model = { isHovered: bool; onClick: unit -> unit; onMouseEnter: unit -> unit; onMouseLeave: unit -> unit } *)

let initialize () = 
  { newState = { isHovered = false }; events = [] }

let handle (state: state) (action: action) =
  match action with
  | OnClick -> { newState = state; events = [ Clicked ]}
  | OnMouseEnter -> { newState = { isHovered = true }; events = []}
  | OnMouseLeave -> { newState = { isHovered = false }; events = []}

let viewModel (state: state) (dispatch: action -> unit) =
  [%bs.obj {
      __tag = "Button";
      isHovered = state.isHovered;
      onClick = (fun () -> dispatch OnClick);
      onMouseEnter = (fun () -> dispatch OnMouseEnter);
      onMouseLeave = (fun () -> dispatch OnMouseLeave)
    }]

let getValue (_state: state) = ()

let block =
  { initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue }
