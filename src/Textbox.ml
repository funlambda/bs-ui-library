open Block2

(* type model = {
    value: string;
    isHovered: bool;
    isFocused: bool;
    onChange: string -> unit;
    onFocus: unit -> unit;
    onBlur: unit -> unit;
    onMouseEnter: unit -> unit;
    onMouseLeave: unit -> unit
}
 *)

type state = {
    value: string;
    isHovered: bool;
    isFocused: bool
}

type action =
    | Change of string
    | Focus
    | Blur
    | MouseEnter
    | MouseLeave

let block: (string, state, action, unit, _, string) Block2.t =
    let initialize (init: string) =
        Result2.mk ({ value = init; isHovered = false; isFocused = false }) in

    let handle (state: state) (action: action) =
        match action with
        | Change s -> Result2.mk { state with value = s }
        | Focus -> Result2.mk { state with isFocused = true }
        | Blur -> Result2.mk { state with isFocused = false }
        | MouseEnter -> Result2.mk { state with isHovered = true }
        | MouseLeave -> Result2.mk { state with isHovered = false } in

    let viewModel (state: state) (dispatch: action -> unit) =
        [%bs.obj {
            __tag = "Textbox";
            value = state.value;
            isHovered = state.isHovered;
            isFocused = state.isFocused;
            onChange = (fun s -> dispatch (Change s));
            onFocus = (fun () -> dispatch Focus);
            onBlur = (fun () -> dispatch Blur);
            onMouseEnter = (fun () -> dispatch MouseEnter);
            onMouseLeave = (fun () -> dispatch MouseLeave)
        }] in

    let getValue (state: state) =
        state.value in

    { initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue; }
