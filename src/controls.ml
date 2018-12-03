open Belt

module Button = struct
  type state = { isHovered: bool }
  type action = | OnClick | OnMouseEnter | OnMouseLeave
  type event = | Clicked
  (* type model = { isHovered: bool; onClick: unit -> unit; onMouseEnter: unit -> unit; onMouseLeave: unit -> unit } *)

  let initialize () = 
    { ActionResult.newState = { isHovered = false }; events = [] }

  let handle (state: state) (action: action) =
    match action with
    | OnClick -> { ActionResult.newState = state; events = [ Clicked ]}
    | OnMouseEnter -> { ActionResult.newState = { isHovered = true }; events = []}
    | OnMouseLeave -> { ActionResult.newState = { isHovered = false }; events = []}

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
    { Block.initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue }
end

module Textbox = struct
  type model = {
    value: string;
    isHovered: bool;
    isFocused: bool;
    onChange: string -> unit;
    onFocus: unit -> unit;
    onBlur: unit -> unit;
    onMouseEnter: unit -> unit;
    onMouseLeave: unit -> unit
  }

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

  let block: (string, state, action, unit, _, string) Block.t =
    let initialize (init: string) =
      ActionResult.mk_simple ({ value = init; isHovered = false; isFocused = false }) in

    let handle (state: state) (action: action) =
      match action with
      | Change s -> ActionResult.mk_simple { state with value = s }
      | Focus -> ActionResult.mk_simple { state with isFocused = true }
      | Blur -> ActionResult.mk_simple { state with isFocused = false }
      | MouseEnter -> ActionResult.mk_simple { state with isHovered = true }
      | MouseLeave -> ActionResult.mk_simple { state with isHovered = false } in

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
end

module Slider = struct
  type state = {
    isSliding: bool;
    position: float
  }

  type action = 
    | StartSliding
    | MoveSlider of float
    | StopSliding

  (* type model = {
      isSliding: bool;
      position: float;
      onStart: unit -> unit;
      onMove: float -> unit;
      onStop: unit -> unit;
  } *)

  let block: (float, state, action, unit, _, float) Block.t =
    let initialize position =
      { ActionResult.newState = { isSliding = false; position = position }; events = [] } in

    let handle (state: state) (action: action) =
      match action with
      | StartSliding ->
        { ActionResult.newState = { isSliding = true; position = state.position }; events = [] }
      | MoveSlider x ->
        { ActionResult.newState = { isSliding = true; position = x }; events = [] }
      | StopSliding ->
        { ActionResult.newState = { isSliding = false; position = state.position }; events = [] } in

    let viewModel (state: state) dispatch =
      [%bs.obj {
        __tag = "Slider";
        isSliding = state.isSliding;
        position = state.position;
        onStart = (fun () -> dispatch StartSliding);
        onMove = (fun p -> dispatch (MoveSlider p));
        onStop = (fun () -> dispatch StopSliding);
      }] in

    let getValue (state: state) =
      state.position in

    { initialize = initialize; handle = handle; viewModel = viewModel; getValue = getValue }
end

module Editor = struct
  type ('a, 'state, 'action, 'event, 'model) t = 
    ('a, 'state, 'action, 'event, 'model, 'a) Block.t
end

module ValidatedEditor = struct
  type ('a, 'state, 'action, 'event, 'model) t = 
    ('a option, 'state, 'action, 'event, 'model, 'a Validated.t) Block.t

  let field f block =
    block
    |> Block.map_init (fun x -> Option.map x f)

  let (<!>) (v_mapper,m_mapper) block = 
    block 
    |> Block.map_value (Validated.map v_mapper)
    |> Block.map_model m_mapper

  let (<*>) b1 b2 =
    Block.(
      b1 <&&> b2
      |> map_value (fun (vF,vA) -> Validated.combine vF vA |> Validated.map (fun (f,a) -> f a))
      |> map_init (fun a -> a, a)
      |> map_model (fun (mF,mA) -> mF mA)
      |> choose_event (function | Either.Left e -> Some e | Either.Right e -> Some e))

  let mk b from_option validate =
    Block.(b |> map_init from_option |> map_value validate)

  let string_textbox = 
    mk Textbox.block (fun x -> Option.getWithDefault x "") (fun x -> Validated.Valid x)

  let int_textbox = 
    mk Textbox.block (function | None -> "" | Some i -> string_of_int i) Validated.validateInt
end
