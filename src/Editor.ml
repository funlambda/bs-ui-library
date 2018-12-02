open Util

type ('value, 'state, 'action, 'event, 'model) t = 
    ('value option, 'state, 'action, 'event, 'model, 'value Validated.t) Block2.t

let field f block =
    block 
    |> Block2.mapInit (Option.map f)

let record (name: string) (editor: ('value, 'state, 'action, 'event, 'model) t) =
    editor |> Block2.mapModel (dynamic name)

let stringEditor =
    Block2.(
        Textbox.block 
        |> mapInit (function | None -> "" | Some x -> x)
        |> mapValue (fun s -> Validated.Valid s)
    )

let intEditor: (int, _, _, _, _) t =
    Block2.(
        Textbox.block 
        |> mapInit (function | None -> "" | Some i -> string_of_int i)
        |> mapValue Validated.validateInt
    )

open Operators

let (<!>) (fV,fM) block = 
    block 
    |> Block2.mapValue (Validated.map fV)
    |> Block2.mapModel fM

let (<*>) b1 b2 =
    Block2.(
        b1 <&&> b2
        |> mapValue (fun (vF,vA) -> Validated.combine vF vA |> Validated.map (fun (f,a) -> f a))
        |> mapInit (fun a -> a, a)
        |> mapModel (fun (mF,mA) -> mF mA)
        |> chooseEvent (function | LeftRight.Left e -> Some e | LeftRight.Right e -> Some e))
