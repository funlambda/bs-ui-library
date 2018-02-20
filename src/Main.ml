open BlockLibrary
open Reaction

let (<&&>) b1 b2 = Block2.combine b1 b2
let (<||>) b1 b2 = Block2.either b1 b2

let block2 =
  Button.block <&&> Button.block
  |> Block2.mapInit (fun () -> (),())
  |> Block2.mapValue ignore

let block3 =
  (Button.block <||> Button.block)
  |> Block2.mapInit (fun () -> LeftRight.Left(()))
  |> Block2.mapValue ignore

let static = Static.block

let block4 =
  (Button.block, static)
  |> Reaction.mkBlock2 (fun (a, b1, b2) ->
      match a with
      | LeftRight.Left _ -> 
        match b1.events with
        | [ Button.Clicked ] -> [ (LeftRight.Right (ReInit (b2.value + 1))) ]
        | _ -> []
      | _ -> [])
  |> Block2.mapInit (fun () -> (), 4)
  |> Block2.mapValue ignore

let runApp onModelChanged = run block4 onModelChanged
