open Block2
open Util

let (<&&>) a b = Block2.combine a b
let (<||>) a b = Block2.either a b

let (<!>) f block = block |> mapValue f
let (<*>) b1 b2 =
    b1 <&&> b2
    |> mapValue (fun (f,a) -> f a)
    |> mapInit (fun a -> a, a)
    |> mapModel (fun (m1,m2) -> List.append m1 m2)
    |> chooseEvent (function | LeftRight.Left e -> Some e | LeftRight.Right e -> Some e)
