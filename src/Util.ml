module LeftRight = struct
  type ('a, 'b) t = 
  | Left of 'a
  | Right of 'b
end

let getOption (x: 'a option) =
    match x with
    | Some y -> y
    | None -> failwith "option has no value"

let float_of_string_opt (s: string) =
    try
        float_of_string s |> (fun x -> Some x)
    with | _ex -> None

let (>>) f1 f2 = fun x -> f2 (f1 x)

let dynamic (tag: string) (addl: (string * 'b) list) =
     ("__tag", tag)::addl

module Option = struct
    let map f = function | None -> None | Some x -> Some (f x)
end
