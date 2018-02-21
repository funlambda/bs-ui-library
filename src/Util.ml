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
