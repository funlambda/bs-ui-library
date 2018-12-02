type 'a t =
  | Invalid of string array
  | Valid of 'a
let to_bsObj inner (x: 'a t) =
  match x with
  | Invalid arr -> 
    [%bs.obj { __tag = "Validated"; state = "Invalid"; errors = arr; value = None }]
  | Valid x -> 
    [%bs.obj { __tag = "Validated"; state = "Valid"; errors = [||]; value = Some (inner x) }]

let map f validated =
  match validated with
  | Invalid msgs -> Invalid msgs
  | Valid value -> Valid (f value)

let bind f validated =
  match validated with
  | Invalid msgs -> Invalid msgs
  | Valid value -> f value

let combine (x: 'a t) (y: 'b t) =
    match (x,y) with
    | (Valid a, Valid b) ->
        Valid (a,b)
    | (Invalid msgs, Valid _) ->
        Invalid msgs
    | (Valid _, Invalid msgs) ->
        Invalid msgs
    | (Invalid msgs1, Invalid msgs2) ->
        Invalid (Array.append msgs1 msgs2)

let validateLength ((min: int), (max: int)) (s: string): string t =
    let invalidMsgs = 
        Array.append 
           (if String.length s < min then [|"Too short"|] else [||])
           (if String.length s > max then [|"Too long"|] else [||]) in

    if Array.length invalidMsgs = 0 then Valid s
    else Invalid invalidMsgs

let validateInt (s: string): int t =
  try 
    Valid (int_of_string s)
  with | _ -> Invalid [| "Expecting an integer" |]

let validateFloat (s: string): float t =
  try 
    Valid (float_of_string s)
  with | _ -> Invalid [| "Expecting a number" |]

let toOption (v: 'a t) =
    match v with | Valid x -> Some x | Invalid _ -> None

let fromOption (noneMsg: string) (x: 'a option) =
    match x with | Some x -> Valid x | None -> Invalid [| noneMsg |]

let get (x: 'a t) =
    match x with | Valid y -> y | Invalid _ -> failwith "Unexpected Invalid value passed in to Validation.get"

let required msg = 
  function | "" -> Invalid [| msg |] | s -> Valid s
let maxLength numChars msg (s: string) = 
  if String.length s > numChars then Invalid [| msg |] else Valid s

let (>>=) b a = bind a b

let (>>>) f1 f2 = fun a -> a |> f1 >>= f2
