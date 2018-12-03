(** Either *)

module Either : sig
    type ('a, 'b) t = Left of 'a | Right of 'b

    val left : 'a -> ('a, 'b) t
    val right : 'b -> ('a, 'b) t

    val either : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c

    val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
    val (<$>) : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
    val map_left : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t

    val bimap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

    val pure : 'b -> ('a, 'b) t
    val apply : ('a, ('b -> 'c)) t -> ('a, 'b) t -> ('a, 'c) t
    val (<*>) : ('a, ('b -> 'c)) t -> ('a, 'b) t -> ('a, 'c) t

    val return : 'b -> ('a, 'b) t
    val bind : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t

    val (>>=) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
    val throw : 'a -> ('a, 'b) t

    val is_left : ('a, 'b) t -> bool
    val is_right : ('a, 'b) t -> bool

    val to_string : ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string

    val error : (exn, 'a) t -> 'a

    val try_ : (unit -> 'b) -> (exn, 'b) t

    val hush : ('a, 'b) t -> 'b option
    val note : 'a -> 'b option -> ('a, 'b) t

    val fold : ('b -> 'c -> 'c) -> 'c -> ('a, 'b) t -> 'c

    (* module Lwt : sig
        val try_ : (unit -> 'b Lwt.t) -> (exn, 'b) t Lwt.t
        val error : (exn, 'b) t -> 'b Lwt.t
    end *)
end = struct
    type ('a, 'b) t = Left of 'a | Right of 'b

    (** Constructor functions *)
    let left a = Left a
    let right b = Right b

    let either l r = function
      | Left v -> l v
      | Right v -> r v

    (** Bifunctor interface *)
    let bimap l r = either (fun v -> left (l v)) (fun v -> right (r v))

    external id : 'a -> 'a = "%identity"
    let const v = fun _ -> v

    (** Functor interface *)
    let map f = bimap id f
    let (<$>) = map
    let map_left f = bimap f id

    (** Monadic interface *)
    let bind m k = either left k m

    let return = right
    let (>>=) = bind
    let throw = left

    (** Applicative interface *)
    let pure = return
    let apply f v = f >>= fun f' -> v >>= fun v' -> pure (f' v')
    let (<*>) = apply

    (** Turn a function result in a value or an error *)
    let try_ f = try pure (f ()) with exn -> throw exn

    (** Predicates *)
    let is_left v = either (const true) (const false) v
    let is_right v = either (const false) (const true) v

    let to_string l r = either
                            (fun v -> Printf.sprintf "Left (%s)" (l v))
                            (fun v -> Printf.sprintf "Right (%s)" (r v))

    (** Extract a value of raise an exception *)
    let error v = either (fun e -> raise e) id v

    (** Silence into an option *)
    let hush v = either (const None) (fun v' -> Some v') v

    (** Expand from an option *)
    let note e = function
      | None -> Left e
      | Some v -> Right v

    (** Catamorphism *)
    let fold f z = either (const z) (fun v -> f v z)

    (* module Lwt = struct
        let try_ f = Lwt.catch
                        (fun () -> Lwt.map pure (f ()))
                        (fun exn -> Lwt.return (throw exn))

        let error v = either Lwt.fail Lwt.return v
    end *)
end

include Either