(** ActionResult *)

type ('state, 'action, 'event) t = {
    newState: 'state
  ; events: 'event list }

let mk (s: 'state) (e: 'event list) =
  { newState = s; events = e }

let mk_simple (s: 'state) =
  mk s []

let map_state (f: ('state1 -> 'state2)) (r: ('state1, 'action1, 'event1) t) = 
  { newState = f r.newState;
    events = r.events }

let map_event (f: ('event1 -> 'event2)) (r: ('state1, 'action1, 'event1) t) =
  { newState = r.newState;
    events = r.events |> List.map f }

let choose_event (f: ('event1 -> 'event2 option)) (r: ('state1, 'action1, 'event1) t) =
  { newState = r.newState;
    events = r.events 
            |> List.map f
            |> List.filter (function | None -> false | Some _ -> true)
            |> List.map (function | Some x -> x | None -> failwith "oops") }

let map_action (f: ('action1 -> 'action2)) (r: ('state1, 'action1, 'event1) t): ('state1, 'action2, 'event1) t =
  { newState = r.newState;
    events = r.events }

let combine (r1: ('state1, 'action1, 'event1) t) (r2: ('state2, 'action2, 'event2) t) = 
  let allEvents = 
    List.append
      (r1.events |> List.map (fun x -> Either.Left x))
      (r2.events |> List.map (fun x -> Either.Right x)) in

  { newState = (r1.newState, r2.newState);
    events = allEvents }
