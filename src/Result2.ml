open Util

type ('state, 'action, 'event) t = {
  newState: 'state;
  events: 'event list
}

let mk (s: 'state) =
  { newState = s; events = [] }

let mapState (f: ('state1 -> 'state2)) (r: ('state1, 'action1, 'event1) t) = 
  { newState = f r.newState;
    events = r.events }

let mapEvent (f: ('event1 -> 'event2)) (r: ('state1, 'action1, 'event1) t) =
  { newState = r.newState;
    events = r.events |> List.map f }

let mapAction (f: ('action1 -> 'action2)) (r: ('state1, 'action1, 'event1) t): ('state1, 'action2, 'event1) t =
  { newState = r.newState;
    events = r.events }

let combine (r1: ('state1, 'action1, 'event1) t) (r2: ('state2, 'action2, 'event2) t) = 
  let allEvents = 
    List.append
      (r1.events |> List.map (fun x -> LeftRight.Left x))
      (r2.events |> List.map (fun x -> LeftRight.Right x)) in

  { newState = (r1.newState, r2.newState);
    events = allEvents }

