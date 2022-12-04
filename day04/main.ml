open Core

let parse_interval s =
  match String.split s ~on:'-' with
  | [ first; second ] -> (Int.of_string first, Int.of_string second)
  | _ -> assert false

let parse_interval_pair l =
  match String.split l ~on:',' with
  | [ first; second ] -> (parse_interval first, parse_interval second)
  | _ -> assert false

(* part 1 *)
(* let contains (a, b) =
   let a1, a2 = a in
   let b1, b2 = b in
   (a1 <= b1 && b2 <= a2) || (b1 <= a1 && a2 <= b2) *)

(* part 2 *)
let overlaps (a, b) =
  let a1, a2 = a in
  let b1, b2 = b in
  (a1 <= b1 && b1 <= a2)
  || (a1 <= b2 && b2 <= a2)
  || (b1 <= a1 && a1 <= b2)
  || (b1 <= a2 && a2 <= b2)

let () =
  In_channel.read_lines "input.txt"
  |> List.map ~f:parse_interval_pair
  |> List.map ~f:overlaps |> List.count ~f:Fn.id |> printf "%d\n"
