open Core
module Int_tuple = Tuple.Comparator (Int) (Int)

let update_tail ~head:(hx, hy) ~tail:(tx, ty) =
  let dx, dy = (hx - tx, hy - ty) in
  if abs dx <= 1 && abs dy <= 1 then (tx, ty)
  else Sign.(tx + to_int (of_int dx), ty + to_int (of_int dy))

let update_rope rope (dx, dy) =
  let rec aux rope prev =
    match rope with
    | x :: xs -> f (update_tail ~head:prev ~tail:x) xs
    | [] -> []
  and f x l = x :: aux l x in

  let hx, hy = List.hd_exn rope in
  let new_head = (hx + dx, hy + dy) in
  let result = f new_head (List.tl_exn rope) in
  (result, result)

let parse_line s =
  let make dx dy n = List.init (Int.of_string n) ~f:(fun _ -> (dx, dy)) in
  match String.split s ~on:' ' with
  | [ "L"; n ] -> make (-1) 0 n
  | [ "R"; n ] -> make 1 0 n
  | [ "U"; n ] -> make 0 1 n
  | [ "D"; n ] -> make 0 (-1) n
  | _ -> failwith "invalid input"

let () =
  let input = In_channel.read_lines "input.txt" in
  let instructions = List.concat_map input ~f:parse_line in

  let rope_length = 10 in
  let rope = List.init rope_length ~f:(fun _ -> (0, 0)) in
  let _, ropes = List.fold_map instructions ~init:rope ~f:update_rope in

  List.map ropes ~f:List.last_exn
  |> Set.stable_dedup_list (module Int_tuple)
  |> List.length |> printf "%d\n"
