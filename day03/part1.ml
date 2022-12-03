(* part 1 *)

open Core

let parse_line l =
  let as_list = String.to_list l in
  List.split_n as_list (List.length as_list / 2)

let find_duplicate (first, second) =
  Set.inter (Char.Set.of_list first) (Char.Set.of_list second) |> Set.choose_exn

let priority c =
  let offset base c = Char.to_int c - Char.to_int base in
  match c with
  | 'a' .. 'z' -> 1 + offset 'a' c
  | 'A' .. 'Z' -> 27 + offset 'A' c
  | _ -> assert false

let () =
  In_channel.read_lines "input.txt"
  |> List.map ~f:parse_line |> List.map ~f:find_duplicate
  |> List.map ~f:priority |> List.fold ~init:0 ~f:( + ) |> printf "%d\n"
