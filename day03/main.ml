(* part 2 *)

open Core

let find_duplicate sacks =
  let string_to_set s = String.to_list s |> Char.Set.of_list in
  List.map sacks ~f:string_to_set
  |> List.reduce ~f:Set.inter |> Option.value_exn |> Set.choose_exn

let priority c =
  let offset_from base c = Char.to_int c - Char.to_int base in
  match c with
  | 'a' .. 'z' -> 1 + offset_from 'a' c
  | 'A' .. 'Z' -> 27 + offset_from 'A' c
  | _ -> assert false

let () =
  In_channel.read_lines "input.txt"
  |> List.groupi ~break:(fun i _ _ -> i mod 3 = 0)
  |> List.map ~f:find_duplicate |> List.map ~f:priority
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"
