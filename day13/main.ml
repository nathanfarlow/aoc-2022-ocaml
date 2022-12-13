open Core

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.expr Lexer.read lexbuf

let rec compare a b =
  let open Ast in
  match (a, b) with
  | Num a, Num b -> Int.compare a b
  | Values a, Values b -> List.compare compare a b
  | Values a, Num b -> compare (Values a) (Values [ Num b ])
  | Num a, Values b -> compare (Values [ Num a ]) (Values b)

let part1 parsed =
  List.groupi parsed ~break:(fun i _ _ -> i mod 2 = 0)
  |> List.map ~f:(fun l -> (List.nth_exn l 0, List.nth_exn l 1))
  |> List.mapi ~f:(fun i (a, b) -> if compare a b <= 0 then i + 1 else 0)
  |> List.fold ~init:0 ~f:( + ) |> printf "%d\n"

let part2 parsed =
  let parsed = List.map ~f:(fun x -> (false, x)) parsed in
  let to_append = [ (true, parse "[[2]]"); (true, parse "[[6]]") ] in
  List.sort (parsed @ to_append) ~compare:(fun (_, a) (_, b) -> compare a b)
  |> List.mapi ~f:(fun i (a, _) -> if a then i + 1 else 1)
  |> List.fold ~init:1 ~f:( * ) |> printf "%d\n"

let () =
  let parsed =
    In_channel.read_lines "input.txt"
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:parse
  in
  part1 parsed;
  part2 parsed
