(* Part 2 *)

open Core

type outcome = Loss | Tie | Win
type move = Rock | Paper | Scissors

let find_move (opponent, outcome) =
  match (opponent, outcome) with
  | Rock, Tie | Paper, Loss | Scissors, Win -> Rock
  | Rock, Win | Paper, Tie | Scissors, Loss -> Paper
  | Rock, Loss | Paper, Win | Scissors, Tie -> Scissors

let score_match (opponent, outcome) =
  let me = find_move (opponent, outcome) in
  let shape_score = match me with Rock -> 1 | Paper -> 2 | Scissors -> 3 in
  let outcome_score = match outcome with Loss -> 0 | Tie -> 3 | Win -> 6 in
  shape_score + outcome_score

let parse_line line =
  match String.split line ~on:' ' with
  | [ a; b ] ->
      let find_p1 = function
        | "A" -> Some Rock
        | "B" -> Some Paper
        | "C" -> Some Scissors
        | _ -> None
      and find_p2 = function
        | "X" -> Some Loss
        | "Y" -> Some Tie
        | "Z" -> Some Win
        | _ -> None
      in
      Option.both (find_p1 a) (find_p2 b)
  | _ -> None

let () =
  let score =
    In_channel.read_lines "input.txt"
    |> List.filter_map ~f:parse_line
    |> List.sum (module Int) ~f:score_match
  in
  printf "Score is: %d\n" score
