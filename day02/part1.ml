(* Part 1 *)

open Core

type outcome = Loss | Tie | Win
type move = Rock | Paper | Scissors

let find_outcome (opponent, me) =
  match (opponent, me) with
  | Rock, Scissors | Paper, Rock | Scissors, Paper -> Loss
  | Rock, Rock | Paper, Paper | Scissors, Scissors -> Tie
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> Win

let score_match (opponent, me) =
  let shape_score = match me with Rock -> 1 | Paper -> 2 | Scissors -> 3 in
  let outcome_score =
    match find_outcome (opponent, me) with Loss -> 0 | Tie -> 3 | Win -> 6
  in
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
        | "X" -> Some Rock
        | "Y" -> Some Paper
        | "Z" -> Some Scissors
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
