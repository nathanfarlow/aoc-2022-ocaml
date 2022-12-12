open Core

type instruction = Nop | Add of int

let parse l =
  List.map l ~f:(fun s ->
      match String.split s ~on:' ' with
      | [ "noop" ] -> Nop
      | [ "addx"; n ] -> Add (Int.of_string n)
      | _ -> failwith "invalid instruction")

let run insns =
  let rec aux cycles x = function
    | [] -> []
    | Nop :: rest -> x :: aux (cycles + 1) x rest
    | Add n :: rest -> x :: x :: aux (cycles + 2) (x + n) rest
  in
  1 :: aux 1 1 insns

let _part1 results =
  List.filter_mapi results ~f:(fun i x ->
      if i mod 40 = 20 then Some (i * x) else None)
  |> List.sum (module Int) ~f:Fn.id

let _part2 results =
  let render_line line =
    List.mapi (List.tl_exn line) ~f:(fun i x ->
        if abs (i - x) <= 1 then "#" else " ")
    |> String.concat
  in
  let chunks = List.groupi results ~break:(fun i _ _ -> i mod 40 = 0) in
  let lines = List.map chunks ~f:render_line in
  List.iter lines ~f:(printf "%s\n")

let () = In_channel.read_lines "input.txt" |> parse |> run |> _part2
