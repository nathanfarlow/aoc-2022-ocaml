open Core

let find_top_elves s n =
  let sorted =
    Str.split (Str.regexp "\n\n") s
    |> List.map ~f:(String.split ~on:'\n')
    |> List.map ~f:(List.sum (module Int) ~f:Int.of_string)
    |> List.sort ~compare:Int.descending
  in
  List.take sorted n |> List.sum (module Int) ~f:Fn.id

let () =
  let input = In_channel.read_all "input.txt" in
  let max_elf = find_top_elves input 3 in
  printf "Sum of top elves: %d\n" max_elf
