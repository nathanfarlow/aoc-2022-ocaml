open Core

type instruction = { amount : int; src : int; dst : int }

let parse_config s =
  String.split_lines s
  |> List.map ~f:String.to_array
  |> List.drop_last_exn |> List.to_array |> Array.transpose_exn
  |> Array.filteri ~f:(fun i _ -> i mod 4 = 1)
  |> Array.map ~f:Array.to_list
  |> Array.map ~f:(fun l -> List.drop_while l ~f:Char.is_whitespace)

let parse_instructions s =
  String.split_lines s
  |> List.map ~f:(fun s ->
         let split = String.split s ~on:' ' |> List.to_array in
         {
           amount = Int.of_string split.(1);
           src = Int.of_string split.(3) - 1;
           dst = Int.of_string split.(5) - 1;
         })

let parse_input input =
  match Str.split (Str.regexp "\n\nmove") input with
  | [ config; instructions ] ->
      (parse_config config, parse_instructions instructions)
  | _ -> assert false

let rec execute config instructions place =
  match instructions with
  | [] -> ()
  | { amount; src = src_index; dst = dst_index } :: instructions ->
      let src = config.(src_index) in
      let dst = config.(dst_index) in
      let new_src = List.drop src amount in
      let new_dst = place (List.take src amount) dst in
      config.(src_index) <- new_src;
      config.(dst_index) <- new_dst;
      execute config instructions place

let _part1_place to_place dst = List.rev_append to_place dst
let part2_place to_place dst = to_place @ dst

let _ =
  let input = In_channel.read_all "input.txt" in
  let config, instructions = parse_input input in
  execute config instructions part2_place;
  Array.iter config ~f:(fun l -> printf "%c" (List.hd_exn l));
  print_endline ""
