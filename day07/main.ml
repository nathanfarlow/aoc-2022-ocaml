open Core

type dir = {
  name : string;
  parent : dir option;
  mutable files : int list;
  mutable dirs : dir list;
}

let find_dir root name =
  List.find root.dirs ~f:(fun dir -> String.equal dir.name name)

let parse_tree lines =
  let root = { name = "/"; parent = None; files = []; dirs = [] } in
  let rec aux lines (node : dir) =
    match lines with
    | [] -> ()
    | x :: xs -> (
        match String.split x ~on:' ' with
        | [ "$"; "cd"; dir ] -> (
            match dir with
            | "/" -> aux xs root
            | ".." -> (
                match node.parent with
                | Some parent -> aux xs parent
                | None -> failwith "Tried to cd .. from root")
            | _ -> (
                match find_dir node dir with
                | Some dir -> aux xs dir
                | None ->
                    failwith "Tried to cd into a directory that doesn't exist"))
        | [ "$"; "ls" ] -> aux xs node
        | [ "dir"; dir ] ->
            node.dirs <-
              { name = dir; parent = Some node; files = []; dirs = [] }
              :: node.dirs;
            aux xs node
        | [ size; _name ] ->
            node.files <- Int.of_string size :: node.files;
            aux xs node
        | _ -> failwith "Invalid entry")
  in
  aux lines root;
  root

let rec dir_size dir =
  let file_sum = List.fold dir.files ~init:0 ~f:(fun sum size -> sum + size) in
  let dir_sum =
    List.fold dir.dirs ~init:0 ~f:(fun sum dir -> sum + dir_size dir)
  in
  file_sum + dir_sum

let rec get_all_sizes dir =
  dir_size dir :: List.concat_map dir.dirs ~f:get_all_sizes

let _part1 n dir =
  get_all_sizes dir
  |> List.filter ~f:(fun size -> size <= n)
  |> List.sum (module Int) ~f:Fn.id

let part2 total required dir =
  let used = dir_size dir in
  let free = total - used in
  let need_to_delete = required - free in
  let sizes = get_all_sizes dir in
  List.filter sizes ~f:(fun size -> size >= need_to_delete)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let _ =
  let lines = In_channel.read_lines "input.txt" in
  let tree = parse_tree lines in
  (* let answer = part1 100000 tree in *)
  let answer = part2 70000000 30000000 tree in
  printf "%d\n" answer
