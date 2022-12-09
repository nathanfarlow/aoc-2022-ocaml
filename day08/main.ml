open Core

let map_trees trees map_direction collect_direction =
  Array.concat_mapi trees ~f:(fun i row ->
      Array.mapi row ~f:(fun j _ ->
          let height = trees.(i).(j) in
          List.map
            ~f:(fun (di, dj) ->
              map_direction trees height (i + di) (j + dj) di dj)
            [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
          |> collect_direction))

let out_of_bounds i arr = i < 0 || i >= Array.length arr

let rec is_visible trees height i j di dj =
  if out_of_bounds i trees || out_of_bounds j trees.(i) then true
  else if trees.(i).(j) >= height then false
  else is_visible trees height (i + di) (j + dj) di dj

let _part1 trees =
  map_trees trees is_visible (List.exists ~f:Fn.id) |> Array.count ~f:Fn.id

let rec scenic_score trees height i j di dj =
  if out_of_bounds i trees || out_of_bounds j trees.(i) then 0
  else if trees.(i).(j) >= height then 1
  else 1 + scenic_score trees height (i + di) (j + dj) di dj

let _part2 trees =
  let multiply = List.fold ~init:1 ~f:( * ) in
  map_trees trees scenic_score multiply
  |> Array.max_elt ~compare:Int.compare
  |> Option.value_exn

let _ =
  let trees =
    In_channel.read_lines "input.txt"
    |> List.map ~f:(fun line ->
           String.to_list line |> List.map ~f:Char.to_int |> List.to_array)
    |> List.to_array
  in
  printf "%d\n" (_part2 trees)
