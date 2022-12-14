open Core

type item = Rock | Sand | Empty

let parse line =
  Str.split (Str.regexp " -> ") line
  |> List.map ~f:(String.split ~on:',')
  |> List.map ~f:(fun l ->
         (Int.of_string @@ List.hd_exn l, Int.of_string @@ List.last_exn l))

let draw_line grid (x1, y1) (x2, y2) =
  for y = min y1 y2 to max y1 y2 do
    for x = min x1 x2 to max x1 x2 do
      grid.(y).(x) <- Rock
    done
  done

let rec draw_path grid path =
  match path with
  | a :: b :: rest ->
      draw_line grid a b;
      draw_path grid (b :: rest)
  | _ -> ()

let drop_sand grid =
  let rec drop_sand' y x =
    if y >= Array.length grid - 1 then None
    else
      match grid.(y).(x) with
      | Rock | Sand -> None
      | Empty -> (
          match grid.(y + 1).(x) with
          | Empty -> drop_sand' (y + 1) x
          | Rock | Sand -> (
              match grid.(y + 1).(x - 1) with
              | Empty -> drop_sand' (y + 1) (x - 1)
              | Rock | Sand -> (
                  match grid.(y + 1).(x + 1) with
                  | Empty -> drop_sand' (y + 1) (x + 1)
                  | Rock | Sand -> Some (y, x))))
  in
  drop_sand' 0 500

let simulate grid =
  let rec simulate' grid =
    match drop_sand grid with
    | None -> grid
    | Some (y, x) ->
        grid.(y).(x) <- Sand;
        simulate' grid
  in
  simulate' grid

let make_grid paths m n =
  let grid = Array.make_matrix ~dimx:m ~dimy:n Empty in
  List.iter paths ~f:(draw_path grid);
  grid

let count_sand grid =
  Array.fold grid ~init:0 ~f:(fun acc row ->
      acc + Array.count row ~f:(function Sand -> true | _ -> false))

let part1 paths =
  make_grid paths 1000 1000 |> simulate |> count_sand |> printf "%d\n"

let part2 paths =
  let grid = make_grid paths 1000 1000 in
  let max_y =
    List.fold paths ~init:0 ~f:(fun acc path ->
        List.fold path ~init:acc ~f:(fun acc (_, y) -> max acc y))
  in
  draw_line grid (0, max_y + 2) (Array.length grid.(0) - 1, max_y + 2);
  simulate grid |> count_sand |> printf "%d\n"

let () =
  let paths = In_channel.read_lines "input.txt" |> List.map ~f:parse in
  part1 paths;
  part2 paths