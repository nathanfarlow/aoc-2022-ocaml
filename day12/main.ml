open Core

let parse lines =
  List.map lines ~f:(fun line ->
      String.to_list line
      |> List.map ~f:(fun x -> Char.to_int x - Char.to_int 'a')
      |> Array.of_list)
  |> Array.of_list

let find_and_set a c v =
  let f c = Char.to_int c - Char.to_int 'a' in
  let i, j =
    Array.find_mapi_exn a ~f:(fun i row ->
        Array.find_mapi row ~f:(fun j x ->
            if x = f c then Some (i, j) else None))
  in
  a.(i).(j) <- f v;
  (i, j)

let dijkstra a ~start:(si, sj) ~finish:(fi, fj) =
  let m = Array.length a in
  let n = Array.length a.(0) in
  let d = Array.make_matrix ~dimx:m ~dimy:n Int.max_value in
  let q =
    Pairing_heap.create
      ~cmp:(fun (i1, j1) (i2, j2) -> Int.compare d.(i1).(j1) d.(i2).(j2))
      ()
  in
  d.(si).(sj) <- 0;
  Pairing_heap.add q (si, sj);
  let rec loop () =
    if Pairing_heap.is_empty q then None
    else
      let i, j = Pairing_heap.pop_exn q in
      if i = fi && j = fj then Some d.(i).(j)
      else
        let neighbours = [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ] in
        List.iter neighbours ~f:(fun (i', j') ->
            if i' >= 0 && i' < m && j' >= 0 && j' < n then
              let step = a.(i').(j') - a.(i).(j) in
              if step <= 1 then
                let dist = d.(i).(j) + 1 in
                if dist < d.(i').(j') then (
                  d.(i').(j') <- dist;
                  Pairing_heap.add q (i', j')));
        loop ()
  in
  loop ()

let _part1 board start finish =
  match dijkstra board ~start ~finish with
  | Some d -> printf "%d\n" d
  | None -> failwith "No path"

let _part2 board finish =
  let min_exn a = Array.min_elt a ~compare:Int.compare |> Option.value_exn in
  Array.mapi board ~f:(fun i row ->
      Array.filter_mapi row ~f:(fun j x ->
          if x = 0 then dijkstra board ~start:(i, j) ~finish else None)
      |> min_exn)
  |> min_exn |> printf "%d\n"

let _ =
  let board = In_channel.read_lines "input.txt" |> parse in
  let _start = find_and_set board 'S' 'a' in
  let finish = find_and_set board 'E' 'z' in
  _part1 board _start finish
(* _part2 board finish *)
