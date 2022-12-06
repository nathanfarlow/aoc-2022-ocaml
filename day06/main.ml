open Core

let find_start n s =
  let rec aux i q = function
    | [] -> failwith "No solution"
    | x :: xs ->
        if Queue.length q < n then (
          Queue.enqueue q x;
          aux (i + 1) q xs)
        else if not (Queue.to_list q |> List.contains_dup ~compare:Char.compare)
        then i
        else
          let _ = Queue.dequeue q in
          Queue.enqueue q x;
          aux (i + 1) q xs
  in
  let l = String.to_list s in
  aux 0 (Queue.create ~capacity:n ()) l

let () = In_channel.read_all "input.txt" |> find_start 14 |> printf "%d\n"
