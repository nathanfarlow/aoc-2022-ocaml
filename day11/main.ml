open Core

type value = Int of int | Old
type operation = Add of value | Mult of value
type test = { num : int; t : int; f : int }
type monkey = { items : int Queue.t; op : operation; test : test }

let parse_monkey m =
  let items =
    let csv = Str.split (Str.regexp ": ") m.(1) |> List.last_exn in
    List.map (Str.split (Str.regexp ", ") csv) ~f:(fun s -> Int.of_string s)
    |> Queue.of_list
  in
  let op =
    let parse v = match v with "old" -> Old | _ -> Int (Int.of_string v)
    and split = String.split m.(2) ~on:' ' in
    match List.rev split with
    | v :: "+" :: _ -> Add (parse v)
    | v :: "*" :: _ -> Mult (parse v)
    | _ -> failwith "Invalid operation"
  in
  let test =
    let last_num s = List.last_exn (String.split s ~on:' ') |> Int.of_string in
    let divisble_by = last_num m.(3) in
    let t = last_num m.(4) in
    let f = last_num m.(5) in
    { num = divisble_by; t; f }
  in
  { items; op; test }

let parse_monkeys s =
  let monkeys = Str.split (Str.regexp "\n\n") s in
  List.map monkeys ~f:(fun m ->
      parse_monkey (String.split_lines m |> Array.of_list))
  |> Array.of_list

let eval_op v = function
  | Add (Int i) -> v + i
  | Add Old -> v + v
  | Mult (Int i) -> v * i
  | Mult Old -> v * v

let eval_test v test = if v mod test.num = 0 then test.t else test.f

let simulate_monkey monkeys i calculate_worry =
  let m = monkeys.(i) in
  while not (Queue.is_empty m.items) do
    let item = Queue.dequeue_exn m.items in
    let worry = calculate_worry i (eval_op item m.op) in
    let next = eval_test worry m.test in
    Queue.enqueue monkeys.(next).items worry
  done

let simulate n monkeys calculate_worry =
  for _ = 1 to n do
    Array.iteri monkeys ~f:(fun i _ ->
        simulate_monkey monkeys i calculate_worry)
  done

let count_inspected monkeys =
  let _part1 v = v / 3 in
  let _part2 =
    let modulus =
      Array.fold monkeys ~init:1 ~f:(fun acc m -> acc * m.test.num)
    in
    fun v -> v mod modulus
  in
  let num_inspected = Array.init (Array.length monkeys) ~f:(fun _ -> 0) in
  let calculate_worry part src v =
    num_inspected.(src) <- num_inspected.(src) + 1;
    part v
  in
  (* part 1 *)
  (* simulate 20 monkeys (calculate_worry _part1); *)
  (* part 2 *)
  simulate 10000 monkeys (calculate_worry _part2);
  Array.sort num_inspected ~compare:Int.descending;
  num_inspected.(0) * num_inspected.(1)

let () =
  In_channel.read_all "input.txt"
  |> parse_monkeys |> count_inspected |> printf "%d\n"
