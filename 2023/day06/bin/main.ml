let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let nth n lst = List.nth lst n
let get_lines filename = read_file filename |> split_str "\n"
let prod = List.fold_left ( * ) 1

type entry = { time : int; distance : int }

let parse_line lines n =
  lines |> nth n |> split_str " " |> List.map int_of_string_opt
  |> List.filter_map Fun.id

let parse_line2 lines n =
  lines |> nth n |> split_str " " |> List.map int_of_string_opt
  |> List.filter_map Fun.id |> List.map string_of_int
  |> String.concat "" |> int_of_string |> (fun x -> [x]) 

let play_entry (entry : entry) : int =
  (*
    (time - button)*button > distance
    button**2 - time*button + time**2/4 < time**2/4 - distance

    button < time/2 + sqrt(time**2/4 - distance) 
    button > time/2 - sqrt(time**2/4 - distance)
    e
    *)
  let time = float_of_int entry.time in
  let dist = float_of_int entry.distance in
  let open Float in
  let lower =
    (time /. 2.0) -. sqrt ((time *. time /. 4.0) -. dist) +. 1e-10
    |> ceil |> int_of_float
  in
  let upper =
    (time /. 2.0) +. sqrt ((time *. time /. 4.0) -. dist) -. 1e-10
    |> floor |> int_of_float
  in
  upper - lower + 1 


let solve lines parse_line =
  let times = parse_line lines 0 in 
  let distances = parse_line lines 1 in
  let () = assert ((List.length times) == (List.length distances)) in
  let entries =
    List.map2 (fun time distance -> { time; distance }) times distances
  in
  entries |> List.map play_entry |> prod 
;;

let sample_lines = get_lines "sample.txt" in
let lines = get_lines "input.txt" in
let () = assert (solve sample_lines parse_line = 288) in
let () = solve lines parse_line |> Printf.printf "part1: %d\n" in
let () = assert (solve sample_lines parse_line2 = 71503) in
let () = solve lines parse_line2 |> Printf.printf "part2: %d\n" in
(*
let () = solve sections parse_seeds2 |> Printf.printf "part2: %d\n" in
*)
print_endline ""
