let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let sum = List.fold_left ( + ) 0

let is_safe lst =
  let rec check_inc tail =
    match tail with
    | [] -> 1
    | [ _ ] -> 1
    | h :: w :: t -> if w > h && w < h + 4 then check_inc (w :: t) else 0
  in
  let rec check_des tail =
    match tail with
    | [] -> 1
    | [ _ ] -> 1
    | h :: w :: t -> if w < h && w > h - 4 then check_des (w :: t) else 0
  in
  match lst with
  | [] -> 1
  | [ _ ] -> 1
  | h :: w :: t ->
      if h < w then check_inc (h :: w :: t) else check_des (h :: w :: t)

let is_safe2 lst =
  let rec remove_one head tail =
    match (tail, head) with
    | [], _ -> 0
    | h :: new_tail, head ->
        if is_safe (List.concat [ head; new_tail ]) > 0 then 1
        else remove_one (List.concat [ head; [ h ] ]) new_tail
  in
  if is_safe lst = 1 then 1 else remove_one [] lst

let solve filename func =
  let data =
    read_file filename |> split_str "\n"
    |> List.map (split_str " ")
    |> List.map (List.filter_map int_of_string_opt)
  in
  data |> List.map func |> sum
;;

let () = assert (solve "sample.txt" is_safe = 2) in
let () = assert (solve "sample.txt" is_safe2 = 4) in
let () = solve "input.txt" is_safe |> string_of_int |> print_endline in
let () = solve "input.txt" is_safe2 |> string_of_int |> print_endline in
print_endline ""
