let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let nth n lst = List.nth lst n
let sum = List.fold_left ( + ) 0
let pow2 n = List.init n Fun.id |> (List.fold_left (fun acc _ -> if acc = 0 then 1 else 2*acc) 0)
(*
let prod = List.fold_left ( * ) 1
let string_of_char = String.make 1
let ( let* ) = Option.bind
*)

let rec is_in lst x =
  match lst with
  | [] -> false
  | h :: _ when h = x -> true
  | _ :: tail -> is_in tail x
 

let get_lines filename = read_file filename |> split_str "\n"

  let parse_line line =
    let x = line |> split_str ": " |> nth 1 |> split_str " | " in
    (*let () = "-" ^ (nth 0 x) ^ "," ^ (nth 1 x) ^"-"|> print_endline in*)
    let my_nums = nth 0 x |> split_str " " |> List.map int_of_string_opt |> List.filter_map Fun.id in
    let win_nums = nth 1 x |> split_str " " |> List.map int_of_string_opt |> List.filter_map Fun.id in
    let winning_nums =  List.filter (is_in win_nums) my_nums in
    winning_nums |> List.length 

let solve lines =
  lines |> List.map parse_line |> List.map pow2 |> sum

let _take_n n lst =
    let rec aux lst n acc = 
        if n = 0 then acc 
        else 
            match lst with
            | [] -> acc
            | h :: tail -> aux tail (n-1 )(h:: acc) 
    in
    aux lst n []

let solve2 lines = 
    let rec play_cards cards  num_cards = 
        if num_cards = 0 then 0
        else 
        match cards with 
        | [] -> 0
        | h :: tail -> (
            let num_won_cards = parse_line h in
            (play_cards tail (num_cards - 1)) + (play_cards tail num_won_cards) + 1
        )
    in  
    let () = print_endline "---" in
    let num_cards = List.length lines in
    play_cards lines num_cards 


;;




let sample_lines = get_lines "sample.txt" in
let lines = get_lines "input.txt" in
let () = assert (solve sample_lines = 13) in
let () = solve lines |> Printf.printf "part1: %d" in
let () = solve2 sample_lines |> Printf.printf "sample2: %d" in
let () = solve2 lines |> Printf.printf "part2: %d" in
print_endline ""
