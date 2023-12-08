let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let nth n lst = List.nth lst n
let get_lines filename = read_file filename |> split_str "\n"
let sum = List.fold_left ( + ) 0

type entry = { hand : string; value : int }
type hands = Five | Four | FullHouse | Three | TwoPair | OnePair | HighCard

let hand_weight = function
  | Five -> 6
  | Four -> 5
  | FullHouse -> 4
  | Three -> 3
  | TwoPair -> 2
  | OnePair -> 1
  | HighCard -> 0

let card = function
  | 'A' -> 12
  | 'K' -> 11
  | 'J' -> 11
  | 'T' -> 10
  | c -> int_of_char c

let hand_to_chars hand = List.init 5 Fun.id |> List.map (String.get hand)

let compare_cards  a b =
  if card a = card b then 0 else if card a > card b then 1 else if card b > card a then -1 else failwith "cards dont coompare"

let get_hand_weight hand =
  let cards = hand_to_chars hand |> List.sort compare_cards in
  let rec get_mults cards last_card acc =
    match cards with
    | [] -> acc
    | card :: tail ->
        if card = last_card then
          match acc with
          | h :: acc_tail -> get_mults tail card ((h + 1) :: acc_tail)
          | _ -> failwith "not possible"
        else get_mults tail card (1 :: acc)
  in
  let mults = get_mults cards ' ' [] |> List.sort compare in
  let t =
    match mults with
    | [ 5 ] -> Five
    | [ 1; 4 ] -> Four
    | [ 2; 3 ] -> FullHouse
    | [ 1; 1; 3 ] -> Three
    | [ 1; 2; 2 ] -> TwoPair
    | [ 1; 1; 1; 2 ] -> OnePair
    | [ 1; 1; 1; 1; 1 ] -> HighCard
    | _ -> failwith "failed hand"
  in
  hand_weight t

let compare_hands a b =
  let rec aux a b =
    match (a, b) with
    | [], [] -> 0
    | ha :: taila, hb :: tailb ->
        if ha = hb then aux taila tailb
        else compare ha hb
    | _ -> failwith "cant compare"
  in
  if a = b then 0
  else
    let w_a = get_hand_weight a in
    let w_b = get_hand_weight b in
    if w_a = w_b then aux (hand_to_chars a) (hand_to_chars b)
    else if w_a > w_b then 1
    else -1

let compare_entries a b = compare_hands a.hand b.hand

let parse_line (line : string) : entry =
  let chunks = line |> split_str " " in
  let hand = chunks |> nth 0 in
  let value = chunks |> nth 1 |> int_of_string in
  { hand; value }

let solve lines =
  let entries = lines |> List.map parse_line |> List.sort compare_entries in
  let () =
    entries |> List.map (fun x -> x.hand) |> String.concat "\n" |> print_endline
  in
  entries |> List.mapi (fun i entry -> entry.value * (i + 1)) |> sum
;;

let () = assert (compare_hands "T55J5" "KTJJT" = 1) in
let () = assert (get_hand_weight "TA5K2" = get_hand_weight "T2J48") in
let () = assert (compare_hands "TA5K2" "T2J48" = 1) in
let sample_lines = get_lines "sample.txt" in
let lines = get_lines "input.txt" in
let () = assert (solve sample_lines = 6440) in
let () = print_endline "---" in
let () = solve lines |> Printf.printf "part1: %d\n" in
(*
   let lines = get_lines "input.txt" in
   let () = assert (solve sample_lines parse_line = 288) in
   let () = solve lines parse_line |> Printf.printf "part1: %d\n" in
   let () = assert (solve sample_lines parse_line2 = 71503) in
   let () = solve lines parse_line2 |> Printf.printf "part2: %d\n" in
*)
print_endline ""
