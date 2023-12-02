let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let sum = List.fold_left ( + ) 0

let filter acc c =
  let asc = Char.code c in
  let out =
    match c with c when 48 <= asc && asc < 58 -> String.make 1 c | _ -> ""
  in
  acc ^ out

let first_last s =
  let len = String.length s in
  let first = String.sub s 0 1 in
  let last = String.sub s (len - 1) 1 in
  first ^ last

let extract_number s =
  let digits = String.fold_left filter "" s in
  match digits with "" -> 0 | _ -> first_last digits |> int_of_string

let solve filename extract_number =
  read_file filename |> split_str "\n" |> List.map extract_number |> sum

(* part 2 starts here *)

let extract_digit s =
  match s with
  | "1" | "one" -> 1
  | "2" | "two" -> 2
  | "3" | "three" -> 3
  | "4" | "four" -> 4
  | "5" | "five" -> 5
  | "6" | "six" -> 6
  | "7" | "seven" -> 7
  | "8" | "eight" -> 8
  | "9" | "nine" -> 9
  | _ -> 0

let left_substrings s n =
  let m = min n (String.length s) in
  let sub i = String.sub s 0 (i + 1) in
  List.init m Fun.id |> List.map sub

let right_substrings s n =
  let len = String.length s in
  let m = min n len in
  let sub i = String.sub s (len - i - 1) (i + 1) in
  List.init m Fun.id |> List.map sub

let rec _find_first (s : string) : int =
  let len = String.length s in
  let num = left_substrings s 5 |> List.map extract_digit |> sum in
  match num with 0 -> _find_first (String.sub s 1 (len - 1)) | _ -> num

let rec find_last (s : string) : int =
  let len = String.length s in
  let num = right_substrings s 5 |> List.map extract_digit |> sum in
  match num with 0 -> find_last (String.sub s 0 (len - 1)) | _ -> num

let extract_number_part2 s =
  let first = _find_first s in
  let last = find_last s in
  (10 * first) + last
;;

let () = assert (solve "sample.txt" extract_number = 142) in
let () = solve "input.txt" extract_number |> string_of_int |> print_endline in
let () = assert (solve "sample2.txt" extract_number_part2 = 281) in
solve "input.txt" extract_number_part2 |> string_of_int |> print_endline
