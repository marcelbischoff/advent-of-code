let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let sum = List.fold_left ( + ) 0

let check_color color =
  let splits = split_str " " color in
  let color = List.nth splits 1 in
  let count = List.nth splits 0 |> int_of_string in
  match color with
  | "red" -> count <= 12
  | "green" -> count <= 13
  | "blue" -> count <= 14
  | _ -> false

let parse_rgb s =
  let colors = split_str ", " s in
  colors |> List.for_all check_color

let parse_rgbs s =
  let items = split_str "; " s in
  items |> List.for_all parse_rgb

let parse_line s =
  let splits = split_str ": " s in
  let first = List.nth splits 0 |> split_str "Game " in
  let game = List.nth first 0 |> int_of_string in
  let second = List.nth splits 1 in
  let okay = parse_rgbs second in
  match okay with true -> game | false -> 0

let solve filename parse_line =
  read_file filename |> split_str "\n" |> List.map parse_line |> sum

(* -- part 2 -- *)

type _rgb_cnt = { red : int; green : int; blue : int }

let rec consume lst cnts =
  match lst with
  | [] -> cnts.red * cnts.green * cnts.blue
  | rgb :: tail -> (
      let splits = split_str " " rgb in
      let color = List.nth splits 1 in
      let count = List.nth splits 0 |> int_of_string in
      match color with
      | "red" ->
          consume tail
            { red = max count cnts.red; green = cnts.green; blue = cnts.blue }
      | "green" ->
          consume tail
            { red = cnts.red; green = max count cnts.green; blue = cnts.blue }
      | "blue" ->
          consume tail
            { red = cnts.red; green = cnts.green; blue = max count cnts.blue }
      | _ -> 0)

let parse_line2 s =
  let splits = split_str ": " s in
  let rgbs = List.nth splits 1 |> split_str "[,;] " in
  consume rgbs { red = 0; green = 0; blue = 0 }
;;

let () = assert (solve "sample.txt" parse_line = 8) in
let () = solve "input.txt" parse_line |> string_of_int |> print_endline in
let () = assert (solve "sample.txt" parse_line2 = 2286) in
solve "input.txt" parse_line2 |> string_of_int |> print_endline
