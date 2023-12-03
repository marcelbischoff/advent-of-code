let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let _sum = List.fold_left ( + ) 0

let scan mat h w =
  let char_of_string s = String.get s 0 in
  let get i j =
    let () = assert (j >= 0 && j < w) in
    let () = assert (i >= 0 && i < h) in
    List.nth (List.nth mat i) j
  in
  let _foo = get 9 2 in
  let is_marker i j =
    if i < 0 || i >= h || j < 0 || j >= w then false
    else
      match get i j |> char_of_string with
      | '0' .. '9' | '.' -> false
      | _ -> true
  in
  let check i j =
    is_marker (i - 1) (j - 1)
    || is_marker (i - 1) j
    || is_marker (i - 1) (j + 1)
    || is_marker (i + 1) (j - 1)
    || is_marker (i + 1) j
    || is_marker (i + 1) (j + 1)
    || is_marker i (j - 1)
    || is_marker i (j + 1)
  in
  let rec aux i j is_part_number part_number acc =
    let new_acc =
      if is_part_number then acc + int_of_string part_number else acc
    in
    if i >= h then acc
    else if j >= w then aux (i + 1) 0 false "" new_acc
    else
      let item = get i j in
      match char_of_string item with
      | '0' .. '9' ->
          aux i (j + 1) (is_part_number || check i j) (part_number ^ item) acc
      | _ -> aux i (j + 1) false "" new_acc
  in
  aux 0 0 false "" 0

let solve file =
  let lines = read_file file |> split_str "\n" in
  let h = List.length lines in
  let mat = lines |> List.map (split_str "") in
  let w = List.nth mat 0 |> List.length in
  (*
  let () = "h = " ^ string_of_int h |> print_endline in
  let () = "w = " ^ string_of_int w |> print_endline in
  let () =
    mat |> List.map (String.concat " ") |> String.concat "\n" |> print_endline
  in
  *)
  scan mat h w
;;

let () = print_endline "" in
let () = assert (solve "sample.txt" = 4361) in
let () = solve "input.txt" |> string_of_int |> print_endline in
print_endline ""
