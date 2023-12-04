let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let sum = List.fold_left ( + ) 0
let prod = List.fold_left ( * ) 1
let string_of_char = String.make 1
let ( let* ) = Option.bind

let get_from_array arr h w i j =
  if j >= 0 && j < w && i >= 0 && i < h then Some (String.get arr (j + (i * w)))
  else None

let scan arr h w =
  let get = get_from_array arr h w in
  let is_marker i j =
    match get i j with Some ('0' .. '9' | '.') | None -> false | _ -> true
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
      let item = get i j |> Option.get in
      match item with
      | '0' .. '9' ->
          aux i (j + 1)
            (is_part_number || check i j)
            (part_number ^ string_of_char item)
            acc
      | _ -> aux i (j + 1) false "" new_acc
  in
  aux 0 0 false "" 0

let solve file scan =
  let lines = read_file file |> split_str "\n" in
  let h = List.length lines in
  let w = List.nth lines 0 |> String.length in
  let arr = lines |> String.concat "" in
  scan arr h w

(* -- part 2 -- *)

let scan2 arr h w =
  let comp a b =
    let tie = compare (fst a) (fst b) in
    if tie = 0 then compare (snd a) (snd b) else tie
  in
  let unique = List.sort_uniq comp in
  let get = get_from_array arr h w in
  let get_star i j =
    let* item = get i j in
    match item with '*' -> Some (i, j) | _ -> None
  in
  let get_stars i j =
    [
      get_star (i - 1) (j - 1);
      get_star (i - 1) j;
      get_star (i - 1) (j + 1);
      get_star (i + 1) (j - 1);
      get_star (i + 1) j;
      get_star (i + 1) (j + 1);
      get_star i (j - 1);
      get_star i (j + 1);
    ]
    |> List.filter_map Fun.id
  in
  let rec aux i j stars part_number acc =
    let update_acc acc =
      let inner acc' a = (a, int_of_string part_number) :: acc' in
      unique stars |> List.fold_left inner acc
    in
    if i >= h then acc
    else if j >= w then aux (i + 1) 0 [] "" (update_acc acc)
    else
      let item = get i j |> Option.get in
      match item with
      | '0' .. '9' ->
          aux i (j + 1)
            (List.concat [ stars; get_stars i j ])
            (part_number ^ string_of_char item)
            acc
      | _ -> aux i (j + 1) [] "" (update_acc acc)
  in
  let part_numbers = aux 0 0 [] "" [] in
  let stars = part_numbers |> List.map fst |> unique in
  let star_value star =
    let part_numbers_filtered =
      List.filter (fun a -> fst a = star) part_numbers
    in
    if List.length part_numbers_filtered = 2 then
      part_numbers_filtered |> List.map snd |> prod
    else 0
  in
  stars |> List.map star_value |> sum
;;

let () = assert (solve "sample.txt" scan = 4361) in
let () = assert (solve "sample.txt" scan2 = 467835) in
let () = solve "input.txt" scan |> string_of_int |> print_endline in
let () = solve "input.txt" scan2 |> string_of_int |> print_endline in
print_endline ""
