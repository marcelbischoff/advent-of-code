let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let sum = List.fold_left ( + ) 0

let conpute_part1 first second =
  List.combine first second |> List.map (fun (x, y) -> x - y |> abs) |> sum

let solve filename func =
  let data =
    read_file filename |> split_str "\n"
    |> List.map (split_str " ")
    |> List.map (List.filter_map int_of_string_opt)
  in
  let extract_sort data n =
    data |> List.filter_map (fun x -> List.nth_opt x n) |> List.sort compare
  in
  func (extract_sort data 0) (extract_sort data 1)

let conpute_part2 first second =
  let mult i = second |> List.map (fun x -> if x == i then 1 else 0) |> sum in
  first |> List.map (fun x -> x * mult x) |> sum
;;

let () = assert (solve "sample.txt" conpute_part1 = 11) in
let () = assert (solve "sample.txt" conpute_part2 = 31) in
let () = solve "input.txt" conpute_part1 |> string_of_int |> print_endline in
solve "input.txt" conpute_part2 |> string_of_int |> print_endline
