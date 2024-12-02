let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let sum = List.fold_left ( + ) 0

let solve filename =
  let data =
    read_file filename |> split_str "\n"
    |> List.map (split_str " ")
    |> List.map (List.filter_map int_of_string_opt)
  in
  let extract_sort data n =
    data |> List.filter_map (fun x -> List.nth_opt x n) |> List.sort compare
  in
  List.combine (extract_sort data 0) (extract_sort data 1)
  |> List.map (fun (x, y) -> x - y |> abs)
  |> sum
;;

let () = assert (solve "sample.txt" = 11) in
solve "input.txt" |> string_of_int |> print_endline
