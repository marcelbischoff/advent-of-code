let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let nth n lst = List.nth lst n
let seq_min = Seq.fold_left min Int.max_int
let get_sections filename = read_file filename |> split_str "\n\n"

type entry = { name : string; start : int; len : int }

type rule = {
  source_name : string;
  target_name : string;
  target_start : int;
  source_start : int;
  len : int;
}

let explode_intervals (source_name : string) (target_name : string) (s : string)
    : rule =
  let res =
    match s |> split_str " " |> List.map int_of_string with
    | [ target_start; source_start; len ] ->
        { source_name; target_name; target_start; source_start; len }
    | _ -> failwith "explode_error"
  in
  res

let parse_section (section : string) : rule list =
  match section |> split_str "\n" with
  | [] -> failwith "section wrong"
  | _maptype :: maps_raw -> (
      match _maptype |> split_str " map:" |> nth 0 |> split_str "-to-" with
      | [ source; target ] ->
          maps_raw |> List.map (explode_intervals source target)
      | _ -> failwith "parse error")

let parse_section_def section =
  match section |> split_str "\n" with
  | [] -> failwith "section wrong"
  | _maptype :: _ -> (
      match _maptype |> split_str " map:" |> nth 0 |> split_str "-to-" with
      | [ source; target ] -> (source, target)
      | _ -> failwith "parse error")

let parse_seeds seeds_raw =
  seeds_raw |> split_str "seeds: " |> nth 0 |> split_str " "
  |> List.map (fun x -> { name = "seed"; start = int_of_string x; len = 1 })
  |> List.to_seq

let solve sections parse_seeds =
  match sections with
  | [] -> failwith "section wrong"
  | seeds_raw :: maps_raw ->
      let start = parse_seeds seeds_raw in
      let maps : rule list =
        maps_raw |> List.map parse_section |> List.concat
      in
      let default_maps : (string * string) list =
        maps_raw |> List.map parse_section_def
      in
      let map (source : entry) : entry =
        let m = maps |> List.filter (fun x -> x.source_name = source.name) in
        let rec aux source maps =
          match maps with
          | h :: _
            when source.start >= h.source_start
                 && source.start < h.source_start + h.len ->
              let offset = source.start - h.source_start in
              {
                name = h.target_name;
                start = h.target_start + offset;
                len = min source.len (h.len - offset);
              }
          | _ :: tail -> aux source tail
          | [] ->
              let new_len =
                m
                |> List.map (fun x -> x.source_start- source.start)
                |> List.filter (fun x -> x>0)
                |> List.to_seq |> seq_min
              in
              {
                name = List.assoc source.name default_maps;
                start = source.start;
                len = min source.len new_len;
              }
        in
        aux source m
      in
      let rec play_ent (source : entry) (curr : entry) : int =
        if curr.name = "location" then
          if curr.len = source.len then curr.start
          else
            let new_source =
              {
                name = "seed";
                start = source.start + curr.len;
                len = source.len - curr.len;
              }
            in
            play_ent new_source new_source |> min curr.start
        else
          let target = map curr in
          target |> play_ent source
      in
      start |> Seq.map (fun src -> play_ent src src) |> seq_min

let parse_seeds2 seeds_raw =
  let nums =
    seeds_raw |> split_str "seeds: " |> nth 0 |> split_str " "
    |> List.map int_of_string
  in
  let rec aux nums acc =
    match nums with
    | start :: len :: tail -> aux tail ({ name = "seed"; start; len } :: acc)
    | [] -> acc
    | _ -> failwith "parse error"
  in
  aux nums [] |> List.to_seq
;;

let sample_sections = get_sections "sample.txt" in
let sections = get_sections "input.txt" in
let () = assert (solve sample_sections parse_seeds = 35) in
let () = solve sections parse_seeds |> Printf.printf "part1: %d\n" in
let () = assert (solve sample_sections parse_seeds2 = 46) in
let () = solve sections parse_seeds2 |> Printf.printf "part2: %d\n" in
print_endline ""
