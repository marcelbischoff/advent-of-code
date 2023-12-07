let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
let nth n lst = List.nth lst n
let seq_min = Seq.fold_left min Int.max_int
let get_sections filename = read_file filename |> split_str "\n\n"

type entry = { name : string; num : int }
type rule = { source_name: string;
    target_name: string;
    target_start: int;
    source_start: int;
    len: int
}
    

let explode_intervals (source_name : string) (target_name : string) (s : string) : rule =
    let res = match s |> split_str " " |> List.map int_of_string with
  | [ target_start; source_start; len ] -> { source_name= source_name; target_name= target_name; target_start = target_start; source_start = source_start; len =len}
  | _ -> failwith "explode_error"
    in res

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
        |> List.map (fun x -> { name = "seed"; num = int_of_string x })
        |> List.to_seq

let solve sections parse_seeds =
  match sections with
  | [] -> failwith "section wrong"
  | seeds_raw :: maps_raw ->
      let start = parse_seeds seeds_raw 
      in
      let maps : rule list =
        maps_raw |> List.map parse_section |> List.concat
      in
      let default_maps : (string * string) list =
        maps_raw |> List.map parse_section_def
      in
      (*
      let _x = default_maps |> List.map (fun x -> Printf.printf "dm: %s %s\n" (fst x) (snd x)) in
        *)
      let map (source : entry) : entry =
        let m = maps |>List.filter (fun x -> (x.source_name = source.name)) in
        let rec aux source maps = 
            match maps with
            | h :: _ when source.num >= h.source_start && source.num < (h.source_start + h.len) -> 
                    {name = h.target_name; num = h.target_start + (source.num - h.source_start)}
            | _:: tail -> aux source tail
            | [] -> { name = List.assoc source.name  default_maps; num = source.num }
        in
        aux source m 
      in
      let rec play_ent source =
        if source.name = "location" then source.num else map source |> play_ent
      in
      start |> Seq.map play_ent |> seq_min

let parse_seeds2 seeds_raw = 
    let nums = seeds_raw |> split_str "seeds: " |> nth 0 |> split_str " " |> List.map int_of_string in
    let rec aux nums acc = 
        match nums with 
        | start:: len:: tail -> (
            let new_seeds = Seq.init len (fun x -> x + start) 
            |> Seq.map (fun x -> { name = "seed"; num =  x }) in 
            aux tail (Seq.append acc new_seeds)
        )
        | [] -> acc
        | _ -> failwith "dump"
    in
    aux nums ([] |> List.to_seq)
;;


let sample_sections = get_sections "sample.txt" in
let sections = get_sections "input.txt" in
let () = assert (solve sample_sections parse_seeds = 35) in
let () = solve sections parse_seeds |> Printf.printf "\npart1: %d\n" in
let () = assert (solve sample_sections parse_seeds2 = 46) in
let () = print_endline "---" in
let () = solve sections parse_seeds2 |> Printf.printf "\n\nsample2: %d\n" in
print_endline ""
