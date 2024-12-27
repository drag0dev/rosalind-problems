open Shared.Utils
open Base

let find_all_matches text pattern =
  let kmp = String.Search_pattern.create pattern in

  let rec find_matches pos =
    match String.Search_pattern.index ~in_:text kmp ~pos:pos with
    | None -> ()
    | Some found_pos ->
            Stdlib.Printf.printf "%d " (found_pos+1);
            find_matches (found_pos + 1) in

  find_matches 0;;

let (motif, dna) = match read_lines "input" with
    | dna :: motif :: [] -> (dna, motif)
    | _ -> failwith "malformed input" in
find_all_matches dna motif;;
