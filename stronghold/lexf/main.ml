open Shared;;

let rec build_subset alphabet n current acc =
    match n with
    | 0 -> current :: acc
    | n ->
            List.fold_left (fun acc letter -> build_subset alphabet (n-1) (letter :: current) acc) acc alphabet;;

let lines = Utils.read_lines "input" in
let lines = List.rev lines in
let (alphabet, n) = match lines with
    | alphabet :: n :: [] ->
            let alphabet = String.split_on_char ' ' alphabet in
            let n = int_of_string n in
            (alphabet, n)
    | _ -> failwith "malformed input" in

let subsets = build_subset alphabet n [] [] in
let subsets = List.map (fun subset -> List.fold_left (fun acc letter -> acc ^ letter) "" subset) subsets in
let subsets = List.sort (fun left right -> String.compare left right) subsets in
List.iter (fun subset -> Printf.printf "%s\n" subset) subsets;;
