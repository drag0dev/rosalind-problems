open Shared.Utils;;

let lines = read_lines "input" in
let line = match lines with
    | h :: [] -> h
    | _ -> failwith "expected only one line" in
let dna = String.to_seq line |> List.of_seq in
let rna = List.map (fun c -> if c = 'T' then 'U' else c) dna |> List.to_seq |> String.of_seq in
Printf.printf "%s\n" rna;;
