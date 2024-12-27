open Shared.Utils;;

let rec split_into_threes input acc =
    match input with
    | [] -> List.rev acc
    | one :: two :: three :: t ->
            let part = Buffer.create 3 in
            Buffer.add_char part one;
            Buffer.add_char part two;
            Buffer.add_char part three;
            let part = Buffer.contents part in
            let acc = part :: acc in
            split_into_threes t acc
    | _ -> failwith "malformed input";;

let encode_to_protein dna_part =
    match dna_part with
    | "UUU" -> "F"
    | "UUC" -> "F"
    | "UUA" -> "L"
    | "UUG" -> "L"
    | "UCU" -> "S"
    | "UCC" -> "S"
    | "UCA" -> "S"
    | "UCG" -> "S"
    | "UAU" -> "Y"
    | "UAC" -> "Y"
    | "UAA" -> ""
    | "UAG" -> ""
    | "UGU" -> "C"
    | "UGC" -> "C"
    | "UGA" -> ""
    | "UGG" -> "W"
    | "CUU" -> "L"
    | "CUC" -> "L"
    | "CUA" -> "L"
    | "CUG" -> "L"
    | "CCU" -> "P"
    | "CCC" -> "P"
    | "CCA" -> "P"
    | "CCG" -> "P"
    | "CAU" -> "H"
    | "CAC" -> "H"
    | "CAA" -> "Q"
    | "CAG" -> "Q"
    | "CGU" -> "R"
    | "CGC" -> "R"
    | "CGA" -> "R"
    | "CGG" -> "R"
    | "AUU" -> "I"
    | "AUC" -> "I"
    | "AUA" -> "I"
    | "AUG" -> "M"
    | "ACU" -> "T"
    | "ACC" -> "T"
    | "ACA" -> "T"
    | "ACG" -> "T"
    | "AAU" -> "N"
    | "AAC" -> "N"
    | "AAA" -> "K"
    | "AAG" -> "K"
    | "AGU" -> "S"
    | "AGC" -> "S"
    | "AGA" -> "R"
    | "AGG" -> "R"
    | "GUU" -> "V"
    | "GUC" -> "V"
    | "GUA" -> "V"
    | "GUG" -> "V"
    | "GCU" -> "A"
    | "GCC" -> "A"
    | "GCA" -> "A"
    | "GCG" -> "A"
    | "GAU" -> "D"
    | "GAC" -> "D"
    | "GAA" -> "E"
    | "GAG" -> "E"
    | "GGU" -> "G"
    | "GGC" -> "G"
    | "GGA" -> "G"
    | "GGG" -> "G"
    | _ -> failwith "unexpected dna part";;

let dna = match read_lines "input" with
    | dna :: [] -> dna
    | _ -> failwith "malformed input";;
let dna = dna |> String.to_seq |> List.of_seq in
let dna = split_into_threes dna [] in
let protein = List.map (fun part -> encode_to_protein part) dna in
let protein = List.fold_left (fun acc part -> acc ^ part ) "" protein in
Printf.printf "%s\n" protein;;
