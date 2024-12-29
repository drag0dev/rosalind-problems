open Shared;;


let rec map_protein_aa_to_possible protein acc =
    match protein with
    | [] -> acc
    | h :: t ->
        let number_of_possible = match h with
            | 'F' -> 2
            | 'L' -> 6
            | 'S' -> 6
            | 'Y' -> 2
            | 'C' -> 2
            | 'W' -> 1
            | 'P' -> 4
            | 'H' -> 2
            | 'Q' -> 2
            | 'R' -> 6
            | 'I' -> 3
            | 'M' -> 1
            | 'T' -> 4
            | 'N' -> 2
            | 'K' -> 2
            | 'V' -> 4
            | 'A' -> 4
            | 'D' -> 2
            | 'E' -> 2
            | 'G' -> 4
            | _ -> failwith "unexpected amino acid" in
        let acc = number_of_possible :: acc in
        map_protein_aa_to_possible t acc;;


let protein = match Utils.read_lines "input" with
    | protein :: [] -> protein
    | _ -> failwith "malformed input" in
let protein = protein |> String.to_seq |> List.of_seq in
let possible = map_protein_aa_to_possible protein [] in
let res = List.fold_left (fun acc possible -> (acc * possible) mod 1_000_000) 1 possible in
let res = (res * 3) mod 1_000_000 in (* account for three stop codons *)
Printf.printf "%d\n" res;;
