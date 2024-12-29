open Shared;;

let amino_acid_to_weight aa =
    match aa with
    |'A' -> 71.03711
    |'C' -> 103.00919
    |'D' -> 115.02694
    |'E' -> 129.04259
    |'F' -> 147.06841
    |'G' -> 57.02146
    |'H' -> 137.05891
    |'I' -> 113.08406
    |'K' -> 128.09496
    |'L' -> 113.08406
    |'M' -> 131.04049
    |'N' -> 114.04293
    |'P' -> 97.05276
    |'Q' -> 128.05858
    |'R' -> 156.10111
    |'S' -> 87.03203
    |'T' -> 101.04768
    |'V' -> 99.06841
    |'W' -> 186.07931
    |'Y' -> 163.06333
    | _ -> failwith "unexpected amino acid" ;;

let protein = match Utils.read_lines "input" with
    | protein :: [] -> protein
    | _ -> failwith "malformed input" in
let protein = protein |> String.to_seq |> List.of_seq in
let masses = List.map (fun aa -> amino_acid_to_weight aa) protein in
let res = List.fold_left (fun acc mass -> acc +. mass) 0.0 masses in
Printf.printf "%f\n" res;;
