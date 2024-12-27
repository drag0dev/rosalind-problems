open Shared.Utils;;

let rec get_gc_count dna acc =
    match dna with
    | [] -> acc
    | h :: t ->
            let acc = match h with
            | 'A' -> acc
            | 'C' -> acc + 1
            | 'G' -> acc + 1
            | 'T' -> acc
            | c ->
                    Printf.printf "%c\n" c;
                    failwith "unexpected letter in dna" in
            get_gc_count t acc;;

let gc_content dna =
    let dna = dna |> String.to_seq |> List.of_seq in
    let len = List.length dna in
    let len = float_of_int len in
    let gc_count = get_gc_count dna 0 in
    let gc_count = float_of_int gc_count in
    (gc_count /. len) *. 100.0;;

let rec find_max_gc_content input acc =
    match input with
    | [] -> acc
    | name :: dna :: t ->
            let (_, max_content) = acc in
            let content = gc_content dna in
            let acc =
                if content > max_content then (name, content)
                else acc in
            find_max_gc_content t acc
    | _ -> failwith "malformed input";;

let list_drop_first l =
    match l with
    | _h :: t -> t
    | _ -> failwith "empty list";;

let rec process_input input dna_acc acc =
    match input with
    | [] ->
            let acc =
                if List.length dna_acc > 0 then
                    let dna_acc = List.rev dna_acc in
                    String.concat "" dna_acc :: acc
                else acc in

            let acc = List.rev acc in
            list_drop_first acc
    | h :: t ->
            let (acc, dna_acc) = if String.starts_with ~prefix:">" h then
                let dna_acc = List.rev dna_acc in
                let dna = String.concat "" dna_acc in
                let h = String.to_seq h |> List.of_seq |> list_drop_first |> List.to_seq |> String.of_seq in
                let acc = h :: dna :: acc in
                (acc, [])
            else
                let dna_acc = h :: dna_acc in
                (acc, dna_acc) in

            process_input t dna_acc acc;;

let lines = read_lines "input" in
let lines = List.rev lines in
let lines = process_input lines [] [] in
let (name, content) = find_max_gc_content lines ("", 0.0) in
Printf.printf "%s\n%f\n" name content;;
