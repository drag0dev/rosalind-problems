open Shared.Utils;;

let rec pairs_after_months months parents children offspring =
    match months with
    | 0 -> children
    | _ ->
            let new_children = parents in
            let parents = parents + children * offspring in
            pairs_after_months (months - 1) parents new_children offspring;;

let line = match read_lines "input" with
    | line :: [] -> line
    | _ -> failwith "expected only one line" in
let (months, offspring) = match String.split_on_char ' ' line with
    | months :: offspring :: [] -> (months, offspring)
    | _ -> failwith "malformed input" in
let months = int_of_string months in
let offspring = int_of_string offspring in
let pairs_count = pairs_after_months (months - 1) 1 1 offspring in
Printf.printf "%d\n" pairs_count;;
