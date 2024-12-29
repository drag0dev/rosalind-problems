open Shared;;

let rec fact_limit n acc limit mmod =
    match n < 1 || n <= limit with
    | true -> acc
    | false ->
            let acc = Big_int.mult_int_big_int n acc in
            let acc = Big_int.mod_big_int acc mmod in
            fact_limit (n-1) acc limit mmod;;

let line = match Utils.read_lines "input" with
    | line :: [] -> line
    | _ -> failwith "malformed input" in
let (n, k) = match String.split_on_char ' ' line with
    | n :: k :: [] ->
            let n = int_of_string n in
            let k = int_of_string k in
            (n, k)
    | _ -> failwith "malformed input" in

let acc = Big_int.big_int_of_int 1 in
let mmod = Big_int.big_int_of_int 1_000_000 in
let variations = fact_limit n acc (n-k) mmod in
Printf.printf "%s\n" (Big_int.string_of_big_int variations);;
