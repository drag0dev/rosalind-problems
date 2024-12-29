open Shared;;

let line = match Utils.read_lines "input" with
    | line :: [] -> line
    | _ -> failwith "malformed input" in
let (homod_homod, homod_hetero, homod_homor, hetero_hetero, hetero_homor, _homor_homor) = match String.split_on_char ' ' line with
    | homod_homod :: homod_hetero :: homod_homor :: hetero_hetero :: hetero_homor :: homor_homor :: [] ->
            let homod_homod = float_of_string homod_homod in
            let homod_hetero = float_of_string homod_hetero in
            let homod_homor = float_of_string homod_homor in
            let hetero_hetero = float_of_string hetero_hetero in
            let hetero_homor = float_of_string hetero_homor in
            let homor_homor = float_of_string homor_homor in
            (homod_homod, homod_hetero, homod_homor, hetero_hetero, hetero_homor, homor_homor)
    | _ -> failwith "malformed input" in
let res = 2.0 *. homod_homod +. 2.0 *. homod_hetero +. 2.0 *. homod_homor +. 2.0 *. 0.75 *. hetero_hetero +. 1.0 *. hetero_homor in
Printf.printf "%f\n" res;;
