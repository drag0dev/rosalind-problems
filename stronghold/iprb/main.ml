open Shared.Utils;;

let line = match read_lines "input" with
    | line :: [] -> line
    | _ -> failwith "malformed input" in

let (k, m, n) = match String.split_on_char ' ' line with
    | k :: m :: n :: [] ->
            let k = float_of_string k in
            let m = float_of_string m in
            let n = float_of_string n in
            (k, m, n)
    | _ -> failwith "malformed input" in

let total_organisms = k +. m +. n in
let homod_homod = (k /. total_organisms) *. ((k -. 1.0) /. (total_organisms -. 1.0)) in
let homod_hetero = (k /. total_organisms) *. (m /. (total_organisms -. 1.0)) +. (m /. total_organisms) *. (k /. (total_organisms -. 1.0)) in
let homod_homor = (k /. total_organisms) *. (n /. (total_organisms -. 1.0)) +. (n /. total_organisms) *. (k /. (total_organisms -. 1.0)) in
let hetero_hetero = (m /. total_organisms) *. ((m -. 1.0) /. (total_organisms -. 1.0)) in
let hetero_homor = (m /. total_organisms) *. (n /. (total_organisms -. 1.0)) +. (n /. total_organisms) *. (m /. (total_organisms -. 1.0)) in
let prob = 1.0 *. homod_homod +. 1.0 *. homod_hetero +. 1.0 *. homod_homor +. 0.75 *. hetero_hetero +. 0.5 *. hetero_homor in
Printf.printf "%f\n" prob;;

