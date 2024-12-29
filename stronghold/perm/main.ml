open Shared;;

let rec fact n acc =
    match n < 1 with
    | true -> acc
    | false ->
            fact (n-1) (acc*n);;

let rec insert x lst =
  match lst with
  | [] -> [[x]]
  | h::r ->
          (x::lst) :: (List.map (fun l -> h::l) (insert x r));;

let rec generate_permutations = function
  | [] -> [[]]
  | h::t ->
          List.flatten (List.map (insert h) (generate_permutations t));;

let rec print_list l =
    match l with
    | [] -> ()
    | h :: t ->
            Printf.printf "%d " h;
            print_list t;;

let line = match Utils.read_lines "input" with
    | line :: [] -> line
    | _ -> failwith "malformed input" in
let n = int_of_string line in
let total_perms = fact n 1 in
let initial_list = Core.List.range 1 (n+1) in
let permutations = generate_permutations initial_list in
Printf.printf "%d\n" total_perms;
List.iter (fun perm -> print_list perm; Printf.printf "\n";) permutations;
