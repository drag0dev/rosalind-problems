open Shared.Utils;;

let (first, second) = match read_lines "input" with
    | first :: second :: [] -> (first, second)
    | _ -> failwith "malformed input" in
let first = first |> String.to_seq |> List.of_seq in
let second = second |> String.to_seq |> List.of_seq in
let diff = List.fold_left2 (fun acc left right -> if left != right then acc + 1 else acc) 0 first second in
Printf.printf "%d\n" diff;;
