open Shared.Utils;;

let lines = read_lines "input" in
let line = match lines with
    | h :: [] -> h
    | _ -> failwith "expected only one line" in
let line = String.to_seq line |> List.of_seq in

let (a, c, g, t) = List.fold_left (fun acc c ->
    let (a_count, c_count, g_count, t_count) = acc in
    match c with
    | 'A' -> (a_count+1, c_count, g_count, t_count)
    | 'C' -> (a_count, c_count+1, g_count, t_count)
    | 'G' -> (a_count, c_count, g_count+1, t_count)
    | 'T' -> (a_count, c_count, g_count, t_count+1)
    | _ -> failwith "unexpected letter in the DNA"
    ) (0, 0, 0, 0) line in

Printf.printf "%d %d %d %d\n" a c g t;;
