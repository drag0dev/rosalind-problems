open Shared.Utils;;

let line = match read_lines "input" with
    | line :: [] -> line
    | _ -> failwith "exepected only one line" in
let line = line |> String.to_seq |> List.of_seq |> List.rev in
let line = List.map (fun c ->
    match c with
    | 'A' -> 'T'
    | 'C' -> 'G'
    | 'T' -> 'A'
    | 'G' -> 'C'
    | _ -> failwith "unexpected letter") line |> List.to_seq |> String.of_seq in
Printf.printf "%s\n" line;;
