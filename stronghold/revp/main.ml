open Shared;;

let rec check_reverse_palindrom strand acc idx len total_len =
    match idx+len > total_len || len > 12 with
    | true -> acc
    | false ->
        let current = String.sub strand idx len in
        let complement = current |> String.to_seq |> Seq.map (fun letter ->
            match letter with
            | 'A' -> 'T'
            | 'T' -> 'A'
            | 'C' -> 'G'
            | 'G' -> 'C'
            | _ -> assert false
        ) |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq in
        let acc = match current = complement with
            | false -> acc
            | true -> (idx+1, len) :: acc in
        check_reverse_palindrom strand acc idx (len+1) total_len;;

let rec find_reverse_palindroms strand acc idx total_len =
    match idx >= total_len with
    | true -> acc
    | false ->
            let acc = check_reverse_palindrom strand acc idx 4 total_len in
            find_reverse_palindroms strand acc (idx+1) total_len;;

let lines = Utils.read_lines "input" in
let fasta = Utils.process_fasta_format lines in
let strand = match fasta with
    | _name :: strand :: [] -> strand
    | _ -> failwith "malformed input" in
let total_len = String.length strand in
let reverse_palindroms = find_reverse_palindroms strand [] 0 total_len in

List.iter (fun (start, len) -> Printf.printf "%d %d\n" start len) reverse_palindroms;;
