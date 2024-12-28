open Shared;;

let generate_freq_at_index fasta_input =
    let strand_len = match fasta_input with
        | _ :: dna :: _t -> String.length dna
        | _ -> failwith "malformed input" in
    let a = Array.make strand_len 0 in
    let c = Array.make strand_len 0 in
    let t = Array.make strand_len 0 in
    let g = Array.make strand_len 0 in

    let rec update_freq strand index a c t g =
        match strand with
        | [] -> ()
        | h :: rest ->
                let () = match h with
                | 'A' -> a.(index) <- a.(index) + 1
                | 'C' -> c.(index) <- c.(index) + 1
                | 'T' -> t.(index) <- t.(index) + 1
                | 'G' -> g.(index) <- g.(index) + 1
                | _ -> failwith "unexpected characted" in
                update_freq rest (index+1) a c t g in

    let rec update_freq_for_all input a c t g =
        match input with
        | [] -> ()
        | _name :: strand :: rest ->
                let strand = strand |> String.to_seq |> List.of_seq in
                let () = update_freq strand 0 a c t g in
                update_freq_for_all rest a c t g
        | _ -> failwith "malformed input" in

    let () = update_freq_for_all fasta_input a c t g in
    (a, c, t, g, strand_len);;

let rec list_max l max =
    match l with
    | [] -> max
    | (letter, num) :: t ->
            let (_max_letter, max_num) = max in
            let max = match num > max_num with
                | false -> max
                | true -> (letter, num) in
            list_max t max;;

let rec generate_signature a c t g idx len acc =
    match idx >= len with
    | true -> List.rev acc |> List.to_seq |> String.of_seq
    | false ->
            let a_count = a.(idx) in
            let a_count = ('A', a_count) in

            let c_count = c.(idx) in
            let c_count = ('C', c_count) in

            let t_count = t.(idx) in
            let t_count = ('T', t_count) in

            let g_count = g.(idx) in
            let g_count = ('G', g_count) in

            let (letter, _) = list_max [a_count; c_count; t_count; g_count] ('0', -1) in
            let acc = letter :: acc in
            generate_signature a c t g (idx+1) len acc;;

let rec print_list l =
    match l with
    | [] -> ()
    | h :: t ->
            Printf.printf " %d" h;
            print_list t;;

let lines = Utils.read_lines "input" |> Utils.process_fasta_format in
let (a, c, t, g, strand_len) = generate_freq_at_index lines in
let signature = generate_signature a c t g 0 strand_len [] in
Printf.printf "%s\n" signature;

Printf.printf "A:";
let a = Array.to_list a in
print_list a;
Printf.printf "\n";

Printf.printf "C:";
let c = Array.to_list c in
print_list c;
Printf.printf "\n";

Printf.printf "G:";
let g = Array.to_list g in
print_list g;
Printf.printf "\n";

Printf.printf "T:";
let t = Array.to_list t in
print_list t;
Printf.printf "\n";
