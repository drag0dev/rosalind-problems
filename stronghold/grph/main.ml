open Shared;;

let rec pair_up_fasta lines acc =
    match lines with
    | [] -> acc
    | name :: strand :: t ->
            let acc = (name, strand) :: acc in
            pair_up_fasta t acc
    | _ -> failwith "malformed input";;

let k_postfix_string s k =
    let rec list_take l n acc =
        match n <= 0 with
        | true -> List.rev acc
        | false ->
                match l with
                | h :: t ->
                        let acc = h :: acc in
                        list_take t (n-1) acc
                | _ -> failwith "not enough elements in the list" in

    let s = s |> String.to_seq |> List.of_seq |> List.rev in
    list_take s k [] |> List.rev |> List.to_seq |> String.of_seq;;

let rec find_matches strands prefix acc =
    match strands with
    | [] -> acc
    | (name, strand) :: t ->
            let acc = match String.starts_with ~prefix:prefix strand with
                | false -> acc
                | true ->
                        name :: acc in
            find_matches t prefix acc;;

let generate_graph strands k =
    List.map (fun pair ->
        let (name, strand) = pair in
        let postfix = k_postfix_string strand k in
        let matches = find_matches strands postfix [] in
        List.fold_left (fun acc m -> if m = name then acc else (name, m) :: acc ) [] matches
    ) strands
    |> List.flatten;;

let rec print_graph edges =
    match edges with
    | [] -> ()
    | (left, right) :: t ->
            Printf.printf "%s %s\n" left right;
            print_graph t;;

let lines = Utils.read_lines "input" in
let input = Utils.process_fasta_format lines in
let input = pair_up_fasta input [] in
let graph = generate_graph input 3 in
print_graph graph;;
