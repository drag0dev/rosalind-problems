open Shared;;

let rec build_sets alphabet n current acc =
    match n < 0 with
    | true -> acc
    | false ->
            let acc = current :: acc in
            List.fold_left (fun acc letter -> build_sets alphabet (n-1) (letter :: current) acc) acc alphabet;;

module AlphabetOrder = Map.Make(Char);;
let rec generate_alphabet_order alphabet idx acc =
    match alphabet with
    | [] -> acc
    | h :: t ->
            let letter = h |> String.to_seq |> List.of_seq in
            let letter = List.nth letter 0 in
            let acc = AlphabetOrder.add letter idx acc in
            generate_alphabet_order t (idx+1) acc;;

let compare_sets alphabet left right =
    let rec compare_sests_aux alphabet left right =
        match (left, right) with
        | ([], []) -> 0
        | (_::_, []) -> 1
        | ([], _::_) -> -1
        | (left_h :: left_t, right_h :: right_t) ->
                let left_order_num = AlphabetOrder.find left_h alphabet in
                let right_order_num = AlphabetOrder.find right_h alphabet in
                match left_order_num = right_order_num with
                | true -> compare_sests_aux alphabet left_t right_t
                | false -> compare left_order_num right_order_num in

    let left = left |> String.to_seq |> List.of_seq in
    let right = right |> String.to_seq |> List.of_seq in
    compare_sests_aux alphabet left right;;

let lines = Utils.read_lines "input" in
let lines = List.rev lines in
let (alphabet, n) = match lines with
    | alphabet :: n :: [] ->
            let alphabet = String.split_on_char ' ' alphabet in
            let n = int_of_string n in
            (alphabet, n)
    | _ -> failwith "malformed input" in
let alpahabet_order = generate_alphabet_order alphabet 0 AlphabetOrder.empty in
let subsets = build_sets alphabet n [] [] in
let subsets = List.map (fun subset -> List.fold_left (fun acc letter -> acc ^ letter) "" subset) subsets in
let subsets = List.sort (fun left right -> compare_sets alpahabet_order left right) subsets in
let subsets = match subsets with | _first :: t -> t | _ -> failwith "malformed subsets" in
let oc = open_out "output" in
List.iter (fun subset -> Printf.fprintf oc "%s\n" subset) subsets;
close_out oc;
