let read_lines filename =
    let channel = open_in filename in

    let try_read () =
        try Some (input_line channel)
        with End_of_file -> None in

    let rec loop acc = match try_read () with
    | Some line -> loop (line :: acc)
    | None ->
            close_in channel;
            acc in
    loop [];;

let list_drop_first l =
    match l with
    | _h :: t -> t
    | _ -> failwith "empty list";;

let rec do_process_fasta_format input dna_acc acc =
    match input with
    | [] ->
                if List.length dna_acc > 0 then
                    let dna_acc = List.rev dna_acc in
                    String.concat "" dna_acc :: acc
                else acc
    | h :: t ->
            let (acc, dna_acc) = if String.starts_with ~prefix:">" h then
                let dna = String.concat "" dna_acc in
                let h = String.to_seq h |> List.of_seq |> list_drop_first |> List.to_seq |> String.of_seq in
                let acc = h :: dna :: acc in
                (acc, [])
            else
                let dna_acc = h :: dna_acc in
                (acc, dna_acc) in

            do_process_fasta_format t dna_acc acc;;

let process_fasta_format input =
    do_process_fasta_format input [] [];;
