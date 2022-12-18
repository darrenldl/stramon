(* let normalize_path ~(cwd : string) (path : string) : string option =
  let rec aux acc parts =
    match parts with
    | [] -> Some (List.rev acc)
    | x :: xs ->
      match x with
      | "." | "" -> aux acc xs
      | ".." -> (
          match acc with
          | [] -> None
          | _ :: ys ->
            aux ys xs
        )
      | _ -> aux (x :: acc) xs
  in
  let parts = String_utils.escaping_split ~on:'/' path in
  let parts =
    if path.[0] = '/' then parts
    else
      String_utils.escaping_split ~on:'/' cwd @ parts
  in
  aux [] parts
  |> Option.map String_utils.concat_file_names
  |> Option.map (fun s -> "/" ^ s) *)
