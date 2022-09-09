type 'a t = {
  value : 'a option;
  children : 'a t String_map.t;
}

let empty : 'a t =
  {
    value = None;
  children = String_map.empty;
}

let add (path : Abs_path.t) (v : 'a) (t : 'a t) : 'a t =
  let rec aux t parts =
    match parts with
    | [] | [""] -> { t with value = Some v }
    | x :: xs ->
      let children =
        String_map.find_opt x t.children
        |> Option.value ~default:empty
        |> (fun sub_trie ->
            String_map.add x (aux sub_trie xs) t.children
          )
      in
      { t with children }
  in
  aux t (Abs_path.parts path)

let find (path : Abs_path.t) (t : 'a t) : 'a option =
  let rec aux t parts =
    match parts with
    | [] | [""] -> t.value
    | x :: xs ->
      match String_map.find_opt x t.children with
      | None -> None
      | Some t -> aux t xs
  in
  aux t (Abs_path.parts path)

(* let to_seq (t : t) : string Seq.t =
  let rec aux (t : t) : string list Seq.t =
    String_map.to_seq t.children
    |> Seq.flat_map (fun (k, v) ->
        Seq.map (fun l ->
            k :: l
          )
          (aux v)
      )
    |> (fun s ->
        if t.is_terminal then
          Seq.cons [] s
        else
          s
      )
  in
  aux t
  |> Seq.map (fun l ->
      String_utils.concat_file_names ("/" :: l)
    )
*)
