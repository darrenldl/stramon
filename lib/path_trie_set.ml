type t = {
  children : t String_map.t;
  is_terminal : bool;
}

let empty : t = { children = String_map.empty; is_terminal = false }

let check_path path =
  if String.length path = 0 || path.[0] <> '/' then
    invalid_arg "expected absolute and non-empty path"

let parts_of_path path =
  if path = "/" then []
  else (
    List.tl @@ String_utils.escaping_split ~on:'/' path
  )

let add (path : string) (t : t) : t =
  let rec aux t parts =
    match parts with
    | [] | [""] -> { t with is_terminal = true }
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
  check_path path;
  aux t (parts_of_path path)

let mem (path : string) (t : t) : bool =
  let rec aux t parts =
    match parts with
    | [] | [""] -> true
    | x :: xs ->
      match String_map.find_opt x t.children with
      | None -> false
      | Some t ->
        aux t xs
  in
  check_path path;
  aux t (parts_of_path path)

let to_seq (t : t) : string Seq.t =
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
