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
    | [] -> { t with value = Some v }
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
  aux t (Abs_path.to_parts path)

let remove (path : Abs_path.t) (t : 'a t) : 'a t =
  let rec aux t parts =
    match parts with
    | [] -> { t with value = None }
    | x :: xs -> (
        match String_map.find_opt x t.children with
        | None -> t
        | Some sub_trie ->
          let children =
            String_map.add x (aux sub_trie xs) t.children
          in
          { t with children }
      )
  in
  aux t (Abs_path.to_parts path)

let find (path : Abs_path.t) (t : 'a t) : 'a option =
  let rec aux t parts =
    match parts with
    | [] -> t.value
    | x :: xs ->
      match String_map.find_opt x t.children with
      | None -> None
      | Some t -> aux t xs
  in
  aux t (Abs_path.to_parts path)

let find_exn path t =
  match find path t with
  | None -> invalid_arg "find_exn: Path does not exist"
  | Some x -> x

let to_seq (t : 'a t) : (Abs_path.t * 'a) Seq.t =
  let rec aux (t : 'a t) : (string list * 'a) Seq.t =
    let sub_tries_seq =
      String_map.to_seq t.children
      |> Seq.flat_map (fun (k, sub_trie) ->
          Seq.map (fun (l, v) ->
              (k :: l, v)
            )
            (aux sub_trie)
        )
    in
    match t.value with
    | None -> sub_tries_seq
    | Some v ->
      Seq.cons ([], v) sub_tries_seq
  in
  aux t
  |> Seq.map (fun (l, v) ->
      (Abs_path.of_parts_exn l, v)
    )
