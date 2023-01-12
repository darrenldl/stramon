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
      CCSeq.cons ([], v) sub_tries_seq
  in
  aux t
  |> Seq.map (fun (l, v) ->
      (Abs_path.of_parts_exn l, v)
    )

let of_seq (s : (Abs_path.t * 'a) Seq.t) : 'a t =
  Seq.fold_left (fun acc (p, x) ->
      add p x acc
    )
    empty
    s

let is_empty t =
  match to_seq t () with
  | Seq.Nil -> true
  | _ -> false

let merge
    (type a b c)
    (f : Abs_path.t -> a option -> b option -> c option)
    (t1 : a t)
    (t2 : b t)
  : c t =
  let s1 = to_seq t1 |> Seq.map fst in
  let s2 = to_seq t2 |> Seq.map fst in
  let keys = CCSeq.append s1 s2
             |> Seq.fold_left (fun acc p ->
                 Abs_path_set.add p acc
               )
               Abs_path_set.empty
  in
  Abs_path_set.to_seq keys
  |> Seq.fold_left (fun t p ->
      match f p (find p t1) (find p t2) with
      | None -> t
      | Some x -> add p x t
    )
    empty

let union
    (type a)
    (f : Abs_path.t -> a -> a -> a option)
    (t1 : a t)
    (t2 : a t)
  : a t =
  merge (fun p x y ->
      match x, y with
      | None, None -> None
      | Some v, None -> Some v
      | None, Some v -> Some v
      | Some x, Some y -> f p x y
    ) t1 t2

let equal (f : 'a -> 'a -> bool) t1 t2 =
  CCSeq.equal (fun (p1, v1) (p2, v2) ->
      Abs_path.equal p1 p2 && f v1 v2
    )
    (to_seq t1) (to_seq t2)
