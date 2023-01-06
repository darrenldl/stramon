type t = unit Path_trie.t

let empty : t = Path_trie.empty

let is_empty t = Path_trie.is_empty t

let add path t =
  Path_trie.add path () t

let remove path t =
  Path_trie.remove path t

let mem path t : bool =
  Option.is_some (Path_trie.find path t)

let union t1 t2 =
  Path_trie.union (fun _p () () -> Some ()) t1 t2

let inter t1 t2 =
  Path_trie.merge (fun _p x y ->
      match x, y with
      | Some (), Some () -> Some ()
      | _, _ -> None
    )
    t1 t2

let equal t1 t2 =
  Path_trie.equal (fun () () -> true) t1 t2

let to_seq t =
  Path_trie.to_seq t
  |> Seq.map fst

let of_seq (s : Abs_path.t Seq.t) =
  s
  |> Seq.map (fun p -> (p, ()))
  |> Path_trie.of_seq

