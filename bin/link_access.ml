type mode = [
  | `R
  | `Rw
]

type t = {
  r : Path_trie_set.t Path_trie.t;
  rw : Path_trie_set.t Path_trie.t;
}

let empty : t =
  {
    r = Path_trie.empty;
    rw = Path_trie.empty;
  }

let add (mode : mode) ~(target : Abs_path.t) (path : Abs_path.t) (t : t) : t =
  let aux x =
    let set =
      Option.value ~default:Path_trie_set.empty
        (Path_trie.find path x)
      |> Path_trie_set.add target
    in
    Path_trie.add path set x
  in
  match mode with
  | `R -> { t with r = aux t.r }
  | `Rw -> { t with rw = aux t.rw }
