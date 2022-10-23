type mode = [
  | `R
  | `Rw
]

type t = {
  r : Path_trie_set.t;
  rw : Path_trie_set.t;
}

let empty : t =
  {
    r = Path_trie_set.empty;
    rw = Path_trie_set.empty;
  }

let add (mode : mode) (path : Abs_path.t) (t : t) : t =
  let aux x =
    Path_trie_set.add path x
  in
  match mode with
  | `R -> { t with r = aux t.r }
  | `Rw -> { t with rw = aux t.rw }
