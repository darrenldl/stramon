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
  match mode with
  | `R -> { t with r = Path_trie_set.add path t.r }
  | `Rw -> { t with rw = Path_trie_set.add path t.rw }
