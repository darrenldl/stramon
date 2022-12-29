type mode = [
  | `R
  | `Rw
]

type t = {
  r : Unix.file_kind list Path_trie.t;
  rw : Unix.file_kind list Path_trie.t;
}

let empty : t =
  {
    r = Path_trie.empty;
    rw = Path_trie.empty;
  }

let add (path : Abs_path.t) (mode : mode) (t : t) : t =
  let aux trie =
    let kind = Stramon_lib.File_utils.kind_of_file path in
    let l = Stramon_lib.Path_trie.find path trie
            |> Option.value ~default:[]
            |> (fun l -> kind :: l)
            |> List.sort_uniq compare
    in
    Path_trie.add path l x
  in
  match mode with
  | `R -> { t with r = aux t.r }
  | `Rw -> { t with rw = aux t.rw }
