type mode = [
  | `R
  | `Rw
]

type t = {
  mutable r : Unix.file_kind list Stramon_lib.Path_trie.t;
  mutable rw : Unix.file_kind list Stramon_lib.Path_trie.t;
}

let make () : t =
  {
    r = Stramon_lib.Path_trie.empty;
    rw = Stramon_lib.Path_trie.empty;
  }

let add (t : t) (path : Stramon_lib.Abs_path.t) (mode : mode) : unit =
  let aux trie =
    match Stramon_lib.File_utils.kind_of_file path with
    | None -> trie
    | Some kind -> (
        let l = Stramon_lib.Path_trie.find path trie
                |> Option.value ~default:[]
                |> (fun l -> kind :: l)
                |> List.sort_uniq compare
        in
        Stramon_lib.Path_trie.add path l trie
      )
  in
  match mode with
  | `R -> t.r <- aux t.r
  | `Rw -> t.rw <- aux t.rw
