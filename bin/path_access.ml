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

let string_of_file_kind (kind : Unix.file_kind) : string =
  let open Unix in
  match kind with
  | S_REG -> "REG"
  | S_DIR -> "DIR"
  | S_CHR -> "CHR"
  | S_BLK -> "BLK"
  | S_LNK -> "LNK"
  | S_FIFO -> "FIFO"
  | S_SOCK -> "SOCK"

let json_of_trie (trie : Unix.file_kind list Stramon_lib.Path_trie.t) : Yojson.Basic.t =
  let l = Stramon_lib.Path_trie.to_seq trie
          |> Seq.map (fun (path, kinds) ->
              let kinds = kinds
                          |> List.map string_of_file_kind
                          |> String.concat ","
              in
              (Stramon_lib.Abs_path.to_string path, `String kinds)
            )
          |> List.of_seq
  in
  `Assoc l

let to_json (t : t) : Yojson.Basic.t =
  `Assoc
    [
      ("r", json_of_trie t.r);
      ("rw", json_of_trie t.rw);
    ]
