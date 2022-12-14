type mode = [
  | `R
  | `Rw
]

type t = {
  r : Unix.file_kind list Stramon_lib.Path_trie.t;
  rw : Unix.file_kind list Stramon_lib.Path_trie.t;
}

let empty : t =
  {
    r = Stramon_lib.Path_trie.empty;
    rw = Stramon_lib.Path_trie.empty;
  }

let add (path : Stramon_lib.Abs_path.t) (mode : mode) (t : t) : t =
  let aux trie =
    let l = Stramon_lib.Path_trie.find path trie
            |> Option.value ~default:[]
            |> (fun l ->
                match Stramon_lib.Utils.kind_of_file path with
                | None -> l
                | Some kind ->
                  kind :: l
              )
            |> List.sort_uniq compare
    in
    Stramon_lib.Path_trie.add path l trie
  in
  match mode with
  | `R -> { t with r = aux t.r }
  | `Rw -> { t with rw = aux t.rw }

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
