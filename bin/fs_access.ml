type mode = [
  | `R
  | `Rw
  | `chown
  | `chmod
  | `stat
]

type t = {
  r : string list Stramon_lib.Path_trie.t;
  rw : string list Stramon_lib.Path_trie.t;
  chown : string list Stramon_lib.Path_trie.t;
  chmod : string list Stramon_lib.Path_trie.t;
  stat : string list Stramon_lib.Path_trie.t;
}

let empty : t =
  {
    r = Stramon_lib.Path_trie.empty;
    rw = Stramon_lib.Path_trie.empty;
    chown = Stramon_lib.Path_trie.empty;
    chmod = Stramon_lib.Path_trie.empty;
    stat = Stramon_lib.Path_trie.empty;
  }

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

let add (path : Stramon_lib.Abs_path.t) (mode : mode) (t : t) : t =
  let aux trie =
    let l = Stramon_lib.Path_trie.find path trie
            |> Option.value ~default:[]
            |> (fun l ->
                match Stramon_lib.Utils.kind_of_file path with
                | None -> l
                | Some kind ->
                  string_of_file_kind kind :: l
              )
            |> List.sort_uniq String.compare
    in
    Stramon_lib.Path_trie.add path l trie
  in
  match mode with
  | `R -> { t with r = aux t.r }
  | `Rw -> { t with rw = aux t.rw }
  | `chmod -> { t with chmod = aux t.chmod }
  | `chown -> { t with chown = aux t.chown }
  | `stat -> { t with stat = aux t.stat }

let json_of_trie (trie : string list Stramon_lib.Path_trie.t) : Yojson.Basic.t =
  let l = Stramon_lib.Path_trie.to_seq trie
          |> Seq.map (fun (path, kinds) ->
              let kinds = String.concat "," kinds in
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
      ("chmod", json_of_trie t.chmod);
      ("chown", json_of_trie t.chown);
      ("stat", json_of_trie t.stat);
    ]
