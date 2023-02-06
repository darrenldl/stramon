type exec = {
  prog : string;
  argv : string list;
}

type t = {
  execs : exec list Int_map.t;
  tree : Int_set.t Int_map.t;
}

let empty : t = {
  execs = Int_map.empty;
  tree = Int_map.empty;
}

let pids (t : t) : Int_set.t =
  Int_map.fold (fun pid _children pids ->
      Int_set.(add pid pids)
    )
    t.tree
    Int_set.empty

let roots (t : t) : Int_set.t =
  Int_map.fold (fun _pid children pids ->
      Int_set.diff pids children
    )
    t.tree
    (pids t)

let add ?parent pid (t : t) : t =
  let t =
    match Int_map.find_opt pid t.tree with
    | None -> { t with tree = Int_map.add pid Int_set.empty t.tree }
    | Some _ -> t
  in
  match parent with
  | None -> t
  | Some parent ->
    let s =
      Option.value ~default:Int_set.empty
      @@ Int_map.find_opt parent t.tree
    in
    { t with tree = Int_map.add parent Int_set.(add pid s) t.tree }

let add_exec pid prog argv (t : t) : t =
  let t = add pid t in
  let execs =
    Int_map.find_opt pid t.execs
    |> Option.value ~default:[]
    |> (fun l -> { prog; argv } :: l)
  in
  { t with execs = Int_map.add pid execs t.execs }

let json_tree_from_root root t =
  let rec aux root : string * Yojson.Basic.t =
    let children = Int_map.find root t.tree in
    if Int_set.is_empty children then
      (string_of_int root, `Null)
    else (
      let subtrees =
        Int_set.to_seq children
        |> Seq.map (fun c ->
            aux c
          )
        |> List.of_seq
      in
      (string_of_int root, `Assoc subtrees)
    )
  in
  aux root

let json_of_exec ({ prog; argv } : exec) : Yojson.Basic.t =
  let argv = List.map (fun s -> `String s) argv in
  `Assoc
    [
      ("prog", `String prog);
      ("argv", `List argv);
    ]

let to_json (t : t) : Yojson.Basic.t =
  let flatten_set s =
    s
    |> Int_set.to_seq
    |> Seq.map (fun x -> `String (string_of_int x))
    |> List.of_seq
  in
  let pids =
    flatten_set (pids t)
  in
  let exec_lookup =
    Int_map.to_seq t.execs
    |> Seq.map (fun (pid, execs) ->
        (string_of_int pid,
         (match execs with
          | [ exec ] -> json_of_exec exec
          | _ -> `List (execs |> List.rev |> List.map json_of_exec))
        )
      )
    |> List.of_seq
  in
  let children_lookup =
    Int_map.to_seq t.tree
    |> Seq.map (fun (pid, children) ->
        (string_of_int pid, `List (flatten_set children))
      )
    |> List.of_seq
  in
  let tree =
    roots t
    |> Int_set.to_seq
    |> Seq.map (fun root -> json_tree_from_root root t)
    |> List.of_seq
  in
  `Assoc
    [
      ("list", `List pids);
      ("exec_lookup", `Assoc exec_lookup);
      ("children_lookup", `Assoc children_lookup);
      ("tree", `Assoc tree);
    ]
