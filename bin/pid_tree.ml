type t = Int_set.t Int_map.t

let empty : t = Int_map.empty

let pids (t : t) : Int_set.t =
  Int_map.fold (fun pid _children pids ->
      Int_set.(add pid pids)
    )
    t
    Int_set.empty

let roots (t : t) : Int_set.t =
  Int_map.fold (fun _pid children pids ->
      Int_set.diff pids children
    )
    t
    (pids t)

let add ?parent pid (t : t) : t =
  let t' =
    match Int_map.find_opt pid t with
    | None -> Int_map.add pid Int_set.empty t
    | Some _ -> t
  in
  match parent with
  | None -> t'
  | Some parent ->
    match Int_map.find_opt parent t with
    | None -> Int_map.add parent Int_set.(add pid empty) t
    | Some s -> Int_map.add parent Int_set.(add pid s) t

let json_tree_from_root root t =
  let rec aux root : string * Yojson.Basic.t =
    let children = Int_map.find root t in
    let subtrees =
      Int_set.to_seq children
      |> Seq.map (fun c ->
          aux c
        )
      |> List.of_seq
    in
    (string_of_int root, `Assoc subtrees)
  in
  aux root

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
  let children_lookup =
    Int_map.to_seq t
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
      ("children_lookup", `Assoc children_lookup);
      ("tree", `Assoc tree);
    ]
