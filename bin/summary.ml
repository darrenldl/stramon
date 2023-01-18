type t = {
  stats : Stramon_lib.Stats.t;
  access : Path_access.t;
}

let make (stats : Stramon_lib.Stats.t) access =
  { stats;
    access;
  }

let json_of_stats (stats : Stramon_lib.Stats.t) : Yojson.Basic.t =
  let l = Stramon_lib.Stats.syscall_count stats
          |> List.map (fun (s, i) -> (s, `Int i))
  in
  `Assoc l

let to_json (t : t) : Yojson.Basic.t =
  `Assoc
    [
      ("stats", json_of_stats t.stats);
      ("fs", Path_access.to_json t.access);
    ]
