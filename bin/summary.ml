type t = {
  stats : Stramon_lib.Stats.t;
  fs : Fs_access.t;
  net : Net_access.t;
}

let make (stats : Stramon_lib.Stats.t) fs net =
  { stats;
    fs;
    net;
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
      ("fs", Fs_access.to_json t.fs);
    ]
