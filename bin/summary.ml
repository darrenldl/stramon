type t = {
  stats : Stramon_lib.Stats.t;
  pid_tree : Pid_tree.t;
  fs : Fs_access.t;
  net : Net_access.t;
}

let make (stats : Stramon_lib.Stats.t) pid_tree fs net =
  { stats;
    pid_tree;
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
      ("pid", Pid_tree.to_json t.pid_tree);
      ("stats", json_of_stats t.stats);
      ("fs", Fs_access.to_json t.fs);
      ("net", Net_access.to_json t.net);
    ]
