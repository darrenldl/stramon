open Test_utils

module Alco = struct
  let suite =
    [
    ]
end

module Qc = struct
  let add_path_and_value_pairs0 =
    QCheck.Test.make ~count:10_000 ~name:"add_path_and_value_pairs0"
      abs_path_int_pairs
      (fun l ->
         QCheck.assume (List.for_all (fun (p, _) -> Option.is_some p) l);
         let l = List.map (fun (p, x) -> (Option.get p, x)) l in
         let trie =
           List.fold_left (fun acc (p, x) ->
               Stramon_lib.Path_trie.add p x acc
             )
             Stramon_lib.Path_trie.empty
             l
         in
         let m =
           List.fold_left (fun acc (p, x) ->
               Path_map.add p x acc
             )
             Path_map.empty
             l
         in
         List.for_all (fun (p, _x) ->
             Path_map.find p m = Stramon_lib.Path_trie.find_exn p trie
           )
           l
      )

  let add_path_and_value_pairs1 =
    QCheck.Test.make ~count:10_000 ~name:"add_path_and_value_pairs1"
      abs_path_int_pairs
      (fun l ->
         QCheck.assume (List.for_all (fun (p, _) -> Option.is_some p) l);
         let l = List.map (fun (p, x) -> (Option.get p, x)) l in
         let trie =
           List.fold_left (fun acc (p, x) ->
               Stramon_lib.Path_trie.add p x acc
             )
             Stramon_lib.Path_trie.empty
             l
         in
         let m =
           List.fold_left (fun acc (p, x) ->
               String_map.add (Stramon_lib.Abs_path.to_string p) x acc
             )
             String_map.empty
             l
         in
         List.for_all (fun (p, _x) ->
             String_map.find (Stramon_lib.Abs_path.to_string p) m = Stramon_lib.Path_trie.find_exn p trie
           )
           l
      )
  let suite =
    [
      add_path_and_value_pairs0;
      add_path_and_value_pairs1;
    ]
end
