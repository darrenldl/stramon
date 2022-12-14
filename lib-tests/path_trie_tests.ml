open Test_utils

module Alco = struct
  let suite =
    [
    ]
end

module Qc = struct
  let add_find_path_and_value_pairs0 =
    QCheck.Test.make ~count:10_000 ~name:"add_find_path_and_value_pairs0"
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

  let add_find_path_and_value_pairs1 =
    QCheck.Test.make ~count:10_000 ~name:"add_find_path_and_value_pairs1"
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

  let add_remove_find_path_and_value_pairs0 =
    QCheck.Test.make ~count:10_000 ~name:"add_remove_find_path_and_value_pairs0"
      QCheck.(pair abs_path_int_pairs abs_path_int_pairs)
      (fun (l1, l2) ->
         QCheck.assume (List.for_all (fun (p, _) -> Option.is_some p) l1);
         QCheck.assume (List.for_all (fun (p, _) -> Option.is_some p) l2);
         let l1 = List.map (fun (p, x) -> (Option.get p, x)) l1 in
         let l2 = List.map (fun (p, x) -> (Option.get p, x)) l2 in
         let trie =
           List.fold_left (fun acc (p, x) ->
               Stramon_lib.Path_trie.add p x acc
             )
             Stramon_lib.Path_trie.empty
             l1
         in
         let trie =
           List.fold_left (fun acc (p, _) ->
               Stramon_lib.Path_trie.remove p acc
             )
             trie
             l2
         in
         let m =
           List.fold_left (fun acc (p, x) ->
               Path_map.add p x acc
             )
             Path_map.empty
             l1
         in
         let m =
           List.fold_left (fun acc (p, _) ->
               Path_map.remove p acc
             )
             m
             l2
         in
         List.for_all (fun (p, _x) ->
             Path_map.find_opt p m = Stramon_lib.Path_trie.find p trie
           )
           l1
      )

  let add_remove_find_path_and_value_pairs1 =
    QCheck.Test.make ~count:10_000 ~name:"add_remove_find_path_and_value_pairs1"
      QCheck.(pair abs_path_int_pairs abs_path_int_pairs)
      (fun (l1, l2) ->
         QCheck.assume (List.for_all (fun (p, _) -> Option.is_some p) l1);
         QCheck.assume (List.for_all (fun (p, _) -> Option.is_some p) l2);
         let l1 = List.map (fun (p, x) -> (Option.get p, x)) l1 in
         let l2 = List.map (fun (p, x) -> (Option.get p, x)) l2 in
         let trie =
           List.fold_left (fun acc (p, x) ->
               Stramon_lib.Path_trie.add p x acc
             )
             Stramon_lib.Path_trie.empty
             l1
         in
         let trie =
           List.fold_left (fun acc (p, _) ->
               Stramon_lib.Path_trie.remove p acc
             )
             trie
             l2
         in
         let m =
           List.fold_left (fun acc (p, x) ->
               String_map.add (Stramon_lib.Abs_path.to_string p) x acc
             )
             String_map.empty
             l1
         in
         let m =
           List.fold_left (fun acc (p, _) ->
               String_map.remove (Stramon_lib.Abs_path.to_string p) acc
             )
             m
             l2
         in
         List.for_all (fun (p, _x) ->
             String_map.find_opt (Stramon_lib.Abs_path.to_string p) m = Stramon_lib.Path_trie.find p trie
           )
           l1
      )

  let to_seq0 =
    QCheck.Test.make ~count:1_000 ~name:"to_seq0"
      abs_path_int_pairs
      (fun l ->
         QCheck.assume (List.for_all (fun (p, _) -> Option.is_some p) l);
         let l = List.map (fun (p, x) -> (Option.get p, x)) l in
         let trie_l =
           List.fold_left (fun acc (p, x) ->
               Stramon_lib.Path_trie.add p x acc
             )
             Stramon_lib.Path_trie.empty
             l
           |> Stramon_lib.Path_trie.to_seq
           |> List.of_seq
         in
         let m =
           List.fold_left (fun acc (p, x) ->
               Path_map.add p x acc
             )
             Path_map.empty
             l
         in
         List.length trie_l = Path_map.cardinal m
         &&
         List.for_all (fun (p, _x) ->
             Path_map.find p m = List.assoc p trie_l
           )
           l
      )

  let to_seq1 =
    QCheck.Test.make ~count:1_000 ~name:"to_seq1"
      abs_path_int_pairs
      (fun l ->
         QCheck.assume (List.for_all (fun (p, _) -> Option.is_some p) l);
         let l = List.map (fun (p, x) -> (Option.get p, x)) l in
         let trie_l =
           List.fold_left (fun acc (p, x) ->
               Stramon_lib.Path_trie.add p x acc
             )
             Stramon_lib.Path_trie.empty
             l
           |> Stramon_lib.Path_trie.to_seq
           |> List.of_seq
         in
         let m =
           List.fold_left (fun acc (p, x) ->
               String_map.add (Stramon_lib.Abs_path.to_string p) x acc
             )
             String_map.empty
             l
         in
         List.length trie_l = String_map.cardinal m
         &&
         List.for_all (fun (p, _x) ->
             String_map.find (Stramon_lib.Abs_path.to_string p) m = List.assoc p trie_l
           )
           l
      )

  let suite =
    [
      add_find_path_and_value_pairs0;
      add_find_path_and_value_pairs1;
      add_remove_find_path_and_value_pairs0;
      add_remove_find_path_and_value_pairs1;
      to_seq0;
      to_seq1;
    ]
end
