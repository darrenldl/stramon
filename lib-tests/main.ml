let () =
  let alco_suites =
    [
      ("Abs_path_tests.Alco", Abs_path_tests.Alco.suite);
      ("Path_trie_tests.Alco", Path_trie_tests.Alco.suite);
      ("Util_tests.Alco", Util_tests.Alco.suite);
    ]
  in
  let qc_suites =
    [
      ("Abs_path_tests.Qc", Abs_path_tests.Qc.suite);
      ("Path_trie_tests.Qc", Path_trie_tests.Qc.suite);
    ]
    |> List.map (fun (name, suite) ->
        (name, List.map QCheck_alcotest.to_alcotest suite))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "stramon-lib" suites
