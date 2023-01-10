open Test_utils

module Alco = struct
  let to_string_case0 () =
    Alcotest.(check string)
      "same string"
      "/"
      Stramon_lib.Abs_path.(of_parts_exn [] |> to_string)

  let part_is_dot_case0 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "."; "abc"; "def" ] |> to_string)

  let part_is_dot_case1 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "abc"; "."; "def" ] |> to_string)

  let part_is_dot_case2 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "abc"; "def"; "." ] |> to_string)

  let part_is_dot_case3 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "abc"; "def"; "."; "." ] |> to_string)

  let part_is_dot_case4 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "."; "."; "abc"; "def" ] |> to_string)

  let part_is_dot_case5 () =
    Alcotest.(check string)
      "same string"
      "/"
      Stramon_lib.Abs_path.(of_parts_exn [ "."; "." ] |> to_string)

  let part_is_empty_case0 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ ""; "abc"; "def" ] |> to_string)

  let part_is_empty_case1 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "abc"; ""; "def" ] |> to_string)

  let part_is_empty_case2 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "abc"; "def"; "" ] |> to_string)

  let part_is_empty_case3 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "abc"; "def"; ""; "" ] |> to_string)

  let part_is_empty_case4 () =
    Alcotest.(check string)
      "same string"
      "/abc/def"
      Stramon_lib.Abs_path.(of_parts_exn [ ""; ""; "abc"; "def" ] |> to_string)

  let part_is_empty_case5 () =
    Alcotest.(check string)
      "same string"
      "/"
      Stramon_lib.Abs_path.(of_parts_exn [ ""; "" ] |> to_string)

  let part_is_double_dot_case0 () =
    Alcotest.(check string)
      "same string"
      "/def"
      Stramon_lib.Abs_path.(of_parts_exn [ "abc"; ".."; "def" ] |> to_string)

  let part_is_double_dot_case1 () =
    Alcotest.(check string)
      "same string"
      "/abc"
      Stramon_lib.Abs_path.(of_parts_exn [ "abc"; "def"; ".." ] |> to_string)

  let part_is_double_dot_case2 () =
    Alcotest.(check (option string))
      "same string"
      None
      Stramon_lib.Abs_path.(of_parts [ ".." ] |> Option.map to_string)

  let suite =
    [
      Alcotest.test_case "to_string_case0" `Quick to_string_case0;
      Alcotest.test_case "part_is_dot_case0" `Quick part_is_dot_case0;
      Alcotest.test_case "part_is_dot_case1" `Quick part_is_dot_case1;
      Alcotest.test_case "part_is_dot_case2" `Quick part_is_dot_case2;
      Alcotest.test_case "part_is_dot_case3" `Quick part_is_dot_case3;
      Alcotest.test_case "part_is_dot_case4" `Quick part_is_dot_case4;
      Alcotest.test_case "part_is_dot_case4" `Quick part_is_dot_case5;
      Alcotest.test_case "part_is_empty_case0" `Quick part_is_empty_case0;
      Alcotest.test_case "part_is_empty_case1" `Quick part_is_empty_case1;
      Alcotest.test_case "part_is_empty_case1" `Quick part_is_empty_case2;
      Alcotest.test_case "part_is_empty_case1" `Quick part_is_empty_case3;
      Alcotest.test_case "part_is_empty_case1" `Quick part_is_empty_case4;
      Alcotest.test_case "part_is_empty_case1" `Quick part_is_empty_case5;
      Alcotest.test_case "part_is_double_dot_case0" `Quick part_is_double_dot_case0;
      Alcotest.test_case "part_is_double_dot_case1" `Quick part_is_double_dot_case1;
      Alcotest.test_case "part_is_double_dot_case1" `Quick part_is_double_dot_case2;
    ]
end

module Qc = struct
  let to_parts_of_parts =
    QCheck.Test.make ~count:100_000 ~name:"to_parts_of_parts" abs_path
      (fun p ->
         QCheck.assume (Option.is_some p);
         let p = Option.get p in
         let p' = p
                  |> Stramon_lib.Abs_path.to_parts
                  |> Stramon_lib.Abs_path.of_parts
                  |> Option.get
         in
         Stramon_lib.Abs_path.equal p p'
      )

  let to_string_of_string =
    QCheck.Test.make ~count:100_000 ~name:"to_string_of_string" abs_path
      (fun p ->
         QCheck.assume (Option.is_some p);
         let p = Option.get p in
         let p' = p
                  |> Stramon_lib.Abs_path.to_string
                  |> Stramon_lib.Abs_path.of_string
                  |> Option.get
         in
         Stramon_lib.Abs_path.equal p p'
      )

  let suite =
    [
      to_parts_of_parts;
      to_string_of_string;
    ]
end
