module Alco = struct
  let string_of_hex_string_case0 () =
    Alcotest.(check string)
      "same string"
      "abcd"
      Stramon_lib.Utils.(Option.get @@ string_of_hex_string "\\x61\\x62\\x63\\x64")

  let string_of_hex_string_case1 () =
    Alcotest.(check string)
      "same string"
      "AbCd"
      Stramon_lib.Utils.(Option.get @@ string_of_hex_string "\\x41\\x62\\x43\\x64")

  let hex_of_string_case0 () =
    Alcotest.(check int64)
      "same int"
      0x1234L
      Stramon_lib.Utils.(Option.get @@ hex_of_string "1234")

  let hex_of_string_case1 () =
    Alcotest.(check int64)
      "same int"
      0x0234L
      Stramon_lib.Utils.(Option.get @@ hex_of_string "234")

  let hex_of_string_case2 () =
    Alcotest.(check int64)
      "same int"
      0x1234CDEFL
      Stramon_lib.Utils.(Option.get @@ hex_of_string "1234CDEF")

  let hex_of_string_case3 () =
    Alcotest.(check int64)
      "same int"
      0x1234CDEL
      Stramon_lib.Utils.(Option.get @@ hex_of_string "1234CDE")

  let octal_of_string_case0 () =
    Alcotest.(check int64)
      "same int"
      0o1234L
      Stramon_lib.Utils.(Option.get @@ octal_of_string "1234")

  let octal_of_string_case1 () =
    Alcotest.(check int64)
      "same int"
      0o0234L
      Stramon_lib.Utils.(Option.get @@ octal_of_string "234")

  let octal_of_string_case2 () =
    Alcotest.(check int64)
      "same int"
      0o1234567L
      Stramon_lib.Utils.(Option.get @@ octal_of_string "1234567")

  let octal_of_string_case3 () =
    Alcotest.(check int64)
      "same int"
      0o123456L
      Stramon_lib.Utils.(Option.get @@ octal_of_string "123456")

  let suite =
    [
      Alcotest.test_case "string_of_hex_string_case0" `Quick string_of_hex_string_case0;
      Alcotest.test_case "string_of_hex_string_case1" `Quick string_of_hex_string_case1;
      Alcotest.test_case "hex_of_string_case0" `Quick hex_of_string_case0;
      Alcotest.test_case "hex_of_string_case1" `Quick hex_of_string_case1;
      Alcotest.test_case "hex_of_string_case2" `Quick hex_of_string_case2;
      Alcotest.test_case "hex_of_string_case3" `Quick hex_of_string_case3;
      Alcotest.test_case "octal_of_string_case0" `Quick octal_of_string_case0;
      Alcotest.test_case "octal_of_string_case1" `Quick octal_of_string_case1;
      Alcotest.test_case "octal_of_string_case2" `Quick octal_of_string_case2;
      Alcotest.test_case "octal_of_string_case3" `Quick octal_of_string_case3;
    ]
end
