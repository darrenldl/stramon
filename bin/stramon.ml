let () =
  Stramon_lib.init ();
  Stramon_lib.monitor [ "ls" ] |> ignore
