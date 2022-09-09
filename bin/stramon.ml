let () =
  Stramon_lib.init ();
  Stramon_lib.monitor [ "falkon" ] |> ignore
