let () =
  Stramon_lib.init ();
  let (pid, pipe) = Stramon_lib.exec [ "ls" ] in
  while true do
    Fmt.pr "@[<v>output from pipe: %s@,@]" (input_line pipe);
    flush stdout;
  done
