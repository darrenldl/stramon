let () =
  print_endline "test0";
  flush stdout;
  let (pid, pipe) = Stramon_lib.exec [ "ls" ] in
  Fmt.pr "@[<v>pid: %d@,@]" pid;
  flush stdout;
  while true do
    Fmt.pr "@[<v>output from pipe: %s@,@]" (input_line pipe);
    flush stdout;
  done
