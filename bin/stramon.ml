let command = ref []

let add_to_command x =
  command := x :: !command

let speclist = Arg.[
    ("--", Rest add_to_command, "")
  ]

let usage_msg = ""

let () =
  Stramon_lib.init ();
  Sys.catch_break true;
  Arg.parse speclist add_to_command "";
  let command = List.rev !command in
  match Stramon_lib.monitor command with
  | Error msg -> (
      Printf.printf "Error: %s\n" msg;
      exit 1
    )
  | Ok (run, cleanup) -> (
      try
        let summary = run () in
        ()
      with
      | Sys.Break -> cleanup ()
    )
