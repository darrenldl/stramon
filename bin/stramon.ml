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
  try (
    let run, cleanup =
      Stramon_lib.monitor command
    in
    try
      let summary = run () in
      ()
    with
    | Sys.Break -> cleanup ()
  ) with
  | Unix.Unix_error _ -> (
      Printf.printf "Error: Failed to create strace process\n";
      exit 1
    )
