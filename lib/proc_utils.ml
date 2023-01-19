let exec ~stdin ~stdout ~stderr ~syscalls cmd : (int * in_channel * (unit -> unit), string) result =
  let rec make_pipe () =
    let pipe_name =
      Fmt.str "/tmp/stramon-%d" (Random.int 1_000_000)
    in
    try
      Unix.mkfifo pipe_name 0o664;
      pipe_name
    with
    | Unix.Unix_error _ ->
      make_pipe ()
  in
  match cmd with
  | [] ->
    Error "No command provided"
  | _ -> (
      let pipe_name = make_pipe () in
      let wrapped_cmd =
        [ "strace"
        ; "-v"
        ; "-xx"
        ; "-f"
        ; "-o"
        ; pipe_name
        ; "--seccomp-bpf"
        ; "--decode-fds=path"
        ; Printf.sprintf "--trace=%s" (String.concat "," syscalls)
        ; "--"
        ]
        @
        cmd
      in
      try
        let pid =
          Unix.(create_process "strace" (Array.of_list wrapped_cmd)) stdin stdout stderr
        in
        let pipe = open_in_bin pipe_name in
        let cleanup = (fun () ->
            (
              try
                Unix.kill Sys.sigkill pid
              with _ -> ()
            );
            (
              try
                close_in pipe
              with _ -> ()
            );
            (
              try
                Sys.remove pipe_name
              with _ -> ()
            );
          )
        in
        Ok (pid, pipe, cleanup)
      with
      | Unix.Unix_error _ ->
        Error "Failed to create strace process"
    )
