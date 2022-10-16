(* tmpe process_status = Unix.process_status

   type exec_result = {
   cmd : string;
   status : process_status;
   }

   let pp_process_status (formatter : Format.formatter) s =
   let open Unix in
   match s with
   | WEXITED i -> Fmt.pf formatter "exited %d" i
   | WSIGNALED i -> Fmt.pf formatter "signaled %d" i
   | WSTOPPED i -> Fmt.pf formatter "stopped %d" i

   let pp_exec_result (formatter : Format.formatter) res =
   Fmt.pf "%s %a"
   res.cmd pp_process_status res.status
*)

let exec cmd : int * in_channel * (unit -> unit) =
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
  let pipe_name = make_pipe () in
  let wrapped_cmd =
    [ "strace"
    ; "-v"
    ; "-xx"
    ; "-f"
    ; "-o"
    ; pipe_name
    ; "--decode-fds=path"
    (* ; "-e"
       ; "trace=%file,%process,%net" *)
    ; "--"
    ]
    @
    cmd
  in
  let pid =
    Unix.(create_process "strace" (Array.of_list wrapped_cmd) stdin stdout stderr)
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
  (pid, pipe, cleanup)
