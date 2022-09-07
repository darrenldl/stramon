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

let exec cmd : int * in_channel =
  let rec make_pipe () =
    let pipe_name =
      Fmt.str "/tmp/stramon-%d" (Random.int 1_000_000)
    in
    try
      Printf.printf "trying %s\n" pipe_name;
      flush stdout;
      Unix.mkfifo pipe_name 0o664;
      Printf.printf "pipe made\n";
      flush stdout;
      pipe_name
    with
    | Unix.Unix_error _ ->
      make_pipe ()
  in
  let pipe_name = make_pipe () in
  Printf.printf "test1\n";
  flush stdout;
  let wrapped_cmd =
    [ "strace"
    ; "-v"
    ; "-xx"
    ; "-f"
    ; "-o"
    ; pipe_name
    ; "--"
    ]
    @
    cmd
  in
  let pid =
    Unix.(create_process "strace" (Array.of_list wrapped_cmd) stdin stdout stderr)
  in
  Printf.printf "test2\n";
  flush stdout;
  Printf.printf "pid: %d\n" pid;
  flush stdout;
  (* let pid =
    Unix.(create_process "ls" [|"ls"|] stdin stdout stderr)
  in *)
  (pid, open_in_bin pipe_name)
