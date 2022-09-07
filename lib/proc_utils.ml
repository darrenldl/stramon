(* type process_status = Unix.process_status

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
  let pipe_name =
    Fmt.str "/tmp/stramon-%d" (Random.int 1_000_000)
  in
  Unix.mkfifo pipe_name 0o664;
  let pipe = open_in_bin pipe_name in
  let wrapped_cmd =
    Fmt.str "strace -v -xx -f -o %s -- %s" pipe_name cmd
  in
  let pid =
    Unix.(create_process wrapped_cmd [||] stdin stdout stderr)
  in
  (pid, pipe)
