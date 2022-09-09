let init () =
  Random.self_init ()

let process_line (ctx : Ctx.t) (line : Strace_pipe.line) =
  let open Strace_pipe in
  Printf.printf "pid %d: %s\n" line.pid line.text

let monitor (cmd : string list) : Summary.t =
  let ctx = Ctx.make () in
  let (_pid, strace_pipe, clean_up) = Proc_utils.exec cmd in
  let rec aux () =
    let open Strace_pipe in
    match read_line ctx strace_pipe with
    | Line line -> (
        process_line ctx line;
        aux ()
      )
    | Not_ready -> aux ()
    | Eof -> (
        clean_up ()
      )
  in
  aux ();
  Ctx.summary ctx
