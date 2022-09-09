let init () =
  Random.self_init ()

let process_line (ctx : Ctx.t) ({ pid; text } : Strace_pipe.line) =
  match Syscall.blob_of_string text with
  | None -> ()
  | Some blob ->
    match blob.name with
    | "openat" | "open" -> (
        match Syscall.syscall_of_blob blob with
        | None -> print_endline "failed"
        | Some syscall -> (
            let open Syscall in
            match syscall.args with
            | String path :: _ ->
              Printf.printf "pid: %d, syscall: %s, path: %S, ret: %s\n" pid syscall.name path blob.ret
            | _ -> ()
          )
      )
    | _ -> ()

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
