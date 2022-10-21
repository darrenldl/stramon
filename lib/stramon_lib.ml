module Path_access = Path_access

module Link_access = Link_access

module Summary = Summary

let init () =
  Random.self_init ()

type 'a handler = 'a -> int -> Syscall.t -> 'a

type 'a handler_db = (string, 'a handler) Hashtbl.t

type 'a monitor_handle = {
  run : unit -> 'a;
  cleanup : unit -> unit;
}

let process_line (handler_db : 'a handler_db) (ctx : 'a Ctx.t) ({ pid; text } : Strace_pipe.line) =
  match Syscall.blob_of_string text with
  | None -> ()
  | Some blob -> (
      match Hashtbl.find_opt handler_db blob.name with
      | None -> ()
      | Some f ->
        match Syscall.of_blob blob with
        | None -> ()
        | Some syscall -> (
            let data = f (Ctx.get_data ctx) pid syscall in
            Ctx.set_data ctx data
          )
    )

let monitor
    (type a)
    ?(stdin = Unix.stdin)
    ?(stdout = Unix.stdout)
    ?(stderr = Unix.stderr)
    ~(handlers : (string * a handler) list)
    ~(init_data : a)
    (cmd : string list)
  : (a monitor_handle, string) result =
  match Proc_utils.exec ~stdin ~stdout ~stderr cmd with
  | Error msg -> Error msg
  | Ok (_pid, strace_pipe, cleanup) -> (
      let ctx = Ctx.make init_data in
      let handler_db : a handler_db =
        List.to_seq handlers
        |> Hashtbl.of_seq
      in
      let rec run () =
        let open Strace_pipe in
        match read_line ctx strace_pipe with
        | Line line -> (
            process_line handler_db ctx line;
            run ()
          )
        | Not_ready -> run ()
        | Eof -> (Ctx.get_data ctx)
      in
      Ok { run; cleanup }
    )

module Summary_handlers = struct
  let open_f (summary : Summary.t) pid (syscall : Syscall.t) =
    Fmt.pr "@[<v>%a@,@]" Syscall.pp_term syscall.ret;
    match syscall.ret with
    | String path ->
      Printf.printf "pid: %d, syscall: %s, ret: %S\n" pid syscall.name path;
      summary
    | _ -> summary

  let list : (string * Summary.t handler) list =
    [ ("openat", open_f)
    ; ("open", open_f)
    ]
end

let monitor_summary
    ?stdin
    ?stdout
    ?stderr
    cmd
  : (Summary.t monitor_handle, string) result =
  monitor ?stdin ?stdout ?stderr
    ~handlers:Summary_handlers.list ~init_data:Summary.empty cmd
