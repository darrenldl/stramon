module Abs_path = Abs_path

module Path_trie = Path_trie

module Path_trie_set = Path_trie_set

module Utils = struct
  let kind_of_file = File_utils.kind_of_file

  let string_of_hex_string = String_utils.string_of_hex_string

  let hex_of_string = String_utils.hex_of_string

  let octal_of_string = String_utils.octal_of_string
end

module Syscall = struct
  include Syscall

  include Syscall_utils
end

module Stats = Stats

type 'a handler_db = (string, 'a Syscall.base_handler) Hashtbl.t

type debug_level = [
  | `None
  | `Registered
  | `All
]

let process_line
    ~(debug_level : debug_level)
    (handler_db : 'a handler_db)
    (ctx : 'a Ctx.t)
    ({ pid; text } : Strace_pipe.line)
  =
  match Syscall.blob_of_string text with
  | None -> ()
  | Some blob -> (
      let stats = Ctx.get_stats ctx in
      Ctx.set_stats ctx (Stats.record_syscall blob.name stats);
      (match debug_level with
       | `None | `Registered -> ()
       | `All -> Fmt.epr "@[<v>%a@,@]" Syscall.pp_blob blob
      );
      match Hashtbl.find_opt handler_db blob.name with
      | None -> ()
      | Some f -> (
          (match debug_level with
           | `None | `All -> ()
           | `Registered -> Fmt.epr "@[<v>%a@,@]" Syscall.pp_base syscall
          );
          match Syscall.base_of_blob blob with
          | None -> ()
          | Some syscall -> (
              match f (Ctx.get_user_ctx ctx) pid syscall with
              | None -> ()
              | Some user_ctx ->
                Ctx.set_user_ctx ctx user_ctx
              | exception _ -> ()
            )
        )
    )

module Monitor_result = struct
  type 'a t = {
    user_ctx : 'a;
    stats : Stats.t;
    exn : exn option;
  }

  let make user_ctx stats exn =
    { user_ctx;
      stats;
      exn;
    }

  let ctx t = t.user_ctx

  let stats t = t.stats

  let exn t = t.exn
end

let monitor
    (type a)
    ?(debug_level = `None)
    ?(stdin = Unix.stdin)
    ?(stdout = Unix.stdout)
    ?(stderr = Unix.stderr)
    ~(handlers : a Syscall.handler list)
    ~(init_ctx : a)
    (cmd : string list)
  : (a Monitor_result.t, string) result =
  match Proc_utils.exec ~stdin ~stdout ~stderr cmd with
  | Error msg -> Error msg
  | Ok (_pid, strace_pipe, cleanup) -> (
      let ctx = Ctx.make init_ctx in
      let handler_db : a handler_db =
        List.to_seq handlers
        |> Seq.map Syscall.base_handler_of_handler
        |> Hashtbl.of_seq
      in
      let rec run () =
        let open Strace_pipe in
        match read_line ctx strace_pipe with
        | Line line -> (
            process_line ~debug_level handler_db ctx line;
            run ()
          )
        | Not_ready -> run ()
        | Eof -> ()
      in
      let exn =
        (
          try
            run ();
            cleanup ();
            None
          with
          | e -> (
              cleanup ();
              Some e
            )
        )
      in
      Ok (Monitor_result.make
            (Ctx.get_user_ctx ctx)
            (Ctx.get_stats ctx)
            exn
         )
    )
