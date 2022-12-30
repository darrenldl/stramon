module Abs_path = Abs_path

module Path_trie = Path_trie

module Path_trie_set = Path_trie_set

module File_utils = File_utils

module Syscall = Syscall

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
          match Syscall.base_of_blob blob with
          | None -> ()
          | Some syscall -> (
              (match debug_level with
               | `None | `All -> ()
               | `Registered -> Fmt.epr "@[<v>%a@,@]" Syscall.pp_base syscall
              );
              match f (Ctx.get_data ctx) pid syscall with
              | None -> ()
              | Some data ->
                Ctx.set_data ctx data
              | exception _ -> ()
            )
        )
    )

module Monitor_result = struct
  type 'a t = {
    data : 'a;
    stats : Stats.t;
    exn : exn option;
  }

  let data t = t.data

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
    ~(init_data : a)
    (cmd : string list)
  : (a Monitor_result.t, string) result =
  match Proc_utils.exec ~stdin ~stdout ~stderr cmd with
  | Error msg -> Error msg
  | Ok (_pid, strace_pipe, cleanup) -> (
      let ctx = Ctx.make init_data in
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
      let exn' =
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
      Ok Monitor_result.{
          data = Ctx.get_data ctx;
          stats = Ctx.get_stats ctx;
          exn = exn';
        }
    )
