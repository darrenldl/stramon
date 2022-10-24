module Abs_path = Abs_path

module Path_trie = Path_trie

module Path_trie_set = Path_trie_set

module Syscall = Syscall

let init () =
  Random.self_init ()

type 'a handler = 'a -> int -> Syscall.t -> 'a

type 'a handler_db = (string, 'a handler) Hashtbl.t

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

module Monitor_result = struct
  type 'a t = {
    data : 'a;
    exn : exn option;
  }

  let data t = t.data

  let exn t = t.exn
end

let monitor
    (type a)
    ?(stdin = Unix.stdin)
    ?(stdout = Unix.stdout)
    ?(stderr = Unix.stderr)
    ~(handlers : (string * a handler) list)
    ~(init_data : a)
    (cmd : string list)
  : (a Monitor_result.t, string) result =
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
          exn = exn';
        }
    )
