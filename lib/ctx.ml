type t = {
  proc_ctxs : (int, Proc_ctx.t) Hashtbl.t;
  summary : Summary.t ref;
}

let make () : t =
  {
    proc_ctxs = Hashtbl.create 100;
    summary = ref Summary.empty;
  }

let summary t = !(t.summary)

let get_proc_ctx t ~pid =
  match Hashtbl.find_opt t.proc_ctxs pid with
  | None ->
    let proc_ctx = Proc_ctx.make () in
    Hashtbl.replace t.proc_ctxs pid proc_ctx;
    proc_ctx
  | Some proc_ctx ->
    proc_ctx

let remove_proc_ctx t ~pid =
  Hashtbl.remove t.proc_ctxs pid
