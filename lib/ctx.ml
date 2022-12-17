type 'a t = {
  proc_ctxs : (int, Proc_ctx.t) Hashtbl.t;
  mutable stats : Stats.t;
  mutable data : 'a;
}

let make (data : 'a) : 'a t =
  {
    proc_ctxs = Hashtbl.create 100;
    stats = Stats.empty;
    data;
  }

let get_data t = t.data

let set_data t x = t.data <- x

let get_stats t = t.stats

let set_stats t x = t.stats <- x

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
