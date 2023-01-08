type 'a t = {
  proc_ctxs : (int, Proc_ctx.t) Hashtbl.t;
  mutable stats : Stats.t;
  mutable user_ctx : 'a;
}

let make (user_ctx : 'a) : 'a t =
  {
    proc_ctxs = Hashtbl.create 100;
    stats = Stats.empty;
    user_ctx;
  }

let get_user_ctx t = t.user_ctx

let set_user_ctx t x = t.user_ctx <- x

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
