type 'a t

val make : 'a -> 'a t

val get_user_ctx : 'a t -> 'a

val set_user_ctx : 'a t -> 'a -> unit

val get_stats : 'a t -> Stats.t

val set_stats : 'a t -> Stats.t -> unit

val get_proc_ctx : 'a t -> pid:int -> Proc_ctx.t

val remove_proc_ctx : 'a t -> pid:int -> unit
