type 'a t

val make : 'a -> 'a t

val get_data : 'a t -> 'a

val set_data : 'a t -> 'a -> unit

val get_stats : 'a t -> Stats.t

val set_stats : 'a t -> Stats.t -> unit

val get_proc_ctx : 'a t -> pid:int -> Proc_ctx.t

val remove_proc_ctx : 'a t -> pid:int -> unit

