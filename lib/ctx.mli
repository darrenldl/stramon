type t

val make : unit -> t

val summary : t -> Summary.t

val get_proc_ctx : t -> pid:int -> Proc_ctx.t

val remove_proc_ctx : t -> pid:int -> unit
