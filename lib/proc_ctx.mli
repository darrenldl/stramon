type t

val make : unit -> t

val get_unfinished_line : t -> string option

val clear_unfinished_line : t -> unit

val set_unfinished_line : t -> string -> unit

val get_cwd : t -> Abs_path.t

val set_cwd : t -> Abs_path.t -> unit

val add_path_fd : t -> fd:int -> Abs_path.t -> unit

val remove_path_fd : t -> fd:int -> unit
