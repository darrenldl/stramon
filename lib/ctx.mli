type t

val make : unit -> t

val summary : t -> Summary.t

val get_unfinished_line : pid:int -> t -> string option

val clear_unfinished_line : pid:int -> t -> unit

val set_unfinished_line : pid:int -> string -> t -> unit

val get_cwd : pid:int -> t -> Abs_path.t

val set_cwd : pid:int -> Abs_path.t -> t -> unit

val add_path_fd : pid:int -> fd:int -> Abs_path.t -> t -> unit

val remove_path_fd : pid:int -> fd:int -> t -> unit
