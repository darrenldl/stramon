type t

val make : unit -> t

val get_unfinished_line : t -> string option

val clear_unfinished_line : t -> unit

val set_unfinished_line : t -> string -> unit
