type t

val make : unit -> t

val get_unfinished_line : pid:int -> t -> string option

val clear_unfinished_line : pid:int -> t -> unit

val set_unfinished_line : pid:int -> string -> t -> unit

val get_cwd : pid:int -> t -> string

val set_cwd : pid:int -> string -> t -> unit
