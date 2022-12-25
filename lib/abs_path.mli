type t

val equal : t -> t -> bool

val compare : t -> t -> int

val root : t

val to_parts : t -> string list

val of_parts : string list -> t option

val of_parts_exn : string list -> t

val of_string : ?cwd:t -> string -> t option

val of_string_exn : ?cwd:t -> string -> t

val to_string : t -> string
