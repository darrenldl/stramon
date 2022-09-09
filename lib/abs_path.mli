type t

val equal : t -> t -> bool

val root : t

val parts : t -> string list

val of_parts : string list -> t option

val of_string : cwd:t -> string -> t option

val to_string : t -> string
