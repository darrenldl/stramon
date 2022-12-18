type t

val empty : t

val record_syscall : string -> t -> t

val syscall_count : t -> (string * int) list

val syscall_count_seq : t -> (string * int) Seq.t
