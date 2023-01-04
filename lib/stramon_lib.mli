(** strace based process behavior monitoring library

    Stramon-lib is primarily designed to power Stramon,
    or to be used in some fashion of process monitoring.
 *)

(** {1 Absolute path} *)

module Abs_path : sig
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
end

(** {1 Path trie} *)

module Path_trie : sig
  type 'a t

  val empty : 'a t

  val add : Abs_path.t -> 'a -> 'a t -> 'a t

  val find : Abs_path.t -> 'a t -> 'a option

  val find_exn : Abs_path.t -> 'a t -> 'a

  val to_seq : 'a t -> (Abs_path.t * 'a) Seq.t
end

(** {1 Path trie based set} *)

module Path_trie_set : sig
  type t

  val empty : t

  val add : Abs_path.t -> t -> t

  val mem : Abs_path.t -> t -> bool
end

(** {1 Misc utilities} *)

module File_utils : sig
  val kind_of_file : Abs_path.t -> Unix.file_kind option
end

(** {1 Main monitoring facilities and data types} *)

module Syscall : sig
  type _open = {
    path : string;
    flags : string list;
    mode : string list;
  }

  type _openat = {
    relative_to : string;
    path : string;
    flags : string list;
    mode : string list;
  }

  type _read = {
    path : string;
    byte_count_requested : int;
    byte_count_read : int;
    errno : string option;
    errno_msg : string option;
  }

  type 'a handler = [
    | `_open of 'a -> int -> _open -> 'a
    | `_openat of 'a -> int -> _openat -> 'a
    | `_read of 'a -> int -> _read -> 'a
  ]
end

module Stats : sig
  type t

  val empty : t

  val syscall_count : t -> (string * int) list

  val syscall_count_seq : t -> (string * int) Seq.t
end

module Monitor_result : sig
  type 'a t

  val data : 'a t -> 'a

  val stats : 'a t -> Stats.t

  val exn : 'a t -> exn option
end

type debug_level = [
  | `None
  | `Registered
  | `All
]

val monitor :
  ?debug_level:debug_level ->
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr -> 
  ?stderr:Unix.file_descr -> 
  handlers:'a Syscall.handler list ->
  init_data:'a ->
  string list ->
  ('a Monitor_result.t, string) result
  (** [debug_level] affects debuging information printed to stderr, defaults to [`None].
    *
    * [stdin], [stdout], [stderr] are passed to {!Unix.create_process},
    * default to {!Unix.stdin}, {!Unix.stdout}, and {!Unix.stderr} respectively.
   *)
