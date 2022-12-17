module Abs_path : sig
  type t

  val equal : t -> t -> bool

  val root : t

  val parts : t -> string list

  val of_parts : string list -> t option

  val of_string : ?cwd:t -> string -> t option

  val to_string : t -> string
end

module Path_trie : sig
  type 'a t

  val empty : 'a t

  val add : Abs_path.t -> 'a -> 'a t -> 'a t

  val find : Abs_path.t -> 'a t -> 'a option

  val find_exn : Abs_path.t -> 'a t -> 'a
end

module Path_trie_set : sig
  type t

  val empty : t

  val add : Abs_path.t -> t -> t

  val mem : Abs_path.t -> t -> bool
end

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

val init : unit -> unit

module Monitor_result : sig
  type 'a t

  val data : 'a t -> 'a

  val exn : 'a t -> exn option
end

val monitor :
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr -> 
  ?stderr:Unix.file_descr -> 
  handlers:'a Syscall.handler list ->
  init_data:'a ->
  string list ->
  ('a Monitor_result.t, string) result
