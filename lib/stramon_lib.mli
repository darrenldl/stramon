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
  type t

  type term =
    | String of string
    | Int of int
    | Pointer of string
    | Struct of (string * term) list
    | Const of string
    | Flags of string list

  val name : t -> string

  val args : t -> term array

  val ret : t -> term

  val errno : t -> string option

  val errno_msg : t -> string option

  val pp_term : Format.formatter -> term -> unit
end

type 'a handler = 'a -> int -> Syscall.t -> 'a

type 'a monitor_handle = {
  pipe_run : unit -> 'a;
  cleanup : unit -> unit;
}

val init : unit -> unit

val monitor :
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr -> 
  ?stderr:Unix.file_descr -> 
  handlers:(string * 'a handler) list ->
  init_data:'a ->
  string list ->
  ('a monitor_handle, string) result
