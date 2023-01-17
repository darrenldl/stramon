(** strace based process behavior monitoring library

    Stramon-lib is primarily designed to power Stramon,
    or to be used in similar fashion of process monitoring.
*)

(** {1 Absolute path} *)

module Abs_path : sig
  (** Normalized absolute path representation *)

  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val root : t

  val to_parts : t -> string list

  val of_parts : string list -> t option
  (** Parts that are empty or are a single dot are ignored,
      i.e. [ [ "abc"; ""; "."; "def" ] ] is treated
      the same as [ [ "abc"; "def" ] ].

      [..] goes back one level if possible, returns [None]
      if not possible.

      Returns [None] if any part

      - has trailing backslashes (odd number of backslashes at the end)
      - or contains [/]
  *)

  val of_parts_exn : string list -> t

  val of_string : ?cwd:t -> string -> t option

  val of_string_exn : ?cwd:t -> string -> t

  val to_string : t -> string
end

(** {1 Path trie} *)

module Path_trie : sig
  type 'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val add : Abs_path.t -> 'a -> 'a t -> 'a t

  val remove : Abs_path.t -> 'a t -> 'a t

  val find : Abs_path.t -> 'a t -> 'a option

  val find_exn : Abs_path.t -> 'a t -> 'a

  val merge : (Abs_path.t -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val union : (Abs_path.t -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val to_seq : 'a t -> (Abs_path.t * 'a) Seq.t

  val of_seq : (Abs_path.t * 'a) Seq.t -> 'a t
end

(** {1 Path trie based path set} *)

module Path_trie_set : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val add : Abs_path.t -> t -> t

  val remove : Abs_path.t -> t -> t

  val mem : Abs_path.t -> t -> bool

  val union : t -> t -> t

  val inter : t -> t -> t

  val equal : t -> t -> bool

  val to_seq : t -> Abs_path.t Seq.t

  val of_seq : Abs_path.t Seq.t -> t
end

(** {1 Main monitoring facilities} *)

module Syscall : sig
  (** Syscall data and handler

      {2 Naming convention}

      Due to reserved OCaml keywords, not all original
      syscall names can be used as is.
      Thus all syscall names here are prefixed with [_]
      in the type names and variant names in [handler]
      to ensure consistency for ease of looking up syscalls.
      This extends to type names as well, e.g.
      [sockaddr] becomes [_sockaddr].

      Field names within the individual record types
      do not follow such naming scheme, however, to avoid making
      the process of
      defining handlers becoming too cumbersome,
      e.g. [type] becomes just [typ] rather than [_type].
  *)

  type flag = [
    | `Const of string
    | `Int of int64
  ]

  type _open = {
    path : string;
    flags : flag list;
    mode : flag list;
    ret : int;
  }

  type _openat = {
    relative_to : string;
    path : string;
    flags : flag list;
    mode : flag list;
  }

  type _read = {
    path : string;
    byte_count_requested : int;
    byte_count_read : int;
    errno : string option;
    errno_msg : string option;
  }

  type _socket = {
    domain : string;
    typ : flag list;
    protocol : int;
    errno : string option;
    errno_msg : string option;
  }

  type _chown = {
    path : string;
    owner : int;
    group : int;
    ret : int;
  }

  type _chmod = {
    path : string;
    mode : int;
    ret : int;
  }

  type _stat = {
    path : string;
    uid : int;
    gid : int;
    ret : int;
  }

  type _sockaddr_in = {
    port : int;
    addr : string;
  }

  type _sockaddr_in6 = {
    port : int;
    flow_info : int;
    addr : string;
    scope_id : int;
  }

  type _sockaddr =
    | AF_INET of _sockaddr_in
    | AF_INET6 of _sockaddr_in6

  type _connect = {
    socket : string;
    sa_family : string;
    addr : _sockaddr;
  }

  type _listen = {
    socket : string;
  }

  type _accept = {
    socket : string;
    addr : _sockaddr;
  }

  type _bind = {
    socket : string;
    addr : _sockaddr;
  }

  type 'a handler = [
    | `_open of 'a -> int -> _open -> 'a
    | `_openat of 'a -> int -> _openat -> 'a
    | `_read of 'a -> int -> _read -> 'a
    | `_socket of 'a -> int -> _socket -> 'a
    | `_chown of 'a -> int -> _chown -> 'a
    | `_chmod of 'a -> int -> _chmod -> 'a
    | `_stat of 'a -> int -> _stat -> 'a
    | `_accept of 'a -> int -> _accept -> 'a
    | `_connect of 'a -> int -> _connect -> 'a
    | `_listen of 'a -> int -> _listen -> 'a
    | `_open of 'a -> int -> _open -> 'a
    | `_bind of 'a -> int -> _bind -> 'a
  ]
  (** A handler receives the user-defined "context",
      process id, and finally the syscall specific data.

      The context is the data passed from one call of handler
      to the next.

      All handlers of the same monitoring session (same [monitor] call)
      share the same context.
  *)
end

module Stats : sig
  (** Basic monitoring statistics *)

  type t

  val empty : t

  val syscall_count : t -> (string * int) list

  val syscall_count_seq : t -> (string * int) Seq.t
end

module Monitor_result : sig
  (** Result of a monitoring session *)

  type 'a t

  val ctx : 'a t -> 'a
  (** Final context of a monitoring session *)

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
  init_ctx:'a ->
  string list ->
  ('a Monitor_result.t, string) result
(** [monitor ~handlers ~init_ctx cmd]
    spawns a monitoring session of [cmd].

    [debug_level] determines the debuging information printed to stderr:

    - [`None] prints nothing (default)
    - [`Registered] prints only data recognized by one of the [handlers]
    - [`All] prints all reconstructed data received from strace

    [stdin], [stdout], [stderr] are passed to [Unix.create_process],
    default to [Unix.stdin], [Unix.stdout], and [Unix.stderr] respectively.

    [init_ctx] defines the initial "context".
    It will be provided to the first handler to be invoked.

    If any of the handlers raises an exception,
    then monitoring is interrupted, and exception is captured in
    the returned monitor result.

    Returns [Error] if [monitor] fails to create process
    with [cmd] wrapped with strace.
    This normally means the strace program is not available.
*)

(** {1 Misc utilities} *)

module Utils : sig
  val kind_of_file : Abs_path.t -> Unix.file_kind option

  val string_of_hex_string : ?preamble_before_each_byte:string -> string -> string option

  val hex_of_string : string -> int64 option

  val octal_of_string : string -> int64 option
end
