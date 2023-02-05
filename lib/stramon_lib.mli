(** Process behavior monitoring library based on strace

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

      Syscall names which collide with reserved
      OCaml keywords are suffixed with [_],
      e.g. [open] becomes [open_],
      unless collision does not occur due
      to name being part of a longer name,
      e.g. part of a longer function name.

      This is to ensure easy lookup
      as the original syscall names remain
      searchable.
  *)

  type literal = [
    | `Const of string
    | `Int of int64
  ]

  type open_ = {
    path : string;
    flags : literal list;
    mode : literal list;
    ret : int;
  }

  type openat = {
    relative_to : string;
    path : string;
    flags : literal list;
    mode : literal list;
  }

  type read = {
    path : string;
    byte_count_requested : int;
    byte_count_read : int;
    errno : string option;
    errno_msg : string option;
  }

  type socket = {
    domain : string;
    typ : literal list;
    protocol : string;
    errno : string option;
    errno_msg : string option;
  }

  type chown = {
    path : string;
    owner : int;
    group : int;
    ret : int;
  }

  type fchownat = {
    relative_to : string;
    path : string;
    owner : int;
    group : int;
    ret : int;
    flags : literal list;
  }

  type chmod = {
    path : string;
    mode : int;
    ret : int;
  }

  type fchmodat = {
    relative_to : string;
    path : string;
    mode : int;
    ret : int;
    flags : literal list;
  }

  type stat = {
    path : string;
    uid : int;
    gid : int;
    ret : int;
  }

  type fstatat = {
    relative_to : string;
    path : string;
    uid : int;
    gid : int;
    ret : int;
    flags : literal list;
  }

  type statx = {
    relative_to : string;
    path : string;
    flags : literal list;
    mask : literal list;
    uid : int;
    gid : int;
    ret : int;
  }

  type sockaddr_in = {
    port : int;
    addr : string;
  }

  type sockaddr_in6 = {
    port : int;
    flow_info : int64;
    addr : string;
    scope_id : int64;
  }

  type sockaddr = [
    | `AF_INET of sockaddr_in
    | `AF_INET6 of sockaddr_in6
  ]

  type connect = {
    socket : string;
    addr : sockaddr;
  }

  type accept = {
    socket : string;
    addr : sockaddr option;
  }

  type bind = {
    socket : string;
    addr : sockaddr;
  }

  type listen = {
    socket : string;
  }

  type fork = {
    pid : int option;
    errno : string option;
    errno_msg : string option;
  }

  type clone = {
    flags : literal list;
    child_tid : int option;
    errno : string option;
    errno_msg : string option;
  }

  type clone3 = {
    flags : literal list;
    child_tid : int option;
    errno : string option;
    errno_msg : string option;
  }

  type execve = {
    program : string;
    argv : string list;
  }

  type execveat = {
    relative_to : string;
    program : string;
    argv : string list;
  }

  type 'a handler = [
    | `open_ of 'a -> int -> open_ -> 'a
    | `openat of 'a -> int -> openat -> 'a
    | `read of 'a -> int -> read -> 'a
    | `socket of 'a -> int -> socket -> 'a
    | `chown of 'a -> int -> chown -> 'a
    | `fchown of 'a -> int -> chown -> 'a
    | `lchown of 'a -> int -> chown -> 'a
    | `fchownat of 'a -> int -> fchownat -> 'a
    | `chmod of 'a -> int -> chmod -> 'a
    | `fchmod of 'a -> int -> chmod -> 'a
    | `fchmodat of 'a -> int -> fchmodat -> 'a
    | `stat of 'a -> int -> stat -> 'a
    | `fstat of 'a -> int -> stat -> 'a
    | `lstat of 'a -> int -> stat -> 'a
    | `fstatat64 of 'a -> int -> fstatat -> 'a
    | `newfstatat of 'a -> int -> fstatat -> 'a
    | `statx of 'a -> int -> statx -> 'a
    | `accept of 'a -> int -> accept -> 'a
    | `connect of 'a -> int -> connect -> 'a
    | `bind of 'a -> int -> bind -> 'a
    | `listen of 'a -> int -> listen -> 'a
    | `fork of 'a -> int -> fork -> 'a
    | `clone of 'a -> int -> clone -> 'a
    | `clone3 of 'a -> int -> clone3 -> 'a
    | `execve of 'a -> int -> execve -> 'a
    | `fexecve of 'a -> int -> execve -> 'a
    | `execveat of 'a -> int -> execveat -> 'a
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
  | `Registered of Format.formatter
]

val monitor :
  ?copy_raw_strace:Format.formatter ->
  ?debug_level:debug_level ->
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr -> 
  ?stderr:Unix.file_descr -> 
  ?max_string_len:int ->
  handlers:'a Syscall.handler list ->
  init_ctx:'a ->
  string list ->
  ('a Monitor_result.t, string) result
(** [monitor ~handlers ~init_ctx cmd]
    spawns a monitoring session of [cmd].

    [debug_level] determines the debuging information printed to stderr:

    - [`None]: print nothing (default)
    - [`Registered]: print data recognized by one of the [handlers]

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

  val remove_c_comments : string -> string
  (** Does not handle nested C comments *)
end
