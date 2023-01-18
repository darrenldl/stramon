open Option_infix

type blob = {
  name : string;
  arg_text : string;
  ret : string;
  errno : string option;
  errno_msg : string option;
}

type literal = [
  | `Const of string
  | `Int of int64
]

type term = [
  | `String of string
  | `Int of int64
  | `Pointer of string
  | `Struct of (string * term) list
  | `Const of string
  | `Flags of literal list
  | `Array of term list
  | `App of string * term list
]

let int64_of_term (term : term) : int64 option =
  match term with
  | `Int x -> Some x
  | _ -> None

let int64_of_term (term : term) : int64 option =
  match term with
  | `Int x -> Some x
  | _ -> None

let int_of_term (term : term) : int option =
  let* x = int64_of_term term in
  try
    Some (Int64.to_int x)
  with
  | _ -> None

type base = {
  name : string;
  args : term list;
  ret : term;
  errno : string option;
  errno_msg : string option;
}

module Parsers = struct
  open Angstrom
  open Parser_components

  let name_p =
    spaces *> non_space_string <* spaces

  let hex_string_p p =
    p >>= fun s ->
    match String_utils.string_of_hex_string
            ~preamble_before_each_byte:"\\x" s
    with
    | None -> fail "invalid hex string"
    | Some s -> return s

  let nat_zero_int64_octal : int64 t =
    char '0' *> num_string
    >>= fun s ->
    match String_utils.octal_of_string s with
    | None -> fail "invalid octal number"
    | Some s -> return s

  let nat_zero_int64_hex : int64 t =
    string "0x" *> hex_num_string
    >>= fun s ->
    match String_utils.hex_of_string s with
    | None -> fail "invalid hex number"
    | Some s -> return s

  let decoded_p =
    non_angle_string *> char '<' *> hex_string_p non_angle_string >>= fun s ->
    char '>' *> return s

  let blob_ret_p =
    spaces *>
    non_space_string >>= fun ret ->
    spaces *>
    (
      (
        non_space_string >>= fun errno -> 
        spaces *>
        (
          (char '(' *> non_parenthesis_string >>= fun errno_msg ->
           char ')' *> return (ret, Some errno, Some errno_msg))
          <|>
          (return (ret, Some errno, None))
        )
      )
      <|>
      (return (ret, None, None))
    )

  let int_p : int64 t =
    choice [
      nat_zero_int64_hex;
      nat_zero_int64_octal;
      nat_zero_int64;
      (char '-' *> nat_zero_int64 >>| fun n -> Int64.neg n);
    ]

  let literal_p : literal t =
    choice [
      (int_p >>| fun x -> `Int x);
      (ident_string >>| fun x -> `Const x);
    ]

  let pointer_p : string t =
    choice [
      (string "0x" >>= fun pre ->
       hex_num_string >>| fun s -> pre ^ s);
      (string "&" >>= fun pre ->
       ident_string >>| fun s -> pre ^ s);
    ]

  let term_p : term t =
    fix (fun p ->
        choice [
          (decoded_p >>| fun s -> `String s);
          (pointer_p >>| fun s -> `Pointer s);
          (int_p >>| fun n -> `Int n);
          (char '"' *> hex_string_p non_quote_string >>= fun s ->
           char '"' *> return (`String s)
          );
          (char '{' *> spaces *>
           sep_by_comma
             ((ident_string >>=
               fun k ->
               spaces *> char '=' *> spaces *>
               p >>| fun v -> (k, v)
              )
              <|>
              (p >>| fun v -> ("", v))
             )
           >>= fun l ->
           spaces *> char '}' *>
           return (`Struct l)
          );
          (char '[' *>
           spaces *>
           sep_by_comma p >>= fun l ->
           spaces *>
           char ']' *>
           return (`Array l)
          );
          (ident_string >>= fun name ->
           char '(' *>
           sep_by_comma p >>= fun l ->
           char ')' *>
           return (`App (name, l))
          );
          (sep_by1 (char '|') literal_p >>| fun l ->
           match l with
           | [x] -> (x :> term)
           | l -> `Flags l
          );
        ]
      )

  let args_p : term list t =
    sep_by_comma term_p

  let ret_p : term t =
    spaces *> term_p <* spaces
end

let blob_of_string (str : string) : blob option =
  let str_len = String.length str in
  let* open_paren_pos = String_utils.find_char '(' str in
  let* ret_eq_pos = String_utils.find_char_rev '=' str in
  let* close_paren_pos = String_utils.find_char_rev ~start:ret_eq_pos ')' str in
  match
    Angstrom.(parse_string ~consume:Consume.All) Parsers.name_p
      (StringLabels.sub ~pos:0 ~len:open_paren_pos str)
  with
  | Error _ -> None
  | Ok name ->
    match
      Angstrom.(parse_string ~consume:Consume.All) Parsers.blob_ret_p
        (StringLabels.sub ~pos:(ret_eq_pos + 1) ~len:(str_len - (ret_eq_pos + 1)) str)
    with
    | Error _ -> None
    | Ok (ret, errno, errno_msg) ->
      let arg_text =
        StringLabels.sub ~pos:(open_paren_pos + 1) ~len:(close_paren_pos - (open_paren_pos + 1)) str
      in
      Some { name; arg_text; ret; errno; errno_msg }

let base_of_blob ({ name; arg_text; ret; errno; errno_msg } : blob) : (base, string) result =
  match
    Angstrom.(parse_string ~consume:Consume.All) Parsers.args_p arg_text
  with
  | Error msg -> Error msg
  | Ok args ->
    match
      Angstrom.(parse_string ~consume:Consume.All) Parsers.ret_p ret
    with
    | Error msg -> Error msg
    | Ok ret ->
      Ok { name; args; ret; errno; errno_msg }

let rec pp_term (formatter : Format.formatter) (x : term) =
  let rec aux formatter x =
    match x with
    | `String s -> Fmt.pf formatter "<string:%S>" s
    | `Int x -> Fmt.pf formatter "<int:%Ld>" x
    | `Pointer s -> Fmt.pf formatter "<ptr:%s>" s
    | `Struct l ->
      Fmt.pf formatter "<struct:{%a}>"
        Fmt.(list ~sep:comma (fun formatter (s, x) ->
            Fmt.pf formatter "%s=%a" s aux x
          ))
        l
    | `Const s ->
      Fmt.pf formatter "<const:%s>" s
    | `Flags l ->
      Fmt.pf formatter "<flags:%a>"
        Fmt.(list
               ~sep:(fun formatter () -> Fmt.pf formatter "|")
               pp_literal)
        l
    | `Array l ->
      Fmt.pf formatter "<array:[%a]>"
        Fmt.(list ~sep:comma aux)
        l
    | `App (f, l) ->
      Fmt.pf formatter "<app:%s(%a)>"
        f
        Fmt.(list ~sep:comma aux) l
  in
  aux formatter x

and pp_literal formatter (x : literal) =
  pp_term formatter (x :> term)

let pp_blob (formatter : Format.formatter) (x : blob) : unit =
  Fmt.pf formatter "%s(%s) = %s %s %s"
    x.name x.arg_text
    x.ret
    (match x.errno with
     | None -> ""
     | Some x -> x
    )
    (match x.errno_msg with
     | None -> ""
     | Some x -> x
    )

let pp_base (formatter : Format.formatter) (x : base) : unit =
  Fmt.pf formatter "%s(%a) = %a %s %s"
    x.name Fmt.(list ~sep:comma pp_term) x.args
    pp_term x.ret
    (match x.errno with
     | None -> ""
     | Some x -> x
    )
    (match x.errno_msg with
     | None -> ""
     | Some x -> x
    )

type _open = {
  path : string;
  flags : literal list;
  mode : literal list;
  ret : int;
}

let _open_of_base (base : base) : _open option =
  let* ret = int_of_term base.ret in
  match base.args with
  | [ `String path; `Flags flags ] -> Some { path; flags; mode = []; ret }
  | [ `String path; `Flags flags; `Flags mode ] -> Some { path; flags; mode; ret }
  | _ -> None

type _openat = {
  relative_to : string;
  path : string;
  flags : literal list;
  mode : literal list;
}

let _openat_of_base (base : base) : _openat option =
  match base.args with
  | [ `String relative_to; `String path; `Flags flags ] ->
    Some { relative_to; path; flags; mode = [] }
  | [ `String relative_to; `String path; `Flags flags; `Flags mode ] ->
    Some { relative_to; path; flags; mode }
  | _ -> None

type _read = {
  path : string;
  byte_count_requested : int;
  byte_count_read : int;
  errno : string option;
  errno_msg : string option;
}

let _read_of_base (base : base) : _read option =
  let errno = base.errno in
  let errno_msg = base.errno_msg in
  let* byte_count_read = int_of_term base.ret in
  match base.args with
  | [ `String path; _; byte_count_requested ] ->
    let* byte_count_requested = int_of_term byte_count_requested in
    Some { path; byte_count_requested; byte_count_read; errno; errno_msg }
  | _ -> None

type _socket = {
  domain : string;
  typ : literal list;
  protocol : string;
  errno : string option;
  errno_msg : string option;
}

let _socket_of_base (base : base) : _socket option =
  let errno = base.errno in
  let errno_msg = base.errno_msg in
  match base.args with
  | [ `Const domain; `Flags typ; `Const protocol ] ->
    Some { domain; typ; protocol; errno; errno_msg }
  | [ `Const domain; `Const typ; `Const protocol ] ->
    Some { domain; typ = [`Const typ]; protocol; errno; errno_msg }
  | _ -> None

type _chown = {
  path : string;
  owner : int;
  group : int;
  ret : int;
}

let _chown_of_base (base : base) : _chown option =
  let* ret = int_of_term base.ret in
  match base.args with
  | [ `String path; owner; group ] -> (
      let* owner = int_of_term owner in
      let* group = int_of_term group in
      Some { path; owner; group; ret }
    )
  | _ -> None

type _chmod = {
  path : string;
  mode : int;
  ret : int;
}

let _chmod_of_base (base : base) : _chmod option =
  let* ret = int_of_term base.ret in
  match base.args with
  | [ `String path; mode ] -> (
      let* mode = int_of_term mode in
      Some { path; mode; ret }
    )
  | _ -> None

type _stat = {
  path : string;
  uid : int;
  gid : int;
  ret : int;
}

let _stat_of_base (base : base) : _stat option =
  let* ret = int_of_term base.ret in
  match base.args with
  | [ `String path; `Struct status ] -> (
      let* uid = List.assoc_opt "st_uid" status in
      let* uid = int_of_term uid in
      let* gid = List.assoc_opt "st_gid" status in
      let* gid = int_of_term gid in
      Some { path;
             uid;
             gid;
             ret;
           }
    )
  | _ -> None

type _sockaddr_in = {
  port : int;
  addr : string;
}

type _sockaddr_in6 = {
  port : int;
  flow_info : int64;
  addr : string;
  scope_id : int64;
}

type _sockaddr = [
  | `AF_INET of _sockaddr_in
  | `AF_INET6 of _sockaddr_in6
]

type _connect = {
  socket : string;
  addr : _sockaddr;
}

let _sockaddr_of_struct (l : (string * term) list) : _sockaddr option =
  let* sa_family = List.assoc_opt "sa_family" l in
  match sa_family with
  | `String "AF_INET" -> (
      let* port = List.assoc_opt "sin_port" l in
      let* port =
        match port with
        | `App (_, [`Int x]) -> Some x
        | _ -> None
      in
      let* addr = List.assoc_opt "sin_addr" l in
      let* addr =
        match addr with
        | `App (_, [`String x]) -> Some x
        | _ -> None
      in
      Some (`AF_INET { port = Int64.to_int port; addr })
    )
  | `String "AF_INET6" -> (
      let* port = List.assoc_opt "sin6_port" l in
      let* port =
        match port with
        | `App (_, [`Int x]) -> Some x
        | _ -> None
      in
      let* flow_info = List.assoc_opt "sin6_flowinfo" l in
      let* flow_info =
        match flow_info with
        | `App (_, [`Int x]) -> Some x
        | _ -> None
      in
      let* addr = List.assoc_opt "sin_addr" l in
      let* addr =
        match addr with
        | `App (_, [`String x]) -> Some x
        | _ -> None
      in
      let* scope_id = List.assoc_opt "sin6_scope_id" l in
      let* scope_id = int64_of_term scope_id in
      Some (`AF_INET6 { port = Int64.to_int port;
                        flow_info;
                        addr;
                        scope_id;
                      })
    )
  | _ -> None

let _connect_of_base (base : base) : _connect option =
  match base.args with
  | [ `String socket; `Struct sockaddr; `Int _protocol ] ->
    let* addr = _sockaddr_of_struct sockaddr in
    Some { socket; addr }
  | _ -> None

type _accept = {
  socket : string;
  addr : _sockaddr option;
}

let _accept_of_base (base : base) : _accept option =
  match base.args with
  | [ `String socket; `Struct sockaddr ] ->
    let* addr = _sockaddr_of_struct sockaddr in
    Some { socket; addr = Some addr }
  | [ `String socket; `Const "NULL"; `Const "NULL" ] ->
    Some { socket; addr = None }
  | _ -> None

type _bind = {
  socket : string;
  addr : _sockaddr;
}

let _bind_of_base (base : base) : _bind option =
  match base.args with
  | [ `String socket; `Struct sockaddr; `Int _protocol ] ->
    let* addr = _sockaddr_of_struct sockaddr in
    Some { socket; addr }
  | _ -> None

type _listen = {
  socket : string;
}

let _listen_of_base (base : base) : _listen option =
  match base.args with
  | [ `String socket; `Int _backlog ] ->
    Some { socket }
  | _ -> None
