open Option_infix

type blob = {
  name : string;
  arg_text : string;
  ret : string;
  errno : string option;
  errno_msg : string option;
}

type term =
  | String of string
  | Int of int
  | Pointer of string
  | Struct of (string * term) list
  | Const of string
  | Flags of string list
  | Array of term list
  | App of string * term list

let int_of_term (term : term) : int option =
  match term with
  | Int x -> Some x
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
    match String_utils.string_of_hex_string s with
    | None -> fail "invalid hex string"
    | Some s -> return s

  let nat_zero_octal : int t =
    char '0' *> num_string
    >>= fun s ->
    match String_utils.octal_of_string s with
    | None -> fail "invalid octal number"
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

  let term_p : term t =
    fix (fun p ->
        choice [
          (decoded_p >>= fun s -> return (String s));
          (string "0x" *> non_space_string >>| fun s -> Pointer s);
          (nat_zero_octal >>| fun n -> Int n);
          (nat_zero >>| fun n -> Int n);
          (char '"' *> hex_string_p non_quote_string >>= fun s ->
           char '"' *> return (String s)
          );
          (sep_by1 (char '|') ident_string >>| fun l ->
           match l with
           | [x] -> Const x
           | l -> Flags l
          );
          (char '{' *> spaces *>
           sep_by_comma (ident_string >>= fun k ->
                         spaces *> char '=' *> spaces *>
                         p >>| fun v -> (k, v)) >>= fun l ->
           spaces *> char '}' *>
           return (Struct l)
          );
          (char '[' *>
           sep_by_comma p >>= fun l ->
           char ']' *>
           return (Array l)
          );
          (ident_string >>= fun name ->
           char '(' *>
           sep_by_comma p >>= fun l ->
           char ')' *>
           return (App (name, l))
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

let base_of_blob ({ name; arg_text; ret; errno; errno_msg } : blob) : base option =
  match
    Angstrom.(parse_string ~consume:Consume.All) Parsers.args_p arg_text
  with
  | Error _ -> None
  | Ok args ->
    match
      Angstrom.(parse_string ~consume:Consume.All) Parsers.ret_p ret
    with
    | Error _ -> None
    | Ok ret ->
      Some { name; args; ret; errno; errno_msg }

let pp_term (formatter : Format.formatter) (x : term) =
  let rec aux formatter x =
    match x with
    | String s -> Fmt.pf formatter "<string:%S>" s
    | Int x -> Fmt.pf formatter "<int:%d>" x
    | Pointer s -> Fmt.pf formatter "<ptr:%s>" s
    | Struct l ->
      Fmt.pf formatter "<struct:{%a}>"
        Fmt.(list (fun formatter (s, x) ->
            Fmt.pf formatter "%s=%a," s aux x
          ))
        l
    | Const s ->
      Fmt.pf formatter "<const:%s>" s
    | Flags l ->
      Fmt.pf formatter "<flags:%a>"
        Fmt.(list
               ~sep:(fun formatter () -> Fmt.pf formatter "|")
               string)
        l
    | Array l ->
      Fmt.pf formatter "<array:[%a]>"
        Fmt.(list ~sep:comma aux)
        l
    | App (f, l) ->
      Fmt.pf formatter "<app:%s(%a)>"
        f
        Fmt.(list ~sep:comma aux) l
  in
  aux formatter x

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
  flags : string list;
  mode : string list;
  ret : int;
}

let _open_of_base (base : base) : _open option =
  let* ret = int_of_term base.ret in
  match base.args with
  | [ String path; Flags flags ] -> Some { path; flags; mode = []; ret }
  | [ String path; Flags flags; Flags mode ] -> Some { path; flags; mode; ret }
  | _ -> None

type _openat = {
  relative_to : string;
  path : string;
  flags : string list;
  mode : string list;
}

let _openat_of_base (base : base) : _openat option =
  match base.args with
  | [ String relative_to; String path; Flags flags ] ->
    Some { relative_to; path; flags; mode = [] }
  | [ String relative_to; String path; Flags flags; Flags mode ] ->
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
  match base.ret with
  | Int byte_count_read -> (
      match base.args with
      | [ String path; _; Int byte_count_requested ] ->
        Some { path; byte_count_requested; byte_count_read; errno; errno_msg }
      | _ -> None
    )
  | _ -> None

type _socket = {
  domain : string;
  typ : string list;
  protocol : int;
  errno : string option;
  errno_msg : string option;
}

let _socket_of_base (base : base) : _socket option =
  let errno = base.errno in
  let errno_msg = base.errno_msg in
  match base.args with
  | [ Const domain; Flags typ; Int protocol ] -> (
      Some { domain; typ; protocol; errno; errno_msg }
    )
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
  | [ String path; Int owner; Int group ] -> Some { path; owner; group; ret }
  | _ -> None

type _chmod = {
  path : string;
  mode : int;
  ret : int;
}

let _chmod_of_base (base : base) : _chmod option =
  let* ret = int_of_term base.ret in
  match base.args with
  | [ String path; Int mode ] -> Some { path; mode; ret }
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
  | [ String path; Struct status ] -> (
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

