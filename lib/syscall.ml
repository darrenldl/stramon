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

type syscall = {
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

  let decoded_p =
    non_angle_string *> char '<' *> hex_string_p non_angle_string >>= fun s ->
    char '>' *> return s

  let blob_ret_p =
    spaces *>
    (decoded_p <|> non_space_string) >>= fun ret ->
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
    choice [
      (decoded_p >>= fun s -> return (String s));
      (string "0x" *> non_space_string >>| fun s -> Pointer s);
      (nat_zero >>| fun n -> Int n);
      (char '"' *> hex_string_p non_quote_string >>= fun s ->
       char '"' *> return (String s)
      );
      (sep_by1 (char '|') non_space_string >>| fun l -> Flags l);
      (non_space_string >>| fun s -> Const s);
    ]

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

let syscall_of_blob ({ name; arg_text; ret; errno; errno_msg } : blob) : syscall option =
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
