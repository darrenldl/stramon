open Angstrom

let is_letter c =
  match c with
  | 'A'..'Z'
  | 'a'..'z' -> true
  | _ -> false

let is_space c =
  match c with
  | ' ' -> true
  | '\t' -> true
  | '\n' -> true
  | _ -> false

let is_not_space c =
  not (is_space c)

let is_digit c =
  match c with
  | '0'..'9' -> true
  | _ -> false

let is_hex_digit c =
  match c with
  | '0'..'9'
  | 'A'..'F'
  | 'a'..'f' -> true
  | _ -> false

let digit : char t =
  satisfy is_digit

let alpha_string : string t =
  take_while1 is_letter

let is_angle c =
  match c with
  | '<' | '>' -> true
  | _ -> false

let is_not_angle c =
  not (is_angle c)

let non_angle_string =
  take_while1 is_not_angle

let any_string : string t = take_while1 (fun _ -> true)

let optional_char target =
  peek_char
  >>= (fun c ->
      match c with
      | None -> return ()
      | Some c ->
        if c = target then
          any_char *> return ()
        else
          return ()
    )

let ident_string : string t =
  take_while1 (fun c ->
      is_letter c
      || is_digit c
      || c = '_'
    )

let skip_non_num_string ~end_markers =
  skip_while (fun c ->
      not (is_digit c)
      &&
      (match end_markers with
       | None -> true
       | Some x -> not (String.contains x c))
    )

let num_string : string t =
  take_while1 is_digit

let hex_num_string : string t =
  take_while1 is_hex_digit

let nat_zero : int t =
  num_string
  >>= fun s ->
  try return (int_of_string s)
  with _ -> fail (Printf.sprintf "Integer %s is out of range" s)

let nat_zero_int64 : int64 t =
  num_string
  >>= fun s ->
  try return (Int64.of_string s)
  with _ -> fail (Printf.sprintf "Integer %s is out of range" s)

let float_non_neg : float t =
  take_while1 is_digit
  >>= fun x ->
  char '.'
  *> take_while1 is_digit
  >>= fun y ->
  let s = x ^ "." ^ y in
  try return (float_of_string s)
  with _ -> fail (Printf.sprintf "Float %s is out of range" s)

let comma : char t = char ','

let dot : char t = char '.'

let hyphen : char t = char '-'

let non_square_bracket_string : string t =
  take_while (function '[' | ']' -> false | _ -> true)

let non_curly_bracket_string : string t =
  take_while (function '{' | '}' -> false | _ -> true)

let non_parenthesis_string : string t =
  take_while (function '(' | ')' -> false | _ -> true)

let non_space_string : string t = take_while1 is_not_space

let non_quote_string : string t =
  take_while (function '"' -> false | _ -> true)

let spaces = skip_while is_space

let spaces1 = take_while is_space *> return ()

let sep_by_comma (p : 'a t) : 'a list t =
  sep_by (spaces *> comma *> spaces) p

let sep_by_comma1 (p : 'a t) : 'a list t =
  sep_by1 (spaces *> comma *> spaces) p

let invalid_syntax ~text ~pos =
  fail (Printf.sprintf "Invalid syntax: %s, pos: %d" text pos)

let extraneous_text_check ~end_markers =
  spaces
  *> pos
  >>= fun pos ->
  take_while (fun c -> not (String.contains end_markers c))
  >>= fun s ->
  match s with "" -> return () | text -> invalid_syntax ~text ~pos
