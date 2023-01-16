let count_backslash_backward s i =
  if i < 0 || i >= String.length s then (
    invalid_arg "count_backslash_backward: invalid i"
  );
  let rec aux i e =
    if i < 0 || s.[i] <> '\\' then
      e - i
    else
      aux (i - 1) e
  in
  aux i i

let has_trailing_escape s =
  let len = String.length s in
  count_backslash_backward s (len - 1) mod 2 = 1

let concat_file_names names =
  let splits =
    names
    |> List.map (fun s ->
        String.split_on_char '/' s)
    |> List.concat
    |> List.filter (fun s -> s <> "")
  in
  let res = String.concat Filename.dir_sep splits in
  match names with
  | [] -> res
  | x :: _ -> if String.sub x 0 1 = "/" then "/" ^ res else res

let int_of_hex_digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'A' .. 'F' -> Some (0xA + (Char.code c - Char.code 'A'))
  | 'a' .. 'f' -> Some (0xA + (Char.code c - Char.code 'a'))
  | _ -> None

let string_of_hex_string (s : string) : string option =
  let s_len = String.length s in
  if s_len mod 4 <> 0 then
    None
  else
    let buf_len = s_len / 4 in
    let buf = Bytes.make buf_len '\x00' in
    let rec aux i =
      if i >= buf_len then
        Some (Bytes.to_string buf)
      else (
        let j = i * 4 in
        if s.[j+0] = '\\'
        && s.[j+1] = 'x'
        then (
          match int_of_hex_digit s.[j+2], int_of_hex_digit s.[j+3] with
          | Some a, Some b ->
            Bytes.set buf i (Char.chr (a * 0x10 + b));
            aux (succ i)
          | _, _ ->
            None
        )
        else
          None
      )
    in
    aux 0

let octal_of_string (s : string) : int option =
  let len = String.length s in
  let max_pos = len - 1 in
  let max_digit_count = Sys.int_size / 3 - 1 in
  let rec aux acc pos =
    if pos < 0 then
      Some acc
    else (
      match s.[pos] with
      | '0' .. '7' as c -> (
          let x = Char.code c - Char.code '0' in
          let y = x lsl (3 * (max_pos - pos)) in
          aux (acc lor y) (pos - 1)
        )
      | _ -> None
    )
  in
  if len > max_digit_count || len = 0 then (
    None
  ) else (
    aux 0 (len - 1)
  )

let hex_of_string (s : string) : int option =
  let len = String.length s in
  let max_pos = len - 1 in
  let max_digit_count = Sys.int_size / 4 - 1 in
  let rec aux acc pos =
    if pos < 0 then
      Some acc
    else (
      match int_of_hex_digit s.[pos] with
      | Some x -> (
          let y = x lsl (4 * (max_pos - pos)) in
          aux (acc lor y) (pos - 1)
        )
      | _ -> None
    )
  in
  if len > max_digit_count || len = 0 then (
    None
  ) else (
    aux 0 (len - 1)
  )

let find_char ?(start = 0) (c : char) (s : string) : int option =
  let str_len = String.length s in
  let rec aux i =
    if i >= str_len then None
    else
    if c = s.[i] then Some i
    else
      aux (succ i)
  in
  aux start

let find_char_rev ?(start : int option) (c : char) (s : string) : int option =
  let str_len = String.length s in
  let start = Option.value ~default:(str_len - 1) start  in
  let rec aux i =
    if i <= 0 then None
    else
    if c = s.[i] then Some i
    else
      aux (pred i)
  in
  aux start
