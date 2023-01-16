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

let string_of_hex_string ?(preamble_before_each_byte = "") (s : string) : string option =
  let s_len = String.length s in
  let preamble_size = String.length preamble_before_each_byte in
  let rec preamble_is_okay s offset pos =
    pos >= preamble_size
    || (s.[offset] = preamble_before_each_byte.[pos]
        && preamble_is_okay s (offset + 1) (pos + 1))
  in
  let chunk_size = preamble_size + 2 in
  if s_len mod chunk_size <> 0 then
    None
  else
    let buf_len = s_len / chunk_size in
    let buf = Bytes.make buf_len '\x00' in
    let rec aux i =
      if i >= buf_len then
        Some (Bytes.to_string buf)
      else (
        let j = i * chunk_size in
        if preamble_is_okay s j 0 then (
          match int_of_hex_digit s.[j+preamble_size+0], int_of_hex_digit s.[j+preamble_size+1] with
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

let octal_of_string (s : string) : int64 option =
  let len = String.length s in
  let max_pos = len - 1 in
  let max_digit_count = 64 / 3 in
  let rec aux acc pos =
    if pos < 0 then
      Some acc
    else (
      match s.[pos] with
      | '0' .. '7' as c -> (
          let x = Int64.of_int (Char.code c - Char.code '0') in
          let y = Int64.shift_left x (3 * (max_pos - pos)) in
          aux (Int64.logor acc y) (pos - 1)
        )
      | _ -> None
    )
  in
  if len > max_digit_count || len = 0 then (
    None
  ) else (
    aux 0L (len - 1)
  )

let hex_of_string (s : string) : int64 option =
  let len = String.length s in
  let max_pos = len - 1 in
  let max_digit_count = 64 / 4 in
  let rec aux acc pos =
    if pos < 0 then
      Some acc
    else (
      match int_of_hex_digit s.[pos] with
      | Some x -> (
          let x = Int64.of_int x in
          let y = Int64.shift_left x (4 * (max_pos - pos)) in
          aux (Int64.logor acc y) (pos - 1)
        )
      | _ -> None
    )
  in
  if len > max_digit_count || len = 0 then (
    None
  ) else (
    aux 0L (len - 1)
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
