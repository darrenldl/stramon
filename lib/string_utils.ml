let escaping_split_on_slash s =
  let acc = ref [] in
  let rec aux seg_start i =
    if i < String.length s then (
      let escaped =
        i >= 1 && s.[i-1] = '\\'
      in
      if escaped then
        aux seg_start (i + 1)
      else (
        if s.[i] = '/' then (
          acc := (String.sub s seg_start (i - seg_start)) :: !acc;
          aux (i + 1) (i + 1)
        ) else
          aux seg_start (i + 1)
      )
    ) else (
      acc := (String.sub s seg_start (i - seg_start)) :: !acc
    )
  in
  aux 0 0;
  List.rev !acc

let escape_slash_if_not_already s =
  let parts = escaping_split_on_slash s in
  String.concat "\\/" parts

let remove_trailing_escapes s =
  let rec aux i =
    if i < 0 then
      ""
    else (
      if s.[i] = '\\' then
        aux (i - 1)
      else
        String.sub s 0 (i + 1)
    )
  in
  aux (String.length s - 1)

let concat_file_names names =
  let splits =
    names
    |> List.map (fun s ->
        escaping_split_on_slash s)
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
