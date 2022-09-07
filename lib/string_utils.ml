let escaping_split ?(escape_char = '\\') ~on s =
  let acc = ref [] in
  let rec aux seg_start i =
    if i < String.length s then (
      let escaped =
        i > 1 && s.[i-1] = escape_char
      in
      if escaped then
        aux seg_start (succ i)
      else (
        if s.[i] = on then (
          acc := (String.sub s seg_start (i - seg_start)) :: !acc;
          aux (succ i) (succ i)
        ) else
          aux seg_start (succ i)
      )
    ) else (
      acc := (String.sub s seg_start (i - seg_start)) :: !acc
    )
  in
  aux 0 0;
  List.rev !acc

let concat_file_names names =
  let splits =
    names
    |> List.map (fun s ->
        escaping_split ~on:'/' ~escape_char:'\\' s)
    |> List.concat
    |> List.filter (fun s -> s <> "")
  in
  let res = String.concat Filename.dir_sep splits in
  match names with
  | [] -> res
  | x :: _ -> if String.sub x 0 1 = "/" then "/" ^ res else res
