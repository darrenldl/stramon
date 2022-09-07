let escaping_split s =
  let acc = ref [] in
  let rec aux seg_start i =
    if i < String.length s then (
      let escaped =
        i > 1 && s.[i-1] = '\\'
      in
      if escaped then
        aux seg_start (succ i)
      else (
        if s.[i] = '/' then (
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
