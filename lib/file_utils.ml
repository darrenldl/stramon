let kind_of_file (path : Abs_path.t) : Unix.file_kind option =
  try
    let stats = Unix.lstat (Abs_path.to_string path) in
    Some stats.st_kind
  with
  | _ -> None
