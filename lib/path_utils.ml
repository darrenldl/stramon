let abs_path_of_path ~(cwd : string) (path : string) =
  if path.[0] = '/' then path
  else
    Filename.concat cwd path
