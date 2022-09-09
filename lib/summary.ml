type t = {
  file : Path_access.t;
  dir : Path_access.t;
  link : string Path_trie.t;
}

let empty : t =
  {
    file = Path_access.empty;
    dir = Path_access.empty;
    link = Path_access.empty;
  }
