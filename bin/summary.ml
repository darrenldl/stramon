type t = {
  file : Path_access.t;
  dir : Path_access.t;
  link : Link_access.t;
}

let empty : t =
  {
    file = Path_access.empty;
    dir = Path_access.empty;
    link = Link_access.empty;
  }