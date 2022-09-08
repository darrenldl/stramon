type t = {
  file : Path_access.t;
  dir : Path_access.t;
  link : Path_access.t;
}

let empty : t =
  {
    file = Path_access.empty;
    dir = Path_access.empty;
    link = Path_access.empty;
  }
