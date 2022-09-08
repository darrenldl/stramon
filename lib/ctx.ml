type t = {
  unfinished_line : (int, string) Hashtbl.t;
  cwd : (int, string) Hashtbl.t;
  summary : Summary.t ref;
}

let make () : t =
  {
    unfinished_line = Hashtbl.create 100;
    cwd = Hashtbl.create 100;
    summary = ref Summary.empty;
  }
