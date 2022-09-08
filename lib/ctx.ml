type t = {
  unfinished_lines : (int, string) Hashtbl.t;
  summary : Summary.t ref;
}

let make () : t =
  {
    unfinished_lines = Hashtbl.create 100;
    summary = ref Summary.empty;
  }
