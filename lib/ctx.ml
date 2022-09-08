type t = {
  unfinished_lines : (int, string) Hashtabl.t;
  summary : ref Summary.t;
}

let make () : t =
  {
    unfinished_lines = Hashtabl.create 100;
  }


