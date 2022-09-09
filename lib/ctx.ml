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

let set_unfinished_line ~pid text t =
  Hashtbl.replace t.unfinished_line pid text

let clear_unfinished_line ~pid t =
  Hashtbl.remove t.unfinished_line pid

let get_unfinished_line ~pid t =
  Hashtbl.find_opt t.unfinished_line pid

let set_cwd ~pid path t =
  Hashtbl.replace t.cwd pid path

let get_cwd ~pid t =
  Option.value
    ~default:(Unix.getcwd ())
    (Hashtbl.find_opt t.cwd pid)
