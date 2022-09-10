type t = {
  mutable unfinished_line : string option;
  mutable cwd : Abs_path.t;
  path_fds : (int, Abs_path.t) Hashtbl.t;
}

let make () : t =
  {
    unfinished_line = None;
    cwd = Option.get
        (Abs_path.(of_string ~cwd:root)
           (Unix.getcwd ()));
    path_fds = Hashtbl.create 100;
  }

let set_unfinished_line t text =
  t.unfinished_line <- Some text

let clear_unfinished_line t =
  t.unfinished_line <- None

let get_unfinished_line t =
  t.unfinished_line

let set_cwd t path =
  t.cwd <- path

let get_cwd t =
  t.cwd

let add_path_fd t ~fd path =
  Hashtbl.replace t.path_fds fd path

let remove_path_fd t ~fd =
  Hashtbl.remove t.path_fds fd
