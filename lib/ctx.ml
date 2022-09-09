type t = {
  unfinished_line : (int, string) Hashtbl.t;
  cwd : (int, Abs_path.t) Hashtbl.t;
  path_fd : (int, (int, Abs_path.t) Hashtbl.t) Hashtbl.t;
  summary : Summary.t ref;
}

let make () : t =
  {
    unfinished_line = Hashtbl.create 100;
    cwd = Hashtbl.create 100;
    path_fd = Hashtbl.create 100;
    summary = ref Summary.empty;
  }

let summary t = !(t.summary)

let set_unfinished_line ~pid text t =
  Hashtbl.replace t.unfinished_line pid text

let clear_unfinished_line ~pid t =
  Hashtbl.remove t.unfinished_line pid

let get_unfinished_line ~pid t =
  Hashtbl.find_opt t.unfinished_line pid

let set_cwd ~pid path t =
  Hashtbl.replace t.cwd pid path

let get_cwd ~pid t =
  match 
    Hashtbl.find_opt t.cwd pid
  with
  | None -> (
      let cwd =
        Option.get
          (Abs_path.(of_string ~cwd:root)
             (Unix.getcwd ()))
      in
      set_cwd ~pid cwd t;
      cwd
    )
  | Some cwd -> cwd

let add_path_fd ~pid ~fd path t =
  match Hashtbl.find_opt t.path_fd pid with
  | None -> (
      let tbl : (int, Abs_path.t) Hashtbl.t =
        Hashtbl.create 100
      in
      Hashtbl.add tbl fd path;
      Hashtbl.add t.path_fd pid tbl
    )
  | Some tbl ->
    Hashtbl.replace tbl fd path

let remove_path_fd ~pid ~fd t =
  match Hashtbl.find_opt t.path_fd pid with
  | None -> ()
  | Some tbl ->
    Hashtbl.remove tbl fd
