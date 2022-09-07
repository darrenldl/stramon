let init () =
  Random.self_init ()

type membership =
  | Is_in
  | Not_in

type file_tree =
  | Leaf
  | Branch of (membership * file_tree) String_map.t

type fs_root = file_tree String_map.t

type file_access = {
  read : fs_root;
  write : fs_root;
}

type summary = {
  file : file_access;
  dir : file_access;
  link : file_access;
}

let profile (cmd : string list) =
  Proc_utils.exec
