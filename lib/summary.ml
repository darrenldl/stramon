type membership =
  | Is_in
  | Not_in

type file_access = {
  read : File_trie.t;
  write : File_trie.t;
}

type access_mode = [
  | `Read
  | `Write
]

type t = {
  file : file_access;
  dir : file_access;
  link : file_access;
}

let add_path (mode : access_mode) (path : string) (access : file_access) : file_access =
  match mode with
  | `Read -> { access with read = File_trie.add path access.read }
  | `Write -> { access with write = File_trie.add path access.write }

