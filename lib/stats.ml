type t = {
  syscall_count : int String_map.t;
}

let empty =
  { syscall_count = String_map.empty;
  }

let record_syscall (name : string) (t : t) : t =
  let syscall_count =
    match String_map.find_opt name t.syscall_count with
    | None -> String_map.add name 1 t.syscall_count
    | Some n -> String_map.add name (n + 1) t.syscall_count
  in
  { t with syscall_count }
