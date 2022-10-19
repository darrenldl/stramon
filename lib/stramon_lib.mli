module Path_access = Path_access

module Link_access = Link_access

module Summary = Summary

type 'a handler = 'a -> int -> Syscall.t -> 'a

type 'a monitor_handle = {
  run : unit -> 'a;
  cleanup : unit -> unit;
}

val init : unit -> unit

val monitor_summary :
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr -> 
  ?stderr:Unix.file_descr -> 
  string list ->
  (Summary.t monitor_handle, string) result

val monitor :
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr -> 
  ?stderr:Unix.file_descr -> 
  handlers:(string * 'a handler) list ->
  init_data:'a ->
  string list ->
  ('a monitor_handle, string) result
