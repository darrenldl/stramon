type entry = {
  syscall_name : string;
  typ_name : string;
}

let make_entry ?typ_name (syscall_name : string) =
  let typ_name = Option.value ~default:syscall_name typ_name in
  { syscall_name; typ_name }

let l : entry list = [
  make_entry "open_";
  make_entry "openat";
  make_entry "read";
  make_entry "chown";
  make_entry ~typ_name:"chown" "fchown";
  make_entry ~typ_name:"chown" "lchown";
  make_entry "fchownat";
  make_entry "chmod";
  make_entry ~typ_name:"chmod" "fchmod";
  make_entry "fchmodat";
  make_entry "stat";
  make_entry ~typ_name:"stat" "fstat";
  make_entry ~typ_name:"stat" "lstat";
  make_entry ~typ_name:"fstatat" "fstatat64";
  make_entry ~typ_name:"fstatat" "newfstatat";
  make_entry "socket";
  make_entry "connect";
  make_entry "accept";
  make_entry "bind";
  make_entry "listen";
]

let output_path = Sys.argv.(1)

let () =
  CCIO.with_out output_path (fun oc ->
      Printf.fprintf oc "open Option_let\n";
      Printf.fprintf oc "open Syscall\n";
      Printf.fprintf oc "\n";

      Printf.fprintf oc "type 'a handler = [\n";
      List.iter (fun { syscall_name; typ_name } ->
          Printf.fprintf oc "| `%s of 'a -> int -> %s -> 'a\n" syscall_name typ_name
        ) l;
      Printf.fprintf oc "]\n\n";

      Printf.fprintf oc "type 'a base_handler = 'a -> int -> base -> 'a option\n\n";

      Printf.fprintf oc {|let base_handler_of_handler (f : 'a handler) : string * 'a base_handler =
      match f with
      |};
      List.iter (fun { syscall_name; typ_name } ->
          let syscall_name' =
            Option.value ~default:syscall_name
            @@ CCString.chop_suffix ~suf:"_" syscall_name
          in
          let typ_name' =
            Option.value ~default:typ_name
            @@ CCString.chop_suffix ~suf:"_" typ_name
          in
          Printf.fprintf oc {|
      | `%s f -> ("%s",
                   (fun ctx pid base ->
                      let+ x = %s_of_base base in
                      f ctx pid x
                   ))
      |} syscall_name syscall_name' typ_name';
        ) l;
    )
