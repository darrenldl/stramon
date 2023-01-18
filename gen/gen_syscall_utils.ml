let l = [
  "open_";
  "openat";
  "read";
  "chown";
  "chmod";
  "stat";
  "socket";
  "connect";
  "accept";
  "bind";
  "listen";
]

let output_path = Sys.argv.(1)

let () =
  CCIO.with_out output_path (fun oc ->
      Printf.fprintf oc "open Option_infix\n";
      Printf.fprintf oc "open Syscall\n";
      Printf.fprintf oc "\n";

      Printf.fprintf oc "type 'a handler = [\n";
      List.iter (fun s ->
          Printf.fprintf oc "| `%s of 'a -> int -> %s -> 'a\n" s s
        ) l;
      Printf.fprintf oc "]\n\n";

      Printf.fprintf oc "type 'a base_handler = 'a -> int -> base -> 'a option\n\n";

      Printf.fprintf oc {|let base_handler_of_handler (f : 'a handler) : string * 'a base_handler =
      match f with
      |};
      List.iter (fun s ->
          let s' = Option.value ~default:s @@ CCString.chop_suffix ~suf:"_" s in
          Printf.fprintf oc {|
      | `%s f -> ("%s",
                   (fun ctx pid base ->
                      let+ x = %s_of_base base in
                      f ctx pid x
                   ))
      |} s s' s';
        ) l;
    )
