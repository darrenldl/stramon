let l = [
  "open";
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
          Printf.fprintf oc "| `_%s of 'a -> int -> _%s -> 'a\n" s s
        ) l;
      Printf.fprintf oc "]\n\n";

      Printf.fprintf oc "type 'a base_handler = 'a -> int -> base -> 'a option\n\n";

      Printf.fprintf oc {|let base_handler_of_handler (f : 'a handler) : string * 'a base_handler =
      match f with
      |};
      List.iter (fun s ->
          Printf.fprintf oc {|
      | `_%s f -> ("%s",
                   (fun ctx pid base ->
                      let+ x = _%s_of_base base in
                      f ctx pid x
                   ))
      |} s s s;
        ) l;
    )
