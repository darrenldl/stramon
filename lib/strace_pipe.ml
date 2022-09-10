type status =
  | Unfinished
  | Resumed
  | Complete

type line = {
  pid : int;
  text : string;
}

type read_result =
  | Line of line
  | Not_ready
  | Eof

module Parsers = struct
  open Angstrom
  open Parser_components

  let line_p =
    nat_zero
    >>= fun pid ->
    non_angle_string >>= fun text ->
    (choice [
        (string "<unfinished" *> non_angle_string *> char '>' *>
         return (text, Unfinished));
        (char '<' *> non_space_string *> spaces *> non_space_string *> string "resumed>" *>
         non_angle_string >>= fun text ->
         return (text, Resumed));
        (return (text, Complete));
      ]
    )
    >>| fun (text, status) ->
    (pid, text, status)
end

let read_line (ctx : Ctx.t) (pipe : in_channel) : read_result =
  match
    Angstrom.(parse_string ~consume:Consume.All) Parsers.line_p (input_line pipe)
  with
  | Ok (pid, text, status) -> (
      let proc_ctx = Ctx.get_proc_ctx ctx ~pid in
      match status with
      | Unfinished -> (
          Proc_ctx.set_unfinished_line proc_ctx text;
          Not_ready
        )
      | Resumed -> (
          match Proc_ctx.get_unfinished_line proc_ctx with
          | None -> Not_ready
          | Some s -> (
              Proc_ctx.clear_unfinished_line proc_ctx;
              Line { pid; text = s ^ text }
            )
        )
      | Complete -> (
          Proc_ctx.clear_unfinished_line proc_ctx;
          Line { pid; text }
        )
    )
  | Error _ -> Not_ready
  | exception _ -> Eof
