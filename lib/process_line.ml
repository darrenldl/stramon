type status =
  | Unfinished
  | Resumed
  | Complete

type line = {
  pid : int;
  text : string;
  status : status;
}

module Parsers = struct
  open Angstrom
  open Parser_components

  let line_p : line t =
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
    { pid; text; status }
end

(* let process_line (ctx : Ctx.t) (pipe : in_channel) :  *)

