type t = {
  mutable unfinished_line : string option;
}

let make () : t =
  {
    unfinished_line = None;
  }

let set_unfinished_line t text =
  t.unfinished_line <- Some text

let clear_unfinished_line t =
  t.unfinished_line <- None

let get_unfinished_line t =
  t.unfinished_line
