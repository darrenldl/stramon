type t = {
  parts : string list;
}

let compare (x : t) (y : t) =
  CCList.compare String.compare x.parts y.parts

let equal x y =
  CCList.equal String.equal x.parts y.parts

let root = { parts = [] }

let to_parts t = t.parts

let of_parts (parts : string list) : t option =
  let rec aux acc parts =
    match parts with
    | [] -> Some { parts = List.rev acc }
    | x :: xs ->
      match x with
      | "." | "" -> aux acc xs
      | ".." -> (
          match acc with
          | [] -> None
          | _ :: ys ->
            aux ys xs
        )
      | _ -> (
          if String_utils.has_trailing_escape x
          || String.contains x '/'
          then
            None
          else
            aux (x :: acc) xs
        )
  in
  aux [] parts

let of_parts_exn parts =
  match of_parts parts with
  | None -> invalid_arg "of_parts_exn: of_parts failed"
  | Some x -> x

let of_string ?(cwd = root) (path : string) : t option =
  let parts = String.split_on_char '/' path in
  let parts =
    match parts with
    | "" :: parts -> parts
    | _ ->
      cwd.parts @ parts
  in
  of_parts parts

let of_string_exn ?cwd s =
  match of_string ?cwd s with
  | None -> invalid_arg "of_string_exn: of_string failed"
  | Some x -> x

let to_string (t : t) : string =
  match t.parts with
  | [] -> "/"
  | _ -> String.concat "/" ("" :: t.parts)
