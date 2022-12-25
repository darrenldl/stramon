type t = {
  parts : string list;
}

let compare (x : t) (y : t) =
  List.compare String.compare x.parts y.parts

let equal x y =
  compare x y = 0

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
      | _ -> aux (x :: acc) xs
  in
  parts
  |> List.map String_utils.escape_slash_if_not_already
  |> List.map String_utils.remove_trailing_escape
  |> aux []

let of_parts_exn parts =
  match of_parts parts with
  | None -> invalid_arg "of_parts_exn: of_parts failed"
  | Some x -> x

let of_string ?(cwd = root) (path : string) : t option =
  let parts = String_utils.escaping_split_on_slash path in
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
  String.concat "/" ("" :: t.parts)
