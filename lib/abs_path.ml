type t = {
  parts : string list;
}

let equal x y =
  List.equal String.equal
    x.parts y.parts

let root = { parts = [] }

let parts t = t.parts

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
  aux [] parts

let of_string ?(cwd = root) (path : string) : t option =
  let parts = String_utils.escaping_split ~on:'/' path
              |> List.rev
              |> (fun l ->
                  match l with
                  | "" :: l -> l
                  | _ -> l
                )
              |> List.rev
  in
  let parts =
    if path.[0] = '/' then parts
    else
      cwd.parts @ parts
  in
  of_parts parts

let to_string (t : t) : string =
  "/" ^ (String.concat "/" t.parts)
