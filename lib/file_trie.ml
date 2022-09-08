type t = {
  children : file_trie String_map.t;
  is_terminal : bool;
}

let empty = { children = String_map.empty; is_terminal = false }

let add (path : string) (t : t) : t =
  let rec aux t parts =
    match parts with
    | [] -> { t with is_terminal = true }
    | x :: xs ->
      let children =
        String_map.find_opt x t.children
    |> Option.value ~default:empty
    |> (fun sub_trie ->
        String_map.add x (aux sub_trie xs) t.children
    )
      in
      { t with children }
  in
  if String.length path = 0 || path.[0] <> '/' then
    invalid_arg "add_path: expected absolute and non-empty path"
  else
    let parts =
      if path = "/" then []
      else (
        List.tl @@ String_utils.escaping_split ~on:'/' path
      )
    in
    aux t 
