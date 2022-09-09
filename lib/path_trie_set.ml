type t = unit Path_trie.t

let empty : t = Path_trie.empty

let add path t =
  Path_trie.add path () t

let mem path t : bool =
  Option.is_some (Path_trie.find path t)

