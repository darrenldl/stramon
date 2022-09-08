include Map.Make (struct
    type t = int

    let compare = Int.compare
  end)
