include Map.Make (struct
    type t = Stramon_lib.Abs_path.t

    let compare = Stramon_lib.Abs_path.compare
  end)
