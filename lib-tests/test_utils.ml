module Print_utils = struct
  let abs_path p =
    match p with
    | None -> ""
    | Some p ->
      Stramon_lib.Abs_path.to_parts p
      |> List.map (fun s -> Printf.sprintf "<%d:%s>" (String.length s) s)
      |> String.concat "|"

  let abs_path_int_pair (p, x) =
    Printf.sprintf "(%s, %d)" (abs_path p) x
end

let nz_small_nat_gen = QCheck.Gen.(map (( + ) 1) small_nat)

let nz_small_nat = QCheck.make nz_small_nat_gen

let abs_path_gen =
  QCheck.Gen.(map (fun l -> Stramon_lib.Abs_path.of_parts l)
                (list_size (int_bound 4) small_string))

let abs_path =
  QCheck.make ~print:Print_utils.abs_path abs_path_gen

let abs_path_int_pair_gen =
  QCheck.Gen.(pair abs_path_gen nz_small_nat_gen)

let abs_path_int_pair =
  QCheck.make ~print:Print_utils.abs_path_int_pair abs_path_int_pair_gen

let abs_path_int_pairs_gen =
  QCheck.Gen.(list abs_path_int_pair_gen)

let abs_path_int_pairs =
  QCheck.make ~print:QCheck.Print.(list Print_utils.abs_path_int_pair)
    abs_path_int_pairs_gen
