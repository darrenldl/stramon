module Print_utils = struct
  let abs_path p =
    match p with
    | None -> ""
    | Some p ->
      Stramon_lib.Abs_path.to_parts p
      |> List.map (fun s -> Printf.sprintf "<%d:%s>" (String.length s) s)
      |> String.concat "|"
end

let nz_small_nat_gen = QCheck.Gen.(map (( + ) 1) small_nat)

let nz_small_nat = QCheck.make nz_small_nat_gen

let abs_path_gen =
  QCheck.Gen.(map (fun l -> Stramon_lib.Abs_path.of_parts l)
                (list_size (int_bound 4) small_string))

let abs_path =
  QCheck.make ~print:Print_utils.abs_path abs_path_gen
