open Test_utils

module Alco = struct
  let suite =
    [
    ]
end

module Qc = struct
  let to_parts_of_parts =
    QCheck.Test.make ~count:100_000 ~name:"to_parts_of_parts" abs_path
      (fun p ->
        QCheck.assume (Option.is_some p);
        let p = Option.get p in
         let p' = p
                  |> Stramon_lib.Abs_path.to_parts
                  |> Stramon_lib.Abs_path.of_parts
                  |> Option.get
         in
         Stramon_lib.Abs_path.equal p p'
      )

  let to_string_of_string =
    QCheck.Test.make ~count:100_000 ~name:"to_string_of_string" abs_path
      (fun p ->
        QCheck.assume (Option.is_some p);
        let p = Option.get p in
         let p' = p
                  |> Stramon_lib.Abs_path.to_string
                  |> Stramon_lib.Abs_path.of_string
                  |> Option.get
         in
         Stramon_lib.Abs_path.equal p p'
      )

  let suite =
    [
      to_parts_of_parts;
      to_string_of_string;
    ]
end
