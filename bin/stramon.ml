let command = ref []

let add_to_command x =
  command := x :: !command

let output_path = ref ""

let force_output = ref false

let speclist = Arg.[
    ("-o", Set_string output_path, "Output JSON file");
    ("-f", Set force_output, "Force overwrite of output file");
    ("--", Rest add_to_command, "");
  ]

let usage_msg = "stramon [-o JSON_OUTPUT] -- prog arg ..."

let pp_file_date_time =
  Timedesc.pp
    ~format:"{year}-{mon:0X}-{day:0X}-{hour:0X}{min:0X}{sec:0X}" ()

let handlers =
  [
  ]

let json_of_stats (stats : Stramon_lib.Stats.t) : Yojson.Basic.t =
  let l = Stramon_lib.Stats.syscall_count stats
          |> List.map (fun (s, i) -> (s, `Int i))
  in
  `Assoc l

let write_json (oc : out_channel) (json : Yojson.Basic.t) : unit =
  Yojson.Basic.to_channel oc json

let () =
  Stramon_lib.init ();
  Sys.catch_break true;
  try
    Arg.parse speclist add_to_command usage_msg;
    let command = List.rev !command in
    let output_path =
      match !output_path with
      | "" -> Fmt.str "stramon_%a.json" pp_file_date_time (Timedesc.now ())
      | s -> s
    in
    if Sys.file_exists output_path && not !force_output then (
      Printf.printf "Error: File %s already exists\n" output_path;
      exit 1
    ) else (
      (
        try
          CCIO.with_out output_path (fun _ -> ())
        with
        | _ -> (
            Printf.printf "Error: Cannot open %s during test open\n" output_path;
            exit 1
          )
      );
      match Stramon_lib.monitor ~handlers:[] ~init_data:() command with
      | Error msg -> (
          Printf.printf "Error: %s\n" msg;
          exit 2
        )
      | Ok res -> (
          try
            CCIO.with_out output_path (fun oc ->
                let stats = Stramon_lib.Monitor_result.stats res in
                let json =
                  `Assoc
                    [
                      ("stats", json_of_stats stats);
                    ]
                in
                write_json oc json
              )
          with
          | _ -> (
              Printf.printf "Error: Failed to write to %s\n" output_path;
              exit 1
            )
        )
    )
  with
  | Sys.Break -> (
      Printf.printf "Ctrl-C received\n";
      exit 1
    )
