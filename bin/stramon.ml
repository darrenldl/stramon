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

let () =
  Stramon_lib.init ();
  Sys.catch_break true;
  Arg.parse speclist add_to_command usage_msg;
  let command = List.rev !command in
  let output_path =
    match !output_path with
    | "" -> Fmt.str "stramon_%a.log" pp_file_date_time (Timedesc.now ())
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
          Printf.printf "Error: Cannot write to %s\n" output_path;
          exit 1
        )
    );
    match Stramon_lib.monitor ~handlers:[] ~init_data:() command with
    | Error msg -> (
        Printf.printf "Error: %s\n" msg;
        exit 2
      )
    | Ok Stramon_lib.{ pipe_run; cleanup } -> (
        try
          let summary = pipe_run () in
          ()
        with
        | Sys.Break -> cleanup ()
      )
  )
