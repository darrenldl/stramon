let command = ref []

let add_to_command x =
  command := x :: !command

let output_path = ref ""

let force_output = ref false

let no_link = ref false

let debug_level = ref "none"

let latest_link_name = "stramon-latest.json"

let speclist = Arg.[
    ("-o", Set_string output_path, "Output JSON file");
    ("-f", Set force_output, "Force overwrite of output file");
    ("--no-link", Set no_link, Fmt.str "Disable adding/updating symlink %s" latest_link_name);
    ("--debug-level", Set_string debug_level, "Debug level, one of: none, registered, all");
    ("--", Rest add_to_command, "");
  ]

let usage_msg = "stramon [-o JSON_OUTPUT] -- prog arg ..."

let pp_file_date_time =
  Timedesc.pp
    ~format:"{year}-{mon:0X}-{day:0X}-{hour:0X}{min:0X}{sec:0X}" ()

let write_json (oc : out_channel) (json : Yojson.Basic.t) : unit =
  Yojson.Basic.to_channel oc json

let access = Path_access.make ()

let _open_handler (path : Stramon_lib.Abs_path.t) (flags : string list) : unit =
  let l = List.filter (fun x ->
      x = "O_RDONLY"
      || x = "O_WRONLY"
      || x = "O_RDWR"
    ) flags
  in
  match l with
  | "O_RDONLY" :: _ -> (
      Path_access.add access path `R
    )
  | "O_WRONLY" :: _ -> (
      Path_access.add access path `Rw
    )
  | "O_RDWR" :: _ -> (
      Path_access.add access path `Rw
    )
  | _ -> ()

let handlers =
  let open Stramon_lib in
  [ `_open (fun () _pid ({ path; flags; mode = _ } : Syscall._open) ->
        let path = Abs_path.of_string_exn path in
        _open_handler path flags
      )
  ; `_openat (fun () _pid ({ relative_to; path; flags; mode = _ } : Syscall._openat) ->
        let cwd = Abs_path.of_string_exn relative_to in
        let path = Abs_path.of_string_exn ~cwd path in
        _open_handler path flags
      )
  ]

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
    let debug_level =
      match !debug_level with
      | "none" -> `None
      | "registered" -> `Registered
      | "all" -> `All
      | _ -> (
          Printf.eprintf "Error: Unrecognized debug level %s\n" !debug_level;
          exit 1
        )
    in
    if Sys.file_exists output_path && not !force_output then (
      Printf.eprintf "Error: File %s already exists\n" output_path;
      exit 1
    ) else (
      (
        try
          CCIO.with_out output_path (fun _ -> ())
        with
        | _ -> (
            Printf.eprintf "Error: Cannot open %s during test open\n" output_path;
            exit 1
          )
      );
      match Stramon_lib.monitor ~debug_level ~handlers ~init_data:() command with
      | Error msg -> (
          Printf.eprintf "Error: %s\n" msg;
          exit 2
        )
      | Ok res -> (
          (
            try
              CCIO.with_out output_path (fun oc ->
                  let stats = Stramon_lib.Monitor_result.stats res in
                  let summary = Summary.make stats access in
                  let json = Summary.to_json summary in
                  write_json oc json
                )
            with
            | _ -> (
                Printf.eprintf "Error: Failed to write to %s\n" output_path;
                exit 1
              )
          );
          if not !no_link then (
            if Unix.has_symlink () then (
              (try
                 Sys.remove latest_link_name
               with
               | _ -> ()
              );
              (try
                 Unix.symlink output_path latest_link_name
               with
               | _ -> (
                   Printf.eprintf "Error: Failed to update symlink %s\n" latest_link_name;
                 )
              )
            ) else (
              Printf.eprintf "Error: Process cannot create symlink\n";
              exit 1
            )
          )
        )
    )
  with
  | Sys.Break -> (
      Printf.eprintf "Ctrl-C received\n";
      exit 1
    )
