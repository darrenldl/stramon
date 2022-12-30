let command = ref []

let add_to_command x =
  command := x :: !command

let output_path = ref ""

let force_output = ref false

let no_link = ref false

let debug_level = ref "none"

let latest_link_name = "stramon-latest.json"

let speclist = Arg.[
    ("-o", Set_string output_path,
     {|JSON file output path, defaults to: stramon_DATE-TIME.json.
If provided path PATH is a directory, then output path is PATH/stramon_DATE-TIME.json|});
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

let _open_handler access (path : Stramon_lib.Abs_path.t) (flags : string list) =
  let l = List.filter (fun x ->
      x = "O_RDONLY"
      || x = "O_WRONLY"
      || x = "O_RDWR"
    ) flags
  in
  match l with
  | "O_RDONLY" :: _ -> (
      Path_access.add path `R access
    )
  | "O_WRONLY" :: _ -> (
      Path_access.add path `Rw access
    )
  | "O_RDWR" :: _ -> (
      Path_access.add path `Rw access
    )
  | _ -> access

let handlers =
  let open Stramon_lib in
  [ `_open (fun access _pid ({ path; flags; mode = _ } : Syscall._open) ->
        let path = Abs_path.of_string_exn path in
        _open_handler access path flags
      )
  ; `_openat (fun access _pid ({ relative_to; path; flags; mode = _ } : Syscall._openat) ->
        let cwd = Abs_path.of_string_exn relative_to in
        let path = Abs_path.of_string_exn ~cwd path in
        _open_handler access path flags
      )
  ]

let () =
  Sys.catch_break true;
  try
    Arg.parse speclist add_to_command usage_msg;
    let command = List.rev !command in
    let output_path, latest_link_path =
      if !output_path = "" then (
        (Fmt.str "stramon_%a.json" pp_file_date_time (Timedesc.now ()),
         latest_link_name
        )
      ) else (
        if Sys.file_exists !output_path
        && Sys.is_directory !output_path
        then (
          (Fmt.str "%s/stramon_%a.json" !output_path pp_file_date_time (Timedesc.now ()),
           Fmt.str "%s/%s" !output_path latest_link_name
          )
        ) else (
          (!output_path,
           Fmt.str "%s/%s" (Filename.dirname !output_path) latest_link_name
          )
        )
      )
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
      match Stramon_lib.monitor ~debug_level ~handlers ~init_data:Path_access.empty command with
      | Error msg -> (
          Printf.eprintf "Error: %s\n" msg;
          exit 2
        )
      | Ok res -> (
          (
            try
              CCIO.with_out output_path (fun oc ->
                  let stats = Stramon_lib.Monitor_result.stats res in
                  let access = Stramon_lib.Monitor_result.data res in
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
                 Sys.remove latest_link_path
               with
               | _ -> ()
              );
              (try
                 Unix.symlink output_path latest_link_path
               with
               | _ -> (
                   Printf.eprintf "Error: Failed to update symlink %s\n" latest_link_path;
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
