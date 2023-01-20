let command = ref []

let add_to_command x =
  command := x :: !command

let output_path = ref ""

let force_output = ref false

let no_link = ref false

let debug = ref false

let latest_link_name = "stramon-latest.json"

let speclist = Arg.[
    ("-o", Set_string output_path,
     {|JSON file output path, defaults to: stramon_DATE-TIME.json.
If provided path PATH is a directory, then output path is PATH/stramon_DATE-TIME.json|});
    ("-f", Set force_output, "Force overwrite of output file");
    ("--no-link", Set no_link, Fmt.str "Disable adding/updating symlink %s" latest_link_name);
    ("--debug", Set debug, "Enable debugging output");
    ("--", Rest add_to_command, "");
  ]

let usage_msg = "stramon [-o JSON_OUTPUT] -- prog arg ..."

let pp_file_date_time =
  Timedesc.pp
    ~format:"{year}-{mon:0X}-{day:0X}-{hour:0X}{min:0X}{sec:0X}" ()

let write_json (oc : out_channel) (json : Yojson.Basic.t) : unit =
  Yojson.Basic.to_channel oc json

let open_handler access (path : Stramon_lib.Abs_path.t) (flags : Stramon_lib.Syscall.literal list) =
  let l = List.filter (fun x ->
      match x with
      | `Const "O_RDONLY"
      | `Const "O_WRONLY"
      | `Const "O_RDWR"
        -> true
      | _ -> false
    ) flags
  in
  match l with
  | `Const "O_RDONLY" :: _ -> (
      Fs_access.add path `R access
    )
  | `Const "O_WRONLY" :: _ -> (
      Fs_access.add path `Rw access
    )
  | `Const "O_RDWR" :: _ -> (
      Fs_access.add path `Rw access
    )
  | _ -> access

let chmod_handler
    ((fs, net) : Fs_access.t * Net_access.t)
    (_pid : int)
    ({ path; _ } : Stramon_lib.Syscall.chmod)
  =
  let open Stramon_lib in
  let path = Abs_path.of_string_exn path in
  let fs = Fs_access.add path `chmod fs in
  (fs, net)

let chown_handler
    ((fs, net) : Fs_access.t * Net_access.t)
    (_pid : int)
    ({ path; _ } : Stramon_lib.Syscall.chown)
  =
  let open Stramon_lib in
  let path = Abs_path.of_string_exn path in
  let fs = Fs_access.add path `chown fs in
  (fs, net)

let stat_handler
    ((fs, net) : Fs_access.t * Net_access.t)
    (_pid : int)
    ({ path; _ } : Stramon_lib.Syscall.stat)
  =
  let open Stramon_lib in
  let path = Abs_path.of_string_exn path in
  let fs = Fs_access.add path `stat fs in
  (fs, net)

let fstatat_handler
    ((fs, net) : Fs_access.t * Net_access.t)
    (_pid : int)
    ({ path; _ } : Stramon_lib.Syscall.fstatat)
  =
  let open Stramon_lib in
  let path = Abs_path.of_string_exn path in
  let fs = Fs_access.add path `stat fs in
  (fs, net)

let handlers : (Fs_access.t * Net_access.t) Stramon_lib.Syscall.handler list =
  let open Stramon_lib in
  [
    `open_ (fun (fs, net) _pid ({ path; flags; mode = _ } : Syscall.open_) ->
        let path = Abs_path.of_string_exn path in
        let fs = open_handler fs path flags in
        (fs, net)
      );
    `openat (fun (fs, net) _pid ({ relative_to; path; flags; mode = _ } : Syscall.openat) ->
        let cwd = Abs_path.of_string_exn relative_to in
        let path = Abs_path.of_string_exn ~cwd path in
        let fs = open_handler fs path flags in
        (fs, net)
      );
    `chmod chmod_handler;
    `fchmod chmod_handler;
    `fchmodat (fun (fs, net) _pid ({ relative_to; path; _ } : Syscall.fchmodat) ->
        let cwd = Abs_path.of_string_exn relative_to in
        let path = Abs_path.of_string_exn ~cwd path in
        let fs = Fs_access.add path `chmod fs in
        (fs, net)
      );
    `chown chown_handler;
    `fchown chown_handler;
    `lchown chown_handler;
    `fchownat (fun (fs, net) _pid ({ relative_to; path; _ } : Syscall.fchownat) ->
        let cwd = Abs_path.of_string_exn relative_to in
        let path = Abs_path.of_string_exn ~cwd path in
        let fs = Fs_access.add path `chown fs in
        (fs, net)
      );
    `stat stat_handler;
    `lstat stat_handler;
    `fstat stat_handler;
    `fstatat64 fstatat_handler;
    `newfstatat fstatat_handler;
    `socket (fun (fs, net) _pid (_ : Syscall.socket) ->
        (fs, net)
      );
    `connect (fun (fs, net) _pid ({ socket = _; addr } : Syscall.connect) ->
        let net = Net_access.add `Connect addr net in
        (fs, net)
      );
    `accept (fun (fs, net) _pid ({ socket = _; addr } : Syscall.accept) ->
        match addr with
        | None -> (fs, net)
        | Some addr ->
          let net = Net_access.add `Accept addr net in
          (fs, net)
      );
    `bind (fun (fs, net) _pid ({ socket = _; addr } : Syscall.bind) ->
        let net = Net_access.add `Bind addr net in
        (fs, net)
      );
    `listen (fun (fs, net) _pid (_ : Syscall.listen) ->
        (fs, net)
      );
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
      if !debug then
        `Registered
      else
        `None
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
      match
        Stramon_lib.monitor
          ~debug_level
          ~handlers
          ~init_ctx:(Fs_access.empty, Net_access.empty)
          command
      with
      | Error msg -> (
          Printf.eprintf "Error: %s\n" msg;
          exit 2
        )
      | Ok res -> (
          (
            try
              CCIO.with_out output_path (fun oc ->
                  let stats = Stramon_lib.Monitor_result.stats res in
                  let (fs, net) = Stramon_lib.Monitor_result.ctx res in
                  let summary = Summary.make stats fs net in
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
