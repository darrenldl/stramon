let command = ref []

let add_to_command x =
  command := x :: !command

let output_path = ref ""

let force_output = ref false

let no_link = ref false

let print_version = ref false

let debug = ref false

let latest_link_name = "stramon-latest.json"

let speclist = Arg.[
    ("-o", Set_string output_path,
     {|JSON file output path, defaults to: stramon_DATE-TIME.json.
If provided path PATH is a directory, then output path is PATH/stramon_DATE-TIME.json|});
    ("-f", Set force_output, "Force overwrite of output file");
    ("--no-link", Set no_link, Fmt.str "Disable adding/updating symlink %s" latest_link_name);
    ("--version", Set print_version, "Print version and exit");
    ("--debug", Set debug, "Enable debugging output");
    ("--", Rest add_to_command, "");
  ]

let usage_msg = "stramon [-o JSON_OUTPUT] -- prog arg ..."

let pp_file_date_time =
  Timedesc.pp
    ~format:"{year}-{mon:0X}-{day:0X}-{hour:0X}{min:0X}{sec:0X}" ()

let write_json (oc : out_channel) (json : Yojson.Basic.t) : unit =
  Yojson.Basic.to_channel oc json

let make_path ~relative_to path =
  let open Stramon_lib in
  let cwd = Abs_path.of_string_exn relative_to in
  if path = "" then
    cwd
  else
    Abs_path.of_string_exn ~cwd path

type ctx = {
  pid_tree : Pid_tree.t;
  fs : Fs_access.t;
  net : Net_access.t;
}

let empty_ctx = {
  pid_tree = Pid_tree.empty;
  fs = Fs_access.empty;
  net = Net_access.empty;
}

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
    ({ pid_tree; fs; net } : ctx)
    (pid : int)
    ({ path; _ } : Stramon_lib.Syscall.chmod)
  =
  let open Stramon_lib in
  let pid_tree = Pid_tree.add pid pid_tree in
  let path = Abs_path.of_string_exn path in
  let fs = Fs_access.add path `chmod fs in
  { pid_tree; fs; net }

let chown_handler
    ({ pid_tree; fs; net } : ctx)
    (pid : int)
    ({ path; _ } : Stramon_lib.Syscall.chown)
  =
  let open Stramon_lib in
  let pid_tree = Pid_tree.add pid pid_tree in
  let path = Abs_path.of_string_exn path in
  let fs = Fs_access.add path `chown fs in
  { pid_tree; fs; net }

let stat_handler
    ({ pid_tree; fs; net } : ctx)
    (pid : int)
    ({ path; _ } : Stramon_lib.Syscall.stat)
  =
  let open Stramon_lib in
  let pid_tree = Pid_tree.add pid pid_tree in
  let path = Abs_path.of_string_exn path in
  let fs = Fs_access.add path `stat fs in
  { pid_tree; fs; net }

let fstatat_handler
    ({ pid_tree; fs; net } : ctx)
    (pid : int)
    ({ relative_to; path; _ } : Stramon_lib.Syscall.fstatat)
  =
  let pid_tree = Pid_tree.add pid pid_tree in
  let path = make_path ~relative_to path in
  let fs = Fs_access.add path `stat fs in
  { pid_tree; fs; net }

let handlers : ctx Stramon_lib.Syscall.handler list =
  let open Stramon_lib in
  [
    `open_ (fun { pid_tree; fs; net } pid ({ path; flags; mode = _ } : Syscall.open_) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        let path = Abs_path.of_string_exn path in
        let fs = open_handler fs path flags in
        { pid_tree; fs; net }
      );
    `openat (fun { pid_tree; fs; net } pid ({ relative_to; path; flags; mode = _ } : Syscall.openat) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        let path = make_path ~relative_to path in
        let fs = open_handler fs path flags in
        { pid_tree; fs; net }
      );
    `chmod chmod_handler;
    `fchmod chmod_handler;
    `fchmodat (fun { pid_tree; fs; net } pid ({ relative_to; path; _ } : Syscall.fchmodat) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        let path = make_path ~relative_to path in
        let fs = Fs_access.add path `chmod fs in
        { pid_tree; fs; net }
      );
    `chown chown_handler;
    `fchown chown_handler;
    `lchown chown_handler;
    `fchownat (fun { pid_tree; fs; net } pid ({ relative_to; path; _ } : Syscall.fchownat) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        let path = make_path ~relative_to path in
        let fs = Fs_access.add path `chown fs in
        { pid_tree; fs; net }
      );
    `stat stat_handler;
    `lstat stat_handler;
    `fstat stat_handler;
    `fstatat64 fstatat_handler;
    `statx (fun { pid_tree; fs; net } pid ({ relative_to; path; _ } : Syscall.statx) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        let path = make_path ~relative_to path in
        let fs = Fs_access.add path `stat fs in
        { pid_tree; fs; net }
      );
    `newfstatat fstatat_handler;
    `socket (fun { pid_tree; fs; net } pid (_ : Syscall.socket) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        { pid_tree; fs; net }
      );
    `connect (fun { pid_tree; fs; net } pid ({ socket = _; addr } : Syscall.connect) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        let net = Net_access.add `Connect addr net in
        { pid_tree; fs; net }
      );
    `accept (fun { pid_tree; fs; net } pid ({ socket = _; addr } : Syscall.accept) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        match addr with
        | None -> { pid_tree; fs; net }
        | Some addr ->
          let net = Net_access.add `Accept addr net in
          { pid_tree; fs; net }
      );
    `bind (fun { pid_tree; fs; net } pid ({ socket = _; addr } : Syscall.bind) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        let net = Net_access.add `Bind addr net in
        { pid_tree; fs; net }
      );
    `listen (fun { pid_tree; fs; net } pid (_ : Syscall.listen) ->
        let pid_tree = Pid_tree.add pid pid_tree in
        { pid_tree; fs; net }
      );
    `fork (fun { pid_tree; fs; net } pid ({ pid = child_pid; _ } : Syscall.fork) ->
        match child_pid with
        | None -> { pid_tree; fs; net }
        | Some child_pid ->
          let pid_tree = Pid_tree.add ~parent:pid child_pid pid_tree in
          { pid_tree; fs; net }
      );
    `clone (fun { pid_tree; fs; net } pid ({ child_tid; _ } : Syscall.clone) ->
        match child_tid with
        | None -> { pid_tree; fs; net }
        | Some child_tid ->
          let pid_tree = Pid_tree.add ~parent:pid child_tid pid_tree in
          { pid_tree; fs; net }
      );
    `clone3 (fun { pid_tree; fs; net } pid ({ child_tid; _ } : Syscall.clone3) ->
        match child_tid with
        | None -> { pid_tree; fs; net }
        | Some child_tid ->
          let pid_tree = Pid_tree.add ~parent:pid child_tid pid_tree in
          { pid_tree; fs; net }
      );
  ]

let () =
  Sys.catch_break true;
  try
    Arg.parse speclist add_to_command usage_msg;
    if !print_version then (
      Printf.printf "%s\n" Version_string.s;
      exit 0
    ) else (
      let command = List.rev !command in
      let output_path, latest_link_target, latest_link_path =
        let default_name =
          Fmt.str "stramon_%a.json" pp_file_date_time (Timedesc.now ())
        in
        if !output_path = "" then (
          (default_name,
           default_name,
           latest_link_name
          )
        ) else (
          if Sys.file_exists !output_path
          && Sys.is_directory !output_path
          then (
            (Fmt.str "%s/%s" !output_path default_name,
             default_name,
             Fmt.str "%s/%s" !output_path latest_link_name
            )
          ) else (
            (!output_path,
             Filename.basename !output_path,
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
            ~init_ctx:empty_ctx
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
                    let {pid_tree; fs; net} =
                      Stramon_lib.Monitor_result.ctx res
                    in
                    let summary = Summary.make stats pid_tree fs net in
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
                   Unix.symlink latest_link_target latest_link_path
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
    )
  with
  | Sys.Break -> (
      Printf.eprintf "Ctrl-C received\n";
      exit 1
    )
