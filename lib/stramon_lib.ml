let init () =
  Random.self_init ()

let profile (cmd : string list) =
  Proc_utils.exec
