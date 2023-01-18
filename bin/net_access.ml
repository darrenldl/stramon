type mode = [
  | `Connect
  | `Accept
  | `Bind
]

type net = {
  connect : int list String_map.t;
  accept : int list String_map.t;
  bind : int list String_map.t;
}

type t = {
  ipv4 : net;
  ipv6 : net;
}

let empty_net = {
  connect = String_map.empty;
  accept = String_map.empty;
  bind = String_map.empty;
}

let empty = {
  ipv4 = empty_net;
  ipv6 = empty_net;
}

let net_add (mode : mode) (addr : string) (port : int) (net : net) : net =
  let aux (addr : string) (port : int) (m : int list String_map.t) =
    let l =
      String_map.find_opt addr m
      |> Option.value ~default:[]
      |> (fun l -> port :: l)
      |> List.sort_uniq Int.compare
    in
    String_map.add addr l m
  in
  match mode with
  | `Connect ->
    { net with connect = aux addr port net.connect }
  | `Accept ->
    { net with accept = aux addr port net.accept }
  | `Bind ->
    { net with bind = aux addr port net.bind }

let add (mode : mode) (addr : Stramon_lib.Syscall.sockaddr) (t : t) : t =
  let open Stramon_lib.Syscall in
  match addr with
  | `AF_INET { port; addr } ->
    { t with ipv4 = net_add mode addr port t.ipv4 }
  | `AF_INET6 { port; addr; _ } ->
    { t with ipv6 = net_add mode addr port t.ipv6 }

let json_of_net (net : net) : Yojson.Basic.t =
  let aux (m : int list String_map.t) =
    let l : (string * Yojson.Basic.t) list =
      String_map.to_seq m
      |> Seq.map (fun (addr, ports) ->
          (addr, `List (List.map (fun port -> `Int port) ports)))
      |> List.of_seq
    in
    `Assoc l
  in
  `Assoc
    [
      ("connect", aux net.connect);
      ("accept", aux net.accept);
      ("bind", aux net.bind);
    ]

let to_json (t : t) : Yojson.Basic.t =
  `Assoc
    [
      ("ipv4", json_of_net t.ipv4);
      ("ipv6", json_of_net t.ipv6);
    ]
