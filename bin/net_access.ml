type mode = [
  | `Connect
  | `Accept
  | `Bind
]

type net = {
  connect : int String_map.t;
  accept : int String_map.t;
  bind : int String_map.t;
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
  match mode with
  | `Connect ->
    { net with connect = String_map.add addr port net.connect }
  | `Accept ->
    { net with accept = String_map.add addr port net.accept }
  | `Bind ->
    { net with bind = String_map.add addr port net.bind }

let add (mode : mode) (addr : Stramon_lib.Syscall.sockaddr) (t : t) : t =
  let open Stramon_lib.Syscall in
  match addr with
  | `AF_INET { port; addr } ->
    { t with ipv4 = net_add mode addr port t.ipv4 }
  | `AF_INET6 { port; addr; _ } ->
    { t with ipv6 = net_add mode addr port t.ipv6 }
