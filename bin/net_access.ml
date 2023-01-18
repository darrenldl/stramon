type net = {
  connect : int String_map.t;
  bind : int String_map.t;
}

type t = {
  ipv4 : net;
  ipv6 : net;
}

let empty_net = {
  connect = String_map.empty;
  bind = String_map.empty;
}

let empty = {
  ipv4 = empty_net;
  ipv6 = empty_net;
}

let add (addr : string) (port : int) (t : t) :
