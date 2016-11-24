let get_addr domain =
  [
    "review", ("192.168.1.136", 10000);
    "wifi", ("192.168.1.136", 10001);
    "stb.mobile", ("192.168.1.136", 10002)]
  |> List.assoc domain


let set_rule domain =
  get_addr domain
  |> fun (ip, port) ->
     Printf.printf "setting rules for %s:%d...\n%!" ip port;
     ip, port



let remove_rule domain =
  get_addr domain
  |> fun (ip, port) ->
     Printf.printf "tearing up rules for %s:%d...\n%!" ip port




