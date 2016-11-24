open Lwt
open Ezjsonm

module Server = Cohttp_lwt_unix.Server
module Client = Cohttp_lwt_unix.Client


let headers = Cohttp.Header.init_with "Access-Control-Allow-Origin" "*"
let empty = Cohttp_lwt_body.empty
let cwd = Sys.getcwd ()

module AcSet = Set.Make(String)

let approved = ref AcSet.empty
let pending = ref AcSet.empty
let rejected = ref AcSet.empty

let elm_of_queries ip domain =
  let ip = Ipaddr.to_string ip in
  ip ^ " " ^ domain


let make_https_server cert priv ip port  =
  let callback conn req body =
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
  if path <> "/domain" then
    Server.respond_not_found ()
  else
    let queries = Uri.query uri in
    try
      let ip = List.assoc "ip" queries |> List.hd |> Ipaddr.of_string_exn in
      let domain = List.assoc "domain" queries |> List.hd in
      let elm = elm_of_queries ip domain in
      if AcSet.mem elm !approved then
        let ip, port = Domain.set_rule domain in
        let body =
          ["ip", string ip; "port", int port]
          |> dict
          |> to_string
          |> Cohttp_lwt_body.of_string in
        Server.respond ~headers ~status:`OK ~body ()
      else if AcSet.mem elm !rejected then
        let body = "Rject access by data owner\r\n" in
        Server.respond_error ~headers ~status:`Unauthorized ~body ()
      else begin
          if not (AcSet.mem elm !pending) then pending := AcSet.add elm !pending;
          let body = "Wait for data owner's decision\r\n" in
          Server.respond_error ~headers ~status:`Unauthorized ~body ()
        end
    with exn ->
      let body = Printexc.to_string exn in
      Server.respond_error ~headers ~status:`Bad_request ~body ()
  in

  let server_config =
    `Crt_file_path cert, `Key_file_path priv, `No_password, `Port port in
  let server = `TLS_native server_config in

  let tls_server_key =
    `TLS (`Crt_file_path cert, `Key_file_path priv, `No_password) in
  Conduit_lwt_unix.init ~src:ip ~tls_server_key () >>= fun conduit_ctx ->
  let ctx = Cohttp_lwt_unix_net.init ~ctx:conduit_ctx () in
  let t = Server.make ~callback () in
  Printf.printf "[gk] listening on %s:%d\n%!" ip port;
  Server.create ~ctx ~mode:server t


let make_http_server ip port  =
  let callback conn req body =
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" path in
    if path <> "/op" then
      Server.respond_not_found ()
    else
      match List.tl steps with
      | "list" :: [c] ->
         let l =
           (if c = "approved" then !approved
            else if c = "pending" then !pending
            else !rejected)
           |> AcSet.elements in
         let body =
           strings l
           |> to_string
           |> Cohttp_lwt_body.of_string in
         Server.respond ~headers ~status:`OK ~body ()
      | "remove" :: c :: ip :: [domain] ->
         let elm = elm_of_queries (Ipaddr.of_string_exn ip) domain in
         let () =
           if c = "approved" then approved := AcSet.remove elm !approved
           else if c = "pending" then pending := AcSet.remove elm !pending
           else rejected := AcSet.remove elm !rejected in
         if c = "approved" then Domain.remove_rule domain;
         Server.respond ~headers ~status:`OK ~body:empty ()
      | op :: ip :: [domain] ->
         let elm = elm_of_queries (Ipaddr.of_string_exn ip) domain in
         pending := AcSet.remove elm !pending;
         (if op = "approve" then
           approved := AcSet.add elm !approved
         else if op = "reject" then
           rejected := AcSet.add elm !rejected);
         Server.respond ~headers ~status:`OK ~body:empty ()
      | _ ->
         Server.respond_not_found ()
  in

  Conduit_lwt_unix.init ~src:ip () >>= fun conduit_ctx ->
  let ctx = Cohttp_lwt_unix_net.init ~ctx:conduit_ctx () in
  let mode = `TCP (`Port port) in
  let t = Server.make ~callback () in
  Printf.printf "[gk] listening on %s:%d\n%!" ip port;
  Server.create ~ctx ~mode t


let () =
  let cert = Filename.concat cwd Sys.argv.(1)
  and priv = Filename.concat cwd Sys.argv.(2) in
  Printf.printf "[gk] cert file %s, private key %s\n%!" cert priv;

  let ip = Sys.argv.(3)
  and port = Sys.argv.(4) |> int_of_string in

  join [make_https_server cert priv ip port;
        make_http_server ip 8080]
  |> Lwt_main.run
