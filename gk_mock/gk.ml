open Lwt
open Ezjsonm

module Server = Cohttp_lwt_unix.Server
module Client = Cohttp_lwt_unix.Client


let headers = Cohttp.Header.init_with "Access-Control-Allow-Origin" "*"
let empty = Cohttp_lwt_body.empty
let cwd = Sys.getcwd ()


let make_server cert priv ip port  =
  let callback conn req body =
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let steps = Astring.String.cuts ~empty:false ~sep:"/" path in
    let ending = try List.rev steps |> List.hd
                 with _ -> "" in
    if ending = "all" then
      Server.respond_not_found ()
    else if ending = "list" then
      let dir = List.rev steps |> List.tl |> List.rev
                |> String.concat "/" in
      let path = Filename.concat cwd dir in
      let files = Sys.readdir path
                  |> Array.to_list in
      let body =
        strings files
        |> to_string
        |> Cohttp_lwt_body.of_string in
      Server.respond ~status:`OK ~headers ~body ()
    else
      let path = Filename.concat cwd (String.concat "/" steps) in
      let meth = Cohttp.Request.meth req in
      if meth = `GET then
        let fname = Server.resolve_file ~docroot:"." ~uri in
        Server.respond_file ~headers ~fname ()
      else if meth = `POST then
        (*let () = Printf.printf "open %s\n%!" path in*)
        let oc = open_out path in
        Cohttp_lwt_body.to_string body >>= fun body_str ->
        (*let () = Printf.printf "get string %s\n%!" body_str in*)
        output_string oc body_str;
        flush oc;
        close_out oc;
        Server.respond ~headers ~status:`OK ~body:empty ()
      else
        Server.respond_not_found ()
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


let () =
  let cert = Filename.concat cwd Sys.argv.(1)
  and priv = Filename.concat cwd Sys.argv.(2) in
  Printf.printf "[gk] cert file %s, private key %s\n%!" cert priv;
  let ip = Sys.argv.(3)
  and port = Sys.argv.(4) |> int_of_string in
  Lwt_main.run (make_server cert priv ip port)
