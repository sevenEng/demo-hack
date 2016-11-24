open Lwt
open Ezjsonm

module Server = Cohttp_lwt_unix.Server
module Client = Cohttp_lwt_unix.Client


let headers = Cohttp.Header.init_with "Access-Control-Allow-Origin" "*"
let cwd = Sys.getcwd ()


let make_server ip port  =
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
      let fname = Server.resolve_file ~docroot:"." ~uri in
      Server.respond_file ~headers ~fname ()
  in

  Conduit_lwt_unix.init ~src:ip () >>= fun conduit_ctx ->
  let ctx = Cohttp_lwt_unix_net.init ~ctx:conduit_ctx () in
  let mode = `TCP (`Port port) in
  let t = Server.make ~callback () in
  Printf.printf "listening on %s:%d\n%!" ip port;
  Server.create ~ctx ~mode t


let () =
  let ip = Sys.argv.(1)
  and port = Sys.argv.(2) |> int_of_string in
  Lwt_main.run (make_server ip port)
