open Core
open Async_io_uring

let handle_connection inet_addr reader writer =
  print_s [%message "client connected" (inet_addr : Socket.Address.Inet.t)];
  Reader.pipe reader
  |> Pipe.iter ~f:(fun str ->
         Writer.write writer str;
         let%bind () = Writer.flushed writer in
         if String.is_substring str ~substring:"bye"
         then Reader.close reader
         else Deferred.unit)
;;

let () =
  Command.run
  @@ Command.async ~summary:"tcp echo server"
  @@ Command.Param.return
  @@ fun () ->
  let port = 8000 in
  let server =
    Tcp.Server.create_inet
      ~on_handler_error:
        (`Call
          (fun inet_addr exn ->
            raise_s
              [%message
                "error handling connection"
                  (inet_addr : Socket.Address.Inet.t)
                  (exn : exn)]))
      (Tcp.Where_to_listen.of_port port)
      handle_connection
  in
  Tcp.Server.close_finished_and_handlers_determined server
;;
