open Core
open Async_kernel

module Server = struct
  type t = { sockfd : Fd.t }

  let accept t =
    Deferred.create (fun ivar ->
        let _accepted_sockaddr =
          Io_uring.prepare_accept
            Ring.global.ring
            Io_uring.Sqe_flags.none
            t.sockfd.fd
            ( Bigstring.create 0 (* We need to supply some data *)
            , fun res _ring_res ->
                Ivar.fill
                  ivar
                  (if res < 0
                  then `Error
                  else `Ok { sockfd = Unix.File_descr.of_int res |> Fd.of_socket }) )
        in
        ())
  ;;

  let create_listen_socket ?(backlog = 100) (address, port) =
    let sockfd = Unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
    Unix.setsockopt sockfd SO_REUSEADDR true;
    let addr = Unix.ADDR_INET (address, port) in
    Unix.bind sockfd ~addr;
    Unix.listen sockfd ~backlog;
    { sockfd = sockfd |> Fd.of_socket }
  ;;

  let create ~on_handler_error where_and_port new_client_callback =
    let listen_socket = create_listen_socket where_and_port in
    let rec accept_loop () =
      let%bind new_client = accept listen_socket in
      (match new_client with
      | `Ok new_socket -> new_client_callback (Reader.create new_socket.sockfd) ()
      | `Error -> on_handler_error ());
      accept_loop ()
    in
    upon (accept_loop ()) (fun () -> ());
    return listen_socket
  ;;
end
