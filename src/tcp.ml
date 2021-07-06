open Core
open Async_kernel

type t = { sockfd : Fd.t }

let create ?(backlog = 100) address port =
  let sockfd = Unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Unix.setsockopt sockfd SO_REUSEADDR true;
  let addr = Unix.ADDR_INET (address, port) in
  Unix.bind sockfd ~addr;
  Unix.listen sockfd ~backlog;
  { sockfd = sockfd |> Fd.of_socket }
;;

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

let reader ?buf_len t = Reader.create ?buf_len t.sockfd
