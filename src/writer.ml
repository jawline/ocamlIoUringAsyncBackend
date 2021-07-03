open Core
open Async_kernel

type t = { fd : Fd.t }

let create fd =
  let fd = Fd.of_unix_fd fd in
  { fd }
;;

let open_file filename =
  (* This deferred will fill when the io uring closure executes *)
  let new_ivar = Ivar.create () in
  (* This buffer must not be garbage collected until the open is processed *)
  let open_buffer = Bigstring.create 32 in
  if Io_uring.prepare_open
       Ring.global.ring
       Io_uring.Sqe_flags.none
       ~filepath:filename
       ~flags:1
       ~mode:0
       open_buffer
       (fun result_fd _flags ->
         if result_fd < 0
         then raise_s [%message "file failed to open"]
         else Ivar.fill new_ivar (create result_fd);
         ())
  then raise_s [%message "Could not schedule open"];
  Ivar.read new_ivar
;;

let write t buffer size =
  let new_ivar = Ivar.create () in
  if Io_uring.prepare_read
       Ring.global.ring
       Io_uring.Sqe_flags.none
       t.fd.unix_fd
       buffer
       ~len:size
       ~offset:(-1)
       (fun result _flags ->
         if result = -1
         then Ivar.fill new_ivar `Error
         else Ivar.fill new_ivar (`Ok result);
         ())
  then raise_s [%message "could not schedule write"];
  Ivar.read new_ivar
;;
