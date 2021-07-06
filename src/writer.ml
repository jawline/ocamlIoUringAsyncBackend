open Core
open Async_kernel

type t =
  { fd : Unix.File_descr.t
  ; mutable pos : int
  }

let create fd = { fd; pos = 0 }

let open_file filename =
  (* This deferred will fill when the io uring closure executes *)
  let new_ivar = Ivar.create () in
  (* This buffer must not be garbage collected until the open is processed *)
  let open_buffer = Bigstring.create 32 in
  if Io_uring.prepare_open
       Ring.global.ring
       Io_uring.Sqe_flags.none
       ~filepath:filename
       ~flags:(1 lor 64 lor 512)
       ~mode:777
       open_buffer
       ( open_buffer
       , fun result_fd _flags ->
           if result_fd < 0
           then raise_s [%message (filename : string) "failed to open" (result_fd : int)]
           else Ivar.fill new_ivar (create (Unix.File_descr.of_int result_fd));
           () )
  then raise_s [%message "Could not schedule open"];
  Ivar.read new_ivar
;;

let write t buffer size =
  let new_ivar = Ivar.create () in
  if Io_uring.prepare_write
       Ring.global.ring
       Io_uring.Sqe_flags.none
       t.fd
       buffer
       ~len:size
       ~offset:t.pos
       ( buffer
       , fun result _flags ->
           if result <= 0
           then Ivar.fill new_ivar `Error
           else Ivar.fill new_ivar (`Ok result);
           () )
  then raise_s [%message "could not schedule write"];
  t.pos <- t.pos + size;
  Ivar.read new_ivar
;;

let safe_write t buffer size =
  let tmp_buffer = Bigstring.create size in
  Bigstring.blit ~src:buffer ~src_pos:0 ~dst_pos:0 ~len:size ~dst:tmp_buffer;
  write t tmp_buffer size
;;
