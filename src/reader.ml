open Core
open Async_kernel

type t =
  { fd : Fd.t
  ; mutable buf : Bigstring.t
  }

let create ?buf_len fd =
  let buf_len =
    match buf_len with
    | None -> 32 * 1024
    | Some buf_len ->
      if buf_len > 0
      then buf_len
      else
        raise_s
          [%message "Reader.create got non positive buf_len" (buf_len : int) (fd : int)]
  and fd = Fd.of_unix_fd fd in
  let buf = Bigstring.create buf_len in
  { fd; buf }
;;

let open_ filename flags mode =
  (* This deferred will fill when the io uring closure executes *)
  let new_ivar = Ivar.create () in
  (* This buffer must not be garbage collected until the open is processed *)
  let open_buffer = Bigstring.create 32 in
  printf "Opening a file\n";
  if Io_uring.prepare_open
       Ring.global.ring
       Io_uring.Sqe_flags.none
       ~filepath:filename
       ~flags
       ~mode
       open_buffer
       (fun result_fd _flags ->
         printf "Opened %i" result_fd;
         if result_fd < 0
         then raise_s [%message "file failed to open"]
         else Ivar.fill new_ivar (create result_fd);
         ())
  then raise_s [%message "Could not schedule open"];
  Ivar.read new_ivar
;;

let read t size =
  let new_ivar = Ivar.create () in
  if Io_uring.prepare_read
       Ring.global.ring
       Io_uring.Sqe_flags.none
       t.fd.unix_fd
       t.buf
       ~len:size
       ~offset:(-1)
       (fun result _flags ->
         printf "Read result %i\n" result;
         printf "%s\n" (Bigstring.to_string t.buf ~pos:0 ~len:result);
         if result = 0
         then Ivar.fill new_ivar `Eof
         else Ivar.fill new_ivar (`Continue (result, t.buf));
         ())
  then raise_s [%message "could not schedule read"];
  Ivar.read new_ivar
;;
