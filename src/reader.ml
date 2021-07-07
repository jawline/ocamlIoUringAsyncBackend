open Core
open Async_kernel

type t =
  { fd : Fd.t
  ; mutable buf : Bigstring.t
  }

let create ?buf_len (fd : Fd.t) =
  let buf_len =
    match buf_len with
    | None -> 1024 * 128
    | Some buf_len ->
      if buf_len > 0
      then buf_len
      else
        raise_s
          [%message "Reader.create got non positive buf_len" (buf_len : int) (fd : Fd.t)]
  in
  { fd; buf = Bigstring.create buf_len }
;;

let open_file ?buf_len filename =
  (* This deferred will fill when the io uring closure executes *)
  let new_ivar = Ivar.create () in
  (* This buffer must not be garbage collected until the open is processed *)
  let open_buffer = Bigstring.create 32 in
  if Io_uring.prepare_open
       Ring.global.ring
       Io_uring.Sqe_flags.none
       ~filepath:filename
       ~flags:0
       ~mode:0
       open_buffer
       ( open_buffer
       , fun result_fd _flags ->
           if result_fd < 0
           then raise_s [%message "file failed to open"]
           else
             Ivar.fill
               new_ivar
               (create ?buf_len (Fd.of_file (Unix.File_descr.of_int result_fd)));
           () )
  then raise_s [%message "Could not schedule open"];
  Ivar.read new_ivar
;;

let read t buffer =
  let len = Bytes.length buffer in
  let buffer = Substring.create buffer in
  let new_ivar = Ivar.create () in
  let user_data =
    ( t.buf
    , fun result _flags ->
        if result = 0
        then Ivar.fill new_ivar `Eof
        else if result < 0
        then Ivar.fill new_ivar `Error
        else (
          Substring.blit_from_bigstring buffer ~src:t.buf ~src_pos:0 ~len:result;
          Ivar.fill new_ivar (`Ok result)) )
  in
  let schedule_result =
    match t.fd.kind with
    | File ->
      Io_uring.prepare_read
        Ring.global.ring
        Io_uring.Sqe_flags.none
        t.fd.fd
        t.buf
        ~len
        ~offset:(-1)
        user_data
    | Socket ->
      Io_uring.prepare_recv
        Ring.global.ring
        Io_uring.Sqe_flags.none
        t.fd.fd
        t.buf
        ~len
        user_data
  in
  if schedule_result then raise_s [%message "could not schedule read"];
  Ivar.read new_ivar
;;
