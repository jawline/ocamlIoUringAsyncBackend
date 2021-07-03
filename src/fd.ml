open Core

type t = { unix_fd : Unix.File_descr.t }

let of_unix_fd unix_fd =
  let unix_fd = Unix.File_descr.of_int unix_fd in
  { unix_fd }
;;
