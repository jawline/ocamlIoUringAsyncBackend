open Core

module Fd_kind = struct
  type t =
    | File
    | Socket
  [@@deriving sexp]
end

type t =
  { kind : Fd_kind.t
  ; fd : Unix.File_descr.t
  }
[@@deriving sexp_of]

let of_file fd = { kind = Fd_kind.File; fd }
let of_socket fd = { kind = Fd_kind.Socket; fd }
