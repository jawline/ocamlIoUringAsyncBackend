open Core
open Async_io_uring

let () =
  Command.run
  @@ Command.async ~summary:"tcp echo server"
  @@ Command.Param.return
  @@ fun () ->
  let%bind reader =
    Reader.open_ "/home/blake/async_replacemenet/async_io_uring/src/async_io0.ml" 0 0
  in
  printf "Determined!\n";
  let rec read_loop () =
    let%bind data = Reader.read reader 4096 in
    match data with
    | `Eof ->
      printf "<<< EOF\n";
      return ()
    | `Continue _ -> read_loop ()
  in
  read_loop ()
;;
