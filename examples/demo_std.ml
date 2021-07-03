open Core
open Async

let run filepath outpath =
  let%bind reader = Reader.open_file filepath in
  let%bind writer = Writer.open_file outpath in
  printf "Determined!\n";
  let count = ref 0 in
  let byte_data = Bytes.create (128 * 1024) in
  let rec read_loop () =
    let%bind data = Reader.read reader byte_data in
    match data with
    | `Eof ->
      printf "<<< EOF %i\n" !count;
      return ()
    | `Ok bytes_count ->
      count := !count + bytes_count;
      Writer.write_bytes writer byte_data;
      read_loop ()
  in
  read_loop ()
;;

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open filepath = flag "-path" (required string) ~doc:"path to load"
      and outpath = flag "-outpath" (required string) ~doc:"path to write to" in
      fun () -> run filepath outpath)
  |> Command.run
;;
