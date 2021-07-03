open Core
open Async_io_uring

let run filepath out_filepath =
  let%bind reader = Reader.open_file filepath in
  let%bind writer = Writer.open_file out_filepath in
  printf "Determined!\n";
  let count = ref 0 in
  let rec read_loop () =
    let%bind data = Reader.read reader (128 * 1024) in
    match data with
    | `Eof ->
      printf "<<< EOF %i\n" !count;
      return ()
    | `Ok (bytes_count, buffer) ->
      count := !count + bytes_count;
      let%bind _written_bytes = Writer.write writer buffer bytes_count in
      read_loop ()
  in
  read_loop ()
;;

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open filepath = flag "-path" (required string) ~doc:"path to load"
      and out_filepath = flag "-outpath" (required string) ~doc:"path to write" in
      fun () -> run filepath out_filepath)
  |> Command.run
;;
