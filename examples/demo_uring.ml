open Core
open Async_io_uring

let run filepath out_filepath =
  let%bind reader = Reader.open_file filepath in
  let%bind writer = Writer.open_file out_filepath in
  printf "Determined!\n";
  let count = ref 0 in
  let rec read_loop () =
    let%bind data = Reader.read reader (1024 * 1024 * 2) in
    match data with
    | `Eof -> printf "Done\n"; return ()
    | `Ok (bytes_count, buffer) ->
      count := !count + bytes_count;
      upon (Writer.safe_write writer buffer bytes_count) (fun written_bytes -> match written_bytes with | `Error -> raise_s [%message "something went wrong"] | `Ok _written -> ());
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
