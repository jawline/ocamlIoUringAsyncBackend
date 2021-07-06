open Core
open Async_io_uring

let cp filepath out_filepath =
  let block_size = 1024 * 32 in
  let%bind reader = Reader.open_file ~buf_len:block_size filepath in
  let%bind writer = Writer.open_file out_filepath in
  printf "Determined!\n";
  let count = ref 0 in
  let rec read_loop () =
    let%bind data = Reader.read reader block_size in
    match data with
    | `Error | `Eof ->
      printf "Done\n";
      return ()
    | `Ok (bytes_count, buffer) ->
      count := !count + bytes_count;
      upon (Writer.safe_write writer buffer bytes_count) (fun written_bytes ->
          match written_bytes with
          | `Error -> raise_s [%message "something went wrong"]
          | `Ok _written -> ());
      read_loop ()
  in
  read_loop ()
;;

let rec crc accum data idx size =
  if idx = size
  then accum
  else crc (accum + Char.to_int (Bigstring.get data idx)) data (idx + 1) size
;;

let count filepath =
  let block_size = 1024 * 32 in
  let%bind reader = Reader.open_file ~buf_len:block_size filepath in
  printf "Determined!\n";
  let count = ref 0 in
  let crc_accum = ref 0 in
  let rec read_loop () =
    let%bind data = Reader.read reader block_size in
    match data with
    | `Eof ->
      let crc = !crc_accum in
      print_s [%message (crc : int)];
      return ()
    | `Error ->
      printf "Exit with error\n";
      return ()
    | `Ok (bytes_count, buffer) ->
      crc_accum := crc !crc_accum buffer 0 bytes_count;
      count := !count + bytes_count;
      read_loop ()
  in
  read_loop ()
;;

let () =
  Command.async
    ~summary:""
    Command.Let_syntax.(
      let%map_open filepath = flag "-path" (required string) ~doc:"path to load"
      and out_filepath = flag "-outpath" (required string) ~doc:"path to write"
      and no_write =
        flag "-nowrite" no_arg ~doc:"Don't copy the file, instead just count size"
      in
      fun () -> if no_write then count filepath else cp filepath out_filepath)
  |> Command.run
;;
