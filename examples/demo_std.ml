open Core
open Async

let cp filepath out_filepath =
  let block_size = 1024 * 32 in
  let%bind reader = Reader.open_file ~buf_len:block_size filepath in
  let%bind writer = Writer.open_file out_filepath in
  printf "Determined!\n";
  let bytes_buffer = Bytes.create block_size in
  let count = ref 0 in
  let rec read_loop () =
    let%bind data = Reader.read reader bytes_buffer in
    match data with
    | `Eof ->
      printf "Done\n";
      return ()
    | `Ok bytes_count ->
      count := !count + bytes_count;
      Writer.write_bytes writer bytes_buffer ~len:bytes_count;
      read_loop ()
  in
  read_loop ()
;;

let rec crc accum data idx size =
  if idx = size
  then accum
  else crc (accum + Char.to_int (Bytes.unsafe_get data idx)) data (idx + 1) size
;;

let count filepath =
  let block_size = 1024 * 32 in
  let%bind reader = Reader.open_file ~buf_len:block_size filepath in
  printf "Determined!\n";
  let count = ref 0 in
  let crc_accum = ref 0 in
  let buffer_bytes = Bytes.create block_size in
  let rec read_loop () =
    let%bind data = Reader.read reader buffer_bytes in
    match data with
    | `Eof ->
      print_s [%message "" ~crc:(!crc_accum : int)];
      return ()
    | `Ok bytes_count ->
      crc_accum := crc !crc_accum buffer_bytes 0 bytes_count;
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
