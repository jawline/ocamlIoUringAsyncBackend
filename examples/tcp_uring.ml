open Core
open Async_io_uring

let rec crc accum data idx size =
  if idx = size
  then accum
  else crc (accum + Char.to_int (Bytes.get data idx)) data (idx + 1) size
;;

let count reader start_time =
  printf "Determined!\n";
  let block_size = 1024 * 128 in
  let count = ref 0 in
  let last_count = ref 0 in
  let crc_accum = ref 0 in
  let read_buffer = Bytes.create block_size in
  let rec read_loop () =
    let%bind data = Reader.read reader read_buffer in
    match data with
    | `Error ->
      printf "Exit with error\n";
      return ()
    | `Eof ->
      let finish_time = Time_ns.now () in
      let crc = !crc_accum in
      let time_elapsed = Time_ns.diff finish_time start_time in
      print_s [%message "finish" (crc : int) (time_elapsed : Time_ns.Span.t)];
      let%bind `Ok = Reader.close reader in
      return ()
    | `Ok bytes_count ->
      crc_accum := crc !crc_accum read_buffer 0 bytes_count;
      count := !count + bytes_count;
      if !count > !last_count + 100000 then last_count := !count;
      read_loop ()
  in
  read_loop ()
;;

let start_server port =
  let%bind _server =
    Tcp.Server.create
      ~on_handler_error:(fun () -> raise_s [%message "error"])
      (Unix.Inet_addr.localhost, port)
      (fun reader _writer -> upon (count reader (Time_ns.now ())) (fun () -> ()))
  in
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:""
    Command.Let_syntax.(
      let%map_open port = flag "-port" (required int) ~doc:"port" in
      fun () -> start_server port)
  |> Command.run
;;
