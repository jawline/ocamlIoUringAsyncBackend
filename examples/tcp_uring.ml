open Core
open Async_io_uring

let rec crc accum data idx size =
  if idx = size
  then accum
  else crc (accum + Char.to_int (Bigstring.get data idx)) data (idx + 1) size
;;

let count reader start_time =
  printf "Determined!\n";
  let block_size = 1024 * 128 in
  let count = ref 0 in
  let last_count = ref 0 in
  let crc_accum = ref 0 in
  let rec read_loop () =
    let%bind data = Reader.read reader block_size in
    match data with
    | `Error ->
      printf "Exit with error\n";
      return ()
    | `Eof ->
      let finish_time = Time_ns.now () in
      let crc = !crc_accum in
      let time_elapsed = Time_ns.diff finish_time start_time in
      print_s [%message "finish" (crc : int) (time_elapsed : Time_ns.Span.t)];
      return ()
    | `Ok (bytes_count, buffer) ->
      crc_accum := crc !crc_accum buffer 0 bytes_count;
      count := !count + bytes_count;
      if !count > !last_count + 100000 then last_count := !count;
      read_loop ()
  in
  read_loop ()
;;

let start_server port =
  let server = Tcp.create Unix.Inet_addr.localhost port in
  let rec accept_client () =
    let%bind client = Tcp.accept server in
    match client with
    | `Ok client ->
      let client_reader = Tcp.reader client in
      upon (count client_reader (Time_ns.now ())) (fun () -> ());
      accept_client ()
    | `Error -> raise_s [%message "error connecting clinet"]
  in
  accept_client ()
;;

let () =
  Command.async
    ~summary:""
    Command.Let_syntax.(
      let%map_open port = flag "-port" (required int) ~doc:"port" in
      fun () -> start_server port)
  |> Command.run
;;
