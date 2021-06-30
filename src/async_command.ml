open Core
include Core.Command

type 'a with_options = ?extract_exn:bool -> 'a

let shutdown_with_error e =
  Caml.at_exit (fun () ->
    Core.prerr_endline (Error.to_string_hum e));
  Async.shutdown 1
;;

let maybe_print_error_and_shutdown = function
  | Ok () -> Async.shutdown 0
  | Error e -> shutdown_with_error e
;;

let in_async ?extract_exn:_ param _on_result =
  Param.map param ~f:(fun staged_main () ->
    let main = Or_error.try_with (fun () -> unstage (staged_main ())) in
    match main with
    | Error e ->
      shutdown_with_error e;
      Async_io0.run ()
    | Ok _main ->
      (Async_io0.run ())
  )
;;

type 'r staged = ([ `Scheduler_started ] -> 'r) Staged.t

module Staged = struct
  let async ?extract_exn ~summary ?readme param =
    let on_result = maybe_print_error_and_shutdown in
    basic ~summary ?readme (in_async ?extract_exn param on_result)
  ;;

  let async_spec ?extract_exn ~summary ?readme spec main =
    async ?extract_exn ~summary ?readme (Spec.to_param spec main)
  ;;

  let async_or_error ?extract_exn ~summary ?readme param =
    let on_result res = maybe_print_error_and_shutdown (Or_error.join res) in
    basic ~summary ?readme (in_async ?extract_exn param on_result)
  ;;

  let async_spec_or_error ?extract_exn ~summary ?readme spec main =
    async_or_error ?extract_exn ~summary ?readme (Spec.to_param spec main)
  ;;
end

let stage_param = Param.map ~f:(fun main () -> stage (fun `Scheduler_started -> main ()))

let async ?extract_exn ~summary ?readme param =
  Staged.async ?extract_exn ~summary ?readme (stage_param param)
;;

let async_or_error ?extract_exn ~summary ?readme param =
  Staged.async_or_error ?extract_exn ~summary ?readme (stage_param param)
;;

let async_spec ?extract_exn ~summary ?readme spec main =
  async ?extract_exn ~summary ?readme (Spec.to_param spec main)
;;

let async_spec_or_error ?extract_exn ~summary ?readme spec main =
  async_or_error ?extract_exn ~summary ?readme (Spec.to_param spec main)
;;
