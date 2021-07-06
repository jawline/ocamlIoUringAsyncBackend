open Core

type t = { ring : (Bigstring.t * (int -> int -> unit)) Io_uring.t }

let make ~ring_size =
  { ring =
      Io_uring.create
        ~max_submission_entries:ring_size
        ~max_completion_entries:(ring_size * 2)
  }
;;

let wait t =
  ignore (Io_uring.submit t.ring : int);
  Stdio.Out_channel.flush Stdio.Out_channel.stdout;
  Io_uring.wait t.ring ~timeout:`Never;
  Io_uring.iter_completions t.ring ~f:(fun ~user_data ~res ~flags ->
      let _, callback = user_data in
      callback res flags);
  Io_uring.clear_completions t.ring;
  Stdio.Out_channel.flush Stdio.Out_channel.stdout;
  ()
;;

let global = make ~ring_size:(1024 * 4)
