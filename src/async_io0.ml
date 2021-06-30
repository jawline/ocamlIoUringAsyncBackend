open Core
include Async_kernel

module Time_ns = Core.Time_ns
module Clock_ns = Async_kernel.Clock_ns
module Scheduler = Async_kernel.Async_kernel_scheduler.Private

let sleep d = Clock_ns.after (Time_ns.Span.of_sec d)
let yield () = Scheduler.yield (Scheduler.t ())

let run () =
  printf "Starting up\n";
  let module State = struct
    type t =
      | Idle
      | Running
      | Will_run_soon
  end
  in
  let module Next_wakeup = struct
    type t =
      | At of Time_ns.t * float
      | No_wakeup
      | Soon
  end
  in
  let state = ref State.Idle in
  let rec loop () =
    printf "Me looping\n";
    let t = Scheduler.t () in
    match !state, Scheduler.uncaught_exn t with
    | _, Some _ | State.Running, None -> printf "Stop?\n";
    | (State.Idle | State.Will_run_soon), None ->
      state := State.Running;
      Scheduler.run_cycle t;
      let next_wakeup : Next_wakeup.t =
        if Scheduler.can_run_a_job t
        then Soon
        else (
          match Scheduler.next_upcoming_event t with
          | None -> No_wakeup
          | Some next ->
            let now = Time_ns.now () in
            let d = Time_ns.diff next now in
            let d_ms = Time_ns.Span.to_ms d in
            if Float.( <= ) d_ms 0. then Soon else At (next, d_ms))
      in
      Option.iter (Scheduler.uncaught_exn_unwrapped t) ~f:(fun (exn, _sexp) -> raise exn);
      (match next_wakeup with
       | No_wakeup -> printf "I was told to stop forever\n"; state := Idle
       | Soon  | At _ (* (_at, _dm_ms) *)->
         state := Will_run_soon;
         loop ())
  in
      state := State.Will_run_soon;
      loop ();
;;

let initialized_ref = ref false

let initialization =
  lazy
    (let t = Scheduler.t () in
     initialized_ref := true;
     Scheduler.set_job_queued_hook t (fun _ -> ());
     Scheduler.set_event_added_hook t (fun _ -> ());
     Scheduler.set_thread_safe_external_job_hook t (fun _ -> ());
     Async_kernel.Monitor.detach_and_iter_errors
       Async_kernel.Monitor.main
       ~f:(fun _exn -> ());
     run ())
;;

let init () = force initialization
let initialized () = !initialized_ref
