include Core
include Async_kernel
module Time_ns = Core.Time_ns
module Time = Time_ns
module Clock_ns = Async_kernel.Clock_ns
module Scheduler = Async_kernel.Async_kernel_scheduler.Private

(* TODO: Move this somewhere sensible *)
let global_scheduler = Scheduler.t ()
