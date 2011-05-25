(* launch a process *)

(* let's forget the env for the moment *)

open Lwt
open Misc

let launch (process, args) = 
  let process = Lwt_process.open_process_none (process, (Array.of_list args)) in 
  display "Process launched with pid %d" (process#pid) ; 
  process#status 
  >>= fun exit_status -> 
  display "Process has returned" ; 
  return ()
