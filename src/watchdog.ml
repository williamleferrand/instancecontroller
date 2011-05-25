(* launch a process *)

(* let's forget the env for the moment *)

open Lwt
open Misc

exception CantAttach of int
exception ProcessHasExited of int

(* Monitoring function (track the signals) **********************************************)

let rec monitor pid = 
  Lwt_unix.waitpid [ Lwt_unix.WUNTRACED ] pid 
  >>= function 
    | _, (Unix.WSTOPPED s) ->
      display "process %d caught signal %d" pid s ; 
      Ptrace.cont pid s ; monitor pid  
    | _, (Unix.WEXITED s)  ->
      display "process %d has exited with code %d" pid s ;
      fail (ProcessHasExited s)
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      fail (ProcessHasExited s)
    

(* Monitor a process ********************************************************************)

let attach pid = 
  let process = Lwt_process2.open_process_none_from_pid pid in
  display "Process launched with pid %d" (process#pid) ; 
  (try
    Ptrace.attach pid ;
  with _ -> raise (CantAttach pid)); 
  monitor pid

let launch (process, args) = 

  let process = Lwt_process.open_process_none (process, (Array.of_list args)) in 
  let pid = process#pid in
  display "Process launched with pid %d" pid;
  (try
     Ptrace.attach pid ;
   with _ -> raise (CantAttach pid)); 
  monitor pid


(*
  let pid = Lwt_process2.spawn (process, Array.of_list args) None [] in 
  display "Process launched with pid %d\n" pid ; 
  Lwt_unix.sleep infinity
*)
