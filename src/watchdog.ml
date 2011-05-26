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
      Ptrace.cont pid s; monitor pid  
    | _, (Unix.WEXITED s)  ->
      display "process %d has exited with code %d" pid s ;
      fail (ProcessHasExited s)
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      fail (ProcessHasExited s)
    
(* Monitor a process ********************************************************************)

let attach target pid = 
  display "Attaching service with pid %d" pid ;  
  let process = Lwt_process2.open_process_none_from_pid pid in
  display "Process launched with pid %d" (process#pid) ; 
  (try
    Ptrace.attach pid ;
  with _ -> raise (CantAttach pid)); 
  monitor pid

let launch (service, process, args) = 
  let process = Lwt_process.open_process_none (process, (Array.of_list args)) in 
  let pid = process#pid in
  display "Process launched with pid %d" pid;
  (try
     Ptrace.attach pid ;
   with _ -> raise (CantAttach pid));
  Proc.save_pid service pid 
  >>= fun _ -> monitor pid

(* Track a service **********************************************************************)

let track ((service, process, args) as target) =
  (* first we look if the process is here *)
  catch 
    (fun () -> 
      Proc.read_pid service
      >>= attach target)
    (function
      | Proc.NoPid _ -> (* no process, we start a fresh one *) launch target
      | CantAttach _ -> launch target 
      | _ as e -> fail e) 
