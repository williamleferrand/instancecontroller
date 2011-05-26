(* launch a process *)

(* let's forget the env for the moment *)
(* take care, it's not *that* easy, you have to fork before starting the monitor *)

open Lwt
open Misc

exception CantAttach of int
exception ProcessHasExited of int

(* Monitoring function (track the signals) **********************************************)

let rec monitor pid = 
  display "Waiting for data from process %d" pid ;
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
  display "Process caught with pid %d" (process#pid) ; 
  (try
    Ptrace.attach pid ;
  with _ -> raise (CantAttach pid)); 
  monitor pid

let launch (service, process, args) = 
  let handle = Lwt_process2.open_process_none (process, (Array.of_list args)) in 
  let pid = handle#pid in
  display "Process %s launched with pid %d" process pid;

  (try
     display "about to trace"; 
     Ptrace.attach pid ;
     display "traced!" ; 
   with _ -> display "oops" ; raise (CantAttach pid));
  
  monitor pid 

(*
(*  Proc.save_pid service pid 
  >>= fun _ -> 
*)
  
  Lwt_unix.sleep 5.0
  >>= fun _ -> 
  
  display "has killed" ;
  return ()
*)
(* Track a service **********************************************************************)

let track ((service, process, args) as target) =
  launch target
 
(*
  (* first we look if the process is here *)
  catch 
    (fun () -> 
      Proc.read_pid service
      >>= attach target)
    (function
      | Proc.NoPid _ -> display "There is no pid" ; (* no process, we start a fresh one *) launch target
      | CantAttach _ -> display "Error, can't attach"; launch target 
      | _ as e -> fail e)
*)
