(* launch a process *)

(* let's forget the env for the moment *)
(* take care, it's not *that* easy, you have to fork before starting the monitor *)

open Lwt
open Misc

exception CantAttach of int
exception ProcessHasExited of int

(* Monitoring function (track the signals) **********************************************)
    
let rec monitor_ptrace pid = 
  display "Waiting for data from process %d" pid ;
  Lwt_unix.waitpid [ Lwt_unix.WUNTRACED ] pid 
  >>= function 
    | _, (Unix.WSTOPPED s) ->
      display "process %d caught signal %d" pid s ; 
      Ptrace.cont pid s; monitor_ptrace pid  
    | _, (Unix.WEXITED s)  ->
      display "process %d has exited with code %d" pid s ;
      fail (ProcessHasExited s)
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      fail (ProcessHasExited s)

let monitor_std pid = 
  display "Waiting for data from (std) process %d" pid ; 
   Lwt_unix.waitpid [] pid 
  >>= function 
    | _, (Unix.WSTOPPED s) ->
      assert false 
    | _, (Unix.WEXITED s)  ->
      display "process %d has exited with code %d" pid s ;
      fail (ProcessHasExited s)
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      fail (ProcessHasExited s)

let monitor_dmz pid = 
  display "Waiting for data from (dmz) process %d" pid ; 
   Lwt_unix.waitpid [] pid 
  >>= function 
    | _, (Unix.WSTOPPED s) ->
      assert false 
    | _, (Unix.WEXITED s)  -> exit s
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      exit s 


(* Super spawner *)
(* This superspawner create an intermediate process to avoid the final service to become
   an orphan *)

let superspawn (service, process, args) = 
   match Unix.fork () with 
       0 ->  
         let handle = Lwt_process.open_process_none (process, (Array.of_list args)) in 
         let pid = handle#pid in
         Proc.save_pid service pid
         >>= fun _ -> 
         monitor_dmz pid 
     | pid -> return pid
       
             
        
(* Monitor a process ********************************************************************)

let attach target pid = 
  display "Attaching service with pid %d" pid ;  
  let process = Lwt_process2.open_process_none_from_pid pid in
  display "Process caught with pid %d" (process#pid) ; 
  (try
     Ptrace.attach pid ;
   with _ -> raise (CantAttach pid)); 
  monitor_ptrace pid

let launch ((service, process, args) as target) = 
  lwt pid = superspawn target in
  display "Process %s launched with pid %d (it's the pid of the dmz)" process pid;
  monitor_std pid 


(* Track a service **********************************************************************)

let track ((service, process, args) as target) =
    (* first we look if the process is here *)
  catch 
    (fun () -> 
      Proc.read_pid service
      >>= attach target)
    (function
      | Proc.NoPid _ -> display "There is no pid" ; (* no process, we start a fresh one *) launch target
      | CantAttach _ -> display "Error, can't attach"; launch target 
      | _ as e -> fail e)

