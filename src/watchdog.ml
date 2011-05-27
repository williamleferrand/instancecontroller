(* launch a process *)

(* let's forget the env for the moment *)
(* take care, it's not *that* easy, you have to fork before starting the monitor *)

open Lwt
open Misc

exception CantAttach of int
exception ProcessHasExited of int
exception DmzCantAttach

(* Monitoring function (track the signals) **********************************************)
    
let rec monitor_ptrace pid = 
  display "Waiting for data from process %d" pid ;
  Lwt_unix.waitpid [ Lwt_unix.WUNTRACED ] pid 
  >>= function  
    | _, (Unix.WSTOPPED (-14)) ->
      display "child died, time to leave"; 
      exit (1)
    | _, (Unix.WSTOPPED s) ->
      display "process %d caught signal %d" pid s ; 
      Ptrace.cont pid s ;
      (* Ptrace.detach pid ; *) 
      (*
        Ptrace.detach pid ; 
        let wait, _ = wait () in 
        wait >>= fun _ -> *)
      monitor_ptrace pid  
    | _, (Unix.WEXITED s)  ->
      display "process %d has exited with code %d" pid s ;
      exit s 
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      exit s

let monitor_std pid = 
  display "Waiting for data from (std) process %d" pid ; 
   Lwt_unix.waitpid [] pid 
  >>= function 
    | _, (Unix.WSTOPPED s) ->
      assert false 
    | _, (Unix.WEXITED 12)  -> fail DmzCantAttach
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
    | _, (Unix.WSTOPPED s) -> assert false 
    | _, (Unix.WEXITED s)  ->
      display "from dmz : process %d exited with return code %d" pid s ;
      
      exit s
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
         display "service %s spawned from dmz with pid %d" service pid ;
         monitor_dmz pid 
     | dmz_pid -> return dmz_pid
       
             
        
(* Monitor a process ********************************************************************)

let attach ((service, process, args) as target) pid = 
  display "Attaching service with pid %d" pid ;  
  match Unix.fork () with 
      0 -> 
        (* DMZ is reparenting the service *)
        (try
           Ptrace.attach pid ;
         with _ -> exit 12) ;
        monitor_ptrace pid
    | dmz_pid ->
      Proc.save_pid_blocking service dmz_pid ;
      monitor_std dmz_pid 

let launch ((service, process, args) as target) = 
  lwt pid = superspawn target in
  display "Process %s launched with pid %d (it's the pid of the dmz)" process pid;
  Proc.save_pid_blocking service pid ;
  display "we saved the pid" ;      
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
      | DmzCantAttach -> display "Error, DMZ can't attach"; launch target
      | CantAttach _ -> display "Error, can't attach"; launch target 
      | _ as e -> fail e)

