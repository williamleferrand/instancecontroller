open Misc


exception CantAttach of int
exception ProcessHasExited of int
exception DmzCantAttach

(* Monitoring function (track the signals) **********************************************)
    
let rec monitor_ptrace pid = 
  display "Waiting for data from process %d" pid ;
  match Unix.waitpid [ Lwt_unix.WUNTRACED ] pid with
    | _, (Unix.WSTOPPED (-14)) ->
      display "child died, time to leave"; 
      exit (1)
    | _, (Unix.WSTOPPED s) ->
      display "process %d caught signal %d" pid s ; 
      Ptrace.cont pid s ;
      monitor_ptrace pid  
    | _, (Unix.WEXITED s)  ->
      display "process %d has exited with code %d" pid s ;
      exit s 
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      exit s

let monitor_std pid = 
  display "Waiting for data from (std) process %d" pid ; 
  match Unix.waitpid [] pid with 
    | _, (Unix.WSTOPPED s) ->
      assert false 
    | _, (Unix.WEXITED 12) ->
      raise DmzCantAttach
    | _, (Unix.WEXITED s)  ->
      display "process %d has exited with code %d" pid s ;
      raise (ProcessHasExited s)
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      raise (ProcessHasExited s)

let monitor_dmz pid = 
  display "Waiting for data from (dmz) process %d" pid ; 
  match Unix.waitpid [] pid with
    | _, (Unix.WSTOPPED s) -> display "ASSERT FALSE" ; assert false 
    | _, (Unix.WEXITED s)  ->
      display "from dmz : process %d exited with return code %d" pid s ;
      exit s
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      exit s 

(* Super spawner *)
(* This superspawner create an intermediate process to avoid the final service to become
   orphaned and reparented to init *)

let superspawn (service, process, args) = 
   match Unix.fork () with 
       0 ->  
         (match Unix.fork () with 
           | 0 -> Unix.execvp process args
           | pid -> monitor_dmz pid)
     | dmz_pid -> dmz_pid

(* Monitor a process ********************************************************************)

(* let attach ((service, process, args) as target) pid = 
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
*)

let launch ((service, process, args) as target) = 
  let pid = superspawn target in
  display "Process %s launched with pid %d (it's the pid of the dmz)" process pid;
  Proc.save_pid_blocking service pid ;
  display "we saved the pid" ;      
  monitor_std pid 


(* Track a service **********************************************************************)
(*
let rec track retries ((service, process, args) as target) =
    (* first we look if the process is here *)
  if retries < 1 then 
    (
      (* Total panic code *)
      display ">>>>>>>>>> PANIC" ; 
      display ">> No more retry attempt for service %s, SERVICE DISRUPTION" service ; 
      ()
    )
  else 
    catch 
      (fun () -> 
        catch 
          (fun () -> 
            Proc.read_pid service
            >>= attach target)
          (function
            | Proc.NoPid _ -> display "There is no pid" ; (* no process, we start a fresh one *) launch target
            | DmzCantAttach -> display "Error, DMZ can't attach"; launch target
            | CantAttach _ -> display "Error, can't attach"; launch target 
            | _ as e -> fail e)) 
      (fun e -> track (retries - 1) target)

*)

let rec track retries ((service, process, args) as target) = 
   if retries < 1 then 
    (
      (* Total panic code *)
      display ">>>>>>>>>> PANIC" ; 
      display ">> No more retry attempt for service %s, SERVICE DISRUPTION" service ; 
      ()
    )
  else 
     (
       try 
         launch target 
       with _ -> track (retries - 1) target
     )


let _ = 
  display ">>> Instance manager <<<" ; 
  let p = "/Users/hypios/reloaded/dev/issuu/backend-piper/ocaml/_build/src/ad_serving/service.native" in 
  track 3 ("test", 
           p, 
           [| p |])
    
