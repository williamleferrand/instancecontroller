(* 
 * 
 *)

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
    | _, (Unix.WEXITED 12)  -> 
      fail DmzCantAttach
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
             0 -> Unix.execvp process args
           | pid -> monitor_dmz pid) 
     | dmz_pid -> return dmz_pid


let launch ((service, process, args) as target) = 
  superspawn target
  >>= monitor_std

(* Main *)

let rec track retries ((service, process, args) as target) = 
  if retries < 1 then 
    (
       (* Total panic code *)
      display ">>>>>>>>>> PANIC" ; 
      display ">> No more retry attempt for service %s, SERVICE DISRUPTION" service ; 
      return 0
    )
  else 
    (
      catch 
        (fun () -> launch target)
        (fun _ -> track (retries - 1) target)
    )


let _ = 
  display ">>> Instance manager <<<" ; 
  let p = "_build/experiments/broken_process.native" in
  
  Lwt_main.run (track 3 ("test", p, [| p |] ))
    
