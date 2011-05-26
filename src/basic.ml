open Misc

exception ProcessHasExited of int

let rec monitor_ptrace pid = 
  match Unix.waitpid [ Unix.WUNTRACED ] pid with 
    | _, (Unix.WSTOPPED s) ->
      display "process %d caught signal %d" pid s ; 
      Ptrace.cont pid s; monitor_ptrace pid  
    | _, (Unix.WEXITED s)  ->
      display "process %d has exited with code %d" pid s ;
      raise (ProcessHasExited s)
    | _, (Unix.WSIGNALED s) ->
      display "process %d was killed by signal %d" pid s ;
      raise (ProcessHasExited s)

let _ = 
  display "Extremely basic watchdog" ;
  display "(to get rid of Lwt masking)" ; 
  
  let pid = int_of_string (Sys.argv.(1)) in 
  display "Catching up process %d" pid ;
  
  Ptrace.attach pid ;
  monitor_ptrace pid 
  
