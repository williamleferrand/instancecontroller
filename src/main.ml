(*
 * Instance manager
 *)

open Lwt
open Misc

let _ = 
  display ">>> Instance manager <<<" ; 
  Lwt_main.run
    (
      catch 
        (fun () -> 
         lwt targets = Targets.from_file (Conf.get_param "targets") in         
         Lwt_list.iter_s Watchdog.track targets)
        (fun e -> 
(*          Report.panic (Printf.sprintf "Panic: exception %s raised, exiting now" (Printexc.to_string e)) ; *)
          display "Panic: exception %s raised, exiting now" (Printexc.to_string e);  return ()
          (* Lwt_unix.sleep 20.0 *) (* Wait for the email to be actually sent *)))
    
