(*
 * Instance manager
 *)

(* For the moment we only monitor one process, (and we wait for more info about Lwt + signals) *)

open Misc

let _ = 
  display ">>> Instance manager <<<" ; 
  try 
    let targets = Targets.from_file (Conf.get_param "targets") in
    match targets with 
      | [ target ] -> Watchdog.track (Conf.get_param_int "max_retry") target     
      |  _ -> display "Too many processes to track" ; exit 1
  with e -> 
    (* Report.panic (Printf.sprintf "Panic: exception %s raised, exiting now" (Printexc.to_string e)) ; *)
    display "Panic: exception %s raised, exiting now" (Printexc.to_string e)
          
    
