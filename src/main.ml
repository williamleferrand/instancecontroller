(*
 * Instance manager
 *)

(* Forget about LWT, it eats signals somewhere .. ;( *)
open Misc

let _ = 
  display ">>> Instance manager <<<" ; 
  try 
    let targets = Targets.from_file (Conf.get_param "targets") in         
    List.iter (Basic.track (Conf.get_param_int "max_retry")) targets     
  with e -> 
    (* Report.panic (Printf.sprintf "Panic: exception %s raised, exiting now" (Printexc.to_string e)) ; *)
    display "Panic: exception %s raised, exiting now" (Printexc.to_string e)
          
    
