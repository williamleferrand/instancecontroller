(*
 * Instance manager
 *)

open Lwt
open Misc

let _ = 
  display ">>> Instance manager <<<" ; 
  Lwt_main.run
    (
       lwt targets = Targets.from_file (Conf.get_param "targets") in
       Report.panic "coucou" ;
       return ()
    )
    
