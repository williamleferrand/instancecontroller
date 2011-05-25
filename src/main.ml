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
       Report.panic "Hi Anders, <br/> <br/> I'm Instance Controller, an OCaml program living in the EC2 planet, designed and implemented to make your life easier.<br/><br/>Soon, I'll be launching and monitoring processes in the cloud and I'll ping you if I face a problem that I can't recover. After all, I'm only a bot ... <br /><br />Yours faithfully,<br /> <br />Instance Controller" ;
       Lwt_unix.sleep 20.0

    )
    
