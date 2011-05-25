(* report an error *)

open Lwt

open Misc
open Sendmail 
      
let q : message Queue.t = Queue.create () 
let c = Lwt_condition.create () 

(* Message queuing ***********************************************************)

let queue message = 
  Queue.add message q; 
  Lwt_condition.signal c ()

let rec watch_and_send () = 
  (match Queue.is_empty q with 
      true -> Lwt_condition.wait c
    | false -> return ())
  >>= fun () -> 
  let message = Queue.take q in 
  catch 
    (fun () -> Lwt_preemptive.detach sendmail [ message ])
    (fun _ -> return ()) >>= fun _ -> watch_and_send ()


(* Utility functions *************************************************************)

let panic issue = 
  queue {
    target_name = "Issuu admin" ; 
    target_email = Conf.get_param "email_admin" ; 
    subject = "Message from Instance Controller"; 
    body_plain = issue ; 
    body_html = issue ;
  }

(* Initializing the queue monitoring *********************************************)

let _ = 
  Lwt.ignore_result (watch_and_send ())
