(* Super quick conf file *)

let params = Hashtbl.create 0

exception MissingParam of string 

let get_param p = try Hashtbl.find params p with _ -> raise (MissingParam p) 
let set_param = Hashtbl.add params

(* Default values ***************************************************************)

let _ = 
  set_param "targets" "targets.conf" ; 
  set_param "mailing_name" "Instance Controller" ;
  set_param "host" "localhost" ;
(*  set_param "mailing_user" Private.gmail_username ; (* <- not in the repository ! *)
  set_param "mailing_password" Private.gmail_password ; 
*)  
  set_param "email_admin" "william@corefarm.com" ; 
  set_param "pid_dir" "" 


(* Read values from command line ************************************************)

let usage = "instance controller - usage is \n" 

let _ = 
  Arg.parse
    [
      "--targets", Arg.String (set_param "targets"), "<id> : files containing the targets" ;
    ] print_endline usage
