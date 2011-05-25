(* Super quick conf file *)

let params = Hashtbl.create 0

let get_param = Hashtbl.find params 
let set_param = Hashtbl.add params

(* Default values ***************************************************************)

let _ = 
  set_param "targets" "targets.conf"


(* Read values from command line ************************************************)

let usage = "instance controller - usage is \n" 

let _ = 
  Arg.parse
    [
      "--targets", Arg.String (set_param "targets"), "<id> : files containing the targets" ;
    ] print_endline usage
