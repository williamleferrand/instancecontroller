(* read the names of the services to launch *)

open Misc

exception CantReadTargetsFile of string

(* Incredibly inefficient Ulex code here *******************************************************)

let append gacc acc = 
  match String.length acc with 
      0 -> gacc 
    | _ -> acc :: gacc 

let rec parse_command_line gacc acc = 
  lexer 
  |  '\'' -> read_until_apostrophe gacc acc lexbuf 
  |  '\"' -> read_until_qmark gacc acc lexbuf 
  |  '#' -> []
  | [ '\t' ' ']+ -> parse_command_line (append gacc acc) "" lexbuf
  | eof -> List.rev (append gacc acc)
  | _ -> parse_command_line gacc (acc ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf

and read_until_apostrophe gacc acc = 
  lexer
    | [ '\'' ] -> parse_command_line (append gacc acc) "" lexbuf
    | eof -> List.rev gacc 
    | _ -> read_until_apostrophe gacc (acc ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf

and read_until_qmark gacc acc = 
  lexer
    | [ '\"' ] -> parse_command_line (append gacc acc) "" lexbuf
    | eof -> List.rev gacc 
    | _ -> read_until_qmark gacc (acc ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf


(* Apply the Ulex runtime *********************************************************************)

(* 
let from_file file =
  catch 
    (fun () -> 
      let ic = open_in file in 
      let targets = ref [] in 
      (try 
         while true do 
           let line = input_line ic in
          
      with End_of_file -> ()); 
          
          while 
     lwt ic = Lwt_io.open_file ~mode:Lwt_io.input file in
     let lines = Lwt_io.read_lines ic in
     Lwt_stream.fold 
       (fun binary acc ->
        
         match parse_command_line [] "" (Ulexing.from_utf8_string binary) with 
             service :: (binary :: args as l) -> 
               display "Service %s, binary is %s, arguments are [%s]" service  binary (String.concat "|" args); 
               (service, binary, l) :: acc
           | _ -> acc)
       lines
       [] 
     >>= fun l -> Lwt_io.close ic 
     >>= fun _ -> return l)
    (fun _ -> fail (CantReadTargetsFile file))
*)

(* the straight way *)

let from_file file = 
  try 
    let ic = open_in file in 
    let targets = ref [] in 
    (try 
       while true do 
         let line = input_line ic in
         match parse_command_line [] "" (Ulexing.from_utf8_string line) with 
             service :: (binary :: args as l) -> 
               display "Service %s, binary is %s, arguments are [%s]" service  binary (String.concat "|" args); 
               targets := (service, binary, (Array.of_list l)) :: !targets 
           | _ -> () 
       done ;
       assert false
             
 with End_of_file -> ()); 
    !targets
  with _ -> raise  (CantReadTargetsFile file)
