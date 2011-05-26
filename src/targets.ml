(* read the names of the services to launch *)

open Lwt
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
  | [ '\t' ' ']+ -> display "reading space with acc: %s" acc; parse_command_line (append gacc acc) "" lexbuf
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

let from_file file =
  catch 
    (fun () -> 
     lwt ic = Lwt_io.open_file ~mode:Lwt_io.input file in
     let lines = Lwt_io.read_lines ic in
     Lwt_stream.fold 
       (fun binary acc ->
         display "Parsing %s" binary ;
         match parse_command_line [] "" (Ulexing.from_utf8_string binary) with 
             service :: binary :: args as l -> 
               display "Service %s, binary is %s, arguments are [%s]" service  binary (String.concat "|" args); 
               (service, binary, l) :: acc
           | _ -> acc)
       lines
       [] 
     >>= fun l -> Lwt_io.close ic 
     >>= fun _ -> return l)
    (fun _ -> fail (CantReadTargetsFile file))
