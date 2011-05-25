(* Read the names of the services to launch *)

open Lwt

exception CantReadTargetsFile of string

let from_file file =
  let s = Str.regexp "[ \t]+" in
  catch 
    (fun () -> 
     lwt ic = Lwt_io.open_file ~mode:Lwt_io.input file in
     let lines = Lwt_io.read_lines ic in
     Lwt_stream.fold 
       (fun binary acc ->
         match Str.split s binary with 
             binary :: args -> (binary, args) :: acc
           | _ -> acc)
       lines
       [] 
     >>= fun l -> Lwt_io.close ic 
     >>= fun _ -> return l)
    (fun _ -> fail (CantReadTargetsFile file))
