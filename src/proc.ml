(* search for services *)

(* we can either retrieve running services from the ps *)

(* or read pid files from the system *)

open Lwt

let pid_file = 
  Printf.sprintf "%s%s.pid" (Conf.get_param "pid_dir")

let save_pid service pid = 
  Lwt_io.open_file ~mode:Lwt_io.output (pid_file service)
  >>= fun oc -> Lwt_io.write_line oc (string_of_int pid) 
  >>= fun _ -> Lwt_io.close oc 

exception NoPid of string

let read_pid service = 
  catch 
    (fun () -> 
      Lwt_io.open_file ~mode:Lwt_io.input (pid_file service) 
      >>= fun ic -> Lwt_io.read_line ic 
      >>= fun pid_s -> Lwt_io.close ic 
      >>= fun _ -> return (int_of_string pid_s))
    (fun e -> fail (NoPid service))
