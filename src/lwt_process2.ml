(* Add on to lwt_process *)

open Lwt

type command = string * string array

let shell cmd = ("/bin/sh", [| "/bin/sh"; "-c"; cmd |])

type redirection =
    [ `Keep
    | `Dev_null
    | `Close
    | `FD_copy of Unix.file_descr
    | `FD_move of Unix.file_descr ]

(* +-----------------------------------------------------------------+
   | Spawing commands                                                |
   +-----------------------------------------------------------------+ *)

let redirect fd redirection = match redirection with
  | `Keep ->
      ()
  | `Dev_null ->
      Unix.close fd;
      let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o666 in
      if fd <> dev_null then begin
        Unix.dup2 dev_null fd;
        Unix.close dev_null
      end
  | `Close ->
      Unix.close fd
  | `FD_copy fd' ->
      Unix.dup2 fd' fd
  | `FD_move fd' ->
      Unix.dup2 fd' fd;
      Unix.close fd'

let spawn (prog, args) env ?(stdin:redirection=`Keep) ?(stdout:redirection=`Keep) ?(stderr:redirection=`Keep) toclose =
  match Unix.fork() with
    | 0 ->
        redirect Unix.stdin stdin;
        redirect Unix.stdout stdout;
        redirect Unix.stderr stderr;
        List.iter Unix.close toclose;
        begin
          try
            match env with
              | None ->
                  Unix.execvp prog args
              | Some env ->
                  Unix.execvpe prog args env
          with _ ->
            (* Prevent hooks from running, otherwise thay may use
               notifications and the result would be unspecified. *)
            Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
            exit 127
        end
    | id ->
        let close = function
          | `FD_move fd ->
              Unix.close fd
          | _ ->
              ()
        in
        close stdin;
        close stdout;
        close stderr;
        id

type state =
  | Running
  | Exited of Unix.process_status

let status (pid, status, rusage) = status
let rusage (pid, status, rusage) = rusage

class virtual common timeout (w : (int * Unix.process_status * Lwt_unix.resource_usage) Lwt.t) pid =
  let status = lazy(w >|= status) and rusage = lazy(w >|= rusage) in
object(self)

  method virtual close : Unix.process_status Lwt.t

  method pid = pid

  method state = match Lwt.poll w with
    | None -> Running
    | Some(pid, status, rusage) -> Exited status

  method kill signum = match Lwt.poll w with
    | None -> Unix.kill pid signum
    | Some _ -> ()

  method status = Lazy.force status
  method rusage = Lazy.force rusage

  initializer
    match timeout with
      | None ->
          ()
      | Some dt ->
          Lwt.ignore_result begin
            try_lwt
              lwt _ = Lwt.pick [Lwt_unix.timeout dt; w] in
              return ()
            with
              | Lwt_unix.Timeout ->
                  (try Unix.kill pid Sys.sigkill with _ -> ());
                  (try_lwt self#close >> return () with _ -> return ())
              | _ ->
                  return ()
          end
end

class process_none ?timeout ?env ?stdin ?stdout ?stderr cmd =
  let pid = spawn cmd env ?stdin ?stdout ?stderr [] in
  let w = Lwt_unix.wait4 [] pid in
  let close = lazy(w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
end


class process_none_from_pid ?timeout pid = 
  let w = Lwt_unix.wait4 [] pid in
  let close = lazy(w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
end


let open_process_none_from_pid ?timeout pid = new process_none_from_pid ?timeout pid
