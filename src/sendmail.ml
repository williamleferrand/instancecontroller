(*
 * lwtgrid 1.1
 *
 * (c) 2011 William Le Ferrand
 *)

open Misc

let (|>) x f = f x

exception SMTP_error of int * string list

type socket =
  | Unix_socket of Unix.file_descr
  | SSL_socket of Ssl.socket

let gmail_user_name = Conf.get_param "mailing_name"
let gmail_user = Conf.get_param "mailing_user"
let gmail_pwd = Conf.get_param "mailing_password"

type message = 
    { 
      target_name : string ; 
      target_email : string ;
      subject : string ; 
      body_plain : string ;
      body_html : string ; }

let create_message to_header subject s_plain s_html =
  let b = Buffer.create 1024 in
    Netsendmail.wrap_mail
      ~from_addr:(gmail_user_name, gmail_user)
      ~to_addrs:[to_header] ~subject:subject
      (Netsendmail.wrap_parts
	 ~content_type:("multipart/alternative", [])
	 [ Netsendmail.wrap_attachment
             ~content_type:("text/plain", [])
             (new Netmime.memory_mime_body s_plain);
           Netsendmail.wrap_attachment
             ~content_type:("text/html", [])
             (new Netmime.memory_mime_body s_html)
	 ]) |>  Netmime.write_mime_message (new Netchannels.output_buffer b);
        Buffer.contents b

let resolve name =
  try Unix.inet_addr_of_string name
  with Failure _ ->
        let h = Unix.gethostbyname name in
          h.Unix.h_addr_list.(0)

let socket_connect host port =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect s (Unix.ADDR_INET((resolve host), port));
    s

class smtp_client ?(hostname = "smtp.gmail.com") ?(port = 25) () =
  object(self)
    val mutable channel = Unix_socket (socket_connect hostname port)
    val mutable debug_level = 0
    val crlf_regexp = Str.regexp "\r\n"
    val new_line_regexp = Str.regexp "\\(\r\n\\|\r\\|\n\\)"

    initializer
      self#handle_reply

    method set_debug_level n =
      debug_level <- n

    method private input_line =
      let input = match channel with
        | Unix_socket s -> Unix.read s
        | SSL_socket s -> Ssl.read s in
      let s = String.create 1 and b = Buffer.create 80 in
        input s 0 1 |> ignore;
        while s.[0] <> '\n' do
          Buffer.add_char b s.[0];
          try input s 0 1 |> ignore
          with End_of_file -> s.[0] <- '\n'
        done;
        Buffer.contents b

    method private output_string s =
      let output = match channel with
        | Unix_socket s -> Unix.write s
        | SSL_socket s -> Ssl.write s in
      let really_output s pos len =
        let rec print_rest n =
          if n < len
          then
            let m = output s (pos+n) (len-n) in
              print_rest (n+m)
          else
            () in
      print_rest 0 in
        really_output s 0 (String.length s)

    method private handle_reply =
      let rec read acc =
        let l = self#input_line in
          if debug_level > 0 then Printf.printf "S: %s\n%!" l;
          if l.[3] = '-' then read (l::acc)
          else int_of_string (String.sub l 0 3) , List.rev (l::acc) in
      let code, msg = read [] in
        match code/100 with
          | 2 | 3 -> ()
          | _ -> raise (SMTP_error (code, msg))

    method private smtp_cmd cmd =
      if debug_level > 0 then Printf.printf "C: %s\n%!" cmd;
      self#output_string cmd;
      self#output_string "\r\n";
      self#handle_reply

    method ehlo ?host () =
      self#smtp_cmd ("EHLO " ^ (
        match host with
          | None -> (Unix.gethostbyname (Unix.gethostname ())).Unix.h_name
          | Some s -> s
        ))

    method starttsl =
      self#smtp_cmd "STARTTLS";
        let ssl_context = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
        let s = match channel with
                  | Unix_socket s -> s
                  | SSL_socket _ -> assert false in (* TODO *)
        let ssl_s = Ssl.embed_socket s ssl_context in
          Ssl.connect ssl_s;
          channel <- SSL_socket ssl_s;

    method login user password =
      let encoded_login =
        Netencoding.Base64.encode
          (Printf.sprintf "%s\000%s\000%s" user user password) in
        self#smtp_cmd ("AUTH PLAIN " ^ encoded_login)

    method mail addr = self#smtp_cmd (Printf.sprintf "MAIL FROM:<%s>" addr)

    method rcpt addr = self#smtp_cmd (Printf.sprintf "RCPT TO:<%s>" addr)
    
    method data email_string =
      self#smtp_cmd "DATA";
      email_string |>
        Str.global_replace new_line_regexp "\r\n" |>
          Str.split crlf_regexp |>
            List.iter (fun s ->
              self#output_string (if String.length s > 0 && s.[0] = '.' then
                                    ("." ^ s ^ "\r\n")
                                  else s^"\r\n"));
      self#smtp_cmd "."

    method quit = self#smtp_cmd "QUIT"
  end
    
let sendmail msg_list =
  
  let client = new smtp_client ~hostname:"smtp.gmail.com" ~port:587 () in
  try
    client#set_debug_level 1;(* Uncomment to see protocol in action *) 
    client#ehlo ~host:(Conf.get_param "host") ();
    client#starttsl;
    client#ehlo ~host:(Conf.get_param "host") ();
  
    client#login gmail_user gmail_pwd;
    
    List.iter (
      fun msg -> 
	client#mail gmail_user;
	client#rcpt (snd (msg.target_name, msg.target_email));
	let email_as_string = 
	  create_message 
	    (msg.target_name, msg.target_email) 
	    msg.subject
	    msg.body_plain
	    msg.body_html in
	
	client#data email_as_string;
    ) msg_list ; 
    
    client#quit
  with
    | SMTP_error (code, _) as e -> Printf.printf "exit with error code %d\n" code; raise e


let _ = 
      Ssl.init();;


