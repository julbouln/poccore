(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003 POC 

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Unix;;

(** OUTDATED ? *)

let establish_server_own server_fun sockaddr =
(*   let domain = domain_of sockaddr in *)
   let sock = Unix.socket PF_INET Unix.SOCK_STREAM 0 
   in Unix.bind sock sockaddr ;
      Unix.listen sock 3;
      while true do
        let (s, caller) = Unix.accept sock 
        in 
	  (
	    match Unix.fork() with
		0 -> 
		  if Unix.fork() <> 0 then exit 0; 		 		 
		  print_string "SERVER: new client connected.";print_newline();
                  let inchan = Unix.in_channel_of_descr s 
                  and outchan = Unix.out_channel_of_descr s 
                  in server_fun inchan outchan ;
                    close_in inchan ;
                    close_out outchan ;
                    exit 0
		      
              | id -> 
		  Unix.close s; ignore(Unix.waitpid [] id);
	  );

      done ;;

let get_my_addr()=
  (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0);;

let client_connect my_address port=
open_connection (Unix.ADDR_INET(my_address,port));;

let shutdown_connection inchan=
  Unix.shutdown (Unix.descr_of_in_channel inchan) Unix.SHUTDOWN_SEND;;

let server_start serv_fun my_address port=
  establish_server_own serv_fun  (Unix.ADDR_INET(my_address, port));;

let client_start client_fun my_address port=
  let ic,oc=client_connect my_address port in
    client_fun ic oc;


