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
open Low;;
open Video;;
open Event_manager;;
open Object;;
open Interface;;

(** GUI event parser *)

let ev_iface_parser a iface curs=
  (match a.etype with
     | "mouse" ->
	 curs#move a.ex a.ey;
	 (match a.eval with
	    | "motion" -> 
		iface#mouseover a.ex a.ey;
	    | "released" -> 
		iface#release a.ex a.ey; 
		curs#set_state "normal";
	    | "pressed" -> 
		iface#click a.ex a.ey; 
		curs#set_state "clicked";
	    | _ -> ()
	 )
     | "keyboard" ->
	 (match a.eval with
	    | "pressed" -> 
		iface#keypress a
	    | _ -> ()
	 )
     | _ -> ()
  )



