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

(** GUI event manager *)

let ev_iface_parser a iface curs=
  curs#move a.ex a.ey;  
  
  if a.etype="mouse" && a.eval= "released" then (
    iface#release a.ex a.ey; 
    curs#set_state "normal";
   );

  if a.etype="mouse" && a.eval= "pressed" then (
    iface#click a.ex a.ey; 
    curs#set_state "clicked";
   );

  if a.etype="mouse" && a.eval= "motion" then (    
    iface#mouseover a.ex a.ey; 
   );;




let clear_iface_ev_stack curs iface=

(* to work with ocmal-3.0.4 *)
while Stack.length !ev_a <> 0 do
(* while Stack.is_empty !ev_a == false do *)
  let a=(Stack.top !ev_a) in

  curs#move a.ex a.ey;  
  
  if a.etype="mouse" && a.eval= "released" then (
    iface#release a.ex a.ey; 
    curs#set_state "normal";
   );

  if a.etype="mouse" && a.eval= "pressed" then (
    iface#click a.ex a.ey; 
    curs#set_state "clicked";
   );

  if a.etype="mouse" && a.eval= "motion" then (    
    iface#mouseover a.ex a.ey; 
   );


  Stack.pop !ev_a;

done;;
