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

open Core_event;;
open Core_video;;
open Core_medias;;
open Core_cursor;;


open Binding;;


open Olua;;


(** Stage subsystem *)
(** Stage is a high-level container for interface and game engine. When you define a stage, you specify some thing to do when loading, leaving, on each frame and the event parser. You can handle multiple stage through the stages global class. See exemples for more informations *)

(** stage class *)
class stage  (cursor:cursors)=
object (self)
  inherit lua_object
  val mutable initialized=false
  val curs=cursor

(** {2 Virtual part} *)


(** what to do when first load stage *)
  method on_load()=()
(** what to do on each frame *)
  method on_loop()=
    curs#put();    
    video#flip();

(** what to do when quit stage *)
  method on_quit()=()
  method on_reinit()=()
(** what to do when continue a stage already loaded *)
  method on_continue()=()
  method on_leave()=()

(** parse the event coming in stage *)
  method ev_parser (e:event)=()		      


(** {2 General part} *)

  (** get if stage initialized *)
  method get_initialized=initialized


  method get_curs=curs
  
  method load()=
    initialized<-true;
    self#on_load();

    eventm#set_parser self#ev_parser;
    eventm#set_on_quit self#on_quit;
    eventm#set_on_loop self#on_loop;
    eventm#init();
    eventm#main();()
  
  method continue()=
    self#on_continue();

    eventm#set_parser self#ev_parser;
    eventm#set_on_quit self#on_quit;
    eventm#set_on_loop self#on_loop;
    eventm#init();
    eventm#main();()

  method reinit()=self#on_reinit();
  method leave()=self#on_leave();

end;;

exception Stage_not_found of string;;

(** General stages handler *)
class stages curs=
object(self)
  inherit lua_object as lo

  val mutable current_stage="none"

  (* create an Hashtbl of stages *)
  val mutable stages=let a=Hashtbl.create 2 in Hashtbl.add a "none" (new stage curs);a;

  (** add a stage in stages *)
  method stage_add n s=
    s#lua_init();
    self#lua_parent_of n (s:>lua_object);

    if (Hashtbl.mem stages n)==true then
      (
	let v=self#stage_get n in
	  v#reinit();
	  Hashtbl.replace stages n s;
      )
    else
      (
	Hashtbl.add stages n s;
      );

  (** check for a stage in stages *)
  method stage_is n=Hashtbl.mem stages n;

  (** get a stage in stages *)
  method stage_get n=
    (try
    Hashtbl.find stages n;
     with Not_found -> raise (Stage_not_found n))

  (** load a stage in stages *)
  method stage_load n=
    (self#stage_get n)#get_curs#set_state "normal";
    if current_stage<>"none" then
      (self#stage_get current_stage)#leave();   
    current_stage<-n;
    (self#stage_get n)#load();
  
  (** continue with a stage in stages *)
  method stage_continue n=
    (self#stage_get n)#get_curs#set_state "normal";
    (self#stage_get current_stage)#leave(); 
    current_stage<-n;
    (self#stage_get n)#continue();

  (** leave a stage in stages *)
  method stage_leave n=(self#stage_get n)#leave();
  
(*  method stage_connect v=v#init(); *)

  method lua_init()=
    lua#set_val (OLuaVal.String "load") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#stage_load);
    lua#set_val (OLuaVal.String "continue") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#stage_continue);
    lua#set_val (OLuaVal.String "leave") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#stage_leave);
    lo#lua_init()

end;;


let generic_cursor=new cursors 30 30 None;;
let stages=new stages generic_cursor;;


(* FIXME : must declare stages here like video and audio *)
