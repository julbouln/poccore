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
open Config;;
open Video;;
open Object;;
open Event;;
open Cursors;;

(** Stage objects class definitions *)

let current_stage=ref "none";;

let bfr_exit()=
  exit(1);;

(* stage class *)
class stage (cursor:cursors)=
object (self)
  val mutable initialized=false
  val curs=cursor
  method get_initialized=initialized


  method on_load()=()
  method on_loop()=()
  method on_quit()=()
  method on_reinit()=()
  method on_continue()=()
  method on_leave()=()

  method get_curs=curs
  
  method load()=
    initialized<-true;
    self#on_load();
    let loop delay=self#on_loop() in
    Callback.register "loop" (loop);
    Callback.register "quit" (self#on_quit);
    event_loop !ev_a !fps
  
  method continue()=
    self#on_continue();
    let loop delay=self#on_loop() in
    Callback.register "loop" (loop);
    Callback.register "quit" (self#on_quit);
    event_loop !ev_a !fps

  method reinit()=self#on_reinit();
  method leave()=self#on_leave();

end;;


(* create an Hashtbl of stages *)
let stages_create curs=let a=Hashtbl.create 2 in Hashtbl.add a "none" (new stage curs);a;;

(* add a stage in stages *)
let stage_add stages n s=
  if (Hashtbl.mem stages n)==true then
    (
     let v=Hashtbl.find stages n in
     v#reinit();
     Hashtbl.replace stages n s;
    )
  else
    (
     Hashtbl.add stages n s;
    );;

(* check for a stage in stages *)
let stage_is stages n=Hashtbl.mem stages n;;

(* get a stage in stages *)
let stage_get stages n=Hashtbl.find stages n;; 

(* load a stage in stages *)
let stage_load stages n=
  (Hashtbl.find stages n)#get_curs#set_state "normal";
  if !current_stage<>"none" then
    (Hashtbl.find stages !current_stage)#leave();   
  current_stage:=n;
  (Hashtbl.find stages n)#load();; 

(* continue with a stage in stages *)
let stage_continue stages n=
  (Hashtbl.find stages n)#get_curs#set_state "normal";
  (Hashtbl.find stages !current_stage)#leave(); 
  current_stage:=n;
  (Hashtbl.find stages n)#continue();; 

(* leave a stage in stages *)
let stage_leave stages n=(Hashtbl.find stages n)#leave();; 

let stage_connect v=v#init();;


