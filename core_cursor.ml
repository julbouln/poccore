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

open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_graphic;;
open Core_anim;;

(* FIXME why so static ? *)

(** Cursor class : handler of graphic cursor *)


class cursors w h (fc:string option)=
  object
    val mutable g=
      match fc with
	| Some f->video#hide_cursor();new graphic_from_file f w h
	| None ->video#show_cursor(); new graphic_object
(*    val mutable viseur=new graphic_object_anim 42 40 "medias/misc/viseur.png" [|0;1;2;3;4;3;4;3|] 2
*)
    val mutable state="normal"
    val mutable pl=1

    method get_x=g#get_rect#get_x;
    method get_y=g#get_rect#get_y;
 
    method move x y=
      g#move x y
    method put()=
      match fc with
	| Some v-> g#put()
	| None -> ()
(*      if state<>"on_ennemy" then
	g#put()
      else (
	viseur#anim();
	viseur#put();
      )
*)
    method get_state=state
    method set_player p=pl<-p
    method set_state n=
      state<-n;            
      match n with
      | "normal" -> g#set_cur_drawing (if pl=1 then 0 else 2)
      | "clicked" -> g#set_cur_drawing (if pl=1 then 1 else 3)
      | "bottom" -> g#set_cur_drawing 4
      | "top" -> g#set_cur_drawing 5
      | "left" -> g#set_cur_drawing 6
      | "right" -> g#set_cur_drawing 7
      | "can_attack" -> g#set_cur_drawing 8
      | "on_ennemy" -> ()
      | _ -> g#set_cur_drawing 0;


  end;;



