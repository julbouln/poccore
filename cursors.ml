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
open Rect;;
open Video;;
open Object;;

open Anim;;

(** Cursor class : handler of graphic cursor *)
class cursors w h f=
  object
    val mutable g=new graphic_scr_resized_object w h f false false

    val mutable viseur=new graphic_object_anim 42 40 "medias/misc/viseur.png" [|0;1;2;3;4;3;4;3|] 2
(*    val mutable g2=new graphic_scr_resized_object 42 40 "medias/misc/viseur.png" false false
    val mutable g2anim=new game_object_anim [|0;1;2;3;4;3;4;3|] 2
*)
    val mutable state="normal"
    val mutable pl=1

    method get_x=g#get_rect#get_x;
    method get_y=g#get_rect#get_y;
 
    method move x y=g#move x y;viseur#move x y(*g2#move x y;*)
    method put()=
      if state<>"on_ennemy" then
	g#put()
      else (
	viseur#anim();
	viseur#put();
(*
	g2anim#anim();
	g2#set_cur_tile (g2anim#get_frame);
	g2#put();
*)
      )
    method get_state=state
    method set_player p=pl<-p
    method set_state n=
      state<-n;      
      match n with
      | "normal" -> g#set_cur_tile (if pl=1 then 0 else 2)
      | "clicked" -> g#set_cur_tile (if pl=1 then 1 else 3)
      | "bottom" -> g#set_cur_tile 4
      | "top" -> g#set_cur_tile 5
      | "left" -> g#set_cur_tile 6
      | "right" -> g#set_cur_tile 7
      | "can_attack" -> g#set_cur_tile 8
      | "on_ennemy" -> ()(*g2#set_cur_tile 0*)
      | _ -> g#set_cur_tile 0;


  end;;



