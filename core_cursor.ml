(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

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


open Value_common;;
open Value_lua;;

open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_graphic;;
open Core_anim;;

(* FIXME why so static ? *)

(** Cursor *)

(** Cursor class : handler of graphic cursor *)
class cursors w h (fc:string option)=
object(self)
  inherit generic_object
  inherit lua_object as lo
  val mutable g=
      match fc with
	| Some f->video#hide_cursor();new graphic_from_file f w h
	| None ->video#show_cursor(); new graphic_object

    val mutable state="normal"

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

    method set_cur_drawing d=
      g#set_cur_drawing d

    method set_state n=
      state<-n;            
      match n with
      | "normal" -> g#set_cur_drawing 0
      | "clicked" -> g#set_cur_drawing 1
      | _ -> g#set_cur_drawing 0;

    method lua_init()=
      lua#set_val (OLuaVal.String "get_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_x));
      lua#set_val (OLuaVal.String "get_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_y));
      lua#set_val (OLuaVal.String "move") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#move);
      lua#set_val (OLuaVal.String "set_cur_drawing") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#set_cur_drawing);
      lo#lua_init()


  end;;



