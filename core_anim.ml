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


open Core_medias;;
open Core_graphic;;

(** Anim subsystem *)

(** Anim class *)
class anim_object frames refresh=
object
    val mutable current_frame=0
    val mutable current_refresh=0
    val mutable frames=frames
    val mutable refresh=refresh

    method get_num_frames=Array.length frames
    method set_frames f=frames<-f
    method set_refresh r=refresh<-r
    method get_frames=frames
    method get_refresh=refresh
    method get_frame=frames.(current_frame)
    method get_current=current_frame
    method get_current_refresh=current_refresh
    method set_current c=current_frame<-c
    method set_current_refresh c=current_refresh<-c

    method anim()=
      if current_refresh = refresh then (
	if current_frame < (Array.length frames)-1 then ( 	  
	  current_frame<-current_frame+1;
	  

	 )
	else
	  (
	   current_frame<-0;
	  );
	current_refresh<-0
       )
      else
	current_refresh<-current_refresh+1

    method print_frame=print_int frames.(current_frame)
  end;;


(** Animated graphic object *)
class graphic_with_anim (graph:graphic_object) frames refresh= 
object
  val mutable anim=new anim_object frames refresh

  method get_anim=anim

  method move x y=graph#move x y
  method anim()=anim#anim();
  method put()=graph#set_cur_drawing(anim#get_frame);graph#put();

end;;

(** Animated graphic object *)
class graphic_anim_from_file w h file frames refresh= 
object
  val mutable anim=new anim_object frames refresh
  val mutable graph=new graphic_from_file file w h

  method get_rect=graph#get_rect

  method get_anim=anim

  method move x y=graph#move x y
  method anim()=anim#anim();
  method put()=graph#set_cur_drawing(anim#get_frame);graph#put();

end;;


class graphic_anim frames refresh= 
object
  val mutable anim=new anim_object frames refresh
  val mutable graph=new graphic_object

  method get_anim=anim

  method get_rect=graph#get_rect

  method move x y=graph#move x y
  method anim()=anim#anim();
  method put()=graph#set_cur_drawing(anim#get_frame);graph#put();

end;;
