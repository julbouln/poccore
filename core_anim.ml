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
open Core_timer;;

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


class anim_object_with_time (timed_frames : (time * int) list)=
object(self)
  val mutable timer=new timer
  val mutable current_frame=0
  val mutable timed_frames=timed_frames

  method get_frame_time f=(fst (List.nth timed_frames f))

  method get_num_frames=List.length timed_frames
  method get_current=current_frame
  method set_current f=current_frame<-f

  method get_frame=(snd (List.nth timed_frames current_frame))

  method next_frame()=
    timer#reset();
    timer#del_task (self#get_frame_time current_frame);

    if current_frame=self#get_num_frames-1 then (
      current_frame=0;
      timer#add_task (self#get_frame_time 0) self#next_frame;
    )
    else (
      current_frame<-current_frame+1;
      timer#add_task (self#get_frame_time current_frame) self#next_frame;
    )

  initializer
    timer#start();
    timer#add_task (self#get_frame_time 0) self#next_frame
    
  method anim()=
    timer#step()
      
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
class graphic_anim_from_file drawing_vault w h file frames refresh= 
object
  val mutable anim=new anim_object frames refresh
  val mutable graph=new graphic_from_file drawing_vault file w h

  method get_rect=graph#get_rect

  method get_anim=anim

  method move x y=graph#move x y
  method anim()=anim#anim();
  method put()=graph#set_cur_drawing(anim#get_frame);graph#put();

end;;


class graphic_anim drawing_vault frames refresh= 
object
  val mutable anim=new anim_object frames refresh
  val mutable graph=new graphic_object drawing_vault

  method get_anim=anim

  method get_rect=graph#get_rect

  method move x y=graph#move x y
  method anim()=anim#anim();
  method put()=graph#set_cur_drawing(anim#get_frame);graph#put();

end;;
