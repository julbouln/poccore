(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003,2004 POC 

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

(** Main video class *)

(* FIXME : need delete these *)
let tile_w=ref 32;;
let tile_h=ref 32;;

let fps=ref 30;;

class video=
object
  val mutable width=0
  val mutable height=0
  val mutable def_w=0
  val mutable def_h=0
  val mutable depth=0
  val mutable fullscreen=false

  method init w h bpp fs=
    width<-w;
    height<-h;
    depth<-bpp;
    fullscreen<-fs;    
    video_init width height depth fullscreen

  method initialized=
    is_video

  method set_def_size w h=def_w<-w;def_h<-h

  method get_w=width
  method get_h=height
  method get_fs=fullscreen
  method get_d=depth

  method f_size_w w=let f=(float_of_int width)/.(float_of_int def_w) in int_of_float(f*.(float_of_int w))
  method f_size_h h=let f=(float_of_int height)/.(float_of_int def_h) in int_of_float(f*.(float_of_int h))
		      
  method get_fact_w()=(float_of_int width)/.(float_of_int def_w)
  method get_fact_h()=(float_of_int height)/.(float_of_int def_h)

  method set_caption s=
    wm_set_caption s

  method flip=
    video_update    

  method blank=
    video_blank_screen

  method get_tile=
    video_surface_get

  method set_clip x y w h=
    video_set_clip x y w h
  


end;;

(** We declare a global video class since we have only one video screen ! *)
let video=new video;;
