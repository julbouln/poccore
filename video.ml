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

(** Video subsystem *)
(** The global video class provide some interaction with the video screen like init or refresh it.
You can define a default size for your screen to automaticly resize tile (see graphic object) 
*)


(* FIXME : need delete these *)
let tile_w=ref 32;;
let tile_h=ref 32;;

let fps=ref 30;;

(** Video class *)
class video=
object
  val mutable width=0
  val mutable height=0
  val mutable def_w=0
  val mutable def_h=0
  val mutable depth=0
  val mutable fullscreen=false

(** init then video screen with width height depth fullscreen *)
  method init w h bpp fs=
    width<-w;
    height<-h;
    depth<-bpp;
    fullscreen<-fs;    
    video_init width height depth fullscreen

(** get if video screen is initialized *)
  method initialized=
    is_video

(** set the default size *)
  method set_def_size w h=def_w<-w;def_h<-h

(** get screen width *)
  method get_w=width
(** get screen height *)
  method get_h=height
(** get if fullscreen *)
  method get_fs=fullscreen
(** get depth *)
  method get_d=depth

(** calculate the width size from the ratio *)
  method f_size_w w=let f=(float_of_int width)/.(float_of_int def_w) in int_of_float(f*.(float_of_int w))
(** calculate the height size from the ratio *)
  method f_size_h h=let f=(float_of_int height)/.(float_of_int def_h) in int_of_float(f*.(float_of_int h))

(** get the width ratio from default width and real width *)		      
  method get_fact_w()=(float_of_int width)/.(float_of_int def_w)
(** get the height ratio from default height and real height *)
  method get_fact_h()=(float_of_int height)/.(float_of_int def_h)

(** set title of screen *)
  method set_caption s=
    wm_set_caption s

(** refresh screen *)
  method flip=
    video_update    

(** blank the screen *)
  method blank=
    video_blank_screen

(** get video screen tile *)
  method get_tile=
    video_surface_get

(** set video screen clip *)
  method set_clip x y w h=
    video_set_clip x y w h
  


end;;

(** We declare a global video class since we have only one video screen ! *)
let video=new video;;
