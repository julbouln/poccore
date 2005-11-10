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


open Core_drawing;;
(*open Binding;;*)

(** Video subsystem *)
(** The global video class provide some interaction with the video screen like init or refresh it.
You can define a default size for your screen to automaticly resize tile (see graphic object) 
*)



(** Video class *)
class ['a] video (drawing_vault : ('a) drawing_vault)=
object
  val mutable screen=drawing_vault#new_drawing_screen()
  val mutable width=0
  val mutable height=0
  val mutable depth=0
  val mutable fullscreen=false

(** init then video screen with width height depth fullscreen *)
  method init w h bpp fs=
    width<-w;
    height<-h;
    depth<-bpp;
    fullscreen<-fs;    
    screen#init width height depth fullscreen

(** get screen width *)
  method get_w=width
(** get screen height *)
  method get_h=height
(** get if fullscreen *)
  method get_fs=fullscreen
(** get depth *)
  method get_d=depth


(** set title of screen *)
  method set_caption s i=
    screen#set_caption s i
(*    wm_set_caption s *)

(** refresh screen *)
  method flip=
    screen#refresh
(*    video_update    *)

(** blank the screen *)
  method blank=
    screen#blank

(** get video screen tile *)
  method get_drawing=
    screen
6~
(** set video screen clip *)
  method set_clip x y w h=
    screen#set_clip x y w h 

  method show_cursor()=
    screen#show_cursor();

  method hide_cursor()=
    screen#hide_cursor();

end;;

(** We declare a global video class since we have only one video screen ! *)
(*let video=new video;;*)
