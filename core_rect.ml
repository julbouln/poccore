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


(** Rectangle class *)

(* from pocengine *)

(** rectangle class x y w h *)
class rectangle (x:int) (y:int) (w:int) (h:int)=
object(self)
    val mutable x=x
    val mutable y=y
    val mutable w=w
    val mutable h=h

    (** set position x y *)
    method set_position nx ny=x<-nx;y<-ny
    method get_x=x
    method get_y=y
    
    (** set size w h *)
    method set_size nw nh=w<-nw;h<-nh

    method get_w=w
    method get_h=h

(** is position in rectangle *)
    method is_position px py=
      if px>x && py>y && px<(x+w) && py<(y+h) then true else false

(** is rectangle in rectangle *)
    method is_in_rect (r:rectangle)=
      let rx=r#get_x and
	ry=r#get_y and
	rw=r#get_w and
	rh=r#get_h in
	if self#is_position rx ry 
	  || self#is_position (rx+rw) (ry+rh) 
	  || self#is_position (rx) (ry+rh) 
	  || self#is_position (rx+rw) (ry) 
	then true else false


    method dump_info()=
      print_string "[rect ";
      print_string " x=";print_int x;
      print_string " y=";print_int y;
      print_string " w=";print_int w;
      print_string " h=";print_int h;print_string "]";
  end;;



(* DEPRECATED *)
(** Sample rectangle structure *)
(*
type position = {x:int;y:int};;
type size = {w:int;h:int};;

class rectangle x y w h=
  object
    val mutable p={x=x;y=y}
    val mutable s={w=w;h=h}
    method set_position nx ny=p<-{x=nx;y=ny}
    method get_position=p
    method get_x=p.x
    method get_y=p.y
    method set_size nw nh=(s<-{w=nw;h=nh})
    method get_size=s    
    method get_w=s.w
    method get_h=s.h
  end;;
*)

