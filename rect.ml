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
(* FIXME : must me included in bfrlib *)

(* NO USE *)
class point (xi:int) (yi:int)=
object
  val mutable x=xi
  val mutable y=yi
  
  method get_x=x
  method get_y=y
 
  method set_x nx=x<-nx
  method set_y ny=y<-ny

  method get=(x,y)
  method set (nx,ny)=x<-nx;y<-ny

end;;

(** Sample rectangle structure *)

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
