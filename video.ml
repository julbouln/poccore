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
open Config;;

let screen_tile=ref (tile_empty());;

(** First loaded module *)

let timeget_init t= 
  t:=time_get();
and
  timeget_result s t= 
  let t2=time_get() in
    print_string (s^string_of_int(t2-(!t)));print_newline();;
let tile_w=ref 32;;
let tile_h=ref 32;;

let flip=video_update;;

let scr_w=ref 800;;
let scr_h=ref 600;;
let fullscreen=ref false;;
let windowed=ref false;;
let fps=ref 30;;


let set_scr_w w=scr_w:=w;;
let set_scr_h h=scr_h:=h;;
let set_fps f=fps:=f;;

let get_fact_w()=(float_of_int !scr_w)/.800.0;;
let get_fact_h()=(float_of_int !scr_h)/.600.0;;

let f_size_w w=let f=(float_of_int !scr_w)/.800.0 in int_of_float(f*.(float_of_int w));;
let f_size_h h=let f=(float_of_int !scr_h)/.600.0 in int_of_float(f*.(float_of_int h));;
