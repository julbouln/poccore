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
open Core_cache;;

(** Font management *)

type font_t=
  | FontTTF of (string*int)
  | FontEmbed;;

exception Font_not_initialized;;

class virtual ['t] font_object=
object
  inherit generic_object

  val mutable f=None
  method get_f=
    match f with
      | Some v->v
      | None -> raise Font_not_initialized;
  method set_f (nt:'t)=f<-(Some nt)


  method virtual get_f: 't
  method virtual set_f: 't->unit  

  method virtual load:font_t->unit
  method virtual get_size:int
  method virtual get_height:int
  method virtual sizeof_text:string->(int*int)
  method virtual get_font_type:font_t
(*  method virtual create_text:string->color *)

end;;


