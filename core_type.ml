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


open Value_lua;;

(** Type manager *)

exception No_obj_type of string;;

class ['a] obj_types=
object(self)
    inherit lua_object as lo
    val mutable object_types=Hashtbl.create 2
    val mutable objects=Hashtbl.create 2

    method get_id="obj_types"
    method add_object_type nm (obj:unit->'a)=
(*      print_string ("OBJ_TYPES : add object type "^nm);print_newline(); *)

      if(Hashtbl.mem objects nm)==false then (
	let o=obj() in
	  o#set_id nm;
	  ignore(o#lua_init()); 
	  self#lua_parent_of nm (o:>lua_object);
	  Hashtbl.add objects nm (o);	  
      );
      Hashtbl.add object_types nm obj

    method get_object_type nm=
      try
	(Hashtbl.find object_types nm)()
      with Not_found -> raise (No_obj_type nm)
    method get_object nm=      
      (Hashtbl.find objects nm)

    method is_object_type nm=(Hashtbl.mem objects nm)

    method count_objects_type=Hashtbl.length objects			       
    method foreach_object_type f= 
      Hashtbl.iter f object_types

    method foreach_object f= 
      Hashtbl.iter f objects

    method lua_init()=
   lua#set_val (OLuaVal.String "foreach_object") 
     (OLuaVal.efunc (OLuaVal.value **->> OLuaVal.unit) 
	(fun f->
	   let g k v=
	     match f with
	       | OLuaVal.Function (s,f)->
		   f [OLuaVal.String k;OLuaVal.Table v#get_lua#to_table];()
	       | _ -> () in
	     self#foreach_object g
	));
      lo#lua_init()
end;;
