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


(** Functional capacity *)

(** {2 Funtional classes} *)

(** functional graphic class *)
class virtual graphic_fun=
object
  method virtual move: int->int->unit
  method virtual get_x: unit->int
  method virtual get_y: unit->int
  method virtual get_w: unit->int
  method virtual get_h: unit->int
  method virtual set_cur_drawing: int->unit
  method virtual cur_drawing: unit->int
  method virtual drawings_size: unit->int
  method virtual show: unit->unit
  method virtual hide: unit->unit
  method virtual set_layer: int->unit
end;;



(** functional sprite class *)
class virtual sprite_fun=
object
  method virtual get_x : unit->int
  method virtual get_y : unit->int
  method virtual jump : int -> int -> unit
end;;

(* FIXME : must be in pocgame ! *)
(** functional game_object class *)
class virtual game_object_fun=
object
  inherit sprite_fun
  method virtual pixel_x : unit->int
  method virtual pixel_y : unit->int
  method virtual case_x : unit->int
  method virtual case_y : unit->int
  method virtual case_w : unit->int
  method virtual case_h : unit->int
  method virtual move : int -> int -> unit
  method virtual direction : unit -> int
  method virtual turn : int -> unit
end;;

(** functional game object map class *)
class virtual game_object_map_fun=
object
  method virtual add_object_from_type : string option -> string -> int -> int -> string
  method virtual get_object_id_at_position : int -> int -> string option
  method virtual delete_object : string -> unit
end;;

(** functional game map class *)
class virtual game_map_fun=
object
  method virtual path_calc : (int*int) -> (int*int) -> (int*int) array
  method virtual resize : int -> int -> unit
  method virtual load_from_file : string -> unit
  method virtual save_to_file : string -> unit
end;;

(** functional game visual class *)
class virtual game_visual_fun=
object
  method virtual set_position: int->int->unit
  method virtual scroll:int->int->unit
end;;

(** {2 Type} *)

type functionizer=
    [
      `GraphicFun of graphic_fun
    | `SpriteFun of sprite_fun	
    | `GameObjectFun of game_object_fun
    | `GameObjectMapFun of game_object_map_fun
    | `GameMapFun of game_map_fun
    | `GameVisualFun of game_visual_fun

    | `NoFun
    ];;


(** {2 Convertion} *)

exception Bad_fun_type of string;;

let graphic_of_fun=function
  | `GraphicFun gr-> gr
  | _->raise (Bad_fun_type "graphic_fun");;

let sprite_of_fun=function
  | `SpriteFun spr-> spr
  | `GameObjectFun spr-> (spr:>sprite_fun)
  | _->raise (Bad_fun_type "sprite_fun");;

let game_object_of_fun=function
  | `GameObjectFun spr-> spr
  | _->raise (Bad_fun_type "game_object_fun");;

let game_object_map_of_fun=function
  | `GameObjectMapFun gom-> gom
  | _->raise (Bad_fun_type "game_object_map_fun");;

let game_map_of_fun=function
  | `GameMapFun gm-> gm
  | _->raise (Bad_fun_type "game_map_fun");;

let game_visual_of_fun=function
  | `GameVisualFun vis-> vis
  | _->raise (Bad_fun_type "game_visual_fun");;
  


(** {2 Functional tree} *)

open Value_common;;

exception Fun_parent_not_set
exception Fun_children_not_set

class ['a] fun_node=
object(self)
  inherit generic_object as go

  
  val mutable parent=(None : ('a) fun_node option) 
  method set_parent p=parent<-(Some p)
  method get_parent=
    match parent with
      | Some p->p
      | None -> raise (Fun_parent_not_set)

  val mutable node=`NoFun
  method set_fun (n:'a)=node<-n
  method get_fun=node

  val mutable children=(None: (('a) fun_node) generic_object_handler option)
  method get_children=
    match children with
      | Some c->c
      | None -> raise (Fun_children_not_set)
  method set_children c=children<-(Some c)

end;;


class _core_fun_node=
object(self)
  inherit [functionizer] fun_node
end;;

class core_fun_node_handler=
object
  inherit [_core_fun_node] generic_object_handler 
end;;

(** Functional Node *)
class core_fun_node=
object(self)
  inherit [functionizer] fun_node
  initializer
    self#set_children (new core_fun_node_handler);
end;;


(** UNUSED *)
class core_fun_object=
object
  val mutable fnode=new core_fun_node
  method get_fnode=fnode
  
  method fun_init()=
    fnode#set_children (new core_fun_node_handler);

end;;



