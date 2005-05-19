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

class virtual sprite_fun=
object
  method virtual get_x : unit->int
  method virtual get_y : unit->int
  method virtual jump : int -> int -> unit
end;;

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

type functionizer=
    [
      `GraphicFun of graphic_fun
    | `SpriteFun of sprite_fun	
    | `GameObjectFun of game_object_fun
    | `NoFun
    ];;

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

class core_fun_node=
object(self)
  inherit [functionizer] fun_node
  initializer
    self#set_children (new core_fun_node_handler);
end;;

class core_fun_object=
object
  val mutable fnode=new core_fun_node
  
  method fun_init()=
    fnode#set_children (new core_fun_node_handler);

end;;



