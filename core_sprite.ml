open Ocommon;;
open Olua;;
open Oval;;

open Core_val;;
open Core_rect;;
open Core_medias;;
open Core_graphic;;
open Core_action;;

(** graphic container *)
class graphics_container=
object(self)
  inherit [graphic_object] generic_object_handler
  inherit lua_object as lo

  method add_graphic n gr=
(*    print_string ("GRAPHICS_CONTAINER : add graphic "^n);print_newline(); *)
    self#add_object (Some n) gr;
    gr#lua_init();
    self#lua_parent_of n (gr:>lua_object)

  method graphics_update()=
    self#foreach_object (
      fun k v->
	ignore(v#get_lua#exec_val_fun (OLuaVal.String "on_update") [OLuaVal.Nil];)
    );
  
  method graphics_register reg=
    self#foreach_object (
      fun k o->
	reg (o:>canvas_object)
    );

  method graphics_unregister unreg=
    self#foreach_object (
      fun k o->
	unreg (o:>canvas_object)
    );
end;;


class sprite_object=
object(self)
  inherit generic_object
  inherit lua_object as lo


  (** type *)
  val mutable name=""
  method get_name=name
  method set_name n=name<-n



(** pixel position *)
  val mutable prect=new rectangle 0 0 0 0
  method get_prect=prect

  method jump x y=
    prect#set_position x y


(** states *)
  val mutable states=new state_actions
  method get_states=states

  method act()=
    states#act();


(** properties *)
  val mutable props=new val_ext_handler
  method get_props=props
  method set_props p=props<-p

(** graphics *)
  val mutable graphics=new graphics_container
  method get_graphics=graphics

  method graphics_register (reg:canvas_object->unit)=
    graphics#graphics_register reg;
  method graphics_unregister (unreg:canvas_object->unit)=
    graphics#graphics_unregister unreg;
  method graphics_update ()=
    graphics#graphics_update()


  method lua_init()=
    lua#set_val (OLuaVal.String "get_prect_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->prect#get_x));
    lua#set_val (OLuaVal.String "get_prect_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->prect#get_y));
    
    lua#set_val (OLuaVal.String "set_prect_position") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (prect#set_position));

    lua#set_val (OLuaVal.String "jump") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (self#jump));
    
    lua#set_val (OLuaVal.String "get_id") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_id));

      lua#set_val (OLuaVal.String "get_type") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_name));


    lua#set_val (OLuaVal.String "properties") (OLuaVal.Table props#to_lua#to_table);

    lua#set_val (OLuaVal.String "graphics_update") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (self#graphics_update));

    graphics#lua_init();
    self#lua_parent_of "graphics" (graphics:>lua_object);
    states#lua_init();
    self#lua_parent_of "states" (states:>lua_object);

    lo#lua_init()

end
