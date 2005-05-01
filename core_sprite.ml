open Value_common;;
open Value_lua;;
open Value_val;;

open Core_val;;
open Core_rect;;
open Core_medias;;
open Core_graphic;;
open Core_action;;
open Core_type;;
open Core_event;;
open Core_video;;

(** Sprites *)

(** graphic container *)
class graphics_container=
object(self)
  inherit [graphic_object] generic_object_handler
  inherit lua_object as lo
  method get_id="graphics"
  method add_graphic n gr=
(*    print_string ("GRAPHICS_CONTAINER : add graphic "^n);print_newline(); *)
    ignore(self#add_object (Some n) gr);
    ignore(gr#lua_init());
    self#lua_parent_of n (gr:>lua_object)

  method graphics_update()=
    self#foreach_object (
      fun k v->
	v#on_update();
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
    prect#set_position x y;
    self#graphics_update();


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
(*    
    lua#set_val (OLuaVal.String "get_id") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_id));
*)  
    lua#set_val (OLuaVal.String "get_type") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_name));
    
    
    lua#set_val (OLuaVal.String "properties") (OLuaVal.Table props#to_lua#to_table);
    
    lua#set_val (OLuaVal.String "graphics_update") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (self#graphics_update));
    
    ignore(graphics#lua_init()); 
    self#lua_parent_of "graphics" (graphics:>lua_object);

    ignore(states#lua_init());    
    self#lua_parent_of "states" (states:>lua_object);

    lo#lua_init()

end

class sprite_object_types=
object(self)
  inherit [sprite_object] obj_types
end;;

open Value_common;;

class sprite_vault=
object(self)
  inherit [sprite_object] generic_object_handler as super
  inherit lua_object as lo
  method get_id="sprites"

  val mutable obj_type=new sprite_object_types
  method get_obj_type=obj_type
  method add_object_type nm (t:unit->'a)=
    obj_type#add_object_type nm t
  method get_object_from_type nm=
    obj_type#get_object_type nm

  val mutable canvas=None
  method set_canvas (c:canvas option)=canvas<-c

  method add_sprite_to_canvas o=
    (match canvas with 
       | Some cvas->o#graphics_register cvas#add_obj;
       | None -> ());

  method del_sprite_from_canvas o=
    (match canvas with 
       | Some cvas->o#graphics_unregister cvas#del_obj;
       | None -> ());

  method add_sprite_at (id:string option) (o:sprite_object) (px:int) (py:int)=
    self#add_sprite_to_canvas o;
    let n=self#add_object id o in
      ignore(o#lua_init());
      self#lua_parent_of n (o:>lua_object);
      o#jump px py;
      n

  method add_sprite_from_type id t x y=
    let o=self#get_object_from_type t in
      self#add_sprite_at id o x y 

  method delete_sprite id=
    let o=self#get_object id in
      self#del_sprite_from_canvas o;
      lo#get_lua#del_val (OLuaVal.String id) ;
      super#delete_object id;

  method update()=
    self#foreach_object (fun k o->
			   o#act();
			)

  method lua_init()=
   lua#set_val (OLuaVal.String "add_sprite_from_type") 
     (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.string) 
	(fun t x y->self#add_sprite_from_type None t x y));
   lua#set_val (OLuaVal.String "add_sprite_named_from_type") 
     (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) 
	(fun n t x y->ignore(self#add_sprite_from_type (Some n) t x y)));
   lua#set_val (OLuaVal.String "delete_sprite") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#delete_sprite);

   lo#lua_init()

end;;

open Core_stage;;

class sprite_engine curs=
object(self)
  inherit stage curs as super

  val mutable interaction=new interaction_lua
  method set_interaction i=interaction<-i
(*
  val mutable canvas=new canvas
  method get_canvas=canvas
*)
  val mutable sprites=new sprite_vault
  method get_sprites=sprites

  method on_load()=
(*    canvas#clear(); *)
    sprites#clear();
    super#on_load();
    ignore(self#lua_init());

  initializer
    sprites#set_canvas (Some canvas);

  method on_loop()=
    super#on_loop();
    sprites#update();
    canvas#refresh 0 0 32 32; 

  method ev_parser e=
    super#ev_parser e;
    interaction#ev_parser e

  method lua_init()=
    ignore(curs#lua_init());
    self#lua_parent_of "cursor" (curs:>lua_object);

    ignore(sprites#lua_init());
    self#lua_parent_of "sprites" (sprites:>lua_object);

    ignore(interaction#lua_init());
    self#lua_parent_of "interaction" (interaction:>lua_object);

    super#lua_init();

end;;
