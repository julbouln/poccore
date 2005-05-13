open Value_common;;
open Value_lua;;
open Value_val;;
open Value_xml;;

open Core_val;;
open Core_rect;;
open Core_medias;;
open Core_graphic;;
open Core_action;;
open Core_type;;
open Core_event;;
open Core_video;;

open Core_fun;;

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

  method delete_graphic n=
    lo#get_lua#del_val (OLuaVal.String n);
    self#delete_object n


  method get_graphic n=
    self#get_object n

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

  method add_graphic n gr=
    graphics#add_graphic n gr;
  method get_graphic n=graphics#get_graphic n
  method delete_graphic n=
    graphics#delete_graphic n

  method graphics_register (reg:canvas_object->unit)=
    graphics#graphics_register reg;
  method graphics_unregister (unreg:canvas_object->unit)=
    graphics#graphics_unregister unreg;
  method graphics_update ()=
    graphics#graphics_update()


  method functionize : functionizer=
    `SpriteFun {
      get_px=(fun()->prect#get_x);
      get_py=(fun()->prect#get_y);
      jump=self#jump;

    }

  method lua_init()=
(* DEPRECATED *)
    lua#set_val (OLuaVal.String "get_prect_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->prect#get_x));
    lua#set_val (OLuaVal.String "get_prect_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->prect#get_y));
    lua#set_val (OLuaVal.String "set_prect_position") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (prect#set_position));
(* /DEPRECATED *)

    lua#set_val (OLuaVal.String "get_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->prect#get_x));
    lua#set_val (OLuaVal.String "get_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->prect#get_y));

    lua#set_val (OLuaVal.String "jump") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (self#jump));

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

(** sprite methods *)
  method set_sprite_state id st_id st_v=
    let s=self#get_object id in
      s#get_states#set_state st_id st_v


(** <xml> *)
  method to_xml()=
    let xml=new xml_node in
      xml#set_tag "sprites";
      
      self#foreach_object (fun spr_id spr->
			     let sxml=new xml_node in
			       sxml#of_list [
				 Tag "sprite";
				 Attribute ("id",spr_id);
				 Attribute ("type",spr#get_name);
			       ];
			       let vh=new val_ext_handler in
				 vh#set_id "args";
				 vh#set_val (`String "position") (`Position (spr#get_prect#get_x,spr#get_prect#get_y));

				 sxml#add_child vh#to_xml;
				 
				 xml#add_child sxml;
			  );
      xml

  method from_xml (xml:xml_node)=
    List.iter (
      fun c->
	let args=new val_ext_handler and
	    props=new val_ext_handler in

	  List.iter (
	    fun cc->
	      match (cc#tag) with
		| "args" -> 
		    args#from_xml cc
		| "properties" -> 
		    props#from_xml cc
		| _ ->()
	  ) c#children;
	  let (x,y)=position_of_val (args#get_val (`String "position")) in
	  let oid=(c#attrib "id") in
	    if self#is_object oid then (
	      let o=self#get_object oid in
		o#get_prect#set_position x y;
	    )
	    else 
	      (
		let nid=self#add_sprite_from_type (Some (c#attrib "id")) (c#attrib "type") x y in
		let o=self#get_object oid in
		  o#get_props#flatten props;
	      )
	      
    ) xml#children;

(** </xml> *)

  method update()=
    self#foreach_object (fun k o->
			   o#act();
			)

  method lua_init()=
    lua#set_val (OLuaVal.String "set_sprite_state") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun id n v->
	    let lo=new lua_obj in
	      lo#from_table v;
	    self#set_sprite_state id (Some n) (val_ext_handler_of_format (ValLua lo))
	 )
      );

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

  val mutable sprites=new sprite_vault
  method get_sprites=sprites

  method get_graphic id gid=
    let s=sprites#get_object id in
      (Some (s#get_graphic gid))

  method add_graphic id gid go=
    let s=sprites#get_object id in
      canvas#add_obj (go:>canvas_object);
      s#add_graphic gid go

  method delete_graphic id gid=
    let s=sprites#get_object id in
    let gr=s#get_graphic gid in
      canvas#del_obj (gr:>canvas_object);
      s#delete_graphic gid


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
