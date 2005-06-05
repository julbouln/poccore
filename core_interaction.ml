open Value_common;;
open Core_event;;
open Core_fun;;

(* FIXME : Go in core_interaction.ml *)
(** interaction *)

open Value_lua;;

class virtual interaction_object=
object(self)
  inherit generic_object

  method virtual on_keypress : (key_type*key_type)->unit
  method virtual on_keyrelease : (key_type*key_type)->unit
    
  method virtual on_mouseclick : int->int->int->unit
  method virtual on_mouserelease : int->int->int->unit

  method virtual on_mouseover : int->int->unit
  method virtual on_mouseout : int->int->unit

  method virtual on_loop : unit->unit


  method ev_parser e=
    (match e with
       | EventMouse em ->
	   (match em with
	      | MouseMotion(x,y) -> 
		  self#on_mouseover x y;
	      | MouseRelease(x,y,but) -> 
		  self#on_mouserelease x y but; 
	      | MouseClick(x,y,but) -> 
		  self#on_mouseclick x y but; 
	      | _ -> ()
	   )
       | EventKeyboard ek->
	 (match ek with
	    | KeyboardPress (k,uk)-> 
		self#on_keypress (k,uk)
	    | KeyboardRelease (k,uk)-> 
		self#on_keyrelease (k,uk)
	    | _ -> ()
	 )
       | _ -> ()
    )

end;;

class interaction_lua=
object(self)
  inherit interaction_object
  inherit lua_object as super

  val mutable fnode=new core_fun_node
  method get_fnode=fnode

  method on_keypress (e:(key_type*key_type))=
    ignore(lua#exec_val_fun (OLuaVal.String "on_keypress") [OLuaVal.String (string_of_key (fst e))])
  method on_keyrelease (e:(key_type*key_type))=
    ignore(lua#exec_val_fun (OLuaVal.String "on_keyrelease") [OLuaVal.String (string_of_key (fst e))])
    
  method on_mouseclick (x : int) (y : int) (but : int)=
ignore(lua#exec_val_fun (OLuaVal.String "on_mouseclick") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y);OLuaVal.Number (float_of_int but)])
  method on_mouserelease (x : int) (y : int) (but: int)=
ignore(lua#exec_val_fun (OLuaVal.String "on_mouserelease") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y);OLuaVal.Number (float_of_int but)])

  method on_mouseover (x : int) (y : int)=
    ignore(lua#exec_val_fun (OLuaVal.String "on_mouseover") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];)
  method on_mouseout (x : int) (y : int)=
    ignore(lua#exec_val_fun (OLuaVal.String "on_mouseout") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];)

  method on_loop ()=
    ignore(lua#exec_val_fun (OLuaVal.String "on_loop") [OLuaVal.Nil];)

  method lua_init()=
    lua#set_val (OLuaVal.String "on_mouseclick") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->OLuaVal.int **->> OLuaVal.unit) (fun x y b->()));
    lua#set_val (OLuaVal.String "on_mouserelease") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y b->()));

    lua#set_val (OLuaVal.String "on_mouseover") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));
    lua#set_val (OLuaVal.String "on_mouseout") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));

    lua#set_val (OLuaVal.String "on_keypress") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun s->()));
    lua#set_val (OLuaVal.String "on_keyrelease") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun s->()));
    lua#set_val (OLuaVal.String "on_loop") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (fun()->()));
    super#lua_init();


end;;

class interaction_objects=
object(self)
  inherit [interaction_lua] generic_object_handler
  inherit lua_object as lo

  val mutable fnode=new core_fun_node
  method get_fnode=fnode

  method get_id="interactions"

  method add_interaction id i=
    let ni=self#add_object (Some id) i in

    i#get_fnode#set_parent fnode;
    fnode#get_children#add_object (Some id) i#get_fnode;

    ignore(i#lua_init());
    self#lua_parent_of ni (i:>lua_object);

end;;
