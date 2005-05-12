open Value_lua;;
open Value_val;;

open Core_stage;;
open Core_val;;
open Core_sprite;;

open Net_message;;
open Net_conn;;
open Net_server;;

(** types de message :
    - add_sprite : ajoute un sprite
    - delete_sprite : supprime un sprite
    - set_sprite_state : change l'etat d'un sprite
    - sync_sprites : synchronize les sprites
 *)


class set_state_message_handler set_state=
object(self)
  inherit message_handler
  method parse msg=
    let spr_id=(string_of_val (msg#get_values#get_val (`String "sprite"))) in
    let st_id=
      if(msg#get_values#is_val (`String "state")) then
	(Some (string_of_val (msg#get_values#get_val (`String "state"))))
      else None in
    let st_v=new val_ext_handler in
      (match st_id with
	| Some v->
	    st_v#from_xml msg#get_data;
	| None ->());
	    set_state spr_id st_id st_v;
    let res=new xml_message in
      res#set_type "response";
      let vl=new val_generic_handler in
	vl#set_id "values";
	vl#set_val (`String "type") (`String msg#get_type);
	res#set_values vl;
	res
  method check msg=
    true
end;;


class add_sprite_message_handler add_sprite=
object(self)
  inherit message_handler
  method parse msg=
    let spr_id=(string_of_val (msg#get_values#get_val (`String "sprite"))) in
    let spr_type=(string_of_val (msg#get_values#get_val (`String "type"))) in
    
    let data=new val_ext_handler in
      data#from_xml msg#get_data;
      let (x,y)=(position_of_val (data#get_val (`String "position"))) in
      add_sprite spr_id spr_type x y;
    let res=new xml_message in
      res#set_type "response";
      let vl=new val_generic_handler in
	vl#set_id "values";
	vl#set_val (`String "type") (`String msg#get_type);
	res#set_values vl;
	res
  method check msg=
    true
end;;

class delete_sprite_message_handler delete_sprite=
object(self)
  inherit message_handler
  method parse msg=
    let spr_id=(string_of_val (msg#get_values#get_val (`String "sprite"))) in
      delete_sprite spr_id;
    let res=new xml_message in
      res#set_type "response";
      let vl=new val_generic_handler in
	vl#set_id "values";
	vl#set_val (`String "type") (`String msg#get_type);
	res#set_values vl;
	res
  method check msg=
    true
end;;

class net_sprite_engine curs=
object(self)
  inherit sprite_engine curs as super

  method net_set_sprite_state (conn:network_object) dst n st_id st_v=
    st_v#set_id "args";
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"set_state\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"sprite\" value=\""^n^"\"/>
                         <val_string name=\"state\" value=\""^st_id^"\"/>
                        </values>
                        <data>"^st_v#to_xml_string^"
                        </data>
                       </message>
                      ")
      );
    sprites#set_sprite_state n (Some st_id) st_v
  method net_set_sprite_no_state (conn:network_object) dst n=
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"set_state\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"sprite\" value=\""^n^"\"/>
                        </values>
                       </message>
                      ")
      );
    sprites#set_sprite_state n (None) (new val_ext_handler)

  method net_add_sprite_named_from_type (conn:network_object) dst n t x y=
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"add_sprite\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"sprite\" value=\""^n^"\"/>
                         <val_string name=\"type\" value=\""^t^"\"/>
                        </values>
<data>
<args>
 <val_position name=\"position\" x=\""^string_of_int x^"\" y=\""^string_of_int y^"\"/>
</args>
</data>
                       </message>
                      ")
      );
    if sprites#is_object n=false then (
      sprites#add_sprite_from_type (Some n) t x y;())

  method net_delete_sprite (conn:network_object) dst n=
    conn#message_send 
      (xml_message_of_string (
	 "<message type=\"delete_sprite\" dst=\""^dst^"\">
                        <values>
                         <val_string name=\"sprite\" value=\""^n^"\"/>
                        </values>
                       </message>
                      ")
      );
    sprites#delete_sprite n

  method net_lua_init conn=
    lua#set_val (OLuaVal.String "net_set_sprite_state") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun dst id n v->
	    let lo=new lua_obj in
	      lo#from_table v;
	    self#net_set_sprite_state conn dst id (n) (val_ext_handler_of_format (ValLua lo))
	 )
      );
    lua#set_val (OLuaVal.String "net_set_sprite_no_state") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#net_set_sprite_no_state conn));
    lua#set_val (OLuaVal.String "net_add_sprite") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (self#net_add_sprite_named_from_type conn));
    lua#set_val (OLuaVal.String "net_delete_sprite") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#net_delete_sprite conn));


end;;

class net_client_sprite_engine curs saddr sport cport=
object(self)
  inherit net_sprite_engine curs as super
  val mutable cli=new network_client cport

  method on_load()=
    cli#get_mph#handler_add "set_state" (new set_state_message_handler self#get_sprites#set_sprite_state);
    cli#get_mph#handler_add "add_sprite" (new add_sprite_message_handler (fun n t x y->ignore(self#get_sprites#add_sprite_from_type (Some n) t x y)));
    cli#get_mph#handler_add "delete_sprite" (new delete_sprite_message_handler self#get_sprites#delete_sprite);
    cli#connect saddr sport;
    super#on_load()

  method lua_init()=
    lua#set_val (OLuaVal.String "get_ident") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->cli#get_ident));
    self#net_lua_init (cli:>network_object);

    super#lua_init()  
end;;


class net_server_sprite_engine sport=
object(self)
  inherit net_sprite_engine generic_cursor as super
  val mutable serv=new network_server sport

(*
  initializer
    sprites#set_canvas None;
*)
  method on_load()=
    serv#get_mph#handler_add "set_state" (new set_state_message_handler self#get_sprites#set_sprite_state);
    serv#get_mph#handler_add "add_sprite" (new add_sprite_message_handler (fun n t x y->ignore(self#get_sprites#add_sprite_from_type (Some n) t x y)));
    serv#get_mph#handler_add "delete_sprite" (new delete_sprite_message_handler self#get_sprites#delete_sprite);
    super#on_load();
(*    serv#set_update self#on_loop; *)
    Thread.create(function()->serv#run()) ();()


  val mutable on_connect_fun=fun v->[OLuaVal.Nil]
  method on_connect c=
    ignore(on_connect_fun [OLuaVal.String c])

  val mutable on_disconnect_fun=fun v->[OLuaVal.Nil]
  method on_disconnect c=
    ignore(on_disconnect_fun [OLuaVal.String c])

  method lua_init()=
    lua#set_val (OLuaVal.String "on_connect") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun cli->()));
    lua#set_val (OLuaVal.String "on_disconnect") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun cli->()));
    self#net_lua_init (serv:>network_object);
    super#lua_init();
    on_connect_fun<-lua#get_fun (OLuaVal.String "on_connect");
    on_disconnect_fun<-lua#get_fun (OLuaVal.String "on_disconnect");
    serv#set_connect self#on_connect;
    serv#set_disconnect self#on_disconnect;
 
end;;

