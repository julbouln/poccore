
(*

*)
open Generic;;
open Olua;;
open Oval;;
open Oxml;;

open Anim;;
open Rect;;



(** Action and state manager *)


class virtual action_object_NEW=
object(self)
  inherit generic_object
  inherit lua_object as lo


  method virtual on_start : val_ext_handler -> unit
  method virtual on_loop : unit -> unit
  method virtual on_stop : unit -> unit

end;;

class action_fun=
object
  inherit action_object_NEW

  val mutable f_on_start=(fun ve->())
  val mutable f_on_loop=(fun()->())
  val mutable f_on_stop=(fun()->())

  method set_on_start f=f_on_start<-f
  method set_on_loop f=f_on_loop<-f
  method set_on_stop f=f_on_stop<-f

  method on_start ve=f_on_start ve
  method on_loop()=f_on_loop()
  method on_stop()=f_on_stop()
end;;

(*
<state_actions>
 <state_object id="idle">
  <action_object id="anim" type="action_anim"/>
  <args>
   <val_int name="refresh" value="2"/>
   <val_list name="frames">
    <val_int value="1"/>
   </val_list>
  </args>
  </action_object>
 </state_object>
 <state_object id="move">
  <action_object id="anim" type="action_anim">
   <!-- ... -->
  </action_object>
  <action_object id="mouvement" type="action_lua">
   <script>
    function self.on_start(ev)
     obj=self.parent;
     if (ev.direction="bottom") then obj.turn 0;
     // ...
    end
    function self.on_loop()
     obj=self.parent;
     speed=obj.properties.speed;
     px=obj.prect.get_x;
     py=obj.prect.get_y;
     if (obj.direction=0) then obj.prect.set_position (px) (py-speed);
     // ...
    end
   </script>
  </action_object>
 </state_object>
</state_actions>


<action_object type="action_anim">
 <args>
  <val_int name="refresh" value="2"/>
  <val_list name="frames">
    <val_int value="1"/>
  </val_list>
 </args>
</action_object>

*)

class action_lua=
object(self)
  inherit action_object_NEW as super

  method on_start ev=
    ignore(lua#exec_val_fun (OLuaVal.String "on_start") [OLuaVal.Table ev#to_lua#to_table])
  method on_loop()=
    ignore(lua#exec_val_fun (OLuaVal.String "on_loop") [OLuaVal.Nil])
  method on_stop()=
    ignore(lua#exec_val_fun (OLuaVal.String "on_stop") [OLuaVal.Nil])

  method lua_init()=
    lua#set_val (OLuaVal.String "on_start") (OLuaVal.efunc (OLuaVal.table **->> OLuaVal.unit) (fun ev->()));
    lua#set_val (OLuaVal.String "on_loop") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (fun()->()));
    lua#set_val (OLuaVal.String "on_stop") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (fun()->()));

    super#lua_init();

end;;


class action_anim frs r=
object(self)
  inherit action_lua as al
  inherit anim_object frs r
  method on_loop()=self#anim();al#on_loop();

  method lua_init()=
    lua#set_val (OLuaVal.String "get_frame") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_frame));

    al#lua_init();

end;;


class state_object_NEW=
object(self)
  inherit [action_object_NEW] generic_object_handler
  inherit generic_object
  inherit lua_object as lo

  method add_action n act=
    print_string ("STATE_OBJECT : add action "^n);print_newline();
    ignore(self#add_object (Some n) act);
    self#lua_parent_of n (act:>lua_object)

  method start (ve:val_ext_handler)=
    self#foreach_object (fun k o-> 
			   let nlv=ve#get_val (`String k) in
			   let nl=list_of_val nlv in
			   let nve=val_ext_handler_of_list nl in
			     o#on_start nve
			);

  method loop()=
    self#foreach_object (fun k o-> o#on_loop());

  method stop()=
    self#foreach_object (fun k o-> o#on_stop());

end;;

class state_actions=
object(self)
  inherit [state_object_NEW] generic_object_handler
  inherit lua_object as lo

  val mutable current=None

  method add_state n st=
    print_string ("STATE_ACTIONS : add state "^n);print_newline();
    ignore(self#add_object (Some n) st);
    self#lua_parent_of n (st:>lua_object)

  method set_state (sn:string option) (ve:val_ext_handler)=
    (match current with
      | Some n->let o=self#get_object n in o#stop()
      | None -> ());
    current<-sn;
    (match current with
      | Some n->let o=self#get_object n in o#start ve
      | None -> ());    

  method act()=
    match current with
      | Some n->let o=self#get_object n in o#loop()
      | None -> ()
    
end;;


(* xml part *)

class xml_action_object_parser=
object(self)
  inherit [action_lua] xml_object_parser (fun()->new action_lua)

  val mutable mt=("",new val_ext_handler,"")
  method set_metatype (m:string*val_ext_handler*string)=mt<-m

  method init_object o=
    let (nm,vh,l)=mt in
    o#set_lua_script (l^lua);

  method get_val_from_meta (m:string*val_ext_handler*string)=
    let ofun()=
      let (nm,vh,l)=m in
      let o=
	new action_lua in
	o#set_lua_script (l);
	o in	
      (ofun)

end;;

class xml_action_anim_parser=
object(self)
  inherit xml_action_object_parser


  method get_val=
    let ofun()=
    let (nm,vh,l)=mt in      
      let o=
	let args=args_parser#get_val in
	  args#merge vh;
	  new action_anim 
	    (Array.map (
	       fun v->
		 int_of_val v
	     )
	       (Array.of_list 
		  (list_of_val (args#get_val (`String "frames")))
	       )
	    )
	      (int_of_val (args#get_val (`String "refresh")))
      in
	self#init_object (o:>action_lua);
	(o:>action_lua)	  
    in      
      (id,ofun)


  method get_val_from_meta (m:string*val_ext_handler*string)=
    let ofun()=
      let (nm,vh,l)=m in
      let o=
	new action_anim 
	  (Array.map (
	     fun v->
	       int_of_val v
	   )
	     (Array.of_list 
		(list_of_val (vh#get_val (`String "frames")))
	     )
	  )
	  (int_of_val (vh#get_val (`String "refresh"))) in
	  o#set_lua_script (l);
	(o:>action_lua) in	
      (ofun)



end;;


class xml_actions_parser=
object(self)
  inherit [xml_action_object_parser,action_lua] xml_parser_container "action_object" (fun()->new xml_action_object_parser)

  method set_metatype (m:string*(string,string*val_ext_handler*string)Hashtbl.t*string)=mt<-m

  val mutable id=""
  method get_id=id

  method parse_attr k v=
    match k with
      | "id" ->id<-v
      | _ -> ()

  method get_type="unique"

  method get_val=
    let ofun()=
      let o=new state_object_NEW in
	self#init o#add_action;
	o in
      (id,ofun)

  method get_val_from_meta (m:string*(string,string*val_ext_handler*string)Hashtbl.t*string)=
    let ofun()=
      let (nm,h,l)=m in
      let o=new state_object_NEW in 
	self#init_from_meta_h o#add_action h;
(*	o#set_lua_script l; *)
	o in
      (ofun)

end;;


let xml_factory_actions_parser()=
  let p=new xml_actions_parser in
    p#parser_add "action_lua" (fun()->new xml_action_object_parser);
    p#parser_add "action_anim" (fun()->new xml_action_anim_parser);
    p;;

(** global default actions parser, can be overided *)
let xml_default_actions_parser=
  let gl=Global.empty("xml_default_actions_parser") in
    Global.set gl xml_factory_actions_parser;
    gl;;
  

class xml_state_actions_parser=
object(self)
  inherit [xml_actions_parser,state_object_NEW] xml_parser_container "state_object" (fun()->(Global.get xml_default_actions_parser)())


  method set_metatype (m:string*(string,string*(string,string*val_ext_handler*string)Hashtbl.t*string)Hashtbl.t*string)=mt<-m

  initializer
    self#parser_add "unique" (fun()->(Global.get xml_default_actions_parser)())


  method get_val=
    let ofun()=
      let o=new state_actions in
	self#init o#add_state;
	o in
      (ofun)

end;;


(** metatype *)

class xml_action_object_mt_parser=
object
  inherit xml_metatype_parser
end;;

class xml_state_object_mt_parser=
object(self)
  inherit [(string*val_ext_handler*string)] xml_stringhash_parser "action_object" (fun()->new xml_action_object_mt_parser)

  val mutable id=""

  method parse_attr k v=
    match k with
      | "id" ->id<-v
      | _ -> ()

  method get_val=
    (id,("unique",(self#get_hash),""))

end;;

class xml_states_mt_parser=
object
  inherit [string*(string,string*val_ext_handler*string)Hashtbl.t*string] xml_stringhash_parser "state_object" (fun()->new xml_state_object_mt_parser)

end;;




(* DEPRECATED *)

(** Main action class *)
class action_object=
  object (self)
    val mutable acting=false

    (* functions *)			 
    val mutable action=function()->()
    
    val mutable on_start=function()->()
    val mutable on_stop=function()->()
 
    val mutable on_change_event=function()->true
    val mutable on_stop_event=function()->true

    method set_action a=action<-a 

    method set_on_start a=on_start<-a 
    method set_on_stop a=on_stop<-a 

    method set_change_event a=on_change_event<-a
    method set_stop_event a=on_change_event<-a

    method start()=acting<-true
    method stop()=acting<-false
    method act()=if acting==true then action()

    method act_on_start()=on_start()
    method act_on_stop()=on_stop()

    method stop_event=on_stop_event()
    method change_event=on_change_event()			  
  
    method get_acting=acting
    method set_acting a=acting<-a
  end;;


(** State class - combine action and anim *)
class state_object n f r =
  object
    inherit anim_object f r
    inherit action_object

    val mutable name=n
    method get_name=name
    method set_name nm=name<-nm
    method print_name=print_string name

    val mutable priority=0
    method set_priority p=priority<-p
    method get_priority=priority
  end;;


exception State_not_found of string


(** State manager to handle multiple state *)
class state_object_manager=
object(self)
  val mutable states=
    let a=Hashtbl.create 2 in
      Hashtbl.add a "none" (new state_object "none" [|0|] 12 );
      a
 
  val mutable stack_states=Stack.create()
  method next_state()=
    if Stack.length stack_states<> 0 then
    (
      let (ns,np)=Stack.pop stack_states in
	self#set_cur_state ns;
	self#current_state#set_priority np
    )


  val mutable cur_state="none"
  method set_cur_state s=cur_state<-s
  method get_cur_state=cur_state 

  method add_state (s:state_object)=
    Hashtbl.add states s#get_name s
  method get_state n=
    if Hashtbl.mem states n then
      Hashtbl.find states n
    else raise (State_not_found n)

  method set_state s p=self#set_instant_state s p

  method set_instant_state s p=
    self#set_cur_state s;
    self#current_state#set_priority p

  method set_decaled_state s p=
    if p>= self#current_state#get_priority then (
(*      print_string ("STATE: next is "^s);print_newline();*)
      Stack.push (s,p) stack_states; 
(*      self#set_cur_state s;
      self#current_state#start();
*)						   
    )
  method current_state=self#get_state cur_state

  method act()=
    self#current_state#act();
    if self#current_state#stop_event then (
(*      self#next_state(); *)
(*      print_string ("STATE: switch to "^cur_state);print_newline(); *)
    )
end;;

