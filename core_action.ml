
open Value_common;;
open Value_lua;;
open Value_val;;


open Core_val;;
open Core_anim;;
open Core_rect;;

open Core_timer;;

(** Action and state manager *)

(** action object - repeat some function *)
class virtual action_object=
object(self)
  inherit generic_object
  inherit lua_object as lo

  method virtual on_start : val_ext_handler -> unit
  method virtual on_loop : unit -> unit
  method virtual on_stop : unit -> unit

end;;

(** action from func val *)
class action_fun=
object
  inherit action_object

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

(** action from lua func definition *)
class action_lua2=
object(self)
  inherit action_object as super


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

(* keep lua fun instead of find *)
(** action from lua func definition *)
class action_lua=
object(self)
  inherit action_object as super

  val mutable start_fun=fun l->[OLuaVal.Nil]
  val mutable loop_fun=fun l->[OLuaVal.Nil]
  val mutable stop_fun=fun l->[OLuaVal.Nil]

  method on_start ev=
    ignore(start_fun [OLuaVal.Table ev#to_lua#to_table])
  method on_loop()=
    ignore(loop_fun [OLuaVal.Nil])
  method on_stop()=
    ignore(stop_fun [OLuaVal.Nil])

  method lua_init()=
    lua#set_val (OLuaVal.String "on_start") (OLuaVal.efunc (OLuaVal.table **->> OLuaVal.unit) (fun ev->()));
    lua#set_val (OLuaVal.String "on_loop") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (fun()->()));
    lua#set_val (OLuaVal.String "on_stop") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (fun()->()));

    super#lua_init();
    start_fun<-lua#get_fun (OLuaVal.String "on_start");
    loop_fun<-lua#get_fun (OLuaVal.String "on_loop");
    stop_fun<-lua#get_fun (OLuaVal.String "on_stop");

end;;




(** action with anim capabilities and lua func definition *)
class action_anim frs r=
object(self)
  inherit action_lua as al
  inherit anim_object frs r

  method on_loop()=self#anim();al#on_loop();

  method on_stop()=
   self#set_current 0;
    al#on_stop();

  method lua_init()=
    lua#set_val (OLuaVal.String "get_frame") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_frame));

    al#lua_init();


end;;


(** action with anim capabilities and lua func definition *)
class action_movement=
object(self)
  inherit action_lua as al

  val mutable dir=0
  val mutable d=0
  val mutable s=0

  val mutable cx=0
  val mutable cy=0

  method on_start ve=
    dir<-int_of_val (ve#get_val (`Int 0));
(*    d<-int_of_val (ve#get_val (`Int 1)); *)
    s<-int_of_val (ve#get_val (`Int 1));
    let get_x=self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "get_prect_x") and
    get_y=self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "get_prect_y") in
      cx<-int_of_val(val_of_lua(List.nth (get_x [OLuaVal.Nil]) 0));
      cy<-int_of_val(val_of_lua(List.nth (get_y [OLuaVal.Nil]) 0));
      al#on_start ve;


  method on_loop()=

    (match dir with 
      | 0 ->cy<-cy-s;
      | 1 ->cx<-cx+s;cy<-cy-s;
      | 2 ->cx<-cx+s;
      | 3 ->cx<-cx+s;cy<-cy+s;
      | 4 ->cy<-cy+s;
      | 5 ->cx<-cx-s;cy<-cy+s;
      | 6 ->cx<-cx-s;
      | 7 ->cx<-cx-s;cy<-cy-s;
      | _ ->());
    let move=self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "jump") in
      move[OLuaVal.Number (float_of_int cx);OLuaVal.Number (float_of_int cy)];
	al#on_loop();


  method lua_init()=
    al#lua_init();


end;;


(** action repeat over specified time *)
class action_timed max_time=
object
  inherit action_lua as al
  val mutable time=new timer
  initializer
    time#set_limit max_time

  method on_start ve=
    al#on_start ve;
(*    let t={h=0;m=1;s=0;f=0} in *)
(*    let t=time_of_val (ve#get_val (`String "interval")) in *)
    let t=time_of_val (ve#get_val (`Int 0)) in
      if time#is_task t=false then
	time#add_task t (fun()->al#on_loop());
      time#start();

  method on_loop()=
    time#step()

  method on_stop()=
    al#on_stop();
    time#reset();
    time#stop();

  method lua_init()=
    al#lua_init();
end;;

(** action happen after specified time *)
class action_intime max_time=
object
  inherit action_lua as al
  val mutable time=new timer
  initializer
    time#set_limit max_time

  method on_start ve=
    al#on_start ve;
(*    let t={h=0;m=1;s=0;f=0} in *)
(*    let t=time_of_val (ve#get_val (`String "interval")) in *)
    let t=time_of_val (ve#get_val (`Int 0)) in
      if time#is_timer t=false then
	time#add_timer_from_now t (fun()->al#on_loop());
      time#start();

  method on_loop()=
    time#step()

  method on_stop()=
    al#on_stop();
    time#reset();
    time#stop();

  method lua_init()=
    al#lua_init();
end;;


(** manage multiple actions in the same time *)
class state_object=
object(self)
  inherit [action_object] generic_object_handler
  inherit generic_object
  inherit lua_object as lo

  method add_action n act=
(*    print_string ("STATE_OBJECT : add action "^n);print_newline(); *)
    ignore(self#add_object (Some n) act);
    ignore(act#lua_init()); 
    self#lua_parent_of n (act:>lua_object)

  method start (ve:val_ext_handler)=
    self#foreach_object (fun k o-> 
			   if ve#is_val (`String k) then (
			     let nlv=ve#get_val (`String k) in
			     let nl=list_of_val nlv in
			     let nve=val_ext_handler_of_list nl in
			       o#on_start nve
			   ) else
			     o#on_start (new val_ext_handler)
			);

  method loop()=
    self#foreach_object (fun k o-> o#on_loop());

  method stop()=
    self#foreach_object (fun k o-> o#on_stop());


(** check before set *)
  method check (ve:val_ext_handler)=
    let v=(ve#to_lua#to_table) in
    match (List.nth (lua#exec_val_fun (OLuaVal.String "on_check") [OLuaVal.Table v]) 0) with
      | OLuaVal.Nil->false
      | _ -> true

  method lua_init()=
    lua#set_val (OLuaVal.String "on_check") 
      (OLuaVal.efunc (OLuaVal.table **->> OLuaVal.bool) 
	 (fun v->true
	 )
      );
    lua#set_val (OLuaVal.String "start") 
      (OLuaVal.efunc (OLuaVal.table **->> OLuaVal.unit) 
	 (fun v->
	    let lo=new lua_obj in
	      lo#from_table v;
	      self#start (val_ext_handler_of_format (ValLua lo))
	 )
      );

    lo#lua_init()

end;;

(** manage multiple state one at time *)
class state_actions=
object(self)
  inherit [state_object] generic_object_handler
  inherit lua_object as lo
  method get_id="state_actions"
  val mutable current=None

  method get_state=current

  method add_state n st=
(*    print_string ("STATE_ACTIONS : add state "^n);print_newline(); *)
    ignore(self#add_object (Some n) st);
    ignore(st#lua_init()); 
    self#lua_parent_of n (st:>lua_object)

  method request_state sn ve ave=
    match sn with
      | Some n->
	  let o=self#get_object n in
	    if (o#check ave) then
	      self#set_state sn ve
      | None ->()


  method set_state (sn:string option) (ve:val_ext_handler)=

    (match current with
      | Some n->let o=self#get_object n in o#stop()
      | None -> ());
    current<-sn;
    (match current with
      | Some n->
(*	  print_string ("STATE_ACTIONS : set state "^n);print_newline(); *)
	    let o=self#get_object n in o#start ve
      | None -> ());    

  method act()=
    match current with
      | Some n->let o=self#get_object n in o#loop()
      | None -> ()


  method lua_init()=
    lua#set_val (OLuaVal.String "get_state") 
      (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.value) 
	 (fun()->
	    match self#get_state with
	      | Some s->OLuaVal.String s
	      | None->OLuaVal.Nil
	 )
);
    lua#set_val (OLuaVal.String "set_state") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun n v->
	    let lo=new lua_obj in
	      lo#from_table v;
	    self#set_state (Some n) (val_ext_handler_of_format (ValLua lo))
	 )
      );

    lua#set_val (OLuaVal.String "request_state") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.table **->OLuaVal.table **->> OLuaVal.unit) 
	 (fun n v av->
	    let lo=new lua_obj and
		alo=new lua_obj in
	      lo#from_table v;
	      alo#from_table av;
	      self#request_state (Some n) (val_ext_handler_of_format (ValLua lo)) (val_ext_handler_of_format (ValLua alo))
	 )
      );

    lo#lua_init();    
    
end;;

