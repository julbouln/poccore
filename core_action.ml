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


open Value_common;;
open Value_object;;
open Value_lua;;
open Value_val;;


open Core_val;;
open Core_anim;;
open Core_rect;;

open Core_timer;;

open Core_fun;;

(** Action and state manager *)

(** action object - repeat some function *)
class virtual action_object=
object(self)
  inherit poc_object as go

  val mutable fnode=new core_fun_node
  method get_fnode=fnode

  method private get_graphics_node=
    fnode#get_parent#get_parent#get_parent#get_children#get_object "graphics"

  method private get_graphic gid=
    let gr=self#get_graphics_node#get_children#get_object gid in
    graphic_of_fun gr#get_fun

  method private get_sprite=
    let spr=fnode#get_parent#get_parent#get_parent in
      sprite_of_fun spr#get_fun

  method private get_sprite_vault=
    let spr=fnode#get_parent#get_parent#get_parent#get_parent in
      sprite_vault_of_fun spr#get_fun

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



(* DEPRECATED *)
(** action with anim capabilities and lua func definition *)
class action_anim_OLD frs r=
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
class action_anim frs r=
object(self)
  inherit action_lua as al
  inherit anim_object frs r


  method on_loop()=
    self#anim();
    let gr=self#get_graphic "main" in
      gr#set_cur_drawing self#get_frame;
    al#on_loop(); 

  method on_stop()=
   self#set_current 0;
    al#on_stop();

  method lua_init()=
    lua#set_val (OLuaVal.String "get_frame") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_frame));

    al#lua_init();


end;;




(** action with sprite methods included and lua func definition *)
class action_sprite=
object(self)
  inherit action_lua as al

  val mutable get_x=fun l->[OLuaVal.Nil]
  val mutable get_y=fun l->[OLuaVal.Nil]
  val mutable jump=fun l->[OLuaVal.Nil]

  method private get_x=int_of_val(val_of_lua(List.nth (get_x [OLuaVal.Nil]) 0))
  method private get_y=int_of_val(val_of_lua(List.nth (get_y [OLuaVal.Nil]) 0))
  method private jump x y=jump[OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];

  method on_start ve=
    get_x<-self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "get_prect_x");
    get_y<-self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "get_prect_y");
    jump<-self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "jump");
    al#on_start ve;


  method lua_init()=
    al#lua_init();

end;;

(* DEPRECATED *)
(** action with movement capabilities and lua func definition *)
class action_movement_OLD=
object(self)
  inherit action_lua as al

  val mutable dir=NORTH
  val mutable d=0
  val mutable s=0

  val mutable cx=0
  val mutable cy=0


  val mutable get_x=fun l->[OLuaVal.Nil]
  val mutable get_y=fun l->[OLuaVal.Nil]
  val mutable jump=fun l->[OLuaVal.Nil]

  method on_start ve=
    dir<-direction_of_val (ve#get_val (`Int 0));
(*    d<-int_of_val (ve#get_val (`Int 1)); *)
    s<-int_of_val (ve#get_val (`Int 1));

    get_x<-self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "get_prect_x");
    get_y<-self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "get_prect_y");
    jump<-self#get_lua#get_parent#get_parent#get_parent#get_fun (OLuaVal.String "jump");

    al#on_start ve;


  method on_loop()=

      cx<-int_of_val(val_of_lua(List.nth (get_x [OLuaVal.Nil]) 0));
      cy<-int_of_val(val_of_lua(List.nth (get_y [OLuaVal.Nil]) 0));
    (match dir with 
      | NORTH ->cy<-cy-s;
      | NORTH_WEST ->cx<-cx+s;cy<-cy-s;
      | WEST ->cx<-cx+s;
      | SOUTH_WEST ->cx<-cx+s;cy<-cy+s;
      | SOUTH ->cy<-cy+s;
      | SOUTH_EAST ->cx<-cx-s;cy<-cy+s;
      | EAST ->cx<-cx-s;
      | NORTH_EAST ->cx<-cx-s;cy<-cy-s;
      | _ ->());

      jump[OLuaVal.Number (float_of_int cx);OLuaVal.Number (float_of_int cy)];
      al#on_loop();


  method lua_init()=
    al#lua_init();


end;;



(** action with movement capabilities and lua func definition *)
class action_movement=
object(self)
  inherit action_lua as al

  val mutable dir=NORTH
  val mutable d=0
  val mutable s=0

  val mutable cx=0
  val mutable cy=0


  method on_start ve=
    dir<-direction_of_val (ve#get_val (`Int 0));
(*    d<-int_of_val (ve#get_val (`Int 1)); *)
    s<-int_of_val (ve#get_val (`Int 1));

    al#on_start ve;


  method on_loop()=

      cx<-self#get_sprite#get_x();
      cy<-self#get_sprite#get_y();
    (match dir with 
      | NORTH ->cy<-cy-s;
      | NORTH_WEST ->cx<-cx+s;cy<-cy-s;
      | WEST ->cx<-cx+s;
      | SOUTH_WEST ->cx<-cx+s;cy<-cy+s;
      | SOUTH ->cy<-cy+s;
      | SOUTH_EAST ->cx<-cx-s;cy<-cy+s;
      | EAST ->cx<-cx-s;
      | NORTH_EAST ->cx<-cx-s;cy<-cy-s;
      | _ ->());

    self#get_sprite#jump cx cy;
    al#on_loop();


  method lua_init()=
    al#lua_init();


end;;



(** action with movement capabilities and lua func definition *)
class action_translation=
object(self)
  inherit action_lua as al

  val mutable time=0
  val mutable dx=0
  val mutable dy=0

  val mutable cx=0
  val mutable cy=0


  method on_start ve=

  let pos=position_of_val (ve#get_val (`Int 0)) in
    dx<-fst pos;
    dy<-snd pos;
    time<-int_of_val (ve#get_val (`Int 1));


    al#on_start ve;


  method on_loop()=

    if time<>0 then (
      cx<-self#get_sprite#get_x();
      cy<-self#get_sprite#get_y();

      let diffx=(dx-cx)/time and
	diffy=(dy-cy)/time in
      


	self#get_sprite#jump (cx+diffx) (cy+diffy);
	
	time<-time-1;
    );
    al#on_loop();


  method lua_init()=
    al#lua_init();


end;;



class action_2d_physics=
object(self)
  inherit action_lua as al

  val mutable grav=9.8

  val mutable mass=0.
  val mutable angle=0.
  val mutable velocity=0.

  val mutable a=0.

  val mutable time=0.

  val mutable v=0.

  val mutable cx=0
  val mutable cy=0

  val mutable nx=0.
  val mutable ny=0.


  method on_start ve=
    time<-0.;
    mass<-float_of_val (ve#get_val (`Int 0)); 
    grav<-float_of_val (ve#get_val (`Int 1));
    angle<-float_of_val (ve#get_val (`Int 2));
    velocity<-float_of_val (ve#get_val (`Int 3));

(*    print_string "mass: ";print_float mass;print_newline();
    print_string "grav: ";print_float grav;print_newline();
    print_string "angle: ";print_float angle;print_newline();
    print_string "velocity: ";print_float velocity;print_newline();
*)
    al#on_start ve;


  method on_loop()=

    cx<-self#get_sprite#get_x();
    cy<-self#get_sprite#get_y();

    a<- grav /. mass;

    v<-velocity +. a *. time; 

    nx <- (cos(angle)*.velocity) +. v*.0.;
    ny <- (sin(angle)*.velocity) +. v;

    self#get_sprite#jump (cx+(int_of_float nx)) (cy+(int_of_float ny));    



(*    print_int cx;print_string " - ";
    print_int cy;print_newline();
*)
    time<-time+.1.;


    al#on_loop();


  method lua_init()=
    al#lua_init();


end;;


class action_collision=
object(self)
  inherit action_lua as al

  val mutable collide_fun=fun l->[OLuaVal.Nil]

  method private on_collide id=
    ignore(collide_fun [OLuaVal.String id])

  method on_start ve=
    al#on_start ve;

  method on_loop()=

    self#get_sprite_vault#foreach_sprite (
      fun spr->
	  if(self#get_sprite#get_id<>spr#get_id) then
	    if(spr#get_prect#is_in_rect self#get_sprite#get_prect) then
	      (
		self#on_collide(spr#get_id);
(*		print_string ("collision "^spr#get_id);print_newline() *)
	      );

    );

    al#on_loop();

  method lua_init()=

    lua#set_val (OLuaVal.String "on_collide") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun id->()));

    al#lua_init();
    collide_fun<-lua#get_fun (OLuaVal.String "on_collide");



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
  inherit generic_object as go
  inherit lua_object as lo


  val mutable fnode=new core_fun_node
  method get_fnode=fnode

  method fun_init()=()


  method add_action n act=
(*    print_string ("STATE_OBJECT : add action "^n);print_newline(); *)
    ignore(self#add_object (Some n) act);

    act#get_fnode#set_parent fnode;
    fnode#get_children#add_object (Some n) act#get_fnode;

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
  val mutable current_ve=None

  val mutable fnode=new core_fun_node
  method get_fnode=fnode

  method fun_init()=
    fnode#set_id "states";

  method get_state=current
  method get_state_val=current_ve

  method add_state n st=
(*    print_string ("STATE_ACTIONS : add state "^n);print_newline(); *)
    ignore(self#add_object (Some n) st);

    st#fun_init();
    st#get_fnode#set_parent fnode;
    fnode#get_children#add_object (Some n) st#get_fnode;

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
	  current_ve<-(Some ve);
	    let o=self#get_object n in o#start ve
      | None -> current_ve<-(None));    

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

    lua#set_val (OLuaVal.String "get_state_val") 
      (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.value) 
	 (fun()->
	    match self#get_state_val with
	      | Some s->OLuaVal.Table s#to_lua#to_table
	      | None->OLuaVal.Nil
	 )
);

    lua#set_val (OLuaVal.String "set_no_state") 
      (OLuaVal.efunc ( OLuaVal.unit **->> OLuaVal.unit) 
	 (fun()->
	    self#set_state (None) (new val_ext_handler)
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

