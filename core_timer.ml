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

(** Time manager *)

(*
 
 h=30x60x60 f = 108000 f
 m = 30x60 f = 1800 f
 s = 30 f
 f = f
*)

let default_fps=Global.empty ("fps");;
Global.set default_fps 30;;

type time=
{
  h:int;
  m:int;
  s:int;
  f:int;
}


class timer=
object(self)
  inherit generic_object
  val mutable timers=Hashtbl.create 2
  method add_timer (t:time) (f:unit->unit)=
    Hashtbl.add timers (self#from_time t) f
  method is_timer (t)=
    Hashtbl.mem timers (self#from_time t)
  method del_timer (t:time)=
    Hashtbl.remove timers (self#from_time t)

  val mutable tasks=Hashtbl.create 2
  method add_task (t:time) (f:unit->unit)=
(*    print_string "CORE_TIMER: add task ";print_newline() *)
    Hashtbl.add tasks (self#from_time t) f
  method is_task (t)=
    Hashtbl.mem tasks (self#from_time t)

  method del_task (t:time)=
    Hashtbl.remove tasks (self#from_time t)

  val fps=(Global.get default_fps)      
  val mutable frm=((Global.get default_fps)*60)
    
  val mutable cfrm=0
 
  method get_cfrm=cfrm

  val mutable run=false

  method start()=    
(*
    print_string "CORE_TIMER: start ";
print_newline() *) 
   run<-true
  method stop()=run<-false

  method set_limit t=frm<-self#from_time t

  method get_cur_frame=cfrm

  method reset()=cfrm<-0

  method step()=
(*
    print_string "CORE_TIMER: step ";
print_newline() *)
    if run then (
      Hashtbl.iter 
	(
	  fun tfr e->
	    if cfrm mod tfr=0 && cfrm>0 then e()
	) tasks;

      if Hashtbl.mem timers cfrm then (
	let e=Hashtbl.find timers cfrm in e();
	  Hashtbl.remove timers cfrm;
      );

      if cfrm<frm then
	cfrm<-cfrm+1
      else
	cfrm<-0
    )

  method to_time fr=
    let h=fr/(fps*60*60) and
	m=(fr mod (fps*60*60))/(fps*60) and
	s=((fr mod (fps*60*60)) mod (fps*60))/fps and
	f=(((fr mod (fps*60*60)) mod (fps*60)) mod fps) in
      {
	h=h;
	m=m;
	s=s;
	f=f;
      }
	
  method from_time t=    
    (t.h*fps*60*60)+ (t.m * fps * 60 ) + (t.s*fps) + t.f

  method add_timer_from_now (t:time) (f:unit->unit)=
(*    print_string "GAME_TIME: add timer "; *)
    let ft=self#from_time t in
    let nt=self#to_time (ft+cfrm) in
(*      print_int cfrm; *)
      self#add_timer nt f;
(*	print_newline(); *)
end;;


open Value_lua;;

(* FIXME : exist in core_val *)
let hash_of_lua_table tbl=
  let a=Hashtbl.create 2 in
    Luahash.iter (
      fun k v ->
	match k with
	  | OLuaVal.String s->Hashtbl.add a s (
	      match v with
		| OLuaVal.Number n->int_of_float n
		| _ -> 0
	    )
	  | _ ->()
    ) tbl;
    a

class lua_timer=
object(self)
  inherit timer
  inherit lua_object as lo

  method private time_of_lua t=
    let h=hash_of_lua_table t in
    let get_v v=Hashtbl.find h v in	
      {h=get_v "h";m=get_v "m";s=get_v "s";f=get_v "f"}
  
  method lua_init()=
    lua#set_val (OLuaVal.String "start") 
      (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (self#start));
    lua#set_val (OLuaVal.String "stop") 
      (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (self#stop));
    lua#set_val (OLuaVal.String "step") 
      (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (self#step));
    lua#set_val (OLuaVal.String "del_task") 
      (OLuaVal.efunc (OLuaVal.table **->> OLuaVal.unit) 
	 (fun t->
	    let ti=self#time_of_lua t in
	      self#del_task ti
	 )
      );


    lua#set_val (OLuaVal.String "add_task") 
      (OLuaVal.efunc (OLuaVal.table **-> OLuaVal.value **->> OLuaVal.unit) 
	 (fun t f->
	    let g()=
	      match f with
		| OLuaVal.Function (s,f)->
		    f [OLuaVal.Nil];()
		| _ -> () in

	    let ti=self#time_of_lua t in
		self#add_task ti g
		  
	 )
      );


    lua#set_val (OLuaVal.String "add_timer_from_now") 
      (OLuaVal.efunc (OLuaVal.table **-> OLuaVal.value **->> OLuaVal.unit) 
	 (fun t f->
	    let g()=
	      match f with
		| OLuaVal.Function (s,f)->
		    f [OLuaVal.Nil];()
		| _ -> () in

	    let ti=self#time_of_lua t in

		self#add_timer_from_now ti g
		  
	 )
      );

    lo#lua_init();
end;;
