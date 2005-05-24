(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003 POC 

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

open Core_event;;
open Core_video;;
open Core_medias;;
open Core_cursor;;
open Core_timer;;
open Core_graphic;;
open Core_font;;
open Core_timer;;

open Binding;;


open Value_lua;;

let rec usleep sec = 
  (try 
     ignore (Unix.select [] [] [] sec)
   with Unix.Unix_error _ -> ());;
class frame_limiter=
object(self)

  val mutable t1=0.
  val mutable t2=0.

  val mutable ffps=float (Global.get default_fps)
  method set_ffps f=ffps<-f
  method get_ffps=ffps

  val mutable fcount=0
  val mutable lcount=0				

  val mutable fpsgr=None
  val mutable show_fps=false
  method set_show_fps s=
    show_fps<-s;
    if show_fps then
      fpsgr<-(Some (new graphic_text "fpsinfo" (FontEmbed) (200,200,200)))
      
  method put_fps()=
    match fpsgr with
      | Some g->
	g#move 8 (video#get_h - 16);
	g#put();
      | None -> ()

  method get_current_fps=
    if lcount<>0 then
      fcount/lcount
    else
      0

  val mutable fmod=0.
  method set_fmod f=fmod<-f;

  method frame_drop=
    (float self#get_current_fps)<ffps

  method start()=
    t1<-Unix.gettimeofday();
  method finish()=
    t2<-Unix.gettimeofday();
    lcount<-lcount+1;
    fcount<-fcount+(int_of_float (1./.((t2 -. t1)+. fmod)));
    (match fpsgr with
      | Some g ->
	  g#set_text ("fps: "^string_of_int(fcount/lcount)); 
      | None ->());
    if (t2 -. t1)<(1./. ffps) then
      usleep ((1./. ffps)  -. ((t2 -. t1) +. fmod));     
  
end;;


(** Stage subsystem *)
(** Stage is a high-level container for interface and game engine. When you define a stage, you specify some thing to do when loading, leaving, on each frame and the event parser. You can handle multiple stage through the stages global class. See exemples for more informations *)

(** stage class *)
class stage  (cursor:cursors)=
object (self)
  inherit generic_object
  inherit lua_object as super
  val mutable initialized=false
  val curs=cursor

  val mutable ltimer=new lua_timer

  val mutable graphic_ops=true
  method set_graphic_ops g=graphic_ops<-g


  (** graphic echange *)
  method get_graphic (id:string) (gid:string)=(None:graphic_object option)
  method add_graphic (id:string) (gid:string) (go:graphic_object)=()
  method delete_graphic (id:string) (gid:string)=()

  val mutable canvas=new canvas
  method get_canvas=canvas
  method set_canvas c=canvas<-c


  val mutable load_fun=fun l->[OLuaVal.Nil]
  val mutable loop_fun=fun l->[OLuaVal.Nil]

(** {2 Virtual part} *)


(** what to do when first load stage *)
  method on_load()=

    ignore(load_fun [OLuaVal.Nil])

(** what to do on each frame *)
  method on_loop()=
    ignore(loop_fun [OLuaVal.Nil]);

  method on_loop_graphic()=()

(** what to do when quit stage *)
  method on_quit()=()
  method on_reinit()=()
(** what to do when continue a stage already loaded *)
  method on_continue()=()
  method on_leave()=()


(** parse the event coming in stage *)
  method ev_parser (e:event)=
    (match e with
       | EventMouse em ->
	   (match em with
	      | MouseMotion(x,y) -> 
		  curs#move x y;
	      | MouseRelease(x,y,but) -> 
		  curs#set_state "normal";
	      | MouseClick(x,y,but) -> 
		  curs#set_state "clicked";
	      | _ -> ()
	   )
       | _ -> ()
    )


(** {2 General part} *)

  (** get if stage initialized *)
  method get_initialized=initialized


  method get_curs=curs

  val mutable frml=new frame_limiter
  method get_frame_limiter=frml
  method load()=
    initialized<-true;
    self#on_load();

    eventm#set_parser self#ev_parser;
    eventm#set_on_quit self#on_quit;
    eventm#set_on_loop 
      (
	fun()->
	  frml#start();
	  self#on_loop();
	  if graphic_ops then
	    (  
	      if frml#frame_drop=false then
		self#on_loop_graphic();
	      curs#put();
	      frml#put_fps();
	      video#flip();
	    );
	  frml#finish();	  
      );
    eventm#init();
    eventm#main();()
  
  method continue()=
    self#on_continue();

    eventm#set_parser self#ev_parser;
    eventm#set_on_quit self#on_quit;
    eventm#set_on_loop self#on_loop;
    eventm#init();
    eventm#main();()

  method reinit()=self#on_reinit();
  method leave()=self#on_leave();

  method lua_init()=
    lua#set_val (OLuaVal.String "on_load") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (fun()->()));
    lua#set_val (OLuaVal.String "on_loop") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) (fun()->()));

    ignore(ltimer#lua_init()); 
    self#lua_parent_of "timer" (ltimer:>lua_object);

    super#lua_init();
    load_fun<-lua#get_fun (OLuaVal.String "on_load");
    loop_fun<-lua#get_fun (OLuaVal.String "on_loop");

end;;

class multi_stage  (cursor:cursors)=
object(self)
  inherit [stage] generic_object_handler3
  inherit stage cursor as super

  method add_stage n o=
    ignore(self#add_object (Some n) o);
    ignore(o#lua_init());
    self#lua_parent_of n (o:>lua_object);
(*    o#set_canvas canvas; *)

  method on_load()=
    super#on_load();
    self#foreach_object (
      fun n s-> s#on_load()
    );


  method on_loop()=
    super#on_loop();
    self#foreach_object (
      fun n s-> s#on_loop()
    );

  method on_loop_graphic()=
    super#on_loop_graphic();
    self#foreach_object (
      fun n s-> s#on_loop_graphic()
    );

  method on_continue()=
    self#foreach_object (
      fun n s-> s#on_continue()
    )

  method on_leave()=
    self#foreach_object (
      fun n s-> s#on_leave()
    )

  method ev_parser e=
    super#ev_parser e;
    self#foreach_object (
      fun n s-> s#ev_parser e
    )


  method move_graphic gid sst sid dst did=
    let sstate=self#get_object sst in
    let go=sstate#get_graphic sid gid in
      match go with
	| Some gr->
	    let dstate=self#get_object dst in
	      dstate#add_graphic did gid gr;
	      sstate#delete_graphic sid gid 
	| None -> ()

  method copy_graphic gid sst sid dst did=
    let sstate=self#get_object sst in
    let go=sstate#get_graphic sid gid in
      match go with
	| Some gr->
	    let dstate=self#get_object dst in
	      dstate#add_graphic did gid (Oo.copy gr);
	| None -> ()
	    
  method lua_init()=
    lua#set_val (OLuaVal.String "move_graphic") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit)  
	  self#move_graphic
);

    lua#set_val (OLuaVal.String "copy_graphic") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit)  
	 self#copy_graphic
);


    super#lua_init()
end;;

exception Stage_not_found of string;;

(** General stages handler *)
class stages curs=
object(self)
  inherit lua_object as lo
  method get_id="stages"
  val mutable current_stage="none"


  (* create an Hashtbl of stages *)
  val mutable stages=let a=Hashtbl.create 2 in Hashtbl.add a "none" (new stage curs);a;

  (** add a stage in stages *)
  method stage_add n s=
    if (Hashtbl.mem stages n)==true then
      (
	let v=self#stage_get n in
	  v#reinit();
	  Hashtbl.replace stages n s;
      )
    else
      (
	Hashtbl.add stages n s;
      );


    s#set_id n;
    ignore(s#lua_init());
    self#lua_parent_of n (s:>lua_object);



  (** check for a stage in stages *)
  method stage_is n=Hashtbl.mem stages n;

  (** get a stage in stages *)
  method stage_get n=
    (try
    Hashtbl.find stages n;
     with Not_found -> raise (Stage_not_found n))

  (** load a stage in stages *)
  method stage_load n=
    (self#stage_get n)#get_curs#set_state "normal";
    if current_stage<>"none" then
      (self#stage_get current_stage)#leave();   
    current_stage<-n;
    (self#stage_get n)#load();
  
  (** continue with a stage in stages *)
  method stage_continue n=
    (self#stage_get n)#get_curs#set_state "normal";
    (self#stage_get current_stage)#leave(); 
    current_stage<-n;
    (self#stage_get n)#continue();

  (** leave a stage in stages *)
  method stage_leave n=(self#stage_get n)#leave();
  
(*  method stage_connect v=v#init(); *)

  method lua_init()=

    lua#set_val (OLuaVal.String "load") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#stage_load);
    lua#set_val (OLuaVal.String "continue") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#stage_continue);
    lua#set_val (OLuaVal.String "leave") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#stage_leave);
    lo#lua_init()

end;;


let generic_cursor=new cursors 30 30 None;;
let stages=new stages generic_cursor;;


(* FIXME : must declare stages here like video and audio *)

