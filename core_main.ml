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


open Core_timer;;
open Core_val;;
open Core_stage;;

open Value_lua;;
open Value_val;;
open Value_xml;;

open Binding;;

(** Main app part *)

let n s=s;;

class info=
object(self)
  val mutable version="not specified"
  val mutable date="not specified"
  val mutable license="not specified"

  val mutable name="no specified"
  method set_name n=name<-n
  method get_name=name

  val mutable cmd="bfr"
  method set_cmd c=cmd<-c
  method get_cmd=cmd

  method set_version v=version<-v
  method get_version=version

  method set_date d=date<-d
  method get_date=date

  method set_license l=license<-l
  method get_license=license


(* authors, license, blabla *)
  method print()=
    print_string "---------------------------------------------------\n";
    print_string (self#get_name^" - "^self#get_version^", ");
    print_string self#get_license;
    
    print_string "---------------------------------------------------\n";


end;;



class game_args=
object
end;;

exception Stages_not_set

class main=
object(self)
  
  val mutable drawing_vault=new binding_drawing_vault 10000 (1./.25.)
  method get_drawing_vault=drawing_vault
  
  val mutable stages=None

  initializer
    stages<-Some (new stages drawing_vault (generic_cursor drawing_vault))

  method get_stages=
    match stages with
      Some s->s
    | None -> raise Stages_not_set
	  
  val mutable info=new info
  method info=info

  val mutable interp=new lua_interp
  method get_interp=interp


(*
  method configfile=
    if Sys.os_type="Unix" then (Sys.getenv("HOME")^"/."^info#get_cmd^".conf")
    else (info#get_cmd^".conf")
*)

(*  val mutable screen_tile=(tile_empty())
  method screen_tile=screen_tile
*)
  (* video part *)
      
  val mutable scr_w=640
  val mutable scr_h=480

  method scr_w=scr_w
  method scr_h=scr_h

  val mutable server=ref false
  method server_mode()= !server

  val mutable fullscreen=ref false
  val mutable windowed=ref false
  val mutable fps=30
  val mutable depth=0
 
  val mutable autosync=ref false
  method get_autosync= !autosync

  method set_depth d=depth<-d
  method set_fs f=fullscreen:=f
  method set_scr_w w=scr_w<-w
  method set_scr_h h=scr_h<-h

  method set_fps f=fps<-f
  method get_fps=fps

  val mutable def_w=640
  val mutable def_h=480

(** set the default size *)
  method set_def_size w h=def_w<-w;def_h<-h


(** calculate the width size from the ratio *)
  method f_size_w w=let f=(float_of_int scr_w)/.(float_of_int def_w) in int_of_float(f*.(float_of_int w))
(** calculate the height size from the ratio *)
  method f_size_h h=let f=(float_of_int scr_h)/.(float_of_int def_h) in int_of_float(f*.(float_of_int h))

(** get the width ratio from default width and real width *)		      
  method get_fact_w()=(float_of_int scr_w)/.(float_of_int def_w)
(** get the height ratio from default height and real height *)
  method get_fact_h()=(float_of_int scr_h)/.(float_of_int def_h)


(*    
  val mutable conf=new config_file
    
  method set_lang l=self#this_config.lang<-l
  method this_config=conf#load self#configfile
  method save_config()=conf#save self#configfile self#this_config
*)
  
(*  initializer
    at_exit (self#save_config);
    print_string self#configfile ;print_newline();
*)

  (* get configs *)
(*  method get_config()=
    if self#this_config.screen_size=0 then (scr_w<-640;scr_h<-480);
    if self#this_config.screen_size=1 then (scr_w<-800;scr_h<-600);
    if self#this_config.screen_size=2 then (scr_w<-1024;scr_h<-768);
    if self#this_config.video_opt2=1 then (fullscreen:=true);
*)  

  method parse_args()=
    let args=[
      ("-fs",Arg.Set (fullscreen),(n("fullscreen mode")));
      ("-ws",Arg.Set (windowed),(n("windowed mode")));
      ("-autosync",Arg.Set (autosync),(n("auto sync medias")));
      ("-server",Arg.Set (server),(n("server mode"))); 
      ("-w",Arg.Int (self#set_scr_w),(n("screen width")));
      ("-h",Arg.Int (self#set_scr_h),(n("screen height")));
      ("-fps",Arg.Int (self#set_fps),(n("frame per second")));
      ("-bpp",Arg.Int (self#set_depth),(n("depth")))] in
(*      ("-lang",Arg.String (self#set_lang),(n("default language")))] in *)
    let usage= "usage : "^info#get_cmd^" [-fs] [-ws] [-w width] [-h height] [-fps fps] [-lang lang]" in
      Arg.parse args (fun s -> ()) usage
	
  val mutable icon=""
  method get_icon=icon
  method set_icon i=icon<-i
    

  method medias_init()=
    if !windowed=true then fullscreen:=false;
    
    Global.set default_fps fps;

    drawing_vault#init (scr_w) (scr_h) (depth) (!fullscreen);
    
(*    audio#init 44100 2 ; *)
    
    drawing_vault#set_caption ( info#get_name^" "^info#get_version) icon;

end;;



(*let main=new main;;*)



