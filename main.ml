open Low;;
(*open Fr;;*)
open Locale;;
open Config;;
open Video;;
open Audio;;



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


(*
let set_lang l=this_config.lang<-l;;
let n e=(Hashtbl.find locales this_config.lang)#get e;;
*)

class game_args=
object
end;;


let n e=(Hashtbl.find locales "en")#get e;;


class main=
object(self)
  val mutable info=new info
  method info=info


  method configfile=
    if Sys.os_type="Unix" then (Sys.getenv("HOME")^"/."^info#get_cmd^".conf")
    else (info#get_cmd^".conf")


  val mutable screen_tile=(tile_empty())
  method screen_tile=screen_tile

  val mutable scr_w=800
  val mutable scr_h=600

  method scr_w=scr_w
  method scr_h=scr_h

  val mutable server=ref false
  method server_mode= !server

  val mutable fullscreen=ref false
  val mutable windowed=ref false
  val mutable fps=32
  val mutable depth=16
 
  val mutable autosync=ref false
  method get_autosync= !autosync

  method set_depth d=depth<-d
  method set_fs f=fullscreen:=f
  method set_scr_w w=scr_w<-w
  method set_scr_h h=scr_h<-h
  method set_fps f=fps<-f
    
  val mutable conf=new config_file
    
  method set_lang l=self#this_config.lang<-l
  method this_config=conf#load self#configfile
  method save_config()=conf#save self#configfile self#this_config
  
  initializer
    at_exit (self#save_config);
    print_string self#configfile ;print_newline();


  (* get configs *)
  method get_config()=
    if self#this_config.screen_size=0 then (scr_w<-640;scr_h<-480);
    if self#this_config.screen_size=1 then (scr_w<-800;scr_h<-600);
    if self#this_config.screen_size=2 then (scr_w<-1024;scr_h<-768);
    if self#this_config.video_opt2=1 then (fullscreen:=true);
    

  method parse_args()=
    let args=[
      ("-fs",Arg.Set (fullscreen),(n("fullscreen mode")));
      ("-ws",Arg.Set (windowed),(n("windowed mode")));
      ("-autosync",Arg.Set (autosync),(n("auto sync medias")));
      ("-server",Arg.Set (server),(n("server mode"))); 
      ("-w",Arg.Int (self#set_scr_w),(n("screen width")));
      ("-h",Arg.Int (self#set_scr_h),(n("screen height")));
      ("-fps",Arg.Int (self#set_fps),(n("frame per second")));
      ("-bpp",Arg.Int (self#set_depth),(n("depth")));
      ("-lang",Arg.String (self#set_lang),(n("default language")))] in 
    let usage= "usage : "^info#get_cmd^" [-fs] [-ws] [-w width] [-h height] [-fps fps] [-lang lang]" in
      Arg.parse args (fun s -> ()) usage
	
  val mutable icon=""
  method get_icon=icon
  method set_icon i=icon<-i
    

  method medias_init()=
    all_init();
    
    if !windowed=true then fullscreen:=false;
    
    screen_tile<-video#init (scr_w) (scr_h) (depth) (!fullscreen);
    video#set_def_size 800 600;
    
    audio#init 44100 2 ;
    
    wm_set_caption ( info#get_name^" "^info#get_version) icon;
    (*"medias/misc/bfr_rebel.xpm"; *)
    
    audio#set_audio_vol ((self#this_config.audio_vol*128)/16);
    audio#set_music_vol ((self#this_config.music_vol*128)/16);
    
  frame_init();
  frame_set(fps);
  

end;;



let main=new main;;
