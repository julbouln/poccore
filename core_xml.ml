open Value_xml;;
open Value_lua;;
open Value_val;;
open Value_xmlparser;;

open Core_val;;
open Core_action;;
open Core_graphic;;
open Core_medias;;
open Core_stage;;
open Core_main;;
open Core_font;;

open Binding;;

(** Core xml interface *)

(** globals *)

(** global default graphics parser, can be overided *)
let xml_default_graphics_parser=
  Global.empty("xml_default_graphics_parser");;

(** global default actions parser, can be overided *)
let xml_default_actions_parser=
  Global.empty("xml_default_actions_parser");;

(** global default stages parser, can be overided *)
let xml_default_stages_parser=
  Global.empty("xml_default_stages_parser");;



(** XML part *)

(** xml font parser : <font path="fontfile" size="sizeoffont"/> *)
class xml_font_parser=
object
  inherit xml_parser

  val mutable file=None
  val mutable size=0

(*  method get_val=new font_object file size *)
  method get_val=
    match file with
      | Some v->FontTTF (v,size)
      | None->FontEmbed
  method parse_attr k v=
    match k with
      | "path" -> file<-(Some v)
      | "size" -> size<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;

(*
(** xml tile parser : <tile path="tilefile"/> *)
class xml_tile_parser=
object
  inherit xml_parser

  val mutable file="none"

  method get_val=tile_load file
  method get_file=file

  method parse_attr k v=
    match k with
      | "path" -> file<-v
      | _ -> ()
  method parse_child k v=()


end;;
*)


(** v_color parser stuff *)
(* 8< *) 

class xml_v_color_parser=
object(self)
  inherit [(int*int*int),xml_color_parser] xml_list_parser "color" (fun()->new xml_color_parser) as list
  inherit xml_color_parser as vcolor

  method get_colors=
    let l=List.rev self#get_list in
     let da=DynArray.of_list l in
     DynArray.to_array da

  method parse_attr k v=vcolor#parse_attr k v
  method parse_child k v=list#parse_child k v

end;;

class xml_v_colors_parser=
object(self)
  inherit xml_parser

  val mutable vcolors=DynArray.create()    

  method get_vcolors=
    List.rev(DynArray.to_list vcolors)

  method parse_child k v=
       match k with
	 | "vcolor" -> let p=new xml_v_color_parser in p#parse v;DynArray.add vcolors (p#get_color,p#get_colors);
	 | _ ->();
  method parse_attr k v=()

end;;


let v_color_from_xml f=
(*  print_string ("XML: load "^f);print_newline(); *)
(*  let colfile=new xml_node (Xml.parse_file f) in *)
  let colfile=xml_node_from_file f in 
  let colparser=new xml_v_colors_parser in    
    colparser#parse colfile;
  let uc=new v_color in
    List.iter (
      fun v->(	
	uc#add_vcolor (fst v) (snd v) 
      )
    )
      colparser#get_vcolors;
  uc



(** Core xml part *)

class xml_val_ext_list_parser otag=
object(self)
  inherit xml_parser
  val mutable vals=new val_ext_handler

  method parse_attr k v=()

  method get_val=vals

  method parse_child k v=
    match k with
      | tag when tag=otag ->
(*	  let n=new xml_node_NEW in
	    n#of_xml_t v#get_node; *)
	    vals#from_xml v
      | _ -> ()
end;;


exception Xml_parser_not_found of string;;

(* 
parser must have : get_type, get_id and get_val with get_val#get_lua
   
*)

class ['pt,'t] xml_container_parser otag (gen_parser:unit->'pt)=
object(self)
  inherit xml_parser

  val mutable lua=""

  val mutable objs=DynArray.create()

  val mutable obj_parsers=Hashtbl.create 2
  method parser_add (n:string) (p:unit->'pt)=Hashtbl.add obj_parsers n p
  method parser_is n=Hashtbl.mem obj_parsers n
  method parser_get n=
    (try
       Hashtbl.find obj_parsers n
     with
	 Not_found -> raise (Xml_parser_not_found n))

  method parse_attr k v=()


  method parse_child k v=
    match k with
      | tag when tag=otag ->
	  let p=gen_parser() in p#parse v;
	    if self#parser_is p#get_type then
	      let sp=(self#parser_get p#get_type)() in (
		  sp#parse v;
		  DynArray.add objs sp#get_val
		)
      | "script" -> lua<-v#pcdata;
      | _ ->()
	  
	  

  method init_simple (add_obj:string->'t->unit)=
	DynArray.iter (
	  fun (n,o)->
	    let no=o() in	  	  
	      no#lua_init();
	      add_obj n (no);

	) objs;



end;;




class ['ot] xml_object_parser (new_obj:unit->'ot)= 
object (self)
  inherit xml_parser
  val mutable args_parser=new xml_val_ext_list_parser "args"

(** object unique id *)
  val mutable id=""
  method get_id=id
(** object type *)
  val mutable nm=""
  method get_type=nm

(** lua code for this object *)
  val mutable lua=""
(** object properties *)

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | "id" ->id<-v
      | _ -> ()
   
  method parse_child k v=
    args_parser#parse_child k v;
    match k with
      | "script" -> lua<-v#pcdata;
      | _ -> ()

(** object initial init *)
  method init_object o=
    o#set_lua_script lua;
    
  method get_val=
    let ofun()=
      let o=
	new_obj()
      in
	self#init_object o;
	o	  
    in      
      (id,ofun)

end;;

class xml_graphic_object_parser=
object (self)
  inherit [graphic_object] xml_object_parser (fun()->new graphic_object) as super
    
  method parse_child k v=
    super#parse_child k v;


(** object initial init *)
  method init_object o=
    o#set_lua_script(lua);
    let args=args_parser#get_val in
      if args#is_val (`String "layer") then
	o#set_layer (int_of_val(args#get_val (`String "layer")));
      
end;;

class xml_graphic_from_file_parser=
object(self)
  inherit xml_graphic_object_parser


  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	  let fn=string_of_val (args#get_val (`String "filename")) and
	      (nw,nh)=size_of_val (args#get_val (`String "size")) in
	    new graphic_from_file fn nw nh
      in
	self#init_object o;
	o	  
    in      
      (id,ofun)


end;;

class xml_graphic_from_drawing_fun_parser=
object(self)
  inherit xml_graphic_object_parser

  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	new graphic_from_drawing_fun_fmt (ValList (args#to_list()))
      in
	self#init_object o;
	o	  
    in      
      (id,ofun)


end;;




class xml_graphic_from_drawing_script_parser=
object(self)
  inherit xml_graphic_object_parser

  val mutable drs=new drawing_script

  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	let ds=text_of_val(args#get_val (`String "drawing_script")) in
	  drs#lua_init();
	  new graphic_from_drawing (random_string "dscr" 15)
	    (fun()->
		(drs#register ds)
	    )
      in
	self#init_object o;
	o	  
    in      
      (id,ofun)

end;;

class xml_graphic_from_drawing_create_parser=
object(self)
  inherit xml_graphic_object_parser

  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	let opname=string_of_val(args#get_val (`String "operation")) and
	    opargs=list_of_val (args#get_val (`String "args")) in	  
	new graphic_from_drawing (random_string "create_op" 15)
	  (
	    fun()->
	      let dr=drawing_vault#new_drawing() in
		dr#exec_op_create_from_list opname opargs;
		[|dr|]
	  );
      in
	self#init_object o;
	o	  
    in      
      (id,ofun)


end;;

class xml_graphics_parser=
object(self)
  inherit [xml_graphic_object_parser,graphic_object] xml_container_parser "graphic_object" (fun()->new xml_graphic_object_parser)

end;;



(* factory parser *)

let xml_factory_graphics_parser()=
  let p=new xml_graphics_parser in
    p#parser_add "graphic_from_file" (fun()->new xml_graphic_from_file_parser);
    p#parser_add "graphic_from_drawing_fun" (fun()->new xml_graphic_from_drawing_fun_parser);
    p#parser_add "graphic_from_drawing_create" (fun()->new xml_graphic_from_drawing_create_parser);
    p#parser_add "graphic_from_drawing_script" (fun()->new xml_graphic_from_drawing_script_parser);
    p;;


Global.set xml_default_graphics_parser xml_factory_graphics_parser;;



class xml_action_object_parser=
object(self)
  inherit [action_lua] xml_object_parser (fun()->new action_lua)


  method init_object o=
    o#set_lua_script (lua);

end;;


class xml_action_timed_parser=
object(self)
  inherit xml_action_object_parser

  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	  new action_timed (time_of_val (args#get_val (`String "limit")))
      in
	self#init_object (o:>action_lua);
	(o:>action_lua)	  
    in      
      (id,ofun)

end;;


class xml_action_intime_parser=
object(self)
  inherit xml_action_object_parser

  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
	  new action_intime (time_of_val (args#get_val (`String "limit")))
      in
	self#init_object (o:>action_lua);
	(o:>action_lua)	  
    in      
      (id,ofun)

end;;



class xml_action_anim_parser=
object(self)
  inherit xml_action_object_parser


  method get_val=
    let ofun()=
      let o=
	let args=args_parser#get_val in
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


end;;


class xml_actions_parser=
object(self)
  inherit [xml_action_object_parser,action_lua] xml_container_parser "action_object" (fun()->new xml_action_object_parser)

  val mutable id=""
  method get_id=id

  method parse_attr k v=
    match k with
      | "id" ->id<-v
      | _ -> ()

  method get_type="unique"

  method get_val=
    let ofun()=
      let o=new state_object in
	o#set_lua_script lua;
	self#init_simple o#add_action;
	o in
      (id,ofun)

end;;

(** Global parser def *)

let xml_factory_actions_parser()=
  let p=new xml_actions_parser in
    p#parser_add "action_lua" (fun()->new xml_action_object_parser);
    p#parser_add "action_anim" (fun()->new xml_action_anim_parser);
    p#parser_add "action_timed" (fun()->new xml_action_timed_parser);
    p#parser_add "action_intime" (fun()->new xml_action_intime_parser);
    p;;

Global.set xml_default_actions_parser xml_factory_actions_parser;;
  

class xml_state_actions_parser=
object(self)
  inherit [xml_actions_parser,state_object] xml_container_parser "state_object" (fun()->(Global.get xml_default_actions_parser)())

  initializer
    self#parser_add "unique" (fun()->(Global.get xml_default_actions_parser)())


  method get_val=
    let ofun()=
      let o=new state_actions in
	self#init_simple o#add_state;
	o in
      (ofun)

end;;


(** stages *)

class xml_stage_parser=
object (self)
  inherit [stage] xml_object_parser (fun()->new stage generic_cursor) as super
    

  method parse_child k v=
    super#parse_child k v;

(** object initial init *)
  method init_object o=
    o#set_lua_script (lua);

end;;


class xml_stages_parser=
object(self)
  inherit [xml_stage_parser,stage] xml_container_parser "stage" (fun()->new xml_stage_parser)
end;;


let xml_factory_stages_parser()=
  let p=new xml_stages_parser in
    p#parser_add "stage" (fun()->new xml_stage_parser);
    p;;


Global.set xml_default_stages_parser xml_factory_stages_parser;;

let stages_init_from_xml f=
(*  let stages_file=new xml_node (Xml.parse_file f) in *)
  let stages_file=xml_node_from_file f in
  let p=(Global.get xml_default_stages_parser)() in
    p#parse stages_file;
    p#init_simple stages#stage_add;
    ignore(stages#lua_init());

;;




class xml_game_parser=
object(self)
  inherit xml_parser
  val mutable info_parser=new xml_val_ext_list_parser "infos"
  val mutable args_parser=new xml_val_ext_list_parser "args"
  val mutable stages_parser=(Global.get xml_default_stages_parser)()
 
  
  method parse_attr k v=()

  method parse_child k v=
    info_parser#parse_child k v;
    args_parser#parse_child k v;
    match k with
      | "stages" -> stages_parser#parse v
      | _ -> ()

  method init()=
    (* infos *)
    if info_parser#get_val#is_val (`String "cmd") then
      main#info#set_cmd (string_of_val (info_parser#get_val#get_val (`String "cmd")));
    if info_parser#get_val#is_val (`String "name") then
      main#info#set_name (string_of_val (info_parser#get_val#get_val (`String "name")));
    if info_parser#get_val#is_val (`String "version") then
      main#info#set_version (string_of_val (info_parser#get_val#get_val (`String "version")));

    (* video *)
    if args_parser#get_val#is_val (`String "video_size") then (
      let (w,h)=(size_of_val (args_parser#get_val#get_val (`String "video_size"))) in
	main#set_scr_w w;
	main#set_scr_h h;
    );
    if args_parser#get_val#is_val (`String "video_default_size") then (
    let (dw,dh)=(size_of_val (args_parser#get_val#get_val (`String "video_default_size"))) in
      main#set_def_size dw dh;
    );

    if args_parser#get_val#is_val (`String "video_depth") then
      main#set_depth (int_of_val (args_parser#get_val#get_val (`String "video_depth")));

    if args_parser#get_val#is_val (`String "video_fullscreen") then
      main#set_fs (bool_of_val (args_parser#get_val#get_val (`String "video_fullscreen")));


    (* others *)
(*
    if args_parser#get_val#is_val (`String "parse_args") then (
    if (bool_of_val (args_parser#get_val#get_val (`String "parse_args"))) then
      main#parse_args();
    );
    if args_parser#get_val#is_val (`String "medias_init") then (
      if (bool_of_val (args_parser#get_val#get_val (`String "medias_init"))) then
	main#medias_init();
    );
*)
    main#parse_args();
    main#medias_init();
(*    if args_parser#get_val#is_val (`String "stages") then (
      stages_init_from_xml (string_of_val (args_parser#get_val#get_val (`String "stages")));
    );
*)
    stages_parser#init_simple stages#stage_add;
    ignore(stages#lua_init());

    if args_parser#get_val#is_val (`String "stage_start") then (
      stages#stage_load (string_of_val (args_parser#get_val#get_val (`String "stage_start")));
    );
end;;


let game_init_from_xml f=
(*  let game_file=new xml_node (Xml.parse_file f) in *)
  let game_file=xml_node_from_file f in
  let p=new xml_game_parser in
    p#parse game_file;
    p#init();;
