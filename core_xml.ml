open Oxml;;
open Oval;;
open Action;;
open Graphic;;


open Stage;;

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
	  vals#from_xml v#get_node
      | _ -> ()
end;;


class xml_metatype_parser=
object
  inherit xml_parser
  val mutable args_parser=new xml_val_ext_list_parser "args"
  
  val mutable id=""
  val mutable nm=""
 
  val mutable lua=""

  method get_val=(id,(nm,args_parser#get_val,lua))

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | "id" ->id<-v
      | _ -> ()
   
  method parse_child k v=
    args_parser#parse_child k v;
    match k with
      | "script" -> lua<-v#get_pcdata;
      | _ -> ()


end;;


exception Xml_parser_not_found of string;;

(* 
parser must have : get_type, get_id and get_val with get_val#get_lua
   
*)

class ['pt,'t] xml_parser_container otag (gen_parser:unit->'pt)=
object(self)
  inherit xml_parser

  val mutable mt=("",Hashtbl.create 2,"")
  method get_mt n=
    let (nm,h,l)=mt in
      Hashtbl.find h n

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
		  let (nm,h,l)=mt in
		  if Hashtbl.mem h p#get_id then (
		    sp#set_metatype (self#get_mt p#get_id);
		  );
		  sp#parse v;
		  DynArray.add objs sp#get_val
		)
      | _ ->()
	  
	  

  method init_simple (add_obj:string->'t->unit)=
	DynArray.iter (
	  fun (n,o)->
	    let no=o() in	  	  
	      no#lua_init();
	      add_obj n (no);

	) objs;


  method init (add_obj:string->'t->unit)=
    let (hnm,h,hl)=mt in
    Hashtbl.iter (
      fun k v->

	let r=ref false in
	DynArray.iter (
	  fun (n,o)->
	    if n=k then (
	      let no=o() in	  	  
		no#lua_init();
		add_obj n (no);
		r:=true;
	    )
	) objs;

	if !r=false then (
	  let (nm,_,_)=v in
	  if self#parser_is nm then (
	    let sp=(self#parser_get nm)() in
	    let no=(sp#get_val_from_meta v)() in
	      no#lua_init();
	      add_obj k no
	  )
	)

    ) h;


  method init_from_meta_h (add_obj:string->'t->unit) h=
    Hashtbl.iter (
      fun k v->

	let r=ref false in
	DynArray.iter (
	  fun (n,o)->
	    if n=k then (
	      let no=o() in	  	  
		no#lua_init();
		add_obj n (no);
		r:=true;
	    )
	) objs;

	if !r=false then (
	  let (nm,_,_)=v in
	  if self#parser_is nm then (
	    let sp=(self#parser_get nm)() in
	    let no=(sp#get_val_from_meta v)() in
	      no#lua_init();
	      add_obj k no
	  )
	)

    ) h;
    


end;;

(* shit *)
(*< NEW object with meta parser *)

class ['ot] xml_object_parser_NEW (new_obj:unit->'ot)= 
object (self)
  inherit xml_parser

(** object unique id *)
  val mutable id=""
  method get_id=id
(** object type *)
  val mutable nm=""
  method get_type=nm

(** object args *)
  val mutable args=new val_ext_handler
  method get_args=args

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | "id" ->id<-v
      | _ -> ()
   
  method parse_child k v=
    let args_parser=new xml_val_ext_list_parser "args" in
    args_parser#parse_child k v;
    args<-args_parser#get_val;
    self#merge_metas();

  (* parse metas file and merge with current *)
  method merge_metas()=
    if args#is_val (`String "metatypes") then (
      let metas=list_of_val(args#get_val (`String "metatypes")) in
	List.iter (
	  fun meta->
	    let metafilename=string_of_val meta in
	    let metafile=new xml_node (Xml.parse_file metafilename) in
	    let p=Oo.copy self in
	      p#parse metafile;
	      args#merge p#get_args;
	      args#append (`String "script") p#get_args;
	) metas;
    );

(** object initial init *)
  method init_object o=
    let lua= (text_of_val(args#get_val (`String "script"))) in
      o#set_lua_script lua
	    
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



class ['ot] xml_objects_parser ct (new_obj:unit->'ot)=
object(self)
  inherit [unit->'ot] xml_stringhash_parser ct (fun()->new xml_object_parser_NEW new_obj)

end;;


class ['ot] xml_objects_typed_parser ct (new_obj:unit->'ot)=
object(self)
  inherit [unit->'ot] xml_stringhash_parser ct (fun()->new xml_object_parser_NEW new_obj)
    
  val mutable obj_parsers=Hashtbl.create 2
  method parser_add (n:string) (p:unit->('ot) xml_object_parser_NEW)=Hashtbl.add obj_parsers n p
  method parser_is n=Hashtbl.mem obj_parsers n
  method parser_get n=
    (try
       Hashtbl.find obj_parsers n
     with
	 Not_found -> raise (Xml_parser_not_found n))

  method parse_child k v=
    match k with
      | tag when tag=ct ->
	  let p=new xml_object_parser_NEW new_obj in p#parse v;
	    if self#parser_is p#get_type then
	      let sp=(self#parser_get p#get_type)() in (
		  sp#parse v;
		  let spr=sp#get_val in
		  Hashtbl.add h (fst spr) (snd spr)
		)
      | _ ->()


end;;


(* NEW object with meta parser >*)

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
      | "script" -> lua<-v#get_pcdata;

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


(*
 <graphic_object type="graphic_from_file">
  <args>
   <val_string name="filename" value="filename.png"/>
   <val_size name="size" w="wsize" h="hsize"/>
  </args>
  <script>
   function self.on_update()
    obj=self.parent;
    self.move(obj.get_pixel_x,obj.get_pixel_y)
   end
  </script>
 </graphic_object>

is equal to

 <graphic_object type="graphic_from_drawing_fun">
  <position x="0" y="0"/>
  <args>
   <val_string value="with_alpha"/>
   <val_color r="255" g="255" b="255"/>
   <val_string value="load_multiple"/>
   <val_string value="filename.png"/>
   <val_size w="wsize" h="hsize"/>   
  </args>
 </graphic_object>  
*)

class xml_graphic_object_parser=
object (self)
  inherit [graphic_object] xml_object_parser (fun()->new graphic_object) as super
    
  val mutable mt=("",new val_ext_handler,"")
  method set_metatype (m:string*val_ext_handler*string)=mt<-m

  method parse_child k v=
    super#parse_child k v;


(** object initial init *)
  method init_object o=
    let (nm,vh,l)=mt in
    o#set_lua_script (l^lua);
(*    o#set_layer layer;
    o#move x y;
*)

  method get_val_from_meta (m:string*val_ext_handler*string)=
    let ofun()=
      let (nm,vh,l)=m in
      let o=new graphic_object in
	o#set_lua_script (l);
	o in	
      ofun
end;;

class xml_graphic_from_file_parser=
object(self)
  inherit xml_graphic_object_parser


  method get_val=
    let ofun()=
      let o=
	let (nm,vh,l)=mt in
	let args=args_parser#get_val in
	  args#merge vh;
	  let fn=string_of_val (args#get_val (`String "filename")) and
	      (nw,nh)=size_of_val (args#get_val (`String "size")) in
	    new graphic_from_file fn nw nh
      in
	self#init_object o;
	o	  
    in      
      (id,ofun)


  method get_val_from_meta (m:string*val_ext_handler*string)=
    let ofun()=
      let (nm,vh,l)=m in
      let o=
      let fn=string_of_val (vh#get_val (`String "filename")) and
	  (nw,nh)=size_of_val (vh#get_val (`String "size")) in
	new graphic_from_file fn nw nh in
	o#set_lua_script (l);
	o in	
      (ofun)

end;;

class xml_graphic_from_drawing_fun_parser=
object(self)
  inherit xml_graphic_object_parser

  method get_val=
    let ofun()=
      let o=
	let (nm,vh,l)=mt in
	let args=args_parser#get_val in
	  args#merge vh;
	new graphic_from_drawing_fun_fmt (ValList (args#to_list()))
      in
	self#init_object o;
	o	  
    in      
      (id,ofun)


  method get_val_from_meta (m:string*val_ext_handler*string)=
    let ofun()=
      let (nm,vh,l)=m in
      let o=
	new graphic_from_drawing_fun id (vh#to_list()) in
	o#set_lua_script (l);
	o in	
      (ofun)

end;;

class xml_graphics_parser=
object(self)
  inherit [xml_graphic_object_parser,graphic_object] xml_parser_container "graphic_object" (fun()->new xml_graphic_object_parser)
  method set_metatype (m:string*(string,string*val_ext_handler*string)Hashtbl.t*string)=mt<-m

end;;


(** metatype *)

class xml_graphic_object_mt_parser=
object(self)
  inherit xml_metatype_parser
end;;


class xml_graphics_mt_parser=
object(self)
  inherit [(string*val_ext_handler*string)] xml_stringhash_parser "graphic_object" (fun()->new xml_graphic_object_mt_parser)

end;;

(* factory parser *)

let xml_factory_graphics_parser()=
  let p=new xml_graphics_parser in
    p#parser_add "graphic_from_file" (fun()->new xml_graphic_from_file_parser);
    p#parser_add "graphic_from_drawing_fun" (fun()->new xml_graphic_from_drawing_fun_parser);
    p;;

(** global default graphics parser, can be overided *)
let xml_default_graphics_parser=
  let gl=Global.empty("xml_default_graphics_parser") in
    Global.set gl xml_factory_graphics_parser;
    gl;;


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


class xml_action_timed_parser=
object(self)
  inherit xml_action_object_parser

  method get_val=
    let ofun()=
    let (nm,vh,l)=mt in      
      let o=
	let args=args_parser#get_val in
	  args#merge vh;
	  new action_timed (time_of_val (args#get_val (`String "limit")))
      in
	self#init_object (o:>action_lua);
	(o:>action_lua)	  
    in      
      (id,ofun)

  method get_val_from_meta (m:string*val_ext_handler*string)=
    let ofun()=
      let (nm,vh,l)=m in
      let o=
	new action_timed (time_of_val (vh#get_val (`String "limit"))) in
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
      let o=new state_object in
	self#init o#add_action;
	o in
      (id,ofun)

  method get_val_from_meta (m:string*(string,string*val_ext_handler*string)Hashtbl.t*string)=
    let ofun()=
      let (nm,h,l)=m in
      let o=new state_object in 
	self#init_from_meta_h o#add_action h;
(*	o#set_lua_script l; *)
	o in
      (ofun)

end;;

(** Global parser def *)

let xml_factory_actions_parser()=
  let p=new xml_actions_parser in
    p#parser_add "action_lua" (fun()->new xml_action_object_parser);
    p#parser_add "action_anim" (fun()->new xml_action_anim_parser);
    p#parser_add "action_timed" (fun()->new xml_action_timed_parser);
    p;;

(** global default actions parser, can be overided *)
let xml_default_actions_parser=
  let gl=Global.empty("xml_default_actions_parser") in
    Global.set gl xml_factory_actions_parser;
    gl;;
  

class xml_state_actions_parser=
object(self)
  inherit [xml_actions_parser,state_object] xml_parser_container "state_object" (fun()->(Global.get xml_default_actions_parser)())


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


(** stages *)

class xml_stage_parser=
object (self)
  inherit [stage] xml_object_parser (fun()->new stage generic_cursor) as super
    
  val mutable mt=("",new val_ext_handler,"")
  method set_metatype (m:string*val_ext_handler*string)=mt<-m


  method parse_child k v=
    super#parse_child k v;

(** object initial init *)
  method init_object o=
    o#set_lua_script (lua);

  method get_val_from_meta (m:string*val_ext_handler*string)=
    let ofun()=
      let (nm,vh,l)=m in
      let o=new stage generic_cursor in
	o#set_lua_script (l);
	o in	
      ofun
end;;


class xml_stages_parser=
object(self)
  inherit [xml_stage_parser,stage] xml_parser_container "stage" (fun()->new xml_stage_parser)
end;;


let xml_factory_stages_parser()=
  let p=new xml_stages_parser in
    p#parser_add "stage" (fun()->new xml_stage_parser);
    p;;

(** global default actions parser, can be overided *)
let xml_default_stages_parser=
  let gl=Global.empty("xml_default_stages_parser") in
    Global.set gl xml_factory_stages_parser;
    gl;;

let stages_init_from_xml f=
  let stages_file=new xml_node (Xml.parse_file f) in
  let p=(Global.get xml_default_stages_parser)() in
    p#parse stages_file;
    p#init_simple stages#stage_add;
    ignore(stages#lua_init());

;;
