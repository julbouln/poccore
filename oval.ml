open Generic;;
open Xml;;
open Oxml;;
open Olua;;

(** Val system with conversion between OCaml, XML and Lua *)

exception Bad_val_type;;
exception Val_not_found of string;;

(** Generic val type *)

type val_generic=
    [
    | `Int of int
    | `String of string
    | `Float of float
    | `Bool of bool
    | `Nil
    ]
;;

let xml_of_val=function
  | `Int i->Xml.Element("val_int",[("value",string_of_int i)],[])
  | `String s->Xml.Element("val_string",[("value",s)],[])
  | `Float f->Xml.Element("val_float",[("value",string_of_float f)],[])
  | `Bool b->Xml.Element("val_bool",[("value",if b then "true" else "false")],[])
  | `Nil -> Xml.Element("val_nil",[],[]);;



let val_of_xml=function
  | Element("val_int",_,_) as x-> `Int (int_of_string (Xml.attrib x "value"))
  | Element("val_string",_,_) as x-> `String (Xml.attrib x "value")
  | Element("val_float",_,_) as x-> `Float (float_of_string (Xml.attrib x "value"))
  | Element("val_bool",_,_) as x-> `Bool (match (Xml.attrib x "value") with
					    | "true" -> true
					    | "false" -> false
					    | _ -> false
					 )
  | Element("val_nil",_,_) as x-> `Nil
  | _->`Nil

let lua_of_val=function
  | `Int i->OLuaVal.Number (float_of_int i) 
  | `String s->OLuaVal.String s
  | `Float f->OLuaVal.Number f
  | `Bool s->OLuaVal.Number (if s then 1. else 0.)
  | `Nil->OLuaVal.Nil
;; 

let val_of_lua=function
  | OLuaVal.Number f->
      if float (truncate f)=f then
	`Int (truncate f)
      else
	`Float f
  | OLuaVal.String s->`String s
  | OLuaVal.Nil -> `Nil
  | _ -> `Nil
;;

let int_of_val=function
  | `Int v->v
  | `String v->int_of_string v
  | `Float v->int_of_float v
  | _->raise Bad_val_type;;

let string_of_val=function
  | `String v->v
  | `Int v->string_of_int v
  | `Float v->string_of_float v
  | _->raise Bad_val_type;;

let float_of_val=function
  | `Int v->v
  | `Float v->float_of_int v
  | _->raise Bad_val_type;;

let bool_of_val=function
  | `Bool v->v
  | _->raise Bad_val_type;;

(** Extended val type *)

type val_ext=
    [
      val_generic
    | `Position of (int*int)
    | `Size of (int*int)
    | `Color of (int*int*int)
    | `List of val_ext list 
(*    | `Function of val_ext list-> val_ext list *)
    ]
;;

let size_of_val=function
  | `Size v->v
  | _->raise Bad_val_type;;

let position_of_val=function
  | `Position v->v
  | _->raise Bad_val_type;;


let color_of_val=function
  | `Color v->v
  | _->raise Bad_val_type;;

let list_of_val=function
  | `List v->v
  | _->raise Bad_val_type;;

let rec xml_of_val_ext=function
  | #val_generic as v->xml_of_val v
  | `Position (x,y)->Xml.Element("val_position",[("x",string_of_int x);("y",string_of_int y)],[])
  | `Size (w,h)->Xml.Element("val_size",[("w",string_of_int w);("h",string_of_int h)],[])
  | `Color (r,g,b)->Xml.Element("val_color",[("r",string_of_int r);("g",string_of_int g);("b",string_of_int b)],[])
  | `List vl->Xml.Element("val_list",[],List.map (fun v->xml_of_val_ext v) vl)
;; 

let rec val_ext_of_xml=function
  | Element("val_position",_,_) as x-> 
      `Position (
	(int_of_string (Xml.attrib x "x")),
	(int_of_string (Xml.attrib x "y"))
      )
  | Element("val_size",_,_) as x-> 
      `Size (
	(int_of_string (Xml.attrib x "w")),
	(int_of_string (Xml.attrib x "h"))
      )
  | Element("val_color",_,_) as x->
      `Color (
	(int_of_string (Xml.attrib x "r")),
	(int_of_string (Xml.attrib x "g")),
	(int_of_string (Xml.attrib x "b"))
      )
  | Element("val_list",_,childs) as x->`List (List.map (fun c->val_ext_of_xml c) childs)
  | Element(_,_,_) as x->val_of_xml x
  | _ -> `Nil
;;

let lua_table_of_list l=
  let tbl=Luahash.create (fun a b->a=b) 2 in
    List.iter (
      fun (k,v) ->
	Luahash.replace tbl ~key:(OLuaVal.String k) ~data:(v)
    ) l;
    tbl;;

let lua_list_of_list valfrom l=
  let tbl=Luahash.create (fun a b->a=b) 2 in
  let i=ref 0 in
    List.iter (
      fun v ->
	Luahash.replace tbl ~key:(OLuaVal.Number (float_of_int !i)) ~data:(valfrom v);
	  i:= !i+1;
    ) l;

    tbl;;

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

let list_of_lua_list valto tbl=
  let a=DynArray.create() in
    Luahash.iter (
      fun k v ->
	match k with
	  | OLuaVal.Number i->DynArray.set a (int_of_float i) (valto v)
	  | _ ->()
    ) tbl;
    DynArray.to_list a

let rec lua_of_val_ext=function
  | #val_generic as v->lua_of_val v
  | `Position (x,y)->
      OLuaVal.Table (lua_table_of_list [
	("x",OLuaVal.Number (float x));
	("y",OLuaVal.Number (float y));
      ])
  | `Size (w,h)->
      OLuaVal.Table (lua_table_of_list [
	("w",OLuaVal.Number (float w));
	("h",OLuaVal.Number (float h));
      ])
  | `Color (r,g,b)->
      OLuaVal.Table (lua_table_of_list [
	("r",OLuaVal.Number (float r));
	("g",OLuaVal.Number (float g));
	("b",OLuaVal.Number (float b));
      ])
  | `List l->OLuaVal.Table (lua_list_of_list lua_of_val_ext l);
;; 

let rec val_ext_of_lua=function
  | OLuaVal.Table tbl->
      let r=ref `Nil in
      let h=hash_of_lua_table tbl in
      let is_v v=Hashtbl.mem h v and
	  get_v v=Hashtbl.find h v in	
	
	if is_v "w" && is_v "h" then
	  r:=`Size (get_v "w",get_v "h");

	if is_v "x" && is_v "y" then
	  r:=`Position (get_v "x",get_v "y");
	
	if is_v "r" && is_v "g" && is_v "b" then
	  r:=`Color (get_v "r",get_v "g",get_v "b");
	
	if !r=(`Nil) then (
	  let l=list_of_lua_list val_ext_of_lua tbl in	  
	    if List.length l>0 then
	      r:=`List l
	);
	!r
  
  | _ as x->val_of_lua x
;;


let ext_of_generic v=(v : val_generic :> val_ext);;

(** Ocaml & Lua & XML interface *)

type val_format_t=
  | TValList
  | TValXml
  | TValXmlString
  | TValLua
  | TValLuaString;;

type ('a) val_format=
  | ValList of 'a list
  | ValXml of Xml.xml
  | ValXmlString of string
  | ValLua of lua_obj
  | ValLuaString of string;;


class ['a] val_handler (xmlfrom:'a->Xml.xml) (xmlto:Xml.xml->'a) (luafrom:'a->OLuaVal.value) (luato:OLuaVal.value->'a)=
object(self)
  inherit generic_object


  method from_format (fmt:('a) val_format)=
    match fmt with
      | ValList l ->self#from_list l
      | ValXml x->self#from_xml x
      | ValXmlString x->self#from_xml_string x
      | ValLua l->self#from_lua l
      | ValLuaString s->self#from_lua_string s

  method to_format (fmt_t:val_format_t)=
    match fmt_t with
      | TValList->ValList (self#to_list())
      | TValXml->ValXml (self#to_xml)
      | TValXmlString->ValXmlString (self#to_xml_string)
      | TValLua->ValLua (self#to_lua)
      | TValLuaString->ValLuaString (self#to_lua_string)


(** OCaml part *)
  val mutable vals=DynArray.create()
  method add_val (n:'a) (v:'a)=
    DynArray.add vals (n,v)

  method set_val (n:'a) (v:'a)=
    if self#is_val n then (
      let ni=ref 0 in
	DynArray.iteri (
	  fun i (nn,vv) ->
	    if nn=n then ni:=i;
	) vals;
	DynArray.set vals !ni (n,v)
    )
    else self#add_val n v
  method get_val (n:'a)=
    let ni=ref (-1) in
      DynArray.iteri (
	  fun i (nn,vv) ->
	    if nn=n then ni:=i;
	) vals;
      if !ni<>(-1) then
	(snd (DynArray.get vals !ni))
      else
	raise (Val_not_found (string_of_val n))
  method is_val (n:'a)=
    let r=ref false in
      DynArray.iteri (
	fun i (nn,vv) ->
	  if nn=n then r:=true;
      ) vals;    
      !r
  method foreach_val f=
    let g (n,v)=f n v in
    DynArray.iter g vals


  method merge (vh:('a) val_handler)=
    vh#foreach_val (
      fun k v->
	if self#is_val k=false then
	  self#add_val k v
    )


(*
  val mutable vals=Hashtbl.create 2

  method add_val (n:'a) (v:'a)=
    Hashtbl.add vals n v
  method set_val (n:'a) (v:'a)=
    if self#is_val n then
      Hashtbl.replace vals n v
    else self#add_val n v
  method get_val n=
    (try
       Hashtbl.find vals n 
     with Not_found ->
       raise (Val_not_found (string_of_val n)))
  method is_val n=Hashtbl.mem vals n
  method clear()=vals<-Hashtbl.create 2
  method foreach_val f=
    Hashtbl.iter f vals
    *)


  method from_named_list (l:('a*'a) list)=
    List.iter (
      fun (k,v)->
	self#set_val k v
    ) l;

  method from_list (l:('a) list)=
    let i=ref 0 in
    List.iter (
      fun (v)->
	self#set_val (`Int !i) v;
	i:= !i+1;
    ) l;

  method to_list ()=
    let a=DynArray.create() in
    self#foreach_val (
      fun n v->
	DynArray.add a v
    );
      DynArray.to_list a

(** XML part *)
  method from_xml_string s=
    let x=Xml.parse_string s in
      self#from_xml x

  method from_xml x=
    self#set_id (Xml.tag x);
    let childs=Xml.children x in
    let i=ref 0 in
    List.iter (
      fun c->
	let v=xmlto c in
	  
	let nm=
	  (try
	     (`String (Xml.attrib c "name")) 
	   with Xml.No_attribute v-> i:= !i+1;`Int (!i-1)) in
	  self#set_val (nm) v;


    ) childs;

  method to_xml_string=
    let x=self#to_xml in
      Xml.to_string x
 
  method to_xml=
    let a=DynArray.create() in
      self#foreach_val (
	fun k v->
	  let xr=xmlfrom v in
	  let xv=
	    (match xr with
	       | Element (t,args,childs)->
		   Element (t,
			    (match k with
			      | `String s->List.append [("name",s)] args
			      | _ -> args),

			    childs)
	       | x -> x) in
		(*  DynArray.add a xv *)
	    (match k with
	      | `String s->	      
		  DynArray.add a xv
	      | `Int i->
(*		  print_int i;print_newline(); *)
(*		  print_string (string_of_val v);print_newline(); *)
(*		  DynArray.set a i xv *)
		  DynArray.add a xv
	      | _ -> ())
	  
    );

      Element(self#get_id,[],
	      (DynArray.to_list a)
	     );

(** Lua part *)
  method from_lua_string str =
    let lo=new lua_obj in
      ignore(lo#parse str);
      self#from_lua ( lo);
      

  method from_lua (t:lua_obj)=
    t#update_vals();
    Luahash.iter (
      fun k v->
	let ak=luato k and
		  av=luato v in
	  self#set_val ak av;
    ) t#to_table;

  method to_lua_string=
    let tbl=self#to_lua#to_table in
	  let str=ref "" in
	    str := (!str^self#get_id^"={");
	    Luahash.iter (
	      fun k v->
		let ak=luato k and
		    av=luato v in
		  (match ak with
		     | `String s-> str := (!str^string_of_val ak^"=")		    
		     | _ -> ()
		  );
		  str:= ( !str^
			    (match av with
			       | `String s -> ("\""^s^"\"")
			       | v -> string_of_val v
			    )
			);
		  str:= (!str^";");
	    ) tbl;
	    str := (!str^"}");
	    !str


  method to_lua=
    let lo=new lua_obj in
      self#foreach_val (
	fun k v ->
	  lo#set_val (luafrom k) (luafrom v)
      );
      lo


end;;

class val_generic_handler=
object
  inherit [val_generic] val_handler xml_of_val val_of_xml lua_of_val val_of_lua 
end;;

class val_ext_handler=
object
  inherit [val_ext] val_handler xml_of_val_ext val_ext_of_xml lua_of_val_ext val_ext_of_lua 
end;;

let val_ext_handler_of_format f=
  let nh=new val_ext_handler in
    nh#from_format f;
    nh;;

let val_ext_handler_of_list l=
  let nh=new val_ext_handler in
    nh#from_list l;
    nh;;

let val_ext_handler_of_xml_string l=
  let nh=new val_ext_handler in
    nh#from_xml_string l;
    nh;;


let format_of_val_ext_handler nh ft=
  nh#to_format ft;;


let list_of_val_ext_handler nh=
  nh#to_list();;


(** xml part *)

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
		  print_string ("type "^p#get_type);print_newline();
		  let (nm,h,l)=mt in
		  if Hashtbl.mem h p#get_id then (
		    print_string ("set meta "^p#get_id);print_newline();
		    sp#set_metatype (self#get_mt p#get_id);
		  );
		  sp#parse v;
		  DynArray.add objs sp#get_val
		)
      | _ ->()
	  
	  
  method init (add_obj:string->'t->unit)=
    let (hnm,h,hl)=mt in
    Hashtbl.iter (
      fun k v->
	print_string ("meta "^k);print_newline();

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
	  print_string ("add "^nm^" from meta");print_newline();
	  if self#parser_is nm then (
	    print_string (nm^" exists!");print_newline();
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
	print_string ("meta "^k);print_newline();

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
	  print_string ("add "^nm^" from meta");print_newline();
	  if self#parser_is nm then (
	    print_string (nm^" exists!");print_newline();
	    let sp=(self#parser_get nm)() in
	    let no=(sp#get_val_from_meta v)() in
	      no#lua_init();
	      add_obj k no
	  )
	)

    ) h;
    


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
