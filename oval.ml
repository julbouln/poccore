open Generic;;
open Xml;;
open Olua;;

(** Val conversion between OCaml, XML and Lua *)

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
(*    | `List of val_ext list *)
    ]
;;

let xml_of_val_ext=function
  | #val_generic as v->xml_of_val v
  | `Position (x,y)->Xml.Element("val_position",[("x",string_of_int x);("y",string_of_int y)],[])
  | `Size (w,h)->Xml.Element("val_size",[("w",string_of_int w);("h",string_of_int h)],[])
  | `Color (r,g,b)->Xml.Element("val_color",[("r",string_of_int r);("g",string_of_int g);("b",string_of_int b)],[]);;
(*  | `List vl->Xml.Element("val_list",[],List.map (fun v->xml_of_val_ext v) vl);; *)

let val_ext_of_xml=function
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

let lua_of_val_ext=function
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
;; 

let val_ext_of_lua=function
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
	!r

  | _ as x->val_of_lua x
;;


let ext_of_generic v=(v : val_generic :> val_ext);;

(** Ocaml & Lua & XML interface *)

class ['a] val_handler (xmlfrom:'a->Xml.xml) (xmlto:Xml.xml->'a) (luafrom:'a->OLuaVal.value) (luato:OLuaVal.value->'a)=
object(self)
  inherit generic_object

(** OCaml part *)
  val mutable vals=Hashtbl.create 2

  method add_val (n:'a) (v:'a)=Hashtbl.add vals n v
  method set_val (n:'a) (v:'a)=
    if self#is_val n then
      Hashtbl.replace vals n v
    else self#add_val n v
  method get_val n=
    (try
       Hashtbl.find vals n 
     with Not_found ->raise (Val_not_found (string_of_val n)))
  method is_val n=Hashtbl.mem vals n

  method clear()=vals<-Hashtbl.create 2

  method from_list (l:('a*'a) list)=
    List.iter (
      fun (k,v)->
	self#set_val k v
    ) l;


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
	   with Xml.No_attribute v-> `Int !i) in
	  self#set_val (nm) v;
	  i:= !i+1;

    ) childs;

  method to_xml_string=
    let x=self#to_xml in
      Xml.to_string x
 
  method to_xml=
    let a=DynArray.create() in
    Hashtbl.iter (
      fun k v->
	let xr=xmlfrom v in
	  DynArray.add a
	    (match xr with
	       | Element (t,args,childs)->Element (t,List.append [("name",string_of_val k)] args,childs)
	       | x -> x);

    ) vals;

      Element(id,[],
	      List.rev (DynArray.to_list a)
	     );

(** Lua part *)
  method from_lua_string (interp:lua_interp) str =
    self#from_lua (List.nth (interp#parse str) 0)

  method from_lua (t:OLuaVal.value)=
    match t with
      | OLuaVal.Table tbl ->
	  Luahash.iter (
	    fun k v->
	      let ak=luato k and
		  av=luato v in
		self#set_val ak av;
	  ) tbl;
      | _ ->  ()   

  method to_lua_string=
    let tbl=self#to_lua in
    let str=ref "" in
      str := (!str^id^"={");
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


  method to_lua_interp (interp:lua_interp)=
    interp#set_global_val id (OLuaVal.Table (self#to_lua))

  method to_lua=
    let tbl=Luahash.create (fun a b->a=b) 2 in
      Hashtbl.iter (
	fun k v ->
	  Luahash.replace tbl ~key:(luafrom k) ~data:(luafrom v)
      ) vals;
      tbl


end;;

class val_generic_handler=
object
  inherit [val_generic] val_handler xml_of_val val_of_xml lua_of_val val_of_lua 
end;;

class val_ext_handler=
object
  inherit [val_ext] val_handler xml_of_val_ext val_ext_of_xml lua_of_val_ext val_ext_of_lua 
end;;

