open Value_lua;;
open Value_xml;;
open Value_val;;

open Core_timer;;

(** Extended val type *)

type val_ext=
    [
      val_generic
    | `Position of (int*int)
    | `Size of (int*int)
    | `Color of (int*int*int)
    | `Time of time
    | `List of val_ext list 
(*    | `Function of val_ext list-> val_ext list *)
    ]
;;

let size_of_val=function
  | `Size v->v
  | _->raise (Bad_val_type "size");;

let position_of_val=function
  | `Position v->v
  | _->raise (Bad_val_type "position");;


let color_of_val=function
  | `Color v->v
  | _->raise (Bad_val_type "color");;

let time_of_val=function
  | `Time v->v
  | _->raise (Bad_val_type "time");;

let list_of_val=function
  | `List v->v
  | _->raise (Bad_val_type "list");;

let rec xml_of_val_ext v=
  let ron=ref (new xml_node) in
  let on= !ron in
    (match v with
       | #val_generic as v-> ron:=xml_of_val v
       | `Position (x,y)->
	   on#of_list 
	     [
	       Tag "val_position";
	       Attribute ("x",string_of_int x);
	       Attribute ("y",string_of_int y)
	     ]
       | `Size (w,h)->
	   on#of_list 
	     [
	       Tag "val_size";
	       Attribute ("w",string_of_int w);
	       Attribute ("h",string_of_int h)
	     ]
       | `Color (r,g,b)->
	   on#of_list 
	     [
	       Tag "val_color";
	       Attribute ("r",string_of_int r);
	       Attribute ("g",string_of_int g);
	       Attribute ("b",string_of_int b);
	     ]
       | `Time t->
	   on#of_list 
	     [
	       Tag "val_time";
	       Attribute ("h",string_of_int t.h);
	       Attribute ("m",string_of_int t.m);
	       Attribute ("s",string_of_int t.s);
	       Attribute ("f",string_of_int t.f);
	     ]
	     
       | `List vl->
	   on#of_list 
	     ([Tag "val_list"]@(List.map (fun v->(xml_of_val_ext v)#to_node) vl))
    );
    on
;; 

let rec val_ext_of_xml x=
  match x#tag with 
  | "val_position"-> 
      `Position (
	(int_of_string (x#attrib "x")),
	(int_of_string (x#attrib "y"))
      )
  | "val_size"-> 
      `Size (
	(int_of_string (x#attrib "w")),
	(int_of_string (x#attrib "h"))
      )
  | "val_color"->
      `Color (
	(int_of_string (x#attrib "r")),
	(int_of_string (x#attrib "g")),
	(int_of_string (x#attrib "b"))
      )
  | "val_time"->
      `Time {
	h=(int_of_string (x#attrib "h"));
	m=(int_of_string (x#attrib "m"));
	s=(int_of_string (x#attrib "s"));
	f=(int_of_string (x#attrib "f"));
      }
  | "val_list" ->`List (List.map (fun cn->val_ext_of_xml cn) x#children)
  | _ -> val_of_xml x
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

(* doesnt work *)
let list_of_lua_list valto tbl=
  let a=DynArray.create() in
    Luahash.iter (
      fun k v ->
	match k with
(*	  | OLuaVal.Number i->DynArray.set a (int_of_float i) (valto v) *)
	  | OLuaVal.Number i->DynArray.add a (valto v) 
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
  | `Time t->
      OLuaVal.Table (lua_table_of_list [
	("h",OLuaVal.Number (float t.h));
	("m",OLuaVal.Number (float t.m));
	("s",OLuaVal.Number (float t.s));
	("f",OLuaVal.Number (float t.f));
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

	if is_v "h" && is_v "m" && is_v "s" && is_v "f" then
	  r:=`Time {h=get_v "h";m=get_v "m";s=get_v "s";f=get_v "f"};
	
	if !r=(`Nil) then (
	  let l=list_of_lua_list val_ext_of_lua tbl in	  
	    if List.length l>0 then
	      r:=`List l
	);
	!r
  
  | _ as x->val_of_lua x
;;


let ext_of_generic v=(v : val_generic :> val_ext);;

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


