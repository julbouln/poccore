open Oxml;;
open Olua;;

type prop=
  | PropInt of int
  | PropFloat of float
  | PropString of string
  | PropBool of bool
  | PropList of prop list
  | PropLua of string * string
  | PropNil;;


exception Bad_prop of string

class properties=
object
  val mutable lo=new lua_object

  val mutable props=Hashtbl.create 2
  method add_prop (n:string) (p:prop)=Hashtbl.add props n p
  method set_prop n p=Hashtbl.replace props n p
  method del_prop n=Hashtbl.remove props n


  method lua_register (m:string) (interp:lua_interp)=
    lo#set_mod m;
    Hashtbl.iter (fun n v->
		    interp#parse (
		      match v with
			| PropFloat f->(m^"."^n^"="^string_of_float f)
			| PropInt i->(m^"."^n^"="^string_of_int i)
			| PropString s->(m^"."^n^"='"^s^"'")
			| PropBool b->(m^"."^n^"="^(if b then "true" else "false"))
			| PropLua (a,c) -> lo#add_function n a c;""
			| _ -> ""
		    );()
		 ) props;
    interp#parse_object lo;()

(*
  method from_db : ?
  method from_xml f : string -> unit
  method to_xml_message : unit ->string
  method to_lua : unit->string
*)

end;;

class xml_prop_parser=
object
  inherit xml_parser as super
  val mutable t=""
  val mutable nm=""
  val mutable v=""
  val mutable args=""

  method get_val=
    match t with
      | "int" -> (nm,PropInt (int_of_string v))
      | "float" -> (nm,PropFloat (float_of_string v))
      | "string" -> (nm,PropString v)
      | "lua" -> (nm,PropLua (args,v))
      | _ -> (nm,PropNil)

  method parse_attr k v=
    match k with
      | "name" -> nm<-v
      | "args" -> args<-v
      | _ -> ()

  method parse_child k v=()  
  method parse (n:xml_node)=
    super#parse n;
    t<-n#get_tag;
    v<-n#get_pcdata;
   
end;;

class xml_prop_list_parser=
object
  inherit [string * prop,xml_prop_parser] xml_list_parser "" (fun()->new xml_prop_parser)
  method parse_child k v=
    match k with
      | _ -> let p=new xml_prop_parser in p#parse v;DynArray.add frms p#get_val
end;;

