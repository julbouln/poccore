(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003,2004 POC 

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

open Unix;;
open Xml;;
open XmlParser;;

open Low;;

open Medias;;
(** Xml parse in an object way *)

exception Bad_xml_node;;

let xml_reduce n (f:Xml.xml->bool)=
  let childd=DynArray.create() in
  List.iter (
    fun x->(
      match x with
	| Element v -> if (f x) then DynArray.add childd x
	| _ ->()
      )
  ) (Xml.children n);
    match n with
      | Element (t,attrs,childs)->Element (t,attrs,DynArray.to_list childd)
      | _ -> raise Bad_xml_node ;;
    

exception Xml_node_attr_not_found of string;;
exception Xml_node_child_not_found of string;;

class xml_node (n:Xml.xml)=
object(self)
  val mutable node=n
  val mutable tag="none"
  val mutable pcdata=""
  val mutable attrs=Hashtbl.create 2
  val mutable children=Hashtbl.create 2

  method set_tag t=tag<-t
  method get_tag=tag

  method set_pcdata t=pcdata<-t
  method get_pcdata=pcdata

  method add_attr k v=Hashtbl.add attrs k v
  method get_attr k=
    (try 
       Hashtbl.find attrs k
     with Not_found -> raise (Xml_node_attr_not_found k))
  method foreach_attr f=Hashtbl.iter f attrs

  method add_child k v=Hashtbl.add children k v
  method get_child k=
    (try 
       Hashtbl.find children k
     with Not_found -> raise (Xml_node_child_not_found k))
  method foreach_child f=Hashtbl.iter f children

  initializer
    (* set tag *)
    self#set_tag (Xml.tag node);

    (* add attributes *)
    List.iter (
      fun (k,v)->(
	self#add_attr k v)
    ) (Xml.attribs node);
    
    (* add children *)
    List.iter (
      fun x->(
	match x with
	  | Element v -> self#add_child (Xml.tag x) (new xml_node x)
	  | PCData v -> self#set_pcdata v
      )
    ) (Xml.children node)
      

  method set_node n=node<-n
  method get_node=node

end;;



class virtual xml_parser=
object(self)
  method tag=""
  method virtual parse_attr: string->string->unit
  method virtual parse_child: string->xml_node->unit

  method parse (n:xml_node)= 
    n#foreach_attr self#parse_attr;
    n#foreach_child self#parse_child;
end;;



(* XML : General parsers *)



(* XML : int parser of form <tag a="int"> *)
class xml_int_parser a=
object
  inherit xml_parser

  val mutable n=0
(*  method get_int=n *)
  method get_val=n

  method parse_attr k v=
    match k with
      | r when r=a -> n<-int_of_string v
      | _ ->()
  method parse_child k v=()


end;;

(* XML : string parser of form <tag a="string"> *)
class xml_string_parser a=
object
  inherit xml_parser

  val mutable n=""
  method get_val=n

  method parse_attr k v=
    match k with
      | r when r=a -> n<-v
      | _ ->()
  method parse_child k v=()


end;;


(* XML : point parser of form <tag x="int" y="int"> *)
class xml_point_parser=
object
  inherit xml_parser

  val mutable x=0
  val mutable y=0
  method get_x=x
  method get_y=y

  method parse_attr k v=
    match k with
      | "x" -> x<-int_of_string v
      | "y" -> y<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;

(* XML : size parser of form <tag w="int" h="int"> *)
class xml_size_parser=
object
  inherit xml_parser

  val mutable w=0
  val mutable h=0
  method get_w=w
  method get_h=h

  method parse_attr k v=
    match k with
      | "w" -> w<-int_of_string v
      | "h" -> h<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;


(* XML : size parser of form <tag r="int" g="int" b="int"> *)
class xml_color_parser=
object
  inherit xml_parser

  val mutable r=0
  val mutable g=0
  val mutable b=0

  method get_val=(r,g,b)
  method get_color=(r,g,b)
  method get_r=r
  method get_g=g
  method get_b=b

  method parse_attr k v=
(*    print_string "add color";print_newline(); *)
    match k with
      | "r" -> r<-int_of_string v
      | "g" -> g<-int_of_string v
      | "b" -> b<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;

(* XML : list parser  *)
class ['a,'b] xml_list_parser ct (pc:unit->'b) =
object
  inherit xml_parser
  val mutable parser_func=pc
  method set_parser_func (npc:unit->'b)=parser_func<-npc

  val mutable frms=DynArray.create()
  method get_list=(DynArray.to_list frms : 'a list)
  method get_array=(DynArray.to_array frms : 'a array)
(*  method get_val n=(DynArray.get frms n : 'a) *)

  method parse_attr k v=()
  method parse_child k v=
    match k with
      | r when r=ct -> let p=parser_func() in p#parse v;DynArray.add frms p#get_val
      | _ -> ()
	      
end;;

class xml_intlist_parser ct pc=
object
  inherit [int,xml_int_parser] xml_list_parser ct pc
end;;

class xml_stringlist_parser ct pc=
object
  inherit [string,xml_string_parser] xml_list_parser ct pc
end;;



class ['k,'v] xml_hash_parser ct pc =
object
  inherit xml_parser

  val mutable frms=Hashtbl.create 2
(*  method get_val n=(DynArray.get frms n : 'a) *)
  method get_hash=frms
  method tag=""
  method parse_attr k v=()
  method parse_child k v=
    match k with
      | r when r=ct -> let p=pc() in p#parse v;
	  let r=p#get_val in
	  Hashtbl.add frms (fst r:'k) (snd r:'v)
      | _ -> ()
end;;

class ['v] xml_stringhash_parser ct pc=
object
  inherit [string,'v] xml_hash_parser ct pc
end;;


(** xml font parser : <font path="fontfile" size="sizeoffont"/> *)
class xml_font_parser=
object
  inherit xml_parser

  val mutable file="none"
  val mutable size=0

  method get_val=new font_object file size

  method parse_attr k v=
    match k with
      | "path" -> file<-v
      | "size" -> size<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;


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


(** v_color parser stuff *)
(* 8< *) 

class xml_v_color_parser=
object(self)
  inherit [color,xml_color_parser] xml_list_parser "color" (fun()->new xml_color_parser) as list
  inherit xml_color_parser as vcolor

  method get_colors=
    self#get_array

  method parse_attr k v=vcolor#parse_attr k v
  method parse_child k v=list#parse_child k v

end;;

class xml_v_colors_parser=
object(self)
  inherit xml_parser

  val mutable vcolors=DynArray.create()    

  method get_vcolors=
    DynArray.to_list vcolors

  method parse_child k v=
       match k with
	 | "vcolor" -> let p=new xml_v_color_parser in p#parse v;DynArray.add vcolors (p#get_color,p#get_colors);
	 | _ ->();
  method parse_attr k v=()

end;;


let v_color_from_xml f=
  print_string ("XML: load "^f);print_newline();
  let colfile=new xml_node (Xml.parse_file f) in
  let colparser=new xml_v_colors_parser in    
    colparser#parse colfile;
  let uc=new unit_color in
    List.iter (
      fun v->(	
	uc#add_vcolor (fst v) (snd v) 
      )
    )
      colparser#get_vcolors;
  uc



