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

(** Xml parse in an object way *)


class xml_node n=
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
  method get_attr k=Hashtbl.find attrs k
  method foreach_attr f=Hashtbl.iter f attrs

  method add_child k v=Hashtbl.add children k v
  method get_child k=Hashtbl.find children k
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
  method virtual tag:string
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
  method get_int=n
  method get_val=n

  method tag=""
  method parse_attr k v=
    match k with
      | a -> n<-int_of_string v
  method parse_child k v=()


end;;

(* XML : string parser of form <tag a="string"> *)
class xml_string_parser a=
object
  inherit xml_parser

  val mutable n=""
  method get_val=n

  method tag=""
  method parse_attr k v=
    match k with
      | a -> n<-v
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

  method tag=""
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

  method tag=""
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

  method tag=""
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
class ['a] xml_list_parser ct pc =
object
  inherit xml_parser

  val mutable frms=DynArray.create()
  method get_list=(DynArray.to_list frms : 'a list)
  method get_array=(DynArray.to_array frms : 'a array)
(*  method get_val n=(DynArray.get frms n : 'a) *)

  method tag=""
  method parse_attr k v=()
  method parse_child k v=
    match k with
      | ct -> let p=pc() in p#parse v;DynArray.add frms p#get_val

end;;

class xml_intlist_parser ct pc=
object
  inherit [int] xml_list_parser ct pc
end;;

class xml_stringlist_parser ct pc=
object
  inherit [string] xml_list_parser ct pc
end;;


