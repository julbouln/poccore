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


(* XML : BFR only parser *)

class xml_vcolor_parser=
object(self)
  inherit [color] xml_list_parser "color" (fun()->new xml_color_parser) as list
  inherit xml_color_parser as vcolor

  method get_colors=
    self#get_array

  method parse_attr k v=vcolor#parse_attr k v
  method parse_child k v=list#parse_child k v

end;;

class xml_unit_color_parser=
object(self)
  inherit xml_parser

  val mutable vcolors=DynArray.create()    

  method tag=""
  method get_vcolors=
    DynArray.to_list vcolors

  method parse_child k v=
       match k with
	 | "vcolor" -> let p=new xml_vcolor_parser in p#parse v;DynArray.add vcolors (p#get_color,p#get_colors);
	 | _ ->();
  method parse_attr k v=()

end;;

type resource=
{
 rfile:string;
(* decal value *)
 rdw:int;
 rdh:int;
(* pixel *)
 rw:int;
 rh:int;
 rframes:int list;
 rrefresh:int;
 rcw:int;
 rch:int;
};;

type gm_object=
{
 ofile:string;
(* decal value *)
 odw:int;
 odh:int;
(* pixel *)
 ow:int;
 oh:int;
 oframes:int list;
 orefresh:int;
 ocw:int;
 och:int;
};;



class xml_gm_object_parser=
object
  inherit xml_parser

  val mutable file=""
  val mutable w=0
  val mutable h=0
  val mutable dw=0
  val mutable dh=0
  val mutable cw=0
  val mutable ch=0
  val mutable frames=[]
  val mutable refresh=0

  method get_val={ofile=file;odw=dw;odh=dh;ow=w;oh=h;oframes=frames;orefresh=refresh;ocw=cw;och=ch}

  method tag=""
  method parse_attr k v=()
  method parse_child k v=
    match k with
      | "file" -> let p=(new xml_string_parser "path") in p#parse v;file<-p#get_val    
      | "pixel_size" -> let p=(new xml_size_parser ) in p#parse v;w<-p#get_w;h<-p#get_h;
      | "case_size" -> let p=(new xml_size_parser ) in p#parse v;cw<-p#get_w;ch<-p#get_h;
      | "decal_value" -> let p=(new xml_size_parser ) in p#parse v;dw<-p#get_w;dh<-p#get_h;
      | "frames" -> let p=(new xml_intlist_parser "frame" (fun()->new xml_int_parser "frame"))  in p#parse v;frames<-p#get_list
      | "refresh" -> let p=(new xml_int_parser "value") in p#parse v;refresh<-p#get_val
      | _ -> ()

end;;

class xml_gm_objects_parser name=
object
  inherit [gm_object] xml_list_parser name (fun()->new xml_gm_object_parser)
end;;


class xml_decors_parser=
object
  inherit xml_gm_objects_parser "decor"
end;;

class xml_resource_parser=
object
  inherit xml_parser

  val mutable file=""
  val mutable w=0
  val mutable h=0
  val mutable dw=0
  val mutable dh=0
  val mutable cw=0
  val mutable ch=0
  val mutable frames=[]
  val mutable refresh=0

  method get_val={rfile=file;rdw=dw;rdh=dh;rw=w;rh=h;rframes=frames;rrefresh=refresh;rcw=cw;rch=ch}

  method tag=""
  method parse_attr k v=()
  method parse_child k v=
    match k with
      | "file" -> let p=(new xml_string_parser "path") in p#parse v;file<-p#get_val    
      | "pixel_size" -> let p=(new xml_size_parser ) in p#parse v;w<-p#get_w;h<-p#get_h;
      | "case_size" -> let p=(new xml_size_parser ) in p#parse v;cw<-p#get_w;ch<-p#get_h;
      | "decal_value" -> let p=(new xml_size_parser ) in p#parse v;dw<-p#get_w;dh<-p#get_h;
      | "frames" -> let p=(new xml_intlist_parser "frame" (fun()->new xml_int_parser "frame"))  in p#parse v;frames<-p#get_list
      | "refresh" -> let p=(new xml_int_parser "value") in p#parse v;refresh<-p#get_val
      | _ -> ()

end;;

class xml_resources_parser=
object
  inherit [resource] xml_list_parser "resource" (fun()->new xml_resource_parser)
end;;



(* TO FINISH *)
class xml_char_parser=
object
  inherit xml_string_parser "name"

  val mutable file=""
  val mutable icon=""
  val mutable face=""
  val mutable spe=""
  val mutable pw=0
  val mutable ph=0
  val mutable cw=0
  val mutable ch=0
  val mutable dx=0
  val mutable dy=0

  val mutable life=0
  val mutable speed=0
  val mutable view=0
  val mutable snds=[]


  method parse_child k v=
    match k with
      | "file" -> let p=(new xml_string_parser "path") in p#parse v;file<-p#get_val    
      | "icon" -> let p=(new xml_string_parser "path") in p#parse v;icon<-p#get_val    
      | "face" -> let p=(new xml_string_parser "path") in p#parse v;face<-p#get_val    
      | "spe" -> let p=(new xml_string_parser "path") in p#parse v;spe<-p#get_val    
      | "pixel_size" -> let p=(new xml_size_parser ) in p#parse v;pw<-p#get_w;ph<-p#get_h
      | "case_size" -> let p=(new xml_size_parser ) in p#parse v;cw<-p#get_w;ch<-p#get_h
      | "decal_value" -> let p=(new xml_point_parser ) in p#parse v;dx<-p#get_x;dy<-p#get_y
      | "life" -> let p=(new xml_int_parser "value") in p#parse v;life<-p#get_val    
      | "speed" -> let p=(new xml_int_parser "value") in p#parse v;speed<-p#get_val 
      | "view" -> let p=(new xml_int_parser "dist") in p#parse v;view<-p#get_val    
      | "select_sounds" ->let p=(new xml_stringlist_parser "path" (fun()->new xml_string_parser "path"))  in p#parse v;snds<-p#get_list
      | _ -> ()
end;;

(*
let test1=new xml_node (Xml.parse_string "<frames><frame n=\"2\"/></frames>") in
let p=new xml_intlist_parser "frame" (fun()->new xml_int_parser "n")  in p#parse test1;;




let test=new xml_node (Xml.parse_file "defs/uni/zork.uni") in
let p=new xml_char_parser in p#parse test;;
*)

type state={frames:int array;refresh:int;action:unit->unit;sound:string array};;

type state_anim=
    {mutable state_name:string;
     mutable anim_frames:int array;
     mutable anim_refresh:int;
     mutable sounds:string array
   };;

let state_anim2state s=
  {frames=s.anim_frames;
   refresh=s.anim_refresh;
   action=(function()->());
   sound=s.sounds
};;


exception Container_state_not_found of string;;

(* FIXME must be game_state_container *)

class game_state_container (states:state_anim array)=
object
  val mutable sts=Hashtbl.create 2;
initializer


  Array.iter (function s->
(*		print_string ("add state "^s.state_name);print_newline(); *)
		Hashtbl.add sts s.state_name (state_anim2state s);
	     ) states;

method get_state n=
  if Hashtbl.mem sts n then
    Hashtbl.find sts n
  else raise (Container_state_not_found n)

end;;

let rec tech_tree_foreach h x=
  let i=ref 0 in
  let childs=Xml.children x in
  let a=Array.make 30 (0,"none") in
  List.iter (fun c->(
    a.(!i)<-(
      match Xml.tag c with
      |"building" ->tech_tree_foreach h c;(0,(Xml.attrib c "type"))
      |"resource" ->Hashtbl.add h (Xml.attrib c "type") [||];(1,(Xml.attrib c "type"))
      |"char" ->Hashtbl.add h (Xml.attrib c "type") [||]; (2,(Xml.attrib c "type"))
      |_ ->(0,"none");
     );i:=!i+1)) childs;
  let na=Array.sub a 0 (!i) in
(*  print_string ("XML:add:"^(Xml.tag x)^":"^(if Xml.tag x<>"tech_tree" then Xml.attrib x "type" else ""));print_newline();*)
  Hashtbl.add h (if Xml.tag x<>"tech_tree" then (Xml.attrib x "type") else "none") na;;


let tech_tree_parse file=
  let x = Xml.parse_file file in
  let h=Hashtbl.create 2 in 
  tech_tree_foreach h x;
  h;;
(*
type xml_state= {
    mutable state:string;
    mutable frames:int array;
    mutable refresh:int;
    mutable snds:string array;
  };;
*)


type xml_proj={
mutable pname:string;
mutable pfile:string;
    mutable p_w:int;
    mutable p_h:int;
mutable psounds:string array;
mutable pframes:int array;
mutable prefresh:int;
}


type xml_unit={
    mutable team:string;
    mutable name:string;
    mutable file:string;
    mutable icon:string;
    mutable face:string;
    mutable spe:string;
    mutable psize_w:int;
    mutable psize_h:int;
    mutable dec_x:int;
    mutable dec_y:int;
    mutable csize_w:int;
    mutable csize_h:int;
    mutable life:int;
    mutable speed:int;
    mutable view:int;
    mutable attack1:int;
    mutable attack2:int;
    mutable attack2_dist:int;
    mutable destruct_f:int array;
    mutable select_sounds:string array;
    mutable states:state_anim array;
    mutable projectile:xml_proj;
  };;

let frames_foreach x=
  let a=DynArray.create() in
  let childs=Xml.children x in
  List.iter (fun c->(
    DynArray.add a (int_of_string(Xml.attrib c "n"))
   ))  childs;
     DynArray.to_array a
;;

let sounds_foreach x=
  let a=DynArray.create() in

  let childs=Xml.children x in
  List.iter (fun c->(
    DynArray.add a (Xml.attrib c "path")
   ))  childs;
(*    if DynArray.length a=0 then
      DynArray.add a "none";
*)
     DynArray.to_array a
;;

let state_foreach x=
  let childs=Xml.children x in
  let s={state_name="none";anim_frames=[|0|];anim_refresh=0;sounds=[|"none"|]} in
  s.state_name<-(Xml.attrib x "name");
  List.iter (fun c->(
      match Xml.tag c with
      |"frames" ->s.anim_frames<-(frames_foreach c)
      |"refresh" ->s.anim_refresh<-int_of_string(Xml.attrib c "value")
      |"sounds" ->s.sounds<-(sounds_foreach c)
      |_ ->();
   ))  childs;
  s
;;

let states_foreach x=
  let a=DynArray.create() in
  let childs=Xml.children x in
  List.iter (fun c->(
    DynArray.add a (state_foreach c)
   )) childs;
  DynArray.to_array a
;;


let proj_foreach x=
  let childs=Xml.children x in

  let s={pname="none";pfile="none";p_w=0;p_h=0;psounds=[|"none"|];pframes=[|0|];prefresh=0} in
  s.pname<-(Xml.attrib x "name");
  List.iter (fun c->(
      match Xml.tag c with
      |"file" ->s.pfile<-(Xml.attrib c "path")
      |"frames" ->s.pframes<-(frames_foreach c)
      |"refresh" ->s.prefresh<-int_of_string(Xml.attrib c "value")
      |"sounds" ->s.psounds<-(sounds_foreach c)
      |"pixel_size" ->s.p_w<-int_of_string(Xml.attrib c "w");s.p_h<-int_of_string(Xml.attrib c "h")
      |_ ->();
   ))  childs;
  s
;;


let unit_parse file=
  let x = Xml.parse_file file in
(*
  let dtd = Dtd.parse_file "defs/uni.dtd" in
  let x = Dtd.prove (Dtd.check dtd) "char" x in
*)

  let childs=Xml.children x in
  let u={team=(Xml.attrib x "team");name=(Xml.attrib x "name");file="none";icon="none";face="none";spe="none";psize_w=0;psize_h=0;dec_x=0;dec_y=0;csize_w=0; csize_h=0; life=0; speed=0; view=0;attack1=0; attack2=0;attack2_dist=0;destruct_f=[|0|];select_sounds=[|"none"|];states=[||];projectile={pname="none";pfile="none";p_w=0;p_h=0;psounds=[|"none"|];pframes=[|0|];prefresh=0}} in
  List.iter (fun c->(
      match Xml.tag c with
      |"file" ->u.file<-(Xml.attrib c "path")
      |"icon" ->u.icon<-(Xml.attrib c "path")
      |"face" ->u.face<-(Xml.attrib c "path")
      |"spe" ->u.spe<-(Xml.attrib c "path")
      |"pixel_size" ->u.psize_w<-int_of_string(Xml.attrib c "w");u.psize_h<-int_of_string(Xml.attrib c "h")
      |"decal_value" ->u.dec_x<-int_of_string(Xml.attrib c "x");u.dec_y<-int_of_string(Xml.attrib c "y")
      |"case_size" ->u.csize_w<-int_of_string(Xml.attrib c "w");u.csize_h<-int_of_string(Xml.attrib c "h")
      |"life" ->u.life<-int_of_string(Xml.attrib c "value");
      |"speed" ->u.speed<-int_of_string(Xml.attrib c "value");
      |"view" ->u.view<-int_of_string(Xml.attrib c "dist");
      |"attack1" ->u.attack1<-int_of_string(Xml.attrib c "power");
      |"attack2" ->u.attack2<-int_of_string(Xml.attrib c "power");u.attack2_dist<-int_of_string(Xml.attrib c "dist")
      |"destruct_frames" ->u.destruct_f<-(frames_foreach c)
      |"select_sounds" ->u.select_sounds<-(sounds_foreach c);
      |"states" ->u.states<-(states_foreach c);
      |"projectile" ->u.projectile<-(proj_foreach c);
      |_ ->();
   ))  childs;
  u;;


let frames_create f=
  let fs=ref [] in
    for i=0 to (Array.length f - 1) do
      fs:=List.append !fs [Element ("frame",[("n",string_of_int f.(i))],[])];
    done;
  let x=Element ("frames",[],!fs) in
    x

let sounds_create s=
  let snds=ref [] in
    for i=0 to (Array.length s - 1) do
      snds:=List.append !snds [Element ("sound",[("path",s.(i))],[])];
    done;
  let x=Element ("sounds",[],!snds) in
    x

let states_create st=
  let sts=ref [] in
    for i=0 to (Array.length st - 1) do
      sts:=List.append !sts [Element ("state",[("name",st.(i).state_name)],
				[frames_create st.(i).anim_frames;
				 sounds_create st.(i).sounds;
				 Element ("refresh",[("value",string_of_int st.(i).anim_refresh)],[])
				])];
    done;
  let x=Element ("states",[],!sts) in
    x

let unit_create u=
  let x=
    Element ("char",[("name",u.name)],[
       Element ("file",[("path",u.file)],[]);
       Element ("icon",[("path",u.icon)],[]);
       Element ("face",[("path",u.face)],[]);
       Element ("spe",[("path",u.spe)],[]);
       Element ("pixel_size",[("w",string_of_int u.psize_w);("h",string_of_int u.psize_h)],[]);
       Element ("case_size",[("w",string_of_int u.csize_w);("h",string_of_int u.csize_h)],[]);
       Element ("decal_value",[("x",string_of_int u.dec_x);("y",string_of_int u.dec_y)],[]);
       Element ("life",[("value",string_of_int u.life)],[]);
       Element ("speed",[("value",string_of_int u.speed)],[]);
       Element ("view",[("dist",string_of_int u.view)],[]);
       Element ("attack1",[("power",string_of_int u.attack1)],[]);
       Element ("attack2",[("power",string_of_int u.attack2);("dist",string_of_int u.attack2_dist)],[]
);
       states_create u.states
     ]) in
    print_string (to_string_fmt x);print_newline();

;;
