(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003,2004,2005 POC 

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

open Generic;;
open Rect;;
open Video;;

open Font;;
open Drawing;;

open Binding;;

open Oxml;;

let medias_dir=(Filename.dirname(Sys.executable_name));;

(** Media objects *)

Random.self_init();;

(** {2 General function (FIXME: must fall in another file)} *)

(** get random number *)
let randomize n= 
 (Random.int n)

let carree x=float_of_int(x*x);;
let racine x=(sqrt x);;

(** Generate a random string of size s with prefix p *)
let random_string p s=
  let str=String.create s in
  String.blit (p^"/_") 0 str 0 5;
  for i=5 to s-1 do
    let r=Char.chr ((randomize 25)+65) in
    String.set str i r;
  done;
  str;;




(** {2 Graphic part} *)

class virtual canvas_object=
object
  inherit generic_object as super
  val mutable layer=0
  method set_layer l=layer<-l
  method get_layer=layer

  val mutable rect=new rectangle 0 0 0 0
  method get_rect=rect

  method virtual move : int -> int -> unit
  method virtual put : unit -> unit


  method print_info()=
    super#print_info();
    print_string (" * position: "^string_of_int rect#get_x^" - "^string_of_int rect#get_y);print_newline();
    print_string (" * size: "^string_of_int rect#get_w^" - "^string_of_int rect#get_h);print_newline();
    print_string (" * layer: "^string_of_int layer);print_newline();
end;;



class canvas_NEW =
object(self)
  val mutable objs_list=RefList.empty()
  val mutable tile_list=RefList.empty()
  val mutable del_list=RefList.empty()

  method clear()=
    objs_list<-RefList.empty();
    tile_list<-RefList.empty();
    del_list<-RefList.empty();

  method foreach_sorted f=
    self#foreach_obj f;

(** sort tiles to put from layer *)
  method sort_layer()=
    self#sort_obj  
       (fun ao bo ->
	    match ao#get_layer with
	      | x when x < bo#get_layer -> -1
	      | x when x = bo#get_layer -> 0
	      | x when x > bo#get_layer -> 1
	      | _ -> 0
      );


  method add_obj (o:canvas_object)=    
(*    o#set_graphic(); *)
    RefList.add objs_list o;


  method del_obj (od:canvas_object)=
  RefList.filter
    ( fun o->
	if o#get_rect#get_x=od#get_rect#get_x 
	  && o#get_rect#get_y=od#get_rect#get_y
	  && o#get_id=od#get_id
	then false else true
    )
    objs_list

(*
  method del_dead ()=
    RefList.filter 
      ( fun o->
	  if o#will_i_dead=false then true else false	     	    
      )
      objs_list
*) 

  method sort_obj (f:canvas_object->canvas_object->int)=
    RefList.sort ~cmp:f objs_list;

  method foreach_obj (f:canvas_object->unit)=
      RefList.iter f objs_list


(** sort tiles to put from position *)

  method sort_position()=
    self#sort_obj 
      ( fun ao bo ->
	  let ax=ao#get_rect#get_x and
	      ay=ao#get_rect#get_y and
	      bx=bo#get_rect#get_x and
	      by=bo#get_rect#get_y and
	      aw=ao#get_rect#get_w and
	      ah=ao#get_rect#get_h and
	      bw=bo#get_rect#get_w and
	      bh=bo#get_rect#get_h in
	    
	    match ay with
	      | y when y+ah<by+bh -> -1
	      | y when y+ah=by+bh -> 0
	      | y when y+ah>by+bh -> 1
	      | _ -> 0
      );

  (** refresh the canvas. Refresh graphic part of each object *)
 method refresh (vx:int) (vy:int) (tw:int) (th:int)=

   self#sort_position();
   self#sort_layer();
   self#foreach_obj (
      fun o->
	let ox=o#get_rect#get_x and
	    oy=o#get_rect#get_y and
	    ow=o#get_rect#get_w and
	    oh=o#get_rect#get_h in
	  
	if 
	  (ox+ow)>vx & (oy+oh)>vy & vx<(ox+800) & vy<(oy+600) then 

	(
	  let nx=ox-vx and
	      ny=oy-vy in

	    if vx<>0 || vy<>0 then (
	      o#move nx ny;
	      o#put();	
	      o#move ox oy
	    )
	    else
	      o#put();
	)
   )


end;;

exception Bad_v_color of int;;

class v_color=
object(self)
  val mutable colors=Hashtbl.create 2

  method add_vcolor (vc:color) (c:color array)=
    Hashtbl.add colors vc c

  method to_array n=
    let c1=DynArray.create() and
	c2=DynArray.create() in
    self#vcolor_foreach 
      (fun k v->
	 DynArray.add c1 k;
	 (try
	    DynArray.add c2 v.(n);
	  with Invalid_argument a->raise (Bad_v_color n))
      );
      (DynArray.to_array c1,DynArray.to_array c2)

(*  method color_change t n=
    let rt=ref t in
    Hashtbl.iter
      (fun k v->
	 let tr=tile_color_change !rt k v.(n) in	 
	   tile_free !rt;
	   rt:=tr
      ) colors;
      !rt
*)
  method vcolor_foreach f=
    Hashtbl.iter f colors
      

end;;

(* NEW : use drawing_vault *)

let get_font_id fnt_t=
  (match fnt_t with 
     | FontTTF (f,s)->(f^":"^string_of_int s)
     | FontEmbed->"font_embed");;


class font_ttf f s=
object
  val mutable font_t=FontTTF(f,s)
  method get_font_t=font_t

  initializer
    font_vault#add_cache (get_font_id font_t) (
      fun()->
	let fnt=font_vault#new_font() in
	  fnt#load font_t;
	  [|fnt|]
    );
end;;

(** Graphic object class parent *)
class graphic_cached_object nid=
  object (self)
    inherit canvas_object

    initializer
      self#set_id nid


	
(* FIXME: must be get_drawing and return drawing_object *)
    val mutable cur_drawing=0

    method get_drawing n=
      (drawing_vault#get_cache_entry id n)

    method set_cur_drawing c=cur_drawing<-c
    method get_cur_drawing=cur_drawing
    method get_drawings_size=(Array.length (drawing_vault#get_cache id))

(* DEPRECATED *)
(*    val mutable cur_tile=0

    method get_tile n=
      (drawing_vault#get_cache_entry id n)#get_t
      
    method set_cur_tile c=cur_tile<-c
    method get_cur_tile=cur_tile
    method get_tiles_size=(Array.length (drawing_vault#get_cache id))
*)

(** canvas *)

    method move x y=
      rect#set_position x y 

    method put() =      
      let t=self#get_drawing cur_drawing in
	video#get_drawing#compose t rect#get_x rect#get_y
(*	tile_put t rect#get_x rect#get_y; *)

  end;;

class graphic_from_drawing n (f)=
object
  inherit graphic_cached_object n

  initializer
    drawing_vault#add_cache n f;
    let dra=drawing_vault#get_cache_simple n in
    rect#set_size (dra#get_w) (dra#get_h); 
    
end;;

class graphic_from_drawing_fun n dfn args=
object
  inherit graphic_cached_object n

  initializer
    drawing_vault#add_cache_from_drawing_fun n dfn args;

    let dra=drawing_vault#get_cache_simple n in
      rect#set_size (dra#get_w) (dra#get_h);

end;;



class graphic_object_from_file file w h=
object(self)
  inherit graphic_from_drawing_fun file "with_alpha"   
	[
	  DrawValColor(255,255,255);
	  DrawValString "load_multiple";
	  DrawValString file;
	  DrawValSize(w,h);
	]
end;;




class graphic_object_resized_from_file file i w h iw ih=
object
  val fgr=new graphic_object_from_file file iw ih
  inherit graphic_from_drawing (file^":"^string_of_int i^":resized") 
    (fun()->
       let dr=(drawing_vault#get_cache_entry file i)#copy() in
	 dr#exec_op_copy "resize" [DrawValSizeFloat(((float_of_int w)/.(float_of_int iw)),((float_of_int h)/.(float_of_int ih)))]	 
    )
end;;

class graphic_object_resized pdraw i fw fh=
object
  inherit graphic_from_drawing (pdraw^":"^string_of_int i^":resized") 
    (fun()->
       let dr=(drawing_vault#get_cache_entry pdraw i)#copy() in
	 dr#exec_op_copy "resize" [DrawValSizeFloat(fw,fh)]	 
    )
end;;


let digest_of_string_list txt=
  (Digest.to_hex(Digest.string (String.concat "" txt)));;

class graphic_object_text fnt_t (txt:string list) color=
object
  inherit graphic_from_drawing ((random_string "text:" 10)^digest_of_string_list txt)
    (fun()->

	 let fnt_n=get_font_id fnt_t in
	   font_vault#add_cache (fnt_n) (
	     fun()->
	       let fnt=font_vault#new_font() in
		 fnt#load fnt_t;
		 [|fnt|]
	   );
	   Array.map
	     (
	       fun tx->
		 let  dr=drawing_vault#new_drawing() in
		   dr#exec_op_create "create_text" [DrawValString fnt_n;DrawValString tx;DrawValColor color];
		   dr
	     ) (Array.of_list txt)
    )
end;;


(** special graphic pattern resize with 9 tiles *)
class graphic_pattern pid pdrawid=
object(self)
  inherit graphic_cached_object pid as super

  val mutable gr=new graphic_cached_object pid

  val mutable crect=new rectangle 0 0 0 0
  method get_crect=crect


  method private init()=
    let pdrawing=(drawing_vault#get_cache_simple pdrawid) in
    let cw=pdrawing#get_w and
	ch=pdrawing#get_h in
      crect#set_size (cw/3) (ch/3);

      gr<-new graphic_from_drawing_fun pid "with_alpha" 
	[
	  DrawValColor(255,255,255);
	  DrawValString "create_multiple";
	  DrawValString pdrawid;
	  DrawValSize(crect#get_w,crect#get_h);
	];
(*
    gr<-new graphic_from_func pid (
      fun()->
	let ta=tile_split ptile crect#get_w crect#get_h in
	  for i=0 to (Array.length (ta))-1 do
	    tile_set_alpha ta.(i) 255 255 255; 
	  done;
	  ta
    );
*)

  initializer
    self#init()

(*

036
147
258

*)

  method real_size=
    let cw=rect#get_w/crect#get_w and
	ch=rect#get_h/crect#get_h in    
      (cw*crect#get_w,ch*crect#get_h)

  method put()=
    let cw=rect#get_w/crect#get_w -1 and
	ch=rect#get_h/crect#get_h -1 in    
      for i=0 to cw do
	for j=0 to ch do
	  (match (i,j) with
	     | (0,0) -> gr#set_cur_drawing 0
	     | (0,ih) when ih=ch ->gr#set_cur_drawing 2
	     | (0,_) ->gr#set_cur_drawing 1
	     | (iw,0) when iw=cw -> gr#set_cur_drawing 6
	     | (_,0) ->gr#set_cur_drawing 3
	     | (iw,ih) when iw=cw && ih=ch -> gr#set_cur_drawing 8
	     | (_,ih) when ih=ch ->gr#set_cur_drawing 5
	     | (iw,_) when iw=cw ->gr#set_cur_drawing 7
	     | (_,_) ->gr#set_cur_drawing 4
	  );
	  gr#move (rect#get_x + (i*crect#get_w)) (rect#get_y + (j*crect#get_h));
	  gr#put();
	  done
      done

end;; 

class graphic_pattern_file pfile=
object
  initializer
    drawing_vault#add_cache_from_drawing_fun (pfile^":simple") "load_simple"
      [DrawValString pfile];

  inherit graphic_pattern pfile (pfile^":simple")

end;;



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
  let uc=new v_color in
    List.iter (
      fun v->(	
	uc#add_vcolor (fst v) (snd v) 
      )
    )
      colparser#get_vcolors;
  uc

