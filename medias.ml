(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003 POC 

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

open Low;;

open Generic;;
open Rect;;
open Video;;
open Vfs;;

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


(** Load an unit tile of 5 directions and make mirrors to have 8 directions *)
let tiles_load_with_mirror5 file w h=
  let t=tiles_load_with_mirror_space file w h in
  let mirror_unit v m =
    (
     let k=ref 0 
     in
     (
      for i=v*((Array.length (t))/8) to (v+1)*(((Array.length (t))/8))-1 do
	(t).(((Array.length (t))/8)*m + !k)<-tile_mirror((t).(i));
	k:= !k + 1;
      done;
     );
    )
  in
  mirror_unit 3 5;
  mirror_unit 2 6;
  mirror_unit 1 7;
  for i=0 to (Array.length (t))-1 do
	tile_set_alpha t.(i) 255 255 255;

  done;
  t 
;;


(** Load an unit tile of 5 directions and make mirrors to have 8 directions *)
let tiles_load_with_mirror3 file w h=
  let t=tiles_load_with_mirror_space3 file w h in
  let mirror_unit v m =
    (
     let k=ref 0 
     in
     (
      for i=v*((Array.length (t))/8) to (v+1)*(((Array.length (t))/8))-1 do
	(t).(((Array.length (t))/8)*m + !k)<-tile_mirror((t).(i));
	k:= !k + 1;
      done;
     );
    )
  and
    copy_unit v m =
    (
     let k=ref 0 
     in
     (
      for i=v*((Array.length (t))/8) to (v+1)*(((Array.length (t))/8))-1 do
	(t).(((Array.length (t))/8)*m + !k)<-tile_copy((t).(i));
	k:= !k + 1;
      done;
     );
    )
  in
copy_unit 2 4;
copy_unit 1 2;
mirror_unit 2 6;

  for i=0 to (Array.length (t))-1 do
	tile_set_alpha t.(i) 255 255 255;

  done;
  t 
;;

let tiles_load_with_mirror file w h=
  let t=tile_load file in
  let aw=tile_get_w t and
      ah=tile_get_h t in
    if aw/w = 5 then
      tiles_load_with_mirror5 file w h
    else
      tiles_load_with_mirror3 file w h
;;

(** {2 Font part} *)

(** Font object class *)
class font_object fontfile s=
  object (self)
    val mutable font=fontfile
    val mutable size=(*int_of_float(video#get_fact_w()*.(float_of_int s))*)s
    initializer 
      if fontfile<>"none" then
	vfs_fonts#create_simple (fontfile ^ ":" ^ (string_of_int(size))^"pt") (font_load fontfile size)

    method get_size=size
    method get_height=
      if fontfile<>"none" then
	font_height (vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt"))
      else 
	(
	  8
	)

    method sizeof_text txt=
      if fontfile<>"none" then     
	font_sizeof (vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt")) txt
      else
	(
	  (String.length txt*8,8);
	)
(*
    method get_font=
      if fontfile<>"none" then
	vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt")
      else font_empty()
*)
    method create_text txt color =
      if fontfile<>"none" then
	tile_text (vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt")) txt color
      else
	(
	  let tmp=tile_box (String.length txt*8) 8 (255,255,255) in
	    tile_set_alpha tmp 255 255 255;
	    tile_string tmp (0,0) txt color; 
	    tmp
	)
(*	tile_empty() *)
(*	tile_string txt color *)
  end;;


(** {2 Sound part} *)

(** Sound object class *)
class sound_object soundfiles=
  object (self)
    initializer       
      for i=0 to (Array.length soundfiles)-1 do	
	vfs_sounds#create_simple_from_func (soundfiles.(i)) (function()->(sound_load soundfiles.(i))) 
      done;
    val mutable rect=new rectangle 0 0 0 0
    val mutable distance=0
    val mutable direction=0
    val mutable angle=0
    val mutable sounds=soundfiles
    val mutable channel=(-1)
    val mutable is_playing=false

    method get_sound i=
      vfs_sounds#get_simple (sounds.(i))

    method move x y=rect#set_position x y;

    method play_simple num=
      channel<-sound_play (self#get_sound num);

    method play (vx:int) (vy:int) num=      
      if channel <>(-1) then (
	if (sound_playing channel)==false then (
	  is_playing<-false;
	  channel<-(-1);
	 ) 
       )
      else (
	let dt=(racine((carree ((vx+ video#get_w/2)/32-rect#get_x)) +. (carree ((vy+ video#get_h/2)/32-rect#get_y)))) in
	let d=int_of_float ((dt/.64.0)*.255.0) in
	channel<-sound_play (self#get_sound num);
(*	if channel <>(-1) then 
	  sound_position channel d 0;
*)	 
	is_playing<-true
       )	  

    method play_random (vx:int) (vy:int)=
      if (sounds.(0)<>"none") then (
      if channel <>(-1) then (
	if (sound_playing channel)==false then (
	  is_playing<-false;
	  channel<-(-1);
	 )
       )
      else (
	let dt=(racine((carree ((vx+ video#get_w/2)/32-rect#get_x)) +. (carree ((vy+ video#get_h/2)/32-rect#get_y)))) in
	let d=int_of_float ((dt/.64.0)*.255.0) in
	channel<-sound_play (self#get_sound (randomize (Array.length sounds)));
(*	if channel <>(-1) then 
	  sound_position channel d 0;
*)
	is_playing<-true
       )	  
      )
	  
  end;;



(** {2 Graphic part} *)

exception Vfs_not_found of string;;
exception Vfs_out_of_bounds of int;;

class virtual canvas_object=
object
  inherit generic_object
  val mutable layer=0
  method set_layer l=layer<-l
  method get_layer=layer

  val mutable rect=new rectangle 0 0 0 0
  method get_rect=rect

  method virtual move : int -> int -> unit
  method virtual put : unit -> unit
end;;



(** Graphic object class parent *)
class graphic_generic_object nid=
  object (self)
    inherit canvas_object

    initializer
      self#set_id nid

    val mutable cur_tile=0
	
    method get_rpos=
	 vfs_tiles#get_rpos id

    method get_tile n=
	vfs_tiles#get_one id n

    method get_tile_shaded n=
      vfs_tiles#get_one (id^":shaded") n
      
    method get_rect=rect
    method set_cur_tile c=cur_tile<-c
    method get_cur_tile=cur_tile
    method get_tiles_size=(Array.length (vfs_tiles#get id))

    method move x y=rect#set_position x y 

    method resize fw fh=
      rect#set_size (int_of_float(fw*.(float_of_int rect#get_w))) (int_of_float(fh*.(float_of_int rect#get_h)));
      let ts=(vfs_tiles#get id) in
      for i=0 to (self#get_tiles_size)-1 do
	ts.(i)<-(tile_resize (self#get_tile i) fw fh); 
      done;


    method put() =      
      let t=self#get_tile cur_tile in
	tile_put t rect#get_x rect#get_y;
	vfs_tiles#free_dyn id t

    method put_to dest = 
      let t=self#get_tile cur_tile in
      tile_put_to t dest rect#get_x rect#get_y;
      vfs_tiles#free_dyn id t

    method put_shaded ()=
      let shaded=(self#get_tile_shaded cur_tile) in      
(*      tile_set_alpha shaded 127 127 127; *)
      tile_put shaded rect#get_x rect#get_y;
      vfs_tiles#free_dyn (id^":shaded") shaded

    method put_shaded_to dest=
      let shaded=(self#get_tile_shaded cur_tile) in      
(*      tile_set_alpha shaded 127 127 127; *)
      tile_put_to shaded dest rect#get_x rect#get_y;
      vfs_tiles#free_dyn (id^":shaded") shaded
      

  end;;


(** Dyn graphic object class *)
class graphic_dyn_object n s f=
  object (self)
    inherit graphic_generic_object n as super 
    initializer
      vfs_tiles#create_dyn_func id s f;
  end;;





(** Graphic object class from a tile *)
class graphic_real_object nm tile=
  object (self)
(* FIXME : need test *)
    inherit graphic_generic_object nm as super 
    initializer
      rect#set_size (tile_get_w tile) (tile_get_h tile);
      vfs_tiles#create_simple id tile;
      
  end;;

(** Graphic object class from a file with simple entry *)
class graphic_simple_object tilefile=
  object (self)
  inherit graphic_generic_object tilefile 
    initializer
      vfs_tiles#create_simple_from_func tilefile (function()->(
	let t=tile_load tilefile in tile_set_alpha t 255 255 255;t;
       ));
      let t=vfs_tiles#get_simple (tilefile) in
      rect#set_size (tile_get_w t) (tile_get_h t);

  end;;

class graphic_simple_from_func tilefile f=
  object (self)
  inherit graphic_generic_object tilefile 
    initializer
      vfs_tiles#create_simple_from_func tilefile f;
      let t=vfs_tiles#get_simple (tilefile) in
      rect#set_size (tile_get_w t) (tile_get_h t);

  end;;

(** Graphic object class from a file with simple entry *)
class graphic_from_func tilefile func=
  object (self)
  inherit graphic_generic_object tilefile 
    initializer
      vfs_tiles#create_from_func tilefile (func);

      let t=vfs_tiles#get_simple (tilefile) in
      rect#set_size (tile_get_w t) (tile_get_h t);

  end;;



(** Graphic object class from a file with multiple entries*)
class graphic_object_alpha wi hi tilesfile mirror is_shaded alpha=
object (self)
  inherit graphic_generic_object tilesfile as super

  initializer
    rect#set_size (wi) (hi);
    if mirror==false then  (
      vfs_tiles#create_from_func tilesfile (
	function()->(
	  
	  let t=tiles_load tilesfile wi hi in
	    if alpha then (
	      for i=0 to (Array.length (t))-1 do
		tile_set_alpha t.(i) 255 255 255; 
	      done;
	    );
	    t

       ));
      if is_shaded=true then
	(
	 vfs_tiles#create_from_func (tilesfile^":shaded") (function()->(
	   let t=tiles_load tilesfile wi hi in
	   let t_s=Array.make (Array.length (t)) (tile_empty()) in
	   for i=0 to (Array.length (t))-1 do
	     let tl=(	      
	       let ta=tile_resize t.(i) 0.25 0.25 in
	       let tb=tile_resize ta 4.0 4.0 in
	       let tc=tile_shade tb in 
	       tile_free ta;
	       tile_free tb;
	       tile_free t.(i); 
		tc
	      )in
	     t_s.(i)<-tl;
	   done;	
	   t_s
	  )))

     )
    else 
      if tilesfile<>"none" then
	vfs_tiles#create_from_func tilesfile (function()->(tiles_load_with_mirror tilesfile wi hi));


end;;

(** Graphic object class from a file with multiple entries*)
class graphic_object wi hi tilesfile mirror is_shaded =
object (self)
  inherit graphic_generic_object tilesfile as super

  initializer
    rect#set_size (wi) (hi);
    if mirror==false then  (
      vfs_tiles#create_from_func tilesfile (function()->(
					      
	let t=tiles_load tilesfile wi hi in
	for i=0 to (Array.length (t))-1 do
	  tile_set_alpha t.(i) 255 255 255; 
	done;
	  t

       ));
      if is_shaded=true then
	(
	 vfs_tiles#create_from_func (tilesfile^":shaded") (function()->(
	   let t=tiles_load tilesfile wi hi in
	   let t_s=Array.make (Array.length (t)) (tile_empty()) in
	   for i=0 to (Array.length (t))-1 do
	     let tl=(	      
	       let ta=tile_resize t.(i) 0.25 0.25 in
	       let tb=tile_resize ta 4.0 4.0 in
	       let tc=tile_shade tb in 
	       tile_free ta;
	       tile_free tb;
	       tile_free t.(i); 
		tc
	      )in
	     t_s.(i)<-tl;
	   done;	
	   t_s
	  )))

     )
    else 
      if tilesfile<>"none" then
	vfs_tiles#create_from_func tilesfile (function()->(tiles_load_with_mirror tilesfile wi hi));


end;;




class v_color=
object
  val mutable colors=Hashtbl.create 2

  method add_vcolor (vc:color) (c:color array)=
    Hashtbl.add colors vc c

  method color_change t n=
    let rt=ref t in
    Hashtbl.iter
      (fun k v->
	 let tr=tile_color_change !rt k v.(n) in	 
	   tile_free !rt;
	   rt:=tr
      ) colors;
      !rt
	
  method vcolor_foreach f=
    Hashtbl.iter f colors
      

end;;

class unit_color=
object
  inherit v_color
end;;

class graphic_object_colored wi hi tilesfile mirror is_shaded (uc:unit_color) (un:int)=
object (self)
  inherit graphic_generic_object (tilesfile^":colored") as super

  initializer
    rect#set_size (wi) (hi);
    if mirror==false then  (
      vfs_tiles#create_from_func (tilesfile^":colored") (function()->(
	let t=tiles_load tilesfile wi hi in
	for i=0 to (Array.length (t))-1 do
	      t.(i)<-uc#color_change t.(i) un;
	      tile_set_alpha t.(i) 255 255 255;
	done;
	t
       ));
      if is_shaded=true then
	(
	 vfs_tiles#create_from_func (tilesfile^":colored:shaded") (function()->(
	   let t=tiles_load tilesfile wi hi in
	   let t_s=Array.make (Array.length (t)) (tile_empty()) in
	   for i=0 to (Array.length (t))-1 do
	     let tl=(	      
	       t.(i)<-uc#color_change t.(i) un;
	       let ta=tile_resize t.(i) 0.25 0.25 in
	       let tb=tile_resize ta 4.0 4.0 in
	       let tc=tile_shade tb in 
	       tile_free ta;
	       tile_free tb;
	       tile_free t.(i); 
		tc
	      )in
	     t_s.(i)<-tl;
	   done;	
	   t_s
	  )))

     )
    else 
      if tilesfile<>"none" then
	vfs_tiles#create_from_func (tilesfile^":colored") (
	  function()->(
	    let t=tiles_load_with_mirror tilesfile wi hi in
	      for i=0 to (Array.length (t))-1 do

	      t.(i)<-uc#color_change t.(i) un;
		  tile_set_alpha t.(i) 255 255 255; 
		  
	      done;
	      t
));



end;;



let resized_objs=let a=Hashtbl.create 2 in Hashtbl.add a "none" false;a;;

class graphic_real_resized_object nm fw fh tile=
  object
    inherit graphic_real_object nm tile as super
    initializer
      if (fw<>1.0 || fh<>1.0) && (Hashtbl.mem resized_objs nm)==false  then (
	super#resize fw fh;
	Hashtbl.add resized_objs nm true;
       )
  end;;


class graphic_scr_resized_real_object nm tile=
  object
    inherit graphic_real_resized_object nm (video#get_fact_w()) (video#get_fact_h()) tile as super
  end;;

(** Graphic object class that is resized *)
class graphic_resized_object wi hi fw fh tilesfile mirror is_shaded=
  object
    inherit graphic_object wi hi tilesfile mirror is_shaded as super
    initializer
      if tilesfile<>"none" then	
	if (fw<>1.0 || fh<>1.0) then (
	  if (Hashtbl.mem resized_objs tilesfile)==false  then (	  
	    super#resize fw fh;
	    Hashtbl.add resized_objs tilesfile true;
	   ) else (
	    rect#set_size (int_of_float(fw*.(float_of_int rect#get_w))) (int_of_float(fh*.(float_of_int rect#get_h)));

	   )
	 )
  end;;

(** Graphic object class that is resized with screen factor *)
class graphic_scr_resized_object wi hi tilesfile mirror is_shaded=
  object
    inherit graphic_resized_object wi hi (video#get_fact_w()) (video#get_fact_h()) tilesfile mirror is_shaded as super
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

let white_border t=

  let w=tile_get_w t and
      h=tile_get_h t in
  let white=(255,255,255) in
  let t2=tile_copy t in
    for x=0 to w-1 do
      for y=0 to h-1 do
	let border=(200,200,200) in
	let color=tile_getpixel t x y in 
	  if color=white then
	    (
	      let res=ref false in
		for i=(-1) to 1 do
		  for j=(-1) to 1 do
		    if (i<>0 or j<>0) then
		      if (x+i)>0 && (x+i)<w && (y+j)>0 && (y+j)<h then  
			(
			  let c=tile_getpixel t (x+i) (y+j) in 
			
			    if c<>border && c<>white then 
			      res:=true
			)
			  
		  done;
		done;
		if !res then 
		  tile_putpixel t2 border x y  
		    
	    )
      done;
    done;
    tile_free t;
    t2




(** special graphic pattern resize with 9 tiles *)
class graphic_pattern pid ptile=
object(self)
  inherit graphic_generic_object pid as super

  val mutable gr=new graphic_generic_object pid

  val mutable crect=new rectangle 0 0 0 0
  method get_crect=crect


  method private init()=
    let cw=tile_get_w ptile and
	ch=tile_get_h ptile in
      crect#set_size (cw/3) (ch/3);

    gr<-new graphic_from_func pid (
      fun()->
	let ta=tile_split ptile crect#get_w crect#get_h in
	  for i=0 to (Array.length (ta))-1 do
	    tile_set_alpha ta.(i) 255 255 255; 
	  done;
	  ta
    );



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
	     | (0,0) -> gr#set_cur_tile 0
	     | (0,ih) when ih=ch ->gr#set_cur_tile 2
	     | (0,_) ->gr#set_cur_tile 1
	     | (iw,0) when iw=cw -> gr#set_cur_tile 6
	     | (_,0) ->gr#set_cur_tile 3
	     | (iw,ih) when iw=cw && ih=ch -> gr#set_cur_tile 8
	     | (_,ih) when ih=ch ->gr#set_cur_tile 5
	     | (iw,_) when iw=cw ->gr#set_cur_tile 7
	     | (_,_) ->gr#set_cur_tile 4
	  );
	  gr#move (rect#get_x + (i*crect#get_w)) (rect#get_y + (j*crect#get_h));
	  gr#put();
	  done
      done

end;; 

class graphic_pattern_file pfile=
object
  inherit graphic_pattern pfile (tile_load pfile)
end;;


(** white border around tile *)
class graphic_white_border tilesfile w h=
object
  inherit graphic_from_func (tilesfile^"/white_border") 
    (fun()->
	 let t=tile_load tilesfile in
	 let t2=white_border t in
	 let ta=tile_split t2 w h in
	   for i=0 to (Array.length (ta))-1 do
	     tile_set_alpha ta.(i) 255 255 255; 
	   done;
	   ta
    )

    as super
    
  val mutable gr=new graphic_object w h tilesfile false false
    
  val mutable over=false
  method set_over o=over<-o

  method move x y=
    super#move x y;
    gr#move x y;

  method put()=
    if over then
      super#put()
    else
      gr#put()
  
end;;

class graphic_generic_white_border id=
object
  inherit graphic_generic_object id
  method set_over (v:bool)=()
end;;
