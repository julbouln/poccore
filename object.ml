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
    val mutable size=int_of_float(video#get_fact_w()*.(float_of_int s))
    initializer 
      if fontfile<>"none" then
	vfs_fonts#create_simple (fontfile ^ ":" ^ (string_of_int(size))^"pt") (font_load fontfile size)

    method get_size=size
    method get_height=
      if fontfile<>"none" then
	font_height (vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt"))
      else 0
    method sizeof_text txt=
      if fontfile<>"none" then     
     font_sizeof (vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt")) txt
      else (0,0)
    method get_font=
      if fontfile<>"none" then
	vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt")
      else font_empty()
    method create_text txt color =
      if fontfile<>"none" then
	tile_text (vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt")) txt color
      else tile_empty()
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

(** Graphic object class parent *)
class graphic_generic_object id=
  object (self)
    val mutable tiles=id


    val mutable rect=new rectangle 0 0 0 0

    val mutable layer=0
    method set_layer l=layer<-l
    method get_layer=layer
	
    val mutable cur_tile=0
	


   method get_rpos=(try
		      vfs_tiles#get_rpos tiles 
		    with _ -> raise (Vfs_not_found tiles)) 


    method get_tile n=
      (try vfs_tiles#get_one tiles n with _ -> raise (Vfs_not_found tiles)) 
    method get_tile_shaded n=
      (try vfs_tiles#get_one (tiles^":shaded") n with _ -> raise (Vfs_not_found tiles)) 
    method get_id=id


      
    method get_rect=rect
    method set_cur_tile c=cur_tile<-c
    method get_cur_tile=cur_tile
    method get_tiles_size=(Array.length (vfs_tiles#get tiles))

    method move x y=rect#set_position x y 
    method resize fw fh=
      rect#set_size (int_of_float(fw*.(float_of_int rect#get_w))) (int_of_float(fh*.(float_of_int rect#get_h)));
      let ts=(vfs_tiles#get tiles) in
      for i=0 to (self#get_tiles_size)-1 do
	ts.(i)<-(tile_resize (self#get_tile i) fw fh); 
      done;


    method put() =      
      let t=self#get_tile cur_tile in
	tile_put t rect#get_x rect#get_y;
	vfs_tiles#free_dyn tiles t

    method put_to dest = 
      let t=self#get_tile cur_tile in
      tile_put_to t dest rect#get_x rect#get_y;
      vfs_tiles#free_dyn tiles t

    method put_shaded ()=
      let shaded=(self#get_tile_shaded cur_tile) in      
(*      tile_set_alpha shaded 127 127 127; *)
      tile_put shaded rect#get_x rect#get_y;
      vfs_tiles#free_dyn (tiles^":shaded") shaded

    method put_shaded_to dest=
      let shaded=(self#get_tile_shaded cur_tile) in      
(*      tile_set_alpha shaded 127 127 127; *)
      tile_put_to shaded dest rect#get_x rect#get_y;
      vfs_tiles#free_dyn (tiles^":shaded") shaded
      

  end;;


(** Dyn graphic object class *)
class graphic_dyn_object n s f=
  object (self)
    inherit graphic_generic_object n as super 
    initializer
      vfs_tiles#create_dyn_func tiles s f;
  end;;

(** Graphic object class from a tile *)
class graphic_real_object nm tile=
  object (self)
(* FIXME : need test *)
    inherit graphic_generic_object nm as super 
    initializer
      rect#set_size (tile_get_w tile) (tile_get_h tile);
      vfs_tiles#create_simple tiles tile;
      
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




class unit_color=
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


