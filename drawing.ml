(*
    poccore - core functionality
    Copyright (C) 2005 POC 

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
open Rect;;
open Generic;;
open Olua;;
open Oxml;;

open Cache;;

(** Drawing operations *)

(** {2 Exceptions *)

exception Draw_op_not_found of string;;
exception Draw_op_val_error of int;;
exception Draw_op_error;;
exception Bad_draw_op_val;;
exception Bad_draw_op_result;;
exception Drawing_not_initialized;;
exception Drawing_not_found of string;;


(** {2 Types *)

type color=(int*int*int);;

type ('t) draw_op_val=
  | DrawValPosition of (int*int)
  | DrawValSize of (int*int)
  | DrawValSizeFloat of (float*float)
  | DrawValRectangle of rectangle
  | DrawValColor of color
  | DrawValString of string
  | DrawValT of 't
  | DrawValTArray of 't array
  | DrawValNil;;

let draw_op_val_to_string v=
  match v with
    | DrawValPosition (x,y)->("DrawValPosition ("^string_of_int x^","^string_of_int y^")")
    | DrawValSize (x,y)->("DrawValSize ("^string_of_int x^","^string_of_int y^")")
    | DrawValSizeFloat (x,y)->("DrawValSizeFloat ("^string_of_float x^","^string_of_float y^")")
    | DrawValColor (r,g,b)->("DrawValColor ("^string_of_int r^","^string_of_int g^","^string_of_int b^")")
    | DrawValString s->("DrawValString "^s);
    | DrawValT t->("DrawValT");
    | DrawValTArray t->("DrawValTArray");
    | DrawValNil ->("DrawValNil")
    | _ -> "DrawOther";;
let get_draw_op_val (ovl:('t) draw_op_val list) (n:int)=
  List.nth ovl n;;

let get_draw_op_position ovl n=match (get_draw_op_val ovl n) with
  | DrawValPosition x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_size ovl n=match (get_draw_op_val ovl n) with
  | DrawValSize x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_size_float ovl n=match (get_draw_op_val ovl n) with
  | DrawValSizeFloat x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_rect ovl n=match (get_draw_op_val ovl n) with
  | DrawValRectangle x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_color ovl n=match (get_draw_op_val ovl n) with
  | DrawValColor x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_string ovl n=match (get_draw_op_val ovl n) with
  | DrawValString x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_t ovl n=match (get_draw_op_val ovl n) with
  | DrawValT x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_t_array ovl n=match (get_draw_op_val ovl n) with
  | DrawValTArray x->x
  | _ -> raise Bad_draw_op_val;;

type draw_op_t=
  | DrawTypeCreate
  | DrawTypeWrite
  | DrawTypeRead
  | DrawTypeCopy;;

type ('t,'dt) draw_op_result=
  | DrawResultUnit of unit
  | DrawResultDrawing of 'dt array
  | DrawResultVal of ('t) draw_op_val;;

let get_draw_op_result_unit ores=match ores with
  | DrawResultUnit x->x
  | _ -> raise Bad_draw_op_result;;
let get_draw_op_result_drawing ores=match ores with
  | DrawResultDrawing x->x
  | _ -> raise Bad_draw_op_result;;
let get_draw_op_result_val ores=match ores with
  | DrawResultVal x->x
  | _ -> raise Bad_draw_op_result;;

(** {2 Classes *)

class ['t] draw_ops=
object(self)
  val mutable ops=Hashtbl.create 2

  method add_op (n:string) (o_t:draw_op_t) (o:(('t) draw_op_val list->('t) draw_op_val))=Hashtbl.add ops n (o_t,o)
  method get_op n=
    (try
       Hashtbl.find ops n
     with Not_found -> raise (Draw_op_not_found n))

end;;

class virtual ['t] drawing_object=
object(self)
  inherit generic_object
  inherit ['t] draw_ops

  method exec_op n args=
    let (op_t,op)=self#get_op n in
    let r=op args in
    let nr=
     (match op_t with
	| DrawTypeCreate -> self#set_t (get_draw_op_t [r] 0) ;DrawResultUnit()
	| DrawTypeWrite ->DrawResultUnit()
	| DrawTypeRead ->DrawResultVal r
	| DrawTypeCopy->DrawResultDrawing(
	    let cr=(match r with
	      | DrawValT ct ->[|self#new_t ct|];
	      | DrawValTArray cta->
		  Array.map (
		    fun nt->
		      self#new_t nt
		  ) cta
	      | _ -> raise Draw_op_error) in
	      cr
     )) in
      (nr:('t,('t) drawing_object) draw_op_result)

  method exec_op_create n args=
    get_draw_op_result_unit (self#exec_op n args)
  method exec_op_write n args=
    get_draw_op_result_unit (self#exec_op n args)
  method exec_op_read n args=
    get_draw_op_result_val (self#exec_op n args)
  method exec_op_copy n args=
    get_draw_op_result_drawing (self#exec_op n args)

  method virtual get_t: 't
  method virtual set_t: 't->unit

  method virtual get_w:int
  method virtual get_h:int

(** create drawing from 't *)
  method virtual new_t : 't -> ('t) drawing_object
      
(** first Create draw_op *)
  method virtual create: int -> int -> color -> unit
(** first Copy draw_op *)  
  method virtual copy: unit -> ('t) drawing_object
(** first Write draw_op *)
  method virtual put_pixel:int->int->color->unit
(** first Read draw_op *)
  method virtual get_pixel:int->int->color

  method virtual compose:('t) drawing_object -> int -> int -> unit

  val mutable lua=new lua_obj
  method get_lua=lua

end;;


class virtual ['t] drawing_screen=
object
  inherit ['t] drawing_object

  method virtual init :int->int->int->bool->unit
  method virtual refresh: unit -> unit    

end;;

class virtual ['t] drawing_handler=
object(self)
  val mutable drs=Hashtbl.create 2

  method virtual new_drawing : unit -> ('t) drawing_object

  method add_drawing_fun (n:string) (o:('t) draw_op_val list->('t) drawing_object array)=
    print_string ("DRAWING_HANDLER: add drawing fun "^n);print_newline();
    Hashtbl.add drs n o
  method get_drawing_fun n=
    (try 
       Hashtbl.find drs n
     with Not_found-> raise (Drawing_not_found n));
    
  method exec_drawing_fun (n:string) (args:('t) draw_op_val list)=
    print_string ("DRAWING_HANDLER: exec "^n);print_newline();
    List.iter (
      fun a->
	print_string (" -"^draw_op_val_to_string a);print_newline();
    )args;
    let drf=self#get_drawing_fun n in
      drf args


end;;

class ['t] drawing_cache s=
object
  inherit [('t) drawing_object] medias_cache s
end;;

class virtual ['t] drawing_vault cache_size=
object(self)
  inherit ['t] drawing_cache cache_size as cache
  inherit ['t] drawing_handler

  (* link between cache & handler *)
  method add_cache_from_drawing_fun (n:string) (dfn:string) (args:('t) draw_op_val list)=
    let drl=(fun()->self#exec_drawing_fun dfn args) in
      self#add_cache n drl;

  initializer
    (** generic drawing creation functions *)

    self#add_drawing_fun "load_simple"
      (
	fun vl->
	  let file=List.nth vl 0 in
	    
	  let dr=self#new_drawing() in
	    dr#exec_op_create "load" [file];
	    [|dr|]
      );

    self#add_drawing_fun "load_multiple" 
      (
	fun vl->
	  let file=List.nth vl 0 and
	      size=List.nth vl 1 in
	    
	  let dr=self#new_drawing() in
	    dr#exec_op_create "load" [file];
	    dr#exec_op_copy "split" [size];
      );

    self#add_drawing_fun "create_multiple" 
      (
	fun vl->
	  let cached=get_draw_op_string vl 0 and
	      size=List.nth vl 1 in
	    
	  let dr=(self#get_cache_simple cached) in
	    dr#exec_op_copy "split" [size];
      );

(* very exciting !!*)
    self#add_drawing_fun "with_alpha"
      (
	fun vl->
	  let par=get_draw_op_string vl 0 and
	      col=List.nth vl (List.length vl -1) in
	  let drl=self#exec_drawing_fun par (List.tl vl) in
	    Array.iter (
	      fun dr->
		dr#exec_op_write "set_alpha" [col]
	    ) drl;
	    drl
      )

end;;

(** poclow binding *)



open Low;;

(** poclow drawing *)
class poclow_drawing_object=
object(self)
  inherit [tile] drawing_object
  val mutable t=None
  method get_t=
    match t with
      | Some v->v
      | None -> raise Drawing_not_initialized;
  method set_t nt=t<-(Some nt)

  method get_w=tile_get_w self#get_t
  method get_h=tile_get_h self#get_t    

  method new_t nt=
    let nd=new poclow_drawing_object in
      nd#set_t (nt);
      nd

  method create w h c=self#set_t (tile_box w h c)
  method copy()=
    self#new_t (tile_copy self#get_t)

  method put_pixel x y c=tile_putpixel self#get_t c x y
  method get_pixel x y=tile_getpixel self#get_t x y

  method compose dr x y=tile_put_to dr#get_t self#get_t x y

(* xml & lua test *)
(*

 <drawing_object id="test" fun="load_multiple">
  <values>
   <val_string str="medias/test.png"/> 
   <val_size w="32" h="32"/>
   <val_color r="255" g="255" b="255"/>
  </values>
  <script>
   i=0;
   while i<test.size do
    test[i].set_alpha(255,255,255);
    i=i+1;
   end;
  </script>
 </drawing_object>
  
*)

  initializer

(** create ops *)

    self#add_op "load" DrawTypeCreate (
      fun ovl->
	let f=(get_draw_op_string ovl 0) in
	print_string ("DRAWING_OBJECT: load "^f);print_newline();
	DrawValT (tile_load f);
    );

(** copy ops *)

    self#add_op "mirror" DrawTypeCopy (
      fun ovl->
	DrawValT (tile_mirror self#get_t);
    );

    self#add_op "split" DrawTypeCopy (
      fun ovl->
	let (w,h)=get_draw_op_size ovl 0 in
	DrawValTArray (tile_split self#get_t w h);
    );

    self#add_op "resize" DrawTypeCopy (
      fun ovl->
	let (w,h)=get_draw_op_size_float ovl 0 in
	DrawValT (tile_resize self#get_t w h);
    );

(** write op *)

    self#add_op "set_alpha" DrawTypeWrite (
      fun ovl->
	let (r,g,b)=get_draw_op_color ovl 0 in
	  (tile_set_alpha self#get_t r g b);
	  DrawValNil;
    );

    self#add_op "line" DrawTypeWrite (
      fun ovl->
	let p1=get_draw_op_position ovl 0 and
	    p2=get_draw_op_position ovl 1 and
	    col=get_draw_op_color ovl 2 in
	  
	  (tile_line self#get_t p1 p2 col);
	  DrawValNil;
    );


end;;

class poclow_drawing_screen=
object(self)
  inherit [tile] drawing_screen
  inherit poclow_drawing_object

  method init w h bpp fs=
    self#set_t (video_init w h bpp fs)

  method refresh()=
    video_update()
end;;

class poclow_drawing_vault s=
object(self)
  inherit [tile] drawing_vault s
  method new_drawing()=new poclow_drawing_object
end;;

let drawing_vault=new poclow_drawing_vault 1000;;
