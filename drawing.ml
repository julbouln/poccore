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
open Oval;;

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

(* use oval *)
type ('t) val_drawing=
    [
      val_ext
    | `DrawingSizeFloat of (float*float) 
    | `DrawingRectangle of rectangle

    | `DrawingT of 't
    | `DrawingTArray of 't array

    | `DrawingList of ('t) val_drawing list
    ];;

type ('t) draw_op_val=
  | DrawValInt of int
  | DrawValString of string

  | DrawValPosition of (int*int)
  | DrawValSize of (int*int)
  | DrawValSizeFloat of (float*float)
  | DrawValRectangle of rectangle
  | DrawValColor of color

  | DrawValList of ('t) draw_op_val list 
  | DrawValNil;;
(*
let draw_op_val_to_string v=
  match v with
    | DrawValPosition (x,y)->("DrawValPosition ("^string_of_int x^","^string_of_int y^")")
    | DrawValSize (x,y)->("DrawValSize ("^string_of_int x^","^string_of_int y^")")
    | DrawValSizeFloat (x,y)->("DrawValSizeFloat ("^string_of_float x^","^string_of_float y^")")
    | DrawValColor (r,g,b)->("DrawValColor ("^string_of_int r^","^string_of_int g^","^string_of_int b^")")
    | DrawValString s->("DrawValString "^s);
    | DrawValInt s->("DrawValInt "^string_of_int s);
    | DrawValList l->("DrawValList");
    | DrawValNil ->("DrawValNil")
    | DrawValRectangle r->("DrawValRectangle")
    | _ -> "DrawOther";;
*)
let get_draw_op_val (ovl:('t) draw_op_val list) (n:int)=
  List.nth ovl n;;

let get_draw_op_string ovl n=match (get_draw_op_val ovl n) with
  | DrawValString x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_int ovl n=match (get_draw_op_val ovl n) with
  | DrawValInt x->x
  | _ -> raise Bad_draw_op_val;;
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
let get_draw_op_list ovl n=match (get_draw_op_val ovl n) with
  | DrawValList x->x
  | _ -> raise Bad_draw_op_val;;

type draw_op_t=
  | DrawTypeCreate
  | DrawTypeWrite
  | DrawTypeRead
  | DrawTypeCopy;;

type ('t) draw_op_result=
  | DrawResultUnit of unit
  | DrawResultT of 't
  | DrawResultTArray of 't array
(*  | DrawResultDrawing of 'dt array *)
  | DrawResultVal of ('t) draw_op_val;;

let get_draw_op_result_unit ores=match ores with
  | DrawResultUnit x->x
  | _ -> raise Bad_draw_op_result;;
(*let get_draw_op_result_drawing ores=match ores with
  | DrawResultDrawing x->x
  | _ -> raise Bad_draw_op_result;;
*)
let get_draw_op_result_t ores=match ores with
  | DrawResultT x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_result_t_array ores=match ores with
  | DrawResultTArray x->x
  | _ -> raise Bad_draw_op_val;;
let get_draw_op_result_val ores=match ores with
  | DrawResultVal x->x
  | _ -> raise Bad_draw_op_result;;

(** {2 Classes *)

class ['t] draw_ops=
object(self)
  val mutable ops=Hashtbl.create 2

  method add_op (n:string) (o_t:draw_op_t) (o:(('t) draw_op_val list->('t) draw_op_result))=Hashtbl.add ops n (o_t,o)
  method get_op n=
    (try
       Hashtbl.find ops n
     with Not_found -> raise (Draw_op_not_found n))

end;;

class virtual ['t] drawing_object=
object(self)
  inherit generic_object
  inherit ['t] draw_ops

  val mutable t=None
  method get_t=
    match t with
      | Some v->v
      | None -> raise Drawing_not_initialized;
  method set_t (nt:'t)=t<-(Some nt)


  method exec_op n args=
    let (op_t,op)=self#get_op n in
    let r=op args in
    let nr=
     (match op_t with
	| DrawTypeCreate -> self#set_t (get_draw_op_result_t r) ;DrawResultUnit()
	| DrawTypeWrite ->r
	| DrawTypeRead ->r
	| DrawTypeCopy->r
(*
	    let cr=(match r with
	      | DrawValT ct ->[|self#new_t ct|];
	      | DrawValTArray cta->
		  Array.map (
		    fun nt->
		      self#new_t nt
		  ) cta
	      | _ -> raise Draw_op_error) in
	      cr
)
*)
     ) in
      (nr:('t) draw_op_result)

  method exec_op_create n args=
    get_draw_op_result_unit (self#exec_op n args)
  method exec_op_write n args=
    get_draw_op_result_unit (self#exec_op n args)
  method exec_op_read n args=
    get_draw_op_result_val (self#exec_op n args)
  method exec_op_copy n args=
    let cr=(match (self#exec_op n args) with
	      | DrawResultT ct ->[|self#new_t ct|];
	      | DrawResultTArray cta->
		  Array.map (
		    fun nt->
		      self#new_t nt
		  ) cta
	      | _ -> raise Draw_op_error) in
      cr
(*    get_draw_op_result_drawing (self#exec_op n args)
*)
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
  method virtual blank: unit->unit
  method virtual refresh: unit -> unit    
  method virtual set_caption: string->string->unit
  method virtual set_clip: int->int->int->int->unit
  method virtual show_cursor:unit->unit
  method virtual hide_cursor:unit->unit

end;;

class virtual ['t] drawing_handler=
object(self)
  val mutable drs=Hashtbl.create 2

  method virtual new_drawing : unit -> ('t) drawing_object
  method virtual new_drawing_screen : unit -> ('t) drawing_screen

  method add_drawing_fun (n:string) (o:('t) draw_op_val list->('t) drawing_object array)=
    print_string ("DRAWING_HANDLER: add drawing fun "^n);print_newline();
    Hashtbl.add drs n o
  method get_drawing_fun n=
    (try 
       Hashtbl.find drs n
     with Not_found-> raise (Drawing_not_found n));

  method call_drawing_fun (args:('t) draw_op_val list)=
    let n=get_draw_op_string args 0 in
    let drf=self#get_drawing_fun n in
      drf (List.tl args)
    
  method exec_drawing_fun (n:string) (args:('t) draw_op_val list)=
(*    print_string ("DRAWING_HANDLER: exec "^n);print_newline();
    List.iter (
      fun a->
	print_string (" -"^draw_op_val_to_string a);print_newline();
    )args;
*)
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
  method add_cache_from_drawing_fun (n:string) (args:('t) draw_op_val list)=
    if self#is_cache_fun (get_draw_op_string args 0)=false then 
      let drl=(fun()->self#call_drawing_fun args) in    
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
	  let col=List.nth vl (0) and
	      par=get_draw_op_string vl 1 in	      
	  let drl=self#exec_drawing_fun par (snd (ExtList.List.split_nth 2 vl)) in
	    Array.iter (
	      fun dr->
		dr#exec_op_write "set_alpha" [col]
	    ) drl;
	    drl
      );

    self#add_drawing_fun "with_border"
      (
	fun vl->
	  let col=get_draw_op_color vl (0) and
	      par=get_draw_op_string vl 1 in	      
	  let drl=self#exec_drawing_fun par (snd (ExtList.List.split_nth 2 vl)) in
	    Array.map (
	      fun idr->
		let dr=idr#copy() in
		let w=dr#get_w and
		    h=dr#get_h in
		for x=0 to w-1 do
		  for y=0 to h-1 do
		    let p=dr#get_pixel x y in
		      if p=(255,255,255) then 
			(
			  let res=ref false in
			    for i=(-1) to 1 do
			      for j=(-1) to 1 do
				if (i<>0 or j<>0) then
				  if (x+i)>0 && (x+i)<w && (y+j)>0 && (y+j)<h then  
				    (
				      let c=idr#get_pixel (x+i) (y+j) in 	
					if c<>col && c<>(255,255,255) then 
					  res:=true
				    )				    
			      done;
			    done;
			    if !res then 
			      dr#put_pixel x y col
				
			)
		  done;
		done;
		  dr
	    ) drl;
      );



    self#add_drawing_fun "with_mirror3"
      (
	fun vl->
	  let par=get_draw_op_string vl 0 in
	  let drl=self#exec_drawing_fun par (List.tl vl) in
	  let column=(Array.length drl)/3 in
	  let ndrl=Array.make (column*8) drl.(0) in
	    Array.iteri (
	      fun i dr->
		ndrl.(i)<-dr;
	    ) drl;
	    let copy_col v m mirror=
	      let k=ref 0 in
		for i=v*column to (v+1)*column - 1 do
		  ndrl.(column*m + !k)<-(if mirror then ((drl.(i))#exec_op_copy "mirror" []).(0) else drl.(i));
		  k:= !k+1;
		done
	in
	      copy_col 2 4 false;
	      copy_col 1 6 true;
	      copy_col 1 2 false;
	      
	      ndrl

  );

    self#add_drawing_fun "with_color_change"
      (
	fun vl->
	  let col1_l=get_draw_op_list vl (0) and
	      col2_l=get_draw_op_list vl (1) and
	      par=get_draw_op_string vl 2 in
	  let drl=self#exec_drawing_fun par (snd (ExtList.List.split_nth 3 vl)) in
	    Array.map (
	      fun dr->
		let ndr=self#new_drawing() in
		let i=ref 0 in
		  List.iter (
		    fun c1->
		      let col1=get_draw_op_color col1_l !i and
			  col2=get_draw_op_color col2_l !i in
			(try
			   ndr#set_t ((ndr#exec_op_copy "color_change" 
					 [DrawValColor col1;
					  DrawValColor col2]).(0)#get_t)
			 with Drawing_not_initialized ->
			   ndr#set_t ((dr#exec_op_copy "color_change" 
					 [DrawValColor col1;
					  DrawValColor col2]).(0)#get_t)
			);
			i:= !i+1;
		  ) col1_l;
		  ndr
	    ) drl;    
      )




end;;


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
