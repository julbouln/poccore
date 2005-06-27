(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

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



open Core_rect;;
open Core_cache;;

open Value_common;;
open Value_lua;;
open Value_xml;;
open Value_val;;

open Core_val;;


(** Drawing operations *)

(** {2 Exceptions} *)

exception Draw_op_not_found of string;;
exception Draw_op_val_error of int;;
exception Draw_op_error;;
exception Bad_draw_op_result;;
exception Drawing_not_initialized;;
exception Drawing_not_found of string;;


(** {2 Types} *)

type color=(int*int*int);;

(* use oval *)
(*type ('t) val_drawing=
    [
      val_ext
    | `DrawingSizeFloat of (float*float) 
    | `DrawingRectangle of rectangle

    | `DrawingList of ('t) val_drawing list
    ];;
*)

type draw_op_t=
  | DrawTypeCreate
  | DrawTypeWrite
  | DrawTypeRead
  | DrawTypeCopy;;

type ('t) draw_op_result=
  | DrawResultUnit of unit
  | DrawResultT of 't
  | DrawResultTArray of 't array
  | DrawResultVal of val_ext;;

let get_draw_op_result_unit ores=match ores with
  | DrawResultUnit x->x
  | _ -> raise Bad_draw_op_result;;
let get_draw_op_result_t ores=match ores with
  | DrawResultT x->x
  | _ -> raise Bad_draw_op_result;;
let get_draw_op_result_t_array ores=match ores with
  | DrawResultTArray x->x
  | _ -> raise Bad_draw_op_result;;
let get_draw_op_result_val ores=match ores with
  | DrawResultVal x->x
  | _ -> raise Bad_draw_op_result;;

(** {2 Classes} *)

class ['t] draw_ops=
object(self)
  val mutable ops=Hashtbl.create 2

  method add_op (n:string) (o_t:draw_op_t) (o:(val_ext_handler->('t) draw_op_result))=
    Hashtbl.add ops n (o_t,o)

  method get_op n=
    (try
       Hashtbl.find ops n
     with Not_found -> raise (Draw_op_not_found n))

  method add_op_from_list n o_t o=
    let no (l:val_ext_handler)=
      o (list_of_val_ext_handler l) in  
      self#add_op n o_t no

(*
  method add_op_from_format n o_t ft o=
    let no (l:val_ext_handler)=
      o (format_of_val_ext_handler l ft) in  
      self#add_op n o_t no
*)
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


(** from list *)

  method exec_op_from_list n args=
    self#exec_op n (val_ext_handler_of_list args)


  method exec_op_create_from_list n args=
    get_draw_op_result_unit (self#exec_op_from_list n args)

  method exec_op_write_from_list n args=
    get_draw_op_result_unit (self#exec_op_from_list n args)

  method exec_op_read_from_list n args=
    get_draw_op_result_val (self#exec_op_from_list n args)

  method exec_op_copy_from_list n args=
    let cr=(match (self#exec_op_from_list n args) with
	      | DrawResultT ct ->[|self#new_t ct|];
	      | DrawResultTArray cta->
		  Array.map (
		    fun nt->
		      self#new_t nt
		  ) cta
	      | _ -> raise Draw_op_error) in
      cr

(** from format *)

  method exec_op_from_format n args=
    self#exec_op n (val_ext_handler_of_format args)


  method exec_op_create_from_format n args=
    get_draw_op_result_unit (self#exec_op_from_format n args)

  method exec_op_write_from_format n args=
    get_draw_op_result_unit (self#exec_op_from_format n args)

  method exec_op_read_from_format n args=
    get_draw_op_result_val (self#exec_op_from_format n args)

  method exec_op_copy_from_format n args=
    let cr=(match (self#exec_op_from_format n args) with
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

  initializer
(*    self#add_op_from_list "create" DrawTypeWrite (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0) and
	    col=color_of_val (List.nth ovl 1) in
	  (*	print_string ("DRAWING_OBJECT: load "^f);print_newline(); *)
	  self#create w h col;
	  DrawResultUnit();
    );
*)
    self#add_op_from_list "put_pixel" DrawTypeWrite (
      fun ovl->
	let (x,y)=position_of_val (List.nth ovl 0) and
	    col=color_of_val (List.nth ovl 1) in
	  (*	print_string ("DRAWING_OBJECT: load "^f);print_newline(); *)
	  self#put_pixel x y col;
	  DrawResultUnit();
    );

    self#add_op_from_list "get_pixel" DrawTypeRead (
      fun ovl->
	let (x,y)=position_of_val (List.nth ovl 0) in
	  (*	print_string ("DRAWING_OBJECT: load "^f);print_newline(); *)
	let col=self#get_pixel x y in
	  DrawResultVal(`Color(col))
    );


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
  method virtual fcompose:('t) drawing_object -> int -> int -> unit
end;;

class virtual ['t] drawing_handler=
object(self)
  val mutable drs=Hashtbl.create 2

  method virtual new_drawing : unit -> ('t) drawing_object
  method virtual new_drawing_screen : unit -> ('t) drawing_screen

  method add_drawing_fun (n:string) (o:val_ext_handler->('t) drawing_object array)=
(*    print_string ("DRAWING_HANDLER: add drawing fun "^n);print_newline(); *)
    Hashtbl.add drs n o

  method add_drawing_fun_from_list (n:string) (o:val_ext list->('t) drawing_object array)=
    let no (l:val_ext_handler)=
      o (list_of_val_ext_handler l) in
    self#add_drawing_fun n no


  method add_drawing_fun_from_format (n:string) ft (o:(val_ext) val_format->('t) drawing_object array)=
    let no (l:val_ext_handler)=
      o (format_of_val_ext_handler l ft) in
      self#add_drawing_fun n no


  method get_drawing_fun n=
    (try 
       Hashtbl.find drs n
     with Not_found-> raise (Drawing_not_found n));

  method call_drawing_fun (args:val_ext_handler)=
    let fargs=args#to_list() in
    let n=string_of_val (List.nth fargs 0) in
    let drf=self#get_drawing_fun n in
    let nargs=new val_ext_handler in
      nargs#from_list (List.tl fargs);
      drf nargs
    
  method exec_drawing_fun (n:string) (args:val_ext_handler)=
    let drf=self#get_drawing_fun n in
      drf args

  method exec_drawing_fun_from_list (n:string) (args:val_ext list)=
    self#exec_drawing_fun n (val_ext_handler_of_list args)

  method exec_drawing_fun_from_format (n:string) (args:(val_ext) val_format)=
    self#exec_drawing_fun n (val_ext_handler_of_format args)


end;;

class virtual ['t] drawing_cache s mt=
object
  inherit [('t) drawing_object] medias_cache s mt
end;;

class virtual ['t] drawing_vault cache_size mt=
object(self)
  inherit ['t] drawing_cache cache_size mt as cache
  inherit ['t] drawing_handler

  (* link between cache & handler *)
(*
  method add_cache_from_drawing_fun (n:string) (args:val_ext_handler)=
    let fargs=args#to_list() in
    if self#is_cache_fun (string_of_val (List.nth fargs 0))=false then 
      let drl=(fun()->self#call_drawing_fun args) in    
	self#add_cache n drl;
*)
  method add_cache_from_drawing_fun (n:string) (fargs:val_ext list)=
    let args=val_ext_handler_of_list fargs in
    if self#is_cache_fun (string_of_val (List.nth fargs 0))=false then 
      let drl=(fun()->self#call_drawing_fun args) in    
	self#add_cache n drl;


  method add_cache_from_drawing_fun_fmt (n:string) (fargs:(val_ext) val_format)=
    let args=val_ext_handler_of_format fargs in
    let largs=list_of_val_ext_handler args in
    if self#is_cache_fun (string_of_val (List.nth largs 0))=false then 
      let drl=(fun()->self#call_drawing_fun args) in    
	self#add_cache n drl;

  method add_cache_from_drawing_fun_fmt_auto (fargs:(val_ext) val_format)=
    let args=val_ext_handler_of_format fargs in
    let largs=list_of_val_ext_handler args in
    let n=ref ("cache") in
(*^string_of_int (randomize 2048)) in *)
      List.iter (
	fun v->
	  match v with
	    | `String s->n:= (!n^"_"^s);
	    | `Int i->n:= (!n^"_"^string_of_int i);
	    | _ ->()
      ) largs;
      
      
      if self#is_cache_fun (string_of_val (List.nth largs 0))=false then (
	let drl=(fun()->self#call_drawing_fun args) in    
	  self#add_cache !n drl;
      );
	  !n


  initializer
    (** generic drawing creation functions *)

    self#add_drawing_fun_from_list "load_simple"
      (
	fun vl->
	  let file=List.nth vl 0 in
	    
	  let dr=self#new_drawing() in
	    dr#exec_op_create_from_list "load" [file];
	    [|dr|]
      );

    self#add_drawing_fun_from_list "load_multiple" 
      (
	fun vl->
	  let file=List.nth vl 0 and
	      size=List.nth vl 1 in
	    
	  let dr=self#new_drawing() in
	    dr#exec_op_create_from_list "load" [file];
	    dr#exec_op_copy_from_list "split" [size];
      );

    self#add_drawing_fun_from_list "create_multiple" 
      (
	fun vl->
	  let cached=string_of_val (List.nth vl 0) and
	      size=List.nth vl 1 in
	    
	  let dr=(self#get_cache_simple cached) in
	    dr#exec_op_copy_from_list "split" [size];
      );

(* very exciting !!*)
    self#add_drawing_fun_from_list "with_alpha"
      (
	fun vl->
	  let col=List.nth vl (0) and
	      par=string_of_val (List.nth vl 1) in	      
	  let drl=self#exec_drawing_fun_from_list par (snd (ExtList.List.split_nth 2 vl)) in
	    Array.iter (
	      fun dr->
		dr#exec_op_write_from_list "set_alpha" [col]
	    ) drl;
	    drl
      );

    self#add_drawing_fun_from_list "with_border"
      (
	fun vl->
	  let col=color_of_val (List.nth vl 0) and
	      par=string_of_val (List.nth vl 1) in	      
	  let drl=self#exec_drawing_fun_from_list par (snd (ExtList.List.split_nth 2 vl)) in
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



    self#add_drawing_fun_from_list "with_mirror3"
      (
	fun vl->
	  let par=string_of_val (List.nth vl 0) in
	  let drl=self#exec_drawing_fun_from_list par (List.tl vl) in
	  let column=(Array.length drl)/3 in
	  let ndrl=Array.make (column*8) drl.(0) in
	    Array.iteri (
	      fun i dr->
		ndrl.(i)<-dr;
	    ) drl;
	    let copy_col v m mirror=
	      let k=ref 0 in
		for i=v*column to (v+1)*column - 1 do
		  ndrl.(column*m + !k)<-(if mirror then ((drl.(i))#exec_op_copy_from_list "mirror" []).(0) else drl.(i));
		  k:= !k+1;
		done
	in
	      copy_col 2 4 false;
	      copy_col 1 6 true;
	      copy_col 1 2 false;
	      
	      ndrl

  );


    self#add_drawing_fun_from_list "with_mirror5"
      (
	fun vl->
	  let par=string_of_val (List.nth vl 0) in
	  let drl=self#exec_drawing_fun_from_list par (List.tl vl) in
	  let column=(Array.length drl)/5 in
	  let ndrl=Array.make (column*8) drl.(0) in
	    Array.iteri (
	      fun i dr->
		ndrl.(i)<-dr;
	    ) drl;
	    let copy_col v m mirror=
	      let k=ref 0 in
		for i=v*column to (v+1)*column - 1 do
		  ndrl.(column*m + !k)<-(if mirror then ((drl.(i))#exec_op_copy_from_list "mirror" []).(0) else drl.(i));
		  k:= !k+1;
		done
	in
	      copy_col 3 5 true;
	      copy_col 2 6 true;
	      copy_col 1 7 true;
	      
	      ndrl

  );



    self#add_drawing_fun_from_list "with_color_change"
      (
	fun vl->
	  let col1_l=list_of_val (List.nth vl 0) and
	      col2_l=list_of_val (List.nth vl 1) and
	      par=string_of_val (List.nth vl 2) in
	  let drl=self#exec_drawing_fun_from_list par (snd (ExtList.List.split_nth 3 vl)) in
	    Array.map (
	      fun dr->
		let ndr=self#new_drawing() in
		let i=ref 0 in
		  List.iter (
		    fun c1->
		      let col1=color_of_val (List.nth col1_l !i) and
			  col2=color_of_val (List.nth col2_l !i) in
			(try
			   ndr#set_t ((ndr#exec_op_copy_from_list "color_change" 
					 [`Color col1;
					  `Color col2]).(0)#get_t)
			 with Drawing_not_initialized ->
			   ndr#set_t ((dr#exec_op_copy_from_list "color_change" 
					 [`Color col1;
					  `Color col2]).(0)#get_t)
			);
			i:= !i+1;
		  ) col1_l;
		  ndr
	    ) drl;    
      )




end;;



