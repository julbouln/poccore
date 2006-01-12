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


open Str;;

open Value_common;;
open Value_val;;
open Value_lua;;
open Value_xml;;


open Core_val;;
open Core_rect;;

open Core_font;;
open Core_drawing;;

open Binding;;



let medias_dir=(Filename.dirname(Sys.executable_name));;

(** Media objects *)


(** {2 General function (FIXME: must fall in another file)} *)


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
  val mutable layer=0
  method set_layer l=layer<-l
  method get_layer=layer

  val mutable rect=new rectangle 0 0 0 0
  method get_rect=rect

  method virtual move : int -> int -> unit
  method virtual put : unit -> unit

  method print_info()=
    print_string (" * position: "^string_of_int rect#get_x^" - "^string_of_int rect#get_y);print_newline();
    print_string (" * size: "^string_of_int rect#get_w^" - "^string_of_int rect#get_h);print_newline();
    print_string (" * layer: "^string_of_int layer);print_newline();
end;;

class canvas =
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
	  && (Oo.id o)=(Oo.id od)
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
(*     | FontTTF (f,s)->(f^"_"^string_of_int s) *)
     | FontTTF (f,s)->f
     | FontEmbed->"font_embed");;

let get_font_size fnt_t=
  (match fnt_t with 
     | FontTTF (f,s)->s
     | FontEmbed->8);;


class font_ttf font_vault f s=
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




exception Drawing_script_error;;

class drawing_script (drawing_vault : binding_drawing_vault)=
object(self)
  inherit lua_object as super
  method get_id="drawing_script"

  val mutable drv=Hashtbl.create 2
  method add_dr n dr=Hashtbl.add drv n dr
  method get_dr n=Hashtbl.find drv n
  method del_dr n=Hashtbl.remove drv n
  method repl_dr n dr=Hashtbl.replace drv n dr

  method rename on nn=
    let o=self#get_dr on in
    self#add_dr nn o;
    self#del_dr on;

  method dr_size n=
    let dr=(self#get_dr n) in
      Array.length dr

  method create_from_vault vid=
    let did=(random_string "drscr" 20) in
    let drs=(drawing_vault#get_cache vid) in
      self#add_dr did drs;
      did

  method op_create name args=
    let did=(random_string "drscr" 20) in
    let dr=drawing_vault#new_drawing() in
      dr#exec_op_create_from_format name (ValLua args);
      self#add_dr did [|dr|];
      did

  method op_copy did nd name args=
    let dr=(self#get_dr did).(nd) in
    let ndid=(random_string "drscr" 20) in
    let ndr=dr#exec_op_copy_from_format name (ValLua args) in
      self#add_dr ndid ndr;
      ndid

  method op_push pid did nd name args=
    let dr=(self#get_dr did).(nd) in
    let drr=(self#get_dr pid) in
    let ndr=dr#exec_op_copy_from_format name (ValLua args) in
    let na=DynArray.of_array drr in
    Array.iter (fun v->
      DynArray.add na v;
	       ) ndr;
      self#repl_dr pid (DynArray.to_array na)

(*      self#add_dr pid ndr; *)
      

  method op_write did nd name args=
    let dr=(self#get_dr did).(nd) in
      dr#exec_op_write_from_format name (ValLua args)

  method op_read did nd name args=
    let dr=(self#get_dr did).(nd) in
      dr#exec_op_read_from_format name (ValLua args)


  method register_with_val v=
    (try 
       let did=(List.nth v 0) in
	 match did with
	   | OLuaVal.String n->
	       self#get_dr n
		 
	   | _ -> raise Drawing_script_error
     with Not_found -> raise Drawing_script_error)

  method register ds=
    let did=List.nth (lua#parse ds) 0 in
      match did with
	| OLuaVal.String n->
	       self#get_dr n

	| _ -> raise Drawing_script_error

  method register_with_args ds (args:OLuaVal.table)=
    lua#set_val (OLuaVal.String "args") (OLuaVal.Table args);
    self#register ds;


  method lua_init_external (lua:lua_obj)=

      lua#set_val (OLuaVal.String "drawing_script") 
	(OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) 
	   (fun()->""));

    lua#set_val (OLuaVal.String "create_from_vault") 
      (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) 
	 (self#create_from_vault));

    lua#set_val (OLuaVal.String "create") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.table **->> OLuaVal.string) 
	 (fun n a->
	    self#op_create n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      )
	 )
      );

    lua#set_val (OLuaVal.String "copy") 
      (OLuaVal.efunc (OLuaVal.string **->OLuaVal.int **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.string) 
	 (fun did nd n a->
	    self#op_copy did nd n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      )
	 )
      );

    lua#set_val (OLuaVal.String "push") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->OLuaVal.int **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun pid did nd n a->
	    self#op_push pid did nd n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      )
	 )
      );

    lua#set_val (OLuaVal.String "write") 
      (OLuaVal.efunc (OLuaVal.string **->OLuaVal.int **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun did nd n a->
	    self#op_write did nd n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      )
	 )
      );

    lua#set_val (OLuaVal.String "read") 
      (OLuaVal.efunc (OLuaVal.string **->OLuaVal.int **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.value) 
	 (fun did nd n a->
	    let v=
	    self#op_read did nd n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      ) in
	      lua_of_val_ext v
	 )
      );

    lua#set_val (OLuaVal.String "size") 
      (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.int) (self#dr_size));

    lua#set_val (OLuaVal.String "rename") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#rename));



  method lua_init()=
    lua#set_val (OLuaVal.String "create_from_vault") 
      (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) 
	 (self#create_from_vault));

    lua#set_val (OLuaVal.String "create") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.table **->> OLuaVal.string) 
	 (fun n a->
	    self#op_create n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      )
	 )
      );

    lua#set_val (OLuaVal.String "copy") 
      (OLuaVal.efunc (OLuaVal.string **->OLuaVal.int **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.string) 
	 (fun did nd n a->
	    self#op_copy did nd n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      )
	 )
      );

    lua#set_val (OLuaVal.String "push") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->OLuaVal.int **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun pid did nd n a->
	    self#op_push pid did nd n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      )
	 )
      );

    lua#set_val (OLuaVal.String "write") 
      (OLuaVal.efunc (OLuaVal.string **->OLuaVal.int **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.unit) 
	 (fun did nd n a->
	    self#op_write did nd n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      )
	 )
      );

    lua#set_val (OLuaVal.String "read") 
      (OLuaVal.efunc (OLuaVal.string **->OLuaVal.int **-> OLuaVal.string **-> OLuaVal.table **->> OLuaVal.value) 
	 (fun did nd n a->
	    let v=
	    self#op_read did nd n 
	      (let lo=new lua_obj in
		 lo#from_table a;
		 lo
	      ) in
	      lua_of_val_ext v
	 )
      );

    lua#set_val (OLuaVal.String "size") 
      (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.int) (self#dr_size));

    lua#set_val (OLuaVal.String "rename") 
      (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) (self#rename));



    super#lua_init();
    

end;;


let add_drawing_fun_from_drawing_script drawing_vault n s =
  let drs=new drawing_script drawing_vault in
    ignore(drs#lua_init());
    drawing_vault#add_drawing_fun_from_format n TValLua
      ( fun v->
	  match v with
	    | ValLua l->drs#register_with_args s l#to_table;
	    | _ -> raise Drawing_script_error
      );;

(* WORKS *)
(*
add_drawing_fun_from_drawing_script "testds" 
"
dr=self.create(\"rect\",{self.args[0],self.args[1]});
return dr
";;
*)
