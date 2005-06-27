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

open Core_val;;
open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_font;;
open Core_drawing;;

open Core_fun;;

open Binding;;



(** Graphic classes *)


exception Drawing_id_not_set;;

(** {2 Graphic} *)

(** Graphic object class parent *)
class graphic_object=
  object (self)
    inherit generic_object as go
    inherit canvas_object
    inherit lua_object as lo

    val mutable cargs=new val_ext_handler
    method set_args a=cargs<-a
    method get_args=cargs

    val mutable fnode=new core_fun_node
    method get_fnode=fnode
      
    method fun_init()=
      fnode#set_fun self#functionize

    val mutable update_fun=fun l->[OLuaVal.Nil]

    val mutable showing=true
    method show()=showing<-true
    method hide()=showing<-false
    method is_showing=showing

    val mutable drawing_id=None
    method set_drawing_id did=drawing_id<-(Some did)
    method get_drawing_id=
      match drawing_id with
	| Some did->did
	| None -> raise Drawing_id_not_set
	
    val mutable cur_drawing=0

    method get_drawing n=
      (drawing_vault#get_cache_entry self#get_drawing_id n)

    method set_cur_drawing c=cur_drawing<-c
    method get_cur_drawing=cur_drawing
    method get_drawings_size=(Array.length (drawing_vault#get_cache self#get_drawing_id))

    (** canvas *)

    method move x y=
      rect#set_position x y 

    method put()=
      if self#is_showing then (
	let t=self#get_drawing cur_drawing in
	  video#get_drawing#compose t rect#get_x rect#get_y
      )


(** for fun *)
    method get_x()=rect#get_x
    method get_y()=rect#get_y
    method get_w()=rect#get_w
    method get_h()=rect#get_h 
    method cur_drawing()=self#get_cur_drawing
    method drawings_size()=self#get_drawings_size

  method functionize : functionizer=
    `GraphicFun 
      (self :> graphic_fun)
(*      move=self#move;
      get_x=(fun()->rect#get_x);
      get_y=(fun()->rect#get_y);
      get_w=(fun()->rect#get_w);
      get_h=(fun()->rect#get_h);
      set_cur_drawing=self#set_cur_drawing;
      get_cur_drawing=(fun()->self#get_cur_drawing);
      get_drawings_size=(fun()->self#get_drawings_size);
      show=self#show;
      hide=self#hide;
      set_layer=self#set_layer;
    }
*)
    method on_update()=
      ignore(lua#exec_val_fun (OLuaVal.String "on_update") [OLuaVal.Nil]);
(*      ignore(update_fun [OLuaVal.Nil]) *)

    method lua_init()=
      lua#set_val (OLuaVal.String "move") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#move);
      lua#set_val (OLuaVal.String "get_x") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_x));
      lua#set_val (OLuaVal.String "get_y") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_y));
      lua#set_val (OLuaVal.String "get_w") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_w));
      lua#set_val (OLuaVal.String "get_h") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_h));
      lua#set_val (OLuaVal.String "set_cur_drawing") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#set_cur_drawing);
      lua#set_val (OLuaVal.String "get_cur_drawing") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_cur_drawing));

     lua#set_val (OLuaVal.String "get_drawing_id") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_drawing_id));
      lua#set_val (OLuaVal.String "get_drawings_size") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_drawings_size));

      lua#set_val (OLuaVal.String "show") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#show);
      lua#set_val (OLuaVal.String "hide") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#hide);

      lua#set_val (OLuaVal.String "set_layer") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#set_layer);

      lua#set_val (OLuaVal.String "creation_args") (OLuaVal.Table cargs#to_lua#to_table);

      lo#lua_init();
      update_fun<-lua#get_fun (OLuaVal.String "on_update");

  end;;


(** Graphic from drawing *)
class graphic_from_drawing did (f)=
object(self)
  inherit graphic_object

  initializer
    self#set_drawing_id did;
    drawing_vault#add_cache did f;
    let dra=drawing_vault#get_cache_simple did in
    rect#set_size (dra#get_w) (dra#get_h); 
    
end;;

(** Graphic from drawing fun (with format)*)
class graphic_from_drawing_fun_fmt args=
object(self)
  inherit graphic_object

  initializer
  let did=drawing_vault#add_cache_from_drawing_fun_fmt_auto args in
    let dra=drawing_vault#get_cache_simple did in
      rect#set_size (dra#get_w) (dra#get_h);
      self#set_drawing_id did;

  method private reinit args=
  let did=drawing_vault#add_cache_from_drawing_fun_fmt_auto args in
    let dra=drawing_vault#get_cache_simple did in
      rect#set_size (dra#get_w) (dra#get_h);
      self#set_drawing_id did;

end;;

(** Graphic from drawing fun *)
class graphic_from_drawing_fun did args=
object(self)
  inherit graphic_object

  initializer
    drawing_vault#add_cache_from_drawing_fun did args;

    let dra=drawing_vault#get_cache_simple did in
      rect#set_size (dra#get_w) (dra#get_h);
      self#set_drawing_id did;

  method private reinit ndid nargs=
    drawing_vault#add_cache_from_drawing_fun ndid nargs;

    let dra=drawing_vault#get_cache_simple ndid in
      rect#set_size (dra#get_w) (dra#get_h);
      self#set_drawing_id ndid;
end;;

(** Graphic from a file *)
class graphic_from_file file w h=
object(self)
(*  inherit graphic_from_drawing_fun file 
    [
      `String "with_alpha";
      `Color(255,255,255);
      `String "load_multiple";
      `String file;
      `Size(w,h);
    ]
*)

  inherit graphic_from_drawing_fun_fmt
    (ValList [
      `String "with_alpha";
      `Color(255,255,255);
      `String "load_multiple";
      `String file;
      `Size(w,h);
    ])
end;;

(** Graphic resized from a file *)
class graphic_resized_from_file file i w h iw ih=
object
  val fgr=new graphic_from_file file iw ih
  inherit graphic_from_drawing (file^"_"^string_of_int i^"_resized") 
    (fun()->
       let dr=(drawing_vault#get_cache_entry file i)#copy() in
	 dr#exec_op_copy_from_list "resize" 
	   [
	     `Size(
	       int_of_float ((float_of_int w)/.(float_of_int iw) *.100.),
	       int_of_float ((float_of_int h)/.(float_of_int ih) *.100.)
	     )
	   ]	 
    )
end;;

(** Graphic resized from a drawing *)
class graphic_resized pdraw i fw fh=
object
  inherit graphic_from_drawing (pdraw^"_"^string_of_int i^"_resized") 
    (fun()->
       let dr=(drawing_vault#get_cache_entry pdraw i)#copy() in
	 dr#exec_op_copy_from_list "resize" [
	  `Size(
	    int_of_float (fw*.100.),
	    int_of_float (fh*.100.)
	  )
	 ]	 
    )
end;;

(** {2 Text} *)

(** utf8 *)
class utf8=
object(self)
  val mutable str=""

  method set v=str<-v
  method get=str

  method length=UTF8.length str
  method sub p l=
    String.sub str (self#byte_get p) ((self#byte_get (p+l))-(self#byte_get p))

  method byte_get n=UTF8.nth str n
  method byte_length=String.length str

end;;


let digest_of_string_list txt=
  (Digest.to_hex(Digest.string (String.concat "" txt)));;



let string_of_color c=
  let (r,g,b)=c in
    ("("^string_of_int r^","^string_of_int g^","^string_of_int b^")");;

let string_of_fnt_t f=
  match f with
    | FontTTF(f,s)->(f^string_of_int s)
    | FontEmbed -> "font_embed8";;


class graphic_object_text fnt_t (txt:string list) color=
object
  inherit graphic_from_drawing 
("text_"^string_of_fnt_t fnt_t^"_"^string_of_color color^"_"^digest_of_string_list txt)
(*((random_string "text_" 10)^digest_of_string_list txt)*)
    (fun()->
(*
	 let fnt_n=get_font_id fnt_t in
	   font_vault#add_cache (fnt_n) (
	     fun()->
	       let fnt=font_vault#new_font() in
		 fnt#load fnt_t;
		 [|fnt|]
	   ); *)
	 let fnt_n=get_font_id fnt_t and
	     fnt_s=get_font_size fnt_t in
	   Array.map
	     (
	       fun tx->
		 let  dr=drawing_vault#new_drawing() in
		   dr#exec_op_create_from_list "create_text" 
		     [
		       `String fnt_n;
		       `Int fnt_s;
		       `String tx;
		       `Color color
		     ];
		   dr#exec_op_write_from_list "set_alpha" [`Color (255,255,255)];
		   
		   dr
	     ) (Array.of_list txt)
    )
end;;


(** graphic text object *)
class graphic_text did fnt_t (col:color)=
object(self)
  inherit graphic_object as super

  initializer 
    self#set_drawing_id did

  val mutable graphic=new graphic_object_text fnt_t ([did]) col;
  val mutable fnt=(font_vault#get_cache_simple ((get_font_id fnt_t)^string_of_int (get_font_size fnt_t)))
 
  val mutable color=col
  method get_color=color
  method set_color c=color<-c

  val mutable lines=1
  method set_lines l=lines<-l
  method get_lines=lines


  val mutable max_size=1000
  method set_max_size s=max_size<-s
  method get_max_size=max_size

  val mutable text=[""]
  method get_text=text


  method lines_size (l:int)=
    let si=ref 0 in
    let i=ref 0 in
    List.iter (
      fun s->
	let utf=new utf8 in
	  utf#set s;
	if !i<l then
	  si:= !si+utf#length;
	i:= !i+1;
    ) text;
      !si

  method line_of_pos (p:int)=
    let si=ref 0 in
    let l=ref 0 in
    let i=ref 0 in
    List.iter (
      fun s->
	let utf=new utf8 in
	  utf#set s;
	  if p> !si && p<= !si+utf#length then
	    l:= !i;
	  
	  si:= !si+utf#length;
	  i:= !i+1;
    ) text;
      !l



  (* NEW cut_string *)
  method private cut_string2 is=
    let nlist=(Str.split_delim (regexp "[\n]+") is) in

    let a=DynArray.create() in      
    let cl=ref 0 in
      List.iter (
	fun s->
	  let utf=new utf8 in
	    utf#set s;
	    let cp=ref 0 and
		lp=ref 0 and
		cs=ref 0 and

		str=ref "" in
	      
	      
	      while !cl<lines && !cp<utf#length do
		while !cs<max_size && !cp<utf#length do
		  (
		    str:= (utf#sub !lp (!cp- !lp));
		    let (cw,ch)=fnt#sizeof_text (!str) in 
		      cs:=cw;
		      cp:= !cp+1;
		  )
		done;
		str:= (utf#sub !lp (!cp- !lp));
		DynArray.add a !str;
		cl:= !cl+1;
		lp:= !cp;
		cs:=0;
	      done;

      ) nlist;

      DynArray.to_list a 
	
  (* OLD cut_string *)	  
  method private cut_string s=
    (*    text<-split_delim (regexp "[\n]+") s; *)
    let a=DynArray.create() in
    let ss=UTF8.length s/max_size in
      for i=0 to ss do

	let md=UTF8.length s - (i*max_size) in
	let l=if md>=max_size then max_size else md in
	let cs=UTF8.nth s (i*max_size) and
	    ce=UTF8.nth s ((i*max_size) + l) in
	    let ns=String.sub s cs (ce-cs) in
	      if String.length ns>0 then
		DynArray.add a ns
      done;
      DynArray.to_list a
	

  method set_text t=

    if t="" then (
      text<-[""];
      graphic<-new graphic_object;
    )
    else (
      text<-self#cut_string2 t;
      graphic<-new graphic_object_text fnt_t (self#get_text) self#get_color;
    );

(*    print_string "taille: ";
    print_int (List.length (self#get_text));
    print_newline();
*)  

(*      new graphic_dyn_object (id^"/text") (List.length self#get_text)
      (function k-> (
	 fnt#create_text (List.nth self#get_text k) self#get_color
       ));
*)
    let cw=ref 0 and
      ch=ref 0 in
      for i=0 to (List.length (self#get_text))-1 do
	let pos=fnt#sizeof_text (List.nth (self#get_text) i) in
	  if !cw<(fst pos) then
	    cw:=(fst pos);
	  ch:=!ch + (snd pos);
      done;
      rect#set_size (!cw) (!ch);
      

  method put()=
    if showing then 
    for i=0 to (List.length self#get_text)-1 do
      graphic#move (rect#get_x) (rect#get_y+(i*fnt#get_height));
      graphic#set_cur_drawing i;
      graphic#put();	
      
    done;

  method lua_init()=
      lua#set_val (OLuaVal.String "set_text") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#set_text);

      lua#set_val (OLuaVal.String "set_size") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#set_max_size);
      lua#set_val (OLuaVal.String "set_lines") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#set_lines);
(*      lua#set_val (OLuaVal.String "get_text") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->String.concat self#get_text)); *)
      super#lua_init();

end;;

(** {2 Pattern} *)

(** special graphic pattern resize with 9 tiles *)
class graphic_pattern_old pdrawid=
object(self)
  inherit graphic_object as super

  val mutable gr=new graphic_object

  val mutable crect=new rectangle 0 0 0 0
  method get_crect=crect


  method private init()=
    let pdrawing=(drawing_vault#get_cache_simple pdrawid) in
    let cw=pdrawing#get_w and
	ch=pdrawing#get_h in
      crect#set_size (cw/3) (ch/3);

      gr<-new graphic_from_drawing_fun_fmt
	(ValList [
	  `String "with_alpha";
	  `Color(255,255,255);
	  `String "create_multiple";
	  `String pdrawid;
	  `Size(crect#get_w,crect#get_h);
	]);

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


(** special graphic pattern resize with 9 tiles *)
class graphic_pattern pdrawid=
object(self)
  inherit graphic_object as super

  val mutable gr=new graphic_object

  val mutable crect=new rectangle 0 0 0 0
  method get_crect=crect


  method private init()=
    let pdrawing=(drawing_vault#get_cache_simple pdrawid) in
    let cw=pdrawing#get_w and
	ch=pdrawing#get_h in
      crect#set_size (cw/3) (ch/3);

      gr<-new graphic_from_drawing_fun_fmt
	(ValList [
(*	  `String "with_alpha";
	  `Color(255,255,255);
*)
	  `String "create_multiple";
	  `String pdrawid;
	  `Size(crect#get_w,crect#get_h);
	]);

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

  val mutable ocw=0
  val mutable och=0

  method create_dr()=
    let cw=rect#get_w/crect#get_w -1 and
	ch=rect#get_h/crect#get_h -1 in

      if cw<>ocw or ch<>och then (
	let rdr=drawing_vault#new_drawing() in
	  rdr#create ((cw+1)*crect#get_w) ((ch+1)*crect#get_h) (255,255,255);
(*
	  rdr#exec_op_create_from_list "box" [`Size (((cw+1)*crect#get_w),((ch+1)*crect#get_h)); `Color (255,255,255)];
*)


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
	      let dr=gr#get_drawing (gr#get_cur_drawing) in
		rdr#compose dr ((i*crect#get_w)) ((j*crect#get_h));
	    done
	  done;
	  rdr#exec_op_write_from_list "set_alpha" [`Color (255,255,255)];
	  let did=(pdrawid^(string_of_int cw)^"x"^(string_of_int ch)) in
	    drawing_vault#add_cache did (fun()->[|rdr|]);
	    self#set_drawing_id did;
	    ocw<-cw;
	    och<-ch;
      )
  method put()=
    self#create_dr();
    super#put();
	

end;; 

class graphic_pattern_file pfile=

  
  let did=drawing_vault#add_cache_from_drawing_fun_fmt_auto 
    (ValList  [
(*       `String "with_alpha";
       `Color(255,255,255);*)
       `String "load_simple";
       `String pfile
       ]) in
object(self)
  inherit graphic_pattern did

end;;



