open Str;;

open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_font;;
open Core_drawing;;

open Ocommon;;
open Oval;;
open Olua;;

open Binding;;



(** Graphic classes *)


exception Drawing_id_not_set;;

(** Graphic object class parent *)
class graphic_object=
  object (self)
    inherit generic_object
    inherit canvas_object
    inherit lua_object as lo


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


    method lua_init()=
      lua#set_val (OLuaVal.String "move") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#move);
      lua#set_val (OLuaVal.String "get_w") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_w));
      lua#set_val (OLuaVal.String "get_h") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_h));
      lua#set_val (OLuaVal.String "set_cur_drawing") (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) self#set_cur_drawing);
      lua#set_val (OLuaVal.String "get_drawings_size") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->self#get_drawings_size));

      lua#set_val (OLuaVal.String "show") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#show);
      lua#set_val (OLuaVal.String "hide") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#hide);

      lo#lua_init();
  end;;


class graphic_from_drawing did (f)=
object(self)
  inherit graphic_object

  initializer
    self#set_drawing_id did;
    drawing_vault#add_cache did f;
    let dra=drawing_vault#get_cache_simple did in
    rect#set_size (dra#get_w) (dra#get_h); 
    
end;;


class graphic_from_drawing_fun_fmt args=
object(self)
  inherit graphic_object

  initializer
  let did=drawing_vault#add_cache_from_drawing_fun_fmt_auto args in
    let dra=drawing_vault#get_cache_simple did in
      rect#set_size (dra#get_w) (dra#get_h);
      self#set_drawing_id did;
end;;


class graphic_from_drawing_fun did args=
object
  inherit graphic_object

  initializer
    drawing_vault#add_cache_from_drawing_fun did args;

    let dra=drawing_vault#get_cache_simple did in
      rect#set_size (dra#get_w) (dra#get_h);

end;;


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

class graphic_object_text fnt_t (txt:string list) color=
object
  inherit graphic_from_drawing 
("text_"^digest_of_string_list txt)
(*((random_string "text_" 10)^digest_of_string_list txt)*)
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
		   dr#exec_op_create_from_list "create_text" 
		     [
		       `String fnt_n;
		       `String tx;
		       `Color color
		     ];
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
  val mutable fnt=(font_vault#get_cache_simple (get_font_id fnt_t))
 
  val mutable color=col
  method get_color=color
  method set_color c=color<-c

  val mutable lines=1
  method set_lines l=lines<-l
  method get_lines=lines


  val mutable max_size=100
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
    for i=0 to (List.length self#get_text)-1 do
      graphic#move (rect#get_x) (rect#get_y+(i*fnt#get_height));
      graphic#set_cur_drawing i;
      graphic#put();	
      
    done;

  method lua_init()=
      lua#set_val (OLuaVal.String "set_text") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#set_text);
(*      lua#set_val (OLuaVal.String "get_text") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->rect#get_text)); *)
      super#lua_init();

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

class graphic_pattern_file pfile=

  
  let did=drawing_vault#add_cache_from_drawing_fun_fmt_auto 
      (ValList  [
	 `String "load_simple";
	 `String pfile
       ]) in
object(self)
  inherit graphic_pattern did

end;;



