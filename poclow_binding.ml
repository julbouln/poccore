open Core_rect;;
open Core_cache;;
open Core_event;;
open Core_font;;
open Core_drawing;;

open Oval;;


(** poclow binding *)

open Low;;

(** poclow font *)

class poclow_font_object=
object(self)
  inherit [font] font_object

  val mutable font_type=FontEmbed
  method get_font_type=font_type

  method load fontt=
    font_type<-fontt;
    match font_type with
      | FontTTF (f,s)->
	  self#set_f (font_load f s);
      | FontEmbed ->();
	  
    method get_size=
    match font_type with
      | FontTTF (f,s)->s
      | FontEmbed->8
    method get_height=
      match font_type with
	| FontTTF (f,s)->
	    font_height self#get_f
	| FontEmbed -> 8


    method sizeof_text txt=
	match font_type with
	  | FontTTF (f,s)->
	      font_sizeof self#get_f txt
	  | FontEmbed -> (String.length txt*8,8);

  end;;


class font_vault s=
object(self)
  inherit [poclow_font_object] medias_cache s (1.)

  method new_font()=new poclow_font_object

  method cache_file_save f dl=()
  method cache_file_load f=[||]
end;;


(* the font vault *)
let font_vault=new font_vault 100;;



(** poclow drawing *)

class poclow_drawing_object=
object(self)
  inherit [tile] drawing_object

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


  initializer

(** create ops *)

    self#add_op_from_list "load" DrawTypeCreate (
      fun ovl->
	let f=string_of_val (List.nth ovl 0) in
(*	print_string ("DRAWING_OBJECT: load "^f);print_newline(); *)
	DrawResultT (tile_load f);
    );

    self#add_op_from_list "rect" DrawTypeCreate (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0) in
	let col=color_of_val (List.nth ovl 1) in
	DrawResultT (tile_rect w h col);
    );

    self#add_op_from_list "box" DrawTypeCreate (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0) in
	let col=color_of_val (List.nth ovl 1) in
	DrawResultT (tile_box w h col);
    );

    self#add_op_from_list "create_text" DrawTypeCreate (
      fun ovl->
	let fnt_n=string_of_val (List.nth ovl 0)  and
	    txt=string_of_val (List.nth ovl 1)  and
	    color=color_of_val (List.nth ovl 2)  in
	let fnt=(font_vault#get_cache_simple fnt_n) in

	  DrawResultT
	    (match fnt#get_font_type with
	       | FontTTF f->
		   tile_text fnt#get_f txt color
	       | FontEmbed ->
		   let tmp=tile_box (String.length txt*8) 8 (255,255,255) in
		     tile_set_alpha tmp 255 255 255;
		     tile_string tmp (0,0) txt color; 
		     tmp
	    ));
(** copy ops *)

    self#add_op_from_list "mirror" DrawTypeCopy (
      fun ovl->
	DrawResultT (tile_mirror self#get_t);
    );

    self#add_op_from_list "split" DrawTypeCopy (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0)  in
	DrawResultTArray (tile_split self#get_t w h);
    );

    self#add_op_from_list "resize" DrawTypeCopy (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0)  in
	let fw=(float_of_int w)/.100. and
	    fh=(float_of_int h)/.100. in

	DrawResultT (tile_resize self#get_t fw fh);
    );

    self#add_op_from_list "color_change" DrawTypeCopy (
      fun ovl->      
      let col1=color_of_val (List.nth ovl 0) and
	  col2=color_of_val (List.nth ovl 1) in
	DrawResultT (tile_color_change self#get_t col1 col2)
    );


(** write op *)

    self#add_op_from_list "set_alpha" DrawTypeWrite (
      fun ovl->
	let (r,g,b)=color_of_val (List.nth ovl 0) in
	  (tile_set_alpha self#get_t r g b);
	  DrawResultUnit();
    );

    self#add_op_from_list "unset_alpha" DrawTypeWrite (
      fun ovl->
	(tile_unset_alpha self#get_t);
	DrawResultUnit();
    );

    self#add_op_from_list "line" DrawTypeWrite (
      fun ovl->
	let p1=position_of_val (List.nth ovl 0) and
	    p2=position_of_val (List.nth ovl 1)  and
	    col=color_of_val (List.nth ovl 2)  in
	  
	  (tile_line self#get_t p1 p2 col);
	  DrawResultUnit();
    );

    self#add_op_from_list "rectangle" DrawTypeWrite (
      fun ovl->
	let p1=position_of_val (List.nth ovl 0) and
	    p2=position_of_val (List.nth ovl 1)  and
	    col=color_of_val (List.nth ovl 2)  in
	  
	  (tile_rectangle self#get_t p1 p2 col);
	  DrawResultUnit();
    );

(** read op *)

    self#add_op_from_list "get_rpos" DrawTypeRead (
      fun ovl->
	let rcol=color_of_val (List.nth ovl 0) in
	let (x1,y1,x2,y2)=tile_refresh_pos self#get_t in
	  DrawResultVal(`List [`Position(x1,y1);`Position(x2,y2)])
    );

end;;

class poclow_drawing_screen=
object(self)
(*  inherit [tile] drawing_screen *)
  inherit poclow_drawing_object

  method init w h bpp fs=
    all_init();
(*    print_string "BINDING(poclow): init screen";print_newline(); *)
    self#set_t (video_init w h bpp fs)

  method refresh()=
    video_update()

  method set_caption s i=
    wm_set_caption s i

  method blank()=
    video_blank_screen()
 
  method set_clip x y w h=
    video_set_clip x y w h

  method show_cursor()=
    video_show_cursor()

  method hide_cursor()=
    video_hide_cursor()

end;;

let digest_of_string txt=
  (Digest.to_hex(Digest.string txt));;

class poclow_drawing_vault s mt=
object(self)
  inherit [tile] drawing_vault s mt
  method new_drawing()=new poclow_drawing_object
  method new_drawing_screen()=new poclow_drawing_screen

  method cache_file_save f dl=
    (* if no cache dir, create it *)
    if Sys.file_exists "cache"=false then
      Unix.mkdir "cache" 0o700;

    Array.iteri (
      fun i d->
	let fn=(digest_of_string f^"_"^string_of_int i^".bmp") in
	  if Sys.file_exists fn=false then (
	    d#exec_op_write_from_list "unset_alpha" [];
	    tile_save_bmp (d#get_t) ("cache/"^fn);
	    d#exec_op_write_from_list "set_alpha" [`Color (255,255,255)];
	  )
    ) dl;

  method cache_file_load f=
    let a=DynArray.create() in
    let i=ref 0 in
      while Sys.file_exists ("cache/"^(digest_of_string f^"_"^string_of_int !i^".bmp")) do
	let fn=(digest_of_string f^"_"^string_of_int !i^".bmp") in
	  DynArray.add a (
	    let nd=self#new_drawing() in	      
	      nd#set_t (tile_load_bmp ("cache/"^fn));
	      nd#exec_op_write_from_list "set_alpha" [`Color (255,255,255)];
	      nd
	  );
	i:= !i+1;
      done;
      DynArray.to_array a

    
end;;

(* the drawing vault *)
let drawing_vault=new poclow_drawing_vault 10000 (1./.25.);;


(* poclow event *)

let parse_unicode k=
  KeyUnicode (UChar.chr k);;

let parse_key k=
(*  print_string "poclow_key : ";print_int k;print_newline(); *)
  match k with 
    | 8 -> KeyBackspace
    | 9 -> KeyTab

    | 13 -> KeyReturn
    | 32 -> KeySpace
    | 27 -> KeyEchap
    | 273 -> KeyUp
    | 274 -> KeyDown
    | 275 -> KeyRight
    | 276 -> KeyLeft

    | 304 -> KeyShift
    | 306 -> KeyCtrl

    | 307 -> KeyAltR
    | 308 -> KeyAltL

    | 313 -> KeyAltGr

    | 33 -> KeyChar "!";

    | 36 -> KeyChar "$"

    | 39 -> KeyChar "'"
    | 40 -> KeyChar "("
    | 41 -> KeyChar ")"
    | 42 -> KeyChar "*"
    | 43 -> KeyChar "+"
    | 44 -> KeyChar ",";
    | 45 -> KeyChar "-"
    | 47 -> KeyChar "/"

    | 48 -> KeyChar "0"
    | 49 -> KeyChar "1";
    | 50 -> KeyChar "2";
    | 51 -> KeyChar "3";
    | 52 -> KeyChar "4";
    | 53 -> KeyChar "5";
    | 54 -> KeyChar "6";
    | 55 -> KeyChar "7";
    | 56 -> KeyChar "8";
    | 57 -> KeyChar "9";

    | 58 -> KeyChar ":";
    | 59 -> KeyChar ";";
    | 60 -> KeyChar "<";
    | 61 -> KeyChar "=";
    | 62 -> KeyChar ">";

    | 97 -> KeyChar "a";
    | 98 -> KeyChar "b";
    | 99 -> KeyChar "c";
    | 100 -> KeyChar "d";
    | 101 -> KeyChar "e";
    | 102 -> KeyChar "f";
    | 103 -> KeyChar "g";
    | 104 -> KeyChar "h";
    | 105 -> KeyChar "i";
    | 106 -> KeyChar "j";
    | 107 -> KeyChar "k";
    | 108 -> KeyChar "l";
    | 109 -> KeyChar "m";
    | 110 -> KeyChar "n";
    | 111 -> KeyChar "o";
    | 112 -> KeyChar "p";
    | 113 -> KeyChar "q";
    | 114 -> KeyChar "r";
    | 115 -> KeyChar "s";
    | 116 -> KeyChar "t";
    | 117 -> KeyChar "u";
    | 118 -> KeyChar "v";
    | 119 -> KeyChar "w";
    | 120 -> KeyChar "x";
    | 121 -> KeyChar "y";
    | 122 -> KeyChar "z";
    | _->KeyUnknow;;


let poclow_event_to_event a=
    (match a.etype with
     | "mouse" ->EventMouse
	 (match a.eval with
	    | "motion" -> 
		MouseMotion(a.ex,a.ey);
	    | "released" -> 
		MouseRelease(a.ex,a.ey,a.ebut);
	    | "pressed" -> 
		MouseClick(a.ex,a.ey,a.ebut);
	    | _ -> MouseError
	 )
     | "keyboard" ->EventKeyboard
	 (match a.eval with
	    | "pressed" -> 
		KeyboardPress (parse_key a.ebut,parse_unicode a.ey)
	    | "released" -> 
		KeyboardRelease (parse_key a.ebut,parse_unicode a.ey)
	    | _ -> KeyboardError
	 )
     | _ -> EventError
  );;

class poclow_event_manager =
object(self) 
  inherit event_manager
  val mutable ev_stack=Stack.create()

  method init()=
    Callback.register "add_event" (self#add);
    Callback.register "loop" (self#for_loop);
    Callback.register "quit" (on_quit)

  method add etype eval ebut ex ey=
    (Stack.push {etype=etype;eval=eval;ebut=ebut;ex=ex;ey=ey} ev_stack)
 
  method loop()=
    event_loop ev_stack;

  method for_loop()=
    while Stack.length ev_stack <> 0 do
      let a=(Stack.top ev_stack) in
	parser (poclow_event_to_event a);
	let r=Stack.pop ev_stack in ()
    done;
    on_loop();


  method main()=
    let ol=self#get_on_loop in
      self#set_on_loop ol;
      self#loop();

end;;


(* the event manager *)
let eventm=new poclow_event_manager;;

