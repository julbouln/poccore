open Rect;;
open Cache;;
open Event;;
open Font;;
open Drawing;;

(** poclow binding *)
open Low;;

all_init();

(** poclow font *)

class poclow_font_object=
object(self)
  inherit [font] font_object
  val mutable f=None
  method get_f=
    match f with
      | Some v->v
      | None -> raise Font_not_initialized;
  method set_f (nt:font)=f<-(Some nt)


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

(*
    method get_font=
      match font with
	| FontTTF f->
	    (Some (vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt")))
	| FontEmbed -> None
*)
(*
    method create_text txt color =
      match font with
	| FontTTF f->
	    tile_text (vfs_fonts#get_simple (font ^ ":" ^ string_of_int(size)^"pt")) txt color
	| FontEmbed ->
	  let tmp=tile_box (String.length txt*8) 8 (255,255,255) in
	    tile_set_alpha tmp 255 255 255;
	    tile_string tmp (0,0) txt color; 
	    tmp
*)
  end;;


class font_vault s=
object(self)
  inherit [poclow_font_object] medias_cache s

  method new_font()=new poclow_font_object
end;;

let font_vault=new font_vault 100;;



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


  initializer

(** create ops *)

    self#add_op "load" DrawTypeCreate (
      fun ovl->
	let f=(get_draw_op_string ovl 0) in
	print_string ("DRAWING_OBJECT: load "^f);print_newline();
	DrawValT (tile_load f);
    );

    self#add_op "rect" DrawTypeCreate (
      fun ovl->
	let (w,h)=(get_draw_op_size ovl 0) in
	let col=(get_draw_op_color ovl 1) in
	DrawValT (tile_rect w h col);
    );

    self#add_op "create_text" DrawTypeCreate (
      fun ovl->
	let fnt_n=(get_draw_op_string ovl 0) and
	    txt=(get_draw_op_string ovl 1) and
	    color=(get_draw_op_color ovl 2) in
	let fnt=(font_vault#get_cache_simple fnt_n) in

	  DrawValT
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

    self#add_op "color_change" DrawTypeCopy (
      fun ovl->      
      let col1=get_draw_op_color ovl 0 and
	  col2=get_draw_op_color ovl 1 in
	DrawValT (tile_color_change self#get_t col1 col2)
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

    self#add_op "rectangle" DrawTypeWrite (
      fun ovl->
	let p1=get_draw_op_position ovl 0 and
	    p2=get_draw_op_position ovl 1 and
	    col=get_draw_op_color ovl 2 in
	  
	  (tile_rectangle self#get_t p1 p2 col);
	  DrawValNil;
    );

(** read pos *)
    self#add_op "get_rpos" DrawTypeRead (
      fun ovl->
	let rcol=(get_draw_op_color ovl 0) in
	let (x1,y1,x2,y2)=tile_refresh_pos self#get_t in
	  DrawValRectangle (new rectangle x1 y1 x2 y2)
    );

end;;

class poclow_drawing_screen=
object(self)
(*  inherit [tile] drawing_screen *)
  inherit poclow_drawing_object

  method init w h bpp fs=
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

class poclow_drawing_vault s=
object(self)
  inherit [tile] drawing_vault s
  method new_drawing()=new poclow_drawing_object
  method new_drawing_screen()=new poclow_drawing_screen

  initializer
    (* FIXME : must be in drawing.ml and use dr#exec_op_copy "color_change" *)
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
			   ndr#set_t (tile_color_change ndr#get_t col1 col2);
			 with Drawing_not_initialized ->
			   ndr#set_t (tile_color_change dr#get_t col1 col2)
			);
			i:= !i+1;
		  ) col1_l;
		  ndr
	    ) drl;    
      )



end;;

let drawing_vault=new poclow_drawing_vault 10000;;


(* poclow event *)

let parse_unicode k=
  KeyUnicode (UChar.chr k);;

let parse_key k=
  match k with 
    | 8 -> KeyBackspace
    | 13 -> KeyReturn
    | 32 -> KeySpace
    | 27 -> KeyEchap
    | 273 -> KeyUp
    | 274 -> KeyDown
    | 275 -> KeyRight
    | 276 -> KeyLeft

    | 304 -> KeyShift
    | 306 -> KeyCtrl
    | 308 -> KeyAlt

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


let eventm=new poclow_event_manager;;

