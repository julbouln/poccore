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
open Core_event;;
open Core_font;;
open Core_drawing;;

open Value_val;;

open Core_val;;

(** ocamlsdl binding *)

open Sdl;;
open Sdlttf;;
open Sdlvideo;;
open Sdlloader;;
open Sdlwm;;

open Sdlgfx;;

(** ocamlsdl font *)

class sdl_font_object=
object(self)
  inherit [Sdlttf.font] font_object

  val mutable font_type=FontEmbed
  method get_font_type=font_type

  method load fontt=
    font_type<-fontt;
    match font_type with
      | FontTTF (f,s)->
	  self#set_f (open_font f s);
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
	      size_utf8 self#get_f txt
	  | FontEmbed -> (String.length txt*8,8);

  end;;


class font_vault s=
object(self)
  inherit [sdl_font_object] medias_cache s (1.)

  method new_font()=new sdl_font_object

  method cache_file_save f dl=()
  method cache_file_load f=[||]
end;;


(* the font vault *)
(*let font_vault=new font_vault 100;;*)



(** ocamlsdl drawing *)

class sdl_drawing_object font_vault=
object(self)
  inherit [Sdlvideo.surface] drawing_object

  method get_w=
    (surface_info self#get_t).w
  method get_h=
    (surface_info self#get_t).h

  method new_t nt=
    let nd=new sdl_drawing_object font_vault in
      nd#set_t (nt);
      nd

  method create w h c=self#set_t (
    let (r,g,b)=c in
    let s=create_RGB_surface [`SWSURFACE] w h 16 
	(Int32.of_int r) 
	(Int32.of_int g) 
	(Int32.of_int b) 
	(Int32.of_int 0) in
  display_format s
  ) 


  method copy()=
    self#new_t (display_format self#get_t)

  method put_pixel x y c=put_pixel self#get_t x y (map_RGB self#get_t c)
  method get_pixel x y=get_RGB self#get_t (get_pixel self#get_t x y)

  method compose dr x y=blit_surface ~src:dr#get_t ~dst:self#get_t ~dst_rect:{r_x=x;r_y=y;r_w=0;r_h=0} ()


  initializer

(** create ops *)

    self#add_op_from_list "load" DrawTypeCreate (
      fun ovl->
	let f=string_of_val (List.nth ovl 0) in
	DrawResultT (display_format (load_image f));
    );

    self#add_op_from_list "rect" DrawTypeCreate (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0) in
	let col=color_of_val (List.nth ovl 1) in
(*	DrawResultT (tile_rect w h col); *)

	let (r,g,b)=col in
	let s=create_RGB_surface [`SWSURFACE] w h 32
	  (Int32.of_int 0) 
	  (Int32.of_int 0) 
	  (Int32.of_int 0) 
	  (Int32.of_int 0) 
	in
	  fill_rect s (map_RGB s white);

(*	  boxRGBA
	    s 
	    {r_x=0;r_y=0;r_w=0;r_h=0} 
	    {r_x=w-1;r_y=h-1;r_w=0;r_h=0} 
	    white
	    0;
*)
	  rectangleRGBA
	    s
	    {r_x=0;r_y=0;r_w=0;r_h=0} 
	    {r_x=w-1;r_y=h-1;r_w=0;r_h=0} 
	    col
	    255;
	  DrawResultT (display_format s); 


    );

    self#add_op_from_list "box" DrawTypeCreate (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0) in
	let col=color_of_val (List.nth ovl 1) in
(*	DrawResultT (tile_box w h col); *)
	let (r,g,b)=col in
	let s=create_RGB_surface [`SWSURFACE] w h 32
	  (Int32.of_int 0) 
	  (Int32.of_int 0) 
	  (Int32.of_int 0) 
	  (Int32.of_int 0) 
	in
	  fill_rect s (map_RGB s col);
	  DrawResultT (display_format s) 
    );


    self#add_op_from_list "create_text" DrawTypeCreate (
      fun ovl->
	let fnt_n=string_of_val (List.nth ovl 0)  and
	    fnt_s=int_of_val (List.nth ovl 1)  and
	    txt=string_of_val (List.nth ovl 2)  and
	    color=color_of_val (List.nth ovl 3)  in

(*	  print_string fnt_n;print_newline(); *)
	  font_vault#add_cache (fnt_n^string_of_int fnt_s) (
	    fun()->
	      let fnt=font_vault#new_font() in
		(match fnt_n with 
		  | "font_embed" -> fnt#load FontEmbed
		  | _ -> fnt#load (FontTTF (fnt_n,fnt_s))
		);
		[|fnt|]
	  );
	let fnt=(font_vault#get_cache_simple (fnt_n^string_of_int fnt_s)) in

	  DrawResultT
	    (match fnt#get_font_type with
	       | FontTTF f->
		   let s=render_utf8_solid fnt#get_f txt color in
		   let si=surface_info s in
		   let s2=display_format(create_RGB_surface_format s [`SWSURFACE] si.w si.h) in
		     fill_rect s2 (map_RGB s2 (255,255,255));

		     blit_surface ~src:s ~dst:s2 ();
		     display_format s2
	       | FontEmbed ->
(*		   let tmp=tile_box (String.length txt*8) 8 (255,255,255) in
		     tile_set_alpha tmp 255 255 255;
		     tile_string tmp (0,0) txt color; 
		     tmp
*)
	    let (r,g,b)=color in	      
	    let tmp=display_format(create_RGB_surface [`SWSURFACE] (String.length txt*8) 8 16
		(Int32.of_int 255) 
		(Int32.of_int 255) 
		(Int32.of_int 255) 
		(Int32.of_int 255)) in
	      fill_rect tmp (map_RGB tmp (255,255,255));
	      stringRGBA tmp {r_x=0;r_y=0;r_w=0;r_h=0} txt color 255;
	      tmp
	    ));

(** copy ops *)

    self#add_op_from_list "mirror" DrawTypeCopy (
      fun ovl->
	let s=rotozoomSurfaceXY self#get_t 0. (-1.) 1. false in
	  DrawResultT (display_format s) 
    );

    self#add_op_from_list "split" DrawTypeCopy (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0)  in
	let da=DynArray.create() in
	let si=(surface_info self#get_t) in
	let x=si.w/w and
	    y=si.h/h in
	  for i=0 to x-1 do
	    for j=0 to y-1 do
	      let s=display_format(create_RGB_surface_format self#get_t [`SWSURFACE] w h) in
		blit_surface ~src:self#get_t ~src_rect:{r_x=i*w;r_y=j*h;r_w=w;r_h=h} ~dst:s ();
		DynArray.add da s
	    done;
	  done;
	DrawResultTArray (DynArray.to_array da);
    );

    self#add_op_from_list "resize" DrawTypeCopy (
      fun ovl->
	let (w,h)=size_of_val (List.nth ovl 0)  in
	let fw=(float_of_int w)/.100. and
	    fh=(float_of_int h)/.100. in
	let s=display_format(zoomSurface self#get_t fw fh false) in
	DrawResultT (s) 
    );

    (* must be in core_drawing *)
    self#add_op_from_list "color_change" DrawTypeCopy (
      fun ovl->      
      let col1=color_of_val (List.nth ovl 0) and
	  col2=color_of_val (List.nth ovl 1) in
      let si=surface_info self#get_t in
      let ns=create_RGB_surface_format self#get_t [`SWSURFACE] ~w:si.w ~h:si.h in
	lock self#get_t;
	lock ns;
	for x=0 to si.w - 1 do
	  for y=0 to si.h - 1 do
	    let pc=get_pixel self#get_t x y in
	      if pc=(map_RGB self#get_t col1) then
		put_pixel ns x y (map_RGB self#get_t col2) 
	      else
		put_pixel ns x y pc 
	  done;
	done;
	unlock ns;
	unlock self#get_t;
	DrawResultT (ns) 
    );


(** write op *)

    self#add_op_from_list "set_alpha" DrawTypeWrite (
      fun ovl->
	let (r,g,b)=color_of_val (List.nth ovl 0) in

	  set_color_key self#get_t ~rle:true (map_RGB self#get_t (r,g,b));
	  if List.length ovl > 1 then (
	    let alpha=int_of_val (List.nth ovl 1) in
	      set_alpha self#get_t ~rle:true alpha;
	  );
	  DrawResultUnit();
    );

    self#add_op_from_list "unset_alpha" DrawTypeWrite (
      fun ovl->
	unset_color_key self#get_t;
	unset_alpha self#get_t;
	DrawResultUnit();
    );

    self#add_op_from_list "line" DrawTypeWrite (
      fun ovl->
	let p1=position_of_val (List.nth ovl 0) and
	    p2=position_of_val (List.nth ovl 1)  and
	    col=color_of_val (List.nth ovl 2)  in
	  lineRGBA 
	    self#get_t 
	    {r_x=(fst p1);r_y=(snd p1);r_w=0;r_h=0}
	    {r_x=(fst p2);r_y=(snd p2);r_w=0;r_h=0}
	    col
	    255;
	  
	  DrawResultUnit();
    );

    self#add_op_from_list "ellipse" DrawTypeWrite (
      fun ovl->
	let p1=position_of_val (List.nth ovl 0) and
	    p2=position_of_val (List.nth ovl 1)  and
	    col=color_of_val (List.nth ovl 2)  in
	  ellipseRGBA 
	    self#get_t 
	    {r_x=(fst p1);r_y=(snd p1);r_w=0;r_h=0}
	    {r_x=(fst p2);r_y=(snd p2);r_w=0;r_h=0}
	    col
	    255;
	  
	  DrawResultUnit();
    );

    self#add_op_from_list "aaellipse" DrawTypeWrite (
      fun ovl->
	let p1=position_of_val (List.nth ovl 0) and
	    p2=position_of_val (List.nth ovl 1)  and
	    col=color_of_val (List.nth ovl 2)  in
	  aaellipseRGBA 
	    self#get_t 
	    {r_x=(fst p1);r_y=(snd p1);r_w=0;r_h=0}
	    {r_x=(fst p2);r_y=(snd p2);r_w=0;r_h=0}
	    col
	    255;
	  
	  DrawResultUnit();
    );

    self#add_op_from_list "filledEllipse" DrawTypeWrite (
      fun ovl->
	let p1=position_of_val (List.nth ovl 0) and
	    p2=position_of_val (List.nth ovl 1)  and
	    col=color_of_val (List.nth ovl 2)  in
	  filledEllipseRGBA 
	    self#get_t 
	    {r_x=(fst p1);r_y=(snd p1);r_w=0;r_h=0}
	    {r_x=(fst p2);r_y=(snd p2);r_w=0;r_h=0}
	    col
	    255;
	  
	  DrawResultUnit();
    );


    self#add_op_from_list "rectangle" DrawTypeWrite (
      fun ovl->
	let p1=position_of_val (List.nth ovl 0) and
	    p2=position_of_val (List.nth ovl 1)  and
	    col=color_of_val (List.nth ovl 2)  in
	  rectangleRGBA 
	    self#get_t 
	    {r_x=(fst p1);r_y=(snd p1);r_w=0;r_h=0}
	    {r_x=(fst p2);r_y=(snd p2);r_w=0;r_h=0}
	    col
	    255;

	  DrawResultUnit();
    );

(** read op *)

    self#add_op_from_list "get_rpos" DrawTypeRead (
      fun ovl->
	let rcol=color_of_val (List.nth ovl 0) in
(*	let (x1,y1,x2,y2)=tile_refresh_pos self#get_t in
	  DrawResultVal(`List [`Position(x1,y1);`Position(x2,y2)])
*)
	let si=(surface_info self#get_t) in
	let da=DynArray.create() in
	  lock self#get_t;
	  for x=0 to si.w - 1 do
	    for y=0 to si.h - 1 do
	      let pc=get_pixel self#get_t x y in
		if pc=(map_RGB self#get_t rcol) then
		  DynArray.add da (`Position (x,y))
	    done;
	  done;

	unlock self#get_t;
	if DynArray.length da < 2 then (
	  DynArray.add da (`Position (0,0));
	  DynArray.add da (`Position (0,0));
	);
	DrawResultVal(`List (DynArray.to_list da))
    );

end;;

class sdl_drawing_screen font_vault=
object(self)
(*  inherit [Sdlvideo.surface] drawing_screen *)
(*  inherit [tile] drawing_screen *)
  inherit sdl_drawing_object font_vault

  method init w h bpp (fs:bool)=
    
    Sdl.init [`EVERYTHING];
    Sdlttf.init();
    Sdlkey.enable_unicode true;

(*    print_string "BINDING(poclow): init screen";print_newline(); *)
    if fs then
      self#set_t (set_video_mode ~w:w ~h:h ~bpp:bpp [`HWSURFACE;`DOUBLEBUF;`FULLSCREEN])
    else
      self#set_t (set_video_mode ~w:w ~h:h ~bpp:bpp [`HWSURFACE;`DOUBLEBUF])

  method refresh()=
    flip self#get_t

  method set_caption s i=
    set_caption s i

  method blank()=
(*    video_blank_screen() *)()
 
  method set_clip x y w h=
    set_clip_rect self#get_t {r_x=x;r_y=y;r_w=w;r_h=h}

  method show_cursor()=
    Sdlmouse.show_cursor true

  method hide_cursor()=
    Sdlmouse.show_cursor false

  method fcompose=self#compose
(*  method fcompose (dr:poclow_drawing_object) x y=tile_put dr#get_t x y*)

end;;

let digest_of_string txt=
  (Digest.to_hex(Digest.string txt));;

class sdl_drawing_vault s mt=
object(self)
  inherit [Sdlvideo.surface] drawing_vault s mt

  val mutable font_vault=new font_vault 100
  method get_font_vault=font_vault


(*  val mutable screen=new sdl_drawing_screen *)

  val mutable screen=None
  method get_screen=
    match screen with
      | Some s->s
      | None -> let s=self#new_drawing_screen() in screen<-Some s;s

  method new_drawing()=new sdl_drawing_object font_vault
  method new_drawing_screen()=new sdl_drawing_screen font_vault

  method cache_file_save f dl=
    (* if no cache dir, create it *)
    if Sys.file_exists "cache"=false then
      Unix.mkdir "cache" 0o700;

    Array.iteri (
      fun i d->
	let fn=(digest_of_string f^"_"^string_of_int i^".bmp") in
	  if Sys.file_exists fn=false then (
	    d#exec_op_write_from_list "unset_alpha" []; 
	    save_BMP (d#get_t) ("cache/"^fn);
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
	      nd#set_t (display_format(load_BMP ("cache/"^fn)));
	      nd#exec_op_write_from_list "set_alpha" [`Color (255,255,255)];
	      nd
	  );
	i:= !i+1;
      done;
      DynArray.to_array a

    
end;;

class default_drawing_object font_vault=
object
  inherit sdl_drawing_object font_vault
end;;

(* the drawing vault *)
(*let drawing_vault=new sdl_drawing_vault 10000 (1./.25.);;*)

class binding_drawing_vault s mt=
object
inherit sdl_drawing_vault s mt
end

(* ocamlsdl event *)

open Sdlevent;;

let parse_unicode k=
  KeyUnicode (Camomile.UChar.of_char k);;

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

    | 300 -> KeyNumLock
    | 301 -> KeyCapsLock

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

(* huhu *)
    | 256 -> KeyChar "0"
    | 257 -> KeyChar "1";
    | 258 -> KeyChar "2";
    | 259 -> KeyChar "3";
    | 260 -> KeyChar "4";
    | 261 -> KeyChar "5";
    | 262 -> KeyChar "6";
    | 263 -> KeyChar "7";
    | 264 -> KeyChar "8";
    | 265 -> KeyChar "9";
    | 270 -> KeyChar ".";

    | 267 -> KeyChar "/";
    | 268 -> KeyChar "*";
    | 269 -> KeyChar "-";
    | 270 -> KeyChar "+";

    | 271 -> KeyReturn;


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


open Sdlmouse;;
let sdl_mousebut_to_int m=
  match m with
    | BUTTON_LEFT -> 0
    | BUTTON_MIDDLE -> 1
    | BUTTON_RIGHT -> 2
    | BUTTON_WHEELUP -> 3
    | BUTTON_WHEELDOWN -> 4;;

let sdl_event_to_event a=
    (match a with
     | MOUSEMOTION m->EventMouse (MouseMotion(m.mme_x,m.mme_y))
     | MOUSEBUTTONUP m->EventMouse (MouseRelease(m.mbe_x,m.mbe_y,(sdl_mousebut_to_int m.mbe_button)))
     | MOUSEBUTTONDOWN m->EventMouse (MouseClick(m.mbe_x,m.mbe_y,(sdl_mousebut_to_int m.mbe_button)));
     | KEYDOWN m->
(*print_char m.keycode;print_newline();*)
(* print_int m.keyunicode;print_newline(); *)	     
EventKeyboard(KeyboardPress (parse_key (Sdlkey.int_of_key m.keysym),

 parse_unicode m.keycode 
(* parse_unicode (Sdlkey.int_of_key m.keysym) *)
))
     | KEYUP m->EventKeyboard(KeyboardRelease (parse_key (Sdlkey.int_of_key m.keysym),
 parse_unicode m.keycode
(*parse_unicode (Sdlkey.int_of_key m.keysym) *)
))
     | _ -> EventError
  );;

(*
if Sdlkey.query_unicode() then(
  print_string "unicode enabled";print_newline());
*)
class sdl_event_manager =
object(self) 
  inherit event_manager

  method init()=()

  method loop()=
    while has_event()=true do
      let a=poll() in
	(match a with
	   | Some e->parser (sdl_event_to_event e);
	   | None -> ()
	);
    done;
	on_loop();

  method main()=
    while true do
    let ol=self#get_on_loop in
      self#set_on_loop ol;
      self#loop();
    done;
    Sdl.quit()

end;;

at_exit (Sdl.quit);;

(* the event manager *)
let eventm=new sdl_event_manager;;

