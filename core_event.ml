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


type key_type=
| KeyTab
| KeyBackspace
| KeyReturn
| KeySpace
| KeyCtrl
| KeyAltR
| KeyAltL
| KeyAltGr
| KeyEchap
| KeyUp
| KeyDown
| KeyRight
| KeyLeft
| KeyShift
| KeyUnicode of UChar.t
| KeyChar of string
| KeyUnknow;;

let string_of_key=function
  | KeyTab -> "tab"
  | KeyBackspace->"backspace"
  | KeyReturn->"return"
  | KeySpace->"space"
  | KeyCtrl->"ctrl"
  | KeyAltR->"altr"
  | KeyAltL->"altl"
  | KeyAltGr->"altgr"
  | KeyEchap->"echap"
  | KeyUp->"up"
  | KeyDown->"down"
  | KeyRight->"right"
  | KeyLeft->"left"
  | KeyShift->"shift"
  | KeyChar c->c
  | _ -> "";;

type mouse_event=
  | MouseClick of (int*int*int) (* x,y,button *)
  | MouseRelease of (int*int*int)
  | MouseMotion of (int*int) (* x,y *) 
  | MouseError
;; 

type keyboard_event=
  | KeyboardPress of (key_type*key_type)
  | KeyboardRelease of (key_type*key_type)
  | KeyboardError;;

type event=
  | EventMouse of mouse_event
  | EventKeyboard of keyboard_event
  | EventError

(** Event manager *)
class virtual event_manager=
object(self)
  val mutable parser=(fun e->())
  val mutable on_loop=(fun()->())
  val mutable on_quit=(fun()->())

  method set_parser (p:event->unit)=parser<-p
  method get_on_loop=on_loop
  method set_on_loop f=on_loop<-f
  method set_on_quit f=on_quit<-f


  method virtual init: unit->unit
  method virtual main:unit->unit
  
end;;


(** interaction *)

open Value_lua;;

class virtual interaction_object=
object(self)

  method virtual on_keypress : (key_type*key_type)->unit
  method virtual on_keyrelease : (key_type*key_type)->unit
    
  method virtual on_mouseclick : int->int->int->unit
  method virtual on_mouserelease : int->int->int->unit

  method virtual on_mouseover : int->int->unit
  method virtual on_mouseout : int->int->unit


  method ev_parser e=
    (match e with
       | EventMouse em ->
	   (match em with
	      | MouseMotion(x,y) -> 
		  self#on_mouseover x y;
	      | MouseRelease(x,y,but) -> 
		  self#on_mouserelease x y but; 
	      | MouseClick(x,y,but) -> 
		  self#on_mouseclick x y but; 
	      | _ -> ()
	   )
       | EventKeyboard ek->
	 (match ek with
	    | KeyboardPress (k,uk)-> 
		self#on_keypress (k,uk)
	    | KeyboardRelease (k,uk)-> 
		self#on_keyrelease (k,uk)
	    | _ -> ()
	 )
       | _ -> ()
    )

end;;

class interaction_lua=
object(self)
  inherit interaction_object
  inherit lua_object as super

  method get_id="interaction"

  method on_keypress (e:(key_type*key_type))=
    ignore(lua#exec_val_fun (OLuaVal.String "on_keypress") [OLuaVal.String (string_of_key (fst e))])
  method on_keyrelease (e:(key_type*key_type))=
    ignore(lua#exec_val_fun (OLuaVal.String "on_keyrelease") [OLuaVal.String (string_of_key (fst e))])
    
  method on_mouseclick (x : int) (y : int) (but : int)=
ignore(lua#exec_val_fun (OLuaVal.String "on_mouseclick") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y);OLuaVal.Number (float_of_int but)])
  method on_mouserelease (x : int) (y : int) (but: int)=
ignore(lua#exec_val_fun (OLuaVal.String "on_mouserelease") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y);OLuaVal.Number (float_of_int but)])

  method on_mouseover (x : int) (y : int)=
    ignore(lua#exec_val_fun (OLuaVal.String "on_mouseover") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];)
  method on_mouseout (x : int) (y : int)=
    ignore(lua#exec_val_fun (OLuaVal.String "on_mouseout") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];)

  method lua_init()=
    lua#set_val (OLuaVal.String "on_mouseclick") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->OLuaVal.int **->> OLuaVal.unit) (fun x y b->()));
    lua#set_val (OLuaVal.String "on_mouserelease") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y b->()));

    lua#set_val (OLuaVal.String "on_mouseover") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));
    lua#set_val (OLuaVal.String "on_mouseout") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));

    lua#set_val (OLuaVal.String "on_keypress") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun s->()));
    lua#set_val (OLuaVal.String "on_keyrelease") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) (fun s->()));
    super#lua_init();


end;;
