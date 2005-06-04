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

open Value_common;;

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


