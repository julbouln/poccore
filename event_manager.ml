(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003 POC 

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
open Low;;
open Video;;

(** Event manager *)

class event_manager =
object(self) 
  val mutable ev_stack=Stack.create()
  val mutable parser=(fun e->())
  val mutable on_loop=(fun()->())
  val mutable on_quit=(fun()->())

  method set_parser p=parser<-p
  method get_on_loop=on_loop
  method set_on_loop f=on_loop<-f
  method set_on_quit f=on_quit<-f

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
	parser a;
	let r=Stack.pop ev_stack in ()
    done;
    on_loop();


  method main()=
    let ol=self#get_on_loop in
      self#set_on_loop ol;
      self#loop();

end;;


let eventm=new event_manager;;

type key_type=
| KeyBackspace
| KeyReturn
| KeySpace
| KeyEchap
| KeyUp
| KeyDown
| KeyRight
| KeyLeft
| KeyChar of string
| KeyUnknow;;

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

(* FIXME : deprecated *)
(*
let ev_stack=Stack.create();;
let ev_a=ref ev_stack;;
let add_event etype eval ebut ex ey=(Stack.push {etype=etype;eval=eval;ebut=ebut;ex=ex;ey=ey} !ev_a);;

let cur_key=ref "none";;

let release_key (k:int)=cur_key:="none";;

let parse_keybut e=
  match e.ebut with 
    | 8 -> "backspace";
    | 13 -> "return";
    | 32 -> "space";
    | 27 -> "escape";
    | 48 -> "0";
    | 49 -> "1";
    | 50 -> "2";
    | 51 -> "3";
    | 52 -> "4";
    | 53 -> "5";
    | 54 -> "6";
    | 55 -> "7";
    | 56 -> "8";
    | 57 -> "9";

    | 97 -> "a";
    | 98 -> "b";
    | 99 -> "c";
    | 100 -> "d";
    | 101 -> "e";
    | 102 -> "f";
    | 103 -> "g";
    | 104 -> "h";
    | 105 -> "i";
    | 106 -> "j";
    | 107 -> "k";
    | 108 -> "l";
    | 109 -> "m";
    | 110 -> "n";
    | 111 -> "o";
    | 112 -> "p";
    | 113 -> "q";
    | 114 -> "r";
    | 115 -> "s";
    | 116 -> "t";
    | 117 -> "u";
    | 118 -> "v";
    | 119 -> "w";
    | 120 -> "x";
    | 121 -> "y";
    | 122 -> "z";
    | _->"none";;


let parse_key k=
  match k with 
    | 8 -> cur_key:="backspace";
    | 13 -> cur_key:="return";
    | 32 -> cur_key:="space";
    | 27 -> exit 2;
    | 48 -> cur_key:="0";
    | 49 -> cur_key:="1";
    | 50 -> cur_key:="2";
    | 51 -> cur_key:="3";
    | 52 -> cur_key:="4";
    | 53 -> cur_key:="5";
    | 54 -> cur_key:="6";
    | 55 -> cur_key:="7";
    | 56 -> cur_key:="8";
    | 57 -> cur_key:="9";

    | 97 -> cur_key:="a";
    | 98 -> cur_key:="b";
    | 99 -> cur_key:="c";
    | 100 -> cur_key:="d";
    | 101 -> cur_key:="e";
    | 102 -> cur_key:="f";
    | 103 -> cur_key:="g";
    | 104 -> cur_key:="h";
    | 105 -> cur_key:="i";
    | 106 -> cur_key:="j";
    | 107 -> cur_key:="k";
    | 108 -> cur_key:="l";
    | 109 -> cur_key:="m";
    | 110 -> cur_key:="n";
    | 111 -> cur_key:="o";
    | 112 -> cur_key:="p";
    | 113 -> cur_key:="q";
    | 114 -> cur_key:="r";
    | 115 -> cur_key:="s";
    | 116 -> cur_key:="t";
    | 117 -> cur_key:="u";
    | 118 -> cur_key:="v";
    | 119 -> cur_key:="w";
    | 120 -> cur_key:="x";
    | 121 -> cur_key:="y";
    | 122 -> cur_key:="z";
    | _->();;

Callback.register "release_key" (release_key);;
Callback.register "parse_key" (parse_key);;
(*Callback.register "add_event" (add_event);;*)

let clear_stack()=
  while Stack.length !ev_a <> 0 do
    let a=(Stack.top !ev_a) in
    Stack.pop !ev_a;
  done;;
*)
