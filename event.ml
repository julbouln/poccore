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

let ev_stack=Stack.create();;
let ev_a=ref ev_stack;;
let add_event etype eval ebut ex ey=(Stack.push {etype=etype;eval=eval;ebut=ebut;ex=ex;ey=ey} !ev_a);;

let cur_key=ref "none";;

let release_key (k:int)=cur_key:="none";;

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
Callback.register "add_event" (add_event);;

let clear_stack()=
  while Stack.length !ev_a <> 0 do
    let a=(Stack.top !ev_a) in
    Stack.pop !ev_a;
  done;;
