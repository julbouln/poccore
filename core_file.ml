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
(** Generic file manipulation *)
open Unix;;

(* Sample ocaml type<->file interface *)
class ['a] file=
object
  method save filename (data:'a)=
    let oc = open_out_bin filename in
      Marshal.to_channel oc data [];
      close_out oc;
  method load filename=
    let ic=open_in_bin filename in
    let a=Marshal.from_channel ic in
      close_in ic;
      (a:'a);
end;;

(* FIXME : go in poccore *)
class dir rd=
object

  val mutable farr=Array.make 100 "none" 
  val mutable dir=rd

  method foreach_file f=
    let d v=if v<>"none" then f v in
    Array.iter d farr

  method load()=
    let d=opendir dir in
    let cf=ref false in
    let c=ref 0 in
      while !cf==false do
	(try 
      let f=(readdir d) in
	farr.(!c)<-f;
	 with
	   | End_of_file -> cf:=true);		
	
	c:= !c+1;
      done;      
      closedir d;


end;;


