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
open Unix;;

(* FIXME : use class here : music_manager *)


(** Music manager *)

class music_manager=
object
  val mutable a=Array.make 100 "none" 
  val mutable d=""

(** load all music from dir *)
method load_musics dir=
  d<-dir;
  let farr=a in
  let d=opendir dir in
  let cf=ref false in
  let c=ref 0 in
  while !cf==false do
    (try 
      let f=(readdir d) in
(*      print_string f;print_newline();*)
      farr.(!c)<-f;
    with
    | End_of_file -> cf:=true);		
    
    c:= !c+1;
  done;      
  closedir d;

method get_music n=a.(n)

method print_musics()=
  for i=0 to (Array.length a)-1 do
    if a.(i)<>"none" then (
      print_string a.(i);print_newline();
     );
  done

method is_music i=
  if a.(i)<>"none" && a.(i)<>"." && a.(i)<>".." then true else false

(** play specific music in all loaded *)
method play_music i=
  let mus=music_load (d^"/"^a.(i)) in
(*  music_set_volume 16; *)
  music_play mus;
  mus

(** play all music loaded *)
method play_musics()=
  for i=0 to (Array.length a)-1 do
    if a.(i)<>"none" && a.(i)<>"." && a.(i)<>".." then (
      let mus=music_load (d^"/"^a.(i)) in
      music_play mus;
(*      music_free mus;
      print_string a.(i);print_newline();
*)
     );
  done


end;;


