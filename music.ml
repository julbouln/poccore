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

let load_musics()=
  let farr=Array.make 100 "none" in
  let d=opendir "medias/musics" in
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
  farr
;;

let print_musics a=
  for i=0 to (Array.length a)-1 do
    if a.(i)<>"none" then (
      print_string a.(i);print_newline();
     );
  done;;

let is_music a i=
  if a.(i)<>"none" && a.(i)<>"." && a.(i)<>".." then true else false

let play_music a i=
  let mus=music_load ("medias/musics/"^a.(i)) in
(*  music_set_volume 16; *)
  music_play mus;
  mus

let play_musics a=
  for i=0 to (Array.length a)-1 do
    if a.(i)<>"none" && a.(i)<>"." && a.(i)<>".." then (
      let mus=music_load ("medias/musics/"^a.(i)) in
      music_play mus;
(*      music_free mus;
      print_string a.(i);print_newline();
*)
     );
  done;;
