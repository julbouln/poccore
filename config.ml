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

open File;;

type config={
(* FIXME: video_opt must be an array *)
(* shadow *)
mutable video_opt1:int;
(* fs *)
mutable video_opt2:int;

(* FIXME: game_opt must be an array *)
(* dif camps *)
mutable game_opt1:int;
(* lifebar *)
mutable game_opt2:int;
(* inhelp *)
mutable game_opt3:int;


(* 
0: 640x480 
1: 800x600
2: 1024x768
*)
mutable screen_size:int;


mutable audio_vol:int;
mutable music_vol:int;


mutable lang:string;
};;


let load_config configfile=
if (Sys.file_exists configfile) then (load_file configfile)
else (
  {	
    video_opt1=1;
    video_opt2=1;
    game_opt1=1;
    game_opt2=1;
    game_opt3=1;
    screen_size=1;
    audio_vol=16;
    music_vol=16;
    lang="en";
  }
);;





