(*
    Battle For Rashitoul - The ultimate strategy/arcade game
    Copyright (C) 2003,2004 POC 

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

(** Main audio class *)

class audio=
object
  val mutable freq=0
  val mutable chans=0
		      		      
  method init f c=
    freq<-f;
    chans<-c;
    audio_init freq chans;

  method initialized=
    is_audio

  method set_audio_vol vol=
    audio_set_volume vol

  method set_music_vol vol=
    let i=music_set_volume vol in ()

end;;

let audio=new audio;;
