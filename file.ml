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
(* FIXME : use a class here *)


let save_file filename data=
let oc = open_out_bin filename in
Marshal.to_channel oc data [];
(*output_value oc data;*)

close_out oc;;

let load_file filename=
let ic=open_in_bin filename in
(*let a=input_value ic in *)
let a=Marshal.from_channel ic in
close_in ic;
a;;
