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
open Rect;;
open Video;;

(** Pseudo VFS system - for not load multiple time the same data. 
 VFS permit to have many object with the same C data ref. 
 We have 2 types of entry : dynamic or static,
 - dynamic are generated each time we need the tile (function()->tile array)
 - static are always loaded in memory (tile array)

Limitations : we can't have a mixed dyn/static entry (array)
*)


class ['a] vfs_files (t: 'a)=
 object (self)
      
   val mutable datas=let a=Hashtbl.create 2 in Hashtbl.add a "none" (Array.make 1 t);a
   val mutable datas_from_func=let a=Hashtbl.create 2 in Hashtbl.add a "none" (fun()->[|t|]);a
   val mutable dyn=let a=Hashtbl.create 2 in Hashtbl.add a "none" (1,(function k->(t)));a
   val mutable free_f=(function t->())
   
   (* create only if there is no entry *)
     
   (** Create entry from function (static) *)
   method create_from_func k f=
     if(Hashtbl.mem datas k)==false then 
       (
(*	let t=f() in *)
	Hashtbl.add datas_from_func k f
(*	self#create k t; *)
       )

   (** Create simple entry from function (static) *)
   method create_simple_from_func k f=
     if(Hashtbl.mem datas k)==false then 
       (
	let f2=fun()->[| f() |] in
	Hashtbl.add datas_from_func k f2;

       )
	 
   (** Create entry (static) *)
   method create k d=
     if(Hashtbl.mem datas k)==false then 
       (
	print_string ("VFS:create:"^k);print_newline();
	Hashtbl.add datas k d;
       )

   (** Create entry with dynamic function. 
       Tiles created within this MUST be freed *)
   method create_dyn_func k s d=
     if(Hashtbl.mem dyn k)==false then 
       (
	print_string ("VFS:create_dyn:"^k);print_newline();
	Hashtbl.add dyn k (s,d)
       )
     else
       Hashtbl.replace dyn k (s,d)
	 
   (** Create simple entry *)
   method create_simple k sd=
     if(Hashtbl.mem datas k)==false then 
       (
	Hashtbl.add datas k [|sd|];
       )

  (** Get an entry *)	
   method get k=
     if(Hashtbl.mem dyn k)==true then 
       (
	let a=Array.make 1 t in
	let o=Hashtbl.find dyn k in
	let n=(fst o) in
	for i=0 to n-1 do
	  let exe=(snd o) in
	  a.(i)<-(exe i);
	done;
	a
       )
     else (
       if Hashtbl.mem datas k=false then (
	 let f=Hashtbl.find datas_from_func k in
	 let d=f() in
	   self#create k d;
	   print_string ("VFS:exec_func:"^k);print_newline();
       );
	 Hashtbl.find datas k
     )	 

   (** Get a simple entry *)	
   method get_simple k=
     if Hashtbl.mem datas k=false then (
	 let f=Hashtbl.find datas_from_func k in
	 let d=f() in
	   self#create k d;
     );
       (Hashtbl.find datas k).(0)

   (** Is one entry *)	
   method is_one k i=
     let s=Array.length (Hashtbl.find datas k) in
     if s<=i then false else true

   (** Get one entry *)	
   method get_one k i=

     if(Hashtbl.mem dyn k)==true then 
       (
	let o=Hashtbl.find dyn k in
	let n=(fst o) in
	let exe=(snd o) in
	exe i;
       )
     else 
       (
	 (self#get k).(i)
(*	let a_s=Array.length (Hashtbl.find datas k) in
	if i<a_s then
	  (self#get k).(i)
(* (Hashtbl.find datas k).(i) *)
	else (
(*	  (Hashtbl.find datas k).(a_s-1) *)
	  (self#get k).(a_s -1)
	 )
*)
       )

   (** Get entry size *)
   method size k=Array.length (Hashtbl.find datas k)

   (** Free entry.
       This only free memory of d data.
       Usefull with dynamic entry *)
   method free d=
     free_f d;

   (** Delete entry *)    
   method delete k=
     if(Hashtbl.mem datas k)==false then (
       let d=Hashtbl.find datas k in
       for i=0 to (Array.length d)-1 do
	 free_f (d.(i));
       done;
       Hashtbl.remove datas k
      )

   (** See if k entry is dynamic and then free d data *)
   method free_dyn k d=
     if(Hashtbl.mem dyn k)==true then 
       free_f d;
	
  end;;



class refresh_pos=
object
  val mutable npos=Hashtbl.create 2

  method add_pos (n:string) (p:rectangle)=Hashtbl.add npos n p
  method get_pos n=Hashtbl.find npos n
  method is_pos n=Hashtbl.mem npos n
end;;

(** VFS sytem - tile manager *)
class vfs_files_tile=
object(self)
    inherit [tile] vfs_files (tile_empty()) as super
        
    val mutable rpos=new refresh_pos

    method add_rpos n=
      if (rpos#is_pos n)==false then (
	let (x1,y1,x2,y2)=tile_refresh_pos (self#get_one n 0) in
(*	print_int x1;
	print_string " - ";			  
	print_int y1;
	print_string " - ";			  
	print_int x2;
	print_string " - ";			  
	print_int y2;
	print_newline();
*)
	rpos#add_pos n (new rectangle x1 y1 x2 y2);
      );		       

    method get_rpos (n:string)=
(*      new rectangle 0 0 0 0 *)
      self#add_rpos n;
      rpos#get_pos n

    initializer      
      free_f<-(function t->tile_free t);

    method create_from_func k f=      
      if(Hashtbl.mem datas k)==false then 
	(
      	 if k<>"none" && is_video()==true then 
	   (
	 (*   let t=f() in

	      super#create k t;
	 *)
	     
	     super#create_from_func k f; 
	 (*    self#add_rpos k;  *)

	   )
	 else 
	   super#create_simple k (tile_empty()); 
	)
    method create k d=      
      if(Hashtbl.mem datas k)==false then (
	if k<>"none" && is_video()==true then (
	  super#create k d;
(*	  self#add_rpos k;  *)

	)
	else 
	  super#create_simple k (tile_empty()); 
       )
  end;;

(** VFS sytem - sound manager *)
class vfs_files_sound=
  object
    inherit [sound] vfs_files (sound_empty()) as super
    initializer
      free_f<-(function t->sound_free t);
    method create_simple_from_func k d=      
      if(Hashtbl.mem datas k)==false then (
	if k<>"none" && is_audio()==true then (
(*	  let t=d() in *)
	  super#create_simple_from_func k d
	 )
	else 
	  super#create_simple k (sound_empty()); 
       )
  end;;

(** VFS sytem - font manager *)
class vfs_files_font=
  object
    inherit [font] vfs_files (font_empty())
    initializer
      free_f<-(function t->font_free t);
  end;;

(* create vfs files *)
let vfs_tiles=new vfs_files_tile;;
let vfs_fonts=new vfs_files_font;;
let vfs_sounds=new vfs_files_sound;;


