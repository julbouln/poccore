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

(** Cache manager *)

exception Cache_fun_not_found of string;;
exception Cache_no_corres of string;;
exception Cache_out_of_bound of (string*int);;
exception Cache_full;;

class virtual ['ct] medias_cache s mt=
object(self)
  val mutable cache=Weak.create s
  val mutable cache_f=Hashtbl.create 2 
  val mutable corres=Hashtbl.create 2    
  val mutable cache_time=Hashtbl.create 2

  method virtual cache_file_save: string -> 'ct array->unit
  method virtual cache_file_load: string ->'ct array

  method set_corres n i=
    if Hashtbl.mem corres n then
      Hashtbl.replace corres n i
    else
      Hashtbl.add corres n i

  method is_corres n=
    Hashtbl.mem corres n

  method get_corres n=
    (try
       Hashtbl.find corres n
     with Not_found -> raise (Cache_no_corres n))

  method is_cache_fun n=
    Hashtbl.mem cache_f n

  method add_cache_fun n (o:unit->('ct) array)=
    if Hashtbl.mem cache_f n=false then
      Hashtbl.add cache_f n o
(*    else
      Hashtbl.replace cache_f n o
*)
  method replace_cache_fun n (o:unit->('ct) array)=
    if Hashtbl.mem cache_f n then
      Hashtbl.replace cache_f n o

  method get_cache_fun n=
    (try 
       Hashtbl.find cache_f n
     with Not_found -> raise (Cache_fun_not_found n));

  method first_free()=
    let i=ref 0 in
    let r=ref (-1) in
      while (!r=(-1) && !i<Weak.length cache) do
	(match Weak.get cache !i with
	  | Some v->()
	  | None -> r:= !i);
	i:= !i+1;
      done;
      if !r=(-1) then raise Cache_full;
      !r

  method private simple_of ol=
    (try
       ol.(0)
     with Invalid_argument i-> raise (Cache_out_of_bound ("simple_of",0)))

  method add_cache n o_f=
    if self#is_cache_fun n=false then (
(*    print_string ("CACHE: add "^n^"");print_newline(); *)
    
    let ff=self#first_free() in
      self#add_cache_fun n o_f;
      self#set_corres n ff;
      let t1=Unix.gettimeofday() in
      let o=o_f() in
      let t2=Unix.gettimeofday() in
(*	(self#simple_of o)#set_id n; *)
	Weak.set cache ff (Some (n,o));
(*	print_string ("CACHE: time "^string_of_float (t2-.t1));print_newline();*)
	if (t2-.t1>mt) then (
	  self#cache_file_save n o;
	  self#replace_cache_fun n (fun()->self#cache_file_load n);
	);
	Hashtbl.add cache_time n (t2-.t1);
    );

  method replace_cache n o=
(*    print_string ("CACHE: replace "^n^"");print_newline(); *)
    let ff=self#get_corres n in
      self#replace_cache_fun n o;
      Weak.set cache ff (Some (n,o()));

  method reload_cache n=
(*    print_string ("CACHE: reload "^n);print_newline(); *)
    let o=(self#get_cache_fun n)() in
    let ff=self#first_free() in
      self#set_corres n ff;
(*      (self#simple_of o)#set_id n; *)
      Weak.set cache ff (Some (n,o));
      o

  method get_cache n=
    match (Weak.get cache (self#get_corres n)) with
      | Some (nn,v)->(*if (self#simple_of v)#get_id=n then*) 
	  if nn=n then
	    v
	  else self#reload_cache n 
      | None ->self#reload_cache n 


  method get_cache_entry n i=
    (try
       (self#get_cache n).(i)
     with Invalid_argument v->raise (Cache_out_of_bound (n,i)))

  method get_cache_simple n=
    self#get_cache_entry n 0;


    
end;;
