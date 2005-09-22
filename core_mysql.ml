(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

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


open Mysql;;

(** Mysql interaction *)

exception Sql_not_connected;;

class sql_connection=
object(self)
  val mutable db=None
  method get_db=
    match db with 
      | Some d->d
      | None->raise Sql_not_connected
	  
  method connect host dbn pass user=
    db<-Some (Mysql.quick_connect ~host:host ~database:dbn ~password:pass ~user:user ());

  method select_db dbn=
    Mysql.select_db self#get_db dbn

  method exec q rf=
    let r=Mysql.exec self#get_db q in
      Mysql.iter r rf;

  method disconnect()=
    Mysql.disconnect self#get_db;
    db<-None;
end


open Value_xml;;

class sql_xml=
object(self)
  inherit sql_connection as conn  

  method is_node tbl id=
    let r=ref false in
      conn#exec ("select id from "^tbl^" where id=\""^id^"\"")
	(fun s->(
	   match s.(0) with
	       | Some v->r:=true
	       | None -> ()
	 )
	); 
      !r

  method private escape_str s=
    Str.global_replace  (Str.regexp "\"") "\\\"" s

  method add_node tbl id (node:xml_node)=
    let q=("insert into " ^ tbl ^ " values (0,\"" ^id^ "\",\"" ^ self#escape_str node#to_string ^ "\")") in
(*      print_string q;print_newline(); *)
    conn#exec q (fun s->());

  method save_node tbl k id (node:xml_node)=
    match k with
      | Some n->
	  conn#exec ("update " ^ tbl ^ " set xml=\"" ^ self#escape_str node#to_string ^ "\" where key="^string_of_int n^" id=\"" ^id^ "\"") (fun s->());
      | None ->
	  conn#exec ("update " ^ tbl ^ " set xml=\"" ^ self#escape_str node#to_string ^ "\" where id=\"" ^id^ "\"") (fun s->());

  method load_node tbl id=
(*    let x=new xml_node in *)
    let na=DynArray.create() in
      conn#exec ("select xml from "^tbl^" where id=\""^id^"\"")
	(fun s->(
	   match s.(0) with
	       | Some v->
		   let x=new xml_node in
		     x#of_string v;
		     DynArray.add na x
	       | None -> ()
	 )
	); 
      DynArray.to_array na
end

