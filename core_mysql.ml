

open Mysql;;


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
    let q=("insert into " ^ tbl ^ " values (\"" ^id^ "\",\"" ^ self#escape_str node#to_string ^ "\")") in
(*      print_string q;print_newline(); *)
    conn#exec q (fun s->());

  method save_node tbl id (node:xml_node)=
    conn#exec ("update " ^ tbl ^ " set xml=\"" ^ self#escape_str node#to_string ^ "\" where id=\"" ^id^ "\"") (fun s->());

  method load_node tbl id=
    let x=new xml_node in
      conn#exec ("select xml from "^tbl^" where id=\""^id^"\"")
	(fun s->(
	   match s.(0) with
	       | Some v->x#of_string v
	       | None -> ()
	 )
	); 
      x
end

