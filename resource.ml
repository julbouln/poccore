
open Low;;

class ['a] resource iid (v:'a)=
object
   val mutable id=iid
   
   val mutable access=0
   method get_access=access
   val mutable data=v
   method set_data nv=data<-nv
   method get_data=access<-access+1;v
   method free_data()=()
end;;

exception Resource_not_found of string;;

class ['a,'b] resource_manager (v:'a) (f:string->'a->'b)=
object
  
  val mutable datas=Hashtbl.create 2

  method del_resource (n:string)=
    if Hashtbl.mem datas n then
      (
	let od=Hashtbl.find datas n in
	  od#free_data();
	  Hashtbl.remove datas n
      )
  method add_resource (n:string) (d:'a)=
    if Hashtbl.mem datas n=false then
      Hashtbl.add datas n (f n d)

  method get_resource (n:string)=
    (try 
       let r=Hashtbl.find datas n in
	 r#get_data
     with Not_found -> raise (Resource_not_found n)
    )
end;;

class tile_resource iid=
object
  inherit [tile] resource iid (tile_empty())
  method free_data()=tile_free data
end;;

class tile_resource_manager=
object
  inherit [tile,tile_resource] resource_manager (tile_empty()) 
    (fun n d->
       let tr=new tile_resource n in
	 tr#set_data d;
	 tr
    ) 
end;;


let tile_resources=new tile_resource_manager;;

class graphic=
object(self)

  val mutable resid="none"

  method set_tile t=tile_resources#add_resource resid t
  method get_tile=tile_resources#get_resource resid

  method load f=resid<-f;self#set_tile (tile_load f)
  method put x y=tile_put self#get_tile x y  
end;;
