open Value_lua;;

(** Type manager *)

exception No_obj_type of string;;

class ['a] obj_types=
object(self)
    inherit lua_object as lo
    val mutable object_types=Hashtbl.create 2
    val mutable objects=Hashtbl.create 2

    method get_id="obj_types"
    method add_object_type nm (obj:unit->'a)=
(*      print_string ("OBJ_TYPES : add object type "^nm);print_newline(); *)

      if(Hashtbl.mem objects nm)==false then (
	let o=obj() in
	  o#lua_init();
	  self#lua_parent_of nm (o:>lua_object);
	  Hashtbl.add objects nm (o);	  
      );
      Hashtbl.add object_types nm obj

    method get_object_type nm=
      try
	(Hashtbl.find object_types nm)()
      with Not_found -> raise (No_obj_type nm)
    method get_object nm=      
      (Hashtbl.find objects nm)

    method is_object_type nm=(Hashtbl.mem objects nm)

    method count_objects_type=Hashtbl.length objects			       
    method foreach_object_type f= 
      Hashtbl.iter f object_types

end;;
