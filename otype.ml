exception No_obj_type of string;;

class ['a] obj_types (none_obj:'a)=
  object
    val mutable object_types=let a=Hashtbl.create 2 in Hashtbl.add a "none" (function()->
(none_obj

));a
    val mutable objects=let a=Hashtbl.create 2 in Hashtbl.add a "none" (
none_obj
)
;a
    method add_object_type nm (obj:unit->'a)=
      if(Hashtbl.mem objects nm)==false then
	Hashtbl.add objects nm (obj());	  
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