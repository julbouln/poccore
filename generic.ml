

(** the most lowlevel object *)
class generic_object=
object
  val mutable id="none"
  (** set the id of this object *)
  method set_id (i:string)=id<-i
  (** get the id of this object *)
  method get_id=id
end;;


exception Object_not_found of string;;

class ['a] generic_object_handler=
object(self)
  val mutable objs=Hashtbl.create 2

  method add_object (id:string option) (o:'a)=
    let nid=
      (match id with
	 | Some nid->nid
	 | None ->("object#"^string_of_int (Oo.id o)))
       in
      o#set_id nid;
      Hashtbl.add objs nid o;nid

  method replace_object id o=
    Hashtbl.replace objs id o

  method get_object id=
    (try
       Hashtbl.find objs id
     with Not_found -> raise (Object_not_found id))

  method delete_object id=
    Hashtbl.remove objs id

  method rename_object id nid=
    let o=self#get_object id in
      self#add_object (Some nid) o;
      self#delete_object id;
    
  method foreach_object f=
    Hashtbl.iter f objs

end;;
