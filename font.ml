open Generic;;
open Cache;;

(** Font management *)

type font_t=
  | FontTTF of (string*int)
  | FontEmbed;;

exception Font_not_initialized;;

class virtual ['t] font_object=
object
  inherit generic_object

  val mutable f=None
  method get_f=
    match f with
      | Some v->v
      | None -> raise Font_not_initialized;
  method set_f (nt:'t)=f<-(Some nt)


  method virtual get_f: 't
  method virtual set_f: 't->unit  

  method virtual load:font_t->unit
  method virtual get_size:int
  method virtual get_height:int
  method virtual sizeof_text:string->(int*int)
  method virtual get_font_type:font_t
(*  method virtual create_text:string->color *)

end;;


