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
  method virtual get_f: 't
  method virtual set_f: 't->unit  

  method virtual load:font_t->unit
  method virtual get_size:int
  method virtual get_height:int
  method virtual sizeof_text:string->(int*int)
  method virtual get_font_type:font_t
(*  method virtual create_text:string->color *)

end;;


