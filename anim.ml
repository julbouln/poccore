open Medias;;

(** Anim subsystem *)

(** Anim class *)
class anim_object frames refresh=
object
    val mutable current_frame=0
    val mutable current_refresh=0
    val mutable frames=frames
    val mutable refresh=refresh

    method get_num_frames=Array.length frames
    method set_frames f=frames<-f
    method set_refresh r=refresh<-r
    method get_frames=frames
    method get_refresh=refresh
    method get_frame=frames.(current_frame)
    method get_current=current_frame
    method get_current_refresh=current_refresh
    method set_current c=current_frame<-c
    method set_current_refresh c=current_refresh<-c

    method anim()=
      if current_refresh = refresh then (
	if current_frame < (Array.length frames)-1 then ( 	  
	  current_frame<-current_frame+1;
	  

	 )
	else
	  (
	   current_frame<-0;
	  );
	current_refresh<-0
       )
      else
	current_refresh<-current_refresh+1

    method print_frame=print_int frames.(current_frame)
  end;;


(** Animated graphic object *)
class graphic_with_anim (graph:graphic_generic_object) frames refresh= 
object
  val mutable anim=new anim_object frames refresh

  method get_anim=anim

  method move x y=graph#move x y
  method anim()=anim#anim();
  method put()=graph#set_cur_tile(anim#get_frame);graph#put();

end;;

(** Animated graphic object *)
class graphic_object_anim w h file frames refresh= 
object
  val mutable anim=new anim_object frames refresh
  val mutable graph=new graphic_scr_resized_object w h file false false

  method get_anim=anim

  method move x y=graph#move x y
  method anim()=anim#anim();
  method put()=graph#set_cur_tile(anim#get_frame);graph#put();

end;;
