

(*

ACTION:
* on_start function 
* repeated function
* on_stop function

* stop_event : action stop when bool
* change_event : action change when bool


STATE
* state priority
* state stack (state change take effect at event 


ex : move 
on_start: set initial value (dest point, reinit prect, reinit cur_path, set path,...)
act : move funct
on_stop : none

stop_event : dest point = cur point
change_event : pixel pos = 0,0


*)

(* for simplification "go" mean "game_object" *)

open Anim;;
open Rect;;

(* 8< Not used for now *)

type action_func_arg=
  | Point of point
  | Rectangle of rectangle
  | Unit of (int * int)
  | Decor of int
  | Resource of int
  | Nil;;

class virtual go_action_func=
object

  method parse_args (args:action_func_arg list) f=
    List.iter (
      fun arg->
	f arg
    )
      args;

  method virtual exec : action_func_arg list -> unit

end;;


class go_action_func_mutable=
object
  inherit go_action_func
  val mutable func=function v->()

  method set_func f=func<-f
  method get_func=func

  method exec (v:action_func_arg list)=func v

end;;

(* >8 *)

class game_object_action=
  object (self)
    val mutable acting=false

    (* functions *)			 
    val mutable action=function()->()
    
    val mutable on_start=function()->()
    val mutable on_stop=function()->()
 
    val mutable on_change_event=function()->true
    val mutable on_stop_event=function()->true

    method set_action a=action<-a 
    method set_on_start a=on_start<-a 
    method set_on_stop a=on_stop<-a 

    method set_change_event a=on_change_event<-a
    method set_stop_event a=on_change_event<-a

    method start()=acting<-true
    method stop()=acting<-false
    method act()=if acting==true then action()

    method act_on_start()=on_start()
    method act_on_stop()=on_stop()

    method stop_event=on_stop_event()
    method change_event=on_change_event()			  
  
    method get_acting=acting
    method set_acting a=acting<-a
  end;;




class game_object_state n f r =
  object
    inherit object_anim f r
    inherit game_object_action 

    val mutable name=n
    method get_name=name
    method set_name nm=name<-nm
    method print_name=print_string name

    val mutable priority=0
    method set_priority p=priority<-p
    method get_priority=priority
  end;;


exception State_not_found of string


class game_object_state_manager=
object(self)
  val mutable states=
    let a=Hashtbl.create 2 in
      Hashtbl.add a "none" (new game_object_state "none" [|0|] 12 );
      a
 
  val mutable stack_states=Stack.create()
  method next_state()=
    if Stack.length stack_states<> 0 then
    (
      let (ns,np)=Stack.pop stack_states in
	self#set_cur_state ns;
	self#current_state#set_priority np
    )


  val mutable cur_state="none"
  method set_cur_state s=cur_state<-s
  method get_cur_state=cur_state 

  method add_state (s:game_object_state)=
    Hashtbl.add states s#get_name s
  method get_state n=
    if Hashtbl.mem states n then
      Hashtbl.find states n
    else raise (State_not_found n)

  method set_state s p=self#set_instant_state s p

  method set_instant_state s p=
    self#set_cur_state s;
    self#current_state#set_priority p

  method set_decaled_state s p=
    if p>= self#current_state#get_priority then (
(*      print_string ("STATE: next is "^s);print_newline();*)
      Stack.push (s,p) stack_states; 
(*      self#set_cur_state s;
      self#current_state#start();
*)						   
    )
  method current_state=self#get_state cur_state

  method act()=
    self#current_state#act();
    if self#current_state#stop_event then (
(*      self#next_state(); *)
(*      print_string ("STATE: switch to "^cur_state);print_newline(); *)
    )
end;;

