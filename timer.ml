

(*
 
 h=30x60x60 f = 108000 f
 m = 30x60 f = 1800 f
 s = 30 f
 f = f
*)

type time=
{
  h:int;
  m:int;
  s:int;
  f:int;
}


class timer=
object(self)
  val mutable timers=Hashtbl.create 2
  method add_timer (t:time) (f:unit->unit)=
    Hashtbl.add timers (self#from_time t) f
  method del_timer (t:time)=
    Hashtbl.remove timers (self#from_time t)

  val mutable tasks=Hashtbl.create 2
  method add_task (t:time) (f:unit->unit)=
    Hashtbl.add tasks (self#from_time t) f
  method is_task (t)=
    Hashtbl.mem tasks (self#from_time t)

  method del_task (t:time)=
    Hashtbl.remove tasks (self#from_time t)
      
  (* default max time to 1m = 1800 frm *)
  val mutable frm=1800
    
  val mutable cfrm=0
 
  method get_cfrm=cfrm

  val mutable run=false

  method start()=run<-true
  method stop()=run<-false

  method set_limit t=frm<-self#from_time t

  method get_cur_frame=cfrm

  method reset()=cfrm<-0

  method step()=
    if run then (
      Hashtbl.iter 
	(
	  fun tfr e->
	    if cfrm mod tfr=0 then e()
	) tasks;

      if Hashtbl.mem timers cfrm then (
	let e=Hashtbl.find timers cfrm in e();
	  Hashtbl.remove timers cfrm;
      );

      if cfrm<frm then
	cfrm<-cfrm+1
      else
	cfrm<-0
    )

  method to_time fr=
    let h=fr/108000 and
	m=(fr mod 108000)/1800 and
	s=((fr mod 108000) mod 1800)/30 and
	f=(((fr mod 108000) mod 1800) mod 30) in
      {
	h=h;
	m=m;
	s=s;
	f=f;
      }
	
  method from_time t=    
    (t.h*108000)+ (t.m * 1800) + (t.s*30) + t.f

  method add_timer_from_now (t:time) (f:unit->unit)=
    print_string "GAME_TIME: add timer ";
    let ft=self#from_time t in
    let nt=self#to_time (ft+cfrm) in
      print_int cfrm;
      self#add_timer nt f;
	print_newline();
end;;
