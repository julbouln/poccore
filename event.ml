
type key_type=
| KeyBackspace
| KeyReturn
| KeySpace
| KeyCtrl
| KeyAlt
| KeyEchap
| KeyUp
| KeyDown
| KeyRight
| KeyLeft
| KeyShift
| KeyUnicode of UChar.t
| KeyChar of string
| KeyUnknow;;

type mouse_event=
  | MouseClick of (int*int*int) (* x,y,button *)
  | MouseRelease of (int*int*int)
  | MouseMotion of (int*int) (* x,y *) 
  | MouseError
;; 

type keyboard_event=
  | KeyboardPress of (key_type*key_type)
  | KeyboardRelease of (key_type*key_type)
  | KeyboardError;;

type event=
  | EventMouse of mouse_event
  | EventKeyboard of keyboard_event
  | EventError

(** Event manager *)
class virtual event_manager=
object(self)
  val mutable parser=(fun e->())
  val mutable on_loop=(fun()->())
  val mutable on_quit=(fun()->())

  method set_parser (p:event->unit)=parser<-p
  method get_on_loop=on_loop
  method set_on_loop f=on_loop<-f
  method set_on_quit f=on_quit<-f


  method virtual init: unit->unit
  method virtual main:unit->unit
  
end;;
