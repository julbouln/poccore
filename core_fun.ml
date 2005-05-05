type graphic_fun=
{
  move: int->int->unit;
  get_x: unit->int;
  get_y: unit->int;
  get_w: unit->int;
  get_h: unit->int;
  set_cur_drawing: int->unit;
  get_cur_drawing: unit->int;
  get_drawings_size: unit->int;
  show: unit->unit;
  hide: unit->unit;
  set_layer: int->unit;
}

type sprite_fun=
{
  get_px: unit-> int;
  get_py: unit-> int;
  jump: int->int->unit;
}

type functionizer=
    [
      `GraphicFun of graphic_fun
    | `SpriteFun of sprite_fun	
    | `NoFun
    ]

class fun_node=
object
end;;
