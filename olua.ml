(** Oo style lua-ml *)

module T  = Lua.Lib.Combine.T1 (Luaiolib.T)
module WT = Lua.Lib.WithType (T)
module C  = Lua.Lib.Combine.C4 (Luaiolib.Make (T.TV1)) (Luacamllib.Make(T.TV1))
                               (WT(Luastrlib.M)) (WT(Luamathlib.M))
			       
module I = Lua.MakeInterp (Lua.Parser.MakeStandard) (Lua.MakeEval (T) (C))



module OLuaVal = I.Value
let ( **-> ) = OLuaVal. ( **-> )
let ( **->> ) x y = x **-> OLuaVal.result y

(** lua class *)
class lua_interp=
object(self)
val mutable vals=DynArray.create();
val mutable interp=I.mk()

method set_global_val n f=
self#add_val n f;
self#register_vals_global()

method set_module_val m n f=
self#add_val n f;
self#register_vals_module m


method get_val f=
I.getglobal interp (OLuaVal.String f)

method add_val n f=
DynArray.add vals (n,f)

method register_vals_global()=
 I.register_globals
 (DynArray.to_list vals)
 interp;
 DynArray.clear vals;
 
method register_vals_module m=
 I.register_module m
 (DynArray.to_list vals)
 interp;
 DynArray.clear vals;
		
method parse e= I.dostring interp e
end;;

let test=new lua_interp in
test#set_module_val "Test" "test" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) Sys.getenv);
test#set_module_val "Test" "test2" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) (fun v->v));
test#set_global_val  "a" (OLuaVal.String "bla2");
let a=(test#get_val "a") in
print_string (OLuaVal.to_string a);

(*
test#parse "
function test3 () 
print (\"test3\") 
end
print(\"bla\");
print (Test.test (\"PWD\"));
print (Test.test2 (\"PWD\"));
print (a)
";

test#parse "test3()";
*)
