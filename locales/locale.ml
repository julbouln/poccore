
class locale (l: string)=
object
 val mutable lang=l
 val mutable tbl=Hashtbl.create 2
 method add (e: string) (t: string)= Hashtbl.add tbl e t

 method get (e: string)= 
 if Hashtbl.mem tbl e then
  let t=Hashtbl.find tbl e in
  if t="" then e else t
 else
  e

end;;

let locales=Hashtbl.create 2;;
Hashtbl.add locales "en" (new locale "en");;
