(** Dijkstra algorithme implementation *)
(* FROM Développement d'applications avec Objective Caml *)

type  cout  = Nan | Cout of float;;

type  mat_adj =  cout array array;;

 

type 'a graphe = { mutable ind : int; 
                    taille : int; 
                    sommets : 'a array;  
                    m :  mat_adj;
		    sadj : (int, int array) Hashtbl.t;
		    rev_sommets:((int*int),int) Hashtbl.t
		 };;


(* 

utiliser des angle (N,NE,E,SE,S,SW,W,NW) au lieu de position (x,y) pour les arc.
pour chaque sommet, on aurait donc les arc en direction possible. La fonction de calcul de trajectoire retournerai ainsi une liste d'angle a atteindre.

*)

(*
type direction=
  | N 
  | NE 
  | E 
  | SE
  | S
  | SW
  | W
  | NW;;

let dir_x d=
match d with
  | N -> 0
  | NE -> 1
  | E -> 1
  | SE -> 1
  | S -> 0
  | SW -> -1
  | W -> -1
  | NW -> -1
;;

let dir_y d=
match d with
  | N -> -1
  | NE -> -1
  | E -> 0
  | SE -> 1
  | S -> 1
  | SW -> 1
  | W -> 0
  | NW -> -1
;;

let dir_cout d=
match d with
  | N -> 1.
  | NE -> 1.41
  | E -> 1.
  | SE -> 1.41
  | S -> 1.
  | SW -> 1.41
  | W -> 1.
  | NW -> 1.41
;;
*)
(*
(x,y)->[N;NE;S;SE]
  ___ ___ ___ 
 |NW | N | NE|
 |___|___|___|
 | W |x,y| E |
 |___|___|___|
 |SW | S | SE|
 |___|___|___|
 

m.(point1).(point2)=cout

*)

let appartient s g=
  Hashtbl.mem g.rev_sommets s;;

let index s g=
  Hashtbl.find g.rev_sommets s;;

(* TOO HIGH *)
(*
 let appartient s g = 
   let rec aux i =
     (i < g.taille) & ((g.sommets.(i) = s) or (aux (i+1)))
   in aux 0;;


let index s g = 
   let rec aux i = 
     if i >= g.taille then raise Not_found
     else if g.sommets.(i) = s then i 
          else aux (i+1)
   in aux 0 ;;
*)

let cree_graphe s t = 
   { ind = 0; taille = t; sommets = Array.create t s; 
     m = Array.create_matrix t t Nan; sadj=Hashtbl.create 2; rev_sommets=Hashtbl.create 2} ;;



let sadj_add p g=
  let da=DynArray.create() in
  let s=g.sommets.(p) in
  let wi=Array.length g.m and
    hi=Array.length g.m in
  let (sx,sy)=s in
  for k=(-1) to 1 do
    for l=(-1) to 1 do
      if (sx+k)>0 && (sy+l)>0 && (sx+k)<wi && (sy+l)<hi && (k<>0 || l<>0) then (
	if appartient (sx+k,sy+l) g then (
	let i=index (sx+k,sy+l) g in	  
	  DynArray.add da i
	)
      )
    done;
  done;
    DynArray.to_array da
;;

let sadj_init g=
Array.iteri (
fun ind v ->
Hashtbl.add g.sadj ind (sadj_add ind g); 
) g.sommets;;


let sadj_around ind g f=
  let aa=Hashtbl.find g.sadj ind in
    Array.iter f aa
      
;;

let ajoute_sommet s g =
   if g.ind = g.taille then failwith "le graphe est plein"
   else if appartient s g then failwith "le sommet existe déjà"
   else (g.sommets.(g.ind) <- s; Hashtbl.add g.rev_sommets s g.ind ;g.ind <- g.ind + 1) ;;

let get_m g x y=
g.m.(x).(y) 
(*
  match g.m.(x).(y) with
    | Cout c -> Cout c
    | Nan -> 
	match g.m.(y).(x) with
	  | Cout c -> Cout c
	  | Nan -> Nan;;
*)



 let ajoute_arc s1 s2 c g = 
  try 
     let x = index s1 g and y = index s2 g in 
       g.m.(x).(y) <- Cout c;
(*       g.m.(y).(x) <- Cout c;*)
	 
   with Not_found -> failwith "sommet inexistant" ;; 



type etat_recherche = { chemins : int array;
                        deja_traites : bool array;
                        distances : cout array;
                        source :int; 
                        nb : int};;
let creer_etat () =  { chemins = [||]; deja_traites = [||]; distances = [||]; 
                       nb = 0; source = 0};;

let a_cout c = match c with Nan -> false | _-> true;;

let float_of_cout c = match c with 
    Nan -> failwith "float_of_cout"
  | Cout x -> x;;
let add_cout  c1 c2 = match (c1,c2) with 
    Cout x, Cout y -> Cout (x+.y)
  | Nan, Cout y -> c2
  | Cout x, Nan -> c1 
  | Nan, Nan ->  c1;;
let inf_cout  c1 c2 = match (c1,c2) with 
    Cout x, Cout y -> x < y
  | Cout x, Nan -> true
  | _, _ ->  false;;


exception Trouve of int;;



let premier_non_traite er g= 
  try 
    for i=0 to er.nb-1 do 
      if not er.deja_traites.(i) then raise (Trouve i)
    done; 
    raise Not_found;
    0
  with Trouve i -> i ;;

let pp_non_traite p er g=
  let si = ref p  
  and sd = ref er.distances.(p) in 
   for i=p+1 to er.nb-1 do  
(*    sadj_around p g 
    (fun i->  
*)
	 if not er.deja_traites.(i) then 
           if inf_cout er.distances.(i)  !sd then 
             ( sd := er.distances.(i);
               si := i )
(*      );  *)
   done;  
    !si,!sd;;

exception No_way;;

let une_etape er  g = 
  let p = premier_non_traite er g in 
  let np,nc = pp_non_traite p er g in
    if not(a_cout nc  ) then raise No_way 
    else 
      begin
	er.deja_traites.(np) <- true;
(*	for i = 0 to er.nb -1 do *)
    sadj_around np g 
    (fun i-> 

          if not er.deja_traites.(i) then 
            if a_cout (get_m g np i) 
(*g.m.(np).(i) *)then
              let ic = add_cout er.distances.(np)  (get_m g np i)  in 
		if inf_cout ic er.distances.(i)   then (
		  er.chemins.(i) <- np;
		  er.distances.(i) <- ic
		) 
    ); 
(*	done;  *)
	er
      end;;

let dij s g = 
  if appartient s g then 
    begin
      let i = index s g in 
      let er = { chemins = Array.create g.ind (-1) ;
                 deja_traites = Array.create g.ind false;
                 distances = Array.create g.ind Nan;
                 nb = g.ind;
                source = i}  in
	er.deja_traites.(i) <- true; 
	for j=0 to g.ind-1 do 
          let c = (get_m g i j) (*g.m.(i).(j)*) in 
            er.distances.(j) <- c;
            if a_cout c || g.sommets.(j)=s then er.chemins.(j) <- i 
	done;
	try
          for k = 0 to er.nb-2 do 
            ignore(une_etape er g) 
          done;
          er
	with No_way -> er
    end
  else failwith "dij: sommet inconnu";;



let aff_etat f (g,et)  dest = 
   if appartient dest g then 
      let d = index dest g in
        let rec aux is = 
           if is = et.source then Printf.printf "%a"  f g.sommets.(is)
           else (
             let old = et.chemins.(is) in 
             aux old;
(*             Printf.printf " -> (%4.1f) %a" (float_of_cout g.m.(old).(is))
                                          f g.sommets.(is)  *)
 Printf.printf " -> %a"  f g.sommets.(is)  

           )
        in 
          if not(a_cout et.distances.(d)) then Printf.printf "no way\n"
          else (
            aux d;
            Printf.printf " = %4.1f\n" (float_of_cout et.distances.(d)));;


(* CACHE *)

type 'a recherche_graphe =  
    { g : 'a graphe; w : etat_recherche Weak.t } ;;


let create_rech_graphe g = 
   { g = g;
     w = Weak.create g.ind } ;;

let dij_rapide s rg = 
   let i = index s rg.g in 
     match Weak.get rg.w i with 
       None -> let er = dij s rg.g in 
                 Weak.set rg.w i (Some er);
                 er
     | Some er -> er;;

 let del_arc s1 s2 g = 
   try
     let x = index s1 g and y = index s2 g in 
       g.m.(x).(y) <- Nan 
   with Not_found -> failwith "sommet inexistant" ;;


