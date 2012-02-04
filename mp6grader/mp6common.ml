(* File: mp6common.ml *)

(*type system*)

type typeVar = int

let rec expand n (list,len) =
    let q = n / 26 in
        if q = 0 then (n :: list, len + 1)
        else expand q (((n mod 26)::list), len + 1);;

let string_of_typeVar n = 
   let (num_list,len) =
       match (expand n ([],0))
       with ([],l) -> ([],l) (* can't actually happen *)
          | ([s],l) -> ([s],l)
          | (x::xs,l) -> ((x - 1) :: xs, l)
   in
   let s = (String.create len)
   in
   let _ =
    List.fold_left
    (fun n c -> (String.set s n c; n + 1))
    0
    (List.map (fun x -> Char.chr(x + 97)) num_list)  (* Char.code 'a' = 97 *)
   in "'"^s;;

type constTy = {name : string; arity : int}

type monoTy = TyVar of typeVar | TyConst of (constTy * monoTy list)

let rec string_of_monoTy t =
  let rec string_of_tylist = function
     []     -> ""
   | t'::[] -> string_of_monoTy t'
   | t'::ts -> string_of_monoTy t'^ ","^ string_of_tylist ts
  in
  let string_of_subty s =
  match s with 
     TyConst ({name = "*"; arity = 2}, _)
   | TyConst ({name = "->"; arity = 2}, _) ->
     ("("^ string_of_monoTy s^ ")")
   | _ ->  string_of_monoTy s
  in 
    match t with
       TyVar n         -> (string_of_typeVar n)
     |TyConst ({name = name; arity = 0}, []) -> name
     |TyConst ({name = name; arity = 1}, [ty]) ->
      (string_of_subty ty^ " "^ name)
     |TyConst ({name = "*"; arity = 2}, [ty1; ty2]) ->
      (string_of_subty ty1^ " * "^ string_of_monoTy ty2)
     |TyConst ({name = "->"; arity = 2}, [ty1; ty2]) ->
      (string_of_subty ty1^ " -> "^ string_of_monoTy ty2)
     |TyConst ({name = name; arity = _}, tys) ->
      ("("^ string_of_tylist tys^ ") "^ name)

(*fresh type variable*)
let (fresh, reset) =
   let nxt = ref 0 in
   let f () = (nxt := !nxt + 1; TyVar(!nxt)) in
   let r () = nxt := 0 in
    (f, r)

let rec accummulate_freeVarsMonoTy fvs ty =
    match ty
    with TyVar n -> n::fvs
       | TyConst (c, tyl) -> List.fold_left accummulate_freeVarsMonoTy fvs tyl

let freeVarsMonoTy ty = accummulate_freeVarsMonoTy [] ty

let bool_ty = TyConst({name="bool"; arity = 0},[])
let int_ty = TyConst ({name="int"; arity = 0}, [])
let float_ty = TyConst ({name="float"; arity = 0},[])
let string_ty = TyConst ({name="string"; arity = 0},[])
let unit_ty = TyConst({name="unit"; arity = 0}, [])
let mk_pair_ty ty1 ty2 = TyConst({name="*"; arity = 2},[ty1;ty2])
let mk_fun_ty ty1 ty2 = TyConst({name="->"; arity = 2},[ty1;ty2])
let mk_list_ty ty = TyConst({name="list"; arity = 1},[ty])

type substitution = (typeVar * monoTy) list

let string_of_substitution subst =
    let rec aux n acc subst =
        match subst with [] -> "]\n"
        | (x,ty)::rest ->
          let next = 
              string_of_typeVar x ^ " |--> " ^ string_of_monoTy ty ^
              (match rest with [] -> "]\n" | _ -> ";") in
          let m = String.length next in
          let p = n + m in
          if p > 79 then aux m (acc ^"\n"^next) rest else aux p (acc^" "^next) rest
    in aux 1 "[" subst


let subst_sort slst = 
	let comp = (fun a b -> match a, b with (i1,t1), (i2,t2) -> compare i1 i2) in 
	List.stable_sort comp slst;;

let subst_to_eqlst s = List.map (fun (n, t) -> (TyVar n, t)) s

let subst_fun (s:substitution) n = (try List.assoc n s with _ -> TyVar n)

(* Some general purpose functions *)

let option_map f o = (match o with None -> None | Some x -> Some (f x))
let option_compose f g o = option_map f (option_map g o)
let partial_map_compose f g x = 
    match g x with None -> None | Some y -> f y

