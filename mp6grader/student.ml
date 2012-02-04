(*
 * File: mp6-skeleton.ml
 *)

open Mp6common

(* Problem 1*)
let asMonoTy1 () = TyConst({name="->"; arity=2},
                    [ TyConst({name="bool"; arity=0},[]);
                      TyConst({name="list"; arity=1},
                     [TyConst({name="int"; arity=0},[])])
                    ]);;
let asMonoTy2 () = TyConst({name="->"; arity=2},
                    [
                        fresh();
                        TyConst({name="->"; arity=2},
                        [
                            fresh();
                            TyConst({name="->"; arity=2},
                            [
                                fresh();
                                fresh();
                            ])
                        ])
                    ])
let asMonoTy3 () = TyConst({name="->"; arity=2},
                    [
                        fresh();
                        TyConst({name="list";arity=1},
                        [
                            TyConst({name="*";arity=2},
                            [
                                fresh();
                                TyConst({name="int";arity=0},[])
                            ])
                        ])
                    ])
let asMonoTy4 () = TyConst({name="*"; arity=2},
                    [
                        TyConst({name="string"; arity=0},[]);
                        TyConst({name="->"; arity=2},
                        [
                            TyConst({name="list";arity=1},
                            [
                                fresh()
                            ]);
                            fresh()
                        ])
                    ])

(* Problem 2*)
let rec subst_fun subst m = 
    match subst with
    | [] -> TyVar(m)
    | (i, sub)::xs ->
            if (m == i) then sub else subst_fun xs m
;;

(* Problem 3*)
let rec monoTy_lift_subst subst monoTy =
    match monoTy with
    | TyVar(t) ->
        subst_fun subst t
    | TyConst(c,l) ->
        TyConst(c,List.map (monoTy_lift_subst subst) l)
;;

(* Problem 4*)
let orFun a b = a || b;;

let rec occurs x ty =
    match ty with
    | TyVar(t) ->
        if t = x then true else false
    | TyConst(c,l) ->
        List.fold_left (orFun) false (List.map (occurs x) l)
;;

(* Problem 5*)
let cross x y = (x,y);;

let rec replace_monoTy t s monoTy =
    match monoTy with
    | TyVar(t) -> monoTy
    | TyConst(c,l) ->
        TyConst(c,List.map (replace_monoTy t s) l)
;;
let rec replace t s l =
    match l with
    | [] -> []
    | (a,b) :: xs ->
        if a = t && b = t then
            (s,s) :: (replace t s xs)
        else (if a = t then
            (s,replace_monoTy t s b) :: (replace t s xs)
        else (if b = t then
            (replace_monoTy t s a,s) :: (replace t s xs) else
            (a,b) :: (replace t s xs)))
;;

let rec unify eqlst =
    match eqlst with
    | [] -> Some ([])
    | ((s,t)::xs) -> 
        (if s = t then
            unify xs
        else 
            match s,t with
            | TyConst(s_a,s_b),TyVar(t_v) ->
                unify ((t,s) :: xs)
            | TyConst(s_a,s_b),TyConst(t_a,t_b) ->
                unify (List.append xs (List.map2 cross s_b t_b))
            | TyVar(s_v),t ->
                if occurs s_v t then
                    None
                else ( let sub = unify (replace s t xs) in
                    match sub with
                    | None -> None
                    | Some(y) -> Some ((s_v,(monoTy_lift_subst y t)) :: y)
                )
        )
;;

(* Extra Credit *)
let equiv_types ty1 ty2 = failwith "Not implemented"
