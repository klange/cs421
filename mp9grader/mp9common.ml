(* File: mp9common.ml *)

(* Lexing types *)
type position = {line_num : int; char_num : int}

exception OpenComm of position
exception CloseComm of position
exception SuperCloseComm of position

(* constants for PicoML *)
type const = Bool of bool | Int of int | Float of float | String of string
           | Nil | Unit

let string_of_const c =
    match c 
    with Int n    -> string_of_int n
       | Bool b   -> if b then "true" else "false"
       | Float f  -> string_of_float f
       | String s -> "\"" ^ s ^ "\""
       | Nil      -> "[]"
       | Unit     -> "()"

(* Infixed binary operators for PicoML *)
type binop = Add | Sub | Mul | Div | Exp | FAdd | FSub | FMul | FDiv 
           | Concat | Cons | Comma | Eq | Less

let string_of_binop = function 
     Add  -> " + "
   | Sub -> " - "
   | Mul -> " * "
   | Div -> " / "
   | Exp -> " ** "
   | FAdd -> " +. "
   | FSub -> " -. "
   | FMul -> " *. "
   | FDiv -> " /. "
   | Concat -> " ^ "
   | Cons -> " :: "
   | Comma -> " , "
   | Eq  -> " = "
   | Less -> " < "

(* Primitive unary operator in PicoML *)
type monop = Head | Tail | Print | Neg | Fst | Snd

let string_of_monop m =
    match m
    with Head  -> "hd"
       | Tail  -> "tl"
       | Print -> "print_int"
       | Neg   -> "~"
       | Fst   -> "fst"
       | Snd   -> "snd"

(* expressions for PicoML *)
type exp =  
   | VarExp of string                (* variables *)
   | ConExp of const                 (* constants *)
   | IfExp of exp * exp * exp        (* if exp1 then exp2 else exp3 *)
   | AppExp of exp * exp             (* exp1 exp2 *)
   | BinExp of binop * exp * exp     (* exp1 % exp2
                                        where % is a builtin binary operator *)
   | MonExp of monop * exp           (* % exp1
                                        where % is a builtin monadic operator *)
   | FunExp of string * exp          (* fun x -> exp *)
   | LetExp of string * exp * exp    (* let x = exp1 in exp2 *)
   | RecExp of string * string * exp * exp    (* let rec f x = exp1 in exp2 *)
   | RaiseExp of exp                 (* raise exp *)
   | TryWithExp of exp * (int option * exp) * ((int option * exp) list)
                                (* try exp with n1 -> exp1 | ... | nm -> expm *)

(* Top-level declarations for PicoML *)
type toplvl = Anon of exp (* f 4;; *)
            | TopLet of (string * exp) (* let f = ... ;; *)
            | TopRec of (string * string * exp) (* let rec f x = ... ;; *)

let rec string_of_exc_match (int_opt, e) =
    let s = match int_opt with None -> "_" | Some n -> string_of_int n in
    (s^" -> "^ string_of_exp e)

and string_of_exp e =
    match e
    with VarExp x -> x
       | ConExp c -> string_of_const c
       | IfExp(bexp, thenexp, elseexp) ->
          "if "^string_of_exp bexp ^
          " then "^string_of_exp thenexp ^
          " else "^string_of_exp elseexp
       | AppExp(funexp,argexp) ->
         (match funexp
          with VarExp _ -> string_of_exp funexp
             | ConExp _ -> string_of_exp funexp
             | AppExp _ -> string_of_exp funexp
             | MonExp _ -> string_of_exp funexp
             | _ -> "("^string_of_exp funexp^")")
         ^" "^
         (match argexp
          with VarExp _ -> string_of_exp argexp
             | ConExp _ -> string_of_exp argexp
             | _ -> "("^string_of_exp argexp^")")
       | BinExp (binop, exp1, exp2) ->
         (match exp1
          with VarExp _ -> string_of_exp exp1
             | ConExp _ -> string_of_exp exp1
             | AppExp _ -> string_of_exp exp1
             | MonExp _ -> string_of_exp exp1
             | _ -> "("^string_of_exp exp1^")")
         ^" "^string_of_binop binop^" "^
         (match exp2
          with VarExp _ -> string_of_exp exp2
             | ConExp _ -> string_of_exp exp2
             | _ -> "("^string_of_exp exp2^")")
       | MonExp (monop, exp) -> string_of_monop monop ^" "^
         (match exp
          with VarExp _ -> string_of_exp exp
             | ConExp _ -> string_of_exp exp
             | _ -> "("^string_of_exp exp^")")
       | FunExp (x, exp) -> "fun "^x^" -> "^string_of_exp exp
       | LetExp (x,exp1,exp2) ->
          "let "^x^" = "^string_of_exp exp1^" in "^string_of_exp exp2
       | RecExp (f,x,exp1,exp2) ->
          "let rec "^f^" "^x^" = "^string_of_exp exp1^" in "^string_of_exp exp2
       | RaiseExp e -> "raise "^ string_of_exp e
       | TryWithExp (e,exc_match,match_list) ->
         "(try "^ string_of_exp e ^
          " with " ^ string_of_exc_match exc_match ^
          List.fold_left
           (fun s m -> (s ^ "| "^ string_of_exc_match m)) "" match_list^ ")";;

(* Util functions *)
let rec drop y = function
   []    -> []
 | x::xs -> if x=y then drop y xs else x::drop y xs

let rec delete_duplicates = function
   []    -> []
 | x::xs -> x::delete_duplicates (drop x xs)

(*type system*)

type typeVar = int

type constTy = {name : string; arity : int}

type monoTy = TyVar of typeVar | TyConst of (constTy * monoTy list)

let rec accummulate_freeVarsMonoTy fvs ty =
    match ty
    with TyVar n -> n::fvs
       | TyConst (c, tyl) -> List.fold_left accummulate_freeVarsMonoTy fvs tyl

let freeVarsMonoTy ty = delete_duplicates (accummulate_freeVarsMonoTy [] ty)

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

let bool_ty = TyConst({name="bool"; arity = 0},[])
let int_ty = TyConst ({name="int"; arity = 0}, [])
let float_ty = TyConst ({name="float"; arity = 0},[])
let string_ty = TyConst ({name="string"; arity = 0},[])
let unit_ty = TyConst({name="unit"; arity = 0}, [])
let mk_pair_ty ty1 ty2 = TyConst({name="*"; arity = 2},[ty1;ty2])
let mk_fun_ty ty1 ty2 = TyConst({name="->"; arity = 2},[ty1;ty2])
let mk_list_ty ty = TyConst({name="list"; arity = 1},[ty])

(*fresh type variable*)
let (fresh, reset) =
   let nxt = ref 0 in
   let f () = (nxt := !nxt + 1; TyVar(!nxt)) in
   let r () = nxt := 0 in
    (f, r)

type polyTy = typeVar list * monoTy  (* the list is for quantified variables *)

let freeVarsPolyTy ((tvs, ty):polyTy) = delete_duplicates(
    List.filter (fun x -> not(List.mem x tvs)) (freeVarsMonoTy ty))

let string_of_polyTy (bndVars, t) = match bndVars with [] -> string_of_monoTy t
    | _ ->  (List.fold_left
             (fun s v -> s ^ " " ^ string_of_typeVar v)
             "Forall"
             bndVars)
             ^ ". " ^ string_of_monoTy t

(* environments *)
type env = (string * polyTy) list

let freeVarsEnv l = delete_duplicates (
    List.fold_right (fun (_,pty) fvs -> freeVarsPolyTy pty @ fvs) l [])

let string_of_env gamma = 
  let rec string_of_env_aux gamma =
    match gamma with
       []        -> ""
     | (x,y)::xs -> x^ " : "^ string_of_polyTy y^
                    match xs with [] -> "" | _  -> ","^
                                                   string_of_env_aux xs
  in
    "{"^ string_of_env_aux gamma^ "}"

(*environment operations*)
let make_env x y = ([(x,y)]:env)
let rec lookup_env (gamma:env) x =
  match gamma with
     []        -> None
   | (y,z)::ys -> if x = y then Some z else lookup_env ys x
let sum_env (delta:env) (gamma:env) = ((delta@gamma):env)
let ins_env (gamma:env) x y = sum_env (make_env x y) gamma

(* fixed signatures *)
let const_signature const = match const with
   Bool b -> (([], bool_ty):polyTy)
 | Int n -> ([], int_ty)
 | Float f -> ([], float_ty)
 | String s -> ([], string_ty)
 | Nil -> ([0],mk_list_ty (TyVar 0))
 | Unit -> ([], unit_ty)

let int_op_ty = (([], (mk_fun_ty int_ty (mk_fun_ty int_ty int_ty))):polyTy)
let float_op_ty =
    (([], (mk_fun_ty float_ty (mk_fun_ty float_ty float_ty))):polyTy)
let string_op_ty =
    (([], (mk_fun_ty string_ty (mk_fun_ty string_ty string_ty))):polyTy)

let binop_signature binop = match binop with
     Add   -> int_op_ty
   | Sub   -> int_op_ty
   | Mul   -> int_op_ty
   | Div   -> int_op_ty
   | Exp   -> float_op_ty
   | FAdd   -> float_op_ty
   | FSub   -> float_op_ty
   | FMul   -> float_op_ty
   | FDiv   -> float_op_ty
   | Concat -> string_op_ty
   | Cons -> 
       let alpha = TyVar 0
       in ([0], 
              mk_fun_ty alpha (mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha)))
   | Comma ->
       let alpha = TyVar 0 in
       let beta = TyVar 1 in
           ([0;1],
            mk_fun_ty alpha (mk_fun_ty beta (mk_pair_ty alpha beta)))
   | Eq -> 
     let alpha = TyVar 0 in ([0],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))
   | Less -> 
     let alpha = TyVar 0 in ([0],mk_fun_ty alpha (mk_fun_ty alpha bool_ty))

let monop_signature monop = match monop with
    | Head -> let alpha = TyVar 0 in([0], mk_fun_ty (mk_list_ty alpha) alpha)
    | Tail -> let alpha = TyVar 0 in
                  ([0], mk_fun_ty (mk_list_ty alpha) (mk_list_ty alpha))
    | Print -> (([], mk_fun_ty int_ty unit_ty):polyTy)
    | Neg -> ([], mk_fun_ty int_ty int_ty)
    | Fst -> let t1,t2 = TyVar 0,TyVar 1
             in ([0;1],mk_fun_ty (mk_pair_ty t1 t2) t1)
    | Snd -> let t1,t2 = TyVar 0,TyVar 1
             in ([0;1],mk_fun_ty (mk_pair_ty t1 t2) t2)

(*judgment*) 
type judgment = { gamma:env; exp:exp; monoTy:monoTy }

let string_of_jexp {gamma=gamma;exp=exp;monoTy=monoTy} =
  string_of_env gamma ^ " |= "^ string_of_exp exp ^
  " : " ^ string_of_monoTy monoTy

type proof = {antecedents : proof list; conclusion : judgment}

(*proof printing*)
let string_of_proof p =
  let depth_max = 10 in
  let rec string_of_struts = function
     []    -> ""
   | x::[] -> (if x then "|-" else "|-")  (* ??? *)
   | x::xs -> (if x then "  " else "| ")^ string_of_struts xs
  in let rec string_of_proof_aux {antecedents = ant; conclusion = conc} depth lst =
    "\n"^ "  "^ string_of_struts lst^
    (if (depth > 0) then "-" else "")^
    let assum = ant in
      string_of_jexp conc ^
      if depth <= depth_max
         then string_of_assum depth lst assum
      else ""
  and string_of_assum depth lst assum =
    match assum with 
       []     -> ""
     | p'::ps -> string_of_proof_aux p' (depth + 1) (lst@[ps=[]])^
                 string_of_assum depth lst ps
  in
    string_of_proof_aux p 0 []^ "\n\n"

type substitution = (typeVar * monoTy) list

let subst_fun (s:substitution) n = (try List.assoc n s with _ -> TyVar n)

(*unification algorithm*)
(* Problem 1 *)
let rec contains n ty =
  match ty with
    TyVar m -> n=m
  | TyConst(st, typelst) ->
     List.fold_left (fun xl x -> if xl then xl else contains n x) false typelst;;

(* Problem 2 *)
let rec substitute ie ty = 
  let n,sub = ie 
  in match ty with
       TyVar m -> if n=m then sub else ty
     | TyConst(st, typelist) -> TyConst(st, List.map (fun t -> substitute ie t) typelist);;

let polyTySubstitute s (pty:polyTy) =
    match s with  (n,residue) ->
    (match pty with (bound_vars, ty) -> 
           if List.mem n bound_vars then pty
           else ((bound_vars, substitute s ty):polyTy))
    

(* Problem 3 *)
let rec monoTy_lift_subst (s:substitution) ty =
  match ty with
    TyVar m -> subst_fun s m
  | TyConst(st, typelst) ->  TyConst(st, List.map (fun t -> monoTy_lift_subst s t) typelst);;

let subst_compose (s2:substitution) (s1:substitution) : substitution =
    (List.filter (fun (tv,_) -> not(List.mem_assoc tv s1)) s2) @ 
    (List.map (fun (tv,residue) -> (tv, monoTy_lift_subst s2 residue)) s1)

let gen (env:env) ty =
    let env_fvs = freeVarsEnv env in
    ((List.filter (fun v -> not(List.mem v env_fvs)) (freeVarsMonoTy ty), ty):polyTy)

let freshInstance ((tvs, ty):polyTy) =
    let fresh_subst = List.fold_right (fun tv s -> ((tv,fresh())::s)) tvs [] in
    monoTy_lift_subst fresh_subst ty

let first_not_in n l =
    let rec first m n l =
        if n > 0 then
         if List.mem m l then first (m+1) n l else m :: (first (m+1) (n - 1) l)
        else []
    in first 0 n l

let alpha_conv ftvs (pty:polyTy) =
    match pty with (btvs, ty) ->
    (let fresh_bvars =
         first_not_in (List.length btvs) (ftvs @ (freeVarsPolyTy pty))
     in (fresh_bvars,
         monoTy_lift_subst (List.combine btvs (List.map (fun v -> TyVar v) fresh_bvars))
         ty))

let polyTy_lift_subst s pty =
	let rec fvsfun x r = match x with
		| TyVar n -> n :: r
		| TyConst (_, l) -> List.fold_right fvsfun l r
	in
	let fvs = List.fold_right fvsfun (snd(List.split s)
                                         @ List.map (fun x -> TyVar x) (fst (List.split s))) 
                                         [] in
    let (nbvs, nty) = alpha_conv fvs pty in
    ((nbvs, monoTy_lift_subst s nty):polyTy)

let env_lift_subst s (env:env) =
    ((List.map (fun (x,polyTy) -> (x,polyTy_lift_subst s polyTy)) env):env)


(* Problem 4 *)
let rec unify eqlst : substitution option =
  let rec addNewEqs lst1 lst2 acc =
    match lst1,lst2 with
      [],[] -> Some acc
    | t::tl, t'::tl' -> addNewEqs tl tl' ((t,t')::acc)
    | _ -> None
  in
  match eqlst with
    [] -> Some([])
    (* Delete *)
  | (s,t)::eqs when s=t -> unify eqs
    (* Eliminate *)
  | (TyVar(n),t)::eqs when not(contains n t)-> 
      let eqs' = List.map (fun (t1,t2) -> (substitute (n,t) t1 , substitute (n,t) t2)) eqs
      in (match unify eqs' with
           None -> None
         | Some(phi) -> Some((n, monoTy_lift_subst phi t):: phi))
    (* Orient *)
  | (TyConst(str, tl), TyVar(m))::eqs -> unify ((TyVar(m), TyConst(str, tl))::eqs)
    (* Decompose *)
  | (TyConst(str, tl), TyConst(str', tl'))::eqs when str=str' -> 
      (match (addNewEqs tl tl' eqs) with
        None -> None
      | Some l -> unify l)
    (* Other *)
  | _ -> None
;;


(*-----------------------------------------------*)

(*constraint list*)
type consList = (monoTy * monoTy) list


(*applying a substitution to a proof*)
let rec proof_lift_subst f = function
   {conclusion={gamma=gamma;exp=exp;monoTy=monoTy}; antecedents = assum} -> 
   {conclusion = {gamma=env_lift_subst f gamma;exp=exp;monoTy=monoTy_lift_subst f monoTy};
    antecedents = List.map (proof_lift_subst f) assum}


let get_ty = function
   None       -> raise(Failure "None")
 | Some(ty,p) -> ty

let get_proof = function
   None       -> raise(Failure "None")
 | Some(ty,p) -> p

let infer gather_exp gamma exp = 
  let ty = fresh() in
  let result = 
    match gather_exp {gamma=gamma;exp=exp;monoTy=ty} with
       None         -> None
     | Some(p,s) -> match s, ty with
          | f,TyVar n -> Some (subst_fun f n, proof_lift_subst f p)
          | _           -> None
  in let _ = reset() in
  result;;


let string_of_constraints c =
  let rec aux c =
     match c with 
     | [] -> ""
     | [(s,t)] ->  (string_of_monoTy s^ " --> "^ string_of_monoTy t)
     | (s,t)::c' -> (string_of_monoTy s^ " --> "^ string_of_monoTy t^
		     "; "^ aux c')
  in ("["^ aux c^ "]\n")

 
let string_of_substitution s =
  let rec aux s =
     match s with 
     | [] -> ""
     | [(i,t)] -> ((string_of_typeVar i)  ^ " --> " ^ string_of_monoTy t)
     | (i,t)::s' -> (((string_of_typeVar i)  ^ " --> ")^
                     string_of_monoTy t^ "; "^ aux s')
  in ("["^ aux s^ "]\n")

let niceInfer gather_exp gamma exp = 
  let ty = fresh()
  in
  let result = 
    match gather_exp {gamma=gamma;exp=exp;monoTy=ty} with
     None ->
      (raise (Failure ("No type for expression: "^
       string_of_exp exp^ "\n"^
       "in the environment: "^
       string_of_env gamma^ "\n")))
   | Some (p,s) ->
   (string_of_proof p^
   ("Unifying substitution: "^
    string_of_substitution s^
    "Substituting...\n"^
    let new_p = proof_lift_subst s p in
    string_of_proof new_p)) in
  let _ = reset() in
  result;;

(* Not used here*)
(* Collect all the TyVar indices in a proof *)
let rec collectTypeVars ty lst =
  match ty with
    TyVar m -> m::lst
  | TyConst(st, typelst) -> List.fold_left (fun xl x -> collectTypeVars x xl) lst typelst

let rec collectEnvVars gamma lst =
    match gamma with
       []        -> lst
     | (x,(_,y))::xs -> collectEnvVars xs (collectTypeVars y lst)

let collectExpJdgVars {gamma=gamma;exp=exp;monoTy=monoTy} lst = 
  collectEnvVars gamma (collectTypeVars monoTy lst)

let rec collectProofVars prf lst =
  match prf with {conclusion = jdg; antecedents = assum}
   -> collectAssumVars assum (collectExpJdgVars jdg lst)
and collectAssumVars assum lst =
  match assum with 
    []     -> lst
  | p::ps -> collectAssumVars ps (collectProofVars p lst)

(* Rename all the variables in a proof in a canonical way *)

(* Why isn't Int a module *)
let id x = x

let rec nubBy f l =
  match l with
  | [] -> []
  | (x::xs) -> x :: nubBy f (List.filter (fun y -> f x y) xs)

let nubWRT f l = nubBy (fun x y -> (f x) = (f y)) l

let nub l = nubWRT id l

let rmAssocDups l = nubWRT fst l

type renaming = (int * int) list

let validRenaming (r:renaming) : bool
  = (r = (nubWRT fst r)) && (r = (nubWRT snd r))

let renamingToSubst (r:renaming) : substitution 
  = List.map (fun (a,b) -> (a,TyVar b)) r

let renameStr (r:renaming) (s:string) : string
  = try string_of_typeVar (List.assoc s (List.map (fun (a,b) -> string_of_typeVar a,b) r))
    with _ -> s

let rec renameMonoTy (r:renaming) (m:monoTy) : monoTy
  = match m with
  | TyVar i -> (try TyVar (List.assoc i r) with _ -> TyVar i)
  | TyConst (c,a) -> TyConst (c,List.map (renameMonoTy r) a)

let renamePolyTy (r:renaming) (p:polyTy) : polyTy
  = let rec fvsfun x a 
          = match x with
          | TyVar n -> n :: a
          | TyConst (_, l) -> List.fold_right fvsfun l a
    in let fvs = (snd(List.split r) @ fst(List.split r)) in
    let (nbvs, nty) = alpha_conv fvs p in
    (nbvs, renameMonoTy r nty)

let renameEnv (r:renaming) (e:env) : env
  = let e1,e2 = List.split (rmAssocDups e)
  in List.map2 (fun i t -> renameStr r i, renamePolyTy r t) e1 e2

let rec renameProof (r:renaming) {conclusion = {gamma = g; exp = e; monoTy = t};
                                  antecedents = a}
      = {conclusion = {gamma = renameEnv r g; exp = e; monoTy = renameMonoTy r t};
         antecedents = List.map (renameProof r) a}

let canonicalize_proof arg =
  match arg with 
  | None -> None
  | (Some(ty, prf)) -> 
  let (varlst,_) =
    List.fold_right (fun x (xl,idx) -> ((x,idx)::xl), idx+1) 
      (delete_duplicates (collectProofVars prf (collectTypeVars ty []))) 
      ([],0)
  in Some(renameMonoTy varlst ty, renameProof varlst prf)

let canon = function None -> None | Some(ty, p) -> canonicalize_proof (Some(ty,p))

(* MP5's inferencer *)
let make_poly tau = ([], tau)
let rec gather_ty_substitution judgement =
	let {gamma = gamma; exp = exp; monoTy = tau} = judgement in
	match exp with
	| ConExp c -> (
		let tau' = const_signature c in
		match unify [(tau, freshInstance tau')] with
		| Some sigma ->
			Some ({antecedents = []; conclusion = judgement}, sigma)
		| _ -> None
	)
   | VarExp x -> (
		match lookup_env gamma x with
		| Some tau' -> (
			match unify [(tau, freshInstance tau')] with
			| Some sigma ->
				Some ({antecedents = []; conclusion = judgement}, sigma)
			| _ -> None
		)
		| _ -> None
	)
	| BinExp (op, e1, e2) ->
		let (tau1, tau2) = (fresh(), fresh()) in
		let tau' = binop_signature op in (
			match gather_ty_substitution {gamma = gamma; exp = e1; monoTy = tau1} with
			| Some (proof1, sigma1) -> (
				match gather_ty_substitution {gamma = env_lift_subst sigma1 gamma; exp = e2; monoTy = tau2} with
				| Some (proof2, sigma2) -> (
					match unify [(
						monoTy_lift_subst (subst_compose sigma2 sigma1) (mk_fun_ty tau1 (mk_fun_ty tau2 tau)),
						freshInstance tau'
					)] with
					| Some sigma3 -> Some (
							{antecedents = [proof1; proof2]; conclusion = judgement},
							subst_compose sigma3 (subst_compose sigma2 sigma1)
						)
					| _ -> None
				)
				| _ -> None
			)
			| _ -> None
		)
	| MonExp (op, e) ->
		let tau1 = fresh() in
		let tau' = monop_signature op in (
			match gather_ty_substitution {gamma = gamma; exp = e; monoTy = tau1} with
			| Some (proof, sigma) -> (
				match unify [(
					monoTy_lift_subst sigma (mk_fun_ty tau1 tau),
					freshInstance tau'
				)] with
				| Some sigma' -> Some (
						{antecedents = [proof]; conclusion = judgement},
						subst_compose sigma' sigma
					)
				| _ -> None
			)
			| _ -> None
		)
	| IfExp (e1, e2, e3) -> (
		match gather_ty_substitution {gamma = gamma; exp = e1; monoTy = bool_ty} with
		| Some (proof1, sigma1) -> (
			let gamma1 = env_lift_subst sigma1 gamma in
			let tau1 = monoTy_lift_subst sigma1 tau in
			match gather_ty_substitution {gamma = gamma1; exp = e2; monoTy = tau1} with
			| Some (proof2, sigma2) -> (
				let gamma2 = env_lift_subst (subst_compose sigma2 sigma1) gamma in
				let tau2 = monoTy_lift_subst (subst_compose sigma2 sigma1) tau in
				match gather_ty_substitution {gamma = gamma2; exp = e3; monoTy = tau2} with
				| Some (proof3, sigma3) -> Some (
						{antecedents = [proof1; proof2; proof3]; conclusion = judgement},
						subst_compose sigma3 (subst_compose sigma2 sigma1)
					)
				| _ -> None
			)
			| _ -> None
		)
		| _ -> None
	)
	| AppExp (e1, e2) ->
		let (tau1, tau2) = (fresh(), fresh()) in (
			match gather_ty_substitution {gamma = gamma; exp = e1; monoTy = tau1} with
			| Some (proof1, sigma1) -> (
				match gather_ty_substitution {gamma = env_lift_subst sigma1 gamma; exp = e2; monoTy = tau2} with
				| Some (proof2, sigma2) -> (
					match unify [(
						monoTy_lift_subst (subst_compose sigma2 sigma1) tau1,
						monoTy_lift_subst (subst_compose sigma2 sigma1) (mk_fun_ty tau2 tau)
					)] with
					| Some sigma3 -> Some (
							{antecedents = [proof1; proof2]; conclusion = judgement},
							subst_compose sigma3 (subst_compose sigma2 sigma1)
						)
					| _ -> None
				)
				| _ -> None
			)
			| _ -> None
		)
	| FunExp (x, e) ->
		let (tau1, tau2) = (fresh(), fresh()) in (
			match gather_ty_substitution {gamma = ins_env gamma x (make_poly tau1); exp = e; monoTy = tau2} with
			| Some (proof, sigma) -> (
				match unify [(
					monoTy_lift_subst sigma tau,
					monoTy_lift_subst sigma (mk_fun_ty tau1 tau2)
				)] with
				| Some sigma' -> Some (
						{antecedents = [proof]; conclusion = judgement},
						subst_compose sigma' sigma
					)
				| _ -> None
			)
			| _ -> None
		)
	| LetExp (x, e1, e2) ->
		let tau1 = fresh() in (
			match gather_ty_substitution {gamma = gamma; exp = e1; monoTy = tau1} with
			| Some (proof1, sigma1) -> (
				let tau1' = gen
					(env_lift_subst sigma1 gamma)
					(monoTy_lift_subst sigma1 tau1)
				in
				match gather_ty_substitution {gamma = ins_env gamma x tau1'; exp = e2; monoTy = tau} with
				| Some (proof2, sigma2) -> Some (
						{antecedents = [proof1; proof2]; conclusion = judgement},
						subst_compose sigma2 sigma1
					)
				| _ -> None
			)
			| _ -> None
		)
	| RecExp (f, x, e1, e2) ->
		let (tau1, tau2) = (fresh(), fresh()) in (
			let gamma1 =
				let gamma' = ins_env gamma f (make_poly (mk_fun_ty tau1 tau2)) in
				ins_env gamma' x (make_poly tau1) in
			match gather_ty_substitution {gamma = gamma1; exp = e1; monoTy = tau2} with
			| Some (proof1, sigma1) -> (
				let tau1' = gen
					(env_lift_subst sigma1 gamma)
					(monoTy_lift_subst sigma1 (mk_fun_ty tau1 tau2))
				in
				let gamma2 =
					let gamma' = env_lift_subst sigma1 gamma in
					ins_env gamma' f tau1' in
				match gather_ty_substitution {gamma = gamma2; exp = e2; monoTy = tau} with
				| Some (proof2, sigma2) -> 
Some (
						{antecedents = [proof1; proof2]; conclusion = judgement}, subst_compose sigma2 sigma1)
				| _ -> None
			)
			| _ -> None
		)
	| RaiseExp e -> (
		match gather_ty_substitution {gamma = gamma; exp = e; monoTy = int_ty} with
		| Some (proof, sigma) -> Some ({antecedents = [proof]; conclusion = judgement}, sigma)
		| _ -> None
	)
	| TryWithExp (e, match1, more_matches) -> (
		match gather_ty_substitution {gamma = gamma; exp = e; monoTy = tau} with
		| Some (proof0, sigma0) ->
			let rec gather_aux matches (accproof, accsigma) = match matches with
				| [] -> Some (accproof, accsigma)
				| (_, e) :: ms ->
					let gamma' = env_lift_subst accsigma gamma in
					let tau' = monoTy_lift_subst accsigma tau in (
						match gather_ty_substitution {gamma = gamma'; exp = e; monoTy = tau'} with
						| Some (proof, sigma) -> gather_aux ms (accproof @ [proof], subst_compose sigma accsigma)
						| _ -> None
					)
			in (
				match gather_aux (match1 :: more_matches) ([proof0], sigma0) with
				| Some (accproof, accsigma) -> Some ({antecedents = accproof; conclusion = judgement}, accsigma)
				| _ -> None
			)
		| _ -> None
	)

let usefulInfer gamma e = canon (infer gather_ty_substitution gamma e)

(* Infer based on top level decls, I guess building up an env as we go *)
let infer_dec gamma dec =
    match dec with 
    | Anon e ->
      (match usefulInfer gamma e with
      | None -> None
      | Some (ty, pf) -> Some (None, ty, pf))
    | TopLet (s, e) ->
      (match usefulInfer gamma (LetExp(s,e,VarExp s)) with 
      | None -> None 
      | Some (ty, pf) -> Some (Some s, ty, pf))
    | TopRec (s, a, e) ->
      (match usefulInfer gamma (RecExp(s,a,e,VarExp s)) with
      | None -> None 
      | Some (ty, pf) -> Some (Some s, ty, pf))

(*********************************************)
(*                  values                   *)

type memory = (string * value) list
and value =
    Unitval                            | Boolval of bool
  | Intval of int                      | Floatval of float
  | Stringval of string                | Pairval of value * value
  | Listval of value list              | Closure of string * exp * memory
  | Recvar of string * exp * memory    | Exn of int

let make_mem x y = ([(x,y)]:memory)
let rec lookup_mem (gamma:memory) x =
  match gamma with
     []        -> raise (Failure ("identifier "^x^" unbound"))
   | (y,z)::ys -> if x = y then z else lookup_mem ys x
let sum_mem (delta:memory) (gamma:memory) = ((delta@gamma):memory)
let ins_mem (gamma:memory) x y = sum_mem (make_mem x y) gamma

(*value output*)
let rec print_value v =
   match v with
    Unitval           -> print_string "()"
  | Intval n          -> print_int n 
  | Floatval r        -> print_float r
  | Boolval true      -> print_string "true"
  | Boolval false     -> print_string "false"
  | Stringval s       -> print_string ("\"" ^ s ^ "\"")
  | Pairval (v1,v2)   -> print_string "(";
                         print_value v1; print_string ", ";
                         print_value v2;
                         print_string ")";
  | Listval l         -> print_string "[";
                         (let rec pl = function
                              []     -> print_string "]"
                            | v::vl  -> print_value v;
                                        if vl <> []
                                        then
                                           print_string "; ";
                                        pl vl
                              in pl l)
  | Closure (x, e, m) -> print_string ("<some closure>")
  | Recvar (x, e, m)  -> print_string ("<some recvar>")
  | Exn n -> (print_string "(Exn "; print_int n; print_string ")")

let compact_memory m =
  let rec comp m rev_comp_m =
      (match m with [] -> List.rev rev_comp_m
        | (x,y) :: m' ->
           if List.exists (fun (x',_) -> x = x') rev_comp_m
              then comp m' rev_comp_m
           else comp m' ((x,y)::rev_comp_m))
  in comp m []

(*memory output*)
let print_memory m =
    let cm = compact_memory m in
    let rec print_m m = 
    (match m with
        []           -> ()
      | (x, v) :: m' -> print_m m';
                        print_string ("val "^x ^ " = ");
                        print_value v;
                        print_string (";\n") ) in
    print_m cm


(* Pervasive memory *)

(*
let pervasive_memory =
  let bi s = (s, BuiltInOpval s) in
  let perv1 =
   List.map bi
   ["+"; "-"; "*"; "/"; "<"; ">"; "<="; ">="; "mod"; "div";
    "+."; "-."; "*."; "/."; "**"; "::"; "head"; "tail"; "="; "^";
     "fst"; "snd"]
  in
  let and_val = Closure ("p",
   IfThenElse (App (Id "fst", Id "p"), App (Id "snd", Id "p"), Bool false),
   [("fst", BuiltInOpval "fst"); ("snd", BuiltInOpval "snd")]) in
  let or_val = Closure ("p",
   IfThenElse (App (Id "fst", Id "p"), Bool true, App (Id "snd", Id "p")),
   [("fst", BuiltInOpval "fst"); ("snd", BuiltInOpval "snd")]) in
  let not_val = Closure ("b", IfThenElse (Id "b", Bool false, Bool true), [])
  in
("and", and_val) ::
("or", or_val) ::
("not", not_val) ::
("nil", Listval []) ::
perv1;;
*)
