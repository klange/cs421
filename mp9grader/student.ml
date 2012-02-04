open Mp9common;;

let const_to_val c = 
    match c with
    | Int i -> (Intval i)
    | Bool b -> (Boolval b)
    | Float f -> (Floatval f)
    | String s -> (Stringval s)
    | Unit -> (Unitval)
    | Nil  -> (Listval [])
;; (* failwith "const_to_val is not implemented yet." *)

let monApply unop v = 
    match unop with
    | Head -> (match v with Listval l -> List.hd l)
    | Tail -> (match v with Listval l -> Listval (List.tl l))
    | Fst  -> (match v with Pairval (a, b) -> a)
    | Snd  -> (match v with Pairval (a, b) -> b)
    | Neg  -> (match v with
        | Intval i -> (Intval (0 - i))
        | Floatval f -> (Floatval (0.0 -. f))
    )
    | Print -> (match v with Intval i -> (print_int i; (Unitval)))
;;

let binApply binop (v1,v2) = 
    match binop with
    | Add -> (match v1 with Intval i1 -> match v2 with Intval i2 -> Intval (i1 + i2))
    | Sub -> (match v1 with Intval i1 -> match v2 with Intval i2 -> Intval (i1 - i2))
    | Mul -> (match v1 with Intval i1 -> match v2 with Intval i2 -> Intval (i1 * i2))
    | Div -> (match v1 with Intval i1 -> match v2 with Intval i2 -> Intval (i1 / i2))
    | Exp -> (match v1 with Floatval f1 -> match v2 with Floatval f2 -> Floatval (f1 ** f2))
    | FAdd -> (match v1 with Floatval f1 -> match v2 with Floatval f2 ->
            Floatval (f1 +. f2))
    | FSub -> (match v1 with Floatval f1 -> match v2 with Floatval f2 ->
            Floatval (f1 -. f2))
    | FMul -> (match v1 with Floatval f1 -> match v2 with Floatval f2 ->
            Floatval (f1 *. f2))
    | FDiv -> (match v1 with Floatval f1 -> match v2 with Floatval f2 ->
            Floatval (f1 /. f2))
    | Concat -> (match v1 with Stringval s1 -> match v2 with Stringval s2 ->
            Stringval (s1 ^ s2))
    | Cons -> (match v2 with Listval l2 ->
            Listval (v1 :: l2))
    | Comma -> Pairval (v1, v2)
    | Eq -> Boolval (v1 = v2)
    | Less -> Boolval (v1 < v2)
    
;;

let rec eval_exp (exp, m) =
    match exp with
    | ConExp c -> (const_to_val c)
    | VarExp x -> (let value = (lookup_mem m x) in match value with
        | Recvar (y, e, m') -> Closure(y, e, (ins_mem m' x value))
        | _ -> value
    )
    | MonExp (mon, e) -> (monApply mon (eval_exp (e,m)))
    | BinExp (binOp, e1, e2) -> (binApply binOp ((eval_exp (e1, m)), (eval_exp (e2,m))))
    | LetExp (str, e1, e2) -> (let x = (eval_exp (e1, m)) in (let m' = (ins_mem m str x) in (eval_exp (e2, m'))))
    | FunExp (str, e) -> (Closure (str, e, m))
    | AppExp (f, e) -> (let c = eval_exp(f, m) in (
        match c with Closure (str, e1, m1) -> 
            eval_exp(e1, (ins_mem m1 str (eval_exp (e,m))))
    ))
    | IfExp (e1, e2, e3) -> let b = eval_exp(e1, m) in (
        (* match b with Boolval boo -> ( *)
            if b = Boolval true then eval_exp(e2, m) else eval_exp(e3, m)
        (* ) *)
    )
    | RecExp (s1, s2, e1, e2) -> eval_exp(e2, (ins_mem m s1 (Recvar (s2, e1, m))))
;;

let eval_dec (dec, m) = 
    match dec with
    | Anon e -> let x = eval_exp(e,m) in ((None, x), m)
    | TopLet (s, e) -> let x = eval_exp(e,m) in ((Some s, x), (ins_mem m s x))
    | TopRec (s1, s2, e) -> let rvar =Recvar(s2, e, m) in ((Some s1, rvar),
    (ins_mem m s1 rvar))
;;
