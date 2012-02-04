type const = Int of int | Bool of bool | Nil

type binop = Add | Sub | Mul | Eq | Less | Cons

type monop = Head | Tail | Print

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
   | RecExp of string * string * exp * exp    (* let rec x = exp1 in exp2 *)
   | OAppExp of exp * exp            (* Extra credit *)

(* Pretty printing *)

let string_of_const c =
    match c 
    with Int n  -> string_of_int n
       | Bool b -> if b then "true" else "false"
       | Nil    -> "[]"


let string_of_binop b =
    match b
    with Add  -> "+"
       | Sub  -> "-"
       | Mul  -> "*"
       | Eq   -> "="
       | Less -> "<"
       | Cons -> "::"

let string_of_monop m =
    match m
    with Head -> "hd"
       | Tail -> "tl"
       | Print -> "print_int"

let rec string_of_exp e =
    match e
    with VarExp x -> x
       | ConExp c -> string_of_const c
       | IfExp(bexp, thenexp, elseexp) ->
          "if "^string_of_exp bexp^
          " then "^string_of_exp thenexp^
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
       | OAppExp(funexp,argexp) ->
         (match funexp
          with VarExp _ -> string_of_exp funexp
             | ConExp _ -> string_of_exp funexp
             | AppExp _ -> string_of_exp funexp
             | MonExp _ -> string_of_exp funexp
             | _ -> "("^string_of_exp funexp^")")
         ^" $ "^
         (match argexp
          with VarExp _ -> string_of_exp argexp
             | ConExp _ -> string_of_exp argexp
             | _ -> "("^string_of_exp argexp^")")

let print_exp exp = print_string ("\n"^(string_of_exp exp)^"\n")

(* Fresh Name stuff *)

let int_to_string n =
    let int_to_int_26_list n =
        let rec aux n l =
            if n <= 0 then l else let c = ((n-1) mod 26) in aux ((n -(c+1))/26) (c::l)
        in aux n []
    in
        let rec aux l = match l with [] -> ""
                            | n::ns -> (String.make 1 (Char.chr (n + 97))) ^ aux ns
        in aux (int_to_int_26_list n)

let freshFor lst = 
    let rec fresh_ n = 
        if List.mem (int_to_string n) lst
           then fresh_ (n+1)
        else int_to_string n
    in fresh_ 1

(* End Fresh name stuff *)

let ackermann = FunExp("m",FunExp("n",
  RecExp("ackermann","m",FunExp("n"
     ,IfExp(BinExp(Eq,VarExp "m",ConExp(Int 0))
        ,BinExp(Add,VarExp "n",ConExp(Int 1))
        ,IfExp(BinExp(Eq,VarExp "n",ConExp(Int 0))
           ,AppExp(AppExp(VarExp "ackermann",BinExp(Sub,VarExp "m",ConExp(Int 1))),ConExp(Int 1))
           ,AppExp(AppExp(VarExp "ackermann",BinExp(Sub,VarExp "m",ConExp(Int 1)))
               ,AppExp(AppExp(VarExp "ackermann",VarExp "m"),BinExp(Sub,VarExp "n",ConExp(Int 1)))))))
     ,AppExp(AppExp(VarExp "ackermann",VarExp "m"),VarExp "n"))))

let ackermanntest (m,n) = AppExp(AppExp(ackermann,ConExp(Int m)),ConExp(Int n))
