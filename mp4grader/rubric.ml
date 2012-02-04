(*
 * grader for mp4
 * This file will be preprocessed to generate the actual OCaml file.
 *)
let rubric_version = "1.0"
let rubric_title = "CS421 Fall 2011 MP3"

open Grader
open Test
(* open Mp1common *)

(*
 * use a timeout of 4 seconds
 *)

let mptest weight pair = compare (=) 4 weight pair

(**************************************************************************
 * You can add new test cases by adding new elements to the following lists
 * Format is:
 * TEST<X>ARG(<weight>, <function_name>, <arg1>, <arg2>, ..., <argX>)
 *
 * <X> is the number of argument that the function being tested takes.
 **************************************************************************)

open Mp4common
open Mp4eval

let elem_sol e es = eval (AppExp(AppExp(Solution.elem,ConExp(Int e)),Solution.import_list es))
let elem_stu e es = eval (AppExp(AppExp(Student.elem,ConExp(Int e)),Solution.import_list es))

let make_canon exp =
let rec make_canon_aux (env, next_num) exp =
    match exp
    with VarExp x -> VarExp (List.assoc x env)
       | ConExp c -> exp
       | IfExp (b,t,e) ->
         IfExp(make_canon_aux (env, next_num) b,
               make_canon_aux (env, next_num) t,
               make_canon_aux (env, next_num) e)
       | AppExp(e1,e2) ->
         AppExp(make_canon_aux (env, next_num) e1,
                make_canon_aux (env, next_num) e2)
       | BinExp(binop,e1,e2) ->
         BinExp(binop,make_canon_aux (env, next_num) e1,
                make_canon_aux (env, next_num) e2)
       | MonExp (monop,e) -> MonExp(monop,make_canon_aux (env, next_num) e)
       | FunExp(x,e) ->
         let newx = int_to_string next_num in
         FunExp(newx, make_canon_aux ((x,newx)::env, next_num+1) e)
       | LetExp(x,e1,e2) ->
         let newx = int_to_string next_num in
         LetExp(newx, make_canon_aux (env, next_num) e1,
                      make_canon_aux ((x,newx)::env, next_num + 1) e2)
       | RecExp(f, x, e1,e2) ->
         let newf = int_to_string next_num in
         let fenv = (f,newf)::env in
         let newx = int_to_string (next_num + 1) in
         RecExp(newf, newx, make_canon_aux ((x,newx)::fenv, next_num + 2) e1,
                make_canon_aux (fenv, next_num + 1) e2)
       | OAppExp(e1,e2) ->
         OAppExp(make_canon_aux (env, next_num) e1,
                 make_canon_aux (env, next_num) e2)
in
let start_info =
    List.fold_left
    (fun (env,next_num) name ->
         if List.mem_assoc name env then (env,next_num)
         else ((name,Mp4common.int_to_string next_num)::env,next_num + 1))
    ([],1)
    (Solution.freeVars exp)
in make_canon_aux start_info exp

let freeVars_canon lst =
  let rec nub lst =
    match lst with
    | [] -> []
    | [x] -> [x]
    | x::y::tl -> if x=y then nub (y::tl)
                         else x::(nub (y :: tl))
  in nub (List.sort String.compare lst)

let freeVars_sol e = freeVars_canon (Solution.freeVars e)
let freeVars_stu e = freeVars_canon (Student.freeVars e)

let freeVars_var_ex = VarExp "x"
let freeVars_if_ex = IfExp (ConExp (Bool true), VarExp "x", VarExp "y")
let freeVars_fun_ex = FunExp ("x",VarExp "x")
let freeVars_let_ex = LetExp ("x",VarExp "y",VarExp "x")
let freeVars_rec_ex = RecExp ("f","x",AppExp(VarExp "f",VarExp "x"),
                              (AppExp (VarExp "f",VarExp "y")))

let id = FunExp("i",VarExp "i")

let cps_stu e = make_canon (Student.cps e id)
let cps_sol e = make_canon (Solution.cps e id)

let cps_eval_stu e = eval (Student.cps e id)
let cps_eval_sol e = eval (Solution.cps e id)

let cps_var_ex = VarExp "x"
let cps_con_ex = ConExp (Int 0)
let cps_if_ex = IfExp (VarExp "b", ConExp (Int 2), ConExp (Int 5))
let cps_app_ex = AppExp (VarExp "f", VarExp "x")
let cps_bin_ex = BinExp (Add, ConExp(Int 5), ConExp(Int 1))
let cps_mon_ex = MonExp (Head, ConExp Nil)
let cps_fun_ex = FunExp ("x", VarExp "x")
let cps_let_ex = LetExp ("x", ConExp(Int 2), VarExp "x")
let cps_rec_ex = RecExp ("f", "x", VarExp "x", ConExp(Int 4))
let cps_oapp_ex = OAppExp (VarExp "f", VarExp "x")

(* This list is for regular problems *)
let rubric =
[
  "import_list"^" "^"[1;2]", mptest 1 (ss_pair1 Solution.import_list Student.import_list ([1;2]));
  "elem_sol"^" "^"3"^" "^"[1;2;3;4]", mptest 1 (ss_pair2 elem_sol elem_stu (3) ([1;2;3;4]));
  "num_of_consts"^" "^"IfExp (BinExp (Eq,VarExp \"l\", ConExp Nil), ConExp (Int 0), VarExp \"x\")", mptest 1 (ss_pair1 Solution.num_of_consts Student.num_of_consts (IfExp (BinExp (Eq,VarExp "l", ConExp Nil), ConExp (Int 0), VarExp "x")));
  "freeVars_sol"^" "^"freeVars_var_ex", mptest 1 (ss_pair1 freeVars_sol freeVars_stu (freeVars_var_ex));
  "freeVars_sol"^" "^"freeVars_if_ex", mptest 1 (ss_pair1 freeVars_sol freeVars_stu (freeVars_if_ex));
  "freeVars_sol"^" "^"freeVars_fun_ex", mptest 1 (ss_pair1 freeVars_sol freeVars_stu (freeVars_fun_ex));
  "freeVars_sol"^" "^"freeVars_let_ex", mptest 1 (ss_pair1 freeVars_sol freeVars_stu (freeVars_let_ex));
  "freeVars_sol"^" "^"freeVars_rec_ex", mptest 1 (ss_pair1 freeVars_sol freeVars_stu (freeVars_rec_ex));
  "freeVars_sol"^" "^"(LetExp(\"x\",VarExp \"y\",FunExp(\"s\",AppExp(AppExp(VarExp \"a\",VarExp \"x\"),VarExp \"s\"))))", mptest 1 (ss_pair1 freeVars_sol freeVars_stu ((LetExp("x",VarExp "y",FunExp("s",AppExp(AppExp(VarExp "a",VarExp "x"),VarExp "s"))))));
  "cps_sol"^" "^"cps_var_ex", mptest 0 (ss_pair1 cps_sol cps_stu (cps_var_ex));
  "cps_sol"^" "^"cps_con_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_con_ex));
  "cps_sol"^" "^"cps_if_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_if_ex));
  "cps_sol"^" "^"cps_app_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_app_ex));
  "cps_sol"^" "^"cps_bin_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_bin_ex));
  "cps_sol"^" "^"cps_mon_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_mon_ex));
  "cps_sol"^" "^"cps_fun_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_fun_ex));
  "cps_sol"^" "^"cps_let_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_let_ex));
  "cps_sol"^" "^"cps_rec_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_rec_ex));
  "cps_sol"^" "^"ackermanntest (3,4)", mptest 1 (ss_pair1 cps_sol cps_stu (ackermanntest (3,4)))
]
(* Note: the last entry should not be followed by a semicolon. *)

(* This list is for extra credit problems *)
let extra_rubric = [
  "cps_sol"^" "^"cps_oapp_ex", mptest 1 (ss_pair1 cps_sol cps_stu (cps_oapp_ex))
]

let _ = Main.main rubric extra_rubric rubric_title rubric_version
