(* CS421 - Fall 2011
 * MP1 
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Mp1common

(* Problem 1 *)
let x = 32.7;;  (* You will want to change this. *)

(* Problem 2 *)
let salute = "Greetings, my friend!";;

(* Problem 3 *)
let times_13 n = 
    n * 13;;

(* Problem 4 *)
let square_plus_x y =
    x +. (y *. y);;

(* Problem 5 *)
let hail name =
    if name = "Elsa" then
        print_string "Wayell, hah theya, Ayelsa!"
    else
        begin
            print_string "Dear, ";
            print_string name;
            print_string ". I wish you the best in CS421.\n";
        end
;;


(* Problem 6 *)
let has_smallest_abs m n =
    if abs(m) < abs(n) then m
    else n
;;

(* Problem 7 *)
let swap_pair (x,y) = (y,x);;

(*Problem 8 *)
let pair_app (f,g) x = (f x, g x);;
