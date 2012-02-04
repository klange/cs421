(* CS421 - Fall 2011
 * MP2 
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open List

(* Problem 1 *)
let rat_eq ((a1,a2),(b1,b2)) =
    a1 * b2 = a2 * b1;;

(* Problem 2 *)
let rec ackermann (m,n) = 
    if m = 0 then n + 1
    else if m > 0 && n = 0 then ackermann(m-1,1)
    else ackermann(m-1,ackermann(m,n-1));;

(* Problem 3 *)
let rec list_ex p xs =
    match xs
    with [] -> false
    | (x :: _xs) -> p x || list_ex p _xs;;

(* Problem 4 *)
let rec elem x xs =
    match xs
    with [] -> false
    | (_x :: _xs) -> x = _x || elem x _xs;;

(* Problem 5 *)
let rec to_float_list xs =
    match xs
    with [] -> []
    | (x :: _xs) -> float_of_int x :: to_float_list _xs;;

(* Problem 6 *)
let rec sum fs =
    match fs
    with [] -> 0.0
    | (x :: xs) -> x +. sum xs;;

(* Problem 7 *)
let rec zip xs ys =
    match (xs, ys)
    with ([],[]) -> []
    | (x :: xs, []) -> []
    | ([], y :: ys) -> []
    | (x :: xs, y :: ys) -> (x,y) :: zip xs ys;;

(* Problem 8 *)
let rec left_side xs =
    match xs
    with [] -> []
    | (x,y) :: xs -> x :: left_side xs;;

let rec right_side xs =
    match xs
    with [] -> []
    | (x,y) :: xs -> y :: right_side xs;;

let split xs =
    (left_side xs, right_side xs);;

let rec fold_right f list y =
    match list
    with [] -> y
    | (x :: xs) -> f x (fold_right f xs y);;

let rec map f l =
    fold_right (fun x y -> f x :: y) l [];;

(* Problem 9 *)
let rec cross xs ys = 
    match xs
    with [] -> []
    | x :: xss -> (
        match ys
        with []-> []
        | y :: yss -> (x,y) :: cross [x] yss
    ) @ cross xss ys;;

(* Problem 10 *)
let rec int_sum fs =
    match fs
    with [] -> 0
    | (x :: xs) -> x + int_sum xs;;

let rec freq_to_prob fs =
    (map (fun m -> (float_of_int m) /. (float_of_int (int_sum fs))) fs);;

(* Problem 11 *)
let rec expected xs =
    match xs
    with [] -> 0.0
    | ((x,y) :: xs) -> (x *. y) +. expected xs;;

(* Problem 12 *)
let rec accept (qs,ss,d,q0,fs) i =
    match i
    with [] -> elem q0 fs
    | ii :: is -> accept (qs,ss,d,d q0 ii,fs) is;;

(* Extra credit *)
let rec valid_dfa (qs,ss,d,q0,fs) =
  raise(Failure "Function not implemented yet.")
