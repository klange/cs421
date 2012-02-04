(* CS 421 Fall 2011 MP3 *)

open Mp3common

(* Forward Recursion *)

(* Problem 1 *)
let rec replace_if p v lst =
	match lst
	with [] -> []
	| (x :: xs) -> 
		if p x then v :: replace_if p v xs
		else x :: replace_if p v xs;;

(* Problem 2 *)
let head_of lst =
	match lst with
	[] -> 0.0
	| (x :: xs) -> x;;

let rec partial_sums lst =
	match lst with
	[] -> []
	| (x :: xs) ->
		(head_of (partial_sums xs) +. x) :: (partial_sums xs);;

(* Problem 3 *)
let rec not_same_next lst =
	let rec not_same_next_prev prev lst =
		match lst with
		[] -> []
		| (x :: xs) -> if (x = prev) then ([] @ (not_same_next_prev x xs))
		else (x :: (not_same_next_prev x xs)) in
	match lst with
	[] -> []
	| (x :: xs) -> x :: (not_same_next_prev x xs);;

(* Tail Recursion *)

(* Problem 4 *)
let rec exists_pred_tail b p lst =
	match lst with
	[] -> b
	| (x :: xs) ->
		if (p x) then (exists_pred_tail true p xs)
		else (exists_pred_tail b p xs);;

let exists_pred p lst =
	exists_pred_tail false p lst;;

(* Problem 5 *)
let rec count_if_tail n p lst =
	match lst with
	[] -> n
	| (x :: xs) ->
		if (p x) then (count_if_tail (n + 1) p xs)
		else (count_if_tail n p xs);;

let count_if p lst =
	count_if_tail 0 p lst;;

(* Problem 6 *)
let rec min_element_tail n i ii lst =
	match lst with
	[] -> i
	| (x :: xs) ->
		if (x < n) then (min_element_tail x (ii+1) (ii+1) xs)
		else (min_element_tail n i (ii+1) xs);;

let min_element lst =
	match lst with
	[] -> (-1)
	| (x :: xs) ->
		min_element_tail x 0 0 xs;;

(* Higher Order Functions *)

(* Problem 7 *)
let replace_if_base = [] (* This may need changing. *)
let replace_if_rec p v x r =
	if (p x) then v :: r else x :: r;;

(* Problem 8 *)
let not_same_next_base = [] (* This may need changing. *)
let not_same_next_rec x r = 
	match r with [] -> x :: []
	| (_r :: rs) -> if (x = _r) then x :: rs
	else x :: r;;

(* Problem 9 *)
let exists_pred_base = false (* This may need changing. *)
let exists_pred_rec p r x =
	if (p x) then true else r;;

(* Problem 10 *)
let count_if_base = 0 (* This may need changing. *)
let count_if_rec p r x =
	if (p x) then r + 1 else r;;

(* Problem 11 *)
let cross_prod xs ys =
	List.map (fun n -> List.map (fun m -> (n,m)) ys) xs;;

(* Continuation Passing *)

(* Problem 12 *)
let subk n m k =
	k (n - m);;
let addk n m k =
	k (n + m);;
let consk h t k =
	k (h :: t);;
let timesk a b k =
	k (a * b);;
let plusk x y k =
	k (x +. y);;
let multk x y k =
	k (x *. y);;
let lessk n m k =
	k (n < m);;
let eqk x y k =
	k (x = y);;

(* Problem 13 *)
let abcdk a b c d k =
	plusk c d (fun cd -> plusk a b (fun ab -> multk ab cd k));;

(* Problem 14 *)
let head_ofk lst k =
	match lst with
	[] -> k 0.0
	| (x :: xs) -> k x;;

let rec partial_sumsk lst k =
	match lst with
	[] -> k []
	| (x :: xs) ->
		(partial_sumsk xs (fun y -> head_ofk y (fun z -> plusk x z (fun a -> consk a y k))));;

(* Problem 15 *)
let rec count_ifk_tail n pk lst k =
	match lst with
	[] -> k n
	| (x :: xs) -> pk x (fun d -> 
		if (d) then addk n 1 (fun y -> count_ifk_tail y pk xs k)
		else (count_ifk_tail n pk xs k));;

let count_ifk pk lst k =
	count_ifk_tail 0 pk lst k;;

(* Problem 16 *)
let rec min_elementk_tail n i ii lst k =
	match lst with
	[] -> (k i)
	| (x :: xs) ->
		lessk x n (fun d -> addk ii 1 (fun z ->
			if d then (min_elementk_tail x z z xs k) else
				(min_elementk_tail n i z xs k)));;

let min_elementk lst k =
	match lst with
	[] -> (k (-1))
	| (x :: xs) ->
	min_elementk_tail x 0 0 xs k;;
