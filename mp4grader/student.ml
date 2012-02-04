open Mp4common

let rec import_list lst = 
    match lst with
    [] -> ConExp Nil
    | x :: xs -> BinExp(Cons, ConExp(Int x), import_list xs)
;;

(*
let rec elem e = fun xs -> if xs = [] then false
                           else if hd xs = e then true
                           else elem e (tl xs)
 *)
let elem = RecExp("elem", "e",
	FunExp("xs",
		(* if xs = [] *)
		IfExp(BinExp(Eq, VarExp "xs", ConExp Nil),
		(* then *)
			(* false *)
			ConExp(Bool false),
		(* else *)
			(*if hd(xs) = e *)
			IfExp(BinExp(Eq, MonExp(Head, VarExp "xs"), VarExp "e"),
			(* then *)
				(* true *)
				ConExp(Bool true),
			(* else *)
				(* ((elem (lt xs)) xs) *)
				AppExp(AppExp(VarExp "elem", VarExp "e"), MonExp(Tail, VarExp "xs"))
			)
		)
	), VarExp "elem");;

let rec num_of_consts expression =
	match expression with
	VarExp(a) -> 0
	| ConExp(a) -> 1
	| IfExp(a,b,c)         -> (num_of_consts a) + (num_of_consts b) + (num_of_consts c)
	| AppExp(a,b)          -> (num_of_consts a) + (num_of_consts b)
	| BinExp(bin,a,b)      -> (num_of_consts a) + (num_of_consts b)
	| MonExp(mon,a)        -> (num_of_consts a)
	| FunExp(str,a)        -> (num_of_consts a)
	| LetExp(str,a,b)      -> (num_of_consts a) + (num_of_consts b)
	| RecExp(str,strb,a,b) -> (num_of_consts a) + (num_of_consts b)
	| OAppExp(a,b)         -> (num_of_consts a) + (num_of_consts b)
;;

let rec freeVars expression = 
	match expression with
	VarExp(a) -> a :: []
	| ConExp(a) -> []
	| AppExp(a,b) -> (freeVars a) @ (freeVars b)
	| BinExp(typ,a,b) -> (freeVars a) @ (freeVars b)
	| IfExp(a,b,c) -> (freeVars a) @ (freeVars b) @ (freeVars c)
	| MonExp(mon,a) -> (freeVars a)
	| FunExp(str,a) -> (List.filter (fun x -> if x = str then false else true) (freeVars a))
	| LetExp(str,a,b) -> (freeVars a) @ (List.filter (fun x -> if x = str then
		false else true) (freeVars b))
	| RecExp(str,strb,a,b) -> (List.filter (fun x -> if x = str then false else
		true) (List.filter (fun y -> if y = strb then false else true)
		(freeVars(a)))) @ (List.filter (fun x -> if x = str then false else true)
		(freeVars(b)))
;;

let rec cps expression cont = 
	match expression with
	VarExp(var) -> AppExp(cont, expression)
	| ConExp(con) -> AppExp(cont, expression)
	| IfExp(guard, _if, _then) ->
		let x = (freshFor ( (freeVars cont) @ (freeVars guard) @ (freeVars _if) @ (freeVars _then) )) in
			(cps
				guard
				(FunExp(
					x,
					IfExp(
						VarExp x,
						cps _if cont,
						cps _then cont
					)
				))
			)
	| AppExp(func, attr) ->
		let a = (freshFor ((freeVars cont) @ (freeVars attr) @ (freeVars func) )) in
			let b = (freshFor (a :: ((freeVars cont) @ (freeVars attr) @ (freeVars func) ))) in
				(cps
					func
					(FunExp(
						a,
						(cps
							attr
							(FunExp(
								b,
								AppExp(
									AppExp(
										VarExp a,
										VarExp b
									),
									cont
								)
							))
						)
					))
				)
	| BinExp(binOp, _first, _second) ->
		let a = (freshFor ((freeVars cont) @ (freeVars _first)@(freeVars _second))) in
			let b = (freshFor (a :: ((freeVars cont) @ (freeVars _first)@(freeVars _second)))) in
				(cps
					_first
					(FunExp(
						a,
						(cps
							_second
							(FunExp(
								b,
								AppExp(
									cont,
									BinExp(
										binOp,
										VarExp a,
										VarExp b
									)
								)
							))
						)
					))
				)
	| MonExp(monOp, _exp) ->
		let a = (freshFor (freeVars cont)) in
			(cps
				_exp
				(FunExp(
					a,
					AppExp(
						cont,
						MonExp(
							monOp,
							VarExp a
						)
					)
				))
			)
	| FunExp(arg, _exp) ->
		let a = (freshFor (freeVars _exp)) in
			AppExp(
				cont,
				FunExp(
					arg,
					FunExp(
						a,
						cps _exp (VarExp a)
					)
				)
			)
	| LetExp(arg, _exp, _in) ->
			AppExp(
				FunExp(
					arg,
					cps _in cont
				),
				_exp
			)
	| RecExp(name,arg,_exp,_in) ->
		let a = (freshFor (arg :: (name :: (freeVars _exp)))) in
			RecExp(
				name,
				arg,
				FunExp(
					a,
					cps _exp (VarExp a)
				),
				cps _in cont
			)
;;
