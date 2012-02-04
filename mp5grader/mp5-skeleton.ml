open Mp5common

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
(* You add clauses here *)

