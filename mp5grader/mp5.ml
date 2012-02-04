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
	| VarExp x-> (
		let _x = lookup_env gamma x in
		match _x with
		| Some derp -> (
			match unify [(tau, freshInstance derp)] with
			| Some sigma ->
				Some ({antecedents = []; conclusion = judgement}, sigma)
			| _ -> None
		)
		| _ -> None
	)
	| BinExp (op,e1,e2) -> (
		let tau' = binop_signature op in
		let tau1 = fresh() in
		let tau2 = fresh() in
		match gather_ty_substitution {gamma = gamma; exp = e1; monoTy = tau1}
		with
		| Some (proof1, subs1) ->
			match gather_ty_substitution {gamma = env_lift_subst subs1 gamma; exp = e2; monoTy =
				tau2} with
				| Some (proof2, subs2) ->
					let subs = subst_compose subs2 subs1 in
					match unify [(monoTy_lift_subst subs (mk_fun_ty tau1
					(mk_fun_ty tau2 tau))), freshInstance tau'] with
					| Some sigma ->
							Some ({antecedents = [proof1;proof2]; conclusion =
								judgement},
								subst_compose sigma subs)
					| _ -> None
				| _ -> None
		| _ -> None
	)
	| MonExp (op,e1) -> (
		let tau' = monop_signature op in
		let tau1 = fresh () in
		match gather_ty_substitution {gamma = gamma; exp = e1; monoTy = tau1}
		with
		| Some (poof,subs) ->
				match unify [(monoTy_lift_subst subs (mk_fun_ty tau1
				tau),freshInstance tau')] with
				| Some sigma ->
						Some ({antecedents = [poof]; conclusion = judgement},
						subst_compose sigma subs)
				| _ -> None
		| _ -> None
	)
	| IfExp (e1,e2,e3) -> (
		match gather_ty_substitution {gamma = gamma; exp = e1; monoTy = bool_ty}
		with
		| Some (p1, s1) ->
			match gather_ty_substitution {gamma = env_lift_subst s1 gamma;
			exp = e2; monoTy = monoTy_lift_subst s1 tau} with
			| Some (p2, s2) ->
				match gather_ty_substitution {gamma = env_lift_subst
				(subst_compose s2 s1) gamma; exp = e3; monoTy = monoTy_lift_subst
				(subst_compose s2 s1) tau} with
				| Some (p3, s3) ->
					Some ({antecedents = [p1;p2;p3]; conclusion = judgement},
						subst_compose s1 (subst_compose s2 s3))
				| _ -> None
			| _ -> None
		| _ -> None
	)
	| FunExp (x, e) -> (
		let tau1 = fresh() in
		let tau2 = fresh () in
		match gather_ty_substitution {gamma = ins_env gamma x ([],tau1); exp =
			e; monoTy = tau2}
		with
		| Some (p1, s1) ->
			match unify[(monoTy_lift_subst s1 tau,monoTy_lift_subst s1
			(mk_fun_ty tau1 tau2))] with
			| Some sigma ->
					Some ({antecedents = [p1]; conclusion = judgement},
					subst_compose sigma s1)
			| _ -> None
		| _ -> None
	)
	| AppExp (e1, e2) -> (
		let tau1 = fresh () in
		match gather_ty_substitution {gamma = gamma; exp = e1; monoTy =
			mk_fun_ty tau1 tau}
		with
		| Some (p1, s1) -> 
			match gather_ty_substitution {gamma = env_lift_subst s1 gamma; exp =
				e2; monoTy = monoTy_lift_subst s1 tau1} with
			| Some (p2, s2) ->
					Some ({antecedents = [p1;p2]; conclusion = judgement},
				subst_compose s2 s1)
			| _ -> None
		| _ -> None
	)
	| LetExp (x, e1, e2) -> (
		let tau1 = fresh () in
		match gather_ty_substitution {gamma = gamma; exp = e1; monoTy = tau1}
		with
		| Some (p1, s1) ->
			match gather_ty_substitution {gamma = ins_env (env_lift_subst s1
			gamma) x (gen (env_lift_subst s1 gamma) (monoTy_lift_subst s1 tau1));
			exp = e2; monoTy = monoTy_lift_subst s1 tau} with
			| Some (p2, s2) ->
				Some ({antecedents = [p1;p2]; conclusion = judgement},
					subst_compose s2 s1)
			| _ -> None
		| _ -> None
	)
	| RecExp (f,x, e1, e2) -> (
		let tau1 = fresh () in
		let tau2 = fresh () in
		match gather_ty_substitution {gamma = ins_env (ins_env gamma x ([], tau1)) f
		([], (mk_fun_ty tau1 tau2)); exp = e1; monoTy = tau2} with
		| Some (p1,s1) ->
			match gather_ty_substitution {gamma = ins_env (env_lift_subst s1
			gamma) f (gen (env_lift_subst s1 gamma) (monoTy_lift_subst s1
			(mk_fun_ty tau1 tau2))); exp = e2; monoTy = monoTy_lift_subst s1
			tau} with
			| Some (p2, s2) ->
				Some ({antecedents = [p1;p2]; conclusion = judgement},
				subst_compose s2 s1)
			| _ -> None
		| _ -> None
	)
	| RaiseExp e -> (
		match gather_ty_substitution {gamma = gamma; exp = e; monoTy = int_ty}
		with
		| Some (p1, s1) ->
			Some ({antecedents = [p1]; conclusion = judgement}, s1)
		| _ -> None
	)
(* You add clauses here *)

