(* $Id: test.ml,v 1.1 2003/09/08 18:56:58 kircher Exp $ *)

(*
 * this file defines some commonly used testing functions.  these
 * functions can be used to define a rubric (see grade.ml for the 
 * definition of the type 'rubric').
 *
 * It contains the following testing functions:
 *
 * compare
 * --------
 *    compare runs a student function and a solution function and
 *    tests for equality of the objects returned.  if they are equal,
 *    full credit is assigned; otherwise, if they are unequal or if the
 *    student solution fails to return within a timeout, no credit is
 *    awarded.
 *)

open Grader
open Util

let check timeout weight grade_fn student : test =
  (fun () ->
    try
      match timer timeout student with
      | None -> ((0, weight), "student solution timed out / exception thrown")
      | Some(x) -> 
	  if (grade_fn x) then 
	    ((weight, weight), "correct") 
	  else
	    ((0, weight), "student solution returns an incorrect value")
    with _ -> ((0, weight), "bug in grading program: please report to staff"))

let compare compare_function timeout weight (solution, student) : test = 
  check timeout weight (fun student_val -> compare_function (solution ()) student_val) student

(*
let compare compare_function timeout weight (solution, student) : test = 
	(fun () ->
		try
			match timer timeout student with
			|	 None -> ((0, weight), "student solution timed out / exception thrown")
			| Some(x) -> if (compare_function (solution()) x) then
							((weight, weight), "correct")
						 else
							((0, weight), "student solution returns an incorrect value");
		with _ -> ((0, weight), "bug in grading program: please report to staff"))
*)

let simple_compare t w x = compare (=) t w x

(* FIXME-- rewrite in terms of check *)
let diff timeout max_output solution student args weight input_string : test =
	(fun () ->
			(* run solution code *)
			let solution_status = execute_child None max_output 
												solution args input_string in
			(* run student code *)
			let student_status  = execute_child (Some(timeout)) max_output 
												student args input_string in
			
			if (not student_status.halted) then
				((0, weight), "solution did not halt in time")			
			else if (solution_status.child_output = student_status.child_output) then
				((weight, weight), "correct")
			else
				((0, weight), "solution output is not correct")) 
				
				(* "(student: [" ^ 
							  student_status.child_output ^
							  "]) (solution: [" ^ 
							  solution_status.child_output ^ 
							  "])"  )) *)

(* 
 *the functions below make solution, student pairs for use in the test_timed
 *functions above
 *)

(* FIXME:  come up with better names *)

let ss_pair0 sol stu = (fun () -> sol), (fun () -> stu)
let ss_pair1 sol stu a1 = (fun () -> sol a1), (fun () -> stu a1)
let ss_pair2 sol stu a1 a2 = (fun () -> sol a1 a2), (fun () -> stu a1 a2)
let ss_pair3 sol stu a1 a2 a3 = (fun () -> sol a1 a2 a3), (fun () -> stu a1 a2 a3)
let ss_pair4 sol stu a1 a2 a3 a4 = (fun () -> sol a1 a2 a3 a4), 
								   (fun () -> stu a1 a2 a3 a4)
let ss_pair5 sol stu a1 a2 a3 a4 a5 = (fun () -> sol a1 a2 a3 a4 a5), 
									  (fun () -> stu a1 a2 a3 a4 a5)

