(* $Id: grader.ml,v 1.1 2003/09/08 18:56:58 kircher Exp $ *)

(*
 * This file defines a general grading routine 'grade';
 * grade takes as input a rubric and produces as output
 * a graded rubric.
 *)

open List

let grader_version = "0.1"

(*
 * a score is a tuple:  (credit, max_credit)
 *)
type score = int * int

(*
 * a grading comment is a string
 *)
type grading_comment = string

(*
 * a grade is a score and a grading_comment
 *)
type grade = score * grading_comment

(*
 * a test is a function which which returns a grade
 *)
type test = unit -> grade

(*
 * a rubric item is a description of the item and a test
 *)
type rubric_item = string * test

(*
 * a rubric is a list of 'rubric_item's
 *)
type rubric = rubric_item list

(*
 * a graded rubric item is a rubric item complete with a grade
 *)
type graded_rubric_item = rubric_item * grade

(*
 * a graded rubric is a list of 'graded_rubric_items'
 *)
type graded_rubric = graded_rubric_item list

let add_scores ((a, b) : score) ((c, d) : score) : score = (a + c, b + d)

(* 
 * grades an assignment according to the rubric 'r' and returns the score
 * 
 * prints out scores to the screen 
 *)
let grade_and_print outfile (r : rubric) : graded_rubric =
	map (fun (description, test) -> 
			match test () with
			| ((credit, max_credit), comment) -> 
				Printf.fprintf outfile "[%d / %d] %s (%s)\n" 
					  credit max_credit description comment;
				flush stdout;
				((description, test), ((credit, max_credit), comment))
		 ) r

let grade (r : rubric) : graded_rubric =
	map (fun (description, test) -> ((description, test), test())) r

let tally_scores (r : graded_rubric) : score =
	fold_right (fun (_, (score, _)) partial_score -> add_scores partial_score score)
				r (0, 0)
