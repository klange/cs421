open Arg
open Grader

(*
 * Main Grader
 *)

let output_file_name = ref None
let netid            = ref None
let which_test       = ref None
let usage_flag       = ref false
let version_flag	 = ref false

let usage_string = Sys.executable_name ^ ": [options]"

let rec command_line_options = ref
	[ 
		"-u", (String(fun x -> ignore(netid := (Some(x))))), 
			"<netid>: output only <netid> followed by the final score" ;

		"-o", (String(fun x -> ignore(output_file_name := (Some(x))))), 
			"<filename>: append all output to a file" ;

		"-t", (Int(fun x -> ignore(which_test := (Some(x))))), 
			"<test number>: run only one specific test" ;

		"-h", (Unit(fun () -> ignore(usage_flag := true))), 
			"usage help" ;
			
		"--version", (Unit(fun () -> ignore(version_flag := true))),
			"print version information" ;
	]

let print_usage () = usage !command_line_options usage_string

let print_versions rubric_title rubric_version = 
	Printf.fprintf stderr "%s" ("grader.cma: version " ^ 
				   				 grader_version ^ "\n") ;
	Printf.fprintf stderr "%s" (rubric_title ^ ": version " ^
							     rubric_version ^ "\n") 

let make_rubric rubric =
	match !which_test with
	|	None  -> rubric
	| Some(n) -> [List.nth rubric (n - 1)]

(*
 * this lets us abstract away the process of getting the graded
 * rubric, which is different depending upon whether we're logging
 * a netid or not
 *)
let grade_rubric r output_file = 
    match !netid with
    | None -> grade_and_print output_file r
	| Some(s) -> grade r

let output_file () =
	match !output_file_name with
	|   None  -> stdout
	| Some(s) -> open_out_gen [Open_append ; Open_creat] 0o644 s

let main rubric extra_rubric rubric_title rubric_version = 
		   parse !command_line_options 
				 (fun _ -> ignore(usage_flag := true)) usage_string;

		   let output_file = output_file () in
		   let rubric      = make_rubric rubric in
		   let extra_rubric = make_rubric extra_rubric in
		   
		   if !usage_flag then
		   	   print_usage ()
		   else if !version_flag then
		   	   print_versions rubric_title rubric_version
		   else
		   	   let graded_rubric = grade_rubric rubric output_file in
			   let (credit, max_credit) = tally_scores graded_rubric in
		   	   let x_graded_rubric = (Printf.fprintf output_file "\n"); grade_rubric extra_rubric output_file in
			   let (x_credit, x_max_credit) = tally_scores x_graded_rubric in
			   
			   match !netid with
			   | None -> Printf.fprintf output_file "Total: [%d / %d]\nExtra: [%d / %d]\n" credit max_credit x_credit x_max_credit
			   | Some(s) -> Printf.fprintf output_file "%s %d\n" s credit
