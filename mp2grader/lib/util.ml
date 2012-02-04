(* $Id: util.ml,v 1.1 2003/09/08 18:56:59 kircher Exp $ *)

(*
 * this file defines some useful utilities
 *)

open Unix

(* 
 * timer runs a function with type () -> 'a for a period of 'timeout'
 * seconds.  if the function returns a value within the time period,
 * timer returns Some(value); otherwise it returns None
 *
 * ASSUMES:  -> there is no pending alarm
 *
 *)
let timer timeout f =
	let old_handler = Sys.signal Sys.sigalrm 
						(Sys.Signal_handle(fun _ -> failwith "")) in
	let result = 
		try
			ignore (handle_unix_error alarm timeout);
			Some(f())
		with _ -> None
	in
		ignore (handle_unix_error alarm 0);   (* cancels pending alarm, if any *)
		Sys.set_signal Sys.sigalrm old_handler;
		result

(*
 * closes all files, except stdin,stdout,stderr.  needed for 'execute_child'.
 *)
(*
external close_all_files : unit -> unit = "close_all_files"
*)
let close_all_files () = ()  (* Would not compile with the above *)

(*
 * execute_child runs 'child_program' (assumed to be either an
 * absolute path or a path that is relative to the current directory)
 * with the args 'args' on the input 'input_string' for 'timeout' seconds.  
 *
 * up to 'max_output' bytes of output are recorded.
 *
 * currently, timeout has type int option (i.e. timeout is optional) but
 * max_output is not.
 *
 * returns a child_status record
 *
 * ASSUMES:   -> there is no pending alarm
 *            
 * TODO -  
 *   (.) make it possible to give no limits for max_output
 *)

type child_status =
	{ 
		halted			: bool ;   (* true iff the child halted on its own *)
		run_time  		: float ;  (* runtime of child in seconds *)  (* for now, set to 0.0 *)
		child_output 	: string ; (* output of the child *)
		return_status   : int	   (* unix child return status *)
	}

let execute_child timeout max_output child_program args input_string : child_status =
	let buffer_chunk = 4096 in
	let (child_to_parent_read, child_to_parent_write) = pipe () in
	let (parent_to_child_read, parent_to_child_write) = pipe () in
	let child_pid = ref 0 in
	let halted = ref true in
	let old_timer_handler = Sys.signal Sys.sigalrm
							   (Sys.Signal_handle (fun _ -> halted := false ; 
							   								kill (!child_pid) 9)) in
	let result =
	
	access child_program [X_OK] ;
	
	child_pid := fork () ;
	
	if ((!child_pid) = 0) then
		begin
		(* child *)

		handle_unix_error close parent_to_child_write ;
		handle_unix_error close child_to_parent_read  ;
		
		handle_unix_error dup2 parent_to_child_read stdin    ;
		handle_unix_error dup2 child_to_parent_write stdout  ;

		(* close any other open files *)
		close_all_files () ;
		
		(*
		 * note that we could drop some priviledges via a setuid() here 
		 *
		 * this is one advantage of not using an ocaml open_process call
		 *)

		(* Printf.fprintf Pervasives.stderr "A hello\n" ; flush Pervasives.stderr ; *)
		
		handle_unix_error execv child_program args ;
		
		(* Printf.fprintf Pervasives.stderr "hello\n" ; flush Pervasives.stderr ; *)
		
		(* not reached *)
		{ halted = false ; run_time = 0.0 ; child_output = "" ; return_status = (-1) }
		end
	else
		(* grader *)
		begin

		(* start timer *)
		(match timeout with
		 | None -> ()
		 | Some(x) -> ignore (handle_unix_error alarm x));

		handle_unix_error close child_to_parent_write ;
		handle_unix_error close parent_to_child_read  ;
		
		let old_pipe_handler = Sys.signal Sys.sigpipe Sys.Signal_ignore in
		let child_output_buffer = String.create (max_output + 1) in
		let input_string_length = String.length input_string in
		let input_string_position = ref 0 in
		let output_string_position  = ref 0 in

		let rec polling_loop () : unit = 
			begin
				let input_fds = (if ((!output_string_position) < max_output) then
									 [child_to_parent_read]
								 else
									 []) in
				let output_fds = (if ((!input_string_position) < input_string_length) then
									 [parent_to_child_write]
								  else
								  	 []) in				
			   try (match (select input_fds output_fds [] (-1.0)) with
				| ((child_to_parent_read::_), _, _) ->
					(* read the output from the child *)
					let bytes_read = handle_unix_error read
									 child_to_parent_read
									 child_output_buffer
									 (!output_string_position)
									 (min (max_output - (!output_string_position))
									 	  buffer_chunk) in
					output_string_position := (!output_string_position) + bytes_read ; 
					
					if ((bytes_read > 0) && ((!output_string_position) < max_output)) then
						polling_loop ()
					else
						()
				| (_, (parent_to_child_write::_), _) ->
					(* write our input to the child *)
					let bytes_written = handle_unix_error write 
										parent_to_child_write 
									   	input_string 
									    (!input_string_position) 
									    (min (input_string_length - (!input_string_position)) 
									   		 buffer_chunk) in
					input_string_position := (!input_string_position) + bytes_written ;
					
					(* if we are finished sending output to the child, close the pipe *)
					if ((!input_string_position) >= input_string_length) then
						handle_unix_error close parent_to_child_write
					else
						() ;
					
					polling_loop ()
						
				| _ -> failwith("unexpected result in polling_loop()")) with
				
			   
			   (* a syscall might be interrupted by SIGCHLD  *)
			   | Unix_error(EINTR, _, _) -> polling_loop ()  

			end in
		(*
		 * enter the main I/O loop.  send input to child and save child's
		 * output
		 *)
		polling_loop () ;

		(* cancel timer, if any *)
		ignore (handle_unix_error alarm 0);

		(* kill our child if it is still running *)
		kill (!child_pid) 9 ;
		
		(* close any pipes if they are still open *)
		handle_unix_error close child_to_parent_read  ;
		if ((!input_string_position < input_string_length)) then
			handle_unix_error close parent_to_child_write 
		else
			() ;
	
		(* restore old pipe handler *)
		Sys.set_signal Sys.sigpipe old_pipe_handler ;
		
		let status = ref 0 in
		
		(match (waitpid [] (!child_pid)) with
		 | (_, WEXITED(s)) -> status := s
		 | (_, WSIGNALED(_)) -> ()
		 | _ -> failwith("weird result from waitpid()")) ;
		
		{
			halted = (!halted) ;
			run_time = 0.0 ;   (* fixme *)
			child_output = String.sub child_output_buffer 0 (!output_string_position); 
			return_status = (!status) 
		}
		end (* let result = ... *) in  
		
		Sys.set_signal Sys.sigalrm old_timer_handler;
		result
	
