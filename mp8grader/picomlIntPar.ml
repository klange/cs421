(*
  interactive-parser.ml - DO NOT EDIT
*)

open Mp8common
open Student

(* Try to detect if something is getting piped in *)
let is_interactive = 0 = (Sys.command "[ -t 0 ]")

let _ =
  (if is_interactive
      then print_endline "\nWelcome to the Student parser \n"
      else ());
  let rec loop gamma = 
  try
    let lexbuf = Lexing.from_channel stdin
    in (if is_interactive 
          then (print_string "> "; flush stdout)
          else ());
       (try
          let dec = main (fun lb -> match Picomllex.token lb with 
                                    | EOF -> raise Picomllex.EndInput
				    | r -> r)
                    lexbuf 
          in match infer_dec gamma dec with
             | None          -> (print_string "\ndoes not type check\n"; loop gamma)
             | Some (s,ty,p) ->
               let (gamma', x) = (match s with 
                                 | Some x -> (ins_env p.conclusion.gamma x (gen gamma ty), x)
		                 | None -> (p.conclusion.gamma,"_"))
               in (print_string ("val "^x^" : ");
                   print_string (string_of_monoTy ty);
                   print_string "\n\nfinal environment:\n\n";
                   print_string (string_of_env gamma');  
                   print_string "\n\nproof:\n";
                   print_string (string_of_proof p); 
                   loop gamma')      
        with Failure s -> (print_newline(); print_endline s; print_newline(); loop gamma)
           | Parsing.Parse_error -> print_string "\ndoes not parse\n"
           | OpenComm {line_num = l; char_num = c} -> (print_string ("\nUnclosed comment ("
                                                       ^string_of_int l
                                                       ^ ":"^string_of_int c^")\n"))
           | CloseComm {line_num = l; char_num = c} -> (print_string ("\nNo comment to close ("
                                                       ^string_of_int l
                                                       ^ ":"^string_of_int c^")\n"))
           | SuperCloseComm {line_num = l; char_num = c} -> 
                                                       (print_string ("\nNo comment to close ("
                                                       ^string_of_int l
                                                       ^ ":"^string_of_int c^")\n")))
        ; loop gamma
  with Picomllex.EndInput -> exit 0
 in loop []
