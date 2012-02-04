(*
  interactive-evaluator.ml - DO NOT EDIT
*)

open Mp9common
open Solution

let _ =
  print_endline "\nWelcome to Solution's PicoML evaluator \n";
  let rec loop (gamma:env) (mem:memory) = 
  try
      let lexbuf = Lexing.from_channel stdin in
        print_string "> "; flush stdout;
        (try
          let dec = Picomlparse.main (fun lb -> match Picomllex.token lb
			             with Picomlparse.EOF -> raise Picomllex.EndInput
				     | r -> r)
                    lexbuf in
          match infer_dec gamma dec with
             None          ->
              (print_string "\ndoes not type check\n"; loop gamma mem)
           | Some (s,ty,p) -> (
             match eval_dec (dec, mem) with 
               ((None, value), m) ->
                  (print_string "\nresult:\n";
                   print_string "_ = ";
                   print_value value;
                   print_string "\n";
                   loop gamma mem)
             | ((Some s,value), m) ->
                  (print_string "\nresult:\n";
                   print_string (s^" = ");
                   print_value value;
                   print_string "\n";
                   loop (ins_env gamma s (gen gamma ty)) m)
             )   
        with Failure s ->
              (print_newline();
               print_endline s;
               print_newline();
               loop gamma mem)
           | Parsing.Parse_error ->
              (print_string "\ndoes not parse\n";
               loop gamma mem));
  with Picomllex.EndInput -> exit 0
 in loop [] []
