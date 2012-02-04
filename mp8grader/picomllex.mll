{

open Student
open Mp8common

exception EndInput

let line_count = ref 1
let char_count = ref 1

let cinc n = char_count := !char_count + n
let linc n = line_count := (char_count := 1; !line_count + n)

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let letter =['a' - 'z' 'A' - 'Z' '_']

let id_char = numeric | letter | "'"
let open_comment = "(*" 
let close_comment = "*)"
let super_close_comment = "**)"

rule token = parse
  | [' ' '\t'] { cinc 1; token lexbuf }  (* skip over whitespace *)
  | ['\n'] { linc 1; token lexbuf }  (* skip over whitespace *)
  | eof             { EOF } 

(* your rules go here *)
  | ";;" 	{ cinc 2; DBLSEMI  }
  | "+" 	{ cinc 1; PLUS  }
  | "-" 	{ cinc 1; MINUS  }
  | "*" 	{ cinc 1; TIMES  }
  | "/" 	{ cinc 1; DIV  }
  | "+." 	{ cinc 2; DPLUS  }
  | "-." 	{ cinc 2; DMINUS  }
  | "*." 	{ cinc 2; DTIMES  }
  | "/." 	{ cinc 2; DDIV  }
  | "^" 	{ cinc 1; CARAT  }
  | "**" 	{ cinc 2; EXP  }
  | "<" 	{ cinc 1; LT  }
  | ">" 	{ cinc 1; GT  }
  | "<=" 	{ cinc 2; LEQ  }
  | ">=" 	{ cinc 2; GEQ  }
  | "=" 	{ cinc 1; EQUALS  }
  | "&&" 	{ cinc 2; AND  }
  | "||" 	{ cinc 2; OR  }
  | "|" 	{ cinc 1; PIPE  }
  | "->" 	{ cinc 2; ARROW  }
  | "::" 	{ cinc 2; DCOLON  }
  | "let" 	{ cinc 3; LET  }
  | "rec" 	{ cinc 3; REC  }
  | "raise" 	{ cinc 5; RAISE }
  | "hd" 	{ cinc 4; HEAD }
  | "tl" 	{ cinc 4; TAIL }
  | "~" 	{ cinc 4; NEG }
  | "print_int" { cinc 9; PRINT }
  | "fst" 	{ cinc 3; FST }
  | "snd" 	{ cinc 3; SND }
  | ";" 	{ cinc 1; SEMI  }
  | "in" 	{ cinc 2; IN  }
  | "if" 	{ cinc 2; IF  }
  | "then" 	{ cinc 4; THEN  }
  | "else" 	{ cinc 4; ELSE  }
  | "fun" 	{ cinc 3; FUN  }
  | "try" 	{ cinc 3; TRY  }
  | "with" 	{ cinc 4; WITH  }
  | "[" 	{ cinc 1; LBRAC  }
  | "]" 	{ cinc 1; RBRAC  }
  | "(" 	{ cinc 1; LPAREN  }
  | ")" 	{ cinc 1; RPAREN  }
  | "," 	{ cinc 1; COMMA  }
  | "_"		{ cinc 1; UNDERSCORE }

  | numeric+ as s { cinc (String.length s); INT (int_of_string s) }
  | (numeric+'.'(numeric*)) as s       { cinc (String.length s); FLOAT (float_of_string s) }

  | "true"	{ cinc 4; BOOL true }
  | "false"	{ cinc 5; BOOL false }
  | "()"	{ cinc 2; UNIT }

  | (lowercase (id_char*)) as s		{ cinc (String.length s); IDENT s }

  | ("//"([^'\n']*)) as s	   { cinc (String.length s); token lexbuf }
  | open_comment	   { cinc 2; comment [({line_num = !line_count; char_num = !char_count - 2})] lexbuf }
  | close_comment	   { raise (CloseComm {line_num = !line_count; char_num = !char_count}) }
  | super_close_comment	   { raise (SuperCloseComm {line_num = !line_count; char_num = !char_count}) }

  | "\""	{ cinc 1; string "" lexbuf }

and comment open_dimens = parse 
   open_comment        { cinc 2; comment ({line_num = !line_count; char_num = !char_count - 2}::open_dimens) lexbuf } 
 | close_comment       { cinc 2;
                         match open_dimens with [pos] -> token lexbuf 
                         | dim::dimens -> comment dimens lexbuf
			 | [] -> raise (Failure "Solution error") } 
 | super_close_comment { cinc 3; token lexbuf }
 | eof		       { raise (OpenComm (List.hd open_dimens)) }
 | _                   { cinc 1; comment open_dimens lexbuf }

and string start_string = parse
   "\""	   	{ cinc 1; STRING start_string }
 | "\\\\"	{ cinc 2; string (start_string ^ "\\") lexbuf }
 | "\\'"	{ cinc 2; string (start_string ^ "'") lexbuf }
 | "\\\""	{ cinc 2; string (start_string ^ "\"") lexbuf }
 | "\\t"	{ cinc 2; string (start_string ^ "\t") lexbuf }
 | "\\n"	{ cinc 2; string (start_string ^ "\n") lexbuf }
 | "\\r"	{ cinc 2; string (start_string ^ "\r") lexbuf }
 | "\n"		{ linc 1; string (start_string ^ "\n") lexbuf }
 | _ as c	{ cinc 1; string (start_string ^ (String.make 1 c)) lexbuf }

(* do not modify this function: *)
{ let lextest s = token (Lexing.from_string s)

let opcom r = OPCOM(r.line_num,r.char_num)
let clcom r = CLCOM(r.line_num,r.char_num)
let sclcom r = SCLCOM(r.line_num,r.char_num)

  let get_all_tokens s =
      let _ = char_count := 1 in
      let _ = line_count := 1 in
      let b = Lexing.from_string (s^"\n") in
      let rec g () = 
      match token b with EOF -> []
      | t -> t :: g () in
      g ()

let try_get_all_tokens s =
    try Some (get_all_tokens s) with Failure "unmatched comment" -> None
    	     		     	     	 | Failure _ -> None
    	     			      	 | OpenComm r -> None
    	     			      	 | CloseComm r -> None
    	     			      	 | SuperCloseComm r -> None
let try_comm_get_all_tokens s =
    try Some (get_all_tokens s) with Failure "unmatched comment" -> None
    	     			      	 | OpenComm r -> Some ([opcom r])
    	     			      	 | CloseComm r -> Some ([clcom r])
    	     			      	 | SuperCloseComm r -> Some ([sclcom r])

 }

