{

open Mp7parse;;
open Mp7common;;

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


rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF } 

(* your rules go here *)
  | ";;"   { DBLSEMI }
  | "+"    { PLUS }
  | "-"    { MINUS }
  | "*"    { TIMES }
  | "/"    { DIV }
  | "+."   { DPLUS }
  | "-."   { DMINUS }
  | "*."   { DTIMES }
  | "/."   { DDIV }
  | "^"    { CARAT }
  | "**"   { EXP }
  | "<"    { LT }
  | ">"    { GT }
  | "<="   { LEQ }
  | ">="   { GEQ }
  | "="    { EQUALS }
  | "&&"   { AND }
  | "||"   { OR }
  | "|"    { PIPE }
  | "->"   { ARROW }
  | "::"   { DCOLON }
  | "let"  { LET }
  | "rec"  { REC }
  | ";"    { SEMI }
  | "in"   { IN }
  | "if"   { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun"  { FUN }
  | "["    { LBRAC }
  | "]"    { RBRAC }
  | "("    { LPAREN }
  | ")"    { RPAREN }
  | ","    { COMMA }
  | "_"    { UNDERSCORE }
  | ['0' - '9']+ as value { INT (int_of_string value) }
  | ['0' - '9']+ ['.'] ['0' - '9']* as value { FLOAT (float_of_string value) }
  | "true" { BOOL(true) }
  | "false" { BOOL(false) }
  | "()" { UNIT }
  | ['a'-'z']['a'-'z' 'A'-'Z' '_' '0'-'9']+ as ident { IDENT(ident) }
  | "(*" { comment 0 lexbuf }
  | "*)" { raise (Failure "unmatched comment") }
  | ['/']['/'] [^'\n']* ['\n'] { token lexbuf }
  | "\"" { STRING(lex_string lexbuf)}

and comment n = parse
  | "**)" { token lexbuf }
  | "*)" { if n > 0 then comment (n - 1) lexbuf else token lexbuf }
  | "(*" { comment (n + 1) lexbuf }
  | eof { raise (Failure "unmatched comment") }
  | _ { comment n lexbuf }

and lex_string = parse
  | "\\\"" { "\"" ^ (lex_string lexbuf) }
  | "\\\\" { "\\" ^ (lex_string lexbuf) }
  | "\\'"  { "\'" ^ (lex_string lexbuf) }
  | "\\t"  { "\t" ^ (lex_string lexbuf) }
  | "\\n"  { "\n" ^ (lex_string lexbuf) }
  | "\\r"  { "\r" ^ (lex_string lexbuf) }
  | "\""   { "" }
  | "\\" (['0'-'9'] ['0'-'9'] ['0'-'9'] as value) { (String.make 1 (char_of_int
  (int_of_string value))) ^ (lex_string lexbuf) }
  | [' ' '!' '#'-'~'] as s { (String.make 1 s) ^ (lex_string lexbuf) }

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
    	     			      	 | OpenComm r -> None
    	     			      	 | CloseComm r -> None
    	     			      	 | SuperCloseComm r -> None
let try_comm_get_all_tokens s =
    try Some (get_all_tokens s) with Failure "unmatched comment" -> None
    	     			      	 | OpenComm r -> Some ([opcom r])
    	     			      	 | CloseComm r -> Some ([clcom r])
    	     			      	 | SuperCloseComm r -> Some ([sclcom r])

 }

