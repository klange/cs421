let output = ref "";;

let print_string str = 
  output := !output ^ str;
  Pervasives.print_string (str)

let print_newline () = print_string "\n";;

let print_endline s = print_string (s^"\n");;

