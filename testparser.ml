(* File calc.ml *)
let _ =
    while true do
      print_string "> "; flush stdout ;
      let lexbuf = Lexing.from_string (read_line ()) in
      try
      (let result = Parser.main Lexer.token lexbuf in
       print_endline (Stringstuff.datCursorToString Base.emptyEnv result) )
      with Parsing.Parse_error ->
        print_endline "Parsing error" 
    done ;;
