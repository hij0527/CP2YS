(*
   K-- Interpreter
*)

{
 open Parser
 exception Eof
 exception LexicalError
 let verbose = false
 let verbose1 s = if verbose then (print_string s; print_newline(); s) else s
 let verbose2 s = if verbose then (print_string s; print_newline()) else ()
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 63
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
                   [("unit", UNIT);
                    ("true", TRUE);
                    ("false", FALSE);
                    ("not", NOT);
                    ("shr", SHR);
                    ("sar", SAR);
                    ("shl", SHL);
                    ("and", ANDL);
                    ("or", ORL);
                    ("if", IF);
                    ("then",THEN);
                    ("else",ELSE);
                    ("end",END);
                    ("function", FUNCTION);
                    ("return", RETURN);
                    ("readint", READINT);
                    ("accept", ACCEPT);
                    ("reject", REJECT);
                    ("assert", ASSERT);
                   ] 
} 

let blank = [' ' '\n' '\t' '\r']+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let number = ['0'-'9']+

rule start =
  parse blank { start lexbuf }
    | "(*" { comment_depth :=1;
             comment lexbuf;
             start lexbuf }
    | number { NUM (int_of_string (verbose1 (Lexing.lexeme lexbuf))) }
    | id { let id = verbose1 (Lexing.lexeme lexbuf)
            in try Hashtbl.find keyword_tbl id
              with _ -> ID id
         }
    | "+" { verbose2 "+"; PLUS }
    | "-" { verbose2 "-"; MINUS }
    | "*" { verbose2 "*"; STAR }
    | "/" { verbose2 "/"; SLASH }
    | "%" { verbose2 "%"; PERCENT }
    | "~" { verbose2 "~"; TILDE }
    | "&" { verbose2 "&"; ANDPERCENT }
    | "|" { verbose2 "|"; VERTBAR}
    | "^" { verbose2 "^"; CARET }
    | "=" { verbose2 "="; EQUAL }
    | "!=" { verbose2 "!="; NOTEQ }
    | "<" { verbose2 "<"; LB }
    | ">" { verbose2 ">"; RB }
    | "]" { verbose2 "]"; RBLOCK }
    | "[" { verbose2 "["; LBLOCK }
    | ":=" { verbose2 ":="; COLONEQ }
    | ";" { verbose2 ";"; SEMICOLON }
    | "," { verbose2 ","; COMMA }
    | "(" { verbose2 "("; LP }
    | ")" { verbose2 ")"; RP }
    | "\"" { verbose2 "\""; DQUOTE }
    | eof { verbose2 "eof"; EOF }
    | _ {raise LexicalError}

and comment = parse
      "(*" {comment_depth := !comment_depth+1; comment lexbuf}
    | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
    | eof {raise Eof}
    | _   {comment lexbuf}
