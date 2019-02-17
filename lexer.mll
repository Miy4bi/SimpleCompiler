(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule lexer = parse
| digit+ as num  { NUM (int_of_string num) }
| "if"                    { IF }
| "else"                  { ELSE }
| "while"                 { WHILE }
| "do"			  { DO }
| "for"		  { FOR }
| ".."			  { TO }
| "scan"                  { SCAN }
| "sprint"                { SPRINT }
| "iprint"                { IPRINT }
| "int"                   { INT }
| "return"                { RETURN }
| "type"                  { TYPE }
| "void"                  { VOID }
| id as text              { ID text }
| '\"'[^'\"']*'\"' as str { STR str }
| '='                     { ASSIGN }
| "=="                    { EQ }
| "!="                    { NEQ }
| '>'                     { GT }
| '<'                     { LT }
| ">="                    { GE }
| "<="                    { LE }
| '+'                     { PLUS }
| '-'                     { MINUS }
| '*'                     { TIMES }
| '/'                     { DIV }
| '%'			  { MOD }
| '^'			  { POW }
| "++"			  { INC }
| "+="			  { ADD }
| '{'                     { LB  }
| '}'                     { RB  }
| '['                     { LS }
| ']'                     { RS }
| '('                     { LP  }
| ')'                     { RP  }
| ','                     { COMMA }
| ';'                     { SEMI }
| [' ' '\t' '\n']         { lexer lexbuf }(* eat up whitespace *)
| eof                     { raise End_of_file }
| _                       { raise No_such_symbol }
and comment = parse
| '\n'			  { () }
| "//"                    { comment lexbuf;comment lexbuf }
| eof			  { Format.eprintf "warning: unterminated comment"}
| _			  { comment lexbuf }