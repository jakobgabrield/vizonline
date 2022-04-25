{ 
    open Parser
    exception Viz_scan_error of string
}

(* 
    actual scanner part
    everything below actually creates tokens 
*)
let digit  = ['0'-'9']
let non_zero_digits = ['1'-'9']
let letter = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
(* -------- whitespaces -------- *)
| whitespace { token lexbuf}
| "/*" {multi_comment lexbuf}
| "//" {single_comment lexbuf}

(* -------- line continuation ---------*)
(*| "\\" { line_continuation lexbuf }*)
| "\\" { token lexbuf }

(* -------- keywords -------- *)
| "func" { FUNC }
| "if"   { IF }
| "else" { ELSE }
| "elif" { ELIF }
| "for" { FOR }
| "while" { WHILE }
| "infinite_loop" { INFINITE_LOOP }
| "return" {RETURN}
| "break" {BREAK}
| "continue" {CONTINUE}
| "try" {TRY}
| "catch" {CATCH}
| "raise" {RAISE} (* i guess we will also need exception right? *)
| "link" {LINK}
| "use" {USE}
| "in" {IN}
| "step" {STEP}
| "as" {AS}
| "..." {RANGE} (* used in the for loop construct *)

(* -------- types -------- *)
| "none" { T_NONE }
| "int" { T_INT }
| "string" { T_STR }
| "bool" { T_BOOL }
| "float" { T_FLOAT }
| "array" { T_ARRAY }

(* -------- arithmetic operators -------- *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }

(* -------- assignment operators -------- *)
| '=' { ASSIGN }
| "+=" {PLUSEQ}
| "-=" {MINUSEQ}
| "*=" {TIMESEQ}
| "/=" {DIVEQ}
| "%=" {MODEQ}

(* -------- relational operators -------- *)
| "==" {EQ}
| "!=" {NEQ}
| ">=" {GTEQ}
| "<=" {LTEQ}
| ">" {GT}
| "<" {LT}
| "and" {AND}
| "or" {OR}
| "not" {NOT}
| "?" {QUESTION}

(* -------- literals -------- *)
| "true" { LIT_BOOL(true) }
| "false" { LIT_BOOL(false) }
| '"' ([^ '"']* as lxm) '"' { LIT_STR(lxm) }
| ('0'+) | non_zero_digits digit*  as lxm {LIT_INT(int_of_string lxm)} (* we store everything as a positive magnitude number *)
| ("0." "0"+) | digit* "." digit+ as lxm { LIT_FLOAT(float_of_string lxm) } (* we store everything as a positive magnitude number *)

(* -------- delimiters -------- *)
| "("  { LPAREN   }
| ")"  { RPAREN   }
| "["  { LBRACKET }
| "]"  { RBRACKET }
| "{"  { LBRACE   }
| "}"  { RBRACE   }
| ":"  { COLON    }
| ";"  { SEMI     }
| ","  { COMMA    }
| "."  { DOT      }
| "|"  { BAR      }
| "->" { ARROW    }

(* --------- IDs ------------ *)
| letter (digit | letter | '_')* as lxm { ID_FUNC(lxm) } (* function names dont need @ *)
| "@" letter (digit | letter | '_')* as lxm { ID_VAR(String.sub lxm 1 ((String.length lxm) - 1)) } (* variable access and decl need @ *)

(* -------- Other ----------- *)
| eof { EOF }
| _ as char {raise (Viz_scan_error ("unexpected character: " ^ Char.escaped char))}

and single_comment = parse
 | '\n' {token lexbuf} (* this is how we will end a single line comment *)
 | _    {single_comment lexbuf} (* want to ignore the rest of the noise *) 
 
 and multi_comment = parse
 | "*/" {token lexbuf} (* end of multi line comment, head back to token *)
 | "/*" {raise (Viz_scan_error ("cannot nest multi-line comments"))} (* no nested comments *)
 | _    {multi_comment lexbuf} (* want to ignore the rest of the noise *)

(*
 and line_continuation = parse
  | whitespace { line_continuation lexbuf }
  | _ { token lexbuf }
 *)