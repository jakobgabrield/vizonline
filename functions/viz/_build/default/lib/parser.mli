type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | PLUSEQ
  | MINUSEQ
  | TIMESEQ
  | DIVEQ
  | MODEQ
  | EQ
  | NEQ
  | GTEQ
  | LTEQ
  | GT
  | LT
  | AND
  | OR
  | NOT
  | QUESTION
  | FUNC
  | IF
  | ELSE
  | ELIF
  | FOR
  | WHILE
  | INFINITE_LOOP
  | RETURN
  | BREAK
  | CONTINUE
  | TRY
  | CATCH
  | RAISE
  | LINK
  | USE
  | IN
  | STEP
  | AS
  | RANGE
  | STRUCT
  | T_NONE
  | T_STR
  | T_INT
  | T_BOOL
  | T_FLOAT
  | T_ARRAY
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COLON
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT
  | BAR
  | EOF
  | ARROW
  | UNCAP_ID of (string)
  | ID_VAR of (string)
  | CAP_ID of (string)
  | LIT_STR of (string)
  | LIT_INT of (int)
  | LIT_FLOAT of (float)
  | LIT_BOOL of (bool)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
