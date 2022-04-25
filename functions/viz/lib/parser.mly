%{
open Ast
%}

/* arithmetic */
%token PLUS MINUS TIMES DIVIDE MOD

/* assignment */
%token ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ

/* relational */
%token EQ NEQ GTEQ LTEQ GT LT AND OR NOT QUESTION

/* keywords */
%token FUNC IF ELSE ELIF FOR WHILE INFINITE_LOOP RETURN BREAK
%token CONTINUE TRY CATCH RAISE LINK USE IN STEP AS RANGE

/* type */
%token T_NONE T_STR T_INT T_BOOL T_FLOAT T_ARRAY

/* delimiters */
%token SEMI LPAREN RPAREN LBRACE RBRACE COLON COMMA LBRACKET RBRACKET DOT BAR BAR
%token EOF ARROW /*LINECONTINUATION*/

/* split id into two, nothing changes outside of parser file */

%token <string> ID_FUNC /* function names */
%token <string> ID_VAR /* variable access or assign */
%token <string> LIT_STR
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <bool> LIT_BOOL

/* precedence following C standard*/
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVEQ MODEQ
%left COMMA
%left SEMI 
%left OR
%left AND
%left EQ NEQ
%nonassoc LBRACKET RBRACKET
%left LT GT LTEQ GTEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG
%right BAR /* bar is used in typecast, this precedence is like c cast right assoc */


%start program
%type <Ast.program> program

%%


program:
  /* nothing */ { [] }
  | fdecls EOF { $1 }

fdecls:
   /* nothing */ { []               }
 | fdecl fdecls { $1 :: $2 }
 
/* @x: string; */
vdecl:
  | ID_VAR COLON builtin_type {($3, $1)}


builtin_type:
  | T_NONE { NoneType }
  | T_STR { StrType }
  | T_INT { IntType }
  | T_BOOL { BoolType }
  | T_FLOAT { FloatType }
  | T_ARRAY BAR builtin_type BAR { ArrayType(Some($3), None) }


/* function declaration */
fdecl:
  /* func with args */ 
  | FUNC ID_FUNC LPAREN formals_opt RPAREN COLON builtin_type LBRACE stmt_list RBRACE
  {
    { 
      rtyp = $7;
      fname = $2;
      formals = $4;
      locals = [];
      body = $9;
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
| formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }


stmt:
  | expr SEMI { Expr $1 }
  | if_stmt { $1 }
  | block { $1 }
  | loop  { $1 }
  | return_statement SEMI { $1 }
  | id_block              { $1 }
  | vdecl var_init_opt SEMI  { VarDecl($1, $2) }
  | vdecl_list vdecl_list_init_opt SEMI { 
      let var_list        = $1 in 
      let vdecl_ty        = fst $2 in 
      let vdecl_exp       = snd $2 in 
      let create_var_decl = (fun var_name -> ((vdecl_ty, var_name), vdecl_exp)) in
      let list_of_decls = List.fold_left (fun lst var_name -> (create_var_decl var_name) :: lst ) [] var_list 
      in VarDeclList(list_of_decls)
    }  

/*
  initializer lists
  want to do something like
  @x, @y, @z -> (int, 10);
  all variables are of type int, and have value 10
  
  or
  @x, @y, @z -> (int);
*/

vdecl_list_init_opt:
  | ARROW LPAREN builtin_type RPAREN            { ($3 , None   )   }
  | ARROW LPAREN builtin_type COMMA expr RPAREN { ($3 , Some($5) ) }

vdecl_list:
  | ID_VAR { [$1] }
  | ID_VAR COMMA vdecl_list { $1 :: $3 }

var_init_opt:
  | { None }
  | ASSIGN expr { Some($2) }

return_statement:
  /* return; i.e. for nonetype return */   
  | RETURN                              { Return NoneLit }
  | RETURN expr                         { Return $2      }
/*logic_expr: within control flow, we dont want SEMI involved in our exprs 
  | expr { $1 } */

loop:
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | INFINITE_LOOP stmt        { While (BoolLit(true), $2)  }
  /* for counter in starting_num ... <ending condition>ending_num step step_number */
  | FOR ID_VAR IN LIT_INT RANGE end_condition LIT_INT increment stmt
            {
                let var_init   = Assign($2, IntLit($4)) in (* ex: i = 0 *)
                let predicate  = Binop(Id($2), $6, IntLit($7)) in (* ex: i < 5 *)
                let update     = Assign( $2, Binop(Id($2), Add, $8) ) in (* ex: i = i + 1 *)
                let block_code = $9 in
                For(var_init, predicate, update, block_code)
            }

end_condition:
  | LT   { Less   }
  | GT   { Great  }
  | GTEQ { Geq    }
  | LTEQ { Leq    }
  | EQ   { Eq     }

increment:
  /* dont define increment, default to 1 */ { IntLit(1) } 
  | STEP LIT_INT { IntLit($2)}

block: 
  | LBRACE stmt_list RBRACE                 { Block $2 }

id_block:
  | LBRACE stmt_list RBRACE  SEMI            { ID_Block $2}

if_stmt:
  | expr QUESTION expr COLON expr SEMI  { If($1, Expr($3), Expr($5)) } /* (1 > 2) ? print("true") : print("false") */
  | IF LPAREN expr RPAREN block %prec NOELSE    { If($3, $5, Block[]) } /* covers if */
  | IF LPAREN expr RPAREN block else_stmt       { If($3, $5, $6) } /* covers if/else */
  
else_stmt:
  | ELIF LPAREN expr RPAREN block %prec NOELSE { If($3, $5, Block[]) }
  | ELIF LPAREN expr RPAREN block else_stmt { If($3, $5, $6) }
  | ELSE stmt { $2 }

expr:
  /* literal */
  | string_literal { StrLit($1) }
  | LIT_INT   { IntLit($1)   }
  | LIT_BOOL  { BoolLit($1)  }
  | LIT_FLOAT { FloatLit($1) }
  | LBRACKET exprs_opt RBRACKET { ArrayLit($2) }

  /* variable access */
  | ID_VAR { Id($1) }

  /* arithmetic */
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }

  /* logical binary ops */
  | expr  EQ      expr { Binop($1, Eq, $3)      }
  | expr  NEQ     expr { Binop($1, Neq,   $3)   }
  | expr  LT      expr { Binop($1, Less,  $3)   }
  | expr  GT      expr { Binop($1, Great, $3)   }
  | expr  LTEQ    expr { Binop($1, Leq,   $3)   }
  | expr  GTEQ    expr { Binop($1, Geq,   $3)   }

  /* logical ops */
  | expr  AND    expr       { Binop($1, And,   $3)   }
  | expr  OR     expr       { Binop($1, Or,    $3)   }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr { Unop(Not, $2) }

  /* assignment */
  | ID_VAR ASSIGN expr { Assign($1, $3) }
  | ID_VAR PLUSEQ expr { Assign($1, Binop(Id($1), Add, $3))}
  | ID_VAR MINUSEQ expr { Assign($1, Binop(Id($1), Sub, $3))} 
  | ID_VAR TIMESEQ expr { Assign($1, Binop(Id($1), Mult, $3))}
  | ID_VAR DIVEQ expr { Assign($1, Binop(Id($1), Div, $3))}
  | ID_VAR MODEQ expr { Assign($1, Binop(Id($1), Mod, $3))}
  
  /* remove clarifying parens */
  | LPAREN expr RPAREN { $2 } /* (expr) -> expr. get rid of parens */

  /* function call */
  | ID_FUNC LPAREN exprs_opt RPAREN { FuncCall($1, $3) }

  /* Array subscript [] */
  | expr LBRACKET expr RBRACKET { Subscript($1, $3) }

  /* just need to ensure that this is right associative */
  /*| BAR AS typ BAR expr {TypeCast($3, $5)}  */

/* allowing for multi-line strings */
string_literal:
 | LIT_STR   {  $1 }
 | LIT_STR string_literal { $1 ^ $2 } /* concat strs */

// Match the following patterns 
// ""
// "expr, expr, expr"
exprs_opt:
  /*nothing*/ { [] }
  | exprs { $1 }

exprs:
  expr  { [$1] }
  | expr COMMA exprs { $1::$3 }