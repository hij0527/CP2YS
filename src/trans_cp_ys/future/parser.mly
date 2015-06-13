/*
 * 
 *
 */

%{
type declLet = NVal of string * Cprime.CP.ne
             | SVal of string * Cprime.CP.se
             | Fun of string * string * Cprime.CP.cmd

exception EmptyBinding
exception ParsingError
let rec desugarLet: declLet list * Cprime.CP.cmd -> Cprime.CP.cmd  =
  fun (l, c) ->
  	match l with
	  [] -> raise EmptyBinding
	| a::[] ->
		(match a with
          NVal(x, e') -> Cprime.CP.LETNV(x,e',c)
        | SVal(x, e') -> Cprime.CP.LETSV(x,e',c)
      	| Fun(f,x,c') -> Cprime.CP.LETF(f,x,c',c))
   | a::r -> 
     (match a with
        NVal(x, e') -> Cprime.CP.LETNV(x,e', desugarLet(r,c))
      | SVal(x, e') -> Cprime.CP.LETSV(x,e', desugarLet(r,c))
      | Fun(f,x,c') -> Cprime.CP.LETF(f,x,c', desugarLet(r,c)))

%}

%token UNIT INT STR
%token <int> NUM
%token <string> ID
%token <string> NID
%token <string> SID
%token <string> FID
%token PLUS MINUS STAR SLASH TILDE PERCENT ANDPERCENT VERTBAR CARET 
%token EQUAL NOTEQ LB RB NOT SHR SHL SAR ANDL ORL
%token LEN CMP CAT CPY
%token LBLOCK RBLOCK COLONEQ SEMICOLON IF THEN ELSE
%token LET IN PROC RETURN READINT READSTR WRITEINT WRITESTR
%token LP RP DQUOTE
%token ACCEPT REJECT ASSERT
%token EOF

%nonassoc IN
%left SEMICOLON
%nonassoc THEN
%nonassoc ELSE
%right COLONEQ
%right WRITEINT WRITESTR
%left ORL
%left ANDL
%left VERTBAR
%left CARET
%left ANDPERCENT
%left EQUAL LB RB
%left SHR SAR SHL
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT TILDE

%start program
%type <Cprime.CP.cmd> program

%%

program:
       cmd EOF { $1 }
    ;

exprs:
      LP exprs RP { $2 }
    | DQUOTE ID DQUOTE { Cprime.CP.SCONST ($2) }
    | SID { Cprime.CP.SVAR ($1) }
    | CAT exprs exprs { Cprime.CP.CAT ($2, $3) }
    | CPY exprs { Cprime.CP.CPY ($2) }
    ;
exprn: 
      LP exprn RP { $2 }
    | NUM { Cprime.CP.NCONST ($1) }
    | MINUS exprn { Cprime.CP.UOP (Cprime.CP.NEG, $2) }
    | NID { Cprime.CP.NVAR ($1) }
    | FID LP exprn RP { Cprime.CP.CALL ($1, $3) }
    | exprn PLUS exprn { Cprime.CP.BOP (Cprime.CP.ADD, $1, $3) }
    | exprn MINUS exprn { Cprime.CP.BOP (Cprime.CP.SUB, $1, $3) }
    | exprn STAR exprn { Cprime.CP.BOP (Cprime.CP.MUL, $1, $3) }
    | exprn SLASH exprn { Cprime.CP.BOP (Cprime.CP.DIV, $1, $3) }
    | exprn PERCENT exprn { Cprime.CP.BOP (Cprime.CP.REM, $1, $3) }
    | exprn ANDL exprn { Cprime.CP.BOP (Cprime.CP.ANDL, $1, $3) }
    | exprn ORL exprn { Cprime.CP.BOP (Cprime.CP.ORL, $1, $3) }
    | exprn ANDPERCENT exprn { Cprime.CP.BOP (Cprime.CP.ANDB, $1, $3) }
    | exprn VERTBAR exprn { Cprime.CP.BOP (Cprime.CP.ORB, $1, $3) }
    | exprn CARET exprn { Cprime.CP.BOP (Cprime.CP.XORB, $1, $3) }
    | exprn SHR exprn { Cprime.CP.BOP (Cprime.CP.SHR, $1, $3) }
    | exprn SAR exprn { Cprime.CP.BOP (Cprime.CP.SAR, $1, $3) }
    | exprn SHL exprn { Cprime.CP.BOP (Cprime.CP.SHL, $1, $3) }
    | exprn EQUAL exprn { Cprime.CP.BOP (Cprime.CP.EQ, $1, $3) }
    | exprn NOTEQ exprn { Cprime.CP.BOP (Cprime.CP.NEQ, $1, $3) }
    | exprn LB exprn { Cprime.CP.BOP (Cprime.CP.LT, $1, $3) }
    | exprn LB EQUAL exprn { Cprime.CP.BOP (Cprime.CP.LE, $1, $4) }
    | exprn RB exprn { Cprime.CP.BOP (Cprime.CP.GT, $1, $3) }
    | exprn RB EQUAL exprn { Cprime.CP.BOP (Cprime.CP.GE, $1, $4) }
    | NOT exprn { Cprime.CP.UOP (Cprime.CP.NOTL, $2) }
    | TILDE exprn { Cprime.CP.UOP (Cprime.CP.NOTB, $2) }
    | LEN exprs { Cprime.CP.LEN ($2) }
    | CMP exprs exprs { Cprime.CP.CMP ($2, $3) }
    ;
cmd:
      LP cmd RP { $2 }
    | UNIT { Cprime.CP.SKIP }
    | LP RP { Cprime.CP.SKIP }
    | NID COLONEQ exprn { Cprime.CP.ASSIGNN ($1,$3) }
    | SID COLONEQ exprs { Cprime.CP.ASSIGNS ($1,$3) }
    | cmd SEMICOLON cmd { Cprime.CP.SEQ ($1,$3) }
    | IF exprn THEN cmd ELSE cmd { Cprime.CP.IF ($2, $4, $6) }
    | LET decls IN cmd { desugarLet($2, $4) }
    | RETURN exprn { Cprime.CP.RETURN ($2) }
    | READINT NID { Cprime.CP.READINT ($2) }
    | READSTR SID { Cprime.CP.READSTR ($2) }
    | WRITEINT exprn { Cprime.CP.WRITEINT ($2) }
    | WRITESTR exprs { Cprime.CP.WRITESTR ($2) }
    | ACCEPT { Cprime.CP.ACCEPT }
    | REJECT { Cprime.CP.REJECT }
    | ASSERT exprn { Cprime.CP.ASSERT ($2) }
    ;
decls: decl {[$1]}
    | decls SEMICOLON decl {$1 @ [$3]}
    ;
decl: NID COLONEQ exprn { NVal ($1, $3) }
    | SID COLONEQ exprs { SVal ($1, $3) }
    | PROC FID LP NID RP EQUAL cmd {Fun ($2, $4, $7)}
    ;

%%
