/*
 * 
 *
 */

%{

exception EmptyBinding
exception ParsingError

%}

%token UNIT INT BOOL
%token <int> NUM
%token TRUE FALSE
%token <string> ID
%token PLUS MINUS STAR SLASH TILDE PERCENT ANDPERCENT VERTBAR CARET COMMA
%token EQUAL NOTEQ LB RB NOT SHR SHL SAR ANDL ORL
%token LBLOCK RBLOCK COLONEQ SEMICOLON IF THEN ELSE END
%token FUNCTION RETURN
%token READINT
%token LP RP DQUOTE
%token ACCEPT REJECT ASSERT
%token EOF

%nonassoc COMMA
%left SEMICOLON
%nonassoc THEN
%nonassoc ELSE
%right COLONEQ
%left ORL
%left ANDL
%left VERTBAR
%left CARET
%left ANDPERCENT
%left EQUAL NOTEQ
%left LB RB
%left SHR SAR SHL
%left PLUS MINUS
%left STAR SLASH PERCENT
%right NOT TILDE

%start program
%type <Cprime.CP.program> program

%%

program:
      funcdecls cmd EOF { Cprime.CP.PRGM ($1, $2) }
    | cmd EOF { Cprime.CP.NOFUNC $1 }
    ;

exprn: 
      LP exprn RP { $2 }
    | TRUE { Cprime.CP.TRUE }
    | FALSE { Cprime.CP.FALSE }
    | NUM { Cprime.CP.NCONST ($1) }
    | MINUS exprn { Cprime.CP.UOP (Cprime.CP.NEG, $2) }
    | ID { Cprime.CP.NVAR ($1) }
    | ID LP exprnlist RP { Cprime.CP.CALL ($1, $3) }
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
    | exprn EQUAL EQUAL exprn { Cprime.CP.BOP (Cprime.CP.EQ, $1, $4) }
    | exprn NOTEQ exprn { Cprime.CP.BOP (Cprime.CP.NEQ, $1, $3) }
    | exprn LB exprn { Cprime.CP.BOP (Cprime.CP.LT, $1, $3) }
    | exprn LB EQUAL exprn { Cprime.CP.BOP (Cprime.CP.LE, $1, $4) }
    | exprn RB exprn { Cprime.CP.BOP (Cprime.CP.GT, $1, $3) }
    | exprn RB EQUAL exprn { Cprime.CP.BOP (Cprime.CP.GE, $1, $4) }
    | NOT exprn { Cprime.CP.UOP (Cprime.CP.NOTL, $2) }
    | TILDE exprn { Cprime.CP.UOP (Cprime.CP.NOTB, $2) }
    ;
exprnlist:
      exprn { [$1] }
    | exprnlist COMMA exprn { $1 @ [$3] }
    ;
cmd:
      LP cmd RP { $2 }
    | UNIT { Cprime.CP.SKIP }
    | LP RP { Cprime.CP.SKIP }
    | ID COLONEQ exprn { Cprime.CP.ASSIGNN ($1,$3) }
    | cmd SEMICOLON cmd { Cprime.CP.SEQ ($1,$3) }
    | IF exprn THEN cmd ELSE cmd END { Cprime.CP.IF ($2, $4, $6) }
    | RETURN exprn { Cprime.CP.RETURN ($2) }
    | READINT ID { Cprime.CP.READINT ($2) }
    | ACCEPT { Cprime.CP.ACCEPT }
    | REJECT { Cprime.CP.REJECT }
    | ASSERT exprn { Cprime.CP.ASSERT ($2) }
    ;
funcdecls:
      func { $1 }
    | funcdecls func { Cprime.CP.FSEQ ($1, $2) }
    ;
func: FUNCTION ID LP params RP EQUAL cmd END { Cprime.CP.FUN ($2, $4, $7) }
    ;
params:
      ID { [$1] }
    | params COMMA ID { $1 @ [$3] }
    ;

%%
