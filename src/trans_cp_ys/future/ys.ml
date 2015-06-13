module type YICES =
  sig
    exception Error of string

    type symbol = string
    type vartype =
        TO of vartype * vartype
      | BV of int
      | INT
      | BOOL
    type uop = NEG | NOTB | NOTL
      | CPY | LEN
    type bop = ADD | SUB | MUL | DIV | REM
      | SHR | SAR | SHL
      | ANDB | XORB | ORB
      | ANDL | ORL
      | EQ | NEQ | LT | GT | LE | GE
      | CAT | CMP
    type vardecl = symbol * vartype
    type expr =
        TRUE | FALSE
      | VAR of symbol
      | NUM of int
      | LAMBDA of vardecl * expr
      | LET of symbol * expr * expr
      | UOP of uop * expr
      | BOP of bop * expr * expr
      | ITE of expr * expr * expr
      | CALL of symbol * expr
    type command =
        SKIP
      | DEFTYPE of symbol * vartype
      | DECLARE of vardecl
      | DEFINE of symbol * vartype * expr
      | ASSERT of expr
      | SEQ of command * command
      | CHECK
      | SHOWMODEL
      | RET of expr
    type program = command
  end

module YS : YICES =
  struct
    exception Error of string

    type symbol = string
    type vartype =
        TO of vartype * vartype
      | BV of int
      | INT
      | BOOL
    type uop = NEG | NOTB | NOTL
      | CPY | LEN
    type bop = ADD | SUB | MUL | DIV | REM
      | SHR | SAR | SHL
      | ANDB | XORB | ORB
      | ANDL | ORL
      | EQ | NEQ | LT | GT | LE | GE
      | CAT | CMP
    type vardecl = symbol * vartype
    type expr =
        TRUE | FALSE
      | VAR of symbol
      | NUM of int
      | LAMBDA of vardecl * expr
      | LET of symbol * expr * expr
      | UOP of uop * expr
      | BOP of bop * expr * expr
      | ITE of expr * expr * expr
      | CALL of symbol * expr
    type command =
        SKIP
      | DEFTYPE of symbol * vartype
      | DECLARE of vardecl
      | DEFINE of symbol * vartype * expr
      | ASSERT of expr
      | SEQ of command * command
      | CHECK
      | SHOWMODEL
      | RET of expr
    type program = command
  end
