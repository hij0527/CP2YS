module type YICES =
  sig
   exception Error of string
    type nvar = string
    type svar = string
    type fvar = string
    type params = string
    type uop = NEG | NOTB | NOTL
    type bop = ADD | SUB | MUL | DIV | REM
      | SHR | SAR | SHL
      | ANDB | XORB | ORB
      | ANDL | ORL
      | EQ | NEQ | LT | GT | LE | GE
    type se =
        SCONST of string
      | SVAR of svar
      | CAT of se * se
      | CPY of se
    type ne =
        NCONST of int
      | NVAR of nvar
      | UOP of uop * ne
      | BOP of bop * ne * ne
      | LEN of se
      | CMP of se * se
      | CALL of fvar * ne
    type cmd =
        SKIP
      | ASSIGNN of nvar * ne
      | ASSIGNS of svar * se
      | SEQ of cmd * cmd
      | IF of ne * cmd * cmd
      | LETNV of nvar * ne * cmd
      | LETSV of svar * se * cmd
      | LETF of fvar * nvar * cmd * cmd
      | RETURN of ne
      | READINT of nvar
      | READSTR of svar
      | WRITEINT of ne
      | WRITESTR of se
      | ACCEPT
      | REJECT
      | ASSERT of ne
    type program = cmd
    type memory
    type value
    val emptyMemory : memory
    val run : memory * program -> value
  end

module YS : YICES =
  struct
    exception Error of string
    type nvar = string
    type svar = string
    type fvar = string
    type params = string
    type uop = NEG | NOTB | NOTL
    type bop = ADD | SUB | MUL | DIV | REM
      | SHR | SAR | SHL
      | ANDB | XORB | ORB
      | ANDL | ORL
      | EQ | NEQ | LT | GT | LE | GE
    type se =
        SCONST of string
      | SVAR of svar
      | CAT of se * se
      | CPY of se
    type ne =
        NCONST of int
      | NVAR of nvar
      | UOP of uop * ne
      | BOP of bop * ne * ne
      | LEN of se
      | CMP of se * se
      | CALL of fvar * ne
    type cmd =
        SKIP
      | ASSIGNN of nvar * ne
      | ASSIGNS of svar * se
      | SEQ of cmd * cmd
      | IF of ne * cmd * cmd
      | LETNV of nvar * ne * cmd
      | LETSV of svar * se * cmd
      | LETF of fvar * nvar * cmd * cmd
      | RETURN of ne
      | READINT of nvar
      | READSTR of svar
      | WRITEINT of ne
      | WRITESTR of se
      | ACCEPT
      | REJECT
      | ASSERT of ne
    type program = cmd
    type memory = int list  (*TODO*)
    type value = int  (*TODO*)
    let emptyMemory = [] (*TODO*)
    let run (m, e) = (print_endline "running!"; 112233)
  end
