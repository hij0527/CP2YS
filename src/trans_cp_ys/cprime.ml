module type CPRIME =
  sig
    exception Error of string
    type nvar = string
    type fvar = string
    type params = string
    type uop = NEG | NOTB | NOTL
    type bop = ADD | SUB | MUL | DIV | REM
      | SHR | SAR | SHL
      | ANDB | XORB | ORB
      | ANDL | ORL
      | EQ | NEQ | LT | GT | LE | GE
    type ne =
        NCONST of int
      | NVAR of nvar
      | PHI of ne * nvar * nvar
      | UOP of uop * ne
      | BOP of bop * ne * ne
      | CALL of fvar * ne
    type cmd =
        SKIP
      | ASSIGNN of nvar * ne
      | SEQ of cmd * cmd
      | IF of ne * cmd * cmd
      | LETNV of nvar * ne * cmd
      | LETF of fvar * nvar * cmd * cmd
      | RETURN of ne
      | READINT of nvar
      | ACCEPT
      | REJECT
      | ASSERT of ne
    type funcdecl = FUN of fvar * nvar * cmd
(*    type program = funcdecl list * cmd *)
    type program = cmd
    type memory
    type value
    val emptyMemory : memory
    val run : memory * program -> value
  end

module CP : CPRIME =
  struct
    exception Error of string
    type nvar = string
    type fvar = string
    type params = string
    type uop = NEG | NOTB | NOTL
    type bop = ADD | SUB | MUL | DIV | REM
      | SHR | SAR | SHL
      | ANDB | XORB | ORB
      | ANDL | ORL
      | EQ | NEQ | LT | GT | LE | GE
    type ne =
        NCONST of int
      | NVAR of nvar
      | PHI of ne * nvar * nvar
      | UOP of uop * ne
      | BOP of bop * ne * ne
      | CALL of fvar * ne
    type cmd =
        SKIP
      | ASSIGNN of nvar * ne
      | SEQ of cmd * cmd
      | IF of ne * cmd * cmd
      | LETNV of nvar * ne * cmd
      | LETF of fvar * nvar * cmd * cmd
      | RETURN of ne
      | READINT of nvar
      | ACCEPT
      | REJECT
      | ASSERT of ne
    type funcdecl = FUN of fvar * nvar * cmd
(*    type program = funcdecl list * cmd *)
    type program = cmd
    type memory = int list  (*TODO*)
    type value = int  (*TODO*)
    let emptyMemory = [] (*TODO*)
    let run (m, e) = (print_endline "running!"; 112233)
  end
