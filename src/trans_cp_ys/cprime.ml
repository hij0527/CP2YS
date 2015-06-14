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
      | CALL of fvar * (ne list)
    type cmd =
        SKIP
      | ASSIGNN of nvar * ne
      | SEQ of cmd * cmd
      | IF of ne * cmd * cmd
      | RETURN of ne
      | READINT of nvar
      | ACCEPT
      | REJECT
      | ASSERT of ne
    type funcdecl = FUN of fvar * (nvar list) * cmd | FSEQ of funcdecl * funcdecl
    type program = PRGM of funcdecl * cmd | NOFUNC of cmd
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
      | CALL of fvar * (ne list)
    type cmd =
        SKIP
      | ASSIGNN of nvar * ne
      | SEQ of cmd * cmd
      | IF of ne * cmd * cmd
      | RETURN of ne
      | READINT of nvar
      | ACCEPT
      | REJECT
      | ASSERT of ne
    type funcdecl = FUN of fvar * (nvar list) * cmd | FSEQ of funcdecl * funcdecl
    type program = PRGM of funcdecl * cmd | NOFUNC of cmd
  end
