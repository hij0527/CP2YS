(*
 * YICES for ver. 2.3
 *)

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
    type bop = ADD | SUB | MUL | DIV | REM
      | SHR | SAR | SHL
      | ANDB | XORB | ORB
      | ANDL | ORL
      | EQ | NEQ | LT | GT | LE | GE
    type expr =
        TRUE | FALSE
      | VAR of symbol
      | NUM of int
      | LAMBDA of symbol * vartype * expr
      | LET of symbol * expr * expr
      | UOP of uop * expr
      | BOP of bop * expr * expr
      | ITE of expr * expr * expr
      | CALL of symbol * expr
    type command =
        SKIP
      | DECLARE of symbol * vartype
      | DEFINE of symbol * vartype * expr
      | ASSERT of expr
      | SEQ of command * command
      | RET of expr
    type program = command
    val program_to_string : program -> string
    val print : program -> unit
    val to_file : program -> string -> bool
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
    type bop = ADD | SUB | MUL | DIV | REM
      | SHR | SAR | SHL
      | ANDB | XORB | ORB
      | ANDL | ORL
      | EQ | NEQ | LT | GT | LE | GE
    type expr =
        TRUE | FALSE
      | VAR of symbol
      | NUM of int
      | LAMBDA of symbol * vartype * expr
      | LET of symbol * expr * expr
      | UOP of uop * expr
      | BOP of bop * expr * expr
      | ITE of expr * expr * expr
      | CALL of symbol * expr
    type command =
        SKIP
      | DECLARE of symbol * vartype
      | DEFINE of symbol * vartype * expr
      | ASSERT of expr
      | SEQ of command * command
      | RET of expr
    type program = command

    let rec t2s t =
      match t with
      | TO (t1, t2) -> "(-> " ^ (t2s t1) ^ " " ^ (t2s t2) ^ ")"
      | BV n -> "(bitvector " ^ (string_of_int n) ^ ")"
      | INT -> "(bitvector 32)"
      | BOOL -> "bool"
    let u2s u =
      match u with
      | NEG -> "bv-neg" | NOTB -> "bv-not" | NOTL -> "not"
    let b2s b =
      match b with
      | ADD -> "bv-add" | SUB -> "bv-sub" | MUL -> "bv-mul"
      | DIV -> "bv-div" | REM -> "bv-rem"
      | SHR -> "bv-lshr" | SAR -> "bv-ashr" | SHL -> "bv-shl"
      | ANDB -> "bv-and" | XORB -> "bv-xor" | ORB -> "bv-or"
      | ANDL -> "and" | ORL -> "or"
      | EQ -> "=" | NEQ -> "/=" | LT -> "bv-lt" | GT -> "bv-gt" | LE -> "bv-le" | GE -> "bv-ge"
    let rec e2s e =
      match e with
      | TRUE -> "true" | FALSE -> "false"
      | VAR s -> s
      | NUM n -> "(mk-bv 32 " ^ (string_of_int n) ^ ")"
      | LAMBDA (s, t, e) -> "(lambda (" ^ s ^ "::" ^ (t2s t) ^ ") " ^ (e2s e) ^ ")"
      | LET (s, e1, e2) -> "(let ((" ^ s ^ " " ^ (e2s e1) ^ ")) " ^ (e2s e2) ^ ")"
      | UOP (u, e) -> "(" ^ (u2s u) ^ " " ^ (e2s e) ^ ")"
      | BOP (b, e1, e2) -> "(" ^ (b2s b) ^ " " ^ (e2s e1) ^ " " ^ (e2s e2) ^ ")"
      | ITE (e1, e2, e3) -> "(ite " ^ (e2s e1) ^ " " ^ (e2s e2) ^ " " ^ (e2s e3) ^ ")"
      | CALL (s, e) -> "(" ^ s ^ " " ^ (e2s e) ^ ")"
    let rec c2s c =
      match c with
      | SKIP -> ""
      | DECLARE (s, t) -> "(define " ^ s ^ "::" ^ (t2s t) ^ ")\n"
      | DEFINE (s, t, e) -> "(define " ^ s ^ "::" ^ (t2s t) ^ " " ^ (e2s e) ^ ")\n"
      | ASSERT e -> "(assert " ^ (e2s e) ^ ")\n"
      | SEQ (c1, c2) -> (c2s c1) ^ (c2s c2)
      | RET e -> (e2s e) ^ "\n"
    let program_to_string pgm = (c2s pgm) ^ "(check)\n" ^ "(show-model)\n"
    let print pgm = print_endline (program_to_string pgm)
    let to_file pgm filename =
      let s = program_to_string pgm in
      print_endline s;
      raise (Error "TODO")
  end

