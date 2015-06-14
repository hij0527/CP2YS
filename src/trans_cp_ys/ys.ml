(*
 * YICES for ver. 2.3
 *)

module type YICES =
  sig
    exception YSError of string

    type symbol = string
    type vartype =
        TO of vartype list * vartype
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
      | LAMBDA of (symbol * vartype) list * expr
      | LET of (symbol * expr) list * expr
      | UOP of uop * expr
      | BOP of bop * expr * expr
      | IF of expr * expr * expr
      | CALL of symbol * (expr list)
    type command =
        SKIP
      | DECLARE of symbol * vartype
      | DEFINE of symbol * vartype * expr
      | ASSERT of expr
      | SEQ of command * command
      | RET of expr
    type program = command
    val type_of : (symbol * vartype) list -> expr -> vartype
    val program_to_string : program -> string
    val print : program -> unit
    val to_file : program -> string -> bool
  end

module YS : YICES =
  struct
    exception YSError of string

    type symbol = string
    type vartype =
        TO of vartype list * vartype
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
      | LAMBDA of (symbol * vartype) list * expr
      | LET of (symbol * expr) list * expr
      | UOP of uop * expr
      | BOP of bop * expr * expr
      | IF of expr * expr * expr
      | CALL of symbol * (expr list)
    type command =
        SKIP
      | DECLARE of symbol * vartype
      | DEFINE of symbol * vartype * expr
      | ASSERT of expr
      | SEQ of command * command
      | RET of expr
    type program = command

    let rec drop lst num =
      if num = 0 then lst else drop (List.tl lst) (num - 1)

    let get_func_type t p =
      match t with
      | TO (l, r) ->
        let anum = List.length l
        and pnum = List.length p in
        if anum < pnum then raise (YSError "too many arguments")
        else if anum = pnum then r
        else TO (drop l pnum, r)
      | _ -> raise (YSError "function type expected")

    let rec type_of typetbl e =
      match e with
      | TRUE | FALSE -> BOOL
      | VAR x -> List.assoc x typetbl
      | NUM _ -> INT
      | LAMBDA (v, e) -> TO (List.map (fun (x, t) -> t) v, type_of typetbl e)
      | LET (_, e) -> type_of typetbl e
      | UOP (NOTL, e) -> BOOL
      | UOP (_, e) -> INT (* ys_type_of e *)
      | BOP (bop, e1, e2) ->
        (match bop with
        | ANDL | ORL | EQ | NEQ | LT | GT | LE | GE -> BOOL
        | _ -> INT
        )
      | IF (cond, e1, e2) -> type_of typetbl e1
      | CALL (f, p) -> get_func_type (List.assoc f typetbl) p

    let rec join s c =
      match s with
      | [] -> ""
      | a::[] -> a
      | a::s -> a ^ c ^ (join s c)
      
    let rec t2s t =
      match t with
      | TO (t1, t2) -> "(-> " ^ (join (List.map (fun t -> t2s t) t1) " ") ^ " " ^ (t2s t2) ^ ")"
      | BV n -> "(bitvector " ^ (string_of_int n) ^ ")"
      | INT -> "(bitvector 32)"
      | BOOL -> "bool"
    let v2s x t = x ^ "::" ^ (t2s t)
    let rec pl2s pl =
      match pl with
      | [] -> raise (YSError "empty parameter")
      | (x, t)::[] -> v2s x t
      | (x, t)::pl -> (v2s x t) ^ " " ^ (pl2s pl)
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
      | LAMBDA (pl, e) -> "\n  (lambda (" ^ (pl2s pl) ^ ")\n    " ^ (e2s e) ^ ")"
      | LET (vl, e) ->
        let vls = join (List.map (fun (x, e) -> "(" ^ x ^ " " ^ (e2s e) ^ ")") vl) "\n\t" in
        "(let (" ^ vls ^ ")\n\t" ^ (e2s e) ^ ")"
      | UOP (u, e) -> "(" ^ (u2s u) ^ " " ^ (e2s e) ^ ")"
      | BOP (b, e1, e2) -> "(" ^ (b2s b) ^ " " ^ (e2s e1) ^ " " ^ (e2s e2) ^ ")"
      | IF (e1, e2, e3) -> "(ite " ^ (e2s e1) ^ " " ^ (e2s e2) ^ " " ^ (e2s e3) ^ ")"
      | CALL (s, el) -> "(" ^ s ^ " " ^ (join (List.map (fun e -> e2s e) el) " ") ^ ")"
    let rec c2s c =
      match c with
      | SKIP -> ""
      | DECLARE (s, t) -> "(define " ^ (v2s s t) ^ ")\n"
      | DEFINE (s, t, e) -> "(define " ^ (v2s s t) ^ " " ^ (e2s e) ^ ")\n"
      | ASSERT e -> "(assert " ^ (e2s e) ^ ")\n"
      | SEQ (c1, c2) -> (c2s c1) ^ (c2s c2)
      | RET e -> (e2s e) ^ "\n"
    let program_to_string pgm = (c2s pgm) ^ "(check)\n" ^ "(show-model)\n"
    let print pgm = print_endline (program_to_string pgm)
    let to_file pgm filename =
      let s = program_to_string pgm in
      print_endline s;
      raise (YSError "TODO")
  end

