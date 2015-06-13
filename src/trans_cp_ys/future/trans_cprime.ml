open Cprime
open Ys

exception TODO

let trans_uop : CP.uop -> YS.uop
= fun uop -> match uop with
  | CP.NEG -> YS.NEG
  | CP.NOTB -> YS.NOTB
  | CP.NOTL -> YS.NOTL

let trans_bop : CP.bop -> YS.bop
= fun bop -> match bop with
  | CP.ADD -> YS.ADD
  | CP.SUB -> YS.SUB
  | CP.MUL -> YS.MUL
  | CP.DIV -> YS.DIV
  | CP.REM -> YS.REM
  | CP.SHR -> YS.SHR
  | CP.SAR -> YS.SAR
  | CP.SHL -> YS.SHL
  | CP.ANDB -> YS.ANDB
  | CP.XORB -> YS.XORB
  | CP.ORB -> YS.ORB
  | CP.ANDL -> YS.ANDL
  | CP.ORL -> YS.ORL
  | CP.EQ -> YS.EQ
  | CP.NEQ -> YS.NEQ
  | CP.LT -> YS.LT
  | CP.GT -> YS.GT
  | CP.LE -> YS.LE
  | CP.GE -> YS.GE

let rec trans_se : CP.se -> YS.expr
= fun se -> match se with
  | CP.SCONST s -> raise TODO
  | CP.SVAR x -> raise TODO
  | CP.CAT (se1, se2) -> YS.BOP (YS.CAT, trans_se se1, trans_se se2)
  | CP.CPY se -> raise TODO

let rec trans_ne : CP.ne -> YS.expr
= fun ne -> match ne with
  | CP.NCONST n -> YS.NUM n
  | CP.NVAR x -> YS.VAR x
  | CP.UOP (uop, ne) -> YS.UOP (trans_uop uop, trans_ne ne)
  | CP.BOP (bop, ne1, ne2) -> YS.BOP (trans_bop bop, trans_ne ne1, trans_ne ne2)
  | CP.LEN se -> YS.UOP (YS.LEN, trans_se se)
  | CP.CMP (se1, se2) -> YS.BOP (YS.CMP, trans_se se1, trans_se se2)
  | CP.CALL (f, ne) -> YS.CALL (f, trans_ne ne)

exception NOTRET
let rec value_of : YS.program -> YS.expr
= fun p -> match p with
  | YS.RET e -> e
  | YS.SEQ (p1, p2) ->
    (try value_of p1 with NOTRET -> value_of p2)
  | _ -> raise NOTRET

let rec trans : CP.program -> YS.program
= fun pgm -> match pgm with
  | CP.SKIP -> YS.SKIP
  | CP.ASSIGNN (x, ne) -> YS.DEFINE (x, YS.INT, trans_ne ne)
  | CP.ASSIGNS (x, se) -> raise TODO
  | CP.SEQ (c1, c2) -> YS.SEQ (trans c1, trans c2)
  | CP.IF (ne, c1, c2) -> raise TODO (*YS.ITE (trans_ne ne, trans c1, trans c2)*)
  | CP.LETNV (x, ne, c) -> raise TODO (*YS.LET (x, trans_ne ne, trans c)*)
  | CP.LETSV (x, se, c) -> raise TODO (*YS.LET (x, trans_se se, trans c)*)
  | CP.LETF (f, x, c1, c2) ->
    let fdef = YS.DEFINE (f, YS.TO (YS.INT, YS.INT),
                  YS.LAMBDA ((x, YS.INT), value_of (trans c1))) in
    YS.SEQ (fdef, trans c2)
  | CP.RETURN ne -> YS.RET (trans_ne ne)
  | CP.READINT x -> YS.DECLARE (x, YS.INT)
  | CP.READSTR x -> raise TODO
  | CP.WRITEINT ne -> YS.SKIP
  | CP.WRITESTR se -> YS.SKIP
  | CP.ACCEPT -> YS.ASSERT YS.TRUE
  | CP.REJECT -> YS.ASSERT YS.FALSE
  | CP.ASSERT ne -> YS.ASSERT (trans_ne ne)

