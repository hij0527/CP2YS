open Cprime
open Ys

module Translator = struct

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

let rec trans_ne : CP.ne -> YS.expr
= fun ne -> match ne with
  | CP.NCONST n -> YS.NUM n
  | CP.NVAR x -> YS.VAR x
  | CP.PHI (ne, x1, x2) -> YS.ITE (trans_ne ne, YS.VAR x1, YS.VAR x2)
  | CP.UOP (uop, ne) -> YS.UOP (trans_uop uop, trans_ne ne)
  | CP.BOP (bop, ne1, ne2) -> YS.BOP (trans_bop bop, trans_ne ne1, trans_ne ne2)
  | CP.CALL (f, ne) -> YS.CALL (f, trans_ne ne)

exception NOTRET
let rec value_of : YS.program -> YS.expr
= fun p -> match p with
  | YS.RET e -> e
  | YS.SEQ (p1, p2) ->
    (try value_of p1 with NOTRET -> value_of p2)
  | _ -> raise NOTRET

let rec trans_cmd : YS.expr -> CP.cmd -> YS.command
= fun cond c -> match c with
  | CP.SKIP -> YS.SKIP
  | CP.ASSIGNN (x, ne) -> YS.DEFINE (x, YS.INT, trans_ne ne)
  | CP.SEQ (c1, c2) -> YS.SEQ (trans_cmd cond c1, trans_cmd cond c2)
  | CP.IF (ne, c1, c2) ->
    let exp = trans_ne ne in
    YS.SEQ (trans_cmd (YS.BOP (YS.ANDL, cond, exp)) c1, trans_cmd (YS.BOP (YS.ANDL, cond, YS.UOP (YS.NOTL, exp))) c2)
  | CP.LETNV (x, ne, c) -> raise TODO (*YS.LET (x, trans_ne ne, trans c)*)
  | CP.LETF (f, x, c1, c2) ->
    let fdef = YS.DEFINE (f, YS.TO (YS.INT, YS.INT),
                  YS.LAMBDA (x, YS.INT, value_of (trans_cmd cond c1))) in
    YS.SEQ (fdef, trans_cmd cond c2)
  | CP.RETURN ne -> YS.RET (trans_ne ne)
  | CP.READINT x -> YS.DECLARE (x, YS.INT)
  | CP.ACCEPT -> YS.ASSERT cond
  | CP.REJECT -> YS.ASSERT (YS.UOP (YS.NOTL, cond))
  | CP.ASSERT ne -> YS.ASSERT (trans_ne ne)

let trans : CP.program -> YS.program
= fun pgm -> trans_cmd YS.TRUE pgm

end

