open Cprime
open Ys

module Translator = struct

exception TransError of string

let type_table : (YS.symbol * YS.vartype) list ref = ref []

let update_type x t =
  type_table := (x,t)::!type_table

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
  | CP.TRUE -> YS.TRUE
  | CP.FALSE -> YS.FALSE
  | CP.NCONST n -> YS.NUM n
  | CP.NVAR x -> YS.VAR x
  | CP.PHI (ne, x1, x2) -> YS.IF (trans_ne ne, YS.VAR x1, YS.VAR x2)
  | CP.UOP (uop, ne) -> YS.UOP (trans_uop uop, trans_ne ne)
  | CP.BOP (bop, ne1, ne2) -> YS.BOP (trans_bop bop, trans_ne ne1, trans_ne ne2)
  | CP.CALL (f, nel) -> YS.CALL (f, List.map (fun e -> trans_ne e) nel)

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
  | CP.ASSIGNN (x, ne) ->
    let ys_ne = trans_ne ne in
    update_type x (YS.type_of !type_table ys_ne);
    YS.DEFINE (x, YS.INT, ys_ne)
  | CP.SEQ (c1, c2) ->
    let ys_c1 = trans_cmd cond c1
    and ys_c2 = trans_cmd cond c2 in
    YS.SEQ (ys_c1, ys_c2)
  | CP.IF (ne, c1, c2) ->
    let exp = trans_ne ne in
    let ys_c1 = trans_cmd (YS.BOP (YS.ANDL, cond, exp)) c1
    and ys_c2 = trans_cmd (YS.BOP (YS.ANDL, cond, YS.UOP (YS.NOTL, exp))) c2 in
    YS.SEQ (ys_c1, ys_c2)
  | CP.RETURN ne -> YS.RET (trans_ne ne)
  | CP.READINT x -> 
    update_type x YS.INT;
    YS.DECLARE (x, YS.INT)
  | CP.ACCEPT -> YS.ASSERT cond
  | CP.REJECT -> YS.ASSERT (YS.UOP (YS.NOTL, cond))
  | CP.ASSERT ne -> YS.ASSERT (trans_ne ne)

let rec compress_cmd : CP.cmd -> CP.cmd
= fun c -> match c with
  | CP.SEQ (c1, c2) ->
    let cc1 = compress_cmd c1
    and cc2 = compress_cmd c2 in
    (match cc1, cc2 with
    | CP.SKIP, _ -> cc2
    | _, CP.SKIP -> cc1
    | _ -> CP.SEQ (cc1, cc2))
  | CP.IF (ne, c1, c2) -> CP.IF (ne, compress_cmd c1, compress_cmd c2)
  | _ -> c

let trans_command c = trans_cmd YS.TRUE (compress_cmd c)

(*
 * trans_fbody cond cmd = (let-symbol * let-expr) list * (return-condition * return-expr) list
 *)
let rec trans_fbody : YS.expr -> CP.cmd -> ((YS.symbol * YS.expr) list) * ((YS.expr * YS.expr) list)
= fun cond c -> match c with
  | CP.SKIP -> [], []
  | CP.ASSIGNN (x, ne) ->
    let ys_ne = trans_ne ne in
    update_type x (YS.type_of !type_table ys_ne);
    [(x, ys_ne)], []
  | CP.SEQ (c1, c2) ->
    let letl1, retl1 = trans_fbody cond c1
    and letl2, retl2 = trans_fbody cond c2 in
    letl1@letl2, retl1@retl2
  | CP.IF (ne, c1, c2) ->
    let newcond = trans_ne ne in
    let letl1, retl1 = trans_fbody (YS.BOP (YS.ANDL, cond, newcond)) c1
    and letl2, retl2 = trans_fbody (YS.BOP (YS.ANDL, cond, YS.UOP (YS.NOTL, newcond))) c2 in
    letl1@letl2, retl1@retl2
  | CP.RETURN ne -> [], [(cond, trans_ne ne)]
  | _ -> raise (TransError "non-expression in function body")

let get_ftype params retlist =
  let returntype = YS.type_of !type_table (snd (List.hd retlist))
  and paramtype = List.map (fun x -> YS.INT) params
  and lambda_params = List.map (fun x -> (x, YS.INT)) params in
  YS.TO (paramtype, returntype), lambda_params

let rec retlist_to_expr (retlist : (YS.expr * YS.expr) list) : YS.expr =
  match retlist with
  | [] -> raise (TransError "no return statement")
  | (YS.TRUE, e)::[] -> e
  | (c, e)::[] ->
    (match YS.type_of !type_table e with
    | YS.INT -> YS.IF (c, e, YS.NUM 0)  (* return 0 for default value *)
    | YS.BOOL -> YS.IF (c, e, YS.FALSE)
    | _ -> raise (TransError "return type error")
    )
  | (c, e)::r -> YS.IF (c, e, retlist_to_expr r)

let rec trans_func : CP.funcdecl -> YS.program
= fun fd -> match fd with
  | CP.FUN (f, p, c) ->
    List.iter (fun x -> update_type x YS.INT) p;
    let letlist, retlist = trans_fbody YS.TRUE c in
    let retexpr = retlist_to_expr retlist in
    let body = if (List.length letlist) > 0 then YS.LET (letlist, retexpr) else retexpr in
    let ftype, param = get_ftype p retlist in
    update_type f ftype;
    YS.DEFINE (f, ftype, YS.LAMBDA (param, body))
  | CP.FSEQ (fd1, fd2) -> YS.SEQ (trans_func fd1, trans_func fd2)

let trans : CP.program -> YS.program
= fun pgm ->
  type_table := [];
  match pgm with
  | CP.PRGM (func, cmd) ->
    let ys_func = trans_func func
    and ys_cmd = trans_command cmd in
    YS.SEQ (ys_func, ys_cmd)
  | CP.NOFUNC cmd -> trans_command cmd
end

