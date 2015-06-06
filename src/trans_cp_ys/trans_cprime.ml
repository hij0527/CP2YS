(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)
open Cprime
open Ys

exception TODO

let rec trans_se : CP.se -> YS.program
= fun se -> match se with
  | CP.SCONST s -> raise TODO
  | CP.SVAR x -> raise TODO
  | CP.CAT (se1, se2) -> raise TODO
  | CP.CPY se -> raise TODO

let rec trans_ne : CP.ne -> YS.program
= fun ne -> match ne with
  | CP.NCONST n -> raise TODO
  | CP.NVAR x -> raise TODO
  | CP.UOP (uop, ne) -> raise TODO
  | CP.BOP (bop, ne1, ne2) -> raise TODO
  | CP.LEN se -> raise TODO
  | CP.CMP (se1, se2) -> raise TODO
  | CP.CALL (f, ne) -> raise TODO

let rec trans : CP.program -> YS.program
= fun pgm -> match pgm with
  | CP.SKIP -> raise TODO
  | CP.ASSIGNN (x, ne) -> raise TODO
  | CP.ASSIGNS (x, se) -> raise TODO
  | CP.SEQ (c1, c2) -> raise TODO
  | CP.IF (ne, c1, c2) -> raise TODO
  | CP.LETNV (x, ne, c) -> raise TODO
  | CP.LETSV (x, se, c) -> raise TODO
  | CP.LETF (f, x, c1, c2) -> raise TODO
  | CP.RETURN ne -> raise TODO
  | CP.READINT x -> raise TODO
  | CP.READSTR x -> raise TODO
  | CP.WRITEINT ne -> raise TODO
  | CP.WRITESTR se -> raise TODO
  | CP.ACCEPT -> raise TODO
  | CP.REJECT -> raise TODO
  | CP.ASSERT ne -> raise TODO
