(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)

open Cprime
open CP
module CPParseTreePrinter : sig val print : program -> unit end =
  struct
    let q x = ["\"" ^ x ^ "\""]
    let pfx = "  "
    let indent l = List.map (fun s -> pfx ^ s) l
    let rec comma = function [] -> []
      | [h] -> [h ^ ","]
      | (h :: t) -> h :: (comma t)
    let ps s l = 
      match l with
        [] -> [s]
        | (h :: t) -> (s ^ "(") :: (List.fold_left (fun l x -> (comma l) @ (indent x)) (indent h) t)
          @ [(")")]
    
    let puop = function NEG -> "NEG" | NOTB -> "NOTB" | NOTL -> "NOTL"
    let pbop = function ADD -> "ADD" | SUB -> "SUB" | MUL -> "MUL" | DIV -> "DIV" | REM -> "REM"
      | SHR -> "SHR" | SAR -> "SAR" | SHL -> "SHL"
      | ANDB -> "ANDB" | XORB -> "XORB" | ORB -> "ORB"
      | ANDL -> "ANDL" | ORL -> "ORL"
      | EQ -> "EQ" | NEQ -> "NEQ" | LT -> "LT" | GT -> "GT" | LE -> "LE" | GE -> "GE"
    
    let rec pse se =
      match se with
        SCONST s -> ps "STR" [[s]]
      | SVAR x -> ps "SVAR" [q x]
      | CAT (se1, se2) -> ps "CAT" [pse se1; pse se2]
      | CPY se -> ps "CPY" [pse se]
    
    let rec pne ne =
      match ne with
        NCONST i -> ps "NUM" [[string_of_int i]]
      | NVAR x -> ps "NVAR" [q x]
      | UOP (uop, ne) -> ps (puop uop) [pne ne]
      | BOP (bop, ne1, ne2) -> ps (pbop bop) [pne ne1; pne ne2]
      | LEN se -> ps "LEN" [pse se]
      | CMP (se1, se2) -> ps "CMP" [pse se1; pse se2]
      | CALL (f, ne) -> ps "CALL" [q f; pne ne]
  
    let rec pc c =
      match c with
        SKIP -> ps "SKIP" []
      | ASSIGNN (nv, ne) -> ps "ASSIGNN" [q nv; pne ne]
      | ASSIGNS (sv, se) -> ps "ASSIGNS" [q sv; pse se]
      | SEQ (c1, c2) -> ps "SEQ" [pc c1; pc c2]
      | IF (ne, c1, c2) -> ps "IF" [pne ne; pc c1; pc c2]
      | LETNV (i, ne, c) -> ps "LETNV" [q i; pne ne; pc c]
      | LETSV (i, se, c) -> ps "LETSV" [q i; pse se; pc c]
      | LETF (f, x, c1, c2) -> ps "LETF" [q f; q x; pc c1; pc c2]
      | RETURN ne -> ps "RETURN" [pne ne]
      | READINT x -> ps "READINT" [q x]
      | READSTR x -> ps "READSTR" [q x]
      | WRITEINT ne -> ps "WRITEINT" [pne ne]
      | WRITESTR se -> ps "WRITESTR" [pse se]
      | ACCEPT -> ps "ACCEPT" []
      | REJECT -> ps "REJECT" []
      | ASSERT ne -> ps "ASSERT" [pne ne]

     let print pgm =  List.iter print_endline (pc pgm)
  end
