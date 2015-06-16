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
    
    let rec pne ne =
      match ne with
      | TRUE -> ps "true" [[]]
      | FALSE -> ps "false" [[]]
      | NCONST i -> ps "NUM" [[string_of_int i]]
      | NVAR x -> ps "NVAR" [q x]
      | PHI (ne, x1, x2) -> ps "PHI" [pne ne; q x1; q x2]
      | UOP (uop, ne) -> ps (puop uop) [pne ne]
      | BOP (bop, ne1, ne2) -> ps (pbop bop) [pne ne1; pne ne2]
      | CALL (f, nel) -> ps "CALL" ([q f]@(List.map (fun e -> pne e) nel))
  
    let rec pc c =
      match c with
        SKIP -> ps "SKIP" []
      | ASSIGNN (nv, ne) -> ps "ASSIGNN" [q nv; pne ne]
      | SEQ (c1, c2) -> ps "SEQ" [pc c1; pc c2]
      | IF (ne, c1, c2) -> ps "IF" [pne ne; pc c1; pc c2]
      | RETURN ne -> ps "RETURN" [pne ne]
      | READINT x -> ps "READINT" [q x]
      | ACCEPT -> ps "ACCEPT" []
      | REJECT -> ps "REJECT" []
      | ASSERT ne -> ps "ASSERT" [pne ne]

    let rec pf f =
      match f with
      | FUN (f, p, c) ->
        let params = ["["]@(List.map (fun x -> (List.hd (q x)) ) p)@["]"] in
        ps "FUNCTION" [q f; params; pc c]
      | FSEQ (f1, f2) -> ps "" [pf f1; pf f2]

    let print pgm = match pgm with
      | PRGM (func, cmd) -> List.iter print_endline (pf func); List.iter print_endline (pc cmd)
      | NOFUNC cmd -> List.iter print_endline (pc cmd)
  end

