open Cprime
open Hashtbl
open Pp

module SSA = struct

exception TODO
exception AlreadyConverted

let vartable = ref (Hashtbl.create 0)
let func_name : (string option) ref = ref None

let update_table t x =
  if Hashtbl.mem t x
    then (let n = Hashtbl.find t x in Hashtbl.replace t x (n + 1))
    else Hashtbl.add t x 1

let func_var x =
  match !func_name with
  | None -> x
  | Some f -> f ^ "@" ^ x

let var_to_ssa x n = x ^ "$" ^ string_of_int n

let next_var x =
  update_table !vartable x;
  let n = Hashtbl.find !vartable x in
  let newvars = Hashtbl.create 1 in
  Hashtbl.add newvars x n;
  (var_to_ssa x n), newvars

let merge_vt vt1 vt2 =
  Hashtbl.iter (fun x n -> if not (Hashtbl.mem vt2 x) then (Hashtbl.add vt2 x n)) vt1;
  vt2

let phi_func tbl ne vt1 vt2 =
  let rec to_phi_assigns ht l =
    (match l with
    | [] -> CP.SKIP
    | (x, (n1, n2))::r ->
      let x = func_var x in
      let nx, newvars = next_var x in
      Hashtbl.add ht x (Hashtbl.find newvars x);
      let c = to_phi_assigns ht r in
      CP.SEQ (CP.ASSIGNN (nx, CP.PHI (ne, var_to_ssa x n1, var_to_ssa x n2)), c)
    )
  in
  let t = Hashtbl.create 128 in
  Hashtbl.iter
    (fun x n -> Hashtbl.add t x (n, Hashtbl.find (if Hashtbl.mem vt2 x then vt2 else tbl) x))
    vt1;
  Hashtbl.iter
    (fun x n -> if Hashtbl.mem t x then () else Hashtbl.add t x (Hashtbl.find tbl x, n))
    vt2;
  let lst = Hashtbl.fold (fun k v l -> (k,v)::l) t [] in
  let final = Hashtbl.create 128 in
  let cmd = to_phi_assigns final lst in
  cmd, final

let rec ne_to_ssa : (CP.nvar, int) Hashtbl.t -> CP.ne -> CP.ne =
fun t ne -> match ne with
  | CP.NCONST n -> CP.NCONST n
  | CP.NVAR x -> let x = func_var x in CP.NVAR (var_to_ssa x (Hashtbl.find t x))
  | CP.UOP (uop, ne1) -> CP.UOP (uop, ne_to_ssa t ne1)
  | CP.BOP (bop, ne1, ne2) ->
    let ssa_ne1 = ne_to_ssa t ne1
    and ssa_ne2 = ne_to_ssa t ne2 in
    CP.BOP (bop, ssa_ne1, ssa_ne2)
  | CP.CALL (f, nel) -> CP.CALL (f, List.map (fun e -> ne_to_ssa t e) nel)
  | CP.PHI _ -> raise AlreadyConverted

let rec cmd_to_ssa : CP.cmd -> (CP.cmd * (CP.nvar, int) Hashtbl.t)
= fun cmd -> match cmd with
  | CP.ASSIGNN (x, ne) ->
    let ssa_ne = ne_to_ssa !vartable ne in
    let ssa_var, newvars = next_var (func_var x) in
    CP.ASSIGNN (ssa_var, ssa_ne), newvars
  | CP.SEQ (c1, c2) ->
    let ssa_c1, vt1 = cmd_to_ssa c1
    and ssa_c2, vt2 = cmd_to_ssa c2 in
    CP.SEQ (ssa_c1, ssa_c2), (merge_vt vt1 vt2)
  | CP.IF (ne, c1, c2) ->
    let oldtbl = Hashtbl.copy !vartable in
    let ssa_ne = ne_to_ssa oldtbl ne
    and ssa_c1, vt1 = cmd_to_ssa c1
    and ssa_c2, vt2 = cmd_to_ssa c2 in
    let ifcmd = CP.IF (ssa_ne, ssa_c1, ssa_c2)
    and phicmd, newvars = phi_func oldtbl ssa_ne vt1 vt2 in
    CP.SEQ (ifcmd, phicmd), newvars
  | CP.RETURN ne -> CP.RETURN (ne_to_ssa !vartable ne), Hashtbl.create 0
  | CP.READINT x ->
    let ssa_var, newvars = next_var (func_var x) in
    CP.READINT ssa_var, newvars
  | CP.SKIP | CP.ACCEPT | CP.REJECT -> cmd, Hashtbl.create 0
  | _ -> raise TODO

let rec func_to_ssa : CP.funcdecl -> CP.funcdecl
= fun fd -> match fd with
  | CP.FUN (f, p, c) ->
    func_name := Some f;
    let ssa_params = List.map (fun x -> fst (next_var (func_var x))) p
    and ssa_c, _ = cmd_to_ssa c in
    CP.FUN (f, ssa_params, ssa_c)
  | CP.FSEQ (fd1, fd2) ->
    let ssa_fd1 = func_to_ssa fd1
    and ssa_fd2 = func_to_ssa fd2 in
    CP.FSEQ (ssa_fd1, ssa_fd2)

let to_ssa : CP.program -> CP.program
= fun pgm ->
  vartable := Hashtbl.create 1024;
  func_name := None;
  match pgm with
  | CP.PRGM (func, cmd) ->
    let f = func_to_ssa func in
    func_name := None;
    let c, _ = cmd_to_ssa cmd in
    CP.PRGM (f, c)
  | CP.NOFUNC cmd -> let c, _ = cmd_to_ssa cmd in CP.NOFUNC c

end

