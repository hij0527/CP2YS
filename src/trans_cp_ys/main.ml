(*
 *)

open Cprime
open Pp
open Ssa
open Ys
open Trans_cprime

let main () =
  let pp = ref false in
  let ssa = ref false in
  let ys = ref false in
  let sm5 = ref false in
  let rall = ref false in
  let all = ref false in
  let k = ref false in
  let src = ref "" in
  let _ =
    Arg.parse
      [("-pp", Arg.Set pp, "display parse tree");
       ("-ssa", Arg.Set ssa, "display parse tree (ssa form)");
       ("-ys", Arg.Set ys, "print translated ys code");
       ("-k", Arg.Set k, "run using k interpreter");
       ("-sm5", Arg.Set sm5, "translate k-- to sm5 and run using sm5 interpreter");
       ("-rall", Arg.Set rall, "run all");
       ("-all", Arg.Set all, "do all")]
      (fun x -> src := x)
      ("Usage: " ^ (Filename.basename Sys.argv.(0)) ^ " [-pp | -psm5 | -psonata | -k | -sm5] [file]")
  in
  let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
  let pgm = Parser.program Lexer.start lexbuf in
   
  if !pp then ignore (CPParseTreePrinter.print pgm)
  else if !ssa then ignore (CPParseTreePrinter.print (SSA.to_ssa pgm))
  else if !ys then ignore (YS.print (Translator.trans (SSA.to_ssa pgm)))
  else ignore (YS.to_file (Translator.trans (SSA.to_ssa pgm)) "result.ys")
  (*
  else if !psm5 then ignore (Sm5.print (Trans_k.trans pgm))
  else if !psonata then ignore (Sonata.print (Rozetta.trans (Trans_k.trans pgm)))
  else if !sm5 then ignore (Sm5.run (Trans_k.trans pgm))
  else if !k then ignore (K.run (K.emptyMemory, K.emptyEnv, pgm))
  else if !rall then (
    print_endline "======== K  ========";
    ignore (K.run (K.emptyMemory, K.emptyEnv, pgm));
    print_endline "======= Sm5 ========";
    ignore (Sm5.run (Trans_k.trans pgm));
    print_endline "====== Sonata ======";
    ignore (Sonata.run(Rozetta.trans (Trans_k.trans pgm)))
  )
  else if !all then (
    print_endline "======== K  ========";
    (KParseTreePrinter.print pgm);
    print_endline "====================";
    ignore (K.run (K.emptyMemory, K.emptyEnv, pgm));
    print_endline "======= Sm5 ========";
    ignore (Sm5.print (Trans_k.trans pgm));
    print_endline "====================";
    ignore (Sm5.run (Trans_k.trans pgm));
    print_endline "====== Sonata ======";
    ignore (Sonata.print (Rozetta.trans (Trans_k.trans pgm)));
    print_endline "====================";
    ignore (Sonata.run(Rozetta.trans (Trans_k.trans pgm)))
  )
  else ignore (Sonata.run(Rozetta.trans (Trans_k.trans pgm)))
  *)

let _ = main ()
