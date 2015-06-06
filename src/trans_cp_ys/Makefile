.SUFFIXES: .ml .mll .mly .cmo .cmi .mli
OBJS=lexer.cmo parser.cmo cprime.cmo ys.cmo trans_cprime.cmo pp.cmo main.cmo
TARGET=run.elf
MLC=ocamlc
YACC=ocamlyacc
LEX=ocamllex

$(TARGET): $(OBJS)
	$(MLC) -o $@ $^

.ml.cmo:
	$(MLC) -c $<
.mli.cmi:
	$(MLC) -c $<
.mly.mli:
	$(YACC) $<
.mly.ml:
	$(YACC) $<
.mll.ml:
	$(LEX) $<

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml *.o *.cmo $(TARGET)

cprime.cmo: cprime.ml
ys.cmo: ys.ml
trans_cprime.cmo : trans_cprime.ml cprime.cmo ys.cmo
pp.cmo : pp.ml trans_cprime.cmo
parser.ml: parser.mly trans_cprime.cmo
parser.mli: parser.mly
parser.cmi: parser.mli
parser.cmo: parser.ml parser.cmi
main.cmo : trans_cprime.cmo main.ml
lexer.cmo: lexer.ml
lexer.ml: lexer.mll parser.cmo

