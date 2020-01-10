OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=unix.cma                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

OUTPROG1=mainloop 
OUTPROG2=testparser
# prog1 should be compiled to bytecode, and is composed of three
# units: mod1, mod2 and mod3.

# The list of object files for prog1
PROG1_OBJS=lib.cmo base.cmo lexer.cmo parser.cmo stringstuff.cmo aideur.cmo toplib.cmo top.cmo
PROG2_OBJS=lib.cmo base.cmo lexer.cmo parser.cmo stringstuff.cmo testparser.cmo
GENERATED=lexer.ml parser.ml parser.mli

mainloop: $(GENERATED) $(PROG1_OBJS)
	$(OCAMLC) -o $(OUTPROG1) $(OCAMLFLAGS) $(PROG1_OBJS)

testparser: $(GENERATED) $(PROG2_OBJS)
	$(OCAMLC) -o $(OUTPROG2) $(OCAMLFLAGS) $(PROG2_OBJS)


lexer.ml: parser.cmo
	ocamllex lexer.mll

parser.cmo: base.cmo parser.cmi


parser.ml parser.mli:
	ocamlyacc parser.mly


# prog2 should be compiled to native-code, and is composed of two
# units: mod4 and mod5.


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f $(OUTPROG1)
	rm -f $(OUTPROG2)
	rm -f *.cm[iox]
	rm -f $(GENERATED)
	rm .depend


# Dependencies
.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
