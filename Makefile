OCAMLC=ocamlc
OCAMLMKTOP=ocamlmktop
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep -intf .mli -intf .mly -impl .mli -impl .mll -impl .mly
INCLUDES=unix.cma                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

OUTPROG1=mainloop 
OUTPROG2=testparser
# prog1 should be compiled to bytecode, and is composed of three
# units: mod1, mod2 and mod3.

PROG1_WITHOUT_TOP=lib.cmo base.cmo lexerExpr.cmo parserExpr.cmo stringstuff.cmo aideur.cmo equation.cmo moreEquations.cmo toplib.cmo menus.cmo

# The list of object files for prog1
PROG1_OBJS=$(PROG1_WITHOUT_TOP) top.cmo
PROG2_OBJS=lib.cmo base.cmo lexerExpr.cmo parserExpr.cmo stringstuff.cmo testparserExpr.cmo
GENERATED=lexerExpr.ml parserExpr.ml parserExpr.mli

mainloop: $(GENERATED) $(PROG1_OBJS)
	$(OCAMLC) -o $(OUTPROG1) $(OCAMLFLAGS) $(PROG1_OBJS)

topcaml: $(GENERATED) $(PROG1_WITHOUT_TOP)
	$(OCAMLMKTOP) -o topcaml $(OCAMLFLAGS) $(PROG1_WITHOUT_TOP)

testparser: $(GENERATED) $(PROG2_OBJS)
	$(OCAMLC) -o $(OUTPROG2) $(OCAMLFLAGS) $(PROG2_OBJS)

# should guess !

parserExpr.ml: base.cmo
parserExpr.mli: base.cmo
lexerExpr.ml: base.cmo parserExpr.cmi
# 	ocamllex lexerExpr.mll

# parserExpr.cmo: base.cmo parserExpr.cmi


# parserExpr.ml parserExpr.mli:
# 	ocamlyacc parserExpr.mly


# prog2 should be compiled to native-code, and is composed of two
# units: mod4 and mod5.


# Common rules
.SUFFIXES: .mli .mll .mly .ml .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.mll.ml :
	ocamllex $(OLEXFLAGS) $<
.mly.ml :
	ocamlyacc $(OYACCFLAGS) $<
.mly.mli:
	ocamlyacc $(OYACCFLAGS) $<

# Clean up
clean:
	rm -f $(OUTPROG1)
	rm -f $(OUTPROG2)
	rm -f topcaml
	rm -f *.cm[iox]
	rm -f $(GENERATED)
	rm .depend


# Dependencies
.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
