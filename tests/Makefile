include ../Makefile.config

OCAMLPATH := ../files:${OCAMLPATH}
export OCAMLPATH

OCAMLC     := ${OCAMLFIND} ocamlc
OCAMLOPT   := ${OCAMLFIND} ocamlopt
OCAMLDEP   := ${OCAMLFIND} ocamldep
PP         := -package ${PROJECTNAME},${PROJECTNAME}.syntax,optcomp -syntax camlp4o
PPTC       := -package ${PROJECTNAME},${PROJECTNAME}.syntax_tc,optcomp -syntax camlp4o
LIBS       := -I ../lib -package num

OCAMLFLAGS   = -w Aef

SOURCES = tests_defs.ml         \
          bimap.ml	        \
          sigs.ml               \
          pickle_tests.ml       \
	  typeable_tests.ml     \
	  bounded_tests.ml      \
	  eq_tests.ml           \
	  dump_tests.ml         \
	  enum_tests.ml         \
	  functor_tests.ml      \
	  show_tests.ml         \
          exp.ml                \
          inline.ml             \

ifneq (${TYPECONV},)

SOURCES += tc.ml                 \

endif

##

tests${EXEEXT}: ${SOURCES:.ml=.cmo}
	${OCAMLC} -o $@ ${LIBS} -linkpkg deriving.cma $^

# Common rules

%.cmi: %.mli
	${OCAMLC} ${OCAMLFLAGS} ${PP} ${LIBS} -c $<
%.cmo: %.ml
	${OCAMLC} ${OCAMLFLAGS} ${PP} ${LIBS} -c $<
%.cmx: %.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${PP} ${LIBS} -c $<

tc.cmo: tc.ml
	${OCAMLC} ${OCAMLFLAGS} ${PPTC} ${LIBS} -c $<
tc.cmx: tc.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${PPTC} ${LIBS} -c $<

# Clean up
clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *${OBJEXT} *${LIBEXT} *.annot
	-rm -f tests${EXEEXT}
distclean: clean
	-rm -f *~ \#* .\#*

# Dependencies
depend:
	${OCAMLDEP} ${PP} *.ml *.mli > .depend

-include .depend
