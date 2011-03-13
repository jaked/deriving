include ../Makefile.config

OCAMLC     := ${OCAMLFIND} ocamlc
OCAMLOPT   := ${OCAMLFIND} ocamlopt
OCAMLDEP   := ${OCAMLFIND} ocamldep
PP         := -package camlp4 -ppopt ../syntax/pa_deriving.cma -syntax camlp4o
LIBS       := -I ../lib -package num

OCAMLFLAGS   = -w Aef

SOURCES = defs.ml               \
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

##

tests: ${SOURCES:.ml=.cmo}
	${OCAMLC} -o $@ ${LIBS} -linkpkg deriving.cma $^

# Common rules

%.cmi: %.mli
	${OCAMLC} ${OCAMLFLAGS} ${PP} ${LIBS} -c $<
%.cmo: %.ml
	${OCAMLC} ${OCAMLFLAGS} ${PP} ${LIBS} -c $<
%.cmx: %.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${PP} ${LIBS} -c $<

# Clean up
clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f tests
distclean: clean
	-rm -f *~ \#* .\#*

# Dependencies
depend:
	${OCAMLDEP} ${PP} *.ml *.mli > .depend

-include .depend
