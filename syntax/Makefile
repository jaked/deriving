include ../Makefile.config

OCAMLC     := ${OCAMLFIND} ocamlc
OCAMLOPT   := ${OCAMLFIND} ocamlopt
OCAMLDEP   := ${OCAMLFIND} ocamldep
LIBS       := -package camlp4
DERIVING_LIBS := -package unix,camlp4.lib

SOURCES := id.ml                 \
           utils.ml              \
           type.ml               \
           defs.ml               \
           base.ml               \
           extend.ml             \

type.cmo type.cmx type.depends: \
	LIBS+=-syntax camlp4o -package camlp4.quotations.r
base.cmo base.cmx base.depends: \
	LIBS+=-syntax camlp4o -package camlp4.quotations.o
extend.cmo extend.cmx extend.depends: \
	LIBS+=-syntax camlp4o -package camlp4.extend,camlp4.quotations.o
pa_deriving.cmo pa_deriving.cmx pa_deriving.depends: \
	LIBS+=-syntax camlp4o -package camlp4.extend,camlp4.quotations.o
pa_deriving_tc.cmo pa_deriving_tc.cmx pa_deriving_tc.depends: \
	LIBS+=-syntax camlp4o -package camlp4.extend,camlp4.quotations.o,type-conv

CLASSES := show_class.ml         \
           dump_class.ml         \
           enum_class.ml         \
           bounded_class.ml      \
           eq_class.ml           \
           typeable_class.ml     \
           functor_class.ml      \
           pickle_class.ml       \

${CLASSES:.ml=.cmo} ${CLASSES:.ml=.cmx} ${CLASSES:.ml=.depends}: \
	LIBS+=-syntax camlp4o -package camlp4.quotations.o

CAMLP4_PREFILES  := Camlp4Parsers/Camlp4OCamlRevisedParser.cmo \
                    Camlp4Parsers/Camlp4OCamlParser.cmo        \
                    Camlp4Printers/Camlp4AutoPrinter.cmo
CAMLP4_POSTFILES := Camlp4Bin.cmo

PREDEP := id.ml

##

all: byte opt

byte:: pa_deriving.cma deriving
opt:: pa_deriving.cmxa deriving.opt

ifneq (${TYPECONV},)
byte:: pa_deriving_tc.cma
opt:: pa_deriving_tc.cmxa
endif

##

pa_deriving.cma: pa_deriving_common.cmo pa_deriving.cmo ${CLASSES:.ml=.cmo}
	${OCAMLC} -o $@ -a $^
pa_deriving.cmxa: pa_deriving_common.cmx pa_deriving.cmx ${CLASSES:.ml=.cmx}
	${OCAMLOPT} -o $@ -a $^

pa_deriving_tc.cma: pa_deriving_common.cmo pa_deriving_tc.cmo ${CLASSES:.ml=.cmo}
	${OCAMLC} -o $@ -a $^
pa_deriving_tc.cmxa: pa_deriving_common.cmx pa_deriving_tc.cmx ${CLASSES:.ml=.cmx}
	${OCAMLOPT} -o $@ -a $^

${CLASSES:.ml=.cmo} pa_deriving.cmo pa_deriving_tc.cmo: pa_deriving_common.cmo
${CLASSES:.ml=.cmx} pa_deriving.cmx pa_deriving_tc.cmx: pa_deriving_common.cmx

##

pa_deriving_common.cmo: ${SOURCES:.ml=.cmo}
	${OCAMLC} -o $@ -pack $^
pa_deriving_common.cmx: ${SOURCES:.ml=.cmx}
	${OCAMLOPT} -o $@ -pack $^
${SOURCES:.ml=.cmx}: FOR_PACK=-for-pack Pa_deriving_common

##

deriving: pa_deriving.cma
	${OCAMLC} -linkpkg -o $@ \
	  ${DERIVING_LIBS} ${CAMLP4_PREFILES} $^ ${CAMLP4_POSTFILES}

deriving.opt: pa_deriving.cmxa
	${OCAMLOPT} -linkpkg -o $@ \
	  ${DERIVING_LIBS} ${CAMLP4_PREFILES:.cmo=.cmx} $^ ${CAMLP4_POSTFILES:.cmo=.cmx}

##

id.ml: id.mlp ../Makefile.config ../VERSION
	sed -e s/%%NAME%%/${PROJECTNAME}/ -e s/%%VERSION%%/`cat ../VERSION`/ $< > $@

# Common rules

%.cmi: %.mli
	${OCAMLC} ${LIBS} -c $<
%.cmo: %.ml
	${OCAMLC} ${LIBS} -c $<
%.cmx: %.ml
	${OCAMLOPT} ${LIBS} ${FOR_PACK} -c $<

# Clean up
clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f ${PREDEP}
	-rm -f deriving deriving.opt
distclean: clean
	-rm -f *~ \#* .\#*

# Dependencies
depend: ${PREDEP}
	echo > .depend
	${MAKE} --no-print-directory \
	  ${SOURCES:.ml=.depends} ${CLASSES:.ml=.depends} \
	  pa_deriving.depends
ifneq (${TYPECONV},)
	${MAKE} --no-print-directory pa_deriving_tc.depends
endif
.PHONY: ${SOURCES:.ml=.depends} ${CLASSES:.ml=.depends}
${SOURCES:.ml=.depends} ${CLASSES:.ml=.depends} pa_deriving.depends pa_deriving_tc.depends: %.depends: %.ml
	${OCAMLDEP} ${LIBS} $^ >> .depend

-include .depend
