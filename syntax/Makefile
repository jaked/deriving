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

type.cmo type.cmx .type.ml.deps: \
	LIBS+=-syntax camlp4o -package camlp4.quotations.r
base.cmo base.cmx .base.ml.deps: \
	LIBS+=-syntax camlp4o -package camlp4.quotations.o
extend.cmo extend.cmx .extend.ml.deps: \
	LIBS+=-syntax camlp4o -package camlp4.extend,camlp4.quotations.o
pa_deriving.cmo pa_deriving.cmx .pa_deriving.ml.deps: \
	LIBS+=-syntax camlp4o -package camlp4.extend,camlp4.quotations.o
pa_deriving_tc.cmo pa_deriving_tc.cmx .pa_deriving_tc.ml.deps: \
	LIBS+=-syntax camlp4o -package camlp4.extend,camlp4.quotations.o,type-conv

CLASSES := show_class.ml         \
           dump_class.ml         \
           enum_class.ml         \
           bounded_class.ml      \
           eq_class.ml           \
           typeable_class.ml     \
           functor_class.ml      \
           pickle_class.ml       \

MAIN := pa_deriving.ml
ifneq (${TYPECONV},)
MAIN_TC := pa_deriving_tc.ml
endif

CLASSES_CMO := $(patsubst %.ml,classes/%.cmo$,${CLASSES})
CLASSES_CMX := $(patsubst %.ml,classes/%.cmx$,${CLASSES})
CLASSES_DEPS := $(patsubst %.ml,classes/.%.ml.deps$,${CLASSES})

${CLASSES_CMO} ${CLASSES_CMX} ${CLASSES_DEPS}: \
	LIBS+=-syntax camlp4o -package camlp4.quotations.o -I classes

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

pa_deriving.cma: pa_deriving_common.cmo pa_deriving.cmo ${CLASSES_CMO}
	${OCAMLC} -o $@ -a $^
pa_deriving.cmxa: pa_deriving_common.cmx pa_deriving.cmx ${CLASSES_CMX}
	${OCAMLOPT} -o $@ -a $^

pa_deriving_tc.cma: pa_deriving_common.cmo pa_deriving_tc.cmo ${CLASSES_CMO}
	${OCAMLC} -o $@ -a $^
pa_deriving_tc.cmxa: pa_deriving_common.cmx pa_deriving_tc.cmx ${CLASSES_CMX}
	${OCAMLOPT} -o $@ -a $^

${CLASSES_CMO} pa_deriving.cmo pa_deriving_tc.cmo: pa_deriving_common.cmo
${CLASSES_CMX} pa_deriving.cmx pa_deriving_tc.cmx: pa_deriving_common.cmx

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

.SECONDARY: id.ml
VERSION := $(shell head -n 1 ../VERSION)
id.ml: id.mlp ../Makefile.config ../VERSION
	[ ! -f id.ml ] || chmod u+w $@
	sed -e s/%%NAME%%/${PROJECTNAME}/ -e s/%%VERSION%%/${VERSION}/ $< > $@
	chmod -w $@

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
	-rm -f deriving deriving.opt
distclean: clean
	-rm -f .*.deps classes/.*.deps
	-rm -f *~ \#* .\#*
	-rm -f id.ml defs.ml

# Dependencies

DEPS := $(patsubst %.ml,.%.ml.deps,${SOURCES} ${MAIN} ${MAIN_TC}) \
        $(patsubst %.ml,classes/.%.ml.deps,${CLASSES}) \
        $(patsubst %.mli,.%.mli.deps,$(wildcard *.mli))

.depend: ${DEPS}
	cat .*.deps classes/.*.deps > .depend

.SECONDARY: ${DEPS}
.%.ml.deps: %.ml
	${OCAMLDEP} ${LIBS} $^ > $@
.%.mli.deps: %.mli
	${OCAMLDEP} ${LIBS} $^ > $@
classes/.%.ml.deps: classes/%.ml
	${OCAMLDEP} ${LIBS} $^ > $@
classes/.%.mli.deps: classes/%.mli
	${OCAMLDEP} ${LIBS} $^ > $@

defs.ml: defs.mli
	ln -s defs.mli defs.ml

include .depend
