include Makefile.config

all: META
	${MAKE} -C syntax
	${MAKE} -C lib

byte: META
	${MAKE} -C syntax byte
	${MAKE} -C lib byte

opt: META
	${MAKE} -C syntax opt
	${MAKE} -C lib opt

META: META.in
	sed s/%%NAME%%/${PROJECTNAME}/ META.in > META

clean:
	${MAKE} -C syntax clean
	${MAKE} -C lib clean
	${MAKE} -C tests clean
	-rm -f META

distclean:
	${MAKE} -C syntax distclean
	${MAKE} -C lib distclean
	${MAKE} -C tests distclean
	-rm -f *~ \#* .\#*

.PHONY: tests
tests:
	${MAKE} -C tests
	./tests/tests

include Makefile.filelist
VERSION := $(shell head -n 1 VERSION)

install:
	${OCAMLFIND} install ${PROJECTNAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL} ${NATIMPL}

install-byte:
	${OCAMLFIND} install ${PROJECTNAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${IMPL}

install-opt:
	${OCAMLFIND} install ${PROJECTNAME} \
	  -patch-version ${VERSION} \
	  META ${INTF} ${NATIMPL}

uninstall:
	${OCAMLFIND} remove ${PROJECTNAME}

reinstall: uninstall install
reinstall-byte: uninstall install-byte
reinstall-opt: uninstall install-opt