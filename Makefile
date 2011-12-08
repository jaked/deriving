include Makefile.config

all: files/META files/META.${PROJECTNAME}
	${MAKE} -C syntax
	${MAKE} -C lib

byte: files/META files/META.${PROJECTNAME}
	${MAKE} -C syntax byte
	${MAKE} -C lib byte

opt: files/META files/META.${PROJECTNAME}
	${MAKE} -C syntax opt
	${MAKE} -C lib opt

files/META: files/META.in
	sed -e "s%__NAME__%${PROJECTNAME}%" \
            -e "s%__LIBDIR__%%" \
            -e "s%__SYNTAXDIR__%%" \
	  $< > $@

files/META.${PROJECTNAME}: files/META.in
	sed -e "s%__NAME__%${PROJECTNAME}%" \
            -e "s%__LIBDIR__%directory = \"../lib\"%" \
            -e "s%__SYNTAXDIR__%directory = \"../syntax\"%" \
	  $< > $@

clean: clean.local
	${MAKE} -C syntax clean DEPEND=no
	${MAKE} -C lib clean DEPEND=no
	${MAKE} -C tests clean
clean.local:
	-rm -f files/META files/META.${PROJECTNAME}

distclean: clean.local
	${MAKE} -C syntax distclean DEPEND=no
	${MAKE} -C lib distclean DEPEND=no
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
	  files/META ${SYNTAX_INTF} ${INTF} ${IMPL} ${NATIMPL} ${DOC}

install-byte:
	${OCAMLFIND} install ${PROJECTNAME} \
	  -patch-version ${VERSION} \
	  files/META ${SYNTAX_INTF} ${INTF} ${IMPL} ${DOC}

install-opt:
	${OCAMLFIND} install ${PROJECTNAME} \
	  -patch-version ${VERSION} \
	  files/META ${SYNTAX_INTF} ${INTF} ${NATIMPL} ${DOC}

uninstall:
	${OCAMLFIND} remove ${PROJECTNAME}

reinstall: uninstall install
reinstall-byte: uninstall install-byte
reinstall-opt: uninstall install-opt