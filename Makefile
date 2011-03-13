include Makefile.config

all:
	${MAKE} -C syntax
	${MAKE} -C lib

byte:
	${MAKE} -C syntax byte
	${MAKE} -C lib byte

opt:
	${MAKE} -C syntax opt
	${MAKE} -C lib opt

clean:
	${MAKE} -C syntax clean
	${MAKE} -C lib clean

distclean:
	${MAKE} -C syntax distclean
	${MAKE} -C lib distclean
	-rm -f *~ \#* .\#*

include Makefile.filelist

install:
	${OCAMLFIND} install ${PROJECTNAME} \
	  -patch-version `cat VERSION` \
	  META ${INTF} ${IMPL} ${NATIMPL}

install-byte:
	${OCAMLFIND} install ${PROJECTNAME} \
	  -patch-version `cat VERSION` \
	  META ${INTF} ${IMPL}

install-opt:
	${OCAMLFIND} install ${PROJECTNAME} \
	  -patch-version `cat VERSION` \
	  META ${INTF} ${NATIMPL}

uninstall:
	${OCAMLFIND} remove ${PROJECTNAME}

reinstall: uninstall install
reinstall-byte: uninstall install-byte
reinstall-opt: uninstall install-opt