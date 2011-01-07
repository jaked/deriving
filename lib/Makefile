OCAMLMAKEFILE = ../OCamlMakefile

PATH       := $(PATH):../syntax

OCAMLOPT   = ocamlopt.opt
OCAMLC     = ocamlc.opt
OCAMLDEP   = ocamldep.opt
OCAMLFLAGS =-w ae
LIBS       = nums str unix

SOURCES = deriving_Show.ml           deriving_Show.mli		\
          deriving_interned.mli      deriving_interned.ml	\
          deriving_Eq.ml             deriving_Eq.mli		\
          deriving_Bounded.ml        deriving_Bounded.mli	\
          deriving_Enum.ml           deriving_Enum.mli		\
          deriving_monad.ml          deriving_monad.mli		\
          deriving_Dump.ml           deriving_Dump.mli		\
          deriving_Typeable.ml       deriving_Typeable.mli	\
          deriving_dynmap.ml         deriving_dynmap.mli	\
          deriving_Pickle.ml	     deriving_Pickle.mli    	\
          deriving_Functor.ml        deriving_Functor.mli	\


RESULT  = deriving

include $(OCAMLMAKEFILE)
