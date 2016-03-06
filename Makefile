NAME=gamestuff
OCAMLBUILDOPTS=-use-ocamlfind -cflag -g
OCAMLBUILD=ocamlbuild $(OCAMLBUILDOPTS) -I src
LIBS=gamestuff
PROGS=

PROGFILES=$(addsuffix .native, $(PROGS))
LIBFILES=$(addsuffix .cmi, $(LIBS)) $(addsuffix .cma, $(LIBS)) $(addsuffix .cmx, $(LIBS)) $(addsuffix .cmxa, $(LIBS)) $(addsuffix .a, $(LIBS))

all: bin lib

bin:
	$(OCAMLBUILD) $(PROGFILES)

lib:
	$(OCAMLBUILD) $(LIBFILES)

install: lib
	ocamlfind install $(NAME) META $(addprefix _build/src/, $(LIBFILES))

uninstall: lib
	ocamlfind remove $(NAME)

clean:
	ocamlbuild -clean
