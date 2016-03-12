NAME=gamestuff
OCAMLBUILDOPTS=-use-ocamlfind -cflag -g -pkg containers
OCAMLBUILD=ocamlbuild $(OCAMLBUILDOPTS) -I src
LIBS=gamestuff args vectors variates xorshift simplex_noise graph_search tilemap tilemap_graph_search pbm retry
PROGS=simplex_demo

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
