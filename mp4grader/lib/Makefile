CC=gcc
OCAMLC=ocamlc

all: grader.cma

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $?

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $?

test.cmo: grader.cmo util.cmo

grader.cma: grader.cmo util.cmo test.cmo main.cmo
	$(OCAMLC) -a -o $@ -custom grader.cmo unix.cma util.cmo test.cmo main.cmo

clean:
	$(RM) -f *.cm? util.o grader.cma
