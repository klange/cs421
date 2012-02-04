CC=gcc
OCAMLC=ocamlc

OCAML_C_INTERFACE_INCLUDE=/usr/local/lib/ocaml
#OCAML_C_INTERFACE_INCLUDE=/home/class/cs321/local/lib/ocaml

all: grader.cma

#util.o: util.c
#	$(CC) -c -o $@ $? -I$(OCAML_C_INTERFACE_INCLUDE)

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $?

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $?

test.cmo: grader.cmo util.cmo

grader.cma: grader.cmo test.cmo util.cmo main.cmo
#	$(OCAMLC) -a -o $@ -custom grader.cmo unix.cma util.o util.cmo test.cmo main.cmo
	$(OCAMLC) -a -o $@ -custom grader.cmo unix.cma util.cmo test.cmo main.cmo

clean:
	$(RM) -f *.cm? util.o grader.cma
