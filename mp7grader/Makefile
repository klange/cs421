MPVERSION=7
STUDENTSRC=mp7

OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
GMAKE=make
RM=rm
CP=cp
LN=ln
MV=mv
TAR=tar
GZIP=gzip
MKDIR=mkdir

GRADER_NAME=grader

LIBRARY_GRADER=lib/grader.cma
MODULE_STUDENT=student
MODULE_SOLUTION=solution
MODULE_RUBRIC=rubric
MODULE_COMMON=mp7parse
MODULE_COMMON2=mp7common

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(GRADER_NAME) 

DISTFILES_SOURCE=pre-rubric.c tests Makefile $(STUDENTSRC)-skeleton.mll
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi



ifeq "$(wildcard $(MODULE_COMMON2).ml)" "$(MODULE_COMMON2).ml" 
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(MODULE_COMMON2).cmi
IMPLEMENTATIONS += $(MODULE_COMMON2).cmo
$(MODULE_COMMON2).cmo: $(MODULE_COMMON2).ml 
	$(OCAMLC) -c $(MODULE_COMMON2).ml
endif

ifeq "$(wildcard $(MODULE_COMMON).mly)" "$(MODULE_COMMON).mly" 
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(MODULE_COMMON).cmi
IMPLEMENTATIONS+= $(MODULE_COMMON).cmo
$(MODULE_COMMON).cmo: $(MODULE_COMMON).mly 
	$(OCAMLYACC) $(MODULE_COMMON).mly
	$(OCAMLC) -c $(MODULE_COMMON).mli
	$(OCAMLC) -c $(MODULE_COMMON).ml
endif


########################################################################
# if mpXcommon.cmo exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON2).cmo)" "$(MODULE_COMMON2).cmo"
IMPLEMENTATIONS += $(MODULE_COMMON2).cmo
endif
ifeq "$(wildcard $(MODULE_COMMON).cmo)" "$(MODULE_COMMON).cmo"
IMPLEMENTATIONS += $(MODULE_COMMON).cmo
endif

DISTFILES_OTHER=README
DISTFILES=$(DISTFILES_SOURCE) $(DISTFILES_OBJECT) $(DISTFILES_OTHER)

IMPLEMENTATIONS += $(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo
OBJECTS=$(IMPLEMENTATIONS) $(MODULE_RUBRIC).cmo

STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_RUBRIC).cm? util.o \
		$(GRADER_NAME)

$(GRADER_NAME): $(LIBRARY_GRADER) $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(LIBRARY_GRADER) $(OBJECTS) 

$(LIBRARY_GRADER):
	$(GMAKE) -C lib
	$(LN) -s lib/util.o .

$(MODULE_STUDENT).cmo: $(MODULE_COMMON2).cmo $(MODULE_COMMON).cmo $(STUDENTSRC).mll 
	$(CP) $(STUDENTSRC).mll $(MODULE_STUDENT).mll
	$(OCAMLLEX) $(MODULE_STUDENT).mll
	$(OCAMLC) -c $(MODULE_STUDENT).ml


########################################################################
# if solution.ml exists, compile it.  otherwise assume solution.cm{o,i}
# exist.
########################################################################
ifeq "$(wildcard $(MODULE_SOLUTION).mll)" "$(MODULE_SOLUTION).mll"
$(MODULE_SOLUTION).cmo: $(MODULE_COMMON2).cmo $(MODULE_COMMON).cmo $(MODULE_SOLUTION).mll
	$(OCAMLLEX) $(MODULE_SOLUTION).mll 
	$(OCAMLC) -c $(MODULE_SOLUTION).ml
endif

$(MODULE_RUBRIC).cmo: $(MODULE_COMMON2).cmo $(MODULE_COMMON).cmo pre-$(MODULE_RUBRIC).c tests $(IMPLEMENTATIONS) $(LIBRARY_GRADER)
	gcc -E pre-$(MODULE_RUBRIC).c | grep -E -v "# 1 \"<built-in>\"" | grep -E -v "# 1 \"<command line>\"" \
                                      | grep -E -v "\"pre-rubric.c\"" | grep -E -v "# 1 \"tests\"" > $(MODULE_RUBRIC).ml
	$(OCAMLC) -c -I lib $(MODULE_RUBRIC).ml
	$(RM) -f $(MODULE_RUBRIC).ml

clean:
	$(GMAKE) -C lib clean
	$(RM) -f $(STUDENT_CLEAN)

##########################################################################
#these targets are used by staff
##########################################################################

TESTNAME=mp$(MPVERSION)grader

dist: $(DISTFILES)
	$(RM) -rf $(TESTNAME)
	$(MKDIR) $(TESTNAME)
	$(MKDIR) $(TESTNAME)/lib
	$(CP) lib/Makefile lib/*.ml $(TESTNAME)/lib
	$(CP) $(DISTFILES) $(MODULE_COMMON).cmo $(MODULE_COMMON).cmi $(MODULE_COMMON2).cmo $(MODULE_COMMON2).cmi $(TESTNAME)
	$(TAR) cpf $(TESTNAME).tar $(TESTNAME)
	$(RM) -rf $(TESTNAME)
	$(GZIP) -9 $(TESTNAME).tar

#if you are a student, do not make dist-clean.  it will delete
#your copy of solution.cmo and you will need to download a new
#copy.
dist-clean: clean
	$(RM) -f $(DISTFILES_OBJECT) mp$(MPVERSION)grader* $(MODULE_STUDENT).ml $(MODULE_STUDENT).mll $(MODULE_COMMON).cm? $(MODULE_COMMON2).cm?

