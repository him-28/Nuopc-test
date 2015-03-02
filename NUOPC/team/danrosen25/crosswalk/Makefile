# GNU Makefile for CSDMS / CF Crosswalk Libraryr

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

SRCPATH = ./src
OBJPATH = ./obj
BINPATH = ./bin
LIBPATH = ./lib
MODPATH = ./mod
VERSION = 1

all: makeLib

test: makeTest runTest

makeLib: $(SRCPATH)/libMapUtility.F90
	$(ESMF_F90COMPILER) -J$(MODPATH) -shared -fPIC -o $(LIBPATH)/libmaputility.so.$(VERSION) $(SRCPATH)/libMapUtility.F90
	$(ESMF_F90COMPILER) -J$(MODPATH) -c $(SRCPATH)/libMapUtility.F90 -o $(OBJPATH)/libMapUtility.o
	ar rc $(LIBPATH)/libmaputility.a.$(VERSION) $(OBJPATH)/libMapUtility.o
	ln -sf libmaputility.so.$(VERSION) $(LIBPATH)/libmaputility.so
	ln -sf libmaputility.a.$(VERSION) $(LIBPATH)/libmaputility.a
	
makeTest: $(SRCPATH)/test.F90 $(LIBPATH)/libmaputility.so 
	$(ESMF_F90COMPILER) -J$(MODPATH) -I$(MODPATH) -c $(SRCPATH)/test.F90 -o $(OBJPATH)/test.o
	$(ESMF_F90LINKER) $(OBJPATH)/test.o -L$(LIBPATH) $(LIBPATH)/libmaputility.a -o test.exe
	
runTest: test.exe
	@./test.exe

# module dependencies:

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: clean

clean:
	rm -rf obj/*
	rm -rf lib/*
	rm -rf bin/*
	rm -rf mod/*
	rm *.exe
	
info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

