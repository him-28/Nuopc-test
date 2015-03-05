# GNU Makefile template for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. Following the ESMF User's Guide, a complete ESMF     ##
## installation should ensure that a single environment variable "ESMFMKFILE" ##
## is made available on the system. This variable should point to the         ##
## "esmf.mk" file.                                                            ##
##                                                                            ##
## This example Makefile uses the "ESMFMKFILE" environment variable.          ##
##                                                                            ##
## If you notice that this Makefile cannot find variable ESMFMKFILE then      ##
## please contact the person responsible for the ESMF installation on your    ##
## system.                                                                    ##
## As a work-around you can simply hardcode the path to "esmf.mk" in the      ##
## include line below. However, doing so will render this Makefile a lot less ##
## flexible and non-portable.                                                 ##
################################################################################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)
BINPATH=./bin
OBJPATH=./obj
SRCPATH=./src
MODPATH=./mod
LIBPATH=./lib
TSTPATH=./tst

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C
vpath %.f90 : $(SRCPATH)
vpath %.F90 : $(SRCPATH)
vpath %.c   : $(SRCPATH)
vpath %.C   : $(SRCPATH)
vpath %.o   : $(OBJPATH)
vpath %.exe : $(BINPATH)

%.o : %.f90
	$(ESMF_F90COMPILER) -I$(MODPATH) -J$(MODPATH) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $< -o $(OBJPATH)/$@

%.o : %.F90
	$(ESMF_F90COMPILER) -I$(MODPATH) -J$(MODPATH) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $< -o $(OBJPATH)/$@
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -I$(MODPATH) -J$(MODPATH) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $< -o $(OBJPATH)/$@

%.o : %.C
	$(ESMF_CXXCOMPILER) -I$(MODPATH) -J$(MODPATH) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $< -o $(OBJPATH)/$@
	
# -----------------------------------------------------------------------------
all: mainApp.exe
	
mainApp.exe: mainApp.o driver.o modelCmpBmi.o nuopcBmiAdapter.o testModelBmiWrapper.o testModel.o
	$(ESMF_F90LINKER) -I$(OBJPATH) $(ESMF_F90LINKOPTS) $(BMI_F90LINKPATHS) $(ESMF_F90LINKPATHS) $(BMI_F90LINKRPATHS) $(ESMF_F90LINKRPATHS) -o $(BINPATH)/$@ $^ $(ESMF_F90ESMFLINKLIBS) $(BMI_F90LINKLIBS)
	ln -sf $(BINPATH)/$@ $@

# module dependencies:
mainApp.o: driver.o
driver.o: modelCmpBmi.o
modelCmpBmi.o: nuopcBmiAdapter.o
nuopcBmiAdapter.o : testModelBmiWrapper.o bmiDefinitions.o
testModelBmiWrapper.o : testModel.o bmiDefinitions.o
testModel.o: bmiDefinitions.o


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f PET*.ESMF_LogFile *.nc
clean:
	rm -f mainApp.exe	
	rm -f $(BINPATH)/*
	rm -f $(LIBPATH)/*
	rm -f $(OBJPATH)/*
	rm -f $(MODPATH)/*
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

test:
	@$(TSTPATH)/testBMIadapter.sh

edit:
	nedit mainApp.F90 driver.F90 modeladapter_bmi.F90 &
