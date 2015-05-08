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

CWD = $(shell pwd)
ESMF_NOAHLIBDIR=$(CWD)/lib
ESMF_NOAHMODDIR=$(CWD)/mod
NOAHDIR=$(CWD)/noahlsm
NUOPCNOAHMKFILE=$(CWD)/nuopcnoahmkfile.mk
SRCDIR=$(CWD)/src

include $(NOAHDIR)/user_build_options

################################################################################
################################################################################

.SUFFIXES: .o .f .F .f90 .F90 .c .C

vpath %.F $(NOAHDIR)
vpath %.f90 $(SRCDIR)
vpath %.F90 $(SRCDIR)
vpath %.c $(SRCDIR)
vpath %.C $(SRCDIR)

%.o : %.F
	$(CPP) $(CPPMACROS) $(NETCDFINC) $< > $(*).f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(LOCAL_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(LOCAL_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $(*).f90	

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(LOCAL_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(LOCAL_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

# -----------------------------------------------------------------------------
all: directories libesmfnoah makefile
	
directories:
	mkdir -p lib mod
	@echo ==================================================================
	@echo "lib and mod directories created."
	@echo ==================================================================
	
libesmfnoah: $(ESMF_NOAHLIBDIR)/libesmfnoah.a

$(ESMF_NOAHLIBDIR)/libesmfnoah.a: NUOPC_Model_Noah.o
	mv *.mod $(ESMF_NOAHMODDIR)/.
	ar -r $(ESMF_NOAHLIBDIR)/libesmfnoah.a *.o
	rm *.o *.f90
	@echo ==================================================================
	@echo "NUOPC Noah library created."
	@echo ==================================================================
	
makefile: $(NUOPCNOAHMKFILE)
	
$(NUOPCNOAHMKFILE):
	touch $(NUOPCNOAHMKFILE)
	@echo "# NUOPC Noah makefile fragment" > $(NUOPCNOAHMKFILE)
	@echo "NOAHMODDIR=$(ESMF_NOAHMODDIR)" >> $(NUOPCNOAHMKFILE)
	@echo "NOAHLIBDIR=$(ESMF_NOAHLIBDIR)"  >> $(NUOPCNOAHMKFILE)
	@echo ==================================================================	
	@echo "NUOPC Noah Makefile fragment created."	
	@echo ==================================================================

NUOPC_Model_Noah.o : module_io.o module_Noahlsm_utility.o \
	module_sf_noahlsm.o module_sf_noahlsm_glacial_only.o \
	module_sfcdif_wrf.o kwm_date_utilities.o
module_io.o : module_ascii_io.o module_netcdf_io.o
module_ascii_io.o : kwm_date_utilities.o
module_sf_noahlsm.o : module_model_constants.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: clean info edit

clean:
	rm -rf $(ESMF_NOAHMODDIR)
	rm -rf $(ESMF_NOAHLIBDIR)
	rm -f *.a *.mod *.o *.f90
	rm -f $(NUOPCNOAHMKFILE)
	@echo ==================================================================
	@echo "Build has been cleaned."
	@echo ==================================================================
	

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

edit:
	nedit src/NUOPC_Model_Noah.F90 &
