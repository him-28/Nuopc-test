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

export ESMF_RUNTIME_COMPLIANCECHECK=ON
ESMFV = 6.1.0bs03
HOST = $(shell hostname -s)
ifeq ($(HOST),parche)
  ESMFMKFILE = /common/esmf/pgi/openmpi/$(ESMFV)/esmf.mk
else
ifeq ($(HOST),tinosa)
  ESMFMKFILE = /common/esmf/$(ESMFV)/esmf.mk
else
ifeq ($(HOST),Tinosa)
  ESMFMKFILE = /common/esmf/$(ESMFV)/esmf.mk
else
ifneq ($(origin ESMFMKFILE), environment)
  $(error Environment variable ESMFMKFILE was not set.)
endif
endif
endif
endif

include $(ESMFMKFILE)

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

% : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<


# -----------------------------------------------------------------------------
esmApp: esmApp.o esm.o con.o med.o atm.o ocn.o wav.o ice.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)

# module dependencies:
esmApp.o: esm.o
esm.o: con.o med.o atm.o ocn.o wav.o ice.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean edit run
dust:
	rm -f PET*.ESMF_LogFile
clean:
	rm -f esmApp *.o *.mod
distclean: dust clean

edit:
	gvim -p *.F90

run: dust
	mpirun -np 4 ./esmApp
