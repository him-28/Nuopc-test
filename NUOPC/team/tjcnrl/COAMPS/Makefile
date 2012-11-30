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
ESMFV = 6.1.0bs08
CHECK = OFF
BOPT = O
NPROC = 1
CONFIG = default.rc

export ESMF_RUNTIME_COMPLIANCECHECK=$(CHECK)
ESMF_BOPT = $(BOPT)

OS = $(shell uname -s)
HOST = $(shell hostname -s)
ifneq ($(findstring $(HOST),Tinosa tinosa),)
  DOMAIN = nrlssc.navy.mil
else
  DOMAIN = $(shell hostname | cut -f2-4 -d.)
endif
ifeq ($(DOMAIN),nrlssc.navy.mil)
  ifeq ($(OS),Linux)
    ESMFMKFILE = /common/esmf/pgi/openmpi/$(ESMFV)/esmf.mk
  else
  ifeq ($(OS),Darwin)
    ESMFMKFILE = /common/esmf/$(ESMFV)/esmf.mk
  else
    $(error OS $(OS) not supported at $(DOMAIN))
  endif
  endif
else
ifneq ($(origin ESMFMKFILE), environment)
  $(error Environment variable ESMFMKFILE was not set.)
endif
endif

include $(ESMFMKFILE)

#ESMF_F90COMPILECPPFLAGS += -DMODULE_MED=MED
ESMF_F90COMPILECPPFLAGS += -DMODULE_ATM=MOD
ESMF_F90COMPILECPPFLAGS += -DMODULE_OCN=MOD
ESMF_F90COMPILECPPFLAGS += -DMODULE_WAV=MOD
#ESMF_F90COMPILECPPFLAGS += -DMODULE_ICE=MOD
#ESMF_F90COMPILECPPFLAGS += -DMODULE_LND=MOD
ESMF_F90COMPILECPPFLAGS += -DMODULE_OBG=MBG
ESMF_F90COMPILECPPFLAGS += -DMODULE_WBG=MBG
#ESMF_F90COMPILECPPFLAGS += -DUSE_MODIFIED_STANDARD_NAMES

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
mainApp: mainApp.o drv.o con.o med.o mod.o mbg.o util.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)

# module dependencies:
mainApp.o: drv.o
drv.o: con.o med.o mod.o mbg.o
con.o: util.o
med.o: util.o
mod.o: util.o
mbg.o: util.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean edit run
dust:
	rm -f PET*.ESMF_LogFile
	rm -f MED_* ATM_* OCN_* WAV_* ICE_* LND_* OBG_* WBG_*
clean:
	rm -f mainApp *.o *.mod core*
distclean: dust clean

edit:
	gvim -p *.F90

run: dust
	-@echo ESMF_RUNTIME_COMPLIANCECHECK=$(CHECK)
	mpirun -np $(NPROC) ./mainApp $(CONFIG)
