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

HR    := ========================================
HR    := $(HR)$(HR)
COMMA := ,
DIR   := $(CURDIR)

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

APP_OBJS := mainApp.o driver.o
APP_MODS := mainApp.mod driver.mod
APP_EXE  := mainApp.exe

ifneq (,$(findstring $(COMMA)satm$(COMMA),$(COMP)))
  include $(satm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_COMP1=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring $(COMMA)xatm$(COMMA),$(COMP)))
  include $(xatm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_COMP2=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif


# -----------------------------------------------------------------------------
# Dependencies
# -----------------------------------------------------------------------------

mainApp.o: driver.o

################################################################################
################################################################################

# -----------------------------------------------------------------------------
#  # Compiler Arguments
# -----------------------------------------------------------------------------

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	@echo $(HR)
	@echo "Compiling $@"
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	@echo $(HR)
	@echo "Compiling $@"
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<

%.o : %.c
	@echo $(HR)
	@echo "Compiling $@"
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	@echo $(HR)
	@echo "Compiling $@"
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

# -----------------------------------------------------------------------------

all: $(APP_EXE)

$(APP_EXE): $(APP_OBJS) libmodel.a
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info
dust:
	rm -f PET*.ESMF_LogFile *.nc
clean:
	rm -f $(APP_EXE) $(APP_OBJS) $(APP_MODS)
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

