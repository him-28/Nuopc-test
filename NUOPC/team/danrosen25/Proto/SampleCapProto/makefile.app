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

DRIVER_OBJS := sample_driver.o
DRIVER_MODS := sample_driver_mod.mod

APP_OBJS := sample_app.o
APP_EXE  := sample_app.exe

ifeq ($(DEBUG),on)
ESMF_F90COMPILEOPTS += -g -traceback
ESMF_CXXCOMPILEOPTS += -g -traceback
ESMF_F90COMPILECPPFLAGS += -DDEBUG
ESMF_CXXCOMPILECPPFLAGS += -DDEBUG
endif

# -----------------------------------------------------------------------------
# Makefile fragments
# -----------------------------------------------------------------------------

DEP_FRONTS    :=
DEP_INCS      :=
DEP_CMPL_OBJS :=
DEP_LINK_OBJS :=
DEP_SHRD_PATH :=
DEP_SHRD_LIBS :=

ifdef COMP1_MK
  include $(COMP1_MK)
  DEP_FRONTS    += -DCOMP1_MOD=$(ESMF_DEP_FRONT)
  DEP_INCS      += $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH += $(addprefix -L, $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_PATH += $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS += $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifdef COMP2_MK
  include $(COMP2_MK)
  DEP_FRONTS    += -DCOMP2_MOD=$(ESMF_DEP_FRONT)
  DEP_INCS      += $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH += $(addprefix -L, $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_PATH += $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS += $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifdef COMP3_MK
  include $(COMP3_MK)
  DEP_FRONTS    += -DCOMP3_MOD=$(ESMF_DEP_FRONT)
  DEP_INCS      += $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH += $(addprefix -L, $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_PATH += $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS += $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifdef COMP4_MK
  include $(COMP4_MK)
  DEP_FRONTS    += -DCOMP4_MOD=$(ESMF_DEP_FRONT)
  DEP_INCS      += $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH += $(addprefix -L, $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_PATH += $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS += $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifdef COMP5_MK
  include $(COMP5_MK)
  DEP_FRONTS    += -DCOMP5_MOD=$(ESMF_DEP_FRONT)
  DEP_INCS      += $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH += $(addprefix -L, $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_PATH += $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS += $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif

################################################################################
################################################################################

# -----------------------------------------------------------------------------
# Compiler Arguments
# -----------------------------------------------------------------------------

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	@echo $(HR)
	@echo "Compiling $@"
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(DEP_FRONTS) $(DEP_INCS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	@echo $(HR)
	@echo "Compiling $@"
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(DEP_FRONTS) $(DEP_INCS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<

%.o : %.c
	@echo $(HR)
	@echo "Compiling $@"
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(DEP_FRONTS) $(DEP_INCS) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	@echo $(HR)
	@echo "Compiling $@"
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(DEP_FRONTS) $(DEP_INCS) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

# -----------------------------------------------------------------------------
# Build Targets
# -----------------------------------------------------------------------------

all: $(APP_EXE)

$(APP_EXE): $(APP_OBJS) $(DRIVER_OBJS) $(DEP_LINK_OBJS)
	@echo $(HR)
	@echo "Building Executable"
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) -o $@ $^ $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(DEP_SHRD_PATH) $(DEP_SHRD_LIBS) $(ESMF_F90ESMFLINKLIBS)

# -----------------------------------------------------------------------------
# Dependencies
# -----------------------------------------------------------------------------

$(APP_OBJS): $(DRIVER_OBJS)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info
dust:
	@echo $(HR)
	@echo "Removing Output Files"
	rm -f PET*.ESMF_LogFile *.nc out err core

clean:
	@echo $(HR)
	@echo "Removing Application Files "
	rm -f $(APP_EXE) $(APP_OBJS) $(APP_MODS) $(DRIVER_OBJS) $(DRIVER_MODS)

distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

