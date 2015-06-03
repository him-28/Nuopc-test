#-------------------------------------------------------------------------------
# COAMPS driver/coupler Makefile
#-------------------------------------------------------------------------------
# CONFIGURATION IDENTIFICATION $HeadURL$
# CONFIGURATION IDENTIFICATION @(#)$Id$
#-------------------------------------------------------------------------------

# ESMF makefile fragment must be included
ifdef ESMFMKFILE
  include $(ESMFMKFILE)
else
  $(error ERROR: ESMFMKFILE must point to the ESMF makefile fragment)
endif

# default target
default :
	$(MAKE) coamps_bkm
	$(MAKE) coamps_nlm
	$(MAKE) coamps_rpm
	$(MAKE) coamps_adm
	$(MAKE) coamps_vda

#-------------------------------------------------------------------------------
# common settings
#-------------------------------------------------------------------------------
SHELL := /bin/sh
CP    := /bin/cp -f
MV    := /bin/mv -f
RM    := /bin/rm -fr
NPROC ?= 8

# directories
ifdef COAMPS_DIR
  BINDIR := $(COAMPS_DIR)/bin
else
  BINDIR := $(CURDIR)/bin
endif
OBJDIR := $(CURDIR)/obj
MODDIR := $(CURDIR)/mod

# single/double precision
ifeq ($(COAMPS_PREC),r8)
  TOP_CPPFLAGS := -DREAL8
else
  TOP_CPPFLAGS := -DREAL4
endif

# for COAMPS_Gsrumd
TOP_CPPFLAGS += -DENABLE_MPI

# top-level module include path
TOP_INCPATH := $(addprefix -I, $(MODDIR))

# driver compile object dependencies
TOP_CMPL_OBJS :=
TOP_CMPL_OBJS += $(OBJDIR)/COAMPS_Futil.o
TOP_CMPL_OBJS += $(OBJDIR)/COAMPS_Connector.o
TOP_CMPL_OBJS += $(OBJDIR)/COAMPS_Mdata.o
TOP_CMPL_OBJS += $(OBJDIR)/COAMPS_Mdumb.o

# driver link object dependencies
TOP_LINK_OBJS :=
TOP_LINK_OBJS += $(TOP_CMPL_OBJS)
TOP_LINK_OBJS += $(OBJDIR)/COAMPS_Gutil.o
TOP_LINK_OBJS += $(OBJDIR)/COAMPS_Gsrumd.o
TOP_LINK_OBJS += $(OBJDIR)/COAMPS_Gdem.o

# initialize variables here so the RHS expansion is immediate
NLM_CPPFLAGS  :=
RPM_CPPFLAGS  :=
ADM_CPPFLAGS  :=
DEP_INCPATH   :=
DEP_CMPL_OBJS :=
DEP_LINK_OBJS :=
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# BKM Settings
#-------------------------------------------------------------------------------
ifeq (coamps_bkm,$(MAKECMDGOALS))
  DEP_CMPL_OBJS += $(OBJDIR)/COAMPS_Gutil.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_BKM_App.o
  DEP_LINK_OBJS += $(DEP_CMPL_OBJS)
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_Gdem.o
endif
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# NLM Settings
#-------------------------------------------------------------------------------
ifeq (coamps_nlm,$(MAKECMDGOALS))
  DEP_CMPL_OBJS += $(TOP_CMPL_OBJS)
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_NLM_App.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_NLM_Drv.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_Dutil.o
  DEP_LINK_OBJS += $(TOP_LINK_OBJS)
  ifdef MKFILE_NLM_ATM1
    include $(MKFILE_NLM_ATM1)
    NLM_CPPFLAGS  += -DFRONT_ATM1=$(ESMF_DEP_FRONT) -DTYPE_ATM1=\"$(TYPE_ATM1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_ATM2
    include $(MKFILE_NLM_ATM2)
    NLM_CPPFLAGS  += -DFRONT_ATM2=$(ESMF_DEP_FRONT) -DTYPE_ATM2=\"$(TYPE_ATM2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_OCN1
    include $(MKFILE_NLM_OCN1)
    NLM_CPPFLAGS  += -DFRONT_OCN1=$(ESMF_DEP_FRONT) -DTYPE_OCN1=\"$(TYPE_OCN1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_OCN2
    include $(MKFILE_NLM_OCN2)
    NLM_CPPFLAGS  += -DFRONT_OCN2=$(ESMF_DEP_FRONT) -DTYPE_OCN2=\"$(TYPE_OCN2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_WAV1
    include $(MKFILE_NLM_WAV1)
    NLM_CPPFLAGS  += -DFRONT_WAV1=$(ESMF_DEP_FRONT) -DTYPE_WAV1=\"$(TYPE_WAV1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_WAV2
    include $(MKFILE_NLM_WAV2)
    NLM_CPPFLAGS  += -DFRONT_WAV2=$(ESMF_DEP_FRONT) -DTYPE_WAV2=\"$(TYPE_WAV2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_ICE1
    include $(MKFILE_NLM_ICE1)
    NLM_CPPFLAGS  += -DFRONT_ICE1=$(ESMF_DEP_FRONT) -DTYPE_ICE1=\"$(TYPE_ICE1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_ICE2
    include $(MKFILE_NLM_ICE2)
    NLM_CPPFLAGS  += -DFRONT_ICE2=$(ESMF_DEP_FRONT) -DTYPE_ICE2=\"$(TYPE_ICE2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_LND1
    include $(MKFILE_NLM_LND1)
    NLM_CPPFLAGS  += -DFRONT_LND1=$(ESMF_DEP_FRONT) -DTYPE_LND1=\"$(TYPE_LND1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_NLM_LND2
    include $(MKFILE_NLM_LND2)
    NLM_CPPFLAGS  += -DFRONT_LND2=$(ESMF_DEP_FRONT) -DTYPE_LND2=\"$(TYPE_LND2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
endif
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# RPM Settings
#-------------------------------------------------------------------------------
ifeq (coamps_rpm,$(MAKECMDGOALS))
  DEP_CMPL_OBJS += $(TOP_CMPL_OBJS)
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_RPM_App.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_RPM_Drv.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_Dutil.o
  DEP_LINK_OBJS += $(TOP_LINK_OBJS)
  ifdef MKFILE_RPM_ATM1
    include $(MKFILE_RPM_ATM1)
    RPM_CPPFLAGS  += -DFRONT_ATM1=$(ESMF_DEP_FRONT) -DTYPE_ATM1=\"$(TYPE_ATM1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_ATM2
    include $(MKFILE_RPM_ATM2)
    RPM_CPPFLAGS  += -DFRONT_ATM2=$(ESMF_DEP_FRONT) -DTYPE_ATM2=\"$(TYPE_ATM2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_OCN1
    include $(MKFILE_RPM_OCN1)
    RPM_CPPFLAGS  += -DFRONT_OCN1=$(ESMF_DEP_FRONT) -DTYPE_OCN1=\"$(TYPE_OCN1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_OCN2
    include $(MKFILE_RPM_OCN2)
    RPM_CPPFLAGS  += -DFRONT_OCN2=$(ESMF_DEP_FRONT) -DTYPE_OCN2=\"$(TYPE_OCN2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_WAV1
    include $(MKFILE_RPM_WAV1)
    RPM_CPPFLAGS  += -DFRONT_WAV1=$(ESMF_DEP_FRONT) -DTYPE_WAV1=\"$(TYPE_WAV1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_WAV2
    include $(MKFILE_RPM_WAV2)
    RPM_CPPFLAGS  += -DFRONT_WAV2=$(ESMF_DEP_FRONT) -DTYPE_WAV2=\"$(TYPE_WAV2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_ICE1
    include $(MKFILE_RPM_ICE1)
    RPM_CPPFLAGS  += -DFRONT_ICE1=$(ESMF_DEP_FRONT) -DTYPE_ICE1=\"$(TYPE_ICE1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_ICE2
    include $(MKFILE_RPM_ICE2)
    RPM_CPPFLAGS  += -DFRONT_ICE2=$(ESMF_DEP_FRONT) -DTYPE_ICE2=\"$(TYPE_ICE2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_LND1
    include $(MKFILE_RPM_LND1)
    RPM_CPPFLAGS  += -DFRONT_LND1=$(ESMF_DEP_FRONT) -DTYPE_LND1=\"$(TYPE_LND1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_RPM_LND2
    include $(MKFILE_RPM_LND2)
    RPM_CPPFLAGS  += -DFRONT_LND2=$(ESMF_DEP_FRONT) -DTYPE_LND2=\"$(TYPE_LND2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
endif
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# ADM Settings
#-------------------------------------------------------------------------------
ifeq (coamps_adm,$(MAKECMDGOALS))
  DEP_CMPL_OBJS += $(TOP_CMPL_OBJS)
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_ADM_App.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_ADM_Drv.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_Dutil.o
  DEP_LINK_OBJS += $(TOP_LINK_OBJS)
  ifdef MKFILE_ADM_ATM1
    include $(MKFILE_ADM_ATM1)
    ADM_CPPFLAGS  += -DFRONT_ATM1=$(ESMF_DEP_FRONT) -DTYPE_ATM1=\"$(TYPE_ATM1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_ATM2
    include $(MKFILE_ADM_ATM2)
    ADM_CPPFLAGS  += -DFRONT_ATM2=$(ESMF_DEP_FRONT) -DTYPE_ATM2=\"$(TYPE_ATM2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_OCN1
    include $(MKFILE_ADM_OCN1)
    ADM_CPPFLAGS  += -DFRONT_OCN1=$(ESMF_DEP_FRONT) -DTYPE_OCN1=\"$(TYPE_OCN1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_OCN2
    include $(MKFILE_ADM_OCN2)
    ADM_CPPFLAGS  += -DFRONT_OCN2=$(ESMF_DEP_FRONT) -DTYPE_OCN2=\"$(TYPE_OCN2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_WAV1
    include $(MKFILE_ADM_WAV1)
    ADM_CPPFLAGS  += -DFRONT_WAV1=$(ESMF_DEP_FRONT) -DTYPE_WAV1=\"$(TYPE_WAV1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_WAV2
    include $(MKFILE_ADM_WAV2)
    ADM_CPPFLAGS  += -DFRONT_WAV2=$(ESMF_DEP_FRONT) -DTYPE_WAV2=\"$(TYPE_WAV2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_ICE1
    include $(MKFILE_ADM_ICE1)
    ADM_CPPFLAGS  += -DFRONT_ICE1=$(ESMF_DEP_FRONT) -DTYPE_ICE1=\"$(TYPE_ICE1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_ICE2
    include $(MKFILE_ADM_ICE2)
    ADM_CPPFLAGS  += -DFRONT_ICE2=$(ESMF_DEP_FRONT) -DTYPE_ICE2=\"$(TYPE_ICE2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_LND1
    include $(MKFILE_ADM_LND1)
    ADM_CPPFLAGS  += -DFRONT_LND1=$(ESMF_DEP_FRONT) -DTYPE_LND1=\"$(TYPE_LND1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  ifdef MKFILE_ADM_LND2
    include $(MKFILE_ADM_LND2)
    ADM_CPPFLAGS  += -DFRONT_LND2=$(ESMF_DEP_FRONT) -DTYPE_LND2=\"$(TYPE_LND2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
endif
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# VDA Settings
#-------------------------------------------------------------------------------
ifeq (coamps_vda,$(MAKECMDGOALS))
  DEP_CMPL_OBJS += $(TOP_CMPL_OBJS)
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_VDA_App.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_RPM_Drv.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_ADM_Drv.o
  DEP_LINK_OBJS += $(OBJDIR)/COAMPS_Dutil.o
  DEP_LINK_OBJS += $(TOP_LINK_OBJS)
  ifdef MKFILE_RPM_ATM1
  ifdef MKFILE_ADM_ATM1
    include $(MKFILE_RPM_ATM1)
    RPM_CPPFLAGS  += -DFRONT_ATM1=$(ESMF_DEP_FRONT) -DTYPE_ATM1=\"$(TYPE_ATM1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_ATM1)
    ADM_CPPFLAGS  += -DFRONT_ATM1=$(ESMF_DEP_FRONT) -DTYPE_ATM1=\"$(TYPE_ATM1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_ATM2
  ifdef MKFILE_ADM_ATM2
    include $(MKFILE_RPM_ATM2)
    RPM_CPPFLAGS  += -DFRONT_ATM2=$(ESMF_DEP_FRONT) -DTYPE_ATM2=\"$(TYPE_ATM2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_ATM2)
    ADM_CPPFLAGS  += -DFRONT_ATM2=$(ESMF_DEP_FRONT) -DTYPE_ATM2=\"$(TYPE_ATM2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_OCN1
  ifdef MKFILE_ADM_OCN1
    include $(MKFILE_RPM_OCN1)
    RPM_CPPFLAGS  += -DFRONT_OCN1=$(ESMF_DEP_FRONT) -DTYPE_OCN1=\"$(TYPE_OCN1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_OCN1)
    ADM_CPPFLAGS  += -DFRONT_OCN1=$(ESMF_DEP_FRONT) -DTYPE_OCN1=\"$(TYPE_OCN1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_OCN2
  ifdef MKFILE_ADM_OCN2
    include $(MKFILE_RPM_OCN2)
    RPM_CPPFLAGS  += -DFRONT_OCN2=$(ESMF_DEP_FRONT) -DTYPE_OCN2=\"$(TYPE_OCN2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_OCN2)
    ADM_CPPFLAGS  += -DFRONT_OCN2=$(ESMF_DEP_FRONT) -DTYPE_OCN2=\"$(TYPE_OCN2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_WAV1
  ifdef MKFILE_ADM_WAV1
    include $(MKFILE_RPM_WAV1)
    RPM_CPPFLAGS  += -DFRONT_WAV1=$(ESMF_DEP_FRONT) -DTYPE_WAV1=\"$(TYPE_WAV1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_WAV1)
    ADM_CPPFLAGS  += -DFRONT_WAV1=$(ESMF_DEP_FRONT) -DTYPE_WAV1=\"$(TYPE_WAV1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_WAV2
  ifdef MKFILE_ADM_WAV2
    include $(MKFILE_RPM_WAV2)
    RPM_CPPFLAGS  += -DFRONT_WAV2=$(ESMF_DEP_FRONT) -DTYPE_WAV2=\"$(TYPE_WAV2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_WAV2)
    ADM_CPPFLAGS  += -DFRONT_WAV2=$(ESMF_DEP_FRONT) -DTYPE_WAV2=\"$(TYPE_WAV2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_ICE1
  ifdef MKFILE_ADM_ICE1
    include $(MKFILE_RPM_ICE1)
    RPM_CPPFLAGS  += -DFRONT_ICE1=$(ESMF_DEP_FRONT) -DTYPE_ICE1=\"$(TYPE_ICE1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_ICE1)
    ADM_CPPFLAGS  += -DFRONT_ICE1=$(ESMF_DEP_FRONT) -DTYPE_ICE1=\"$(TYPE_ICE1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_ICE2
  ifdef MKFILE_ADM_ICE2
    include $(MKFILE_RPM_ICE2)
    RPM_CPPFLAGS  += -DFRONT_ICE2=$(ESMF_DEP_FRONT) -DTYPE_ICE2=\"$(TYPE_ICE2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_ICE2)
    ADM_CPPFLAGS  += -DFRONT_ICE2=$(ESMF_DEP_FRONT) -DTYPE_ICE2=\"$(TYPE_ICE2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_LND1
  ifdef MKFILE_ADM_LND1
    include $(MKFILE_RPM_LND1)
    RPM_CPPFLAGS  += -DFRONT_LND1=$(ESMF_DEP_FRONT) -DTYPE_LND1=\"$(TYPE_LND1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_LND1)
    ADM_CPPFLAGS  += -DFRONT_LND1=$(ESMF_DEP_FRONT) -DTYPE_LND1=\"$(TYPE_LND1)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
  ifdef MKFILE_RPM_LND2
  ifdef MKFILE_ADM_LND2
    include $(MKFILE_RPM_LND2)
    RPM_CPPFLAGS  += -DFRONT_LND2=$(ESMF_DEP_FRONT) -DTYPE_LND2=\"$(TYPE_LND2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
    include $(MKFILE_ADM_LND2)
    ADM_CPPFLAGS  += -DFRONT_LND2=$(ESMF_DEP_FRONT) -DTYPE_LND2=\"$(TYPE_LND2)\"
    DEP_INCPATH   += $(addprefix -I, $(ESMF_DEP_INCPATH))
    DEP_CMPL_OBJS += $(ESMF_DEP_CMPL_OBJS)
    DEP_LINK_OBJS += $(ESMF_DEP_LINK_OBJS)
  endif
  endif
endif
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# object dependencies
#-------------------------------------------------------------------------------
$(OBJDIR)/COAMPS_Dutil.o : COAMPS_Macros.h
$(OBJDIR)/COAMPS_Futil.o : COAMPS_Macros.h $(OBJDIR)/COAMPS_Gsrumd.o
$(OBJDIR)/COAMPS_Gutil.o : COAMPS_Macros.h $(OBJDIR)/COAMPS_Gdem.o
$(OBJDIR)/COAMPS_Gdem.o  : COAMPS_Macros.h

$(OBJDIR)/COAMPS_Connector.o : COAMPS_Macros.h $(OBJDIR)/COAMPS_Futil.o
$(OBJDIR)/COAMPS_Mdata.o     : COAMPS_Macros.h $(OBJDIR)/COAMPS_Futil.o $(OBJDIR)/COAMPS_Gutil.o
$(OBJDIR)/COAMPS_Mdumb.o     : COAMPS_Macros.h

$(OBJDIR)/COAMPS_BKM_App.o : COAMPS_Macros.h $(DEP_CMPL_OBJS)

$(OBJDIR)/COAMPS_NLM_App.o : COAMPS_Macros.h $(OBJDIR)/COAMPS_NLM_Drv.o $(OBJDIR)/COAMPS_Dutil.o
$(OBJDIR)/COAMPS_RPM_App.o : COAMPS_Macros.h $(OBJDIR)/COAMPS_RPM_Drv.o $(OBJDIR)/COAMPS_Dutil.o
$(OBJDIR)/COAMPS_ADM_App.o : COAMPS_Macros.h $(OBJDIR)/COAMPS_ADM_Drv.o $(OBJDIR)/COAMPS_Dutil.o
$(OBJDIR)/COAMPS_VDA_App.o : COAMPS_Macros.h $(OBJDIR)/COAMPS_RPM_Drv.o $(OBJDIR)/COAMPS_ADM_Drv.o $(OBJDIR)/COAMPS_Dutil.o

$(OBJDIR)/COAMPS_NLM_Drv.o : COAMPS_Macros.h $(DEP_CMPL_OBJS)
$(OBJDIR)/COAMPS_RPM_Drv.o : COAMPS_Macros.h $(DEP_CMPL_OBJS)
$(OBJDIR)/COAMPS_ADM_Drv.o : COAMPS_Macros.h $(DEP_CMPL_OBJS)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# build targets
#-------------------------------------------------------------------------------
coamps_bkm : EXE := $(BINDIR)/coamps_bkm.exe
coamps_bkm : $(OBJDIR)/COAMPS_BKM_App.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) -o $(EXE) $(DEP_LINK_OBJS) \
		$(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)
	@$(MAKE) modmv

coamps_nlm : EXE := $(BINDIR)/coamps_nlm.exe
coamps_nlm : $(OBJDIR)/COAMPS_NLM_App.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) -o $(EXE) $(DEP_LINK_OBJS) \
		$(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)
	@$(MAKE) modmv

coamps_rpm : EXE := $(BINDIR)/coamps_rpm.exe
coamps_rpm : $(OBJDIR)/COAMPS_RPM_App.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) -o $(EXE) $(DEP_LINK_OBJS) \
		$(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)
	@$(MAKE) modmv

coamps_adm : EXE := $(BINDIR)/coamps_adm.exe
coamps_adm : $(OBJDIR)/COAMPS_ADM_App.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) -o $(EXE) $(DEP_LINK_OBJS) \
		$(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)
	@$(MAKE) modmv

coamps_vda : EXE := $(BINDIR)/coamps_vda.exe
coamps_vda : $(OBJDIR)/COAMPS_VDA_App.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) -o $(EXE) $(DEP_LINK_OBJS) \
		$(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)
	@$(MAKE) modmv
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# object compile rules
#-------------------------------------------------------------------------------
.SUFFIXES: .F90

$(OBJDIR)/%.o : %.F90 | $(BINDIR) $(OBJDIR) $(MODDIR)
	$(ESMF_F90COMPILER) -c -o $@ \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $<

$(OBJDIR)/COAMPS_BKM_App.o : COAMPS_BkgdIsRqrd.F90
	$(ESMF_F90COMPILER) -c -o $@ \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $<

$(OBJDIR)/COAMPS_NLM_App.o : COAMPS_AppDriver.F90
	$(ESMF_F90COMPILER) -c -o $@ -DFRONT_DRM=COAMPS_NLM \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) $(DEP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $<

$(OBJDIR)/COAMPS_RPM_App.o : COAMPS_AppDriver.F90
	$(ESMF_F90COMPILER) -c -o $@ -DFRONT_DRM=COAMPS_RPM \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) $(DEP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $<

$(OBJDIR)/COAMPS_ADM_App.o : COAMPS_AppDriver.F90
	$(ESMF_F90COMPILER) -c -o $@ -DFRONT_DRM=COAMPS_ADM \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) $(DEP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $<

$(OBJDIR)/COAMPS_VDA_App.o : COAMPS_VdaDriver.F90
	$(ESMF_F90COMPILER) -c -o $@ -DFRONT_RPM=COAMPS_RPM -DFRONT_ADM=COAMPS_ADM \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) $(DEP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $<

$(OBJDIR)/COAMPS_NLM_Drv.o : COAMPS_Driver.F90
	$(ESMF_F90COMPILER) -c -o $@ -DFRONT_DRM=COAMPS_NLM \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) $(DEP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $(NLM_CPPFLAGS) $<

$(OBJDIR)/COAMPS_RPM_Drv.o : COAMPS_Driver.F90
	$(ESMF_F90COMPILER) -c -o $@ -DFRONT_DRM=COAMPS_RPM \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) $(DEP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $(RPM_CPPFLAGS) $<

$(OBJDIR)/COAMPS_ADM_Drv.o : COAMPS_Driver.F90
	$(ESMF_F90COMPILER) -c -o $@ -DFRONT_DRM=COAMPS_ADM -DADJOINT \
		$(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(TOP_INCPATH) $(DEP_INCPATH) \
		$(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(TOP_CPPFLAGS) $(ADM_CPPFLAGS) $<
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# utility targets
#-------------------------------------------------------------------------------
.PHONY: dust clean distclean modmv run_nlm run_rpm run_adm run_vda

dust:
	$(RM) PET*.ESMF_LogFile
clean : dust
	$(RM) $(OBJDIR) $(MODDIR) *.o *.mod core.*
distclean : clean
	$(RM) $(BINDIR) *.exe

$(BINDIR) $(OBJDIR) $(MODDIR) :
	@mkdir -p $@

modmv :
	@mods=`ls *.mod 2>/dev/null`; \
	if [ "$${mods}" != "" ]; then $(MV) $${mods} $(MODDIR)/.; fi

run_nlm: dust
	mpirun -np $(NPROC) $(BINDIR)/coamps_nlm.exe $(CONFIG)
run_rpm: dust
	mpirun -np $(NPROC) $(BINDIR)/coamps_rpm.exe $(CONFIG)
run_adm: dust
	mpirun -np $(NPROC) $(BINDIR)/coamps_adm.exe $(CONFIG)
run_vda: dust
	mpirun -np $(NPROC) $(BINDIR)/coamps_vda.exe $(CONFIG)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Dummy rules to avoid gnumake trying to remake the actual makefiles.
# This helps to reduce overhead and makes the debug mode (make -d) output
# more readable by cutting down on the remake rule output. 
#-------------------------------------------------------------------------------
Makefile :
	@echo ;
%/esmf.mk :
	@echo ;
#-------------------------------------------------------------------------------
