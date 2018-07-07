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

################################################################################

ESMF_TRACE = ON

ifeq ($(ESMF_TRACE),ON)
 ESMF_F90COMPILECPPFLAGS += -DESMF_TRACE
 ESMF_F90LINKOPTS        += $(ESMF_TRACE_STATICLINKOPTS)
 ESMF_F90ESMFLINKLIBS    += $(ESMF_TRACE_STATICLINKLIBS)
endif

################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<


# -----------------------------------------------------------------------------
offlineApp.exe: offlineApp.o lndWrapper.o lndCoupler.o lndDriver.o lndState.o lndFields.o lndLogger.o lndWriter.o lndClock.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)

# module dependencies:
offlineApp.o: lndWrapper.o lndCoupler.o lndDriver.o settings.h
lndWrapper.o: lndDriver.o settings.h
lndDriver.o:  lndState.o lndLogger.o lndWriter.o lndClock.o settings.h
lndClock.o:   lndLogger.o settings.h
lndState.o:   lndLogger.o lndClock.o lndFields.o settings.h
lndWriter.o:  lndState.o lndCoupler.o lndLogger.o lndClock.o settings.h
lndFields.o:  lndLogger.o settings.h
lndCoupler.o: settings.h
lndLogger.o:  settings.h

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f *.nc *.log log.* *.ESMF_LogFile core 
	rm -rf traceout gather parallel esmf
clean:
	rm -f *.exe *.o *.mod
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

edit:
	nedit offlineApp.F90 lndDriver.F90 lndState.F90 lndLogger.F90 lndWriter.F90 &
