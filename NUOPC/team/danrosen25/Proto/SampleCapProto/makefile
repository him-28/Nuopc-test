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

all: cap app

cap:
	@echo $(HR)
	@echo "Making Cap"
	make -f makefile.sample_cap nuopc DEBUG=$(DEBUG)

app: 
	@echo $(HR)
	@echo "Making Application"
	make -f makefile.app COMP1_MK=sample_cap.mk DEBUG=$(DEBUG)

dust: dustapp
distclean: distcleanapp cleancap
clean: cleanapp cleancap

dustapp:
	@echo $(HR)
	@echo "Cleaning Output"
	make -f makefile.app dust

distcleanapp:
	@echo $(HR)
	@echo "Cleaning Application and Output"
	make -f makefile.app distclean

cleanapp:
	@echo $(HR)
	@echo "Cleaning Application"
	make -f makefile.app clean
cleancap:
	@echo $(HR)
	@echo "Cleaning Cap"
	make -f makefile.sample_cap nuopcclean

