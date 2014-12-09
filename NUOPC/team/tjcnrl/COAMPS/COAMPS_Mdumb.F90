!-------------------------------------------------------------------------------
! COAMPS Dummy Component (error if invoked)
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_Mdumb.F90"
#include "COAMPS_Macros.h"
module COAMPS_Mdumb
  use ESMF
  implicit none
  private
  public SetServices
  contains
  subroutine SetServices(comp, rc)
    type(ESMF_GridComp)  :: comp
    integer, intent(out) :: rc
    character(ESMF_MAXSTR) :: cname
    call ESMF_GridCompGet(comp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(cname)//': COAMPS dummy component should never be invoked')
    return  ! bail out
  end subroutine
end module
