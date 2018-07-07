#include "settings.h"

module lndWrapper

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndDriver

  implicit none
  
  private

  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(lnd_cmp, rc)
    type(ESMF_GridComp)  :: lnd_cmp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(lnd_cmp, ESMF_METHOD_INITIALIZE, &
      userRoutine=esmf_lnd_ini, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(lnd_cmp, ESMF_METHOD_RUN, &
      userRoutine=esmf_lnd_run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSetEntryPoint(lnd_cmp, ESMF_METHOD_FINALIZE, &
      userRoutine=esmf_lnd_fin, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine SetServices

  subroutine esmf_lnd_ini(lnd_cmp,imp_state,exp_state,clock,rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: lnd_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    type(ESMF_FieldBundle) :: fields

    rc = ESMF_SUCCESS

    ! Initialize Applicaiton
    call lnd_ini(fields=fields,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Store fields in exp_state
    call ESMF_StateAdd(exp_state, fieldbundleList=(/fields/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine esmf_lnd_ini

  !-----------------------------------------------------------------------------

  subroutine esmf_lnd_run(lnd_cmp,imp_state,exp_state,clock,rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: lnd_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    ! Run Application
    call lnd_run(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end subroutine esmf_lnd_run

  !-----------------------------------------------------------------------------

  subroutine esmf_lnd_fin(lnd_cmp,imp_state,exp_state,clock, rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: lnd_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    ! Finalize Application
    call lnd_fin(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end subroutine esmf_lnd_fin

  !-----------------------------------------------------------------------------

end module lndWrapper
