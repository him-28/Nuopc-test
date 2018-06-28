#include "settings.h"

module lndDriver

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndState,  only : state_ini, state_fin, state_log, state
  use lndFields, only : fieldBundle_fill
  use lndWriter, only : writer_ini, writer_fin, writer_run, writer_log
  use lndClock,  only : clock_adv, clock_log_curr, clock_end_check
  use lndLogger, only : log_ini, log_fin, log_info, log_warning, log_error

  implicit none
  
  private

  public lnd_ini
  public lnd_run
  public lnd_fin
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine lnd_ini(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN,&
      logkindflag=DEFAULT_LOGKIND,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
   
    call log_ini(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call state_ini(rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call state_log()
    call writer_ini(state%fields, config=state%config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call writer_log(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call log_info(msg="initialized")
  end subroutine lnd_ini

  !-----------------------------------------------------------------------------

  subroutine lnd_run(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    do while ( .NOT. clock_end_check(state%clock) ) 
      call clock_log_curr(state%clock, msg="entered advance loop")
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      ! Insert Computation Here
      state%step = state%step + 1
      call fieldBundle_fill(state%fields, start=state%lcl_min &
        , step=state%step, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call clock_adv(state%clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call clock_log_curr(state%clock, msg="exiting advance loop")
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call writer_run(state%clock%time_current, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end do
  end subroutine lnd_run

  !-----------------------------------------------------------------------------

  subroutine lnd_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_info(msg="finalizing")
    call writer_fin(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call state_fin(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call log_fin(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_Finalize() 
  end subroutine lnd_fin

end module lndDriver
