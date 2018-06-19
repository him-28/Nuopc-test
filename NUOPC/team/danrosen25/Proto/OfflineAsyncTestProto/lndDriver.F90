#include "settings.h"

module lndDriver

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndState, only : state_ini, state_fin, state, field_fill
  use lndWriter, only : write_ini, write_fin, write_state
  use lndClock
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
    ! LOCAL VARIABLES
    type(type_clock) :: clock

    call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN,&
      logkindflag=SETTINGS_LOGKIND,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    clock = clock_create(0.0,10.0,1.0,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call log_ini(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call state_ini(clock,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
#if ASYNC == ON
#if PARALLEL == ON
    call write_ini(0.0,10.0,2.0,async=.true.,parallel=.true.,rc=rc)
#else
    call write_ini(0.0,10.0,2.0,async=.true.,parallel=.false.,rc=rc)
#endif
#else
#if PARALLEL == ON
    call write_ini(0.0,10.0,2.0,async=.false.,parallel=.true.,rc=rc)
#else
    call write_ini(0.0,10.0,2.0,async=.false.,parallel=.false.,rc=rc)
#endif
#endif
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

    do while ( .NOT. end_check(state%clock) ) 
      call clock_log(state%clock, msg="entered advance loop", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      ! Insert Computation Here
      call lnd_adv(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call clock_adv(state%clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call write_state(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call clock_log(state%clock, msg="exiting advance loop", rc=rc)
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
    call write_fin(rc=rc)
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

  !-----------------------------------------------------------------------------

  subroutine lnd_adv(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    state%step = state%step + 1
    call field_fill(state%sfctmp, member=1, step=state%step, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call field_fill(state%infexs, member=2, step=state%step, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT) 

  end subroutine lnd_adv

  !-----------------------------------------------------------------------------

end module lndDriver
