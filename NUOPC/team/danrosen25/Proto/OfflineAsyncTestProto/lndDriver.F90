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

  subroutine lnd_ini(vm,fields,rc)
    ! ARGUMENTS
    type(ESMF_VM),intent(in),optional           :: vm
    type(ESMF_FieldBundle),intent(out),optional :: fields
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    type(ESMF_VM) :: gblvm
    logical       :: isCreated

    ! check to see if ESMF is initialized. initialize if not
    call ESMF_VMGetGlobal(vm=gblvm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    isCreated = ESMF_VMIsCreated(vm=gblvm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (.NOT.isCreated) then
      call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN,&
        logkindflag=DEFAULT_LOGKIND,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    call log_ini(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call state_ini(vm=vm,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call state_log()
    call writer_ini(state%fields, config=state%config, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call writer_log(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call clock_log_curr(state%clock, msg="initialized")

    if ( present(fields) ) then
      fields = state%fields
    endif
  end subroutine lnd_ini

  !-----------------------------------------------------------------------------

  subroutine lnd_run(rc)
    ! ARGUMENTS
    integer,intent(out)                        :: rc
    ! LOCAL VARIABLES
    real :: stime
    real :: ftime

    rc = ESMF_SUCCESS

    do while ( .NOT. clock_end_check(state%clock) ) 
      call clock_log_curr(state%clock, msg="entered advance loop")
      ! Mock Computation
      call cpu_time(stime)      
      state%step = state%step + 1
      if ( state%physics_node ) then
        call fieldBundle_fill(state%fields, start=state%lcl_min &
          , step=state%step, msWait=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      call cpu_time(ftime)
      state%cputime = state%cputime + (ftime-stime)
      call clock_adv(state%clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call clock_log_curr(state%clock, msg="exiting advance loop")
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

    call clock_log_curr(state%clock, msg="finalizing")
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
  end subroutine lnd_fin

end module lndDriver
