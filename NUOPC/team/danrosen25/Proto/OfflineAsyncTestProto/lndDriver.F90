module lndDriver

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndState, only : state_ini, state_fin, state
  use lndWriter
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
      logkindflag=ESMF_LOGKIND_NONE,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call state_ini(rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call log_ini(lcl_pet_id=state%lcl_pet_id &
      ,gbl_pet_cnt=state%gbl_pet_cnt &
      ,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call log_info(msg="state_ini complete")
  end subroutine lnd_ini

  !-----------------------------------------------------------------------------

  subroutine lnd_run(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS
  
    call gatherWrite(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine lnd_run

  !-----------------------------------------------------------------------------

  subroutine lnd_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_fin(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
  end subroutine lnd_fin

  !-----------------------------------------------------------------------------

end module lndDriver
