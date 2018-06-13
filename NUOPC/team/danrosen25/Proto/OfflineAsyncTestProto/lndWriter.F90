module lndWriter

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndState
  use lndLogger

  implicit none
  
  private

  public asyncWrite
  public asyncParallelWrite
  public gatherWrite
  public parallelWrite
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine asyncWrite(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_info("asyncWrite called")
    call log_info("asyncWrite finished")
  end subroutine

  subroutine asyncParallelWrite(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_info("asyncParallelWrite called")
    call log_info("asyncParallelWrite finished")
  end subroutine

  subroutine gatherWrite(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_info("gatherWrite called")
    call log_info("gatherWrite finished")
  end subroutine

  subroutine parallelWrite(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_info("parallelWrite called")
    call log_info("parallelWrite finished")
  end subroutine

  !-----------------------------------------------------------------------------

end module
