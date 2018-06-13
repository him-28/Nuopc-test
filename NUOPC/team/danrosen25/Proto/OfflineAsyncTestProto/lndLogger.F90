module lndLogger

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF

  implicit none
  
  private

  public log_ini
  public log_fin
  public log_info
  public log_warning
  public log_error

  type type_log_state
    logical :: initialized = .false.
    integer :: log_unit = -1
    logical :: log_time = .false.
  end type type_log_state

  type(type_log_state)       :: logState
  character(len=*),parameter :: INF = "INFO"
  character(len=*),parameter :: WRN = "WARNING"
  character(len=*),parameter :: ERR = "ERROR"
  character(len=*),parameter :: logFmt = "(' ',A7,' ',A31)"

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine log_ini(lcl_pet_id,gbl_pet_cnt,rc)
    ! ARGUMENTES
    integer,intent(in)  :: lcl_pet_id
    integer,intent(in)  :: gbl_pet_cnt
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    character(len=10)   :: maxFileNumb
    character(len=10)   :: fileNumb
    character(len=3)    :: padding
    character(len=32)   :: fileName

    if ( gbl_pet_cnt > 9999999999 ) then
      call log_reset()
      call log_error("max pet count reached")
      rc = ESMF_RC_ARG_OUTOFRANGE
      return
    endif

    write(maxFileNumb,"(I10)") gbl_pet_cnt
    write(padding,"(I3)") LEN_TRIM(ADJUSTL(maxFileNumb))
    write(fileNumb,"(I0."//TRIM(padding)//")") lcl_pet_id
    write(fileName,"('PET_',A,'.log')") TRIM(fileNumb)

    call ESMF_UtilIOUnitGet(unit=logState%log_unit, rc=rc)
    OPEN(UNIT=logState%log_unit,FILE=fileName)
    logState%initialized = .true.
  end subroutine log_ini

  !-----------------------------------------------------------------------------

  subroutine log_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_flush()
    call log_reset()
  end subroutine log_fin

  !-----------------------------------------------------------------------------

  subroutine log_reset()
    ! LOCAL VARIABLES
    logical isOpen

    if ( logState%initialized ) then
      inquire(unit=logState%log_unit, opened=isOpen)
      if ( isOpen ) then
        close(logState%log_unit)
      endif
    endif
    logState%initialized = .false.
    logState%log_unit = -1
  end subroutine log_reset

  !-----------------------------------------------------------------------------

  subroutine log_flush()
    if ( logState%initialized ) then
      call flush(logState%log_unit)
    else
      call flush(6)
    endif
  end subroutine log_flush

  !-----------------------------------------------------------------------------

  subroutine log_info(msg)
    ! ARGUMENTS
    character(len=*) :: msg

    if ( logState%initialized ) then
      write (logState%log_unit,logFmt) INF,msg
    else
      write (*,logFmt) INF,msg
    endif
  end subroutine log_info

  !-----------------------------------------------------------------------------

  subroutine log_warning(msg)
    ! ARGUMENTS
    character(len=*) :: msg

    if ( logState%initialized ) then
      write (logState%log_unit,logFmt) WRN,msg
    else
      write (*,logFmt) WRN,msg
    endif
  end subroutine log_warning

  !-----------------------------------------------------------------------------

  subroutine log_error(msg)
    ! ARGUMENTS
    character(len=*) :: msg

    if ( logState%initialized ) then
      write (logState%log_unit,logFmt) ERR,msg
    else
      write (*,logFmt) ERR,msg
    endif
  end subroutine log_error

  !-----------------------------------------------------------------------------

end module
