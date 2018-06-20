#include "settings.h"

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
  public abort_error

  type type_log_state
    logical           :: initialized = .false.
    integer           :: log_unit    = 6
    character(len=16) :: pet_id      = 'PET?'
    logical           :: opt_time    = .false.
    logical           :: opt_flush   = .false.
  end type type_log_state

  type(type_log_state)       :: logState
  integer,parameter          :: maxMsgLen   = 55
  integer,parameter          :: maxMsgLenR4 = maxMsgLen-12
  integer,parameter          :: maxMsgLenI4 = maxMsgLen-12
  integer,parameter          :: maxMsgLenL1 = maxMsgLen-12
  character(len=*),parameter :: logInf   = "('INFO    ',A,' ',A)"
  character(len=*),parameter :: logInfI4 = "('INFO    ',A,' ',A,'=',I11)"
  character(len=*),parameter :: logInfR4 = "('INFO    ',A,' ',A,'=',F11.1)"
  character(len=*),parameter :: logInfL1 = "('INFO    ',A,' ',A,'=',L11)"
  character(len=*),parameter :: logWrn   = "('WARNING ',A,' ',A)"
  character(len=*),parameter :: logErr   = "('ERROR   ',A,' ',A)"

  interface log_info
     module procedure log_info_msg
     module procedure log_info_i4
     module procedure log_info_r4
     module procedure log_info_l1
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine log_ini(flush,rc)
    ! ARGUMENTES
    logical,intent(in),optional :: flush
    integer,intent(out)         :: rc
    ! LOCAL VARIABLES
    type(ESMF_VM)     :: vm
    logical           :: isInit
    logical           :: isCreated
    integer*4         :: lcl_pet_id
    integer*4         :: gbl_pet_cnt
    character(len=10) :: maxFileNumb
    character(len=3)  :: padding
    character(len=32) :: fileName

    call ESMF_VMGetGlobal(vm=vm, rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
      isCreated = .false.
    else
      isCreated = ESMF_VMIsCreated(vm=vm, rc=rc)
      if ( rc /= ESMF_SUCCESS ) then
        isCreated = .false.
      endif
    endif

    if (.NOT.isCreated) then
      call log_error(msg="log_ini ESMF initializtion error")
      call log_reset(rc)
      rc = ESMF_RC_OBJ_INIT
    else
      call ESMF_VMGetCurrent(vm=vm, rc=rc)
      if ( rc /= ESMF_SUCCESS ) then
        lcl_pet_id  = -1
        gbl_pet_cnt = -1
      endif
      call ESMF_VMGet(vm, localPet=lcl_pet_id, petCount=gbl_pet_cnt, rc=rc)
      if ( rc /= ESMF_SUCCESS ) then
        lcl_pet_id  = -1
        gbl_pet_cnt = -1
      endif

      if ( gbl_pet_cnt > 9999999999 .or. gbl_pet_cnt < 1 ) then
        call log_error(msg="log_ini current vm pet count error")
        call log_reset(rc)
        rc = ESMF_RC_ARG_OUTOFRANGE
      else
        if ( present(flush) ) then
          logState%opt_flush = flush
        endif
        write(maxFileNumb,"(I10)") (gbl_pet_cnt-1)
        write(padding,"(I3)") LEN_TRIM(ADJUSTL(maxFileNumb))
        write(logState%pet_id,"(I0."//TRIM(padding)//")") lcl_pet_id
        logState%pet_id = 'PET'//ADJUSTL(logState%pet_id)
        write(fileName,"(A,'.log')") TRIM(logState%pet_id)

        call ESMF_UtilIOUnitGet(unit=logState%log_unit, rc=rc)
        if ( rc /= ESMF_SUCCESS ) then
          call log_error(msg="log_ini could not get io unit")
          call log_reset(rc)
        else
          OPEN(unit=logState%log_unit, file=fileName, iostat=rc)
          if ( rc /= 0 ) then
            call log_error(msg="log_ini file could not be opened")
            call log_reset(rc)
          else
            logState%initialized = .true.
          endif
        endif
      endif
    endif

  end subroutine log_ini

  !-----------------------------------------------------------------------------

  subroutine log_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_flush()
    call log_reset(rc)
  end subroutine log_fin

  !-----------------------------------------------------------------------------

  subroutine log_reset(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    logical :: isOpen

    rc = 0

    if ( logState%initialized ) then
      INQUIRE(unit=logState%log_unit, opened=isOpen, iostat=rc)
      if ( rc /= 0 ) then
        write (*,logErr) TRIM(logState%pet_id),"log_reset inquire"
      else
        if ( isOpen ) then
          call log_flush()
          CLOSE(unit=logState%log_unit, iostat=rc)
          if ( rc /= 0 ) then
            write (*,logErr) TRIM(logState%pet_id),"log_reset close"
          endif
        endif
      endif
    endif
    logState%initialized = .false.
    logState%log_unit    = 6
    logState%pet_id      = 'PET?'
    logState%opt_flush   = .false.
    logState%opt_time    = .false.
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

  subroutine log_info_msg(msg)
    ! ARGUMENTS
    character(len=*),intent(in) :: msg
    ! LOCAL VARIABLES
    integer :: maxLen

    maxLen = MIN(LEN(msg),maxMsgLen)
    if ( logState%initialized ) then
      write (logState%log_unit,logInf) TRIM(logState%pet_id),msg(1:maxLen)
    else
      write (*,logInf) TRIM(logState%pet_id),msg(1:maxLen)
    endif
    if ( logState%opt_flush ) call log_flush()
  end subroutine log_info_msg

  !-----------------------------------------------------------------------------

  subroutine log_info_i4(msg,value)
    ! ARGUMENTS
    character(len=*),intent(in) :: msg
    integer*4,intent(in)        :: value
    ! LOCAL VARIABLES
    integer          :: maxLen

    maxLen = MIN(LEN(msg),maxMsgLenI4)
    if ( logState%initialized ) then
      write (logState%log_unit,logInfI4) TRIM(logState%pet_id) &
        ,msg(1:maxLen), value
    else
      write (*,logInfI4) TRIM(logState%pet_id),msg(1:maxLen),value
    endif
    if ( logState%opt_flush ) call log_flush()
  end subroutine log_info_i4

  !-----------------------------------------------------------------------------

  subroutine log_info_r4(msg,value)
    ! ARGUMENTS
    character(len=*),intent(in) :: msg
    real*4,intent(in)           :: value
    ! LOCAL VARIABLES
    integer          :: maxLen

    maxLen = MIN(LEN(msg),maxMsgLenR4)
    if ( logState%initialized ) then
      write (logState%log_unit,logInfR4) TRIM(logState%pet_id) &
        ,msg(1:maxLen), value
    else
      write (*,logInfR4) TRIM(logState%pet_id),msg(1:maxLen),value
    endif
    if ( logState%opt_flush ) call log_flush()
  end subroutine log_info_r4

  !-----------------------------------------------------------------------------

  subroutine log_info_l1(msg,value)
    ! ARGUMENTS
    character(len=*),intent(in) :: msg
    logical,intent(in)          :: value
    ! LOCAL VARIABLES
    integer          :: maxLen

    maxLen = MIN(LEN(msg),maxMsgLenL1)
    if ( logState%initialized ) then
      write (logState%log_unit,logInfL1) TRIM(logState%pet_id) &
        ,msg(1:maxLen), value
    else
      write (*,logInfL1) TRIM(logState%pet_id),msg(1:maxLen),value
    endif
    if ( logState%opt_flush ) call log_flush()
  end subroutine log_info_l1

  !-----------------------------------------------------------------------------

  subroutine log_warning(msg)
    ! ARGUMENTS
    character(len=*),intent(in) :: msg
    ! LOCAL VARIABLES
    integer          :: maxLen

    maxLen = MIN(LEN(msg),maxMsgLen)
    if ( logState%initialized ) then
      write (logState%log_unit,logWrn) TRIM(logState%pet_id),msg(1:maxLen)
    else
      write (*,logWrn) TRIM(logState%pet_id),msg(1:maxLen)
    endif
    if ( logState%opt_flush ) call log_flush()
  end subroutine log_warning

  !-----------------------------------------------------------------------------

  subroutine log_error(msg)
    ! ARGUMENTS
    character(len=*),intent(in) :: msg
    ! LOCAL VARIABLES
    integer          :: maxLen

    maxLen = MIN(LEN(msg),maxMsgLen)
    if ( logState%initialized ) then
      write (logState%log_unit,logErr) TRIM(logState%pet_id),msg(1:maxLen)
    else
      write (*,logErr) TRIM(logState%pet_id),msg(1:maxLen)
    endif
    if ( logState%opt_flush ) call log_flush()
  end subroutine log_error

  !-----------------------------------------------------------------------------

  subroutine abort_error(msg)
    ! ARGUMENTS
    character(len=*),intent(in) :: msg

    call log_error(msg)
    call log_flush()
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call abort()
  end subroutine abort_error

  !-----------------------------------------------------------------------------

end module
