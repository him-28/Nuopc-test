#include "settings.h"

module lndClock

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndLogger, only : log_info, log_error, log_warning

  implicit none
  
  private

  public type_clock
  public clock_create
  public clock_check
  public end_check
  public clock_reset
  public clock_restart
  public clock_adv
  public clock_log

  type type_clock
    logical :: initialized  = .false.
    real*4  :: start_time   =  0.0
    real*4  :: end_time     =  0.0
    real*4  :: current_time =  0.0
    real*4  :: time_step    =  0.0
  end type type_clock

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function clock_create(start_time,end_time,time_step,rc)
    ! RESULT
    type(type_clock)    :: clock_create
    ! ARGUMENTS
    real*4,intent(in)   :: start_time
    real*4,intent(in)   :: end_time
    real*4,intent(in)   :: time_step
    integer,intent(out) :: rc

    rc = 0

    if ( start_time < end_time ) then
      if ( time_step > 0 ) then
        clock_create%start_time   = start_time
        clock_create%end_time     = end_time
        clock_create%current_time = start_time
        clock_create%time_step    = time_step
        clock_create%initialized  = .true.
      else
        call clock_reset(clock_create)
        call log_error(msg="clock_create timestep must be positivie")
      endif
    elseif ( start_time > end_time ) then
      if ( time_step < 0 ) then
        clock_create%start_time   = start_time
        clock_create%end_time     = end_time
        clock_create%current_time = start_time
        clock_create%time_step    = time_step
        clock_create%initialized  = .true.
      else
        call clock_reset(clock_create)
        call log_error(msg="clock_create timestep must be negative")
      endif
    else
      clock_create%start_time   = start_time
      clock_create%end_time     = end_time
      clock_create%current_time = start_time
      clock_create%time_step    = time_step
      clock_create%initialized  = .true.
    endif
  end function clock_create

  !-----------------------------------------------------------------------------

  function clock_check(clock,rc)
    ! RESULT
    logical :: clock_check
    ! ARGUMENTS
    type(type_clock),intent(in) :: clock
    integer,intent(out)         :: rc

    rc = 0

    if ( clock%initialized ) then
      clock_check = .true.
    else
      clock_check = .false.
    endif
  end function clock_check

  !-----------------------------------------------------------------------------

  function end_check(clock)
    ! RESULT
    logical :: end_check
    ! ARGUMENTS
    type(type_clock),intent(in) :: clock

    if ( clock%time_step >= 0 ) then
      if ( clock%current_time >= clock%end_time ) then
        end_check = .true.
      else
        end_check = .false.
      endif
    else
      if ( clock%current_time < clock%end_time ) then
        end_check = .true.
      else
        end_check = .false.
      endif
    endif
  end function end_check

  !-----------------------------------------------------------------------------

  subroutine clock_reset(clock)
    ! ARGUMENTS
    type(type_clock),intent(inout) :: clock

    clock%start_time   = 0.0
    clock%end_time     = 0.0
    clock%current_time = 0.0
    clock%time_step    = 0.0
    clock%initialized  = .false.
  end subroutine clock_reset

  !-----------------------------------------------------------------------------

  subroutine clock_restart(clock,rc)
    ! ARGUMENTS
    type(type_clock),intent(inout) :: clock
    integer,intent(out)            :: rc

    rc = 0

    clock%current_time = clock%start_time
  end subroutine clock_restart

  !-----------------------------------------------------------------------------

  subroutine clock_adv(clock,rc)
    ! ARGUMENTS
    type(type_clock),intent(inout) :: clock
    integer,intent(out)            :: rc

    rc = 0

    clock%current_time = clock%current_time + clock%time_step
  end subroutine clock_adv

  !-----------------------------------------------------------------------------

  subroutine clock_log(clock,msg,rc)
    ! ARGUMENTS
    type(type_clock),intent(in) :: clock
    character(len=*),intent(in) :: msg
    integer,intent(out)         :: rc
    ! LOCAL VARIABLES
    character(len=10)           :: currTimeStr

    rc = 0

    write(currTimeStr,"(F10.1)") clock%current_time
    call log_info(TRIM(currTimeStr)//" "//msg)
  end subroutine clock_log

  !-----------------------------------------------------------------------------

end module lndClock
