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
  public clock_ini
  public clock_fin
  public clock_check
  public clock_end_check
  public clock_restart
  public clock_adv
  public clock_log
  public clock_log_curr

  type type_clock
    logical            :: initialized  = .false.
    real(ESMF_KIND_R8) :: time_start   =  0.0
    real(ESMF_KIND_R8) :: time_end     =  0.0
    real(ESMF_KIND_R8) :: time_current =  0.0
    real(ESMF_KIND_R8) :: time_step    =  0.0
  end type type_clock

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine clock_ini(clock,time_start,time_end,time_step,rc)
    ! ARGUMENTS
    type(type_clock),intent(inout) :: clock
    real(ESMF_KIND_R8),intent(in)  :: time_start
    real(ESMF_KIND_R8),intent(in)  :: time_end
    real(ESMF_KIND_R8),intent(in)  :: time_step
    integer,intent(out)            :: rc

    rc = 0

    if ( time_start < time_end ) then
      if ( time_step > 0 ) then
        clock%time_start   = time_start
        clock%time_end     = time_end
        clock%time_current = time_start
        clock%time_step    = time_step
        clock%initialized  = .true.
      else
        call clock_fin(clock)
        call log_error(msg="clock timestep must be positivie")
      endif
    elseif ( time_start > time_end ) then
      if ( time_step < 0 ) then
        clock%time_start   = time_start
        clock%time_end     = time_end
        clock%time_current = time_start
        clock%time_step    = time_step
        clock%initialized  = .true.
      else
        call clock_fin(clock)
        call log_error(msg="clock timestep must be negative")
      endif
    else
      clock%time_start   = time_start
      clock%time_end     = time_end
      clock%time_current = time_start
      clock%time_step    = time_step
      clock%initialized  = .true.
    endif
  end subroutine clock_ini

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

  function clock_end_check(clock)
    ! RESULT
    logical :: clock_end_check
    ! ARGUMENTS
    type(type_clock),intent(in) :: clock

    if ( clock%time_step >= 0 ) then
      if ( clock%time_current >= clock%time_end ) then
        clock_end_check = .true.
      else
        clock_end_check = .false.
      endif
    else
      if ( clock%time_current < clock%time_end ) then
        clock_end_check = .true.
      else
        clock_end_check = .false.
      endif
    endif
  end function clock_end_check

  !-----------------------------------------------------------------------------

  subroutine clock_fin(clock)
    ! ARGUMENTS
    type(type_clock),intent(inout) :: clock

    clock%time_start   = 0.0
    clock%time_end     = 0.0
    clock%time_current = 0.0
    clock%time_step    = 0.0
    clock%initialized  = .false.
  end subroutine clock_fin

  !-----------------------------------------------------------------------------

  subroutine clock_restart(clock,rc)
    ! ARGUMENTS
    type(type_clock),intent(inout) :: clock
    integer,intent(out)            :: rc

    rc = 0

    clock%time_current = clock%time_start
  end subroutine clock_restart

  !-----------------------------------------------------------------------------

  subroutine clock_adv(clock,rc)
    ! ARGUMENTS
    type(type_clock),intent(inout) :: clock
    integer,intent(out)            :: rc

    rc = 0

    clock%time_current = clock%time_current + clock%time_step
  end subroutine clock_adv

  !-----------------------------------------------------------------------------

  subroutine clock_log(clock)
    ! ARGUMENTS
    type(type_clock),intent(in)  :: clock

    call log_info("clock.time_start  ",clock%time_start)
    call log_info("clock.time_end    ",clock%time_end)
    call log_info("clock.time_step   ",clock%time_step)
    call log_info("clock.time_current",clock%time_current)
  end subroutine clock_log

  !-----------------------------------------------------------------------------

  subroutine clock_log_curr(clock,msg)
    ! ARGUMENTS
    type(type_clock),intent(in) :: clock
    character(len=*),intent(in) :: msg

    call log_info(TRIM(msg)//" time_current",clock%time_current)
  end subroutine clock_log_curr

  !-----------------------------------------------------------------------------

end module lndClock
