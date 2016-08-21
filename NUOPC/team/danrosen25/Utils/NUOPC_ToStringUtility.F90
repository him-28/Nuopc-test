#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "NUOPC_ToStringUtility.F90"
#define MODNAME "NUOPC_ToStringUtility"

#define VERBOSITY_MIN 0
#define VERBOSITY_MAX 255
#define VERBOSITY_DBG 1023

module NUOPC_ToStringUtility
  use ESMF
  use NUOPC

  implicit none

  private
  public :: NUOPC_ClockToString
  public :: NUOPC_TimeToString

contains

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

  subroutine NUOPC_ClockToString(clock, timestr, rc)
    ! ARGUMENTS
    type(ESMF_Clock)                        :: clock
    character (len=*), intent(out)          :: timestr
    integer, intent(out),optional           :: rc

    ! LOCAL VARIABLES
    type(ESMF_Time)             :: currTime

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize

    ! Get the current time from the clock
    call ESMF_ClockGet(clock=clock,currTime=currTime,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call NUOPC_TimeToString(currTime,timestr,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_TimeToString(time, timestr, rc)
    ! ARGUMENTS
    type(ESMF_Time), intent(in)             :: time
    character (len=*), intent(out)          :: timestr
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    character (len=256)        :: tmpstr = ''
    integer                    :: strlen

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize

    timestr = '' ! clear string

    if (len(timestr) < 19) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OUTOFRANGE,   &
        msg="Time string is too short!", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)
      return
    endif

    call ESMF_TimeGet(time,timeString=tmpstr,rc=rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'

  end subroutine

end module
