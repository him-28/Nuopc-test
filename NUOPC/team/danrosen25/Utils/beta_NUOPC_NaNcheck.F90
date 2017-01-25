#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "beta_NUOPC_NanCheck.F90"
#define MODNAME "beta_NUOPC_NanCheck"

module beta_NUOPC_NanCheck

  implicit none

  private

!  public :: NUOPC_NanCheck

!  interface NUOPC_NanCheck
!    module procedure NUOPC_NanCheck_R43D
!    module procedure NUOPC_NanCheck_R42D
!    module procedure NUOPC_NanCheck_R41D
!    module procedure NUOPC_NanCheck_R4
!    module procedure NUOPC_NanCheck_R83D
!    module procedure NUOPC_NanCheck_R82D
!    module procedure NUOPC_NanCheck_R81D
!    module procedure NUOPC_NanCheck_R8
!  end interface

contains

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------
  logical function NUOPC_NanCheck_R43D(array) result(check)
    ! ARGUMENTS
    real, dimension(:,:,:), intent(in) :: array
    ! LOCAL VARIALBES
    integer :: loopi,loopj,loopl

    check = .FALSE.
    do loopl=lbound(array,3),ubound(array,3)
    do loopj=lbound(array,2),ubound(array,2)
    do loopi=lbound(array,1),ubound(array,1)
      if (array(loopi,loopj,loopl) /= array(loopi,loopj,loopl)) return
    enddo
    enddo
    enddo
    check = .TRUE.
  end function

  logical function NUOPC_NanCheck_R42D(array) result(check)
    ! ARGUMENTS
    real, dimension(:,:), intent(in) :: array
    ! LOCAL VARIALBES
    integer :: loopi,loopj

    check = .FALSE.
    do loopj=lbound(array,2),ubound(array,2)
    do loopi=lbound(array,1),ubound(array,1)
      if (array(loopi,loopj) /= array(loopi,loopj)) return
    enddo
    enddo
    check = .TRUE.
  end function

  logical function NUOPC_NanCheck_R41D(array) result(check)
    ! ARGUMENTS
    real, dimension(:), intent(in) :: array
    ! LOCAL VARIALBES
    integer :: loopi

    check = .FALSE.
    do loopi=lbound(array,1),ubound(array,1)
      if (array(loopi) /= array(loopi)) return
    enddo
    check = .TRUE.
  end function

  logical function NUOPC_NanCheck_R4(scalar) result(check)
    ! ARGUMENTS
    real, intent(in) :: scalar
    ! LOCAL VARIALBES

    check = .FALSE.
    if (scalar /= scalar) return
    check = .TRUE.
  end function

end module
