module module_wrt_grid_comp

  use ESMF

  implicit none

  private

  public :: SetServices

contains

      subroutine SetServices(wrt_comp, rc)
        type(ESMF_GridComp)  :: wrt_comp
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        call ESMF_GridCompSetEntryPoint(wrt_comp, ESMF_METHOD_INITIALIZE, &
             userRoutine=wrt_initialize, rc=rc)
        if(rc/=0) write(*,*)'Error: write grid comp, initial'
!
        call ESMF_GridCompSetEntryPoint(wrt_comp, ESMF_METHOD_RUN, &
             userRoutine=wrt_run, rc=rc)
        if(rc/=0) write(*,*)'Error: write grid comp, run'
!
        call ESMF_GridCompSetEntryPoint(wrt_comp, ESMF_METHOD_FINALIZE, &
             userRoutine=wrt_finalize, rc=rc)
        if(rc/=0) write(*,*)'Error: write grid comp, run'

      end subroutine SetServices

      subroutine wrt_initialize(wrt_comp, imp_state_write, exp_state_write, clock, rc)
!
!-----------------------------------------------------------------------
!***  INITIALIZE THE WRITE GRIDDED COMPONENT.
!-----------------------------------------------------------------------
!
      type(esmf_GridComp)               :: wrt_comp
      type(ESMF_State)                  :: imp_state_write, exp_state_write
      type(esmf_Clock)                  :: clock
      integer,intent(out)               :: rc

      ! -- local variables
      type(ESMF_VM) :: vm
      integer :: localPet, petCount

      ! -- begin
      rc = ESMF_SUCCESS
      print *,'wrt_initialize...'

      call ESMF_GridCompGet(wrt_comp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      write(6,'("wrt_initialize: up on PET ",i0,"/",i0)') localPet, petCount

      end subroutine wrt_initialize

      subroutine wrt_run(wrt_comp, imp_state_write, exp_state_write, clock, rc)
!
!-----------------------------------------------------------------------
!***  INITIALIZE THE WRITE GRIDDED COMPONENT.
!-----------------------------------------------------------------------
!
      type(esmf_GridComp)               :: wrt_comp
      type(ESMF_State)                  :: imp_state_write, exp_state_write
      type(esmf_Clock)                  :: clock
      integer,intent(out)               :: rc

      rc = ESMF_SUCCESS
      print *,'wrt_run ...'

      end subroutine wrt_run

      subroutine wrt_finalize(wrt_comp, imp_state_write, exp_state_write, clock, rc)
!
!-----------------------------------------------------------------------
!***  INITIALIZE THE WRITE GRIDDED COMPONENT.
!-----------------------------------------------------------------------
!
      type(esmf_GridComp)               :: wrt_comp
      type(ESMF_State)                  :: imp_state_write, exp_state_write
      type(esmf_Clock)                  :: clock
      integer,intent(out)               :: rc

      rc = ESMF_SUCCESS
      print *,'wrt_finalize ...'

      end subroutine wrt_finalize

end module module_wrt_grid_comp
