module module_fcst_grid_comp
!
!-----------------------------------------------------------------------
!***  Forecast gridded component.
!-----------------------------------------------------------------------
!
  use ESMF
  use atmos_model_mod,  only: update_atmos_model_fields

  implicit none

  integer :: numLevels, numTracers, numSoilLayers

  type(ESMF_Grid) :: fcstGrid

  private

  public :: SetServices
  public :: fcstGrid
  public :: numLevels, numTracers, numSoilLayers

contains

  subroutine SetServices(fcst_comp, rc)
!
    type(ESMF_GridComp)  :: fcst_comp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_INITIALIZE, &
         userRoutine=fcst_initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_RUN, &
         userRoutine=fcst_run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_FINALIZE, &
         userRoutine=fcst_finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine SetServices

  subroutine fcst_initialize(fcst_comp, importState, exportState, clock, rc)
!
!-----------------------------------------------------------------------
!***  INITIALIZE THE WRITE GRIDDED COMPONENT.
!-----------------------------------------------------------------------
!
    type(esmf_GridComp)                    :: fcst_comp
    type(ESMF_State)                       :: importState, exportState
    type(esmf_Clock)                       :: clock
    integer,intent(out)                    :: rc
!
!***  LOCAL VARIABLES

    fcstGrid = ESMF_GridCreateCubedSphere(tilesize=96, &
      staggerlocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
      name='fcst_grid', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompSet(fcst_comp, grid=fcstGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    numLevels  = 50
    numTracers = 4 + 66
    numSoilLayers = 4

  end subroutine fcst_initialize

  subroutine fcst_run(fcst_comp, importState, exportState, clock, rc)
!
!----------------------------------------------------------------------- 
!***  the run step for the fcst gridded component.  
!----------------------------------------------------------------------- 
!
    type(ESMF_GridComp)        :: fcst_comp
    type(ESMF_State)           :: importState, exportState
    type(ESMF_Clock)           :: clock
    integer,intent(out)        :: rc

    ! local variables
    integer :: localDe, localDeCount
    type(ESMF_Grid) :: grid

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(fcst_comp, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridGet(grid, localDECount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! update coupled fields for chemistry
    do localDe = 0, localDeCount-1
      call update_atmos_model_fields(de=localDe, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    end do

  end subroutine fcst_run

  subroutine fcst_finalize(fcst_comp, importState, exportState,clock,rc)
!
!----------------------------------------------------------------------- 
!***  the run step for the fcst gridded component.  
!----------------------------------------------------------------------- 
!
    type(ESMF_GridComp)        :: fcst_comp
    type(ESMF_State)           :: importState, exportState
    type(ESMF_Clock)           :: clock
    integer,intent(out)        :: rc

    rc = ESMF_SUCCESS

  end subroutine fcst_finalize

end module module_fcst_grid_comp
