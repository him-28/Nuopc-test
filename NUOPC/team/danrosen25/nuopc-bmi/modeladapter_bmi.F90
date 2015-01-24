module MODELADAPTER_BMI

  !-----------------------------------------------------------------------------
  ! MODEL Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance
  use bmif
  
  implicit none
  
  private
  
  public SetServices
  
  type(BMI_Model) :: bmodel

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
   call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine PrintBMIinfo()

    character(len=item_name_length), pointer        :: invarnames(:)
    character(len=item_name_length), pointer     :: outvarnames(:)
    character(len=component_name_length), pointer    :: compname
    real                            :: start
    real                            :: end
    real                            :: step
    
    call BMI_Get_input_var_names(bmodel,invarnames)
    call BMI_Get_output_var_names(bmodel,outvarnames)
    call BMI_Get_component_name(bmodel,compname)
    call BMI_Get_time_step(bmodel,step)
    call BMI_Get_start_time(bmodel,start)
    call BMI_Get_end_time(bmodel,end)
    !call BMI_Get_grid_shape (bmodel, var_name, shape)
    !call BMI_Get_grid_spacing (self, var_name, spacing)

    print *, "BMI Initialize Complete"
    print *, "     Step: ",step
    print *, "     Start: ",start
    print *, "     End: ",end
    print *, "     Input Variable Names: ",invarnames
    print *, "     Output Variable Names: ",outvarnames
    print *, "     Component Name: ",compname
  end subroutine

  subroutine PrintBMIvarInfo(var_name)
    character (len=item_name_length), intent (in) :: var_name
    integer             :: type
    character (len=10)   :: units ! Assumed length for units string
    integer             :: rank
    integer             :: gtype
    integer, dimension (1:2)  :: gshape   ! Assumed shape for grid shape array
    real, dimension (1:2)     :: gspacing ! Assumed shape for grid spacing array
    real, dimension (1:2)     :: gorigin ! Assumed shape for grid spacing array

    call BMI_Get_var_type (bmodel, var_name, type)
    call BMI_Get_var_units (bmodel, var_name, units)
    call BMI_Get_var_rank (bmodel, var_name, rank)
    call BMI_Get_grid_type (bmodel, var_name, gtype)
    call BMI_Get_grid_shape (bmodel, var_name, gshape)
    call BMI_Get_grid_spacing (bmodel, var_name, gspacing)
    call BMI_Get_grid_origin (bmodel, var_name, gorigin)

    print *,"Variable Info: ",var_name
    print *, "     Type: ", type
    print *, "     Units: ", units
    print *, "     Rank: ", rank
    print *, "     Grid Type: ", gtype
    print *, "     Grid Shape: ", gshape
    print *, "     Grid Spacing: ", gspacing
    print *, "     Grid Origin: ", gorigin
  end subroutine

  subroutine PrintBMIallVarInfo()
    character(len=item_name_length), pointer    :: invarnames(:)
    character(len=item_name_length), pointer    :: outvarnames(:)
    integer                                     :: i

    call BMI_Get_input_var_names(bmodel,invarnames)
    call BMI_Get_output_var_names(bmodel,outvarnames)

    do i=1,SIZE(invarnames)
        call PrintBMIvarInfo(invarnames(i))
    end do
  end subroutine

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)     :: model
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock, brefclock
    type(ESMF_TimeInterval) :: stabilityTimeStep
    integer, intent(out)    :: rc
    real                    :: start, end, step

    rc = ESMF_SUCCESS

    call BMI_Initialize(bmodel,"") ! Initialize BMI without config file
    call PrintBMIinfo() ! Print BMI information after initializing
    call PrintBMIallVarInfo()

    call BMI_Get_time_step(bmodel,step)
    call BMI_Get_start_time(bmodel,start)
    call BMI_Get_end_time(bmodel,end)

    ! Translate startTime and stopTime
    call ESMF_TimeIntervalSet(stabilityTimeStep, s = INT(step), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Enabeling the following macro, i.e. renaming it to WITHIMPORTFIELDS,
    ! will result in a model component that advertise import Field dependencies.
    ! In the single model case, where there isn't another model to satisfy these
    ! dependencies, it is expected to be caught by the compatability checking.
#define WITHIMPORTFIELDS___disable
#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    call NUOPC_StateAdvertiseField(importState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    
    ! exportable field: air_pressure_at_sea_level
    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    
    rc = ESMF_SUCCESS
    
    ! create a Grid object for Fields
    gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
      100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 10, 100, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    real                          :: bmi_time

    call BMI_Update(bmodel) ! Update BMI
    call BMI_Get_current_time(bmodel,bmi_time)
    print *,"BMI_Update Complete - Time: ", bmi_time

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(model, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.
    
    call NUOPC_ClockPrintCurrTime(clock, &
      "------>Advancing MODEL from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_ClockPrintStopTime(clock, &
      "--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

end module
