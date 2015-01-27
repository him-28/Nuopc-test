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



  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)     :: model
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock, compclock
    type(ESMF_Time)         :: reftime, startTime, stopTime
    type(ESMF_TimeInterval) :: stabilityTimeStep, interval
    integer, intent(out)    :: rc
    real                    :: start, end, step
    character(len=10)       :: units
    character(len=item_name_length), pointer    :: invarnames(:), outvarnames(:)
    integer                                     :: i

    rc = ESMF_SUCCESS

    call BMI_Initialize(bmodel,"") ! Initialize BMI without config file
    call BMIAdapter_PrintComponentInfo() ! Print BMI information after initializing
    call BMIAdapter_PrintAllVarInfo()

    call BMI_Get_time_step(bmodel,step)
    call BMI_Get_start_time(bmodel,start)
    call BMI_Get_end_time(bmodel,end)
    call BMI_Get_time_units (bmodel, units)

    if (units .eq. 's') then
      call ESMF_TimeIntervalSet(stabilityTimeStep, s = INT(step), rc=rc)
    else if(units .eq. 'm') then
      call ESMF_TimeIntervalSet(stabilityTimeStep, m = INT(step), rc=rc)
    else
      call ESMF_TimeIntervalSet(stabilityTimeStep, s = INT(step), rc=rc)
    end if
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    compclock = NUOPC_ClockInitialize(clock, stabilityTimeStep, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSetClock(model, externalClock=compclock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call BMIAdapter_AddAllFieldsToDictionary(rc=rc)
    if(ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

#define WITHIMPORTFIELDS_OFF
#ifdef WITHIMPORTFIELDS
    ! importable field(s): Get Input Var Names from BMI

    call BMI_Get_input_var_names(bmodel,invarnames)
    do i=1,SIZE(invarnames)
        call NUOPC_StateAdvertiseField(importState, &
            StandardName=invarnames(i), name="aaaa", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    end do
#endif

    ! exportable field(s): Get Output Var Names from BMI
    call BMI_Get_output_var_names(bmodel,outvarnames)

    do i=1,SIZE(outvarnames)
        call NUOPC_StateAdvertiseField(exportState, &
            StandardName=outvarnames(i), name="bbbb", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    end do

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
    field = ESMF_FieldCreate(name="aaaa", grid=gridIn, &
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
    field = ESMF_FieldCreate(name="bbbb", grid=gridOut, &
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
    
!    call NUOPC_ClockPrintCurrTime(clock, &
!      "------>Advancing MODEL from: ", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
    call NUOPC_ClockPrintStopTime(clock, &
      "------>Advancing MODEL to stop time: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !========================================
  ! Print BMI model information
  !========================================

  subroutine BMIAdapter_PrintComponentInfo()

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

    print *, "BMI Component Info"
    print *, "     Step: ",step
    print *, "     Start: ",start
    print *, "     End: ",end
    print *, "     Input Variable Names: ",invarnames
    print *, "     Output Variable Names: ",outvarnames
    print *, "     Component Name: ",compname
  end subroutine

  subroutine test(units)
    character(len=10), intent(inout) :: units
  end subroutine

  subroutine BMIAdapter_PrintVarInfo(var_name)
    character (len=item_name_length), intent (in) :: var_name
    integer             :: type
    character(len=10)   :: units ! Assumed length for units string
    character (len=10)  :: dict_units ! Assume length for units string
    integer             :: rank
    integer             :: gtype
    integer, dimension (1:2)  :: gshape   ! Assumed shape for grid shape array
    real, dimension (1:2)     :: gspacing ! Assumed shape for grid spacing array
    real, dimension (1:2)     :: gorigin ! Assumed shape for grid spacing array
    logical :: in_dictionary
    integer :: rc

    call BMI_Get_var_type (bmodel, var_name, type)
    call BMI_Get_var_units (bmodel, var_name, units)
    call BMI_Get_var_rank (bmodel, var_name, rank)
    call BMI_Get_grid_type (bmodel, var_name, gtype)
    call BMI_Get_grid_shape (bmodel, var_name, gshape)
    call BMI_Get_grid_spacing (bmodel, var_name, gspacing)
    call BMI_Get_grid_origin (bmodel, var_name, gorigin)

    in_dictionary = NUOPC_FieldDictionaryHasEntry(var_name, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    print *, "     Type: ", type
    print *, "     Units: ", units
    print *, "     Rank: ", rank
    print *, "     Grid Type: ", gtype
    print *, "     Grid Shape: ", gshape
    print *, "     Grid Spacing: ", gspacing
    print *, "     Grid Origin: ", gorigin
    print *, "     In Dictionary: ", in_dictionary

    if (in_dictionary) then
        call NUOPC_FieldDictionaryGetEntry(var_name, canonicalUnits=dict_units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        print *,"     Dictionary Units: ",dict_units
    end if

  end subroutine

  subroutine BMIAdapter_PrintAllVarInfo()
    character(len=item_name_length), pointer    :: invarnames(:), outvarnames(:)
    integer                                     :: i

    call BMI_Get_input_var_names(bmodel,invarnames)
    call BMI_Get_output_var_names(bmodel,outvarnames)

    do i=1,SIZE(invarnames)
        print *,"In Variable Info: ",invarnames(i)
        call BMIAdapter_PrintVarInfo(invarnames(i))
    end do

    do i=1,SIZE(outvarnames)
        print *,"Out Variable Info: ",outvarnames(i)
        call BMIAdapter_PrintVarInfo(outvarnames(i))
    end do

  end subroutine

  !========================================
  ! BMI Defined Fields
  !========================================

  subroutine BMIAdapter_AddFieldToDictionary(var_name, rc)
    character (len=item_name_length), intent (in) :: var_name
    character(len=10)   :: units, dict_units ! Assumed length for units string
    integer                                     :: rc
    logical                                     :: in_dictionary
    character(len=100)                            :: unit_errmsg

    rc = ESMF_SUCCESS
    call BMI_Get_var_units (bmodel, var_name, units)

    in_dictionary = NUOPC_FieldDictionaryHasEntry(var_name, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    if (in_dictionary) then
        call NUOPC_FieldDictionaryGetEntry(var_name, canonicalUnits=dict_units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        if (dict_units .ne. units) then
            rc=ESMF_RC_NOT_VALID
            unit_errmsg = "BMI field units: " // units // "do not match dictionary units: " // dict_units
            if(ESMF_LogFoundError(rcToCheck=rc,msg=unit_errmsg, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out
        end if
    else
        call NUOPC_FieldDictionaryAddEntry(var_name, canonicalUnits=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    end if

  end subroutine

  subroutine BMIAdapter_AddAllFieldsToDictionary(rc)
    character(len=item_name_length), pointer    :: invarnames(:), outvarnames(:)
    integer                                     :: i
    integer, intent(out)                        :: rc

    call BMI_Get_input_var_names(bmodel,invarnames)
    call BMI_Get_output_var_names(bmodel,outvarnames)

    do i=1,SIZE(invarnames)
        call BMIAdapter_AddFieldToDictionary(invarnames(i),rc=rc)
        if(ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out
    end do
    do i=1,SIZE(outvarnames)
        call BMIAdapter_AddFieldToDictionary(outvarnames(i),rc=rc)
        if(ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out
    end do
  end subroutine

end module
