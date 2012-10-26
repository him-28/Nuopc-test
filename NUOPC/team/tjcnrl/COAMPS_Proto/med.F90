module MED

  !-----------------------------------------------------------------------------
  ! MED Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, only: &
    model_routine_SS    => routine_SetServices, &
    model_label_Advance => label_Advance
  
  implicit none
  
  private
  
  public SetServices

  integer :: numImport
  character(ESMF_MAXSTR), allocatable :: impStdName(:)
  character(ESMF_MAXSTR), allocatable :: impFldName(:)
  integer :: numExport
  character(ESMF_MAXSTR), allocatable :: expStdName(:)
  character(ESMF_MAXSTR), allocatable :: expFldName(:)
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call model_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for finalize method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=model_label_Advance, &
      userRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    integer :: stat
    integer :: i
    character(ESMF_MAXSTR) :: msg
    
    rc = ESMF_SUCCESS

    ! importable fields
    numImport = 15
    allocate(impStdName(numImport), impFldName(numImport), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of import field name arrays failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    impStdName( 1) = "eastward_wind_at_model_lowest_level"
    impFldName( 1) = "wind_u"
    impStdName( 2) = "northward_wind_at_model_lowest_level"
    impFldName( 2) = "wind_v"
    impStdName( 3) = "air_temperature_at_model_lowest_level"
    impFldName( 3) = "air_temp"
    impStdName( 4) = "surface_eastward_sea_water_velocity"
    impFldName( 4) = "ssc_u"
    impStdName( 5) = "surface_northward_sea_water_velocity"
    impFldName( 5) = "ssc_v"
    impStdName( 6) = "sea_surface_temperature"
    impFldName( 6) = "sst"
    impStdName( 7) = "surface_eastward_wind_to_wave_stress"
    impFldName( 7) = "tau_atm_wave_u"
    impStdName( 8) = "surface_northward_wind_to_wave_stress"
    impFldName( 8) = "tau_atm_wave_v"
    impStdName( 9) = "surface_eastward_wave_to_ocean_stress"
    impFldName( 9) = "tau_ocn_wave_u"
    impStdName(10) = "surface_northward_wave_to_ocean_stress"
    impFldName(10) = "tau_ocn_wave_v"
    impStdName(11) = "sea_ice_eastward_drift_velocity"
    impFldName(11) = "ice_drift_u"
    impStdName(12) = "sea_ice_northward_drift_velocity"
    impFldName(12) = "ice_drift_v"
    impStdName(13) = "sea_ice_concentration"
    impFldName(13) = "ice_conc"
    impStdName(14) = "sea_ice_thickness"
    impFldName(14) = "ice_thick"
    impStdName(15) = "sea_ice_temperature"
    impFldName(15) = "ice_temp"
    do i = 1,numImport
      call NUOPC_StateAdvertiseField(importState, &
        StandardName=trim(impStdName(i)), name=trim(impFldName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) then
        write(msg,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i, &
          ', '//trim(impStdName(i))//', '//trim(impFldName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! exportable fields
    numExport = 13
    allocate(expStdName(numExport), expFldName(numExport), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of export field name arrays failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out
    expStdName( 1) = "eastward_wind_at_10m_height"
    expFldName( 1) = "wind_u"
    expStdName( 2) = "northward_wind_at_10m_height"
    expFldName( 2) = "wind_v"
    expStdName( 3) = "surface_eastward_sea_water_velocity"
    expFldName( 3) = "ssc_u"
    expStdName( 4) = "surface_northward_sea_water_velocity"
    expFldName( 4) = "ssc_v"
    expStdName( 5) = "air_sea_temperature_difference"
    expFldName( 5) = "ast"
    expStdName( 6) = "surface_downward_eastward_stress"
    expFldName( 6) = "tau_u"
    expStdName( 7) = "surface_downward_northward_stress"
    expFldName( 7) = "tau_v"
    expStdName( 8) = "sea_surface_downward_eastward_stress"
    expFldName( 8) = "tau_atm_ocn_u"
    expStdName( 9) = "sea_surface_downward_northward_stress"
    expFldName( 9) = "tau_atm_ocn_v"
    expStdName(10) = "sea_ice_surface_downward_eastward_stress"
    expFldName(10) = "tau_atm_ice_u"
    expStdName(11) = "sea_ice_surface_downward_northward_stress"
    expFldName(11) = "tau_atm_ice_v"
    expStdName(12) = "sea_ice_basal_upward_eastward_stress"
    expFldName(12) = "tau_ocn_ice_u"
    expStdName(13) = "sea_ice_basal_upward_northward_stress"
    expFldName(13) = "tau_ocn_ice_v"
    do i = 1,numExport
      call NUOPC_StateAdvertiseField(exportState, &
        StandardName=trim(expStdName(i)), name=trim(expFldName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) then
        write(msg,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i, &
          ', '//trim(impStdName(i))//', '//trim(impFldName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    integer                 :: i
    
    rc = ESMF_SUCCESS
    
    ! create a Grid object for Fields
    gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
      100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 20, 200, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! realize import fields
    do i = 1,numImport
      field = ESMF_FieldCreate(name=trim(impFldName(i)), grid=gridIn, &
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
    enddo

    ! realize export fields
    do i = 1,numExport
      field = ESMF_FieldCreate(name=trim(expFldName(i)), grid=gridIn, &
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
    enddo

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call NUOPC_ClockPrintCurrTime(clock, &
      "-------->MED Advance() mediating for: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_ClockPrintStopTime(clock, &
      "----------------> model time step to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer :: stat

    rc = ESMF_SUCCESS

    ! deallocate import field name arrays
    deallocate(impStdName, impFldName, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of import field name arrays failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

    ! deallocate export field name arrays
    deallocate(expStdName, expFldName, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of export field name arrays failed.", &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return  ! bail out

  end subroutine

end module
