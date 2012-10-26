#define FILENAME "med.F90"

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
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! set entry point for finalize method
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=model_label_Advance, &
      userRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR) :: msg
    integer :: stat
    integer :: i
    
    rc = ESMF_SUCCESS

    ! importable fields
    numImport = 15
    allocate(impStdName(numImport), impFldName(numImport), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of import field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    impStdName( 1) = "eastward_wind_at_10m_height"
    impStdName( 2) = "northward_wind_at_10m_height"
    impStdName( 3) = "air_temperature_at_2m_height"
    impStdName( 4) = "surface_eastward_sea_water_velocity"
    impStdName( 5) = "surface_northward_sea_water_velocity"
    impStdName( 6) = "sea_surface_temperature"
    impStdName( 7) = "surface_eastward_wind_to_wave_stress"
    impStdName( 8) = "surface_northward_wind_to_wave_stress"
    impStdName( 9) = "surface_eastward_wave_to_ocean_stress"
    impStdName(10) = "surface_northward_wave_to_ocean_stress"
    impStdName(11) = "sea_ice_eastward_drift_velocity"
    impStdName(12) = "sea_ice_northward_drift_velocity"
    impStdName(13) = "sea_ice_concentration"
    impStdName(14) = "sea_ice_thickness"
    impStdName(15) = "sea_ice_temperature"
    do i = 1,numImport
      call NUOPC_FieldDictionaryGetEntry(trim(impStdName(i)), &
        defaultShortName=impFldName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_FieldDictionaryGetEntry: ',i,', '//trim(impStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call NUOPC_StateAdvertiseField(importState, &
        StandardName=trim(impStdName(i)), name=trim(impFldName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i,', '//trim(impStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! exportable fields
    numExport = 13
    allocate(expStdName(numExport), expFldName(numExport), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of export field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    expStdName( 1) = "eastward_wind_at_10m_height"
    expStdName( 2) = "northward_wind_at_10m_height"
    expStdName( 3) = "air_sea_temperature_difference"
    expStdName( 4) = "surface_eastward_sea_water_velocity"
    expStdName( 5) = "surface_northward_sea_water_velocity"
    expStdName( 6) = "surface_downward_eastward_stress"
    expStdName( 7) = "surface_downward_northward_stress"
    expStdName( 8) = "sea_surface_downward_eastward_stress"
    expStdName( 9) = "sea_surface_downward_northward_stress"
    expStdName(10) = "sea_ice_surface_downward_eastward_stress"
    expStdName(11) = "sea_ice_surface_downward_northward_stress"
    expStdName(12) = "sea_ice_basal_upward_eastward_stress"
    expStdName(13) = "sea_ice_basal_upward_northward_stress"
    do i = 1,numExport
      call NUOPC_FieldDictionaryGetEntry(trim(expStdName(i)), &
        defaultShortName=expFldName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_FieldDictionaryGetEntry: ',i,', '//trim(expStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call NUOPC_StateAdvertiseField(exportState, &
        StandardName=trim(expStdName(i)), name=trim(expFldName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i,', '//trim(expStdName(i))
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
    character(ESMF_MAXSTR) :: msg
    type(ESMF_Field)       :: field
    type(ESMF_Grid)        :: gridIn
    type(ESMF_Grid)        :: gridOut
    integer                :: i

    rc = ESMF_SUCCESS

    ! create a Grid object for Fields
    gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
      100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 20, 200, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! realize import fields
    do i = 1,numImport
      field = ESMF_FieldCreate(name=trim(impFldName(i)), grid=gridIn, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'ESMF_FieldCreate: ',i,', '//trim(impStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_StateRealizeField: ',i,', '//trim(impStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! realize export fields
    do i = 1,numExport
      field = ESMF_FieldCreate(name=trim(expFldName(i)), grid=gridIn, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'ESMF_FieldCreate: ',i,', '//trim(expStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_StateRealizeField: ',i,', '//trim(expStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
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
      line=__LINE__, file=FILENAME)) return  ! bail out

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
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    call NUOPC_ClockPrintStopTime(clock, &
      "----------------> model time step to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

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
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Dellocation of import field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! deallocate export field name arrays
    deallocate(expStdName, expFldName, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Dellocation of export field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

  end subroutine

end module
