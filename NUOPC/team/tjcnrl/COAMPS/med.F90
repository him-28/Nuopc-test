#define FILENAME "med.F90"

module MED

  !-----------------------------------------------------------------------------
  ! COAMPS Mediator Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, only: &
    model_routine_SS      => routine_SetServices, &
    model_type_IS         => type_InternalState, &
    model_label_IS        => label_InternalState, &
    model_label_Advance   => label_Advance
  use UTIL

  implicit none

  private

  public SetServices

  character (*), parameter :: defaultVerbosity = "low"
  character (*), parameter :: label_InternalState = "MED_InternalState"
  integer, parameter :: maxFields = 40

! Mask codes
  integer, parameter :: MASK_INLAND_WATER =  -1
  integer, parameter :: MASK_WATER        =   0
  integer, parameter :: MASK_LAND         =   1
  integer, parameter :: MASK_FROZEN_WATER =   2
  integer, parameter :: MASK_FROZEN_LAND  =   3

  type type_InternalStateStruct
    logical :: verbose
    integer :: numImport
    integer :: numFgImport
    character(ESMF_MAXSTR), pointer :: impStdName(:)
    character(ESMF_MAXSTR), pointer :: impFldName(:)
    integer :: numExport
    character(ESMF_MAXSTR), pointer :: expStdName(:)
    character(ESMF_MAXSTR), pointer :: expFldName(:)
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type


  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    character(ESMF_MAXSTR)        :: verbosity
    type(type_InternalState)      :: is
    integer                       :: localrc, stat

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! determine verbosity
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=verbosity, &
      defaultValue=defaultVerbosity, convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (trim(verbosity)=="high") then
      verbose = .true.
    else
      verbose = .false.
    endif

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetServices', ESMF_LOGMSG_INFO)

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    is%wrap%verbose = verbose

    ! the NUOPC model component will register the generic methods
    call model_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set entry points for initialize methods
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP2, phase=2, rc=rc)
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

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetServices', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer                       :: i
    character(len=NUOPC_PhaseMapStringLength) :: initPhases(4)

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered InitializeP0', ESMF_LOGMSG_INFO)

    ! define initialization phases
    ! skip over IPDv01p2 because we want connected status set on
    ! import/export fields prior to realization
    initPhases(1) = "IPDv01p1=1"
    initPhases(2) = "IPDv01p3=2"
    initPhases(3) = "IPDv01p4=3"
    initPhases(4) = "IPDv01p5=4"
    call ESMF_AttributeSet(gcomp, &
      name="InitializePhaseMap", valueList=initPhases, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! define importable fields
    allocate(is%wrap%impStdName(maxFields), &
             is%wrap%impFldName(maxFields), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of import field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    i = 0
    i = i+1; is%wrap%impStdName(i) = "eastward_wind_at_10m_height"
    i = i+1; is%wrap%impStdName(i) = "northward_wind_at_10m_height"
    i = i+1; is%wrap%impStdName(i) = "air_temperature_at_2m_height"
    i = i+1; is%wrap%impStdName(i) = "surface_eastward_sea_water_velocity"
    i = i+1; is%wrap%impStdName(i) = "surface_northward_sea_water_velocity"
    i = i+1; is%wrap%impStdName(i) = "sea_surface_temperature"
    i = i+1; is%wrap%impStdName(i) = "surface_eastward_wind_to_wave_stress"
    i = i+1; is%wrap%impStdName(i) = "surface_northward_wind_to_wave_stress"
    i = i+1; is%wrap%impStdName(i) = "surface_eastward_wave_to_ocean_stress"
    i = i+1; is%wrap%impStdName(i) = "surface_northward_wave_to_ocean_stress"
    i = i+1; is%wrap%impStdName(i) = "eastward_stokes_drift_current"
    i = i+1; is%wrap%impStdName(i) = "northward_stokes_drift_current"
    i = i+1; is%wrap%impStdName(i) = "sea_ice_eastward_drift_velocity"
    i = i+1; is%wrap%impStdName(i) = "sea_ice_northward_drift_velocity"
    i = i+1; is%wrap%impStdName(i) = "sea_ice_concentration"
    i = i+1; is%wrap%impStdName(i) = "sea_ice_thickness"
    i = i+1; is%wrap%impStdName(i) = "sea_ice_temperature"
    is%wrap%numFgImport = i
#ifdef USE_MODIFIED_STANDARD_NAMES
    i = i+1; is%wrap%impStdName(i) = "mbg_surface_eastward_sea_water_velocity"
    i = i+1; is%wrap%impStdName(i) = "mbg_surface_northward_sea_water_velocity"
    i = i+1; is%wrap%impStdName(i) = "mbg_sea_surface_temperature"
    i = i+1; is%wrap%impStdName(i) = "mbg_surface_eastward_wind_to_wave_stress"
    i = i+1; is%wrap%impStdName(i) = "mbg_surface_northward_wind_to_wave_stress"
    i = i+1; is%wrap%impStdName(i) = "mbg_surface_eastward_wave_to_ocean_stress"
    i = i+1; is%wrap%impStdName(i) = "mbg_surface_northward_wave_to_ocean_stress"
    i = i+1; is%wrap%impStdName(i) = "mbg_eastward_stokes_drift_current"
    i = i+1; is%wrap%impStdName(i) = "mbg_northward_stokes_drift_current"
#else
    i = i+1; is%wrap%impStdName(i) = "surface_eastward_sea_water_velocity"
    i = i+1; is%wrap%impStdName(i) = "surface_northward_sea_water_velocity"
    i = i+1; is%wrap%impStdName(i) = "sea_surface_temperature"
    i = i+1; is%wrap%impStdName(i) = "surface_eastward_wind_to_wave_stress"
    i = i+1; is%wrap%impStdName(i) = "surface_northward_wind_to_wave_stress"
    i = i+1; is%wrap%impStdName(i) = "surface_eastward_wave_to_ocean_stress"
    i = i+1; is%wrap%impStdName(i) = "surface_northward_wave_to_ocean_stress"
    i = i+1; is%wrap%impStdName(i) = "eastward_stokes_drift_current"
    i = i+1; is%wrap%impStdName(i) = "northward_stokes_drift_current"
#endif
    is%wrap%numImport = i
    do i = 1,is%wrap%numImport
      call NUOPC_FieldDictionaryGetEntry(trim(is%wrap%impStdName(i)), &
        defaultShortName=is%wrap%impFldName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_FieldDictionaryGetEntry: ',i, &
          ', '//trim(is%wrap%impStdName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      if (i.gt.is%wrap%numFgImport) &
        is%wrap%impFldName(i) = "mbg_"//trim(is%wrap%impFldName(i))
    enddo

    ! define exportable fields
    allocate(is%wrap%expStdName(maxFields), &
             is%wrap%expFldName(maxFields), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of export field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    i = 0
    i = i+1; is%wrap%expStdName(i) = "eastward_wind_at_10m_height"
    i = i+1; is%wrap%expStdName(i) = "northward_wind_at_10m_height"
    i = i+1; is%wrap%expStdName(i) = "air_sea_temperature_difference"
    i = i+1; is%wrap%expStdName(i) = "surface_eastward_sea_water_velocity"
    i = i+1; is%wrap%expStdName(i) = "surface_northward_sea_water_velocity"
    i = i+1; is%wrap%expStdName(i) = "sea_surface_temperature"
    i = i+1; is%wrap%expStdName(i) = "surface_downward_eastward_stress"
    i = i+1; is%wrap%expStdName(i) = "surface_downward_northward_stress"
    i = i+1; is%wrap%expStdName(i) = "sea_surface_downward_eastward_stress"
    i = i+1; is%wrap%expStdName(i) = "sea_surface_downward_northward_stress"
    i = i+1; is%wrap%expStdName(i) = "sea_ice_surface_downward_eastward_stress"
    i = i+1; is%wrap%expStdName(i) = "sea_ice_surface_downward_northward_stress"
    i = i+1; is%wrap%expStdName(i) = "sea_ice_basal_upward_eastward_stress"
    i = i+1; is%wrap%expStdName(i) = "sea_ice_basal_upward_northward_stress"
    i = i+1; is%wrap%expStdName(i) = "eastward_stokes_drift_current"
    i = i+1; is%wrap%expStdName(i) = "northward_stokes_drift_current"
    is%wrap%numExport = i
    do i = 1,is%wrap%numExport
      call NUOPC_FieldDictionaryGetEntry(trim(is%wrap%expStdName(i)), &
        defaultShortName=is%wrap%expFldName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_FieldDictionaryGetEntry: ',i, &
          ', '//trim(is%wrap%expStdName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving InitializeP0', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    character(ESMF_MAXSTR)        :: fname
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered InitializeP1', ESMF_LOGMSG_INFO)

    ! advertise importable fields
    do i = 1,is%wrap%numImport
      call NUOPC_StateAdvertiseField(importState, trim(is%wrap%impStdName(i)), &
        name=trim(is%wrap%impFldName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i, &
          ', '//trim(is%wrap%impFldName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! advertise exportable fields
    do i = 1,is%wrap%numExport
      call NUOPC_StateAdvertiseField(exportState, trim(is%wrap%expStdName(i)), &
        name=trim(is%wrap%expFldName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i, &
        ', '//trim(is%wrap%expFldName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving InitializeP1', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    character(ESMF_MAXSTR)        :: fname
    logical                       :: connected
    type(ESMF_Field)              :: field
    type(ESMF_Grid)               :: gridIn
    type(ESMF_Grid)               :: gridOut
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered InitializeP2', ESMF_LOGMSG_INFO)

    ! create a Grid object for Fields
    gridIn = NUOPC_GridCreateSimpleXY(  0._ESMF_KIND_R8,  0._ESMF_KIND_R8, &
      50._ESMF_KIND_R8, 50._ESMF_KIND_R8, 50, 50, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! realize connected import fields (& remove unconnected)
    do i = 1,is%wrap%numImport
      fname = trim(is%wrap%impFldName(i))
      connected = NUOPC_StateIsFieldConnected(importState, trim(fname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_StateIsFieldConnected: ',i,', '//trim(fname)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      if (connected) then
        field = ESMF_FieldCreate(name=trim(fname), grid=gridIn, &
          typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'ESMF_FieldCreate: ',i,', '//trim(fname)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
        call NUOPC_StateRealizeField(importState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'NUOPC_StateRealizeField: ',i,', '//trim(fname)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
      else
        call ESMF_StateRemove(importState, (/trim(fname)/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'ESMF_StateRemove: ',i, &
            ', '//trim(is%wrap%impStdName(i))
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
      endif
    enddo

    ! realize connected export fields (& remove unconnected)
    do i = 1,is%wrap%numExport
      fname = trim(is%wrap%expFldName(i))
      connected = NUOPC_StateIsFieldConnected(exportState, trim(fname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_StateIsFieldConnected: ',i,', '//trim(fname)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      if (connected) then
        field = ESMF_FieldCreate(name=trim(fname), grid=gridOut, &
          typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'ESMF_FieldCreate: ',i,', '//trim(fname)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
        call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'NUOPC_StateRealizeField: ',i,', '//trim(fname)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
      else
        call ESMF_StateRemove(exportState, (/trim(fname)/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'ESMF_StateRemove: ',i,', '//trim(fname)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
      endif
    enddo

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving InitializeP2', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    integer                       :: localPet
    type(ESMF_StateItem_Flag)     :: expItemType
    type(ESMF_Field)              :: expField
    type(ESMF_StateItem_Flag)     :: impFgItemType, impBgItemType
    type(ESMF_Field)              :: impFgField, impBgField

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered ModelAdvance', ESMF_LOGMSG_INFO)

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query the Component for PET info
    call ESMF_GridCompGet(gcomp, localPet=localPet, rc=rc)
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
    
    if (localPet.eq.0) then
      call NUOPC_ClockPrintCurrTime(clock, &
        "-------->"//trim(cname)//" Advance() mediating for: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

      call NUOPC_ClockPrintStopTime(clock, &
        "----------------> model time step to: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

    ! load sst from import to export
    call ESMF_StateGet(exportState, "sst", expItemType, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (expItemType.ne.ESMF_STATEITEM_NOTFOUND) then
      call ESMF_StateGet(exportState, "sst", expField, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_StateGet(importState, "mbg_sst", impBgItemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      if (impBgItemType.ne.ESMF_STATEITEM_NOTFOUND) then
        call ESMF_StateGet(importState, "mbg_sst", impBgField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        call FieldCopyData(impBgField, expField, 9._ESMF_KIND_R8, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif
      call ESMF_StateGet(importState, "sst", impFgItemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      if (impFgItemType.ne.ESMF_STATEITEM_NOTFOUND) then
        call ESMF_StateGet(importState, "sst", impFgField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        call FieldCopyData(impFgField, expField, 5._ESMF_KIND_R8, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      endif
    endif

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving ModelAdvance', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    character(ESMF_MAXSTR),pointer:: stdNameList(:)
    type(ESMF_Field),pointer      :: fieldList(:)
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered Finalize', ESMF_LOGMSG_INFO)

    ! write final export fields
    call NUOPC_StateBuildStdList(exportState, stdNameList, stdFieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (associated(fieldList)) then
    do i = 1,size(fieldList)
      call FieldWrite(gcomp, fieldList(i), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'FieldWrite: ',i,', '//trim(stdNameList(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo
    endif
    if (associated(stdNameList)) deallocate(stdNameList)
    if (associated(fieldList)) deallocate(fieldList)

    ! deallocate import field name arrays
    if (associated(is%wrap%impStdName)) then
      deallocate(is%wrap%impStdName, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of import std name arrays failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif
    if (associated(is%wrap%impFldName)) then
      deallocate(is%wrap%impFldName, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of import fld name arrays failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

    ! deallocate export field name arrays
    if (associated(is%wrap%expStdName)) then
      deallocate(is%wrap%expStdName, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of export std name arrays failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif
    if (associated(is%wrap%expFldName)) then
      deallocate(is%wrap%expFldName, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of export fld name arrays failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

end module
