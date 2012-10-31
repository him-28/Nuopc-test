#define FILENAME "modData.F90"

module MODData

  !-----------------------------------------------------------------------------
  ! Data MODEL Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS      => routine_SetServices, &
    model_type_IS         => type_InternalState, &
    model_label_IS        => label_InternalState, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance

  implicit none

  private

  public SetServices

  character (*), parameter :: defaultVerbosity = "low"
  character (*), parameter :: label_InternalState = "MOD_InternalState"

  type type_InternalStateStruct
    logical :: verbose
    integer :: numImport
    character(ESMF_MAXSTR), pointer :: impStdName(:)
    integer :: numExport
    character(ESMF_MAXSTR), pointer :: expStdName(:)
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
    character(ESMF_MAXSTR)        :: vrbString
    type(type_InternalState)      :: is
    integer                       :: localrc, stat

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! determine verbosity
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=vrbString, &
      defaultValue=defaultVerbosity, convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (trim(vrbString)=="high") then
      verbose = .true.
    else
      verbose = .false.
    endif

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetServices', ESMF_LOGMSG_INFO)

    ! trap unsupported model names
    select case (cname(1:3))
      case ('ATM','OCN','WAV','ICE')
      case default
        call ESMF_LogWrite('Model name not supported: '//cname(1:3), &
          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return  ! bail out
    end select

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
    select case (cname(1:3))
      case ('ATM')
      case ('OCN','WAV','ICE')
        call ESMF_MethodAdd(gcomp, label=model_label_SetClock, &
          userRoutine=SetClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
    end select

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
    select case (cname(1:3))
      case ('ATM')
        is%wrap%numImport = 2
        allocate(is%wrap%impStdName(is%wrap%numImport), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of import field name arrays failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        is%wrap%impStdName( 1) = "surface_downward_eastward_stress"
        is%wrap%impStdName( 2) = "surface_downward_northward_stress"
      case ('OCN')
        is%wrap%numImport = 4
        allocate(is%wrap%impStdName(is%wrap%numImport), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of import field name arrays failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        is%wrap%impStdName( 1) = "sea_surface_downward_eastward_stress"
        is%wrap%impStdName( 2) = "sea_surface_downward_northward_stress"
        is%wrap%impStdName( 3) = "eastward_stokes_drift_current"
        is%wrap%impStdName( 4) = "northward_stokes_drift_current"
      case ('WAV')
        is%wrap%numImport = 5
        allocate(is%wrap%impStdName(is%wrap%numImport), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of import field name arrays failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        is%wrap%impStdName( 1) = "eastward_wind_at_10m_height"
        is%wrap%impStdName( 2) = "northward_wind_at_10m_height"
        is%wrap%impStdName( 3) = "surface_eastward_sea_water_velocity"
        is%wrap%impStdName( 4) = "surface_northward_sea_water_velocity"
        is%wrap%impStdName( 5) = "air_sea_temperature_difference"
      case ('ICE')
        is%wrap%numImport = 4
        allocate(is%wrap%impStdName(is%wrap%numImport), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of import field name arrays failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        is%wrap%impStdName( 1) = "sea_ice_surface_downward_eastward_stress"
        is%wrap%impStdName( 2) = "sea_ice_surface_downward_northward_stress"
        is%wrap%impStdName( 3) = "sea_ice_basal_upward_eastward_stress"
        is%wrap%impStdName( 4) = "sea_ice_basal_upward_northward_stress"
    end select

    ! define exportable fields
    select case (cname(1:3))
      case ('ATM')
        is%wrap%numExport = 3
        allocate(is%wrap%expStdName(is%wrap%numExport), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of export field name arrays failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        is%wrap%expStdName( 1) = "eastward_wind_at_10m_height"
        is%wrap%expStdName( 2) = "northward_wind_at_10m_height"
        is%wrap%expStdName( 3) = "air_temperature_at_2m_height"
      case ('OCN')
        is%wrap%numExport = 3
        allocate(is%wrap%expStdName(is%wrap%numExport), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of export field name arrays failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        is%wrap%expStdName( 1) = "surface_eastward_sea_water_velocity"
        is%wrap%expStdName( 2) = "surface_northward_sea_water_velocity"
        is%wrap%expStdName( 3) = "sea_surface_temperature"
      case ('WAV')
        is%wrap%numExport = 6
        allocate(is%wrap%expStdName(is%wrap%numExport), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of export field name arrays failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        is%wrap%expStdName( 1) = "surface_eastward_wind_to_wave_stress"
        is%wrap%expStdName( 2) = "surface_northward_wind_to_wave_stress"
        is%wrap%expStdName( 3) = "surface_eastward_wave_to_ocean_stress"
        is%wrap%expStdName( 4) = "surface_northward_wave_to_ocean_stress"
        is%wrap%expStdName( 5) = "eastward_stokes_drift_current"
        is%wrap%expStdName( 6) = "northward_stokes_drift_current"
      case ('ICE')
        is%wrap%numExport = 5
        allocate(is%wrap%expStdName(is%wrap%numExport), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of export field name arrays failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        is%wrap%expStdName( 1) = "sea_ice_eastward_drift_velocity"
        is%wrap%expStdName( 2) = "sea_ice_northward_drift_velocity"
        is%wrap%expStdName( 3) = "sea_ice_concentration"
        is%wrap%expStdName( 4) = "sea_ice_thickness"
        is%wrap%expStdName( 5) = "sea_ice_temperature"
    end select

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
      call NUOPC_StateAdvertiseField(importState, &
        StandardName=trim(is%wrap%impStdName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i, &
          ', '//trim(is%wrap%impStdName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! advertise exportable fields
    do i = 1,is%wrap%numExport
      call NUOPC_StateAdvertiseField(exportState, &
        StandardName=trim(is%wrap%expStdName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i, &
        ', '//trim(is%wrap%expStdName(i))
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
    select case (cname(1:3))
      case ('ATM')
        gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
          100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 20, 100, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        gridOut = gridIn ! for now out same as in
      case ('OCN')
        gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
          100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 100, 20, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        gridOut = gridIn ! for now out same as in
      case ('WAV')
        gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
          100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 100, 20, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        gridOut = gridIn ! for now out same as in
      case ('ICE')
        gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
          100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 100, 20, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        gridOut = gridIn ! for now out same as in
    end select

    ! realize all import fields
    do i = 1,is%wrap%numImport
      call NUOPC_FieldDictionaryGetEntry(trim(is%wrap%impStdName(i)), &
        defaultShortName=fname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_FieldDictionaryGetEntry: ',i, &
          ', '//trim(is%wrap%impStdName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      connected = .true.
      if (connected) then
        field = ESMF_FieldCreate(name=trim(fname), grid=gridIn, &
          typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'ESMF_FieldCreate: ',i, &
            ', '//trim(is%wrap%impStdName(i))
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
        call NUOPC_StateRealizeField(importState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'NUOPC_StateRealizeField: ',i, &
            ', '//trim(is%wrap%impStdName(i))
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
      call NUOPC_FieldDictionaryGetEntry(trim(is%wrap%expStdName(i)), &
        defaultShortName=fname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_FieldDictionaryGetEntry: ',i, &
          ', '//trim(is%wrap%expStdName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      connected = NUOPC_StateIsFieldConnected(exportState, trim(fname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,i2,a)') 'NUOPC_StateIsFieldConnected: ',i, &
          ', '//trim(is%wrap%expStdName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      if (connected) then
        field = ESMF_FieldCreate(name=trim(fname), grid=gridIn, &
          typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'ESMF_FieldCreate: ',i, &
            ', '//trim(is%wrap%expStdName(i))
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
        call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'NUOPC_StateRealizeField: ',i, &
            ', '//trim(is%wrap%expStdName(i))
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
      else
        call ESMF_StateRemove(exportState, (/trim(fname)/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msgString,'(a,i2,a)') 'ESMF_StateRemove: ',i, &
            ', '//trim(is%wrap%expStdName(i))
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
      endif
    enddo

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving InitializeP2', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

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
    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetClock', ESMF_LOGMSG_INFO)

    ! query the Component for its clock
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    select case (cname(1:3))
      case ('ATM')
      case ('OCN')
        call ESMF_TimeIntervalSet(stabilityTimeStep, m=3, rc=rc) ! 3 minute steps
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      case ('WAV')
        call ESMF_TimeIntervalSet(stabilityTimeStep, m=3, rc=rc) ! 3 minute steps
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      case ('ICE')
        call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
    end select
    call NUOPC_GridCompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetClock', ESMF_LOGMSG_INFO)

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
    type(ESMF_Time)               :: currTime
    type(ESMF_TimeInterval)       :: timeStep
    integer                       :: localPet

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

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.

    if (localPet.eq.0) then
      call NUOPC_ClockPrintCurrTime(clock, &
        "------>Advancing "//trim(cname)//" from: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out

      call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    
      call NUOPC_TimePrint(currTime + timeStep, &
        "---------------------> to: ", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
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

    ! deallocate import field name arrays
    deallocate(is%wrap%impStdName, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of import field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! deallocate export field name arrays
    deallocate(is%wrap%expStdName, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of export field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

end module
