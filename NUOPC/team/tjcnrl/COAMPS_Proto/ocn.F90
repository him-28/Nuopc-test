#define FILENAME "ocn.F90"

module OCN

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS      => routine_SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance

  implicit none

  private

  public SetServices

  integer :: numImport
  character(ESMF_MAXSTR), allocatable :: impStdName(:)
  character(ESMF_MAXSTR), allocatable :: impFldName(:)
  logical, allocatable                :: impActive(:)
  integer :: numExport
  character(ESMF_MAXSTR), allocatable :: expStdName(:)
  character(ESMF_MAXSTR), allocatable :: expFldName(:)
  logical, allocatable                :: expActive(:)

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
    call ESMF_MethodAdd(gcomp, label=model_label_SetClock, &
      userRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

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
    character(len=NUOPC_PhaseMapStringLength) :: initPhases(4)

    rc = ESMF_SUCCESS

    ! define initialization phases
    ! skip over IPDv01p2 because we want connected status set on
    ! import/export fields prior to realization
    initPhases(1) = "IPDv01p1=1"
    initPhases(2) = "IPDv01p3=2"
    initPhases(3) = "IPDv01p4=3"
    initPhases(4) = "IPDv01p5=4"
    call ESMF_AttributeSet(gcomp, &
      name="InitializationPhaseMap", valueList=initPhases, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! define importable fields
    numImport = 4
    allocate(impStdName(numImport), impFldName(numImport), &
      impActive(numImport), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of import field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    impActive(:) = .true.
    impStdName( 1) = "sea_surface_downward_eastward_stress"
    impStdName( 2) = "sea_surface_downward_northward_stress"
    impStdName( 3) = "eastward_stokes_drift_current"
    impStdName( 4) = "northward_stokes_drift_current"
    do i = 1,numImport
      call NUOPC_FieldDictionaryGetEntry(trim(impStdName(i)), &
        defaultShortName=impFldName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_FieldDictionaryGetEntry: ',i,', '//trim(impStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! define exportable fields
    numExport = 3
    allocate(expStdName(numExport), expFldName(numExport), &
      expActive(numExport), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of export field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    expActive(:) = .false.
    expStdName( 1) = "surface_eastward_sea_water_velocity"
    expStdName( 2) = "surface_northward_sea_water_velocity"
    expStdName( 3) = "sea_surface_temperature"
    do i = 1,numExport
      call NUOPC_FieldDictionaryGetEntry(trim(expStdName(i)), &
        defaultShortName=expFldName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_FieldDictionaryGetEntry: ',i,', '//trim(expStdName(i))
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
    integer :: stat
    integer :: i

    rc = ESMF_SUCCESS

    ! advertise importable fields
    do i = 1,numImport
      call NUOPC_StateAdvertiseField(importState, &
        StandardName=trim(impStdName(i)), name=trim(impFldName(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_StateAdvertiseField: ',i,', '//trim(impStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! advertise exportable fields
    do i = 1,numExport
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

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
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
      100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 100, 20, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! realize all import fields
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

    ! realize active export fields (& remove inactive)
    do i = 1,numExport
      expActive(i) = NUOPC_StateIsFieldConnected(exportState, expFldName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,i2,a)') 'NUOPC_StateIsFieldConnected: ',i,', '//trim(expStdName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      if (expActive(i)) then
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
      else
        call ESMF_StateRemove(exportState, (/trim(expFldName(i))/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          write(msg,'(a,i2,a)') 'ESMF_StateRemove: ',i,', '//trim(expStdName(i))
          call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
          return  ! bail out
        endif
      endif
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=3, rc=rc) ! 3 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call NUOPC_GridCompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: name
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Time)               :: currTime
    type(ESMF_TimeInterval)       :: timeStep

    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, name=name, clock=clock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.

    call NUOPC_ClockPrintCurrTime(clock, &
      "------>Advancing "//trim(name)//" from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    
    call NUOPC_TimePrint(currTime + timeStep, &
      "---------------------> to: ", rc=rc)
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
    deallocate(impStdName, impFldName, impActive, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of import field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! deallocate export field name arrays
    deallocate(expStdName, expFldName, expActive, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of export field name arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

  end subroutine

end module
