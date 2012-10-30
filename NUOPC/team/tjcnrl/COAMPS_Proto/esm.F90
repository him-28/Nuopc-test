#define FILENAME "esm.F90"

module ESM

  !-----------------------------------------------------------------------------
  ! Driver Component for ESM with explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, only: &
    driver_routine_SS             => routine_SetServices, &
    driver_type_IS                => type_InternalState, &
    driver_type_PetList           => type_PetList, &
    driver_label_IS               => label_InternalState, &
    driver_label_SetModelCount    => label_SetModelCount, &
    driver_label_SetModelPetLists => label_SetModelPetLists, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_Finalize         => label_Finalize

  use CON,     only: cplSS     => SetServices
  use MED,     only: medSS     => SetServices
  use MODlive, only: modLiveSS => SetServices
  use MODdata, only: modDataSS => SetServices

  implicit none

  private

  public SetServices

  character (*), parameter :: label_DriverName = "ESM"
  character (*), parameter :: label_InternalState = "ESM_InternalState"

  integer, parameter :: med = 1
  integer, parameter :: atm = 2
  integer, parameter :: ocn = 3
  integer, parameter :: wav = 4
  integer, parameter :: ice = 5
  integer, parameter :: modCount = 5
  character (7) :: modName(modCount)
  logical       :: modActive(modCount)
  character (4) :: modType(modCount)
  character (7) :: conName(modCount,modCount)
  logical       :: conActive(modCount,modCount)

  type type_InternalStateStruct
    integer :: placeholder
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
    character(ESMF_MAXSTR)             :: msg
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    integer                            :: i, j
    type(ESMF_Time)                    :: startTime
    type(ESMF_Time)                    :: stopTime
    type(ESMF_TimeInterval)            :: timeStep
    type(ESMF_Clock)                   :: internalClock

    rc = ESMF_SUCCESS

    ! set active models
    modActive(med) = .true. ! mediator must always be active
    modActive(atm) = .true.
    modActive(ocn) = .true.
    modActive(wav) = .true.
    modActive(ice) = .true.

    ! set model types
    modType(med) = 'live' ! mediator must always be live
    modType(atm) = 'live'
    modType(ocn) = 'live'
    modType(wav) = 'live'
    modType(ice) = 'live'

    ! set model names
    modName(med) = 'MED'
    modName(atm) = 'ATM'//modType(atm)
    modName(ocn) = 'OCN'//modType(ocn)
    modName(wav) = 'WAV'//modType(wav)
    modName(ice) = 'ICE'//modType(ice)

    ! set connector names
    do j=1,modCount
    do i=1,modCount
      conName(i,j) = modName(i)(1:3)//'2'//modName(j)(1:3)
    enddo
    enddo

    ! set active connectors
    conActive = .false.
    if (modActive(atm)) then
      conActive(med,atm) = .true.
      conActive(atm,med) = .true.
    endif
    if (modActive(ocn)) then
      conActive(med,ocn) = .true.
      conActive(ocn,med) = .true.
    endif
    if (modActive(wav)) then
      conActive(med,wav) = .true.
      conActive(wav,med) = .true.
    endif
    if (modActive(ice)) then
      conActive(med,ice) = .true.
      conActive(ice,med) = .true.
    endif
    if (modActive(ocn).and.modActive(wav)) then
      conActive(ocn,wav) = .true.
      conActive(wav,ocn) = .true.
    endif

    ! set name for this component
    call ESMF_GridCompSet(gcomp, name=label_DriverName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! NUOPC_Driver registers the generic methods
    call driver_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=driver_label_SetModelCount, &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_MethodAdd(gcomp, label=driver_label_SetModelPetLists, &
      userRoutine=SetModelPetLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_MethodAdd(gcomp, label=driver_label_SetModelServices, &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_MethodAdd(gcomp, label=driver_label_Finalize, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set the driver clock
    call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    internalClock = ESMF_ClockCreate(name=label_DriverName//" Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_GridCompSet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! add required fields to NUOPC field dictionary
    call SetFieldDictionary(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: msg
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    integer                            :: i, j

    rc = ESMF_SUCCESS

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set the modelCount
    superIS%wrap%modelCount = modCount

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelPetLists(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: msg
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    integer                            :: i, j
    integer                            :: petCount, petLayout
    type(driver_type_PetList), pointer :: modPL(:)

    rc = ESMF_SUCCESS

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set some useful pointers
    modPL => superIS%wrap%modelPetLists

    ! get the petCount
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! allocate and set the model petLists
    petLayout = 0
    select case (petLayout)
    case (1)
      if (modActive(med)) then
        allocate(modPL(med)%petList(4))
        modPL(med)%petList = (/0,1,2,3/)
      endif
      j = 0
      do i = 2,modCount
        if (modActive(i)) then
          allocate(modPL(i)%petList(1))
          modPL(i)%petList = (/j/)
          j = j+1
        endif
      enddo
    case default
      do i = 1,modCount
        if (modActive(i)) then
          allocate(modPL(i)%petList(4))
          modPL(i)%petList = (/0,1,2,3/)
        endif
      enddo
    end select

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: msg
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    integer                            :: i, j
    type(ESMF_GridComp), pointer       :: modComp(:)
    type(ESMF_CplComp), pointer        :: conComp(:,:)
    type(NUOPC_RunSequence), pointer   :: runSeq(:)

    rc = ESMF_SUCCESS

    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set some useful pointers
    modComp => superIS%wrap%modelComp
    conComp => superIS%wrap%connectorComp
    runSeq => superIS%wrap%runSeq

    ! set model component names
    do i = 1,modCount
      call ESMF_GridCompSet(modComp(i), name=modName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,1i2,a)') 'ESMF_GridCompSet: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! set connector component names
    do j = 1,modCount
    do i = 1,modCount
      call ESMF_CplCompSet(conComp(i,j), name=conName(i,j), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msg,'(a,2i2,a)') 'ESMF_CplCompSet: ',i,j,', '//trim(conName(i,j))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo
    enddo

    ! SetServices for active models
    i = med
    if (modActive(i)) then
      call ESMF_GridCompSetServices(modComp(i), medSS, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME) .or. &
          ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msg,'(a,1i2,a)') 'ESMF_GridCompSetServices: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(modComp(i), name="Verbosity", value="high", &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msg,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    endif
    do i = 2,modCount
      if (.not.modActive(i)) cycle
      select case (modType(i))
      case ('live')
        call ESMF_GridCompSetServices(modComp(i), modLiveSS, userRc=localrc, rc=rc)
      case ('data')
        call ESMF_GridCompSetServices(modComp(i), modDataSS, userRc=localrc, rc=rc)
      end select
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME) .or. &
          ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msg,'(a,1i2,a)') 'ESMF_GridCompSetServices: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(modComp(i), name="Verbosity", value="high", &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msg,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! SetServices for active connectors
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      call ESMF_CplCompSetServices(conComp(i,j), cplSS, userRc=localrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME) .or. &
          ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msg,'(a,2i2,a)') 'ESMF_CplCompSetServices: ',i,j,', '//trim(conName(i,j))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(conComp(i,j), name="Verbosity", value="high", &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msg,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//trim(conName(i,j))
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo
    enddo

    ! override the default run sequence defined by the generic Driver
    call NUOPC_RunSequenceDeallocate(runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call NUOPC_RunSequenceAdd(runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out    
    if (modActive(atm).and.modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=atm, j=med, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(ocn).and.modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=ocn, j=med, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(wav).and.modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=wav, j=med, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(ice).and.modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=ice, j=med, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=med, j=0, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(atm).and.modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=med, j=atm, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(ocn).and.modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=med, j=ocn, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(wav).and.modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=med, j=wav, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(ice).and.modActive(med)) then
      call NUOPC_RunElementAdd(runSeq(1), i=med, j=ice, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(ocn).and.modActive(wav)) then
      call NUOPC_RunElementAdd(runSeq(1), i=ocn, j=wav, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(ocn).and.modActive(wav)) then
      call NUOPC_RunElementAdd(runSeq(1), i=wav, j=ocn, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(atm)) then
      call NUOPC_RunElementAdd(runSeq(1), i=atm, j=0, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(ocn)) then
      call NUOPC_RunElementAdd(runSeq(1), i=ocn, j=0, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(wav)) then
      call NUOPC_RunElementAdd(runSeq(1), i=wav, j=0, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif
    if (modActive(ice)) then
      call NUOPC_RunElementAdd(runSeq(1), i=ice, j=0, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)           :: msg
    integer                          :: localrc, stat
    type(driver_type_IS)             :: superIS
    type(type_InternalState)         :: is
    integer                          :: i, j

    rc = ESMF_SUCCESS

    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetFieldDictionary(rc)
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)           :: msg
    integer                          :: localrc, stat

    rc = ESMF_SUCCESS

    call NUOPC_FieldDictionaryAddEntry( &
       "eastward_wind_at_10m_height", &
       canonicalUnits="m s-1", &
       defaultLongName="N/A", &
       defaultShortName="wind_10m_u", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "northward_wind_at_10m_height", &
       canonicalUnits="m s-1", &
       defaultLongName="N/A", &
       defaultShortName="wind_10m_v", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "air_temperature_at_2m_height", &
       canonicalUnits="K", &
       defaultLongName="N/A", &
       defaultShortName="air_temp_2m", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "surface_eastward_wind_to_wave_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_atm_wav_u", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "surface_northward_wind_to_wave_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_atm_wav_v", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "surface_eastward_wave_to_ocean_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_wav_ocn_u", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "surface_northward_wave_to_ocean_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_wav_ocn_v", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_eastward_drift_velocity", &
       canonicalUnits="m s-1", &
       defaultLongName="N/A", &
       defaultShortName="ice_drift_u", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_northward_drift_velocity", &
       canonicalUnits="m s-1", &
       defaultLongName="N/A", &
       defaultShortName="ice_drift_v", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_concentration", &
       canonicalUnits="ice", &
       defaultLongName="N/A", &
       defaultShortName="ice_conc", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_thickness", &
       canonicalUnits="m", &
       defaultLongName="N/A", &
       defaultShortName="ice_thick", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_temperature", &
       canonicalUnits="K", &
       defaultLongName="N/A", &
       defaultShortName="ice_temp", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_surface_downward_eastward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_ocn_u", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_surface_downward_northward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_ocn_v", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_surface_downward_eastward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_atm_ice_u", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_surface_downward_northward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_atm_ice_v", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_basal_upward_eastward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_ocn_ice_u", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call NUOPC_FieldDictionaryAddEntry( &
       "sea_ice_basal_upward_northward_stress", &
       canonicalUnits="Pa", &
       defaultLongName="N/A", &
       defaultShortName="tau_ocn_ice_v", &
       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine

end module
