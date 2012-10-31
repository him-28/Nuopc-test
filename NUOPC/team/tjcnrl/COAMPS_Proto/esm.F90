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

  character (*), parameter :: label_InternalState = "ESM_InternalState"

  integer, parameter :: med = 1
  integer, parameter :: atm = 2
  integer, parameter :: ocn = 3
  integer, parameter :: wav = 4
  integer, parameter :: ice = 5
  integer, parameter :: modCount = 5

  type type_InternalStateStruct
    character(7), pointer :: modName(:)
    logical     , pointer :: modActive(:)
    character(4), pointer :: modType(:)
    integer     , pointer :: modPetCount(:)
    type(driver_type_PetList), pointer :: modPetLists(:)
    character(7), pointer :: conName(:,:)
    logical     , pointer :: conActive(:,:)
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
    character(ESMF_MAXSTR)             :: cname
    character(ESMF_MAXSTR)             :: msg
    integer                            :: localrc, stat
    type(type_InternalState)           :: is
    integer                            :: i, j
    integer                            :: petCount
    character(7), pointer              :: modName(:)
    logical     , pointer              :: modActive(:)
    character(4), pointer              :: modType(:)
    character(7), pointer              :: conName(:,:)
    logical     , pointer              :: conActive(:,:)
    integer     , pointer              :: modPetCount(:)
    type(driver_type_PetList), pointer :: modPetLists(:)
    integer     , pointer              :: petList(:)
    type(ESMF_Time)                    :: startTime
    type(ESMF_Time)                    :: stopTime
    type(ESMF_TimeInterval)            :: timeStep
    type(ESMF_Clock)                   :: internalClock

    ! define input namelist
    integer, parameter :: maxPetCount = 1000
    integer            :: iunit
    logical      :: atmActive, ocnActive, wavActive, iceActive
    character(4) :: atmType, ocnType, wavType, iceType
    integer      :: medPetCount
    integer      :: medPetList(maxPetCount)
    integer      :: atmPetCount, ocnPetCount, wavPetCount, icePetCount
    integer      :: atmPetList(maxPetCount), ocnPetList(maxPetCount), &
                    wavPetList(maxPetCount), icePetList(maxPetCount)
    namelist / esmnl / medPetCount, medPetList, &
      atmActive, atmType, atmPetCount, atmPetList, &
      ocnActive, ocnType, ocnPetCount, ocnPetList, &
      wavActive, wavType, wavPetCount, wavPetList, &
      iceActive, iceType, icePetCount, icePetList

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetServices', ESMF_LOGMSG_INFO)

    ! get the petCount
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set namelist variable defaults
    if (petCount.gt.maxPetCount) then
      write(msg,'(a,i0,a,i0)') 'petCount > maxPetCount: petCount = ', &
        petCount,',   maxPetCount = ',maxPetCount
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
      rc = ESMF_FAILURE
      return  ! bail out
    endif
    atmActive = .false.
    ocnActive = .false.
    wavActive = .false.
    iceActive = .false.
    atmType = 'live'
    ocnType = 'live'
    wavType = 'live'
    iceType = 'live'
    medPetCount = petCount
    atmPetCount = petCount
    ocnPetCount = petCount
    wavPetCount = petCount
    icePetCount = petCount
    do i = 1,maxPetCount
      if (i.le.petCount) then
        medPetList(i) = i-1
        atmPetList(i) = i-1
        ocnPetList(i) = i-1
        wavPetList(i) = i-1
        icePetList(i) = i-1
      else
        medPetList(i) = -1
        atmPetList(i) = -1
        ocnPetList(i) = -1
        wavPetList(i) = -1
        icePetList(i) = -1
      endif
    enddo

    ! input namelist
    call ESMF_UtilIOUnitGet(iunit, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    open(iunit,file='parmnl',status='old')
    read(iunit,nml=esmnl)
    close(iunit)

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! allocate internal state arrays
    allocate(is%wrap%modName(modCount), &
             is%wrap%modActive(modCount), &
             is%wrap%modType(modCount), &
             is%wrap%modPetCount(modCount), &
             is%wrap%modPetLists(modCount), &
             is%wrap%conName(modCount,modCount), &
             is%wrap%conActive(modCount,modCount), &
             stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! set some useful pointers
    modName   => is%wrap%modName
    modActive => is%wrap%modActive
    modType   => is%wrap%modType
    conName   => is%wrap%conName
    conActive => is%wrap%conActive
    modPetCount => is%wrap%modPetCount
    modPetLists => is%wrap%modPetLists

    ! set active models
    modActive(med) = .true. ! mediator must always be active
    modActive(atm) = atmActive
    modActive(ocn) = ocnActive
    modActive(wav) = wavActive
    modActive(ice) = iceActive

    ! set/check model types
    modType(med) = 'live' ! mediator must always be live
    modType(atm) = atmType
    modType(ocn) = ocnType
    modType(wav) = wavType
    modType(ice) = iceType
    do i = 1,modCount
      select case (modType(i))
      case ('live','data')
      case default
        call ESMF_LogWrite('Model type not supported: '//modType(i), &
          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return  ! bail out
      end select
    enddo

    ! set model names
    modName(med) = 'MED'
    modName(atm) = 'ATM'//modType(atm)
    modName(ocn) = 'OCN'//modType(ocn)
    modName(wav) = 'WAV'//modType(wav)
    modName(ice) = 'ICE'//modType(ice)

    ! set/check the model petCounts & allocate model PetLists
    modPetCount(med) = medPetCount
    modPetCount(atm) = atmPetCount
    modPetCount(ocn) = ocnPetCount
    modPetCount(wav) = wavPetCount
    modPetCount(ice) = icePetCount
    do i = 1,modCount
      if (modPetCount(i).gt.petCount) then
        write(msg,'(a,i0,a,i0)') modName(i)//' petCount = ',modPetCount(i), &
          ' is greater than available petCount = ',petCount
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return  ! bail out
      endif
      allocate(modPetLists(i)%petList(modPetCount(i)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of "//modName(i)//" petList array failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    enddo

    ! set/check the model PetLists
    modPetLists(med)%petList(1:medPetCount) = medPetList(1:medPetCount)
    modPetLists(atm)%petList(1:atmPetCount) = atmPetList(1:atmPetCount)
    modPetLists(ocn)%petList(1:ocnPetCount) = ocnPetList(1:ocnPetCount)
    modPetLists(wav)%petList(1:wavPetCount) = wavPetList(1:wavPetCount)
    modPetLists(ice)%petList(1:icePetCount) = icePetList(1:icePetCount)
    do i = 1,modCount
      petList => modPetLists(i)%petList
      if (any(petList.lt.0).or.any(petList.ge.petCount)) then
        write(msg,'(a,i0,a)') modName(i)//' petList values must be in [0,',petCount,')'
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return  ! bail out
      endif
    enddo

    ! set connector names
    do j = 1,modCount
    do i = 1,modCount
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
    internalClock = ESMF_ClockCreate(name=trim(cname)//" Clock", &
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

    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetServices', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(driver_type_IS)               :: superIS

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetModelCount', ESMF_LOGMSG_INFO)

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set the modelCount
    superIS%wrap%modelCount = modCount

    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetModelCount', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelPetLists(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    integer                            :: i, j

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetModelPetLists', ESMF_LOGMSG_INFO)

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set the model petLists
    do i = 1,modCount
      if (.not.is%wrap%modActive(i)) cycle
      superIS%wrap%modelPetLists(i)%petList => is%wrap%modPetLists(i)%petList
    enddo

    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetModelPetLists', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    character(ESMF_MAXSTR)             :: msg
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    character(7), pointer              :: modName(:)
    logical     , pointer              :: modActive(:)
    character(4), pointer              :: modType(:)
    character(7), pointer              :: conName(:,:)
    logical     , pointer              :: conActive(:,:)
    integer                            :: i, j
    type(ESMF_GridComp), pointer       :: modComp(:)
    type(ESMF_CplComp), pointer        :: conComp(:,:)
    type(NUOPC_RunSequence), pointer   :: runSeq(:)

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetModelServices', ESMF_LOGMSG_INFO)

    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set some useful pointers
    modComp => superIS%wrap%modelComp
    conComp => superIS%wrap%connectorComp
    runSeq => superIS%wrap%runSeq

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set some useful pointers
    modName   => is%wrap%modName
    modActive => is%wrap%modActive
    modType   => is%wrap%modType
    conName   => is%wrap%conName
    conActive => is%wrap%conActive

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

    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetModelServices', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    character(ESMF_MAXSTR)             :: msg
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    integer                            :: i, j

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_LogWrite('>>>'//trim(cname)//' entered Finalize', ESMF_LOGMSG_INFO)

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! deallocate internal state arrays
    ! note: deallocation of of petList arrays is done by NUOPC_Driver
    deallocate(is%wrap%modName, is%wrap%modActive, is%wrap%modType, &
               is%wrap%modPetCount, is%wrap%modPetLists, &
               is%wrap%conName, is%wrap%conActive, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    call ESMF_LogWrite('<<<'//trim(cname)//' leaving Finalize', ESMF_LOGMSG_INFO)

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
