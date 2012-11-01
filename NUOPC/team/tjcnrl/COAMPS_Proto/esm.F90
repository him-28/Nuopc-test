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

  logical      , parameter :: defaultVerbose = .true.
  logical      , parameter :: defaultModActive = .true.
  character (4), parameter :: defaultModType = 'live'
  character (*), parameter :: label_InternalState = "ESM_InternalState"

  integer, parameter :: modCount = 5
  integer, parameter :: med = 1, atm = 2, ocn = 3, wav = 4, ice = 5
  character (3) :: modShortNameLC(modCount) = (/'med','atm','ocn','wav','ice'/)
  character (3) :: modShortNameUC(modCount) = (/'MED','ATM','OCN','WAV','ICE'/)

  type type_InternalStateStruct
    logical               :: verbose
    character(7), pointer :: modName(:)
    logical     , pointer :: modActive(:)
    character(4), pointer :: modType(:)
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
    character(ESMF_MAXSTR)             :: msgString
    logical                            :: verbose
    integer                            :: localrc, stat
    type(type_InternalState)           :: is
    integer                            :: i, j
    type(ESMF_Config)                  :: config
    logical                            :: configIsPresent
    character(ESMF_MAXSTR)             :: label
    character(7), pointer              :: modName(:)
    logical     , pointer              :: modActive(:)
    character(4), pointer              :: modType(:)
    character(7), pointer              :: conName(:,:)
    logical     , pointer              :: conActive(:,:)
    type(ESMF_Time)                    :: startTime
    type(ESMF_Time)                    :: stopTime
    type(ESMF_TimeInterval)            :: timeStep
    type(ESMF_Clock)                   :: internalClock

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! check/get/set the config
    ! if config is not present the set it to be an empty config
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (configIsPresent) then
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    else
      config = ESMF_ConfigCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_GridCompSet(gcomp, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    endif

    ! process config for verbose
    label = 'verbose:'
    call ESMF_ConfigGetAttribute(config, verbose, default=defaultVerbose, &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

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

    ! allocate internal state arrays
    allocate(is%wrap%modName(modCount), &
             is%wrap%modActive(modCount), &
             is%wrap%modType(modCount), &
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

    ! process config for modActive
    modActive(med) = .true. ! mediator must always be active
    do i = 2,modCount
      label = modShortNameLC(i)//'Active:'
      call ESMF_ConfigGetAttribute(config, modActive(i), default=defaultModActive, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! process config for modType
    modType(med) = 'live' ! mediator must always be live
    do i = 2,modCount
      if (.not.modActive(i)) cycle
      label = modShortNameLC(i)//'Type:'
      call ESMF_ConfigGetAttribute(config, modType(i), default=defaultModType, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      select case (modType(i))
      case ('live','data')
      case default
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': Model type not supported: '//modType(i))
        return  ! bail out
      end select
    enddo

    ! set model names
    modName(med) = 'MED'
    do i = 2,modCount
      if (.not.modActive(i)) cycle
      modName(i) = modShortNameUC(i)//modType(i)
    enddo

    ! set connector names
    do j = 1,modCount
    do i = 1,modCount
      conName(i,j) = modShortNameUC(i)//'2'//modShortNameUC(j)
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

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetServices', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    character(ESMF_MAXSTR)             :: msgString
    logical                            :: verbose
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetModelCount', ESMF_LOGMSG_INFO)

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set the modelCount
    superIS%wrap%modelCount = modCount

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetModelCount', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelPetLists(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    character(ESMF_MAXSTR)             :: msgString
    logical                            :: verbose
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    character(7), pointer              :: modName(:)
    logical     , pointer              :: modActive(:)
    character(4), pointer              :: modType(:)
    character(7), pointer              :: conName(:,:)
    logical     , pointer              :: conActive(:,:)
    integer                            :: i, j, n
    integer                            :: petCount
    integer                            :: modPetCount(modCount)
    type(ESMF_Config)                  :: config
    character(ESMF_MAXSTR)             :: label
    character(ESMF_MAXSTR)             :: petLayoutOption

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetModelPetLists', ESMF_LOGMSG_INFO)

    ! set some useful pointers
    modName   => is%wrap%modName
    modActive => is%wrap%modActive
    modType   => is%wrap%modType
    conName   => is%wrap%conName
    conActive => is%wrap%conActive

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! get the petCount
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query component for its config
    call ESMF_GridCompGet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! process config for petLayoutOption
    label = 'petLayoutOption:'
    call ESMF_ConfigGetAttribute(config, petLayoutOption, default='sequential', &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set the model petLists based on petLayoutOption
    select case (trim(petLayoutOption))

    ! petLayoutOption = sequential:
    !   * models defined on all pets
    !   * mediator defined on all pets
    !   * no other config options required
    case ('sequential')
      i = med
      allocate(superIS%wrap%modelPetLists(i)%petList(petCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of "//modName(i)//" petList array failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      do j = 1,petCount
        superIS%wrap%modelPetLists(i)%petList(j) = j-1
      enddo
      do i = 2,modCount
        if (.not.modActive(i)) cycle
        allocate(superIS%wrap%modelPetLists(i)%petList(petCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of "//modName(i)//" petList array failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        superIS%wrap%modelPetLists(i)%petList(:) = &
          superIS%wrap%modelPetLists(med)%petList(:)
      enddo

    ! petLayoutOption = concurrent:
    !   * models defined on non-overlapping sets of pets
    !   * mediator defined on all pets
    !   * requires config inputs for modPetCount for active mod = atm, ocn, wav, ice
    !   * medPetCount is set to petCount
    !   * requires \sum(modPetCount) = petCount
    case ('concurrent')
      i = med
      allocate(superIS%wrap%modelPetLists(i)%petList(petCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of "//modName(i)//" petList array failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      do j = 1,petCount
        superIS%wrap%modelPetLists(i)%petList(j) = j-1
      enddo
      n = 0
      do i = 2,modCount
        if (.not.modActive(i)) cycle
        label=modShortNameLC(i)//'PetCount:'
        call ESMF_ConfigGetAttribute(config, petLayoutOption, label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when petLayoutOption'// &
            ' = concurrent and '//modShortNameUC(i)//' is active')
          return  ! bail out
        endif
        if (modPetCount(i).lt.1.or.modPetCount(i).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' must be > 0 and < petCount')
          return  ! bail out
        endif
        n = n + modPetCount(i)
      enddo
      if (n.ne.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': petLayoutOption = concurrent requires'// &
          ' \sum(modPetCount) = petCount for active models')
        return  ! bail out
      endif
      n = 0
      do i = 2,modCount
        if (.not.modActive(i)) cycle
        allocate(superIS%wrap%modelPetLists(i)%petList(modPetCount(i)), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of "//modName(i)//" petList array failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        do j = 1,modPetCount(i)
          superIS%wrap%modelPetLists(i)%petList(j) = n
          n = n + 1
        enddo
      enddo

    ! petLayoutOption = specified:
    !   * models defined on specified sets of pets
    !   * medPetList optional, default is all pets
    !   * requires config inputs for modPetList for active mod = for atm, ocn, wav, ice
    !   * requires min(modPetList) >= 0 && max(modPetList) < petCount
!   case ('specified')

    ! unsupported petLayoutOption
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
       msg=trim(cname)//': petLayoutOption not supported: '//trim(petLayoutOption))
      return  ! bail out
    end select

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetModelPetLists', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    character(ESMF_MAXSTR)             :: msgString
    logical                            :: verbose
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
    character(ESMF_MAXSTR)             :: verbosity

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetModelServices', ESMF_LOGMSG_INFO)

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set some useful pointers
    modComp => superIS%wrap%modelComp
    conComp => superIS%wrap%connectorComp
    runSeq => superIS%wrap%runSeq

    ! query component for its internal State
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

    ! set verbosity for components
    if (verbose) then
      verbosity = "high"
    else
      verbosity = "low"
    endif

    ! set model component names and attributes
    do i = 1,modCount
      call ESMF_GridCompSet(modComp(i), name=modName(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSet: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(modComp(i), name="Verbosity", value=trim(verbosity), &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! set connector component names and attributes
    do j = 1,modCount
    do i = 1,modCount
      call ESMF_CplCompSet(conComp(i,j), name=conName(i,j), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,2i2,a)') 'ESMF_CplCompSet: ',i,j,', '//trim(conName(i,j))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(conComp(i,j), name="Verbosity", value=trim(verbosity), &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//trim(conName(i,j))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
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
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSetServices: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
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
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSetServices: ',i,', '//trim(modName(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
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
        write(msgString,'(a,2i2,a)') 'ESMF_CplCompSetServices: ',i,j,', '//trim(conName(i,j))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
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
    if (verbose) call NUOPC_RunSequencePrint(runSeq(1))

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetModelServices', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    character(ESMF_MAXSTR)             :: msgString
    logical                            :: verbose
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    type(type_InternalState)           :: is
    integer                            :: i, j

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered Finalize', ESMF_LOGMSG_INFO)

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! deallocate internal state arrays
    deallocate(is%wrap%modName, is%wrap%modActive, is%wrap%modType, &
               is%wrap%conName, is%wrap%conActive, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state arrays failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetFieldDictionary(rc)
    integer, intent(out) :: rc

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
