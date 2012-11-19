#define FILENAME "esm.F90"

!-------------------------------------------------------------------------------
! Define included component modules
!-------------------------------------------------------------------------------
#define MODULE_CON CON

#define INCLUDE_MED
#define MODULE_MED MED

#define INCLUDE_ATM
#define MODULE_ATMlive MODlive
#define MODULE_ATMdata MODdata

#define INCLUDE_OCN
#define MODULE_OCNlive MODlive
#define MODULE_OCNdata MODdata

#define INCLUDE_WAV
#define MODULE_WAVlive MODlive
#define MODULE_WAVdata MODdata

#define INCLUDE_ICE
#define MODULE_ICElive MODlive
#define MODULE_ICEdata MODdata

#undef  INCLUDE_LND
#define MODULE_LNDlive MODlive
#define MODULE_LNDdata MODdata


!-------------------------------------------------------------------------------
! Driver component with explicit time stepping
!-------------------------------------------------------------------------------
module ESM

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

  use MODULE_CON    , only: cplSS     => SetServices
#ifdef INCLUDE_MED
  use MODULE_MED    , only: medSS     => SetServices
#endif
#ifdef INCLUDE_ATM
  use MODULE_ATMlive, only: atmLiveSS => SetServices
  use MODULE_ATMdata, only: atmDataSS => SetServices
#endif
#ifdef INCLUDE_OCN
  use MODULE_OCNlive, only: ocnLiveSS => SetServices
  use MODULE_OCNdata, only: ocnDataSS => SetServices
#endif
#ifdef INCLUDE_WAV
  use MODULE_WAVlive, only: wavLiveSS => SetServices
  use MODULE_WAVdata, only: wavDataSS => SetServices
#endif
#ifdef INCLUDE_ICE
  use MODULE_ICElive, only: iceLiveSS => SetServices
  use MODULE_ICEdata, only: iceDataSS => SetServices
#endif
#ifdef INCLUDE_LND
  use MODULE_LNDlive, only: lndLiveSS => SetServices
  use MODULE_LNDdata, only: lndDataSS => SetServices
#endif

  implicit none
  save
  private

  public SetServices

  logical     , parameter :: defaultVerbose = .true.
  logical     , parameter :: defaultModActive = .true.
  character(4), parameter :: defaultModType = 'live'

  integer, parameter :: maxModCount = 6
  integer      :: modCount
  integer      :: med, atm, ocn, wav, ice, lnd
  character(3) :: modShortNameLC(maxModCount)
  character(3) :: modShortNameUC(maxModCount)
  character(7) :: modName(maxModCount)
  character(4) :: modType(maxModCount)
  logical      :: modActive(maxModCount)
  character(7) :: conName(maxModCount,maxModCount)
  logical      :: conActive(maxModCount,maxModCount)

  character(ESMF_MAXSTR) :: cname
  character(ESMF_MAXSTR) :: msgString
  logical                :: verbose
  type(ESMF_Config)      :: config

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                            :: localrc, stat
    integer                            :: i, j, k
    logical                            :: configIsPresent
    character(ESMF_MAXSTR)             :: label
    integer(ESMF_KIND_I4)              :: time(6)
    type(ESMF_Time)                    :: startTime
    type(ESMF_Time)                    :: stopTime
    type(ESMF_TimeInterval)            :: timeStep
    type(ESMF_TimeInterval)            :: timeDiff
    type(ESMF_TimeInterval)            :: zeroTimeInterval
    type(ESMF_Clock)                   :: internalClock

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! check/get the config (config is required)
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (configIsPresent) then
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    else
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config')
      return  ! bail out
    endif

    ! report on compiled modules
#ifdef INCLUDE_MED
    call ESMF_LogWrite(trim(cname)//': compiled with    MED module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without MED module', ESMF_LOGMSG_INFO)
#endif
#ifdef INCLUDE_ATM
    call ESMF_LogWrite(trim(cname)//': compiled with    ATM module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ATM module', ESMF_LOGMSG_INFO)
#endif
#ifdef INCLUDE_OCN
    call ESMF_LogWrite(trim(cname)//': compiled with    OCN module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without OCN module', ESMF_LOGMSG_INFO)
#endif
#ifdef INCLUDE_WAV
    call ESMF_LogWrite(trim(cname)//': compiled with    WAV module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without WAV module', ESMF_LOGMSG_INFO)
#endif
#ifdef INCLUDE_ICE
    call ESMF_LogWrite(trim(cname)//': compiled with    ICE module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ICE module', ESMF_LOGMSG_INFO)
#endif
#ifdef INCLUDE_LND
    call ESMF_LogWrite(trim(cname)//': compiled with    LND module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without LND module', ESMF_LOGMSG_INFO)
#endif

    ! set model count, model index mapping, and model short names
    modCount = 0
#ifdef INCLUDE_MED
    modCount = modCount + 1
    med = modCount
    modShortNameLC(med) = 'med'
    modShortNameUC(med) = 'MED'
#endif
#ifdef INCLUDE_ATM
    modCount = modCount + 1
    atm = modCount
    modShortNameLC(atm) = 'atm'
    modShortNameUC(atm) = 'ATM'
#endif
#ifdef INCLUDE_OCN
    modCount = modCount + 1
    ocn = modCount
    modShortNameLC(ocn) = 'ocn'
    modShortNameUC(ocn) = 'OCN'
#endif
#ifdef INCLUDE_WAV
    modCount = modCount + 1
    wav = modCount
    modShortNameLC(wav) = 'wav'
    modShortNameUC(wav) = 'WAV'
#endif
#ifdef INCLUDE_ICE
    modCount = modCount + 1
    ice = modCount
    modShortNameLC(ice) = 'ice'
    modShortNameUC(ice) = 'ICE'
#endif
#ifdef INCLUDE_LND
    modCount = modCount + 1
    lnd = modCount
    modShortNameLC(lnd) = 'lnd'
    modShortNameUC(lnd) = 'LND'
#endif
    do i = 1,modCount
      write(msgString,'(a,i0)') trim(cname)//': '//modShortNameUC(i)//' model index: ',i
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! process config for verbose
    label = 'verbose:'
    call ESMF_ConfigGetAttribute(config, verbose, default=defaultVerbose, &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! process config for modActive
    do i = 1,modCount
      label = modShortNameLC(i)//'Active:'
      call ESMF_ConfigGetAttribute(config, modActive(i), default=defaultModActive, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      if (modActive(i)) then
        write(msgString,'(a)') trim(cname)//': '//modShortNameUC(i)//' is     active'
      else
        write(msgString,'(a)') trim(cname)//': '//modShortNameUC(i)//' is not active'
      endif
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! process config for modType
#ifdef INCLUDE_MED
    modType(med) = '' ! mediator type is not used
    do i = 2,modCount
#else
    do i = 1,modCount
#endif
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
      write(msgString,'(a)') trim(cname)//': '//modShortNameUC(i)//' type: '//modType(i)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! set model names
    do i = 1,modCount
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
#ifdef INCLUDE_MED
    if (modActive(med)) then
      do i = 2,modCount
        if (modActive(i)) then
          conActive(i,med) = .true.
          conActive(med,i) = .true.
        endif
      enddo
    else
      do j = 2,modCount
      do i = 2,modCount
        if (i.eq.j) cycle
        if (modActive(i).and.modActive(j)) then
          conActive(i,j) = .true.
          conActive(j,i) = .true.
        endif
      enddo
      enddo
    endif
#else
    do j = 1,modCount
    do i = 1,modCount
      if (i.eq.j) cycle
      if (modActive(i).and.modActive(j)) then
        conActive(i,j) = .true.
        conActive(j,i) = .true.
      endif
    enddo
    enddo
#endif
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      write(msgString,'(a)') trim(cname)//': '//conName(i,j)//' connector is active'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    enddo

    ! process config for required startTime input
    label = 'startTime:'
    call ESMF_ConfigGetAttribute(config, time, count=6, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' YYYY MM DD hh mm ss')
      return  ! bail out
    endif
    write(msgString,'(a,6(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=1,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeSet(startTime, yy=time(1), mm=time(2), dd=time(3), &
      h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! process config for required stopTime input
    label = 'stopTime:'
    call ESMF_ConfigGetAttribute(config, time, count=6, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' YYYY MM DD hh mm ss')
      return  ! bail out
    endif
    write(msgString,'(a,6(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=1,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeSet(stopTime, yy=time(1), mm=time(2), dd=time(3), &
      h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! process config for required timeStep input
    label = 'timeStep:'
    call ESMF_ConfigGetAttribute(config, time(4:6), count=3, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' hh mm ss')
      return  ! bail out
    endif
    write(msgString,'(a,3(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=4,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeIntervalSet(timeStep, h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! check that simulation time is multiple of timeStep
    call ESMF_TimeIntervalSet(zeroTimeInterval, h=0, m=0, s=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    timeDiff = stopTime - startTime
    if (mod(timeDiff,timeStep) .ne. zeroTimeInterval) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': simulation time is not a multiple of timeStep')
      return  ! bail out
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

    ! create/set the driver clock
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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(driver_type_IS)               :: superIS

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
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    integer                            :: i, j
    integer                            :: k, l, m, n, p
    integer                            :: k1, k2
    integer                            :: modStart
    integer                            :: petCount
    integer                            :: modPetCount(maxModCount)
    integer     , pointer              :: modPetList(:)
    character(ESMF_MAXSTR)             :: label
    character(ESMF_MAXSTR)             :: petLayoutOption
    logical                            :: isPresent
    integer     , allocatable          :: list(:), ncol(:)

    rc = ESMF_SUCCESS

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! get the petCount
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! process config for petLayoutOption
    label = 'petLayoutOption:'
    call ESMF_ConfigGetAttribute(config, petLayoutOption, default='sequential', &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_LogWrite(trim(cname)//': petLayoutOption = '//trim(petLayoutOption), &
    ESMF_LOGMSG_INFO)

    ! set the model petLists based on petLayoutOption
    select case (trim(petLayoutOption))

    ! petLayoutOption = sequential
    !   * active models defined on all pets
    !   * MED defined on all pets
    !   * no other config options required
    case ('sequential')
      do i = 1,modCount
        if (.not.modActive(i)) cycle
        allocate(superIS%wrap%modelPetLists(i)%petList(petCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of "//modName(i)//" petList array failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        modPetList => superIS%wrap%modelPetLists(i)%petList
        do j = 1,petCount
          modPetList(j) = j-1
        enddo
      enddo

    ! petLayoutOption = concurrent
    !   * active models defined on non-overlapping sets of PETs
    !   * requires <mod>PetCount input for active models
    !   * medPetCount optional, default is MED defined on all PETs
    !   * requires \sum(<mod>PetCount) <= petCount
    case ('concurrent')
      modStart = 1
#ifdef INCLUDE_MED
      if (modActive(med)) then
        label=modShortNameLC(med)//'PetCount:'
        call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
        if (.not.isPresent.or.rc.ne.ESMF_SUCCESS) then
          modPetCount(med) = petCount
          if (verbose) then
            write(msgString,'(a,i0)') trim(cname)//': '// &
              modShortNameUC(med)//' PET count: ',modPetCount(med)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          endif
          allocate(superIS%wrap%modelPetLists(med)%petList(petCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of "//modName(i)//" petList array failed.", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          modPetList => superIS%wrap%modelPetLists(med)%petList
          do j = 1,petCount
            modPetList(j) = j-1
          enddo
          modStart = 2
        endif
      endif
#endif
      n = 0
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
        label=modShortNameLC(i)//'PetCount:'
        call ESMF_ConfigGetAttribute(config, modPetCount(i), label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when petLayoutOption'// &
            ' = concurrent and '//modShortNameUC(i)//' is active')
          return  ! bail out
        endif
        if (verbose) then
          write(msgString,'(a,i0)') trim(cname)//': '// &
            modShortNameUC(i)//' PET count: ',modPetCount(i)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
        endif
        if (modPetCount(i).lt.1.or.modPetCount(i).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' must be > 0 and < petCount')
          return  ! bail out
        endif
        n = n + modPetCount(i)
      enddo
      if (n.gt.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': petLayoutOption = concurrent requires'// &
          ' \sum(<mod>PetCount) <= petCount for active models')
        return  ! bail out
      endif
      n = 0
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
        allocate(superIS%wrap%modelPetLists(i)%petList(modPetCount(i)), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of "//modName(i)//" petList array failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        modPetList => superIS%wrap%modelPetLists(i)%petList
        do j = 1,modPetCount(i)
          modPetList(j) = n
          n = n + 1
        enddo
      enddo

    ! petLayoutOption = specified
    !   * active models defined on specified sets of PETs
    !   * requires <mod>PetList input for active models
    !   * medPetList optional, default is MED defined on all PETs
    !   * requires min(<mod>PetList) >= 0 && max(<mod>PetList) < petCount
    case ('specified')
      modStart = 1
#ifdef INCLUDE_MED
      if (modActive(med)) then
        label=modShortNameLC(med)//'PetList::'
        call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
        if (.not.isPresent.or.rc.ne.ESMF_SUCCESS) then
          modPetCount(med) = petCount
          if (verbose) then
            write(msgString,'(a,i0)') trim(cname)//': '// &
              modShortNameUC(med)//' PET count: ',modPetCount(med)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          endif
          allocate(superIS%wrap%modelPetLists(med)%petList(petCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of "//modName(i)//" petList array failed.", &
            line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
          modPetList => superIS%wrap%modelPetLists(med)%petList
          do j = 1,petCount
            modPetList(j) = j-1
          enddo
          modStart = 2
        endif
      endif
#endif
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
        label=modShortNameLC(i)//'PetList::'
        call ESMF_ConfigGetDim(config, m, n, label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when petLayoutOption'// &
            ' = specified and '//modShortNameUC(i)//' is active')
          return  ! bail out
        endif
        if (verbose) then
          write(msgString,'(a,i0)') trim(cname)//': '// &
            modShortNameUC(i)//' petList table number of rows: ',m
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          write(msgString,'(a,i0)') trim(cname)//': '// &
            modShortNameUC(i)//' petList table max number of columns: ',n
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
        endif
        allocate(list(n), ncol(m), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of "//modName(i)//" petList table failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        do p = 1,2
          if (p.eq.2) then
            if (verbose) then
              write(msgString,'(a,i0)') trim(cname)//': '// &
                modShortNameUC(i)//' PET count: ',modPetCount(i)
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
            endif
            if (modPetCount(i).lt.1.or.modPetCount(i).gt.petCount) then
              call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
                msg=trim(cname)//': '//modShortNameUC(i)// &
                ' PET count must be > 0 and <= petCount')
              return  ! bail out
            endif
            allocate(superIS%wrap%modelPetLists(i)%petList(modPetCount(i)), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation of "//modName(i)//" petList array failed.", &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            modPetList => superIS%wrap%modelPetLists(i)%petList
          endif
          call ESMF_ConfigFindLabel(config, trim(label), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) then
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
              msg=trim(cname)//': '//trim(label)//' is required when petLayoutOption'// &
              ' = specified and '//modShortNameUC(i)//' is active')
            return  ! bail out
          endif
          modPetCount(i) = 0
          do l=1,m
            call ESMF_ConfigNextLine(config, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=FILENAME)) then
              write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                ' next line ',l,' failed'
              call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
              return  ! bail out
            endif
            if (p.eq.1) then
              ncol(l) = ESMF_ConfigGetLen(config, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME)) then
                write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                  ' get length ',l,' failed'
                call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
                return  ! bail out
              endif
              modPetCount(i) = modPetCount(i) + ncol(l)
            else
              call ESMF_ConfigGetAttribute(config, list(1:ncol(l)), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=FILENAME)) then
                write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                  ' get row ',l,' failed'
                call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
                return  ! bail out
              endif
              modPetCount(i) = modPetCount(i) + ncol(l)
              modPetList(modPetCount(i)-ncol(l)+1:modPetCount(i)) = list(1:ncol(l))
            endif
          enddo
        enddo
        deallocate(list, ncol, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of "//modName(i)//" petList table failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (verbose) then
          n = 10
          m = ceiling(real(modPetCount(i))/real(n))
          write(msgString,'(a)') trim(cname)//': '//modShortNameUC(i)//' petList:'
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          do l = 1,m
            k1 = min((l-1)*n+1,modPetCount(i))
            k2 = min((l-1)*n+n,modPetCount(i))
            write(msgString,'(a,100i7)') trim(cname)//': ', (modPetList(k),k=k1,k2)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          enddo
        endif
        if (minval(modPetList).lt.0.or.maxval(modPetList).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//modShortNameUC(i)//' petList must be > 0 and < petCount')
          return  ! bail out
        endif
        do j = 1,modPetCount(i)
          if (count(modPetList.eq.modPetList(j)).gt.1) then
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
              msg=trim(cname)//': '//modShortNameUC(i)//' petList has duplicate entries')
            return  ! bail out
          endif
        enddo
      enddo

    ! unsupported petLayoutOption
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
       msg=trim(cname)//': petLayoutOption not supported: '//trim(petLayoutOption))
      return  ! bail out
    end select

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                            :: localrc, stat
    type(driver_type_IS)               :: superIS
    integer                            :: i, j
    type(ESMF_GridComp), pointer       :: modComp(:)
    type(ESMF_CplComp), pointer        :: conComp(:,:)
    type(NUOPC_RunSequence), pointer   :: runSeq(:)
    character(ESMF_MAXSTR)             :: verbosity

    rc = ESMF_SUCCESS

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! set some useful pointers
    modComp => superIS%wrap%modelComp
    conComp => superIS%wrap%connectorComp
    runSeq => superIS%wrap%runSeq

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
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(modComp(i), name="Verbosity", value=trim(verbosity), &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//modName(i)
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
        write(msgString,'(a,2i2,a)') 'ESMF_CplCompSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(conComp(i,j), name="Verbosity", value=trim(verbosity), &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo
    enddo

    ! SetServices for active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      select case (modShortNameLC(i))
#ifdef INCLUDE_MED
      case ('med')
        call ESMF_GridCompSetServices(modComp(i), medSS, userRc=localrc, rc=rc)
#endif
#ifdef INCLUDE_ATM
      case ('atm')
        select case (modType(i))
        case ('live')
          call ESMF_GridCompSetServices(modComp(i), atmLiveSS, userRc=localrc, rc=rc)
        case ('data')
          call ESMF_GridCompSetServices(modComp(i), atmDataSS, userRc=localrc, rc=rc)
        end select
#endif
#ifdef INCLUDE_OCN
      case ('ocn')
        select case (modType(i))
        case ('live')
          call ESMF_GridCompSetServices(modComp(i), ocnLiveSS, userRc=localrc, rc=rc)
        case ('data')
          call ESMF_GridCompSetServices(modComp(i), ocnDataSS, userRc=localrc, rc=rc)
        end select
#endif
#ifdef INCLUDE_WAV
      case ('wav')
        select case (modType(i))
        case ('live')
          call ESMF_GridCompSetServices(modComp(i), wavLiveSS, userRc=localrc, rc=rc)
        case ('data')
          call ESMF_GridCompSetServices(modComp(i), wavDataSS, userRc=localrc, rc=rc)
        end select
#endif
#ifdef INCLUDE_ICE
      case ('ice')
        select case (modType(i))
        case ('live')
          call ESMF_GridCompSetServices(modComp(i), iceLiveSS, userRc=localrc, rc=rc)
        case ('data')
          call ESMF_GridCompSetServices(modComp(i), iceDataSS, userRc=localrc, rc=rc)
        end select
#endif
#ifdef INCLUDE_LND
      case ('lnd')
        select case (modType(i))
        case ('live')
          call ESMF_GridCompSetServices(modComp(i), lndLiveSS, userRc=localrc, rc=rc)
        case ('data')
          call ESMF_GridCompSetServices(modComp(i), lndDataSS, userRc=localrc, rc=rc)
        end select
#endif
      end select
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc) .or. &
          ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) then
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSetServices: ',i,', '//modName(i)
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
          line=__LINE__, file=FILENAME, rcToReturn=rc) .or. &
          ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) then
        write(msgString,'(a,2i2,a)') 'ESMF_CplCompSetServices: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo
    enddo

    ! override the default run sequence defined by the generic Driver
    ! notes: j = 0 indicates connector to driver; j < 0 indicates model run
    call NUOPC_RunSequenceDeallocate(runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call NUOPC_RunSequenceAdd(runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out    
#ifdef INCLUDE_MED
    if (modActive(med)) then
      do i = 2,modCount
        if (.not.conActive(i,med)) cycle
        call NUOPC_RunElementAdd(runSeq(1), i=i, j=med, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      enddo
      call NUOPC_RunElementAdd(runSeq(1), i=med, j=-1, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      do j = 2,modCount
        if (.not.conActive(med,j)) cycle
        call NUOPC_RunElementAdd(runSeq(1), i=med, j=j, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      enddo
      do i = 2,modCount
        if (.not.modActive(i)) cycle
        call NUOPC_RunElementAdd(runSeq(1), i=i, j=-1, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      enddo
    else
      do j = 2,modCount
      do i = 2,modCount
        if (i.eq.j) cycle
        if (.not.conActive(i,j)) cycle
        call NUOPC_RunElementAdd(runSeq(1), i=i, j=j, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      enddo
      enddo
      do i = 2,modCount
        if (.not.modActive(i)) cycle
        call NUOPC_RunElementAdd(runSeq(1), i=i, j=-1, phase=1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
      enddo
    endif
#else
    do j = 1,modCount
    do i = 1,modCount
      if (i.eq.j) cycle
      if (.not.conActive(i,j)) cycle
      call NUOPC_RunElementAdd(runSeq(1), i=i, j=j, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    enddo
    enddo
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      call NUOPC_RunElementAdd(runSeq(1), i=i, j=-1, phase=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    enddo
#endif
    call NUOPC_RunSequencePrint(runSeq(1))

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
