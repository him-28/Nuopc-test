#define FILENAME "drv.F90"

!-------------------------------------------------------------------------------
! Define included component modules
!-------------------------------------------------------------------------------
#define MODULE_CON CON
#define MODULE_MED MED
#define MODULE_ATM MOD
#define MODULE_OCN MOD
#define MODULE_WAV MOD
#define MODULE_ICE MOD
#define MODULE_OBG MBG
#define MODULE_WBG MBG


!-------------------------------------------------------------------------------
! Driver component with explicit time stepping
!-------------------------------------------------------------------------------
module DRV

  use ESMF
  use NUOPC
  use NUOPC_Driver

  use MODULE_CON, only: cplSS => SetServices
#ifdef MODULE_MED
  use MODULE_MED, only: medSS => SetServices
#endif
#ifdef MODULE_ATM
  use MODULE_ATM, only: atmSS => SetServices
#endif
#ifdef MODULE_OCN
  use MODULE_OCN, only: ocnSS => SetServices
#endif
#ifdef MODULE_WAV
  use MODULE_WAV, only: wavSS => SetServices
#endif
#ifdef MODULE_ICE
  use MODULE_ICE, only: iceSS => SetServices
#endif
#ifdef MODULE_LND
  use MODULE_LND, only: lndSS => SetServices
#endif
#ifdef MODULE_OBG
  use MODULE_OBG, only: obgSS => SetServices
#endif
#ifdef MODULE_WBG
  use MODULE_WBG, only: wbgSS => SetServices
#endif

  implicit none
  save
  private

  public SetServices

  logical     , parameter :: defaultVerbose = .true.
  logical     , parameter :: defaultModActive = .true.

  integer, parameter :: maxModCount = 8
  integer      :: modCount
  integer      :: med, atm, ocn, wav, ice, lnd
  integer      :: obg, wbg
  character(3) :: modNameLC(maxModCount)
  character(3) :: modNameUC(maxModCount)
  logical      :: modActive(maxModCount)
  character(7) :: conNameUC(maxModCount,maxModCount)
  character(7) :: conNameLC(maxModCount,maxModCount)
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
#ifdef MODULE_MED
    call ESMF_LogWrite(trim(cname)//': compiled with    MED module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without MED module', ESMF_LOGMSG_INFO)
#endif
#ifdef MODULE_ATM
    call ESMF_LogWrite(trim(cname)//': compiled with    ATM module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ATM module', ESMF_LOGMSG_INFO)
#endif
#ifdef MODULE_OCN
    call ESMF_LogWrite(trim(cname)//': compiled with    OCN module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without OCN module', ESMF_LOGMSG_INFO)
#endif
#ifdef MODULE_WAV
    call ESMF_LogWrite(trim(cname)//': compiled with    WAV module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without WAV module', ESMF_LOGMSG_INFO)
#endif
#ifdef MODULE_ICE
    call ESMF_LogWrite(trim(cname)//': compiled with    ICE module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ICE module', ESMF_LOGMSG_INFO)
#endif
#ifdef MODULE_LND
    call ESMF_LogWrite(trim(cname)//': compiled with    LND module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without LND module', ESMF_LOGMSG_INFO)
#endif
#ifdef MODULE_OBG
    call ESMF_LogWrite(trim(cname)//': compiled with    OBG module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without OBG module', ESMF_LOGMSG_INFO)
#endif
#ifdef MODULE_WBG
    call ESMF_LogWrite(trim(cname)//': compiled with    WBG module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without WBG module', ESMF_LOGMSG_INFO)
#endif

    ! set model count, model index mapping, and model names
    modCount = 0
#ifdef MODULE_MED
    modCount = modCount + 1
    med = modCount
    modNameLC(med) = 'med'
    modNameUC(med) = 'MED'
#endif
#ifdef MODULE_ATM
    modCount = modCount + 1
    atm = modCount
    modNameLC(atm) = 'atm'
    modNameUC(atm) = 'ATM'
#endif
#ifdef MODULE_OCN
    modCount = modCount + 1
    ocn = modCount
    modNameLC(ocn) = 'ocn'
    modNameUC(ocn) = 'OCN'
#endif
#ifdef MODULE_WAV
    modCount = modCount + 1
    wav = modCount
    modNameLC(wav) = 'wav'
    modNameUC(wav) = 'WAV'
#endif
#ifdef MODULE_ICE
    modCount = modCount + 1
    ice = modCount
    modNameLC(ice) = 'ice'
    modNameUC(ice) = 'ICE'
#endif
#ifdef MODULE_LND
    modCount = modCount + 1
    lnd = modCount
    modNameLC(lnd) = 'lnd'
    modNameUC(lnd) = 'LND'
#endif
#ifdef MODULE_OBG
    modCount = modCount + 1
    obg = modCount
    modNameLC(obg) = 'obg'
    modNameUC(obg) = 'OBG'
#endif
#ifdef MODULE_WBG
    modCount = modCount + 1
    wbg = modCount
    modNameLC(wbg) = 'wbg'
    modNameUC(wbg) = 'WBG'
#endif
    do i = 1,modCount
      write(msgString,'(a,i0)') trim(cname)//': '//modNameUC(i)//' model index: ',i
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
      label = modNameUC(i)//'_active:'
      call ESMF_ConfigGetAttribute(config, modActive(i), default=defaultModActive, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      if (modActive(i)) then
        write(msgString,'(a)') trim(cname)//': '//modNameUC(i)//' is     active'
      else
        write(msgString,'(a)') trim(cname)//': '//modNameUC(i)//' is not active'
      endif
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! set connector names
    do j = 1,modCount
    do i = 1,modCount
      conNameUC(i,j) = modNameUC(i)//'2'//modNameUC(j)
      conNameLC(i,j) = modNameLC(i)//'2'//modNameLC(j)
    enddo
    enddo

    ! set active connectors
    conActive = .false.
#ifdef MODULE_MED
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
#ifdef MODULE_OBG
    do i = 1,modCount
      conActive(i,obg) = .false.
    enddo
#endif
#ifdef MODULE_WBG
    do i = 1,modCount
      conActive(i,wbg) = .false.
    enddo
#endif
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      write(msgString,'(a)') trim(cname)//': '//conNameUC(i,j)//' connector is active'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    enddo

    ! process config for required startTime input
    label = 'start_time:'
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
    label = 'stop_time:'
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
    label = 'time_step:'
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
    call routine_SetServices(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=label_SetModelCount, &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_MethodAdd(gcomp, label=label_SetModelPetLists, &
      userRoutine=SetModelPetLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_MethodAdd(gcomp, label=label_SetModelServices, &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! create/set the driver clock
    internalClock = ESMF_ClockCreate(name=trim(cname)//"_clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_GridCompSet(gcomp, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(type_InternalState)           :: superIS

    rc = ESMF_SUCCESS

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, superIS, rc)
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
    type(type_InternalState)           :: superIS
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
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! get the petCount
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! process config for petLayoutOption
    label = 'pet_layout_option:'
    call ESMF_ConfigGetAttribute(config, petLayoutOption, default='sequential', &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_LogWrite(trim(cname)//': '//trim(label)//' '//trim(petLayoutOption), &
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
          msg="Allocation of "//modNameUC(i)//" PET list array failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        modPetList => superIS%wrap%modelPetLists(i)%petList
        do j = 1,petCount
          modPetList(j) = j-1
        enddo
      enddo

    ! petLayoutOption = concurrent
    !   * active models defined on non-overlapping sets of PETs
    !   * requires <MOD>_pet_count input for active models
    !   * medPetCount optional, default is MED defined on all PETs
    !   * requires \sum(<MOD>_pet_count) <= petCount
    case ('concurrent')
      modStart = 1
#ifdef MODULE_MED
      if (modActive(med)) then
        label=modNameUC(med)//'_pet_count:'
        call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
        if (.not.isPresent.or.rc.ne.ESMF_SUCCESS) then
          modPetCount(med) = petCount
          if (verbose) then
            write(msgString,'(a,i0)') trim(cname)//': '// &
              modNameUC(med)//' PET count: ',modPetCount(med)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          endif
          allocate(superIS%wrap%modelPetLists(med)%petList(petCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of "//modNameUC(i)//" PET list array failed.", &
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
#ifdef MODULE_OBG
        if (i.eq.obg) cycle
#endif
#ifdef MODULE_OBG
        if (i.eq.wbg) cycle
#endif
        label=modNameUC(i)//'_pet_count:'
        call ESMF_ConfigGetAttribute(config, modPetCount(i), label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
            ' = concurrent and '//modNameUC(i)//' is active')
          return  ! bail out
        endif
        if (verbose) then
          write(msgString,'(a,i0)') trim(cname)//': '// &
            modNameUC(i)//' PET count: ',modPetCount(i)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
        endif
        if (modPetCount(i).lt.1.or.modPetCount(i).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' must be > 0 and < # PETs')
          return  ! bail out
        endif
        n = n + modPetCount(i)
      enddo
      if (n.gt.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': pet_layout_option = concurrent requires'// &
          ' \sum(<MOD>_pet_count) <= # PETs for active models')
        return  ! bail out
      endif
      n = 0
      do i = modStart,modCount
        if (.not.modActive(i)) cycle
#ifdef MODULE_OBG
        if (i.eq.obg) then
          modPetCount(i) = modPetCount(atm)
        endif
#endif
#ifdef MODULE_WBG
        if (i.eq.wbg) then
          modPetCount(i) = modPetCount(atm)
        endif
#endif
        allocate(superIS%wrap%modelPetLists(i)%petList(modPetCount(i)), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of "//modNameUC(i)//" PET list array failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        modPetList => superIS%wrap%modelPetLists(i)%petList
#ifdef MODULE_OBG
        if (i.eq.obg) then
          modPetList(:) = superIS%wrap%modelPetLists(atm)%petList(:)
        else
        do j = 1,modPetCount(i)
          modPetList(j) = n
          n = n + 1
        enddo
        endif
#endif
#ifdef MODULE_WBG
        if (i.eq.wbg) then
          modPetList(:) = superIS%wrap%modelPetLists(atm)%petList(:)
        else
        do j = 1,modPetCount(i)
          modPetList(j) = n
          n = n + 1
        enddo
        endif
#endif
      enddo

    ! petLayoutOption = specified
    !   * active models defined on specified sets of PETs
    !   * requires <MOD>_pet_list input for active models
    !   * MED_pet_list optional, default is MED defined on all PETs
    !   * requires min(<MOD>_pet_list) >= 0 && max(<MOD>_pet_list) < petCount
    case ('specified')
      modStart = 1
#ifdef MODULE_MED
      if (modActive(med)) then
        label=modNameUC(med)//'_pet_list::'
        call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
        if (.not.isPresent.or.rc.ne.ESMF_SUCCESS) then
          modPetCount(med) = petCount
          if (verbose) then
            write(msgString,'(a,i0)') trim(cname)//': '// &
              modNameUC(med)//' PET count: ',modPetCount(med)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          endif
          allocate(superIS%wrap%modelPetLists(med)%petList(petCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of "//modNameUC(i)//" PET list array failed.", &
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
#ifdef MODULE_OBG
        if (i.eq.obg) cycle
#endif
#ifdef MODULE_OBG
        if (i.eq.wbg) cycle
#endif
        label=modNameUC(i)//'_pet_list::'
        call ESMF_ConfigGetDim(config, m, n, label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
            ' = specified and '//modNameUC(i)//' is active')
          return  ! bail out
        endif
        if (verbose) then
          write(msgString,'(a,i0)') trim(cname)//': '// &
            trim(label)//' table number of rows: ',m
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          write(msgString,'(a,i0)') trim(cname)//': '// &
            trim(label)//' table max number of columns: ',n
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
        endif
        allocate(list(n), ncol(m), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of "//modNameUC(i)//" PET list table failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        do p = 1,2
          if (p.eq.2) then
            if (verbose) then
              write(msgString,'(a,i0)') trim(cname)//': '// &
                modNameUC(i)//' PET count: ',modPetCount(i)
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
            endif
            if (modPetCount(i).lt.1.or.modPetCount(i).gt.petCount) then
              call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
                msg=trim(cname)//': '//modNameUC(i)// &
                ' PET count must be > 0 and <= # PETs')
              return  ! bail out
            endif
            allocate(superIS%wrap%modelPetLists(i)%petList(modPetCount(i)), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg="Allocation of "//modNameUC(i)//" PET list array failed.", &
              line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
            modPetList => superIS%wrap%modelPetLists(i)%petList
          endif
          call ESMF_ConfigFindLabel(config, trim(label), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=FILENAME)) then
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
              msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
              ' = specified and '//modNameUC(i)//' is active')
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
          msg="Deallocation of "//modNameUC(i)//" PET list table failed.", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
        if (verbose) then
          n = 10
          m = ceiling(real(modPetCount(i))/real(n))
          write(msgString,'(a)') trim(cname)//': '//modNameUC(i)//' PET list:'
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
            msg=trim(cname)//': '//modNameUC(i)//' PET list ids must be > 0 and < # PETs')
          return  ! bail out
        endif
        do j = 1,modPetCount(i)
          if (count(modPetList.eq.modPetList(j)).gt.1) then
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
              msg=trim(cname)//': '//modNameUC(i)//' PET list has duplicate entries')
            return  ! bail out
          endif
        enddo
      enddo
#ifdef MODULE_OBG
      allocate(superIS%wrap%modelPetLists(obg)%petList(modPetCount(atm)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of "//modNameUC(obg)//" PET list array failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      superIS%wrap%modelPetLists(obg)%petList(:) = superIS%wrap%modelPetLists(atm)%petList(:)
#endif
#ifdef MODULE_WBG
      allocate(superIS%wrap%modelPetLists(wbg)%petList(modPetCount(atm)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of "//modNameUC(wbg)//" PET list array failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      superIS%wrap%modelPetLists(wbg)%petList(:) = superIS%wrap%modelPetLists(atm)%petList(:)
#endif

    ! unsupported petLayoutOption
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
       msg=trim(cname)//': pet_layout_option not supported: '//trim(petLayoutOption))
      return  ! bail out
    end select

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                            :: localrc, stat
    type(type_InternalState)           :: superIS
    integer                            :: i, j
    type(ESMF_GridComp), pointer       :: modComp(:)
    type(ESMF_CplComp), pointer        :: conComp(:,:)
    type(NUOPC_RunSequence), pointer   :: runSeq(:)
    character(ESMF_MAXSTR)             :: verbosity

    rc = ESMF_SUCCESS

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, superIS, rc)
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
      call ESMF_GridCompSet(modComp(i), name=modNameUC(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSet: ',i,', '//modNameUC(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(modComp(i), name="Verbosity", value=trim(verbosity), &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//modNameUC(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_GridCompSet(modComp(i), config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,1i2,a)') 'Set config: ',i,', '//modNameUC(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo

    ! set connector component names and attributes
    do j = 1,modCount
    do i = 1,modCount
      call ESMF_CplCompSet(conComp(i,j), name=conNameUC(i,j), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,2i2,a)') 'ESMF_CplCompSet: ',i,j,', '//conNameUC(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_AttributeSet(conComp(i,j), name="Verbosity", value=trim(verbosity), &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//conNameUC(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
      call ESMF_CplCompSet(conComp(i,j), config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) then
        write(msgString,'(a,2i2,a)') 'Set config: ',i,j,', '//conNameUC(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return  ! bail out
      endif
    enddo
    enddo

    ! SetServices for active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      select case (modNameLC(i))
#ifdef MODULE_MED
      case ('med')
        call ESMF_GridCompSetServices(modComp(i), medSS, userRc=localrc, rc=rc)
#endif
#ifdef MODULE_ATM
      case ('atm')
        call ESMF_GridCompSetServices(modComp(i), atmSS, userRc=localrc, rc=rc)
#endif
#ifdef MODULE_OCN
      case ('ocn')
        call ESMF_GridCompSetServices(modComp(i), ocnSS, userRc=localrc, rc=rc)
#endif
#ifdef MODULE_WAV
      case ('wav')
        call ESMF_GridCompSetServices(modComp(i), wavSS, userRc=localrc, rc=rc)
#endif
#ifdef MODULE_ICE
      case ('ice')
        call ESMF_GridCompSetServices(modComp(i), iceSS, userRc=localrc, rc=rc)
#endif
#ifdef MODULE_LND
      case ('lnd')
        call ESMF_GridCompSetServices(modComp(i), lndSS, userRc=localrc, rc=rc)
#endif
#ifdef MODULE_OBG
      case ('obg')
        call ESMF_GridCompSetServices(modComp(i), obgSS, userRc=localrc, rc=rc)
#endif
#ifdef MODULE_WBG
      case ('wbg')
        call ESMF_GridCompSetServices(modComp(i), wbgSS, userRc=localrc, rc=rc)
#endif
      end select
      if (ESMF_LogFoundError(rcToCheck=rc,      msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc) .or. &
          ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME, rcToReturn=rc)) then
        write(msgString,'(a,1i2,a)') 'ESMF_GridCompSetServices: ',i,', '//modNameUC(i)
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
        write(msgString,'(a,2i2,a)') 'ESMF_CplCompSetServices: ',i,j,', '//conNameUC(i,j)
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
#ifdef MODULE_MED
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

end module
