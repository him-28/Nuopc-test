#include "settings.h"

program offlineApp

  !-----------------------------------------------------------------------------
  ! Generic offline application
  !-----------------------------------------------------------------------------

  use ESMF
  use lndDriver,  only : lnd_ini, lnd_run, lnd_fin
  use lndWrapper, only : lndSS => SetServices

  implicit none

  ! Local Variables
  integer                    :: rc, urc
  type(ESMF_GridComp)        :: phyComp
  type(ESMF_GridComp)        :: wrtComp
  type(ESMF_VM)              :: vm
  type(ESMF_State)           :: phyState
  type(ESMF_State)           :: wrtState
  type(ESMF_RouteHandle)     :: rh
  integer                    :: petCount
  integer                    :: localPet
  integer                    :: argCount
  character(len=ESMF_MAXSTR) :: configFile
  type(ESMF_Config)          :: config
  character(len=ESMF_MAXSTR) :: label
  logical                    :: isPresent
  character(len=ESMF_MAXSTR) :: value
  logical                    :: opt_esmf_async
  integer                    :: petListBounds(2)
  integer,allocatable        :: petList(:)
  integer                    :: pIndex
 
  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=DEFAULT_LOGKIND, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  ! Determine the local PET identifier
  call ESMF_VMGetGlobal(vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm=vm, petCount=petCount, localPet=localPet, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Read the config filename from the command line arguments
  if (localPet .eq. DEFAULT_ROOT) then
    configFile = DEFAULT_CONFIG
    call ESMF_UtilGetArgC(argCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (argCount.gt.0) then
      call ESMF_UtilGetArg(1, argValue=configFile, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
  endif
  call ESMF_VMBroadcast(vm=vm, bcstData=configFile, count=ESMF_MAXSTR, &
    rootPet=DEFAULT_ROOT, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create config
  config = ESMF_ConfigCreate(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ConfigLoadFile(config, trim(configFile), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! get Async from config file
  label="WriteESMF_Async:"
  call ESMF_ConfigFindLabel(config, label=TRIM(label) &
    , isPresent=isPresent, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if ( isPresent ) then
    call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    select case (value)
      case ('true','TRUE','True','t','T','1' )
        opt_esmf_async = .true.
      case ('false','FALSE','False','f','F','0' )
        opt_esmf_async = .false.
      case default
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Invalid "//TRIM(label), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endselect
  else
    opt_esmf_async = DEFAULT_ESMF_ASYNC
  endif

  if ( opt_esmf_async ) then
    call ESMF_LogWrite("lndApp STARTING", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get petlist from config file
    label="LND_petlist_bounds:"
    call ESMF_ConfigFindLabel(config, label=TRIM(label) &
      , isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if ( isPresent ) then
      call ESMF_ConfigGetAttribute(config, petListBounds &
        , label=TRIM(label), default=-1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
        petListBounds(1) = 0
        petListBounds(2) = petCount - 1
      endif
      allocate(petList(petListBounds(2)-petListBounds(1)+1),stat=rc)
      if (ESMF_LogFoundAllocError(statusToCheck=rc, &
        msg="Allocation of petList memory failed.", &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do pIndex=petListBounds(1), petListBounds(2)
        petList(pIndex-petListBounds(1)+1) = pIndex ! PETs are 0 based
      enddo
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Missing "//TRIM(label), &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    ! Create the earth system Component
    phyComp = ESMF_GridCompCreate(name="PHYSICS", petList=petList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    deallocate(petlist,stat=rc)
    if (ESMF_LogFoundDeallocError(statusToCheck=rc, &
      msg="Deallocation of petlist memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get petlist from config file
    label="WRITER_petlist_bounds:"
    call ESMF_ConfigFindLabel(config, label=TRIM(label) &
      , isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if ( isPresent ) then
      call ESMF_ConfigGetAttribute(config, petListBounds &
        , label=TRIM(label), default=-1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
        petListBounds(1) = 0
        petListBounds(2) = petCount - 1
      endif
      allocate(petList(petListBounds(2)-petListBounds(1)+1),stat=rc)
      if (ESMF_LogFoundAllocError(statusToCheck=rc, &
        msg="Allocation of petList memory failed.", &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      do pIndex=petListBounds(1), petListBounds(2)
        petList(pIndex-petListBounds(1)+1) = pIndex ! PETs are 0 based
      enddo
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Missing "//TRIM(label), &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    ! Create the earth system Component
    wrtComp = ESMF_GridCompCreate(name="WRITER", petList=petList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    deallocate(petlist,stat=rc)
    if (ESMF_LogFoundDeallocError(statusToCheck=rc, &
      msg="Deallocation of petlist memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    ! SetServices for the earth system Component
    call ESMF_GridCompSetServices(phyComp, lndSS, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! SetServices for the earth system Component
    call ESMF_GridCompSetServices(wrtComp, lndSS, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Add RH to state for each component
    phyState = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!    call ESMF_StateAdd(phyState, routehandleList=(/rh/), rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    wrtState = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!    call ESMF_StateAdd(wrtState, routehandleList=(/rh/), rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Call Initialize for the earth system Component
    call ESMF_GridCompInitialize(phyComp, importState=phyState &
      , exportState=phyState, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
    ! Call Initialize for the earth system Component
    call ESMF_GridCompInitialize(wrtComp, importState=wrtState &
      , exportState=wrtState, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Reconcile physics state across all PETS
!    call statereconcile
    ! Reconcile writer state across all PETS
!    call statereconcile

    ! Get fieldBundle from physics state
!    call stateget(physicsState, fieldBundle)
    ! Get fieldBundle from writer state
!    call stateget(writerState, fieldBundle)

    ! Create fieldBundle redist routeHandle
!    call fieldbundlerediststore(rh)

    ! Call Run  for earth the system Component
    call ESMF_GridCompRun(phyComp, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Call Run  for earth the system Component
    call ESMF_GridCompRun(wrtComp, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Call Finalize for the earth system Component
    call ESMF_GridCompFinalize(phyComp, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Call Finalize for the earth system Component
    call ESMF_GridCompFinalize(wrtComp, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(phyState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(wrtState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Destroy the earth system Component
    call ESMF_GridCompDestroy(phyComp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Destroy the earth system Component
    call ESMF_GridCompDestroy(wrtComp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_LogWrite("lndApp FINISHED", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  else
    ! Initialize Applicaiton
    call lnd_ini(rc=rc)
    ! Run Application
    call lnd_run(rc=rc)
    ! Finalize Application
    call lnd_fin(rc=rc)
  endif

  call ESMF_ConfigDestroy(config,rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize()

end program

