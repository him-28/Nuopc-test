#include "settings.h"

module lndState

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndLogger, only : log_info, log_error, abort_error
  use lndClock,  only : type_clock, clock_ini, clock_fin, clock_log
  use lndFields, only : fieldBundle_ini, fieldBundle_fin, fieldBundle_log &
    , fieldBundle_fill

  implicit none
  
  private

  public state
  public state_ini
  public state_fin
  public state_log
  public state_get_bnds
  public type_lnd_state

  type type_lnd_state
    logical                :: initialized   = .false.
    integer                :: mpi_lnd_comm  = -1
    integer                :: lcl_pet_id    = -1
    integer                :: lcl_de_id     = -1
    integer                :: gbl_pet_cnt   = -1
    integer                :: root          = DEFAULT_ROOT
    integer                :: gbl_min(2)    = (/1,1/)
    integer                :: gbl_max(2)    = (/DEFAULT_X,DEFAULT_Y/)
    integer                :: gbl_edg(2)    = (/-1,-1/)
    integer                :: gbl_cnt       = DEFAULT_X*DEFAULT_Y
    character(len=64)      :: gbl_bnds      = "(?:?,?:?)"
    integer                :: lcl_min(2)    = (/-1,-1/)
    integer                :: lcl_max(2)    = (/-1,-1/)
    integer                :: lcl_edg(2)    = (/-1,-1/)
    integer                :: lcl_cnt       = -1
    character(len=64)      :: lcl_bnds      = "(?:?,?:?)"
    integer,allocatable    :: all_min(:,:)
    integer,allocatable    :: all_max(:,:)
    integer,allocatable    :: all_cnt(:)
    integer                :: step       = 1
    type(type_clock)       :: clock 
    type(ESMF_Config)      :: config
    type(ESMF_DistGrid)    :: distGrid
    type(ESMF_Grid)        :: grid
    type(ESMF_FieldBundle) :: fields
  end type type_lnd_state

  type(type_lnd_state) :: state

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine state_ini(rc)
    ! ARGUMENTS
    integer,intent(out)         :: rc
    ! LOCAL VARIABLES
    type(ESMF_VM)              :: vm
    type(ESMF_DELayout)        :: delayout
    logical                    :: isCreated
    logical                    :: isPresent
    integer                    :: argCount
    character(len=ESMF_MAXSTR) :: configFile
    character(len=ESMF_MAXSTR) :: value
    character(len=ESMF_MAXSTR) :: label
    real(ESMF_KIND_R8)         :: time_start
    real(ESMF_KIND_R8)         :: time_end
    real(ESMF_KIND_R8)         :: time_step
    integer                    :: deCount
    integer                    :: localDeCount
    integer                    :: dimCount
    integer                    :: tileCount
    logical                    :: regDecompFlag
    integer                    :: localDeToDeMap(1)

    ! set root
    state%root = DEFAULT_ROOT

    ! check to see if ESMF is initialized
    call ESMF_VMGetGlobal(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    isCreated = ESMF_VMIsCreated(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (.NOT.isCreated) call abort_error(msg="state_ini ESMF not initialized")

    ! get pet and communicator information
    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_VMGet(vm &
      ,localPet=state%lcl_pet_id &
      ,petCount=state%gbl_pet_cnt &
      ,mpiCommunicator=state%mpi_lnd_comm &
      ,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create config
    state%config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (state%lcl_pet_id .eq. state%root) then
      configFile = DEFAULT_CONFIG
      call ESMF_UtilGetArgC(argCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (argCount.gt.0) then
        call ESMF_UtilGetArg(1, argValue=configFile, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif
    call ESMF_VMBroadcast(vm=vm, bcstData=configFile, count=ESMF_MAXSTR, &
      rootPet=state%root, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ConfigLoadFile(state%config, configFile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! get ClockStart from config file
    label="ClockStart:"
    call ESMF_ConfigFindLabel(state%config, label=TRIM(label) &
      , isPresent=isPresent, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
    if ( isPresent ) then
      call ESMF_ConfigGetAttribute(state%config, value &
        , label=TRIM(label), rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
      read (value,*,iostat=rc) time_start
      if ( rc /= 0 ) call log_error (trim(label)//" value error")
    else
      call log_error(TRIM(label)//" is missing")
    endif
    ! get ClockEnd from config file
    label="ClockEnd:"
    call ESMF_ConfigFindLabel(state%config, label=TRIM(label) &
      , isPresent=isPresent, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
    if ( isPresent ) then
      call ESMF_ConfigGetAttribute(state%config, value &
        , label=TRIM(label), rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
      read (value,*,iostat=rc) time_end
      if ( rc /= 0 ) call log_error (trim(label)//" value error")
    else
      call log_error(TRIM(label)//" is missing")
    endif
    ! get ClockStep from config file
    label="ClockStep:"
    call ESMF_ConfigFindLabel(state%config, label=TRIM(label) &
      , isPresent=isPresent, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
    if ( isPresent ) then
      call ESMF_ConfigGetAttribute(state%config, value &
        , label=TRIM(label), rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
      read (value,*,iostat=rc) time_step
      if ( rc /= 0 ) call log_error (trim(label)//" value error")
    else
      call log_error(TRIM(label)//" is missing")
    endif

    ! create the clock
    call clock_ini(state%clock,time_start,time_end,time_step,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! create distGrid
    state%distGrid = ESMF_DistGridCreate( &
       minIndex=(/state%gbl_min(1),state%gbl_min(2)/) &
      ,maxIndex=(/state%gbl_max(1),state%gbl_max(2)/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    state%gbl_edg(1) = state%gbl_max(1)-state%gbl_min(1)+1
    state%gbl_edg(2) = state%gbl_max(2)-state%gbl_min(2)+1
    write(state%gbl_bnds,"('(',I0,':',I0,',',I0,':',I0,')')") &
      state%gbl_min(1),state%gbl_max(1),state%gbl_min(2),state%gbl_max(2)
    ! get dimCount, tileCount, deCount, regDecomp, localDeCount
    call ESMF_DistGridGet(state%distGrid, delayout=delayout &
      , dimCount=dimCount, tileCount=tileCount, deCount=deCount &
      , regDecompFlag=regDecompFlag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DELayoutGet(delayout, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! check regDecomp, dimCount, tileCount, localDeCount
    if ( .NOT. regDecompFlag ) then
      call abort_error("state_ini regDecomp required")
    elseif ( dimCount /= 2 ) then
      call abort_error("state_ini two dimensions required")
    elseif ( tileCount /= 1 ) then
      call abort_error("state_ini one tile required")
    elseif ( localDeCount /= 1 ) then
      call abort_error("state_ini one DE per PET required")
    endif
    ! allocate space for all de min, max, and cnt
    allocate(state%all_min(dimCount,0:deCount-1), stat=rc)
    if ( rc /= 0 ) call abort_error("state_ini alloc all_min")
    allocate(state%all_max(dimCount,0:deCount-1), stat=rc)
    if ( rc /= 0 ) call abort_error("state_ini alloc all_max")
    allocate(state%all_cnt(0:deCount-1), stat=rc)
    if ( rc /= 0 ) call abort_error("state_ini alloc all_cnt")
    ! get all de min, max, and cnt
    call ESMF_DistGridGet(state%distGrid, minIndexPDe=state%all_min &
      , maxIndexPDe=state%all_max, elementCountPDe=state%all_cnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! get local de information
    call ESMF_DELayoutGet(delayout, localDeToDeMap=localDeToDeMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    state%lcl_de_id = localDeToDeMap(1)
    state%lcl_min = state%all_min(:,state%lcl_de_id)
    state%lcl_max = state%all_max(:,state%lcl_de_id)
    state%lcl_cnt = state%all_cnt(state%lcl_de_id)
    state%lcl_edg(1) = state%lcl_max(1)-state%lcl_min(1)+1
    state%lcl_edg(2) = state%lcl_max(2)-state%lcl_min(2)+1
    write(state%lcl_bnds,"('(',I0,':',I0,',',I0,':',I0,')')") &
      state%lcl_min(1),state%lcl_max(1),state%lcl_min(2),state%lcl_max(2)

    ! create grid
    state%grid = ESMF_GridCreate(distgrid=state%distGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call fieldBundle_ini(fieldBundle=state%fields, start=state%lcl_min &
      , grid=state%grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    state%initialized = .true.

  end subroutine state_ini

  !-----------------------------------------------------------------------------

  subroutine state_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call fieldBundle_fin(state%fields,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridDestroy(grid=state%grid)
    call ESMF_DistGridDestroy(distgrid=state%distgrid)
    call ESMF_ConfigDestroy(config=state%config)
    call clock_fin(clock=state%clock)
    state%step     = 1
    deallocate(state%all_cnt)
    deallocate(state%all_max)
    deallocate(state%all_min)
    state%lcl_bnds = "(?:?,?:?)"
    state%lcl_cnt = -1
    state%lcl_edg = (/-1,-1/)
    state%lcl_max = (/-1,-1/)
    state%lcl_min = (/-1,-1/)
    state%gbl_bnds = "(?:?,?:?)"
    state%gbl_cnt = DEFAULT_X*DEFAULT_Y
    state%gbl_edg = (/-1,-1/)
    state%gbl_max = (/DEFAULT_X,DEFAULT_Y/)
    state%gbl_max = (/1,1/)
    state%root    = DEFAULT_ROOT
    state%gbl_pet_cnt  = -1
    state%lcl_de_id    = -1
    state%lcl_pet_id   = -1
    state%mpi_lnd_comm  = -1
    state%initialized  = .false.
  end subroutine state_fin

  !-----------------------------------------------------------------------------

  subroutine state_log()
    call log_info("state.initialized ",state%initialized)
    call log_info("state.mpi_lnd_comm",state%mpi_lnd_comm)
    call log_info("state.lcl_pet_id  ",state%lcl_pet_id)
    call log_info("state.lcl_de_id   ",state%lcl_de_id)
    call log_info("state.gbl_pet_cnt ",state%gbl_pet_cnt)
    call log_info("state.root        ",state%root)
    call log_info("state.gbl_min_x   ",state%gbl_min(1))
    call log_info("state.gbl_max_x   ",state%gbl_max(1))
    call log_info("state.gbl_min_y   ",state%gbl_min(2))
    call log_info("state.gbl_max_y   ",state%gbl_max(2))
    call log_info("state.gbl_edg_x   ",state%gbl_edg(1))
    call log_info("state.gbl_edg_y   ",state%gbl_edg(2))
    call log_info("state.gbl_cnt     ",state%gbl_cnt)
    call log_info("state.lcl_min_x   ",state%lcl_min(1))
    call log_info("state.lcl_max_x   ",state%lcl_max(1))
    call log_info("state.lcl_min_y   ",state%lcl_min(2))
    call log_info("state.lcl_max_y   ",state%lcl_max(2))
    call log_info("state.lcl_edg_x   ",state%lcl_edg(1))
    call log_info("state.lcl_edg_y   ",state%lcl_edg(2))
    call log_info("state.lcl_cnt     ",state%lcl_cnt)
!    state%all_min
!    state%all_max
!    state%all_cnt
    call log_info("state.step        ",state%step)
    call clock_log(clock=state%clock)
!    state%distgrid
!    state%grid
!    state%fields
  end subroutine state_log

  !-----------------------------------------------------------------------------

  subroutine state_get_bnds(gbl_min,gbl_max,gbl_edg,lcl_min,lcl_max,lcl_edg,rc)
    ! ARGUMENTS
    integer,intent(out),optional :: gbl_min(2)
    integer,intent(out),optional :: gbl_max(2)
    integer,intent(out),optional :: gbl_edg(2)
    integer,intent(out),optional :: lcl_min(2)
    integer,intent(out),optional :: lcl_max(2)
    integer,intent(out),optional :: lcl_edg(2)
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    if ( .NOT. state%initialized ) rc = ESMF_RC_OBJ_INIT

    if ( present(gbl_min) ) gbl_min = state%gbl_min
    if ( present(gbl_max) ) gbl_max = state%gbl_max
    if ( present(gbl_edg) ) gbl_edg = state%gbl_edg
    if ( present(lcl_min) ) lcl_min = state%lcl_min
    if ( present(lcl_max) ) lcl_max = state%lcl_max
    if ( present(lcl_edg) ) lcl_edg = state%lcl_edg
  end subroutine state_get_bnds

  !-----------------------------------------------------------------------------

end module lndState
