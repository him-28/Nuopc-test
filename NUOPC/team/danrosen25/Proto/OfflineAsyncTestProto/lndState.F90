#include "settings.h"

module lndState

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndLogger, only : log_info, log_error, abort_error
  use lndClock, only : type_clock, clock_reset

  implicit none
  
  private

  public state
  public state_ini
  public state_fin
  public state_log
  public type_lnd_state
  public field_fill

  type type_lnd_state
    logical             :: initialized  = .false.
    integer             :: lnd_mpi_com  = -1
    integer             :: lcl_pet_id   = -1
    integer             :: lcl_de_id    = -1
    integer             :: gbl_pet_cnt  = -1
    integer             :: root         = DEFAULT_ROOT
    integer             :: gbl_max(2)   = (/DEFAULT_X,DEFAULT_Y/)
    integer             :: gbl_cnt      = DEFAULT_X*DEFAULT_Y
    integer             :: lcl_min(2)   = (/-1,-1/)
    integer             :: lcl_max(2)   = (/-1,-1/)
    integer             :: lcl_edg(2)   = (/-1,-1/)
    integer             :: lcl_cnt      = -1
    integer,allocatable :: all_min(:,:)
    integer,allocatable :: all_max(:,:)
    integer,allocatable :: all_cnt(:)
    integer             :: step         = 1
    logical             :: async        = DEFAULT_ASYNC
    logical             :: parallel     = DEFAULT_PARALLEL
    type(type_clock)    :: clock 
    type(ESMF_Config)   :: config
    type(ESMF_DistGrid) :: distGrid
    type(ESMF_Grid)     :: grid
    type(ESMF_Field)    :: sfctmp
    type(ESMF_Field)    :: infexs
  end type type_lnd_state

  type(type_lnd_state) :: state

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine state_ini(clock,rc)
    ! ARGUMENTS
    type(type_clock),intent(in) :: clock
    integer,intent(out)         :: rc
    ! LOCAL VARIABLES
    type(ESMF_VM)              :: vm
    type(ESMF_DELayout)        :: delayout
    logical                    :: isCreated
    integer                    :: argCount
    character(len=ESMF_MAXSTR) :: configFile
    character(len=ESMF_MAXSTR) :: value
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
    if (.NOT.isCreated) then
      call abort_error(msg="state_ini ESMF not initialized")
    endif

    ! get pet and communicator information
    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_VMGet(vm &
      ,localPet=state%lcl_pet_id &
      ,petCount=state%gbl_pet_cnt &
      ,mpiCommunicator=state%lnd_mpi_com &
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

    ! Get async, parallel
    call ESMF_ConfigGetAttribute(state%config, value, default="f", &
      label='Async:', rc=rc)
    select case (value)
    case ('true','TRUE','True','t','T','1' )
      state%async = .true.
    case default
      state%async = .false.
    endselect
    call ESMF_ConfigGetAttribute(state%config, value, default="f", &
      label='Parallel:', rc=rc)
    select case (value)
    case ('true','TRUE','True','t','T','1' )
      state%parallel = .true.
    case default
      state%parallel = .false.
    endselect

    ! create distGrid
    state%distGrid = ESMF_DistGridCreate(minIndex=(/1,1/) &
      ,maxIndex=(/state%gbl_max(1),state%gbl_max(2)/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
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

    ! create grid
    state%grid = ESMF_GridCreate(distgrid=state%distGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    state%sfctmp = ESMF_FieldCreate(grid=state%grid &
      ,typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call field_fill(state%sfctmp, member=1, step=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    state%infexs = ESMF_FieldCreate(grid=state%grid &
      ,typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call field_fill(state%infexs, member=2, step=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    state%clock = clock
    state%initialized = .true.

  end subroutine state_ini

  !-----------------------------------------------------------------------------

  subroutine state_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call state_reset()
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end subroutine state_fin

  !-----------------------------------------------------------------------------

  subroutine state_reset()
    call ESMF_FieldDestroy(field=state%infexs)
    call ESMF_FieldDestroy(field=state%sfctmp)
    call ESMF_GridDestroy(grid=state%grid)
    call ESMF_DistGridDestroy(distgrid=state%distgrid)
    call ESMF_ConfigDestroy(config=state%config)
    call clock_reset(clock=state%clock)
    state%async    = DEFAULT_ASYNC
    state%parallel = DEFAULT_PARALLEL
    state%step     = 1
    deallocate(state%all_cnt)
    deallocate(state%all_max)
    deallocate(state%all_min)
    state%lcl_cnt = -1
    state%lcl_edg = (/-1,-1/)
    state%lcl_max = (/-1,-1/)
    state%lcl_min = (/-1,-1/)
    state%gbl_cnt = DEFAULT_X*DEFAULT_Y
    state%gbl_max = (/DEFAULT_X,DEFAULT_Y/)
    state%root    = DEFAULT_ROOT
    state%gbl_pet_cnt  = -1
    state%lcl_de_id    = -1
    state%lcl_pet_id   = -1
    state%lnd_mpi_com  = -1
    state%initialized  = .false.
  end subroutine state_reset

  !-----------------------------------------------------------------------------

  subroutine state_log()
    call log_info("initialized",state%initialized)
    call log_info("lnd_mpi_com",state%lnd_mpi_com)
    call log_info("lcl_pet_id ",state%lcl_pet_id)
    call log_info("lcl_de_id  ",state%lcl_de_id)
    call log_info("gbl_pet_cnt",state%gbl_pet_cnt)
    call log_info("root       ",state%root)
    call log_info("gbl_max_x  ",state%gbl_max(1))
    call log_info("gbl_max_y  ",state%gbl_max(2))
    call log_info("gbl_cnt    ",state%gbl_cnt)
    call log_info("lcl_min_x  ",state%lcl_min(1))
    call log_info("lcl_max_x  ",state%lcl_max(1))
    call log_info("lcl_min_y  ",state%lcl_min(2))
    call log_info("lcl_max_y  ",state%lcl_max(2))
    call log_info("lcl_edg_x  ",state%lcl_edg(1))
    call log_info("lcl_edg_y  ",state%lcl_edg(2))
    call log_info("lcl_cnt    ",state%lcl_cnt)
    call log_info("async      ",state%async)
    call log_info("parallel   ",state%parallel)
!    state%all_min
!    state%all_max
!    state%all_cnt
!    state%step
!    state%clock
!    state%distgrid
!    state%grid
!    state%sfctmp
!    state%infexs
  end subroutine state_log

  !-----------------------------------------------------------------------------

  subroutine field_fill(field,member,step,rc)
    ! ARGUMENTS
    type(ESMF_Field),intent(inout) :: field
    integer,intent(in)             :: member
    integer,intent(in)             :: step
    integer,intent(out)            :: rc
    ! LOCAL VAIRABLES
    type(ESMF_TypeKind_Flag)   :: typekind
    integer                    :: rank
    integer                    :: localDeCount
    real(ESMF_KIND_R4),pointer :: dataPtrR4D2(:,:)
    integer                    :: deIndex
    integer                    :: i,j

    call ESMF_FieldGet(field, typekind=typekind, rank=rank, &
      localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    nullify(dataPtrR4D2)
    do deIndex=0, localDeCount-1
      call ESMF_FieldGet(field, localDe=deIndex, farrayPtr=dataPtrR4D2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      do j=lbound(dataPtrR4D2,2),ubound(dataPtrR4D2,2)
      do i=lbound(dataPtrR4D2,1),ubound(dataPtrR4D2,1)
        dataPtrR4D2(i,j) = real( &
          sin(real(member)*3.1416*(state%lcl_min(1)+i+real(step))/180.)&
          * &
          cos(real(member)*3.1416*(state%lcl_min(2)+j+real(step))/180.)&
          , ESMF_KIND_R4)
      enddo
      enddo
      nullify(dataPtrR4D2)
    enddo

 end subroutine field_fill

  !-----------------------------------------------------------------------------

end module
