module lndState

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndLogger, only : log_ini, log_fin, log_info, log_error

  implicit none
  
  private

  public state
  public state_ini
  public state_fin

  type type_lnd_state
    logical :: initialized = .false.
    integer :: lnd_mpi_com =  0
    integer :: lcl_pet_id  = -1
    integer :: gbl_pet_cnt =  0
    type(ESMF_State)    :: writeState
    type(ESMF_DistGrid) :: distGrid
    type(ESMF_Grid)     :: grid
    type(ESMF_Field)    :: sfctmp
    type(ESMF_Field)    :: infexs
  end type type_lnd_state

  type(type_lnd_state) :: state

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine state_ini(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    type(ESMF_VM) :: vm
    logical       :: isInit
    logical       :: isCreated

    call ESMF_VMGetGlobal(vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    isCreated = ESMF_VMIsCreated(vm=vm, rc=rc)
    if (.NOT.isCreated) then
      call state_reset()
      call log_error(msg="state_ini failed")
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
      return
    endif

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

    state%distGrid = ESMF_DistGridCreate(minIndex=(/1,1/) &
      ,maxIndex=(/628,628/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    state%grid = ESMF_GridCreate(distgrid=state%distGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    state%sfctmp = ESMF_FieldCreate(grid=state%grid &
      ,typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    state%infexs = ESMF_FieldCreate(grid=state%grid &
      ,typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    state%initialized = .true.

  end subroutine state_ini

  !-----------------------------------------------------------------------------

  subroutine state_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    call state_reset()
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine state_fin

  !-----------------------------------------------------------------------------

  subroutine state_reset()

    state%lnd_mpi_com =  0
    state%lcl_pet_id  = -1
    state%gbl_pet_cnt =  0
    state%initialized = .false.

  end subroutine state_reset

  !-----------------------------------------------------------------------------

end module
