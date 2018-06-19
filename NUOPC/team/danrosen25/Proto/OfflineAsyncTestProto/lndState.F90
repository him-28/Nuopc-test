#include "settings.h"

module lndState

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndLogger, only : log_info, log_error
  use lndClock, only : type_clock, clock_reset

  implicit none
  
  private

  public state
  public state_ini
  public state_fin
  public type_lnd_state
  public field_fill

  type type_lnd_state
    logical             :: initialized  = .false.
    integer             :: lnd_mpi_com  =  0
    integer             :: lcl_pet_id   = -1
    integer             :: gbl_pet_cnt  =  0
    integer             :: root         =  0
    integer             :: gbl_x  = 628
    integer             :: gbl_y  = 628
    integer             :: gbl_xy = 628*628
    integer             :: step   = 1
    type(type_clock)    :: clock
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

  subroutine state_ini(clock,rc)
    ! ARGUMENTS
    type(type_clock),intent(in) :: clock
    integer,intent(out)         :: rc
    ! LOCAL VARIABLES
    type(ESMF_VM)       :: vm
    logical             :: isInit
    logical             :: isCreated
    integer,allocatable :: minIndexPDe(:,:)
    integer             :: deIndex

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
      ,maxIndex=(/state%gbl_x,state%gbl_y/), rc=rc)
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
    state%initialized  = .false.
    state%lnd_mpi_com  =   0
    state%lcl_pet_id   =  -1
    state%gbl_pet_cnt  =   0
    state%root         =   0
    state%gbl_x        = 628
    state%gbl_y        = 628
    state%gbl_xy       = 628*628
    state%step         = 1
    call clock_reset(clock=state%clock)
    call ESMF_FieldDestroy(field=state%infexs)
    call ESMF_FieldDestroy(field=state%sfctmp)
    call ESMF_GridDestroy(grid=state%grid)
    call ESMF_DistGridDestroy(distgrid=state%distgrid)
  end subroutine state_reset

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
          sin(real(member)*3.1416*(i+real(step))/180.)&
          * &
          cos(real(member)*3.1416*(j+real(step))/180.)&
          , ESMF_KIND_R4)
      enddo
      enddo
      nullify(dataPtrR4D2)
    enddo

 end subroutine field_fill

  !-----------------------------------------------------------------------------

end module
