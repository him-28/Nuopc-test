#include "settings.h"

module lndFields

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use lndLogger, only : log_info, log_error, abort_error

  implicit none
  
  private

  public fieldBundle_ini
  public fieldBundle_fin
  public fieldBundle_log
  public fieldBundle_fill
  public fieldList
  public type_lnd_field

  type type_lnd_field
    character(len=32)      :: fieldName
  end type type_lnd_field

  type(type_lnd_field),dimension(14) :: fieldList = (/ &
      type_lnd_field(fieldName='infexs') &
    , type_lnd_field(fieldName='sdrain') &
    , type_lnd_field(fieldName='stemp1') &
    , type_lnd_field(fieldName='stemp2') &
    , type_lnd_field(fieldName='stemp3') &
    , type_lnd_field(fieldName='stemp4') &
    , type_lnd_field(fieldName='smois1') &
    , type_lnd_field(fieldName='smois2') &
    , type_lnd_field(fieldName='smois3') &
    , type_lnd_field(fieldName='smois4') &
    , type_lnd_field(fieldName='sliqm1') &
    , type_lnd_field(fieldName='sliqm2') &
    , type_lnd_field(fieldName='sliqm3') &
    , type_lnd_field(fieldName='sliqm4') &
    /)

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine fieldBundle_ini(fieldBundle,start,grid,rc)
    ! ARGUMENTS
    type(ESMF_FieldBundle),intent(out) :: fieldBundle
    integer,intent(in)                 :: start(2)
    type(ESMF_Grid),intent(in)         :: grid
    integer,intent(out)                :: rc
    ! LOCAL VARIABLES
    integer          :: fIndex
    type(ESMF_Field) :: field

    rc = ESMF_SUCCESS

    fieldBundle = ESMF_FieldBundleCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    do fIndex=1, size(fieldList)
      field = ESMF_FieldCreate(grid=grid &
        , typekind=DEFAULT_TYPEKIND &
        , name=fieldList(fIndex)%fieldName,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call field_missing(field, value=DEFAULT_MISSING, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_FieldBundleAdd(fieldBundle, fieldList=(/field/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo
  end subroutine fieldBundle_ini

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_fin(fieldBundle,rc)
    ! ARGUMENTS
    type(ESMF_FieldBundle),intent(inout) :: fieldBundle
    integer,intent(out)                  :: rc
    ! LOCAL VARIABLES
    logical                      :: isCreated
    integer                      :: fCount
    type(ESMF_Field),allocatable :: fieldList(:)
    integer                      :: fIndex

    rc = ESMF_SUCCESS

    isCreated=ESMF_FieldBundleIsCreated(fieldBundle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (isCreated) then
      call ESMF_FieldBundleGet(fieldBundle, fieldCount=fCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      allocate(fieldList(fCount),stat=rc)
      if ( rc /= 0 ) call abort_error("allocate fieldList error")

      call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      do fIndex=1, fCount
        call ESMF_FieldDestroy(fieldList(fIndex),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      enddo

      deallocate(fieldList,stat=rc)
      if ( rc /= 0 ) call abort_error("deallocate fieldList error")

      call ESMF_FieldBundleDestroy(fieldBundle, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
  end subroutine fieldBundle_fin

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_log(fieldBundle)
    ! ARGUMENTS
    type(ESMF_FieldBundle),intent(in) :: fieldBundle
    ! LOCAL VAIRABLES
    integer                      :: rc
    character(ESMF_MAXSTR)       :: fbName
    integer                      :: fCount
    type(ESMF_Field),allocatable :: fieldList(:)
    integer                      :: fIndex

    call ESMF_FieldBundleGet(fieldBundle, fieldCount=fCount, name=fbName, rc=rc)
    if ( rc /= 0 ) call abort_error("field bundle get error")

    call log_info(TRIM(fbName)//".count",fCount)

    allocate(fieldList(fCount),stat=rc)
    if ( rc /= 0 ) call abort_error("allocate fieldList error")

    call ESMF_FieldBundleGet(fieldBundle &
      , itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=fieldList, rc=rc)
    if ( rc /= 0 ) call abort_error("field bundle get error")

    do fIndex=1, fCount
      call field_log(fieldList(fIndex))
    enddo

    deallocate(fieldList,stat=rc)
    if ( rc /= 0 ) call abort_error("deallocate fieldList error")
  end subroutine fieldBundle_log

  !-----------------------------------------------------------------------------

  subroutine field_log(field)
    ! ARGUMENTS
    type(ESMF_Field),intent(in) :: field
    ! LOCAL VAIRABLES
    integer                    :: rc
    character(ESMF_MAXSTR)     :: fname
    real(DEFAULT_KIND)         :: fmin
    real(DEFAULT_KIND)         :: fmax
    integer                    :: fedge(2)
    integer                    :: deCount
    integer                    :: dIndex
    real(DEFAULT_KIND),pointer :: farray(:,:)
    
    call ESMF_FieldGet(field, localDeCount=deCount, name=fname, rc=rc)
    if ( rc /= ESMF_SUCCESS) call log_error("field get error")

    nullify(farray)
    do dIndex=0, deCount-1
      call ESMF_FieldGet(field, localDe=dIndex, farrayPtr=farray, rc=rc)
      if ( rc /= ESMF_SUCCESS) call log_error("field get error")
      fmin = MINVAL(farray)
      fmax = MAXVAL(farray)
      fedge(1) = SIZE(farray,1)
      fedge(2) = SIZE(farray,2)
      call log_info(TRIM(fname)//".lclmin",fmin)
      call log_info(TRIM(fname)//".lclmax",fmax)
      call log_info(TRIM(fname)//".lcl_edg_x",fedge(1))
      call log_info(TRIM(fname)//".lcl_edg_y",fedge(2))
      nullify(farray)
    enddo
  end subroutine field_log

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_missing(fieldBundle,value,rc)
    ! ARGUMENTS
    type(ESMF_FieldBundle),intent(inout) :: fieldBundle
    real(DEFAULT_KIND),intent(in)        :: value
    integer,intent(out)                  :: rc
    ! LOCAL VAIRABLES
    integer                      :: fCount
    type(ESMF_Field),allocatable :: fieldList(:)
    integer                      :: fIndex

    call ESMF_FieldBundleGet(fieldBundle, fieldCount=fCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    allocate(fieldList(fCount),stat=rc)
    if ( rc /= 0 ) call abort_error("allocate fieldList error")

    call ESMF_FieldBundleGet(fieldBundle &
      , itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    do fIndex=1, fCount
      call field_missing(fieldList(fIndex), value=value, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

    deallocate(fieldList,stat=rc)
    if ( rc /= 0 ) call abort_error("deallocate fieldList error")

  end subroutine fieldBundle_missing

  !-----------------------------------------------------------------------------

  subroutine field_missing(field,value,rc)
    ! ARGUMENTS
    type(ESMF_Field),intent(inout) :: field
    real(DEFAULT_KIND),intent(in)  :: value
    integer,intent(out)            :: rc
    ! LOCAL VAIRABLES
    integer                      :: deCount
    integer                      :: dIndex
    real(DEFAULT_KIND),pointer   :: farray(:,:)

    rc = ESMF_SUCCESS

    nullify(farray)
    call ESMF_FieldGet(field, localDeCount=deCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    do dIndex=0, deCount-1
      call ESMF_FieldGet(field, localDe=dIndex, farrayPtr=farray, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      farray=value
      nullify(farray)
    enddo
  end subroutine field_missing

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_fill(fieldBundle,start,step,msWait,rc)
    ! ARGUMENTS
    type(ESMF_FieldBundle),intent(inout) :: fieldBundle
    integer,intent(in)                   :: start(2)
    integer,intent(in)                   :: step
    integer,intent(in),optional          :: msWait
    integer,intent(out)                  :: rc
    ! LOCAL VAIRABLES
    integer                      :: fCount
    type(ESMF_Field),allocatable :: fieldList(:)
    real                         :: stime
    real                         :: ctime
    logical                      :: sleep
    integer                      :: fIndex

    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fieldBundle, fieldCount=fCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    allocate(fieldList(fCount),stat=rc)
    if ( rc /= 0 ) call abort_error("allocate fieldList error")

    call ESMF_FieldBundleGet(fieldBundle &
      , itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    do fIndex=1, fCount
      call field_fill(fieldList(fIndex), start=start &
        , member=fIndex, step=step, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    enddo

    deallocate(fieldList,stat=rc)
    if ( rc /= 0 ) call abort_error("deallocate fieldList error")

    ! BUSY WAIT LOOP
    if ( present(msWait) ) then
      call cpu_time(stime)
      sleep = .true.
      do while ( sleep )
        call cpu_time(ctime)
        if ( (ctime-stime) >= (msWait/1000) ) sleep = .false.
      end do
    endif
  end subroutine fieldBundle_fill

  !-----------------------------------------------------------------------------

  subroutine field_fill(field,start,member,step,rc)
    ! ARGUMENTS
    type(ESMF_Field),intent(inout) :: field
    integer,intent(in)             :: start(2)
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
          sin(real(member)*3.1416*(start(1)+i+real(step))/180.)&
          * &
          cos(real(member)*3.1416*(start(2)+j+real(step))/180.)&
          , ESMF_KIND_R4)
      enddo
      enddo
      nullify(dataPtrR4D2)
    enddo

 end subroutine field_fill

  !-----------------------------------------------------------------------------

end module lndFields
