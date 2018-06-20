#include "settings.h"

module lndWriter

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NETCDF
  use MPI
  use lndState
  use lndLogger
  use lndClock

  implicit none

  private

  public write_ini
  public write_fin
  public write_state

  type type_writer
    logical :: initialized  = .false.
    real*4  :: writer_start = 0.0
    real*4  :: writer_end   = 0.0
    real*4  :: writer_step  = 0.0
    real*4  :: writer_next  = 0.0
  end type type_writer

  type(type_writer) :: writer
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine write_ini(write_start,write_end,write_step,rc)
    ! ARGUMENTS
    real*4,intent(in)   :: write_start 
    real*4,intent(in)   :: write_end
    real*4,intent(in)   :: write_step
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    if ( write_step == 0 ) then
        call write_reset()
        call log_error("write_step cannot be zero")
    else if ( write_start < write_end ) then
      if ( write_step < 0 ) then
        call write_reset()
        call log_error("write_step must be positive")
        rc = ESMF_RC_ARG_OUTOFRANGE
      else
        writer%writer_start = write_start
        writer%writer_end   = write_end
        writer%writer_step  = write_step
        writer%writer_next  = write_start + write_step
        writer%initialized  = .true.
      endif
    else
      if ( write_step > 0 ) then
        call write_reset()
        call log_error("write_step must be negative")
        rc = ESMF_RC_ARG_OUTOFRANGE
      else
        writer%writer_start = write_start
        writer%writer_end   = write_end
        writer%writer_step  = write_step
        writer%writer_next  = write_start + write_step
        writer%initialized  = .true.
      endif
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine write_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call write_reset()
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine write_state(rc)
    ! ARGUMENTS
    integer,intent(out)             :: rc
    ! LOCAL VARIABLES
    character(len=10) :: sStr
    character(len=10) :: eStr
    character(len=3)  :: padding
    character(len=10) :: clockStr
    character(len=32) :: fname

    rc = ESMF_SUCCESS

    if ( .NOT. writer%initialized ) then
      call log_error(msg="writer is not initialized")
      rc = ESMF_RC_OBJ_INIT
    else if ( state%clock%current_time >= writer%writer_start & 
      .and. state%clock%current_time <= writer%writer_end &
      .and. state%clock%current_time >= writer%writer_next ) then
      write(sStr,"(I0)") INT(state%clock%start_time)
      write(eStr,"(I0)") INT(state%clock%end_time)
      write(padding,"(I3)") MAX(LEN_TRIM(ADJUSTL(sStr)),LEN_TRIM(ADJUSTL(eStr)))
      write(clockStr,"(I0."//TRIM(padding)//")") INT(state%clock%current_time)
      clockStr = ADJUSTL(clockStr)
      write(fname,"('OUTPUT',A,'.nc')") TRIM(clockStr)
      if( state%async ) then
        if ( state%parallel ) then
          call log_info("async_parallel write: "//TRIM(fname))
          call writer_async_parallel(fname,rc=rc)
        else
          call log_info("async_noParallel write: "//TRIM(fname))
          call writer_async_noParallel(fname,rc=rc)
        endif
      else
        if ( state%parallel ) then
          call log_info("noAsync_parallel write: "//TRIM(fname))
          call writer_noAsync_parallel(fname,rc=rc)
        else
          call log_info("noAsync_noParallel write: "//TRIM(fname))
          call writer_noAsync_noParallel(fname,rc=rc)
        endif
      endif      
      writer%writer_next = writer%writer_next + writer%writer_step
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine write_reset()
    writer%writer_start = 0.0
    writer%writer_end   = 0.0
    writer%writer_step  = 0.0
    writer%writer_next  = 0.0
    writer%initialized  = .false.
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine writer_async_noParallel(fname,rc)
    ! ARGUMENTS
    character(len=*),intent(in) :: fname
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_warning("async_noParallel is not implemented ")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine writer_async_parallel(fname,rc)
    ! ARGUMENTS
    character(len=*),intent(in) :: fname
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_warning("async_parallel is not implemented ")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine writer_noAsync_noParallel(fname,rc)
    ! ARGUMENTS
    character(len=*),intent(in) :: fname
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    real*4,allocatable  :: wrtData(:,:)
    integer             :: ncId
    integer             :: varInfexs
    integer             :: varSfctmp
    integer             :: dimIds(2)

    rc = ESMF_SUCCESS

    if ( .NOT.state%initialized ) then
        call log_error("state_write: state must be initialized.")
    endif
    if ( state%lcl_pet_id == state%root ) then
      ! create netcdf file and define variables
      rc = nf90_create(fname, NF90_CLOBBER, ncId)
      if ( rc /= nf90_NoErr ) call log_error("netcdf create error")
      rc = nf90_def_dim(ncId, "x", state%gbl_max(1), dimIds(2) )
      if ( rc /= nf90_NoErr ) call log_error("netcdf define x error")
      rc = nf90_def_dim(ncId, "y", state%gbl_max(2), dimIds(1) )
      if ( rc /= nf90_NoErr ) call log_error("netcdf define y error")
      rc = nf90_def_var(ncId, "infexs", NF90_FLOAT, dimIds, varInfexs)
      if ( rc /= nf90_NoErr ) call log_error("netcdf define infexs error")
      rc = nf90_def_var(ncId, "sfctmp", NF90_FLOAT, dimIds, varSfctmp)
      if ( rc /= nf90_NoErr ) call log_error("netcdf define sfctmp error")
      rc = nf90_enddef(ncId)
      if ( rc /= nf90_NoErr ) call log_error("netcdf end define error")
      ! allocate memory for gather
      allocate(wrtData(state%gbl_max(1),state%gbl_max(2)),stat=rc)
      if ( rc /= 0 ) then
        call log_error("error allocating memory for gather")
      endif
    else
      ! allocate zero memory for gather
      allocate(wrtData(0,0),stat=rc)
      if ( rc /= 0 ) then
        call log_error("error allocating memory for gather")
      endif
    endif

!    Note MPI_GATHERV cannot be used because decomposition may
!    not be a contiguos block in the recv array

    ! infexs
    call ESMF_FieldGather(state%infexs, farray=wrtData &
      , rootPet=state%root, rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
      call log_error("state_write ESMF_FieldGather error")
    endif
    if ( state%lcl_pet_id == state%root ) then
      rc = nf90_put_var(ncId, varInfexs, wrtData)
      if ( rc /= nf90_NoErr ) call log_error("netcdf write infexs error")
    endif

    ! sfctmp
    call ESMF_FieldGather(state%sfctmp, farray=wrtData &
      , rootPet=state%root, rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
      call log_error("state_write ESMF_FieldGather error")
    endif
    if ( state%lcl_pet_id == state%root ) then
      rc = nf90_put_var(ncId, varSfctmp, wrtData)
      if ( rc /= nf90_NoErr ) call log_error("netcdf write sfctmp error")
      rc = nf90_close(ncId)
      if ( rc /= nf90_NoErr ) call log_error("netcdf close error")
    endif

    deallocate(wrtData,stat=rc)
    if ( rc /= 0 ) then
      call log_error("error deallocating memory for gather")
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine writer_noAsync_parallel(fname,rc)
    ! ARGUMENTS
    character(len=*),intent(in) :: fname
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    real*4,pointer      :: wrtData(:,:)
    integer             :: ncId
    integer             :: varInfexs
    integer             :: varSfctmp
    integer             :: dimIds(2)
    integer             :: starts(2), counts(2) 

    rc = ESMF_SUCCESS

    if ( .NOT.state%initialized ) then
        call log_error("state_write: state must be initialized.")
    endif

    rc = nf90_create(path=fname &
      , cmode=IOR(IOR(NF90_CLOBBER,NF90_NETCDF4),NF90_MPIIO) &
      , ncId=ncid, comm=state%lnd_mpi_com, info=MPI_INFO_NULL)
    if ( rc /= nf90_NoErr ) call log_error("netcdf create error")
    rc = nf90_def_dim(ncId, "x", state%gbl_max(1), dimIds(2) )
    if ( rc /= nf90_NoErr ) call log_error("netcdf define x error")
    rc = nf90_def_dim(ncId, "y", state%gbl_max(2), dimIds(1) )
    if ( rc /= nf90_NoErr ) call log_error("netcdf define y error")
    rc = nf90_def_var(ncId, "infexs", NF90_FLOAT, dimIds, varInfexs)
    if ( rc /= nf90_NoErr ) call log_error("netcdf define infexs error")
    rc = nf90_def_var(ncId, "sfctmp", NF90_FLOAT, dimIds, varSfctmp)
    if ( rc /= nf90_NoErr ) call log_error("netcdf define sfctmp error")
    rc = nf90_enddef(ncId)
    if ( rc /= nf90_NoErr ) call log_error("netcdf end define error")

    ! infexs
    nullify(wrtData)
    call ESMF_FieldGet(state%infexs, farrayPtr=wrtData, rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
      call log_error("state_write ESMF_FieldGet error")
    endif
    rc = nf90_put_var(ncId, varInfexs, wrtData &
      , start=(/state%lcl_min(1),state%lcl_min(2)/) &
      , count=(/state%lcl_edg(1),state%lcl_edg(2)/))
    if ( rc /= nf90_NoErr ) call log_error("netcdf write infexs error")

    ! sfctmp
    nullify(wrtData)
    call ESMF_FieldGet(state%sfctmp, farrayPtr=wrtData, rc=rc)
    if ( rc /= ESMF_SUCCESS ) then
      call log_error("state_write ESMF_FieldGet error")
    endif
    rc = nf90_put_var(ncId, varSfctmp, wrtData &
      , start=(/state%lcl_min(1),state%lcl_min(2)/) &
      , count=(/state%lcl_edg(1),state%lcl_edg(2)/))
    if ( rc /= nf90_NoErr ) call log_error("netcdf write sfctmp error")

    rc = nf90_close(ncId)
    if ( rc /= nf90_NoErr ) call log_error("netcdf close error")

  end subroutine

  !-----------------------------------------------------------------------------

end module
