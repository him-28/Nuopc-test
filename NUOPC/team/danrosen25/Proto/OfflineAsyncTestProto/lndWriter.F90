#include "settings.h"

module lndWriter

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NETCDF
  use MPI
  use lndState, only : state_get_bnds
  use lndLogger, only : log_info, log_error, log_warning, abort_error

  implicit none

  private

  public SetServices
  public writer_ini
  public writer_fin
  public writer_run
  public writer_log

  type type_writer
    logical             :: initialized      = .false.
    real(ESMF_KIND_R8)  :: time_start       = 0.0
    real(ESMF_KIND_R8)  :: time_end         = 0.0
    real(ESMF_KIND_R8)  :: time_step        = 0.0
    real(ESMF_KIND_R8)  :: time_next        = 0.0
    character(len=3)    :: padding = "1"
    logical :: opt_esmf_fbwrt   = DEFAULT_ESMF_FBWRT
    logical :: opt_esmf_async   = DEFAULT_ESMF_ASYNC
    logical :: opt_nc_par       = DEFAULT_NC_PAR
    integer :: opt_nc_partype   = DEFAULT_NC_PARTYPE
    integer :: opt_nc_paraccess = DEFAULT_NC_PARACCESS
    integer :: lcl_pet_id       = -1
    integer :: root             = DEFAULT_ROOT
    integer :: mpi_wrt_comm     = -1
    type(ESMF_FieldBundle) :: fields
    type(ESMF_VM) :: writer_vm
  end type type_writer

  type(type_writer) :: this
  integer,parameter :: WRTR_NETCDF4MPIIO = IOR(NF90_NETCDF4,NF90_MPIIO)
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(write_cmp, rc)
    type(ESMF_GridComp)  :: write_cmp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(write_cmp, ESMF_METHOD_INITIALIZE, &
      userRoutine=esmf_write_ini, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("write set services error")

    call ESMF_GridCompSetEntryPoint(write_cmp, ESMF_METHOD_RUN, &
      userRoutine=esmf_write_run, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("write set services error")

    call ESMF_GridCompSetEntryPoint(write_cmp, ESMF_METHOD_FINALIZE, &
      userRoutine=esmf_write_fin, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("write set services error")
  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine esmf_write_ini(write_cmp,imp_state,exp_state,clock,rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: write_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    type(ESMF_FieldBundle)                  :: fields
    integer                                 :: itemCount
    character (len=ESMF_MAXSTR),allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag),allocatable   :: itemTypeList(:)
    integer                                 :: iIndex
    logical                                 :: isPresent
    type(ESMF_Field)                        :: field
    logical                                 :: configIsPresent
    type(ESMF_Config)                       :: config
    type(ESMF_TimeInterval)                 :: timeStep
    type(ESMF_Time)                         :: startTime
    type(ESMF_Time)                         :: stopTime
    real(ESMF_KIND_R8)                      :: time_start
    real(ESMF_KIND_R8)                      :: time_end
    real(ESMF_KIND_R8)                      :: time_step

    rc = ESMF_SUCCESS

    fields = ESMF_FieldBundleCreate(rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error("field bundle create error")

    ! add imp_state fields to field bundle
    call ESMF_StateGet(imp_state, itemCount=itemCount, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error("state get error")
    allocate(itemNameList(itemCount),stat=rc)
    if ( rc /= 0 ) call log_error("allocate error")
    allocate(itemTypeList(itemCount),stat=rc)
    if ( rc /= 0 ) call log_error("allocate error")
    call ESMF_StateGet(imp_state, itemorderflag=ESMF_ITEMORDER_ADDORDER &
      , itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error("state get error")
    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD ) then
        call ESMF_FieldBundleGet(fields, fieldName=itemNameList(iIndex) &
          , isPresent=isPresent, rc=rc)
        if ( rc /= 0 ) call log_error("field bundle get error")
        if ( .NOT. isPresent ) then
          call ESMF_StateGet(imp_state, itemName=itemNameList(iIndex) &
            , field=field, rc=rc)
          if ( rc /= ESMF_SUCCESS ) call log_error("state get error")
          call ESMF_FieldBundleAdd(fields, fieldList=(/field/), rc=rc)
          if ( rc /= ESMF_SUCCESS ) call log_error("field bundle add error")
        endif
      endif
    enddo
    deallocate(itemNameList,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate error")
    deallocate(itemTypeList,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate error")

    ! add exp_state fields to field bundle
    call ESMF_StateGet(exp_state, itemCount=itemCount, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error("state get error")
    allocate(itemNameList(itemCount),stat=rc)
    if ( rc /= 0 ) call log_error("allocate error")
    allocate(itemTypeList(itemCount),stat=rc)
    if ( rc /= 0 ) call log_error("allocate error")
    call ESMF_StateGet(exp_state, itemorderflag=ESMF_ITEMORDER_ADDORDER &
      , itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error("state get error")
    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD ) then
        call ESMF_FieldBundleGet(fields, fieldName=itemNameList(iIndex) &
          , isPresent=isPresent, rc=rc)
        if ( rc /= 0 ) call log_error("field bundle get error")
        if ( .NOT. isPresent ) then
          call ESMF_StateGet(exp_state, itemName=itemNameList(iIndex) &
            , field=field, rc=rc)
          if ( rc /= ESMF_SUCCESS ) call log_error("state get error")
          call ESMF_FieldBundleAdd(fields, fieldList=(/field/), rc=rc)
          if ( rc /= ESMF_SUCCESS ) call log_error("field bundle add error")
        endif
      endif
    enddo
    deallocate(itemNameList,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate error")
    deallocate(itemTypeList,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate error")

    ! convert clock to time_start, time_end, and time_step
    call ESMF_ClockGet(clock, timeStep=timeStep, startTime=startTime &
      , stopTime=stopTime, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("clock get error")
    call ESMF_TimeIntervalGet(timeStep, s_r8=time_step, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("time_interval get error")
    call ESMF_TimeGet(startTime, s_r8=time_start, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("time get error")
    call ESMF_TimeGet(stopTime, s_r8=time_end, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("time get error")

    ! check write_cmp for config
    call ESMF_GridCompGet(write_cmp, configIsPresent=configIsPresent, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("grid comp get error")

    ! get config and call writer_inid
    if (configIsPresent) then
      call ESMF_GridCompGet(write_cmp, config=config, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error("grid comp get error")
      call writer_ini(fields, config=config &
        , time_start=time_start, time_end=time_end &
        , time_step=time_step, rc=rc)
      if ( rc /= 0 ) call log_error("write ini error")      
    else
      call writer_ini(fields, time_start=time_start, time_end=time_end &
        , time_step=time_step, rc=rc)
      if ( rc /= 0 ) call log_error("write ini error")
    endif

  end subroutine esmf_write_ini

  !-----------------------------------------------------------------------------

  subroutine esmf_write_run(write_cmp,imp_state,exp_state,clock,rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: write_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc
    ! LOCAL VARIABLES
    type(ESMF_Time)    :: currTime
    real(ESMF_KIND_R8) :: current_time

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("clock get error")
    call ESMF_TimeGet(currTime, s_r8=current_time, rc=rc)
    if( rc /= ESMF_SUCCESS ) call log_error("time get error")

    call writer_run(current_time, rc=rc)
    if ( rc /= 0 ) call log_error("write run error")

  end subroutine esmf_write_run

  !-----------------------------------------------------------------------------

  subroutine esmf_write_fin(write_cmp,imp_state,exp_state,clock, rc)
    ! ARGUMENTS
    type(ESMF_GridComp) :: write_cmp
    type(ESMF_State)    :: imp_state
    type(ESMF_State)    :: exp_state
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call writer_fin(rc=rc)
    if ( rc /= 0 ) call log_error("write fin error")

    call ESMF_FieldBundleDestroy(this%fields, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error("field bundle destroy error")

  end subroutine esmf_write_fin

  !-----------------------------------------------------------------------------

  subroutine writer_ini(fields,config,time_start,time_end,time_step,rc)
    ! ARGUMENTS
    type(ESMF_FieldBundle),intent(in)        :: fields
    type(ESMF_Config),intent(inout),optional :: config
    real(ESMF_KIND_R8),intent(in),optional   :: time_start
    real(ESMF_KIND_R8),intent(in),optional   :: time_end
    real(ESMF_KIND_R8),intent(in),optional   :: time_step
    integer,intent(out)                      :: rc
    ! LOCAL VARIABLES
    logical                    :: isPresent
    character(len=ESMF_MAXSTR) :: label
    character(len=ESMF_MAXSTR) :: value
    character(len=10)          :: sStr
    character(len=10)          :: eStr
    integer                    :: mpi_curr_comm

    rc = ESMF_SUCCESS

    if ( present(config) ) then
      ! get WriteStart from config file
      label="WriteStart:"
      call ESMF_ConfigFindLabel(config, label=TRIM(label) &
        , isPresent=isPresent, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
      if ( isPresent ) then
        call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
        read (value,*,iostat=rc) this%time_start
        if ( rc /= 0 ) call log_error (trim(label)//" value error")
      else
        if ( present(time_start) ) then
          this%time_start = time_start
        else
          call log_error(trim(label)//" is missing")
        endif
      endif
      ! get WriteEnd from config file
      label="WriteEnd:"
      call ESMF_ConfigFindLabel(config, label=TRIM(label) &
        , isPresent=isPresent, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
      if ( isPresent ) then
        call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
        read (value,*,iostat=rc) this%time_end
        if ( rc /= 0 ) call log_error (trim(label)//" value error")
      else
        if ( present(time_end) ) then
          this%time_end = time_end
        else
          call log_error(trim(label)//" is missing")
        endif
      endif
      ! get WriteStep from config file
      label="WriteStep:"
      call ESMF_ConfigFindLabel(config, label=TRIM(label) &
        , isPresent=isPresent, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
      if ( isPresent ) then
        call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
        read (value,*,iostat=rc) this%time_step
        if ( rc /= 0 ) call log_error (trim(label)//" value error")
      else
        if ( present(time_step) ) then
          this%time_step = time_step
        else
          call log_error(trim(label)//" is missing")
        endif
      endif
      ! get WriteESMF_FB from config file
      label="WriteESMF_FB:"
      call ESMF_ConfigFindLabel(config, label=TRIM(label) &
        , isPresent=isPresent, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
      if ( isPresent ) then
        call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
        select case (value)
        case ('true','TRUE','True','t','T','1' )
          this%opt_esmf_fbwrt = .true.
        case ('false','FALSE','False','f','F','0' )
          this%opt_esmf_fbwrt = .false.
        case default
         if ( rc /= 0 ) call log_error (trim(label)//" value error")
        endselect
      else
        this%opt_esmf_fbwrt = DEFAULT_ESMF_FBWRT
      endif
      ! get Async from config file
      label="WriteESMF_Async:"
      call ESMF_ConfigFindLabel(config, label=TRIM(label) &
        , isPresent=isPresent, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
      if ( isPresent ) then
        call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
        select case (value)
        case ('true','TRUE','True','t','T','1' )
          this%opt_esmf_async = .true.
        case ('false','FALSE','False','f','F','0' )
          this%opt_esmf_async = .false.
        case default
          if ( rc /= 0 ) call log_error (trim(label)//" value error")
        endselect
      else
        this%opt_esmf_async = DEFAULT_ESMF_ASYNC
      endif
      ! get Parallel from config file
      label="WriteNC_Par:"
      call ESMF_ConfigFindLabel(config, label=TRIM(label) &
        , isPresent=isPresent, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
      if ( isPresent ) then
        call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
        select case (value)
        case ('true','TRUE','True','t','T','1' )
          this%opt_nc_par = .true.
        case ('false','FALSE','False','f','F','0' )
          this%opt_nc_par = .false.
        case default
          if ( rc /= 0 ) call log_error (trim(label)//" value error")
        endselect
      else
        this%opt_nc_par = DEFAULT_NC_PAR
      endif
      ! get parallel file type from config file
      label="WriteNC_ParType:"
      call ESMF_ConfigFindLabel(config, label=TRIM(label) &
        , isPresent=isPresent, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
      if ( isPresent ) then
        call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
        this%opt_nc_partype = ESMF_UtilString2Int(value &
         , specialStringList=(/"NF90_NETCDF4|NC_MPIIO","NC_NETCDF4|NC_MPIIO" &
                              ,"NF90_NETCDF4",         "NC_NETCDF4" &
                              ,"NF90_HDF5",            "NC_HDF5" &
                              ,"NF90_MPIIO",           "NC_MPIIO" & 
                              ,"NF90_PNETCDF",         "NC_PNETCDF"/) &
         , specialValueList=(/WRTR_NETCDF4MPIIO,WRTR_NETCDF4MPIIO &
                             ,NF90_NETCDF4,     NF90_NETCDF4 &
                             ,NF90_HDF5,        NF90_HDF5 &
                             ,NF90_MPIIO,       NF90_MPIIO &
                             ,NF90_PNETCDF,     NF90_PNETCDF/) &
         , rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error (trim(label)//" value error")
      else
        this%opt_nc_partype = DEFAULT_NC_PARTYPE
      endif
      ! get parallel access from config file
      label="WriteNC_ParAccess:"
      call ESMF_ConfigFindLabel(config, label=TRIM(label) &
        , isPresent=isPresent, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" find error")
      if ( isPresent ) then
        call ESMF_ConfigGetAttribute(config, value, label=TRIM(label), rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error(trim(label)//" get error")
        this%opt_nc_paraccess = ESMF_UtilString2Int(value &
         , specialStringList=(/"NF90_INDEPENDENT","NC_INDEPENDENT" &
                              ,"NF90_COLLECTIVE" ,"NC_COLLECTIVE"/) &
         , specialValueList=(/NF90_INDEPENDENT,NF90_INDEPENDENT &
                             ,NF90_COLLECTIVE ,NF90_COLLECTIVE/) &
         , rc=rc)
        if ( rc /= ESMF_SUCCESS ) call log_error (trim(label)//" value error")
      else
        this%opt_nc_paraccess = DEFAULT_NC_PARACCESS
      endif
    else
      if ( present(time_start) ) then
        this%time_start = time_start
      else
        call log_error("writer.time_start is missing")
      endif
      if ( present(time_end) ) then
        this%time_end = time_end
      else
        call log_error("writer.time_end is missing")
      endif
      if ( present(time_step) ) then
        this%time_step = time_step
      else
        call log_error("writer.time_step is missing")
      endif
      this%opt_esmf_fbwrt   = DEFAULT_ESMF_FBWRT
      this%opt_esmf_async   = DEFAULT_ESMF_ASYNC
      this%opt_nc_par       = DEFAULT_NC_PAR
      this%opt_nc_partype   = DEFAULT_NC_PARTYPE
      this%opt_nc_paraccess = DEFAULT_NC_PARACCESS
    endif ! if present(config)

    if ( this%time_step == 0 ) then
      call writer_fin(rc)
      call log_error("writer.time_step cannot be zero")
    else if ( this%time_start < this%time_end ) then
      if ( this%time_step < 0 ) then
        call writer_fin(rc)
        call log_error("writer.time_step must be positive")
        rc = ESMF_RC_ARG_OUTOFRANGE
      else
        this%time_next  = this%time_start + this%time_step
        this%initialized  = .true.
      endif
    else
      if ( this%time_step > 0 ) then
        call writer_fin(rc)
        call log_error("writer.time_step must be negative")
        rc = ESMF_RC_ARG_OUTOFRANGE
      else
        this%time_next = this%time_start + this%time_step
        this%initialized = .true.
      endif
    endif

    write(sStr,"(I0)") INT(this%time_start)
    write(eStr,"(I0)") INT(this%time_end)
    write(this%padding,"(I3)") MAX(LEN_TRIM(ADJUSTL(sStr)) &
                                  ,LEN_TRIM(ADJUSTL(eStr)))

    call ESMF_VMGetCurrent(vm=this%writer_vm,rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error("current vm get error")

    call ESMF_VMGet(this%writer_vm, localPet=this%lcl_pet_id &
      , mpiCommunicator=mpi_curr_comm, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call log_error("vm get error")

    call mpi_comm_dup(mpi_curr_comm,this%mpi_wrt_comm,rc)
    if ( rc /= MPI_SUCCESS ) call log_error("mpi comm dup error")

    this%fields = fields

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine writer_run(current_time,rc)
    ! ARGUMENTS
    real(ESMF_KIND_R8),intent(in) :: current_time
    integer,intent(out)           :: rc
    ! LOCAL VARIABLES
    character(len=10) :: clockStr
    character(len=32) :: fname

    rc = ESMF_SUCCESS

    if ( .NOT. this%initialized ) then
      call log_error(msg="writer is not initialized")
      rc = ESMF_RC_OBJ_INIT
    else if ( current_time >= this%time_start & 
      .and. current_time <= this%time_end &
      .and. current_time >= this%time_next ) then

      write(clockStr,"(I0."//TRIM(this%padding)//")") INT(current_time)
      clockStr = ADJUSTL(clockStr)
      write(fname,"('OUTPUT',A,'.nc')") TRIM(clockStr)
      if( this%opt_esmf_async ) then
        call log_info("write esmf_async: "//TRIM(fname))
        T_ENTER("async")
        call write_esmf_async(fname,rc=rc)
        T_EXIT("async")
      else if ( this%opt_esmf_fbwrt ) then
        call log_info("write esmf_fbwrt: "//TRIM(fname))
        T_ENTER("fbwrt")
        call write_esmf_fbwrt(fname,rc=rc)
        T_EXIT("fbwrt")
      else if ( this%opt_nc_par ) then
        call log_info("write nc_par: "//TRIM(fname))
        T_ENTER("nc_par")
        call write_nc_parallel(fname,rc=rc)
        T_EXIT("nc_par")
      else
        call log_info("write gather: "//TRIM(fname))
        T_ENTER("gather")
        call write_nc_gather(fname,rc=rc)
        T_EXIT("gather")
      endif
      this%time_next = this%time_next + this%time_step
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine writer_fin(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    if ( this%initialized ) then
      call mpi_comm_free(this%mpi_wrt_comm,rc)
      if ( rc /= MPI_SUCCESS ) call log_error("mpi comm free error")
    endif
    this%lcl_pet_id       = -1
    this%root             = DEFAULT_ROOT
    this%padding          = "1"
    this%time_start       = 0.0
    this%time_end         = 0.0
    this%time_step        = 0.0
    this%time_next        = 0.0
    this%opt_esmf_fbwrt   = DEFAULT_ESMF_FBWRT
    this%opt_esmf_async   = DEFAULT_ESMF_ASYNC
    this%opt_nc_par       = DEFAULT_NC_PAR
    this%opt_nc_partype   = DEFAULT_NC_PARTYPE
    this%opt_nc_paraccess = DEFAULT_NC_PARACCESS
    this%mpi_wrt_comm     = -1
    this%initialized      = .false.
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine writer_log(rc)
    ! ARGUMENTS
    integer,intent(out) :: rc

    rc = ESMF_SUCCESS

    call log_info("writer.initialized     ",this%initialized)
    call log_info("writer.time_start      ",this%time_start)
    call log_info("writer.time_end        ",this%time_end)
    call log_info("writer.time_step       ",this%time_step)
    call log_info("writer.time_next       ",this%time_next)
    call log_info("writer.opt_esmf_fbwrt  ",this%opt_esmf_fbwrt)
    call log_info("writer.opt_esmf_async  ",this%opt_esmf_async)
    call log_info("writer.opt_nc_par      ",this%opt_nc_par)
    call log_info("writer.opt_nc_partype  ",this%opt_nc_partype)
    call log_info("writer.opt_nc_paraccess",this%opt_nc_paraccess)
    call log_info("writer.mpi_wrt_comm    ",this%mpi_wrt_comm)
  end subroutine writer_log

  !-----------------------------------------------------------------------------

  subroutine write_esmf_async(fname,rc)
    ! ARGUMENTS
    character(len=*),intent(in)       :: fname
    integer,intent(out)               :: rc

    rc = ESMF_SUCCESS

    call log_warning("write esmf_async is not implemented ")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine write_esmf_fbwrt(fname,rc)
    ! ARGUMENTS
    character(len=*),intent(in)       :: fname
    integer,intent(out)               :: rc

    rc = ESMF_SUCCESS

    call ESMF_FieldBundleWrite(this%fields, fileName=fname, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call abort_error("field bundle write error")

  end subroutine write_esmf_fbwrt

  !-----------------------------------------------------------------------------

  subroutine write_nc_gather(fname,rc)
    ! ARGUMENTS
    character(len=*),intent(in) :: fname
    integer,intent(out)         :: rc
    ! LOCAL VARIABLES
    integer                        :: fCount
    type(ESMF_Field),allocatable   :: fieldList(:)
    character(len=ESMF_MAXSTR)     :: fieldName
    integer                        :: ncId
    integer                        :: gbl_edg(2)
    integer,allocatable            :: varId(:)
    integer                        :: dimIds(2)
    integer                        :: fIndex
    real(DEFAULT_KIND),allocatable :: wrtData(:,:)

    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(this%fields, fieldCount=fCount, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call abort_error("get fieldCount error")
    allocate(fieldList(fCount),stat=rc)
    if ( rc /= 0 ) call abort_error("allocate field list error")
    allocate(varID(fCount),stat=rc)
    if ( rc /= 0 ) call abort_error("allocate var ID error")

    call ESMF_FieldBundleGet(this%fields, itemorderflag=ESMF_ITEMORDER_ADDORDER &
      , fieldList=fieldList, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call abort_error("get field list error")

    call state_get_bnds(gbl_edg=gbl_edg, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call abort_error("get bnds error")

    if ( this%lcl_pet_id == this%root ) then
      ! create netcdf file and define variables
      rc = nf90_create(path=fname, cmode=NF90_CLOBBER, ncid=ncId)
      if ( rc /= nf90_NoErr ) call log_error("netcdf create error")
      rc = nf90_def_dim(ncId, "x", gbl_edg(1), dimIds(2) )
      if ( rc /= nf90_NoErr ) call log_error("netcdf define x error")
      rc = nf90_def_dim(ncId, "y", gbl_edg(2), dimIds(1) )
      if ( rc /= nf90_NoErr ) call log_error("netcdf define y error")
      do fIndex=1, fCount
        call ESMF_FieldGet(fieldList(fIndex), name=fieldName, rc=rc)
        if ( rc /= ESMF_SUCCESS ) call abort_error("get field name error")
        rc = nf90_def_var(ncId, fieldName, NF90_FLOAT, dimIds, varId(fIndex))
        if(rc /= nf90_NoErr) call log_error("define "//TRIM(fname)//" error")
      end do
      rc = nf90_enddef(ncId)
      if ( rc /= nf90_NoErr ) call log_error("netcdf end define error")

      ! allocate memory for gather
      allocate(wrtData(gbl_edg(1),gbl_edg(2)),stat=rc)
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

    do fIndex = 1, fCount
      call ESMF_FieldGet(fieldList(fIndex), name=fieldName, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call abort_error("get field name error")
      call ESMF_FieldGather(fieldList(fIndex), farray=wrtData &
        , rootPet=this%root, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call abort_error("ESMF_FieldGather error")
      if ( this%lcl_pet_id == this%root ) then
        rc = nf90_put_var(ncId, varID(fIndex), wrtData)
        if ( rc /= nf90_NoErr ) call log_error("netcdf write error")
      endif
    enddo

    deallocate (fieldList,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate field list error")
    deallocate (varID,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate var ID list error")
    deallocate(wrtData,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate write data error")

    if ( this%lcl_pet_id == this%root ) then
      rc = nf90_close(ncId)
      if ( rc /= nf90_NoErr ) call log_error("netcdf close error")
    endif

  end subroutine write_nc_gather

  !-----------------------------------------------------------------------------

  subroutine write_nc_parallel(fname,rc)
    ! ARGUMENTS
    character(len=*),intent(in)       :: fname
    integer,intent(out)               :: rc
    ! LOCAL VARIABLES
    integer                      :: fCount
    type(ESMF_Field),allocatable :: fieldList(:)
    character(len=ESMF_MAXSTR)   :: fieldName
    integer                      :: ncId
    integer                      :: gbl_edg(2)
    integer                      :: lcl_min(2)
    integer                      :: lcl_edg(2)
    integer,allocatable          :: varId(:)
    integer                      :: dimIds(2)
    integer                      :: fIndex
    real(DEFAULT_KIND),pointer   :: wrtData(:,:)

    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(this%fields, fieldCount=fCount, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call abort_error("get fieldCount error")
    allocate(fieldList(fCount),stat=rc)
    if ( rc /= 0 ) call abort_error("allocate field list error")
    allocate(varID(fCount),stat=rc)
    if ( rc /= 0 ) call abort_error("allocate var ID error")

    call ESMF_FieldBundleGet(this%fields &
      , itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=fieldList, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call abort_error("get field list error")

    call state_get_bnds(gbl_edg=gbl_edg, lcl_min=lcl_min &
      , lcl_edg=lcl_edg, rc=rc)
    if ( rc /= ESMF_SUCCESS ) call abort_error("get bnds error")

    rc = nf90_create(path=fname &
      , cmode=IOR(NF90_CLOBBER,this%opt_nc_partype) &
      , ncId=ncid, comm=this%mpi_wrt_comm, info=MPI_INFO_NULL)
    if ( rc /= nf90_NoErr ) call log_error("netcdf create error")
    rc = nf90_def_dim(ncId, "x", gbl_edg(1), dimIds(2) )
    if ( rc /= nf90_NoErr ) call log_error("netcdf define x error")
    rc = nf90_def_dim(ncId, "y", gbl_edg(2), dimIds(1) )
    if ( rc /= nf90_NoErr ) call log_error("netcdf define y error")
    do fIndex=1, fCount
      call ESMF_FieldGet(fieldList(fIndex), name=fieldName, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call abort_error("get field name error")
      rc = nf90_def_var(ncId, fieldName, NF90_FLOAT, dimIds, varID(fIndex))
      if ( rc /= nf90_NoErr ) call log_error("define "//TRIM(fname)//" error")
    end do
    rc = nf90_enddef(ncId)
    if ( rc /= nf90_NoErr ) call log_error("netcdf end define error")

    do fIndex=1, fCount
      call ESMF_FieldGet(fieldList(fIndex), name=fieldName, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call abort_error("get field name error")
      nullify(wrtData)
      call ESMF_FieldGet(fieldList(fIndex), farrayPtr=wrtData, rc=rc)
      if ( rc /= ESMF_SUCCESS ) call abort_error("get field data error")
      rc = nf90_var_par_access(ncId,varID(fIndex),this%opt_nc_paraccess)
      if ( rc /= nf90_NoErr ) call log_error("netcdf collective access error")
      rc = nf90_put_var(ncId, varId(fIndex), wrtData &
        , start=(/lcl_min(1),lcl_min(2)/) &
        , count=(/lcl_edg(1),lcl_edg(2)/))
      if ( rc /= nf90_NoErr ) call log_error("netcdf write error")
    end do

    deallocate (fieldList,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate field list error")
    deallocate (varID,stat=rc)
    if ( rc /= 0 ) call log_error("deallocate var ID list error")

    rc = nf90_close(ncId)
    if ( rc /= nf90_NoErr ) call log_error("netcdf close error")

  end subroutine write_nc_parallel

  !-----------------------------------------------------------------------------

end module lndWriter
