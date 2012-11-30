#define FILENAME "con.F90"

module CON

  !-----------------------------------------------------------------------------
  ! COAMPS Connector Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => routine_SetServices, &
    con_type_IS         => type_InternalState, &
    con_label_IS        => label_InternalState, &
    con_label_ComputeRH => label_ComputeRouteHandle, &
    con_label_ExecuteRH => label_ExecuteRouteHandle, &
    con_label_ReleaseRH => label_ReleaseRouteHandle
  use UTIL
  
  implicit none
  
  private
  
  public SetServices

  character (*), parameter :: defaultVerbosity = "low"
  character (*), parameter :: label_InternalState = "CON_InternalState"

! Mask codes
  integer, parameter :: MASK_INLAND_WATER =  -1
  integer, parameter :: MASK_WATER        =   0
  integer, parameter :: MASK_LAND         =   1
  integer, parameter :: MASK_FROZEN_WATER =   2
  integer, parameter :: MASK_FROZEN_LAND  =   3

! Values to mask out
  integer :: mask(4) = (/ MASK_INLAND_WATER, &
                          MASK_LAND,         &
                          MASK_FROZEN_WATER, &
                          MASK_FROZEN_LAND   /)

  type type_InternalStateStruct
    logical :: verbose
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(ccomp, rc)
    type(ESMF_CplComp)  :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    character(ESMF_MAXSTR)        :: verbosity
    type(type_InternalState)      :: is
    integer                       :: localrc, stat

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! determine verbosity
    call ESMF_AttributeGet(ccomp, name="Verbosity", value=verbosity, &
      defaultValue=defaultVerbosity, convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (trim(verbosity)=="high") then
      verbose = .true.
    else
      verbose = .false.
    endif

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered SetServices', ESMF_LOGMSG_INFO)

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    is%wrap%verbose = verbose

    ! the NUOPC connector component will register the generic methods
    call con_routine_SS(ccomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! attach specializing method to compute the connection RouteHandle
    call ESMF_MethodAdd(ccomp, label=con_label_ComputeRH, &
      userRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! attach specializing method to execute the connection RouteHandle
    call ESMF_MethodAdd(ccomp, label=con_label_ExecuteRH, &
      userRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! attach specializing method to release the connection RouteHandle
    call ESMF_MethodAdd(ccomp, label=con_label_ReleaseRH, &
      userRoutine=ReleaseRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving SetServices', ESMF_LOGMSG_INFO)

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ComputeRH(ccomp, rc)
    type(ESMF_CplComp)  :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(con_type_IS)             :: superIS
    type(type_InternalState)      :: is
    integer                       :: localrc, stat
    integer                         :: i
    integer                         :: cplCount
    character(ESMF_MAXSTR), pointer :: cplList(:)
    integer                         :: srcCount, dstCount
    character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered ComputeRH', ESMF_LOGMSG_INFO)
    
    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(ccomp, con_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! report the cplList and FieldBundle lists
    call NUOPC_CplCompAttributeGet(ccomp, cplListSize=cplCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_FieldBundleGet(superIS%wrap%srcFields, fieldCount=srcCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_FieldBundleGet(superIS%wrap%dstFields, fieldCount=dstCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    if (cplCount.ne.srcCount .or. cplCount.ne.dstCount) then
      write(msgString,'(a)') trim(cname)//': cplList count does not agree with FieldBundle counts'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
      return  ! bail out
    endif
    write(msgString,'(a,i0,a)') trim(cname)//': List of coupled fields (',cplCount,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    if (cplCount.gt.0) then
      write(msgString,'(a,a5,a,a20,a,a20,a,a)') &
        trim(cname)//': ','index',' ','srcField',' ','dstField','   ','standardName'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
      allocate(cplList(cplCount), srcList(cplCount), dstList(cplCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of cplList() failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
      call NUOPC_CplCompAttributeGet(ccomp, cplList=cplList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_FieldBundleGet(superIS%wrap%srcFields, fieldNameList=srcList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_FieldBundleGet(superIS%wrap%dstFields, fieldNameList=dstList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      do i=1, cplCount
        write(msgString,'(a,i5,a,a20,a,a20,a,a)') &
          trim(cname)//': ',i,' ',trim(srcList(i)),' ',trim(dstList(i)),'   ',trim(cplList(i))
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
      enddo
      deallocate(cplList, srcList, dstList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of cplList() failed.", &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    endif

    ! store regrid
    call ESMF_FieldBundleRegridStore(superIS%wrap%srcFields, superIS%wrap%dstFields, &
      routehandle=superIS%wrap%rh, unmappedAction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving ComputeRH', ESMF_LOGMSG_INFO)
 
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ExecuteRH(ccomp, rc)
    type(ESMF_CplComp)  :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(con_type_IS)             :: superIS
    type(type_InternalState)      :: is
    integer                       :: localrc, stat

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered ExecuteRH', ESMF_LOGMSG_INFO)

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(ccomp, con_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! apply regrid
    select case (cname(1:3))
    case ('OBG','WBG')
      call ESMF_FieldBundleRegrid(superIS%wrap%srcFields, superIS%wrap%dstFields, &
        routehandle=superIS%wrap%rh, zeroRegion=ESMF_REGION_TOTAL, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    case default
      call ESMF_FieldBundleRegrid(superIS%wrap%srcFields, superIS%wrap%dstFields, &
        routehandle=superIS%wrap%rh, zeroRegion=ESMF_REGION_SELECT, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    end select

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving ExecuteRH', ESMF_LOGMSG_INFO)
 
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(ccomp, rc)
    type(ESMF_CplComp)  :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(con_type_IS)             :: superIS
    type(type_InternalState)      :: is
    integer                       :: localrc, stat

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered ReleaseRH', ESMF_LOGMSG_INFO)

    ! query component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(ccomp, con_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! release regrid
    call ESMF_FieldBundleRegridRelease(superIS%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving ReleaseRH', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: localrc, stat

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite('>>>'//trim(cname)//' entered Finalize', ESMF_LOGMSG_INFO)

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

end module
