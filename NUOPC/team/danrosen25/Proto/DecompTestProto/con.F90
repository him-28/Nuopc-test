    ! Disabling the following macro, e.g. renaming to MEMCOPY_disable,
    ! will result in a model component that executes FieldBundleRedist 
    ! instead of locally copying memory.
#define MEMCOPY

module CON

  !-----------------------------------------------------------------------------
  ! Connector Component.
  !-----------------------------------------------------------------------------
  
  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => SetServices, &
    con_label_ComputeRH => label_ComputeRouteHandle, &
    con_label_ExecuteRH => label_ExecuteRouteHandle, &
    con_label_ReleaseRH => label_ReleaseRouteHandle, &
    NUOPC_ConnectorGet, NUOPC_ConnectorSet
  
  implicit none
  
  private
  
  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    type(ESMF_Field), allocatable :: srcFieldList(:)
    type(ESMF_Field), allocatable :: dstFieldList(:)
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type
 
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC connector component will register the generic methods
    call NUOPC_CompDerive(connector, con_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method to compute the connection RouteHandle
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ComputeRH, &
      specRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method to execute the connection RouteHandle
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ExecuteRH, &
      specRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method to release the connection RouteHandle
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ReleaseRH, &
      specRoutine=ReleaseRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ComputeRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    type(type_InternalState)      :: is
    type(ESMF_FieldBundle)        :: srcFields, dstFields
    type(ESMF_RouteHandle)        :: rh
    integer                       :: stat

    rc = ESMF_SUCCESS
    
#ifdef MEMCOPY
    ! allocate memory for internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_UserCompSetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call PROTO_FieldListStore(connector=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize with Redist, instead of the default Regrid
    call ESMF_FieldBundleRedistStore(srcFields, dstFields, &
      routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_ConnectorSet(connector, rh=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
 
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ExecuteRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    character(len=ESMF_MAXSTR)    :: cName
    type(ESMF_RouteHandle)        :: rh
    type(ESMF_FieldBundle)        :: dstFields, srcFields
    character(len=ESMF_MAXSTR)    :: logMsg

    rc = ESMF_SUCCESS

#ifdef MEMCOPY
    ! execute memcopy
!    call ESMF_CplCompGet(connector, name=cName, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    write (logMsg,"(A,A)") trim(cName)//": ", &
!      " calling FieldBundleMemCopy"
!    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    call PROTO_FieldBundleMemCopy(connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    ! execute rh
!    call ESMF_CplCompGet(connector, name=cName, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    write (logMsg,"(A,A)") trim(cName)//": ", &
!      " calling FieldBundleRedist"
!    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, rh=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldBundleRedist(srcFields, dstFields, &
      routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
 
  end subroutine

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    type(type_InternalState)      :: is
    type(ESMF_RouteHandle)        :: rh

    rc = ESMF_SUCCESS

#ifdef MEMCOPY
    ! Get internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(is%wrap%dstFieldList)
    deallocate(is%wrap%srcFieldList)
    deallocate(is%wrap)
#else
    call NUOPC_ConnectorGet(connector, rh=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
   ! release rh
    call ESMF_FieldBundleRegridRelease(rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
      
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldListStore(connector, rc)
    type(ESMF_CplComp), intent(in) :: connector
    integer, intent(out)           :: rc

    ! local variables
    type(type_InternalState) :: is
    type(ESMF_FieldBundle)   :: srcFields, dstFields
    integer                  :: srcFieldCount, dstFieldCount
    integer                  :: fIndex

    rc = ESMF_SUCCESS

    ! Get internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get src field list count
    call ESMF_FieldBundleGet(srcFields, fieldCount=srcFieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! get dst field list count
    call ESMF_FieldBundleGet(dstFields, fieldCount=dstFieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (srcFieldCount .ne. dstFieldCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="MEMCOPY INCOMPATIBILITY DETECTED: field counts must match.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    allocate(is%wrap%srcFieldList(srcFieldCount))
    call ESMF_FieldBundleGet(srcFields, fieldList=is%wrap%srcFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(is%wrap%dstFieldList(dstFieldCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=is%wrap%dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do fIndex=1, srcFieldCount
!      call ESMF_CplCompGet(connector, name=cName, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, &
!        file=__FILE__)) &
!        return  ! bail out
!      call ESMF_FieldGet(is%wrap%srcFieldList(fIndex), name=fName, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, &
!        file=__FILE__)) &
!        return  ! bail out
!      write (logMsg,"(A,A,A)") trim(cName)//": ", &
!        " checking grid decomp from field ", &
!        trim(fName)
!      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      call PROTO_FieldCheck(is%wrap%srcFieldList(fIndex), &
        is%wrap%dstFieldList(fIndex), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldCheck(srcField, dstField, rc)
    type(ESMF_Field), intent(in)    :: srcField
    type(ESMF_Field), intent(inout) :: dstField
    integer, intent(out)            :: rc
 
    ! local variables
    type(ESMF_Grid)               :: grid1, grid2
    type(ESMF_GridMatch_Flag)     :: gridmatch
    type(ESMF_DistGrid)           :: distgrid1, distgrid2
    type(ESMF_DistGridMatch_Flag) :: distgridmatch
    integer                       :: localDeCount1, localDeCount2

    rc = ESMF_SUCCESS

    call ESMF_FieldGet(srcField, grid=grid1, &
      localDeCount=localDeCount1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(dstField, grid=grid2, & 
      localDeCount=localDeCount2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    gridmatch = ESMF_GridMatch(grid1, grid2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (gridmatch .eq. ESMF_GRIDMATCH_EXACT) then
    elseif (gridmatch .eq. ESMF_GRIDMATCH_ALIAS) then
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="MEMCOPY INCOMPATIBILITY DETECTED: Grids must match.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    if (localDeCount1 .ne. localDeCount2) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="MEMCOPY INCOMPATIBILITY DETECTED: localDeCounts must match.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_GridGet(grid1, distgrid=distgrid1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridGet(grid2, distgrid=distgrid2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    distgridmatch = ESMF_DistGridMatch(distgrid1, distgrid2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (distgridmatch .eq. ESMF_DISTGRIDMATCH_EXACT) then
    elseif (distgridmatch .eq. ESMF_DISTGRIDMATCH_ALIAS) then
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="MEMCOPY INCOMPATIBILITY DETECTED: DistGrids must match.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldBundleMemCopy(connector, rc)
    type(ESMF_CplComp),     intent(in) :: connector
    integer, intent(out)               :: rc

    ! local variables
    type(type_InternalState)      :: is
    integer                       :: srcFieldCount, dstFieldCount
    integer                       :: fIndex
    character(len=ESMF_MAXSTR)    :: fName
    character(len=ESMF_MAXSTR)    :: logMsg

    rc = ESMF_SUCCESS

    ! Get internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Get src field list count
    srcFieldCount = size(is%wrap%srcFieldList)
    dstFieldCount = size(is%wrap%srcFieldList)

    if (srcFieldCount .ne. dstFieldCount) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="MEMCOPY INCOMPATIBILITY DETECTED: field counts must match.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    do fIndex=1, srcFieldCount
!    call ESMF_CplCompGet(connector, name=cName, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, &
!        file=__FILE__)) &
!        return  ! bail out
!      call ESMF_FieldGet(srcFieldList(fIndex), name=fName, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, &
!        file=__FILE__)) &
!        return  ! bail out
!      write (logMsg,"(A,A,A)") trim(cName)//": ", &
!        " copying data from field ", &
!        trim(fName)
!      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      call PROTO_FieldMemCopy(is%wrap%srcFieldList(fIndex), &
        is%wrap%dstFieldList(fIndex), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

  end subroutine


  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldMemCopy(srcField, dstField, rc)
    type(ESMF_Field), intent(in)    :: srcField
    type(ESMF_Field), intent(inout) :: dstField
    integer, intent(out)            :: rc
 
    ! local variables
    integer                       :: localDeCount1, localDeCount2
    integer                       :: deIndex
    real(ESMF_KIND_R8), pointer   :: srcFarray(:,:), dstFarray(:,:)

    rc = ESMF_SUCCESS

    call ESMF_FieldGet(srcField, localDeCount=localDeCount1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(dstField, localDeCount=localDeCount2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (localDeCount1 .ne. localDeCount2) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="MEMCOPY INCOMPATIBILITY DETECTED: localDeCounts must match.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    nullify(dstFarray)
    nullify(srcFarray)

    do deIndex=0, localDeCount1-1

      call ESMF_FieldGet(srcField, localDe=deIndex, farrayPtr=srcFarray, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldGet(dstField, localDe=deIndex, farrayPtr=dstFarray, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      dstFarray(:,:) = srcFarray(:,:)

      nullify(dstFarray)
      nullify(srcFarray)

    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  logical function IsMemCopy(connector, rc)
    type(ESMF_CplComp)   :: connector
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR) :: attrString

    ! get MemCopy attribute
    call ESMF_AttributeGet(connector, name='MemCopy', value=attrString, &
      defaultValue="true", convention='NUOPC', purpose='Instance', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    select case (attrString)
    case ('true','TRUE','True','t','T','1' )
      IsMemCopy = .true.
    case default
      IsMemCopy = .false.
    endselect

  end function

  !-----------------------------------------------------------------------------

end module

