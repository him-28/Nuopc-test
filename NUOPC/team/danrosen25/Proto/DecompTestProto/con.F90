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
    NUOPC_ConnectorGet, NUOPC_ConnectorSet
  
  implicit none
  
  private
  
  public SetServices
  
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
      
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ComputeRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    integer                       :: localrc
    type(ESMF_FieldBundle)        :: srcFields, dstFields
    type(ESMF_RouteHandle)        :: rh

    rc = ESMF_SUCCESS
    
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

    call ESMF_CplCompGet(connector, name=cName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
      dstFields=dstFields, rh=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifdef MEMCOPY
    ! execute memcopy
    write (logMsg,"(A,A)") trim(cName)//": ", &
      " calling FieldBundleMemCopy"
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    call PROTO_FieldBundleMemCopy(cName, srcFields, dstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    ! execute rh
    write (logMsg,"(A,A)") trim(cName)//": ", &
      " calling FieldBundleRedist"
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    call ESMF_FieldBundleRedist(srcFields, dstFields, &
      routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
 
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldBundleMemCopy(label, srcFields, dstFields, rc)
    character(*), intent(in)              :: label
    type(ESMF_FieldBundle), intent(in)    :: srcFields
    type(ESMF_FieldBundle), intent(inout) :: dstFields
    integer, intent(out)                  :: rc

    ! local variables
    integer                       :: srcFieldCount, dstFieldCount
    type(ESMF_Field), allocatable :: srcFieldList(:), dstFieldList(:)
    integer                       :: fIndex
    character(len=ESMF_MAXSTR)    :: fName
    character(len=ESMF_MAXSTR)    :: logMsg

    rc = ESMF_SUCCESS

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

    allocate(srcFieldList(srcFieldCount))
    call ESMF_FieldBundleGet(srcFields, fieldList=srcFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(dstFieldList(dstFieldCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do fIndex=1, srcFieldCount
      call ESMF_FieldGet(srcFieldList(fIndex), name=fName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      write (logMsg,"(A,A,A)") trim(label)//": ", &
        " copying data from field ", &
        trim(fName)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      call PROTO_FieldMemCopy(srcFieldList(fIndex), dstFieldList(fIndex), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    deallocate(dstFieldList)
    deallocate(srcFieldList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldMemCopy(srcField, dstField, rc)
    type(ESMF_Field), intent(in)    :: srcField
    type(ESMF_Field), intent(inout) :: dstField
    integer, intent(out)            :: rc
 
    ! local variables
    type(ESMF_Grid)               :: grid1, grid2
    type(ESMF_GridMatch_Flag)     :: gridmatch
    type(ESMF_DistGrid)           :: distgrid1, distgrid2
    type(ESMF_DistGridMatch_Flag) :: distgridmatch
    integer                       :: localDeCount1, localDeCount2
    integer                       :: deIndex
    real(ESMF_KIND_R8), pointer   :: srcFarray(:,:), dstFarray(:,:)

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

end module

