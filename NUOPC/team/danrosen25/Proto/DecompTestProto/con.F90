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
    character(len=ESMF_MAXSTR)    :: cName
    logical                       :: memcopy
    type(ESMF_FieldBundle)        :: srcFields, dstFields
    type(ESMF_RouteHandle)        :: rh
    character(len=ESMF_MAXSTR)    :: logMsg

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(connector, name=cName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call IsMemCopy(connector, memcopy=memcopy, rc=rc)
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

    if (memcopy) then
      ! check the fields for grid and distgrid consistency
      write (logMsg,"(A,A)") trim(cName)//": ", &
        " connector attribute MemCopy: true"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      call PROTO_FieldBundleCheck(srcFields=srcFields, &
        dstFields=dstFields, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      ! specialize with Redist, instead of the default Regrid
      write (logMsg,"(A,A)") trim(cName)//": ", &
        " connector attribute MemCopy: false"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
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
    endif
 
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ExecuteRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    logical                       :: memcopy
    type(ESMF_FieldBundle)        :: dstFields, srcFields
    type(ESMF_RouteHandle)        :: rh

    rc = ESMF_SUCCESS

    call IsMemCopy(connector, memcopy=memcopy, rc=rc)
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

    if (memcopy) then
      ! execute memcopy
      call PROTO_FieldBundleMemCopy(srcFields=srcFields, dstFields=dstFields, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      ! execute rh
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
    endif
 
  end subroutine

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    logical                       :: memcopy
    type(ESMF_RouteHandle)        :: rh

    rc = ESMF_SUCCESS

    call IsMemCopy(connector, memcopy=memcopy, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (memcopy) then
    else
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
    endif
      
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldBundleCheck(srcFields, dstFields, rc)
    type(ESMF_FieldBundle), intent(in) :: srcFields
    type(ESMF_FieldBundle), intent(in) :: dstFields
    integer, intent(out)               :: rc

    ! local variables
    integer                       :: srcFieldCount, dstFieldCount
    type(ESMF_Field), allocatable :: srcFieldList(:), dstFieldList(:)
    integer                       :: fIndex

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
        msg="MEMCOPY INCOMPATIBILITY DETECTED: "// &
            "field counts must match", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    allocate(srcFieldList(srcFieldCount))
    call ESMF_FieldBundleGet(srcFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=srcFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(dstFieldList(dstFieldCount))
    call ESMF_FieldBundleGet(dstFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do fIndex=1, srcFieldCount
      call PROTO_FieldCheck(srcFieldList(fIndex), &
        dstFieldList(fIndex), rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    deallocate(dstFieldList)
    deallocate(srcFieldList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldCheck(srcField, dstField, rc)
    type(ESMF_Field), intent(in) :: srcField
    type(ESMF_Field), intent(in) :: dstField
    integer, intent(out)         :: rc
 
    ! local variables
    character(len=ESMF_MAXSTR)    :: fName
    type(ESMF_Grid)               :: grid1, grid2
    type(ESMF_GridMatch_Flag)     :: gridmatch
    type(ESMF_DistGrid)           :: distgrid1, distgrid2
    type(ESMF_DistGridMatch_Flag) :: distgridmatch
    integer                       :: localDeCount1, localDeCount2

    rc = ESMF_SUCCESS

    call ESMF_FieldGet(srcField, grid=grid1, &
      localDeCount=localDeCount1, name=fName, rc=rc)
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
        msg="MEMCOPY INCOMPATIBILITY DETECTED: "// &
            "Grids must match "//trim(fName), &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    if (localDeCount1 .ne. localDeCount2) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="MEMCOPY INCOMPATIBILITY DETECTED: "// &
            "localDeCounts must match "//trim(fName), &
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
        msg="MEMCOPY INCOMPATIBILITY DETECTED: "// &
            "DistGrids must match "//trim(fName), &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine PROTO_FieldBundleMemCopy(srcFields, dstFields, check, rc)
    type(ESMF_FieldBundle), intent(in)  :: srcFields
    type(ESMF_FieldBundle), intent(in)  :: dstFields
    logical, intent(in), optional       :: check
    integer, intent(out)                :: rc

    ! local variables
    logical                       :: lcheck
    integer                       :: srcFieldCount, dstFieldCount
    type(ESMF_Field), allocatable :: srcFieldList(:), dstFieldList(:)
    integer                       :: fIndex

    rc = ESMF_SUCCESS

    if (present(check)) then
      lcheck = check
    else
      lcheck = .false.
    endif

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
        msg="MEMCOPY INCOMPATIBILITY DETECTED: "// &
            "field counts must match", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif

    allocate(srcFieldList(srcFieldCount))
    call ESMF_FieldBundleGet(srcFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=srcFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(dstFieldList(dstFieldCount))
    call ESMF_FieldBundleGet(dstFields, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, fieldList=dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do fIndex=1, srcFieldCount
      if (lcheck) then
        call PROTO_FieldCheck(srcFieldList(fIndex), &
          dstFieldList(fIndex), rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      call PROTO_FieldMemCopy(srcFieldList(fIndex), &
        dstFieldList(fIndex), rc)
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
    integer                       :: localDeCount1, localDeCount2
    character(len=ESMF_MAXSTR)    :: fName
    integer                       :: deIndex
    real(ESMF_KIND_R8), pointer   :: srcFarray(:,:), dstFarray(:,:)

    rc = ESMF_SUCCESS

    call ESMF_FieldGet(srcField, localDeCount=localDeCount1, &
      name=fName, rc=rc)
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
        msg="MEMCOPY INCOMPATIBILITY DETECTED: "// &
            "localDeCounts must match "//trim(fName), &
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

  subroutine IsMemCopy(connector, memcopy, rc)
    type(ESMF_CplComp), intent(in) :: connector
    logical, intent(out)           :: memcopy
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
      memcopy = .true.
    case default
      memcopy = .false.
    endselect

  end subroutine

  !-----------------------------------------------------------------------------

end module

