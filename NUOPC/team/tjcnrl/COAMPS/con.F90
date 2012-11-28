#define FILENAME "con.F90"

module CON

  !-----------------------------------------------------------------------------
  ! Connector Component.
  !-----------------------------------------------------------------------------

  ! Enabling the followng macro, i.e. removing the "_disable" suffix so it is 
  ! simply "WITHSTATEUSE", will activate sections of code that demonstrate how
  ! the "state" member inside the NUOPC_Connector internal State is used. The
  ! example creates an FieldBundle that's a duplicate of superIS%wrap%dstFields,
  ! and precomputes two RouteHandles. The first is a Regrid, while the second
  ! is simply an identity operation using FieldRedist() to show the principle.
#define WITHSTATEUSE_disable

  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => routine_SetServices, &
    con_type_IS         => type_InternalState, &
    con_label_IS        => label_InternalState, &
    con_label_ComputeRH => label_ComputeRouteHandle, &
    con_label_ExecuteRH => label_ExecuteRouteHandle, &
    con_label_ReleaseRH => label_ReleaseRouteHandle
  
  implicit none
  
  private
  
  public SetServices

  character (*), parameter :: defaultVerbosity = "low"
  character (*), parameter :: label_InternalState = "CON_InternalState"

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

#ifdef WITHSTATEUSE
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
#endif

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
    character(ESMF_MAXSTR), pointer :: cplList(:)
    integer                         :: cplListSize, i, j
#ifdef WITHSTATEUSE
    type(ESMF_FieldBundle)        :: dstFields, interDstFields
    type(ESMF_Field), allocatable :: fields(:)
    integer                       :: fieldCount, i
    type(ESMF_Grid)               :: Grid
    type(ESMF_TypeKind_Flag)      :: typekind    
    type(ESMF_Field)              :: field
    type(ESMF_RouteHandle)        :: rh1, rh2
#endif

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

    ! report the cplList attribute
    call NUOPC_CplCompAttributeGet(ccomp, cplListSize=cplListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    allocate(cplList(cplListSize), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of cplList() failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    call NUOPC_CplCompAttributeGet(ccomp, cplList=cplList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    write(msgString,'(a,i0,a)') trim(cname)//': List of coupled fields (',cplListSize,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    do i=1, cplListSize
      write(msgString,'(a,i2,a)') trim(cname)//':   ',i,', '//trim(cplList(i))
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    deallocate(cplList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of cplList() failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

#ifdef WITHSTATEUSE
    ! replicate dstFields FieldBundle in order to provide intermediate Fields
    dstFields = superIS%wrap%dstFields
    call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    allocate(fields(fieldCount))
    call ESMF_FieldBundleGet(dstFields, fieldList=fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    interDstFields = ESMF_FieldBundleCreate(name="interDstFields", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    do i=1, fieldCount
      call ESMF_FieldGet(fields(i), grid=grid, typekind=typekind, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      field = ESMF_FieldCreate(grid, typekind, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_FieldBundleAdd(interDstFields, (/field/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
    enddo
    ! add interDstFields to the state member
    call ESMF_StateAdd(superIS%wrap%state, (/interDstFields/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! compute the first RouteHandle for srcFields->interDstFields (Regrid)
    call ESMF_FieldBundleRegridStore(superIS%wrap%srcFields, interDstFields, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, routehandle=rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_RouteHandleSet(rh1, name="src2interDstRH", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! compute the second RouteHandle for interDstFields->dstFields (Redist)
    call ESMF_FieldBundleRedistStore(interDstFields, superIS%wrap%dstFields, &
      routehandle=rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    call ESMF_RouteHandleSet(rh2, name="interDst2dstRH", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! add rh1, rh2 to the state member
    call ESMF_StateAdd(superIS%wrap%state, (/rh1, rh2/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
#else      
    ! specialize with Regrid
    call ESMF_FieldBundleRegridStore(superIS%wrap%srcFields, superIS%wrap%dstFields, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, routehandle=superIS%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
#endif

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving ComputeRH', ESMF_LOGMSG_INFO)
 
  end subroutine
  
  !-----------------------------------------------------------------------------

#ifdef WITHSTATEUSE
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
    type(ESMF_FieldBundle)        :: interDstFields
    type(ESMF_RouteHandle)        :: rh1, rh2

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

    ! retrieve interDstFields FieldBundle from state member
    call ESMF_StateGet(superIS%wrap%state, "interDstFields", interDstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! retrieve rh1 from state member
    call ESMF_StateGet(superIS%wrap%state, "src2interDstRH", rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! retrieve rh2 from state member
    call ESMF_StateGet(superIS%wrap%state, "interDst2dstRH", rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! apply rh1
    call ESMF_FieldBundleRegrid(superIS%wrap%srcFields, interDstFields, &
      routehandle=rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! apply rh2
    call ESMF_FieldBundleRedist(interDstFields, superIS%wrap%dstFields, &
      routehandle=rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving ExecuteRH', ESMF_LOGMSG_INFO)
 
  end subroutine
#endif

  !-----------------------------------------------------------------------------

#ifdef WITHSTATEUSE
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
    type(ESMF_FieldBundle)        :: interDstFields
    type(ESMF_RouteHandle)        :: rh1, rh2

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

    ! retrieve interDstFields FieldBundle from state member
    call ESMF_StateGet(superIS%wrap%state, "interDstFields", interDstFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! retrieve rh1 from state member
    call ESMF_StateGet(superIS%wrap%state, "src2interDstRH", rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! retrieve rh2 from state member
    call ESMF_StateGet(superIS%wrap%state, "interDst2dstRH", rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! release rh1
    call ESMF_FieldBundleRegridRelease(rh1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! release rh2
    call ESMF_FieldBundleRegridRelease(rh2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! Could destroy intermediate Fields and interDstFields FieldBundle here,
    ! but it is more convenient to let ESMF automatic garbage collection take
    ! care of them.

    if (verbose) &
    call ESMF_LogWrite('<<<'//trim(cname)//' leaving ReleaseRH', ESMF_LOGMSG_INFO)

  end subroutine
#endif

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
