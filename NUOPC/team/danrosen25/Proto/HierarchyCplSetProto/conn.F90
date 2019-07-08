!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module CON

  !-----------------------------------------------------------------------------
  ! Connector Component.
  !-----------------------------------------------------------------------------
  
  ! Enabling the followng macro, i.e. setting it to WITHSTATEUSE_on,
  ! will activate sections of code that demonstrate how
  ! the "state" member inside the NUOPC_Connector is used. The
  ! example creates an FieldBundle that's a duplicate of dstFields inside the
  ! connector, and precomputes two RouteHandles. The first is a Regrid, while 
  ! the second is simply an identity operation using FieldRedist() to show the
  ! principle.
#define FIELDWRITE_on
#define ARRAYGET_on

  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => SetServices, &
    con_label_ExecuteRH => label_ExecuteRouteHandle, &
    con_label_ReleaseRH => label_ReleaseRouteHandle, &
    con_label_ComputeRH => label_ComputeRouteHandle, &
    NUOPC_ConnectorGet, NUOPC_ConnectorSet
  
  implicit none
  
  private
  
  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    character(ESMF_MAXSTR), allocatable :: rhCplSet(:)
    type(ESMF_RouteHandle), allocatable :: rhList(:)
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
    call NUOPC_CompSpecialize(connector, specLabel=con_label_ExecuteRH, &
      specRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
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
    integer                         :: lPet
    character(ESMF_MAXSTR)          :: cname
    type(type_InternalState)        :: is
    character(ESMF_MAXSTR), pointer :: cplSetList(:)
    integer                         :: i
    type(ESMF_FieldBundle)          :: dstFields, srcFields
    integer                         :: fieldCount
    integer                         :: dstFieldRank
    integer                         :: deCount
    type(ESMF_Field),allocatable    :: dstFieldList(:)
    real(ESMF_KIND_R8),pointer      :: dstFieldPtr(:,:) 
    integer                         :: stat
 
    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(connector, name=cname, localPet=lPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out
    call ESMF_UserCompSetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    nullify(cplSetList)
    call NUOPC_ConnectorGet(connector, cplSetList=cplSetList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (associated(cplSetList)) then
      allocate(is%wrap%rhCplSet(size(cplSetList)))
      allocate(is%wrap%rhList(size(cplSetList)))
      do i=1, size(cplSetList)
        is%wrap%rhCplSet(i) = cplSetList(i)
      enddo
    else
      allocate(is%wrap%rhCplSet(0))
      allocate(is%wrap%rhList(0))
    endif

    do i=1, size(is%wrap%rhCplSet)
      call ESMF_LogWrite(trim(cname)//": ComputeRH "//trim(is%wrap%rhCplSet(i)), &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
        dstFields=dstFields, CplSet=is%wrap%rhCplSet(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldBundleRedistStore(srcFields, dstFields, &
        routehandle=is%wrap%rhList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (fieldCount .gt. 0) then
        allocate(dstFieldList(fieldCount))
        call ESMF_FieldBundleGet(dstFields, fieldList=dstFieldList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
#ifdef ARRAYGET_on
        call ESMF_FieldGet(dstFieldList(1), rank=dstFieldRank, &
          localDeCount=deCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (deCount .ne. 0) then
          call ESMF_FieldGet(dstFieldList(1), farrayPtr=dstFieldPtr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
#endif
#ifdef FIELDWRITE_on
        call ESMF_FieldWrite(dstFieldList(1), &
          'CplSet_'//trim(is%wrap%rhCplSet(i))//'.nc', &
          variableName='field1', overwrite=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
#endif
        deallocate(dstFieldList)
      endif
    enddo
          
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ExecuteRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    integer                         :: lPet
    character(ESMF_MAXSTR)          :: cname
    type(type_InternalState)        :: is
    integer                         :: i
    type(ESMF_FieldBundle)          :: dstFields, srcFields
    integer                         :: stat

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(connector, localPet=lPet, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do i=1, size(is%wrap%rhCplSet)
      call ESMF_LogWrite(trim(cname)//": ExecuteRH "//trim(is%wrap%rhCplSet(i)), &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_ConnectorGet(connector, srcFields=srcFields, &
        dstFields=dstFields, CplSet=is%wrap%rhCplSet(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldBundleRedist(srcFields, dstFields, &
        routehandle=is%wrap%rhList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(connector, rc)
    type(ESMF_CplComp)  :: connector
    integer, intent(out) :: rc
    
    ! local variables
    integer                         :: lPet
    character(ESMF_MAXSTR)          :: cname
    type(type_InternalState)        :: is
    integer                         :: i

    rc = ESMF_SUCCESS

    call ESMF_CplCompGet(connector, localPet=lPet, name=cname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(connector, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do i=1, size(is%wrap%rhCplSet)
      call ESMF_LogWrite(trim(cname)//": ReleaseRH "//trim(is%wrap%rhCplSet(i)), &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
   
      call ESMF_FieldBundleRedistRelease(routehandle=is%wrap%rhList(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo
    deallocate(is%wrap%rhCplSet)
    deallocate(is%wrap%rhList)

  end subroutine
  
end module

