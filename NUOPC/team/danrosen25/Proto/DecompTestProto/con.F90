module CON

  !-----------------------------------------------------------------------------
  ! Connector Component.
  !-----------------------------------------------------------------------------
  
  use ESMF
  use NUOPC
  use NUOPC_Connector, only: &
    con_routine_SS      => SetServices, &
    con_label_ComputeRH => label_ComputeRouteHandle, &
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
  
end module

