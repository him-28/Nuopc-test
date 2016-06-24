module LND

    !-----------------------------------------------------------------------------
    ! LND Component.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Model, &
        model_routine_SS      => SetServices, &
        model_label_SetClock  => label_SetClock, &
        model_label_Advance   => label_Advance, &
        model_label_DataInitialize => label_DataInitialize

    implicit none
  
    private
  
    public SetServices
  
!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------
  
    subroutine SetServices(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc
    
        rc = ESMF_SUCCESS
    
        ! the NUOPC model component will register the generic methods
        call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! set entry point for methods that require specific implementation
        call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! attach specializing method(s)
        call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetClock, &
            specRoutine=SetClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
            specRoutine=ModelAdvance, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine
  
    !-----------------------------------------------------------------------------

    subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: gcomp
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc
    
        rc = ESMF_SUCCESS

        call NUOPC_Advertise(exportState, &
            StandardName="surface_downward_heat_flux_in_air", &
            name="sdhfia", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine
  
    !-----------------------------------------------------------------------------

    subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: gcomp
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc
        type(ESMF_Field) :: field
        type(ESMF_Grid)         :: gridIn
        type(ESMF_Grid)         :: gridOut
    
        rc = ESMF_SUCCESS

        ! create a Grid object for Fields
        gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/10, 100/), &
            minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
            maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
            coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
            rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        gridOut = gridIn

        field = ESMF_FieldCreate(name="sdhfia", grid=gridOut, &
            typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
	
        call NUOPC_Realize(exportState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
    end subroutine
  
    !-----------------------------------------------------------------------------

    subroutine SetClock(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc
    
        ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_TimeInterval)       :: stabilityTimeStep

        rc = ESMF_SUCCESS
    
        ! query the Component for its clock, importState and exportState
        call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      
        ! initialize internal clock
        ! here: parent Clock and stability timeStep determine actual model timeStep
        !TODO: stabilityTimeStep should be read in from configuation
        !TODO: or computed from internal Grid information
        call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_CompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
    end subroutine

    !-----------------------------------------------------------------------------

    subroutine ModelAdvance(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc
    
        ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_State)              :: importState, exportState
        type(ESMF_Time)               :: currTime
        type(ESMF_TimeInterval)       :: timeStep
        type(ESMF_RouteHandle)        :: rh
        type(ESMF_Field)              :: field
        real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
        integer :: clb(2), cub(2), i, j

        rc = ESMF_SUCCESS
    
        ! query the Component for its clock, importState and exportState
        call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
            exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_StateGet(exportState, "lnd2atm_rh", &
            routehandle=rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_StateGet(exportState, "sdhfia", &
            field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out


        call ESMF_FieldGet(field, localDE=0, farrayPtr=farrayPtr, &
            computationalLBound=clb, computationalUBound=cub, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

         do i=clb(1), cub(1)
            do j=clb(2), cub(2)
                farrayPtr(i,j) = 1.0
            enddo
         enddo


        call ESMF_FieldRegrid(srcField=field, &
            routehandle=rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    
        call ESMF_ClockPrint(clock, options="currTime", &
            preString="------>Advancing LND from: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        call ESMF_TimePrint(currTime + timeStep, &
            preString="--------------------------------> to: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine


end module



