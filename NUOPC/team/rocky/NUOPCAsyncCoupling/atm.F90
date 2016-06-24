module ATM

    !-----------------------------------------------------------------------------
    ! ATM Component.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Model, &
        model_routine_SS    => SetServices, &
        model_label_Advance => label_Advance, &
        model_routine_Run   => routine_Run, &
        model_label_SetRunClock => label_SetRunClock
  
    implicit none
  
    ! these need to be available to deeply nested physics
    type(ESMF_RouteHandle) :: lnd2atm_rh
    type(ESMF_Field)       :: fromLand

    private
  
    public SetServices
  
!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------
  
    subroutine SetServices(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc
    
        rc = ESMF_SUCCESS
    
        ! the NUOPC model component will register the generic methods
        call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! set entry point for methods that require specific implementation
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! attach specializing method(s)
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_RUN, &
            phaseLabelList=(/"phys"/), userRoutine=model_routine_Run, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
            specRoutine=ModelAdvancePhys, specPhaseLabel="phys", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_RUN, &
            phaseLabelList=(/"dyn"/), userRoutine=model_routine_Run, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
            specRoutine=ModelAdvanceDyn, specPhaseLabel="dyn", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_MethodRemove(model, model_label_SetRunClock, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

        call NUOPC_CompSpecialize(model, specLabel=model_label_SetRunClock, &
            specRoutine=SetRunClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
           return  ! bail out
    
    end subroutine
  
    !-----------------------------------------------------------------------------

    subroutine InitializeP1(model, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc
    
        rc = ESMF_SUCCESS
    
        ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
        ! will result in a model component that does not advertise any importable
        ! Fields. Use this if you want to drive the model independently.
#define WITHIMPORTFIELDS
#ifdef WITHIMPORTFIELDS
        ! importable field: sea_surface_temperature
        call NUOPC_Advertise(importState, &
            StandardName="sea_surface_temperature", name="sst", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

         ! importable field: surface_downward_heat_flux_in_air
        call NUOPC_Advertise(importState, &
            StandardName="surface_downward_heat_flux_in_air", name="sdhfia", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
#endif
    
        ! exportable field: air_pressure_at_sea_level
        call NUOPC_Advertise(exportState, &
            StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! exportable field: surface_net_downward_shortwave_flux
        call NUOPC_Advertise(exportState, &
            StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine
  
    !-----------------------------------------------------------------------------

    subroutine InitializeP2(model, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc
    
        ! local variables    
        type(ESMF_Field)        :: field
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
        gridOut = gridIn ! for now out same as in

#ifdef WITHIMPORTFIELDS
        ! importable field: sea_surface_temperature
        field = ESMF_FieldCreate(name="sst", grid=gridIn, &
            typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_Realize(importState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! importable field: surface_downward_heat_flux_in_air
        field = ESMF_FieldCreate(name="sdhfia", grid=gridIn, &
            typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_Realize(importState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

#endif

        ! exportable field: air_pressure_at_sea_level
        field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
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

        ! exportable field: surface_net_downward_shortwave_flux
        field = ESMF_FieldCreate(name="rsns", grid=gridOut, &
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

    subroutine ModelAdvanceDyn(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc
    
        ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_State)              :: importState, exportState

        rc = ESMF_SUCCESS
    
        ! query the Component for its clock, importState and exportState
        call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
            exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
        ! Because of the way that the internal Clock was set by default,
        ! its timeStep is equal to the parent timeStep. As a consequence the
        ! currTime + timeStep is equal to the stopTime of the internal Clock
        ! for this call of the ModelAdvance() routine.
    
        call dynamics()

        call ESMF_ClockPrint(clock, options="currTime", &
            preString="------>Advancing ATM from: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        call ESMF_ClockPrint(clock, options="stopTime", &
            preString="--------------------------------> to: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

     subroutine ModelAdvancePhys(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc

         ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_State)              :: importState, exportState
        type(ESMF_Time)               :: currTime
        type(ESMF_TimeInterval)       :: timeStep

        rc = ESMF_SUCCESS

        ! query the Component for its clock, importState and exportState
        call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, &
            exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! advance the model: currTime -> currTime + timeStep

        call ESMF_StateGet(importState, "lnd2atm_rh", &
            routehandle=lnd2atm_rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_StateGet(importState, "sdhfia", &
            field=fromLand, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! Here we assume the model has a deeply nested physics
        ! calling structure with a number of physics packages called.
        ! This is exemplified by the skeleton subroutines below.

        call physics()

        call ESMF_ClockPrint(clock, options="currTime", &
            preString="------>Advancing ATM from: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_ClockPrint(clock, options="stopTime", &
            preString="--------------------------------> to: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

    subroutine dynamics()
        print *, "Inside dynamics"
    end subroutine

    subroutine physics()
        print *, "Inside physics"
        call physicsLayer1()
    end subroutine

    subroutine physicsLayer1()
        print *, "Inside physics layer 1"
        call physicsLayer2()
    end subroutine

    subroutine physicsLayer2()
        print *, "Inside physics layer 2"
        call physicsPackage1()
        call physicsPackage2()
        call land()
        call physicsPackage3()
    end subroutine

    subroutine physicsPackage1()
        print *, "Inside physics package 1"
    end subroutine

    subroutine physicsPackage2()
        print *, "Inside physics package 2"
    end subroutine

    subroutine physicsPackage3()
        print *, "Inside physics package 3"
    end subroutine

    subroutine land()
        integer :: rc
        type(ESMF_RouteHandle) :: rh
        integer :: clb(2), cub(2), i, j
        real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
        real :: total

        print *, "Inside ATM land"

        ! Here we are deep inside the physics at the place
        ! where we need data from the external land.  Use
        ! the passed-down routehandle to receive the regridding
        ! field in place.  This will block until the corresponding
        ! call is made in the land model.

        call ESMF_FieldRegrid(dstField=fromLand, routehandle=lnd2atm_rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_FieldGet(fromLand, localDE=0, farrayPtr=farrayPtr, &
            computationalLBound=clb, computationalUBound=cub, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

         total = 0
         do i=clb(1), cub(1)
            do j=clb(2), cub(2)
                total = total + farrayPtr(i,j)
            enddo
         enddo

         print *, "Sum received on ATM side = ", total

    end subroutine


    subroutine SetRunClock(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Clock)           :: modelClock, driverClock
        type(ESMF_Time)            :: currTime
        type(ESMF_TimeInterval)    :: timeStep

        rc = ESMF_SUCCESS

       ! query the model for clocks
        call NUOPC_ModelGet(gcomp, modelClock=modelClock, &
          driverClock=driverClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        ! set the modelClock to have the current start time as the driverClock
        call ESMF_ClockGet(driverClock, currTime=currTime, timeStep=timeStep, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_ClockSet(modelClock, currTime=currTime, timeStep=timeStep, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      
        ! check and set the component clock against the driver clock
        call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

    end subroutine SetRunClock



end module


