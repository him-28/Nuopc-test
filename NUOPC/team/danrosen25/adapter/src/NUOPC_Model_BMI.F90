module NUOPC_Model_BMI

    !-----------------------------------------------------------------------------
    ! MODEL Component.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Model, &
        model_routine_SS => SetServices ! Override NUOPC_Model SetServices
    use NuopcBmiAdapter ! NUOPC BMI Adapter is a component class of NUOPC Model BMI
  
    implicit none
  
    private
  
    public &
        SetModel, &
        SetServices, &
        routine_Run

    public &
        label_AdvanceClock, &
        label_CheckImport, &
        label_DataInitialize, &
        label_SetClock, &
        label_SetRunClock

    character(:),allocatable :: modelConfigFile

!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------

    subroutine SetModel(configFile, initialize,finalize,update, getStartTime, getEndTime, getCurrentTime, getTimeStep, getTimeUnits, getVarType, getVarUnits, getVarRank, getGridType, getGridShape, getGridSpacing, getGridOrigin, getDouble, getDoubleAt, setDouble, setDoubleAt, getInputVarNames, getOutputVarNames, getComponentName, rc)
        character(*),intent(in) :: configFile
        procedure(bmiInitialize) :: initialize
        procedure(bmiUpdate) :: update
        procedure(bmiFinalize) :: finalize
        procedure(bmiGetStartTime) :: getStartTime
        procedure(bmiGetEndTime) :: getEndTime
        procedure(bmiGetCurrentTime) :: getCurrentTime
        procedure(bmiGetTimeStep) :: getTimeStep
        procedure(bmiGetTimeUnits) :: getTimeUnits
        procedure(bmiGetVarType) :: getVarType
        procedure(bmiGetVarUnits) :: getVarUnits
        procedure(bmiGetVarRank) :: getVarRank
        procedure(bmiGetGridType) :: getGridType
        procedure(bmiGetGridShape) :: getGridShape
        procedure(bmiGetGridSpacing) :: getGridSpacing
        procedure(bmiGetGridOrigin) :: getGridOrigin
        procedure(bmiGetDouble) :: getDouble
        procedure(bmiGetDoubleAt) :: getDoubleAt
        procedure(bmiSetDouble) :: setDouble
        procedure(bmiSetDoubleAt) :: setDoubleAt
        procedure(bmiGetInputVarNames) :: getInputVarNames
        procedure(bmiGetOutputVarNames) :: getOutputVarNames
        procedure(bmiGetComponentName) :: getComponentName
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        modelConfigFile = configFile

        call BMIAdapter_SetProcedures( &
            initialize = initialize, &
            finalize = finalize, &
            update = update, &
            getStartTime = getStartTime, &
            getEndTime = getEndTime, &
            getCurrentTime = getCurrentTime, &
            getTimeStep = getTimeStep, &
            getTimeUnits = getTimeUnits, &
            getVarType = getVarType, &
            getVarUnits = getVarUnits, &
            getVarRank = getVarRank, &
            getGridType = getGridType, &
            getGridShape = getGridShape, &
            getGridSpacing = getGridSpacing, &
            getGridOrigin = getGridOrigin, &
            getDouble = getDouble, &
            getDoubleAt = getDoubleAt, &
            setDouble = setDouble, &
            setDoubleAt = setDoubleAt, &
            getInputVarNames = getInputVarNames, &
            getOutputVarNames = getOutputVarNames, &
            getComponentName = getComponentName )

    end subroutine SetModel
    
    !-----------------------------------------------------------------------------
    subroutine SetServices(gcomp,rc)
        type(ESMF_GridComp)   :: gcomp
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        call BMIAdapter_isModelSet(rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail ou

        call model_routine_SS(gcomp,rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail ou

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
        call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, &
            specRoutine=ModelAdvance, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSpecialize(gcomp,specLabel=label_Finalize, &
            specRoutine=ModelFinalize, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine


    subroutine InitializeP1(model, importState, exportState, clock, rc)
        type(ESMF_GridComp)     :: model
        type(ESMF_State)        :: importState, exportState
        type(ESMF_Clock)        :: clock, compclock
        type(ESMF_TimeInterval) :: stabilityTimeStep
        integer, intent(out)    :: rc

        rc = ESMF_SUCCESS

        call BMIAdapter_Initialize(trim(adjustl(modelConfigFile)),rc) ! Initialize BMI Model
        if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Initialize BMI Failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call BMIAdapter_PrintComponentInfo() ! Print BMI information after initializing
        call BMIAdapter_PrintAllVarInfo()

        compclock = BMIAdapter_ClockCreate(clock, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSetClock(model, externalClock=compclock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call BMIAdapter_AddAllFieldsToDictionary(rc=rc)
        if(ESMF_LogFoundError(rcToCheck=rc,msg="BMIAdapter Add Fields to Dictionary Failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

#define WITHIMPORTFIELDS_OFF
#ifdef WITHIMPORTFIELDS
    call BMIAdapter_StateAdvertiseInputFields(importState,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Advertise Input Fields Failed", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

            call BMIAdapter_StateAdvertiseOutputFields(exportState,rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Advertise Output Fields Failed", &
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
        gridIn = BMIAdapter_SingleGridCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Single Grid Create error.", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        gridOut = gridIn ! for now out same as in

#ifdef WITHIMPORTFIELDS
  call BMIAdapter_StateRealizeInputFields(importState,gridIn,rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Realize Input Fields Error", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

            call BMIAdapter_StateRealizeOutputFields(exportState,gridOut,rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Realize Output Fields Error", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    !  if(localPet .eq. 0) then
    !  call BMIAdapter_PrintFieldArray(exportState,"surface_elevation",rc)
    !  if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Realize Output Fields Error", &
    !      line=__LINE__, &
    !      file=__FILE__)) &
    !      return  ! bail out
    !  end if
    end subroutine

    !-----------------------------------------------------------------------------

    subroutine ModelAdvance(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_State)              :: importState, exportState

        rc = ESMF_SUCCESS

        call BMIAdapter_Update(rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="BMI Update Error", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! query the Component for its clock, importState and exportState
        call ESMF_GridCompGet(model, clock=clock, importState=importState, &
            exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_ClockPrintStopTime(clock, &
            "------>Advancing MODEL to stop time: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call BMIAdapter_PrintCurrentTime(rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

    !-----------------------------------------------------------------------------

    subroutine ModelFinalize(model,rc)
        type(ESMF_Gridcomp) :: model
        integer,intent(out) :: rc
        type(ESMF_State)              :: exportState

        rc = ESMF_SUCCESS

        ! query the Component for its exportState
        call ESMF_GridCompGet(model, exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        !    if(localPet .eq. 0) then
        !    call BMIAdapter_PrintFieldArray(exportState,"surface_elevation",rc)
        !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        !      line=__LINE__, &
        !      file=__FILE__)) &
        !      return  ! bail out
        !    end if

        call BMIAdapter_Finalize(rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter finalize failed", &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine


end module NUOPC_Model_BMI

