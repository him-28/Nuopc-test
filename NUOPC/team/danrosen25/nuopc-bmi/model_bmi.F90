module MODEL_BMI

  !-----------------------------------------------------------------------------
  ! MODEL Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance, &
    model_label_Finalize => label_Finalize
  use NUOPC_BMI_ADAPTER
  
  implicit none
  
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
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(model,specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)     :: model
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock, compclock
    type(ESMF_TimeInterval) :: stabilityTimeStep
    integer, intent(out)    :: rc

    rc = ESMF_SUCCESS


    call BMIAdapter_Initialize("",rc) ! Initialize BMI Model
    if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter Initialize BMI Failed", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (localPet .eq. 0) then
        call BMIAdapter_PrintComponentInfo() ! Print BMI information after initializing
    end if

    if (localPet .eq. 0) then
        call BMIAdapter_PrintAllVarInfo()
    end if

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
    character(item_name_length), pointer    :: invarnames(:), outvarnames(:)
    integer                                     :: i
    
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

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.
    
!    call NUOPC_ClockPrintCurrTime(clock, &
!      "------>Advancing MODEL from: ", rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
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

end module
