! $Id$

#define FILENAME "NUOPC_DriverCOAMPS.F90"

module NUOPC_DriverCOAMPS

  !-----------------------------------------------------------------------------
  ! Generic Driver Component for COAMPS with explicit time stepping
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, only: &
    Driver_routine_SS             => routine_SetServices, &
    Driver_type_IS                => type_InternalState, &
    Driver_label_IS               => label_InternalState, &
    Driver_label_SetModelCount    => label_SetModelCount, &
    Driver_label_SetModelPetLists => label_SetModelPetLists, &
    Driver_label_SetModelServices => label_SetModelServices, &
    Driver_label_Finalize         => label_Finalize

  implicit none
  
  private
  
  public routine_SetServices
  public type_InternalState, type_InternalStateStruct
  public label_InternalState, label_SetModelPetLists
  public label_SetModelServices, label_Finalize
  
  character(*), parameter :: &
    label_InternalState = "DriverCOAMPS_InternalState"
  character(*), parameter :: &
    label_SetModelPetLists = "DriverCOAMPS_SetModelPetLists"
  character(*), parameter :: &
    label_SetModelServices = "DriverCOAMPS_SetModelServices"
  character(*), parameter :: &
    label_Finalize = "DriverCOAMPS_Finalize"
  
  type type_InternalStateStruct
    type(ESMF_GridComp) :: med
    type(ESMF_State)    :: medIS, medES
    integer, pointer    :: medPetList(:)
    type(ESMF_GridComp) :: atm
    type(ESMF_State)    :: atmIS, atmES
    integer, pointer    :: atmPetList(:)
    type(ESMF_GridComp) :: ocn
    type(ESMF_State)    :: ocnIS, ocnES
    integer, pointer    :: ocnPetList(:)
    type(ESMF_GridComp) :: wav
    type(ESMF_State)    :: wavIS, wavES
    integer, pointer    :: wavPetList(:)
    type(ESMF_CplComp)  :: med2atm
    integer, pointer    :: med2atmPetList(:)
    type(ESMF_CplComp)  :: atm2med
    integer, pointer    :: atm2medPetList(:)
    type(ESMF_CplComp)  :: med2ocn
    integer, pointer    :: med2ocnPetList(:)
    type(ESMF_CplComp)  :: ocn2med
    integer, pointer    :: ocn2medPetList(:)
    type(ESMF_CplComp)  :: med2wav
    integer, pointer    :: med2wavPetList(:)
    type(ESMF_CplComp)  :: wav2med
    integer, pointer    :: wav2medPetList(:)
    type(ESMF_CplComp)  :: ocn2wav
    integer, pointer    :: ocn2wavPetList(:)
    type(ESMF_CplComp)  :: wav2ocn
    integer, pointer    :: wav2ocnPetList(:)
    type(NUOPC_RunSequence), pointer  :: runSeq(:)
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! NUOPC_Driver registers the generic methods
    call Driver_routine_SS(gcomp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! attach specializing method(s)
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelCount, &
      userRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelPetLists, &
      userRoutine=SetModelPetLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_SetModelServices, &
      userRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_MethodAdd(gcomp, label=Driver_label_Finalize, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelCount(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(Driver_type_IS)  :: superIS

    rc = ESMF_SUCCESS

    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
      
    ! set the modelCount
    superIS%wrap%modelCount = 4

  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelPetLists(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    type(Driver_type_IS)      :: superIS
    logical                   :: existflag

    rc = ESMF_SUCCESS

    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! nullify the petLists
    nullify(is%wrap%medPetList)
    nullify(is%wrap%atmPetList)
    nullify(is%wrap%ocnPetList)
    nullify(is%wrap%wavPetList)
    nullify(is%wrap%med2atmPetList)
    nullify(is%wrap%atm2medPetList)
    nullify(is%wrap%med2ocnPetList)
    nullify(is%wrap%ocn2medPetList)
    nullify(is%wrap%med2wavPetList)
    nullify(is%wrap%wav2medPetList)
    nullify(is%wrap%ocn2wavPetList)
    nullify(is%wrap%wav2ocnPetList)

    ! SPECIALIZE by calling into optional attached method to set modelPetLists
    call ESMF_MethodExecute(gcomp, label=label_SetModelPetLists, &
      existflag=existflag, userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    if (existflag) then
      ! query Component for super internal State
      nullify(superIS%wrap)
      call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out

      ! set the component petLists
      superIS%wrap%modelPetLists(1)%petList => is%wrap%medPetList
      superIS%wrap%modelPetLists(2)%petList => is%wrap%atmPetList
      superIS%wrap%modelPetLists(3)%petList => is%wrap%ocnPetList
      superIS%wrap%modelPetLists(4)%petList => is%wrap%wavPetList

      ! set the connector petLists
      superIS%wrap%connectorPetLists(1,2)%petList => is%wrap%med2atmPetList
      superIS%wrap%connectorPetLists(2,1)%petList => is%wrap%atm2medPetList
      superIS%wrap%connectorPetLists(1,3)%petList => is%wrap%med2ocnPetList
      superIS%wrap%connectorPetLists(3,1)%petList => is%wrap%ocn2medPetList
      superIS%wrap%connectorPetLists(1,4)%petList => is%wrap%med2wavPetList
      superIS%wrap%connectorPetLists(4,1)%petList => is%wrap%wav2medPetList
      superIS%wrap%connectorPetLists(3,4)%petList => is%wrap%ocn2wavPetList
      superIS%wrap%connectorPetLists(4,3)%petList => is%wrap%wav2ocnPetList
    endif

  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                   :: localrc, stat
    type(Driver_type_IS)      :: superIS
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS
    
    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(gcomp, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! map components and states
    is%wrap%med = superIS%wrap%modelComp(1)
    is%wrap%medIS = superIS%wrap%modelIS(1)
    is%wrap%medES = superIS%wrap%modelES(1)
    is%wrap%atm = superIS%wrap%modelComp(2)
    is%wrap%atmIS = superIS%wrap%modelIS(2)
    is%wrap%atmES = superIS%wrap%modelES(2)
    is%wrap%ocn = superIS%wrap%modelComp(3)
    is%wrap%ocnIS = superIS%wrap%modelIS(3)
    is%wrap%ocnES = superIS%wrap%modelES(3)
    is%wrap%wav = superIS%wrap%modelComp(4)
    is%wrap%wavIS = superIS%wrap%modelIS(4)
    is%wrap%wavES = superIS%wrap%modelES(4)

    ! map connectors
    is%wrap%med2atm = superIS%wrap%connectorComp(1,2)
    is%wrap%atm2med = superIS%wrap%connectorComp(2,1)
    is%wrap%med2ocn = superIS%wrap%connectorComp(1,3)
    is%wrap%ocn2med = superIS%wrap%connectorComp(3,1)
    is%wrap%med2wav = superIS%wrap%connectorComp(1,4)
    is%wrap%wav2med = superIS%wrap%connectorComp(4,1)
    is%wrap%ocn2wav = superIS%wrap%connectorComp(3,4)
    is%wrap%wav2ocn = superIS%wrap%connectorComp(4,3)

    ! maybe too much? but maybe nice to have the component names specified?
    call ESMF_GridCompSet(is%wrap%atm, name="ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridCompSet(is%wrap%ocn, name="OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridCompSet(is%wrap%wav, name="WAV", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_GridCompSet(is%wrap%med, name="MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%med2atm, name="MED2ATM", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%atm2med, name="ATM2MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%med2ocn, name="MED2OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%ocn2med, name="OCN2MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%med2wav, name="MED2WAV", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%wav2med, name="WAV2MED", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%ocn2wav, name="OCN2WAV", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    call ESMF_CplCompSet(is%wrap%wav2ocn, name="WAV2OCN", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! The default run sequence defined by the generic Driver Component is not
    ! suitable for COAMPS. The default RunSeq must be overwritten.
    call NUOPC_RunSequenceDeallocate(superIS%wrap%runSeq, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! add a single run sequence element
    call NUOPC_RunSequenceAdd(superIS%wrap%runSeq, 1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out    
    ! atm2med in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=2, j=1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! ocn2med in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=3, j=1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! wav2med in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=4, j=1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! med     in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=0, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! med2atm in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=2, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! med2ocn in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=3, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! med2wav in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=1, j=4, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! ocn2wav in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=3, j=4, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! wav2ocn in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=4, j=3, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! atm     in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=2, j=0, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! ocn     in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=3, j=0, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    ! wav     in runSeq(1)
    call NUOPC_RunElementAdd(superIS%wrap%runSeq(1), i=4, j=0, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    ! nullify the runSeq
    nullify(is%wrap%runSeq)
    
    ! SPECIALIZE by calling into attached method to SetServices for modelComps
    call ESMF_MethodExecute(gcomp, label=label_SetModelServices, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

    ! optionally overwrite the default run sequence
    if (associated(is%wrap%runSeq)) then
      call NUOPC_RunSequenceDeallocate(superIS%wrap%runSeq, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=FILENAME)) &
        return  ! bail out
      superIS%wrap%runSeq => is%wrap%runSeq
    endif

  end subroutine

  !-----------------------------------------------------------------------------
  
  subroutine Finalize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    logical                   :: existflag

    rc = ESMF_SUCCESS

    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(gcomp, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=FILENAME)) &
      return  ! bail out

    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=FILENAME, &
      rcToReturn=rc)) &
      return  ! bail out

  end subroutine

end module
