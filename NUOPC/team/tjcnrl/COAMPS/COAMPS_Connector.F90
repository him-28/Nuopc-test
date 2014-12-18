!-------------------------------------------------------------------------------
! COAMPS Connector Component
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_Connector.F90"
#include "COAMPS_Macros.h"


module COAMPS_Connector

  use ESMF
  use NUOPC
! use NUOPC_Connector, parent_SetServices => SetServices
  use NUOPC_Connector, only: parent_SetServices => SetServices, &
    label_ComputeRouteHandle, label_ExecuteRouteHandle, &
    label_ReleaseRouteHandle, label_Finalize, &
    NUOPC_ConnectorGet, NUOPC_ConnectorSet
  use COAMPS_Futil
  
  implicit none
  
  private
  
  public SetServices

  character (*), parameter :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    logical :: verbose
    type(ESMF_VM)                   :: vm
    character(6)                    :: conType
    integer                         :: cplCount
    integer(ESMF_KIND_I4)  ,pointer :: srcMaskValues(:) => null()
    integer(ESMF_KIND_I4)  ,pointer :: dstMaskValues(:) => null()
    character(ESMF_MAXSTR) ,pointer :: srcNames(:) => null()
    character(ESMF_MAXSTR) ,pointer :: dstNames(:) => null()
    type(ESMF_RouteHandle)          :: remapRH
    type(ESMF_Field)                :: remapStatusField
    type(ExtendRouteHandle)         :: extendRH
    logical                         :: includeABG
    logical                         :: includeOBG
    logical                         :: includeWBG
    integer(ESMF_KIND_I4)           :: numwt
    character(ESMF_MAXSTR) ,pointer :: wtnam(:) => null()
    integer(ESMF_KIND_I4)  ,pointer :: wtcnt(:) => null()
    real(ESMF_KIND_R8)     ,pointer :: wtime(:) => null()
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
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer                       :: i

    rc = ESMF_SUCCESS

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! initialize timers
    is%wrap%numwt = 9
    allocate(is%wrap%wtnam(is%wrap%numwt), is%wrap%wtcnt(is%wrap%numwt), &
      is%wrap%wtime(is%wrap%numwt), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of wall timer memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    is%wrap%wtnam(1) = 'InitializeP0'
    is%wrap%wtnam(2) = 'ComputeRH'
    is%wrap%wtnam(3) = 'RemapStore'
    is%wrap%wtnam(4) = 'ExtendStore'
    is%wrap%wtnam(5) = 'ExecuteRH'
    is%wrap%wtnam(6) = 'Remap'
    is%wrap%wtnam(7) = 'Extend'
    is%wrap%wtnam(8) = 'ReleaseRH'
    is%wrap%wtnam(9) = 'Finalize'
    is%wrap%wtcnt(:) = 0
    is%wrap%wtime(:) = 0d0

    ! the NUOPC connector component will register the generic methods
    call NUOPC_CompDerive(ccomp, parent_SetServices, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set initialize phase 0 requires use of ESMF method
    call ESMF_CplCompSetEntryPoint(ccomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(ccomp, specLabel=label_ComputeRouteHandle, &
      specRoutine=ComputeRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(ccomp, specLabel=label_ExecuteRouteHandle, &
      specRoutine=ExecuteRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(ccomp, specLabel=label_ReleaseRouteHandle, &
      specRoutine=ReleaseRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(ccomp, specLabel=label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(ccomp, importState, exportState, clock, rc)
    type(ESMF_CplComp)   :: ccomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    character(ESMF_MAXSTR)        :: verbosity
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=1, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer                       :: i
    character(ESMF_MAXSTR)        :: attrString

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! determine verbosity
    call ESMF_AttributeGet(ccomp, name='Verbosity', value=verbosity, &
      defaultValue='low', convention='NUOPC', purpose='General', rc=rc)
!   call NUOPC_CompAttributeGet(ccomp, name='Verbosity', value=verbosity, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (trim(verbosity)=='high') then
      is%wrap%verbose = .true.
    else
      is%wrap%verbose = .false.
    endif
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered InitializeP0', ESMF_LOGMSG_INFO)

    ! get connector type
    call ESMF_AttributeGet(ccomp, name='ConnectorType', value=attrString, &
      defaultValue='bilinr', convention='COAMPS', purpose='General', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    select case (trim(attrString))
    case ('memcpy','redist','bilinr','bicubc')
      is%wrap%conType = trim(attrString)
    case default
      write(msgString,'(a)') trim(cname)//': ConnectorType not supported: '// &
        trim(attrString)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
      return ! bail out
    endselect

    ! get background model info
    call ESMF_AttributeGet(ccomp, name='AtmBackground', value=attrString, &
      defaultValue='none', convention='COAMPS', purpose='General', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    is%wrap%includeABG = trim(attrString).eq.'model' .or. &
                         trim(attrString).eq.'mediator'
    call ESMF_AttributeGet(ccomp, name='OcnBackground', value=attrString, &
      defaultValue='none', convention='COAMPS', purpose='General', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    is%wrap%includeOBG = trim(attrString).eq.'model' .or. &
                         trim(attrString).eq.'mediator'
    call ESMF_AttributeGet(ccomp, name='WavBackground', value=attrString, &
      defaultValue='none', convention='COAMPS', purpose='General', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    is%wrap%includeWBG = trim(attrString).eq.'model' .or. &
                         trim(attrString).eq.'mediator'

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving InitializeP0', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ComputeRH(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=2, it2=3, it3=4
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    integer                       :: i
    character(ESMF_MAXSTR), pointer :: cplList(:)
    integer                       :: srcCount, dstCount
    type(ESMF_FieldBundle)        :: srcFields, dstFields
    type(ESMF_Config)             :: config
    character(ESMF_MAXSTR)        :: label
    integer                       :: maskValueInlandWater
    integer                       :: maskValueWater
    integer                       :: maskValueLand
    integer                       :: maskValueFrozenWater
    integer                       :: maskValueFrozenLand
    integer                       :: numValidSrc, numMaskedSrc
    integer                       :: numNotInSrc, numExtendFill
    logical                       :: redist

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ComputeRH', ESMF_LOGMSG_INFO)

    ! query Component for its config & vm
    call ESMF_CplCompGet(ccomp, config=config, vm=is%wrap%vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get size of couple list
    call NUOPC_CompAttributeGet(ccomp, cplListSize=is%wrap%cplCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    write(msgString,'(a,i0,a)') trim(cname)// &
      ': List of coupled fields (',is%wrap%cplCount,'):'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    ! if no coupled fields, then return
    if (is%wrap%cplCount.eq.0) goto 1

    ! get field bundles from connecter internal state
    call NUOPC_ConnectorGet(ccomp, srcFields=srcFields, dstFields=dstFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! report the cplList and FieldBundle lists
    call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (is%wrap%cplCount.ne.srcCount .or. is%wrap%cplCount.ne.dstCount) then
      write(msgString,'(a)') trim(cname)// &
        ': cplList count does not agree with FieldBundle counts'
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
      return ! bail out
    endif
    write(msgString,'(a,a5,a,a10,a,a10,a3,a)') &
      trim(cname)//': ','index',' ','srcField',' ','dstField',' ','standardName'
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    allocate(is%wrap%srcNames(is%wrap%cplCount), is%wrap%dstNames(is%wrap%cplCount), &
      cplList(is%wrap%cplCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of cplList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call NUOPC_CompAttributeGet(ccomp, cplList=cplList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(srcFields, fieldNameList=is%wrap%srcNames, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldNameList=is%wrap%dstNames, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    do i=1, is%wrap%cplCount
      write(msgString,'(a,i5,a,a10,a,a10,a3,a)') &
        trim(cname)//': ',i,' ',trim(is%wrap%srcNames(i)),' ', &
        trim(is%wrap%dstNames(i)),'   ',trim(cplList(i))
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    deallocate(cplList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of cplList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! get mask values
    call ESMF_ConfigGetAttribute(config, maskValueInlandWater, &
      default=COAMPS_MASK_INLAND_WATER, label='mask_value_inland_water:', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ConfigGetAttribute(config, maskValueWater, &
      default=COAMPS_MASK_WATER, label='mask_value_water:', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ConfigGetAttribute(config, maskValueLand, &
      default=COAMPS_MASK_LAND, label='mask_value_land:', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ConfigGetAttribute(config, maskValueFrozenWater, &
      default=COAMPS_MASK_FROZEN_WATER, label='mask_value_frozen_water:', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ConfigGetAttribute(config, maskValueFrozenLand, &
      default=COAMPS_MASK_FROZEN_LAND, label='mask_value_frozen_land:', rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set source mask values
    select case (trim(cname))
      case ('ATM-TO-WAV')
        allocate(is%wrap%srcMaskValues(0), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of srcMaskValues() failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
      case default
        allocate(is%wrap%srcMaskValues(4), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of srcMaskValues() failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        is%wrap%srcMaskValues = (/ maskValueInlandWater, &
                                   maskValueLand,        &
                                   maskValueFrozenWater, &
                                   maskValueFrozenLand   /)
    endselect

    ! set destination mask values
    select case (trim(cname))
      case default
        allocate(is%wrap%dstMaskValues(4), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of dstMaskValues() failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        is%wrap%dstMaskValues = (/ maskValueInlandWater, &
                                   maskValueLand,        &
                                   maskValueFrozenWater, &
                                   maskValueFrozenLand   /)
    endselect

    ! store remap
    call ESMF_VMWtime(ws2Time)
    select case (is%wrap%conType)
    case ('redist','bilinr','bicubc')
      call FieldBundleRemapStore(srcFields, dstFields, is%wrap%remapRH, &
        is%wrap%vm, cname=trim(cname), remapType=is%wrap%conType, &
        srcMaskValues=is%wrap%srcMaskValues, dstMaskValues=is%wrap%dstMaskValues, &
        remapStatusField=is%wrap%remapStatusField, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      numValidSrc = RemapStatusCheck(is%wrap%remapStatusField, &
        REMAP_STATUS_VALID_SRC, is%wrap%vm, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      write(msgString,'(a,i10)') trim(cname)// &
        ': RemapStore: RemapStatusCheck: # VALID_SRC =   ',numValidSrc
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
      numNotInSrc = RemapStatusCheck(is%wrap%remapStatusField, &
        REMAP_STATUS_NOT_IN_SRC, is%wrap%vm, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      write(msgString,'(a,i10)') trim(cname)// &
        ': RemapStore: RemapStatusCheck: # NOT_IN_SRC =  ',numNotInSrc
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
      numMaskedSrc = RemapStatusCheck(is%wrap%remapStatusField, &
        REMAP_STATUS_MASKED_SRC, is%wrap%vm, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      write(msgString,'(a,i10)') trim(cname)// &
        ': RemapStore: RemapStatusCheck: # MASKED_SRC =  ',numMaskedSrc
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    endselect
    call ESMF_VMWtime(wf2Time)
    is%wrap%wtime(it2) = is%wrap%wtime(it2) + wf2Time - ws2Time
    is%wrap%wtcnt(it2) = is%wrap%wtcnt(it2) + 1

    ! store extend fill
    call ESMF_VMWtime(ws3Time)
    select case (is%wrap%conType)
    case ('redist','bilinr','bicubc')
      call FieldBundleExtendStore(dstFields, is%wrap%extendRH, &
        is%wrap%vm, is%wrap%remapStatusField, cname=trim(cname), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      numMaskedSrc = RemapStatusCheck(is%wrap%remapStatusField, &
        REMAP_STATUS_MASKED_SRC, is%wrap%vm, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      write(msgString,'(a,i10)') trim(cname)// &
        ': ExtendStore: RemapStatusCheck: # MASKED_SRC =  ',numMaskedSrc
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
      numExtendFill = RemapStatusCheck(is%wrap%remapStatusField, &
        REMAP_STATUS_EXTEND_FILL, is%wrap%vm, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      write(msgString,'(a,i10)') trim(cname)// &
        ': ExtendStore: RemapStatusCheck: # EXTEND_FILL = ',numExtendFill
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    endselect
    call ESMF_VMWtime(wf3Time)
    is%wrap%wtime(it3) = is%wrap%wtime(it3) + wf3Time - ws3Time
    is%wrap%wtcnt(it3) = is%wrap%wtcnt(it3) + 1

    ! output remapStatus and trap unmapped cells
    select case (is%wrap%conType)
    case ('redist','bilinr','bicubc')
      call RemapStatusWrite(is%wrap%remapStatusField, trim(cname), is%wrap%vm, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      select case (trim(cname))
      case ('MED-TO-ATM','MED-TO-OCN','MED-TO-WAV','MED-TO-ICE','MED-TO-LND', &
            'ABG-TO-MED','OBG-TO-MED','WBG-TO-MED')
        if (numMaskedSrc.gt.0.or.numNotInSrc.gt.0) then
          write(msgString,'(a)') trim(cname)// &
          ': '//cname(8:10)//' grid cells unmapped from '// &
          cname(1:3)//' grid is not supported'
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
          return ! bail out
        endif
      case ('OBG-TO-ATM','WBG-TO-ATM','ABG-TO-OCN','ABG-TO-WAV')
        if (numMaskedSrc.gt.0.or.numNotInSrc.gt.0) then
          write(msgString,'(a)') trim(cname)// &
          ': '//cname(8:10)//' grid cells unmapped from '// &
          cname(1:3)//' grid is not supported'
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
          return ! bail out
        endif
      case ('ATM-TO-OCN','ATM-TO-WAV','ATM-TO-ICE','ATM-TO-LND','ATM-TO-MED')
        if ((numMaskedSrc.gt.0.or.numNotInSrc.gt.0).and..not.is%wrap%includeABG) then
          write(msgString,'(a)') trim(cname)// &
          ': '//cname(8:10)//' grid cells unmapped from '// &
          cname(1:3)//' grid requires active ABG'
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
          return ! bail out
        endif
      case ('OCN-TO-ATM','OCN-TO-WAV','OCN-TO-ICE','OCN-TO-MED')
        if ((numMaskedSrc.gt.0.or.numNotInSrc.gt.0).and..not.is%wrap%includeOBG) then
          write(msgString,'(a)') trim(cname)// &
          ': '//cname(8:10)//' grid cells unmapped from '// &
          cname(1:3)//' grid requires active OBG'
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
          return ! bail out
        endif
      case ('WAV-TO-ATM','WAV-TO-MED')
        if ((numMaskedSrc.gt.0.or.numNotInSrc.gt.0).and..not.is%wrap%includeWBG) then
          write(msgString,'(a)') trim(cname)// &
          ': '//cname(8:10)//' grid cells unmapped from '// &
          cname(1:3)//' grid requires active WBG'
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
          return ! bail out
        endif
      case ('WAV-TO-OCN','WAV-TO-ICE') ! *** WBG currenly only exports ATM fields ***
        if (numMaskedSrc.gt.0.or.numNotInSrc.gt.0) then
          write(msgString,'(a)') trim(cname)// &
          ': '//cname(8:10)//' grid cells unmapped from '// &
          cname(1:3)//' grid is not supported'
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msgString), rcToReturn=rc)
          return ! bail out
        endif
      endselect
    endselect

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ComputeRH', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1
 
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ExecuteRH(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=5, it2=6, it3=7
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time
    type(ESMF_FieldBundle)        :: srcFields, dstFields

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no coupled fields, then return
    if (is%wrap%cplCount.eq.0) return

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ExecuteRH', ESMF_LOGMSG_INFO)

    ! get field bundles from connecter internal state
    call NUOPC_ConnectorGet(ccomp, srcFields=srcFields, dstFields=dstFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! fill dst field with 'missing value'
    call FieldBundleFill(dstFields, missingValue, dstMaskVal=is%wrap%dstMaskValues, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! apply remap
    call ESMF_VMWtime(ws2Time)
    select case (is%wrap%conType)
    case ('redist','bilinr','bicubc')
      if (verbose) &
      call ESMF_LogWrite(trim(cname)//': execute FieldBundleRemap', ESMF_LOGMSG_INFO)
      call FieldBundleRemap(srcFields, dstFields, is%wrap%remapRH, &
        zeroRegion=ESMF_REGION_SELECT, checkFlag=.false., rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    case ('memcpy')
      if (verbose) &
      call ESMF_LogWrite(trim(cname)//': execute FieldBundleCopy', ESMF_LOGMSG_INFO)
      call FieldBundleCopy(dstFields, srcFields, &
        dstMaskVal=is%wrap%dstMaskValues, srcMaskVal=is%wrap%srcMaskValues, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endselect
    call ESMF_VMWtime(wf2Time)
    is%wrap%wtime(it2) = is%wrap%wtime(it2) + wf2Time - ws2Time
    is%wrap%wtcnt(it2) = is%wrap%wtcnt(it2) + 1

    ! extend fill on destination fields
    call ESMF_VMWtime(ws3Time)
    select case (is%wrap%conType)
    case ('redist','bilinr','bicubc')
      if (verbose) &
      call ESMF_LogWrite(trim(cname)//': execute FieldBundleExtend', ESMF_LOGMSG_INFO)
      call FieldBundleExtend(dstFields, is%wrap%extendRH, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endselect
    call ESMF_VMWtime(wf3Time)
    is%wrap%wtime(it3) = is%wrap%wtime(it3) + wf3Time - ws3Time
    is%wrap%wtcnt(it3) = is%wrap%wtcnt(it3) + 1

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ExecuteRH', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1
 
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ReleaseRH(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=8, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no coupled fields, then return
    if (is%wrap%cplCount.eq.0) return

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered ReleaseRH', ESMF_LOGMSG_INFO)

    ! release remap
    select case (is%wrap%conType)
    case ('redist','bilinr','bicubc')
      call FieldBundleRemapRelease(is%wrap%remapRH, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endselect

    ! release extend
    call FieldBundleExtendRelease(is%wrap%extendRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! release remapStatusField
!*** let ESMF automatic garbage collection take care of this ***
!   call ESMF_FieldDestroy(is%wrap%remapStatusField, rc=rc)
!   if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! deallocate mask arrays
    if (associated(is%wrap%srcMaskValues)) then
      deallocate(is%wrap%srcMaskValues, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of srcMaskValues array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%dstMaskValues)) then
      deallocate(is%wrap%dstMaskValues, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of dstMaskValues array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    ! deallocate field name arrays
    if (associated(is%wrap%srcNames)) then
      deallocate(is%wrap%srcNames, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of srcNames array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%dstNames)) then
      deallocate(is%wrap%dstNames, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of dstNames array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving ReleaseRH', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(ccomp, rc)
    type(ESMF_CplComp)   :: ccomp
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)        :: cname
    character(ESMF_MAXSTR)        :: msgString
    logical                       :: verbose
    type(type_InternalState)      :: is
    integer                       :: lrc, stat
    integer, parameter            :: it1=9, it2=0, it3=0
    real(ESMF_KIND_R8)            :: ws1Time, wf1Time
    real(ESMF_KIND_R8)            :: ws2Time, wf2Time
    real(ESMF_KIND_R8)            :: ws3Time, wf3Time

    rc = ESMF_SUCCESS

    ! start timing
    call ESMF_VMWtime(ws1Time)

    ! query the Component for its name
    call ESMF_CplCompGet(ccomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(ccomp, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    verbose = is%wrap%verbose

    ! if no coupled fields, then return
    if (is%wrap%cplCount.eq.0) return

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered Finalize', ESMF_LOGMSG_INFO)

    ! finish timing
    call ESMF_VMWtime(wf1Time)
    is%wrap%wtime(it1) = is%wrap%wtime(it1) + wf1Time - ws1Time
    is%wrap%wtcnt(it1) = is%wrap%wtcnt(it1) + 1

    ! print timing
    call PrintTimers(trim(cname), is%wrap%wtnam, is%wrap%wtcnt, is%wrap%wtime)

    ! deallocate timers
    if (associated(is%wrap%wtnam)) then
      deallocate(is%wrap%wtnam, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtnam array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%wtcnt)) then
      deallocate(is%wrap%wtcnt, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtcnt array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    if (associated(is%wrap%wtime)) then
      deallocate(is%wrap%wtime, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of wtime array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    ! deallocate internal state memory
    if (associated(is%wrap)) then
      deallocate(is%wrap, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of internal state memory failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

end module
