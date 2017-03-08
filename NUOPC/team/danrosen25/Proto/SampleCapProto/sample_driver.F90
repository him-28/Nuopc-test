#define MODNAME "SAMPLE_DRIVER"

module sample_driver_mod

  !-----------------------------------------------------------------------------
  ! Code that specializes generic NUOPC_Driver
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices

#define STR_(x) #x
#define STR(x) STR_(x)
#ifdef COMP1_MOD
      use COMP1_MOD, only: COMP1_SS => SetServices
#endif
#ifdef COMP2_MOD
      use COMP2_MOD, only: COMP2_SS => SetServices
#endif
#ifdef COMP3_MOD
      use COMP3_MOD, only: COMP3_SS => SetServices
#endif
#ifdef COMP4_MOD
      use COMP4_MOD, only: COMP4_SS => SetServices
#endif
#ifdef COMP5_MOD
      use COMP5_MOD, only: COMP5_SS => SetServices
#endif
  
  implicit none
  
  private
  
  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'
  CHARACTER(LEN=16), DIMENSION(10), PARAMETER :: CUSTOMFIELDLIST = (/ &
    "dummy_field_1", "dummy_field_2", &
    "dummy_field_3", "dummy_field_4", &
    "dummy_field_5", "dummy_field_6", &
    "dummy_field_7", "dummy_field_8", &
    "dummy_field_9", "dummy_field_10" /)
  CHARACTER(LEN=10), DIMENSION(10), PARAMETER :: CUSTOMUNITSLIST = (/ &
    "Pa",            "kg",    &
    "W m-2",         "m",     &
    "kg",            "m s-1", &
    "kg",            "W m-2", &
    "m",             "K"      /)

  type model_internalstate_type
    integer  :: verbosity        = 1
    integer  :: timeStepSeconds  = 3600
    integer  :: stepCount        = 24
  end type

  type model_internalstate_wrapper
    type(model_internalstate_type), pointer :: wrap
  end type
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "SetServices"

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS
    
    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "SetModelServices"

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR)            :: dname
    integer                           :: stat
    type(model_internalstate_wrapper) :: is
    type(ESMF_Config)                 :: config
    character(ESMF_MAXSTR)            :: tmpStr
    type(NUOPC_FreeFormat)            :: attrFF
    type(ESMF_Time)                   :: startTime
    type(ESMF_Time)                   :: stopTime
    type(ESMF_TimeInterval)           :: timeStep
    type(ESMF_Clock)                  :: internalClock
    integer                           :: compListSize
    character(len=32), allocatable    :: compList(:)
    character(len=32)                 :: compMod
    character(len=32)                 :: compName
    integer                           :: i
    type(ESMF_GridComp)               :: child

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query the Driver for info
    call ESMF_GridCompGet(driver, name=dname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! allocate memory for this internal state and set it in the
    ! component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      method=METHOD, file=__FILE__, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! create, open, and set the config
    config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_ConfigLoadFile(config, "sample.rc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
    call ESMF_GridCompSet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! read and ingest free format driver attributes
    attrFF = NUOPC_FreeFormatCreate(config, &
      label=trim(dname)//"_attributes::", relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
    call NUOPC_CompAttributeIngest(driver, attrFF, addFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call ESMF_AttributeGet(driver, name="Verbosity", value=tmpStr, &
      defaultValue="default", convention="NUOPC", purpose="Instance", &
      rc=rc)
    is%wrap%verbosity = ESMF_UtilString2Int(tmpStr, &
      specialStringList=(/"default","none","max"/), &
      specialValueList=(/is%wrap%verbosity,0,255/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call ESMF_AttributeGet(driver, name="TimeStepSeconds", value=tmpStr, &
      defaultValue="default", convention="NUOPC", purpose="Instance", &
      rc=rc)
    is%wrap%timeStepSeconds = ESMF_UtilString2Int(tmpStr, &
      specialStringList=(/"default"/), &
      specialValueList=(/is%wrap%timeStepSeconds/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    call ESMF_AttributeGet(driver, name="StepCount", value=tmpStr, &
      defaultValue="default", convention="NUOPC", purpose="Instance", &
      rc=rc)
    is%wrap%stepCount = ESMF_UtilString2Int(tmpStr, &
      specialStringList=(/"default"/), &
      specialValueList=(/is%wrap%stepCount/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    if (is%wrap%verbosity .gt. 0) then

      write (tmpStr, "(A,(A,I0))") trim(dname), &
        ': Verbosity=',is%wrap%verbosity
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)

      write (tmpStr, "(A,(A,I0))") trim(dname), &
        ': Time Step (seconds)=',is%wrap%timeStepSeconds
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)

      write (tmpStr, "(A,(A,I0))") trim(dname), &
        ': Step Count=',is%wrap%stepCount
      call ESMF_LogWrite(trim(tmpStr),ESMF_LOGMSG_INFO)

    endif

    ! set the driver clock
    call ESMF_TimeSet(startTime, s = 0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeSet(stopTime, &
      s=(is%wrap%timeStepSeconds * is%wrap%stepCount), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeStep, s=is%wrap%timeStepSeconds, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    internalClock = ESMF_ClockCreate(name=trim(dname)//" Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! determine the component list
    compListSize = ESMF_ConfigGetLen(config, &
      label=trim(dname)//"_component_list:", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out
    allocate(compList(compListSize), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of component list memory failed.", &
      method=METHOD, file=__FILE__, rcToReturn=rc)) return ! bail out
    call ESMF_ConfigGetAttribute(config, valueList=compList, &
      label=trim(dname)//"_component_list:", count=compListSize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! determine information for each component and add to the driver
    do i=1, compListSize

      call ESMF_ConfigGetAttribute(config, compMod, &
        label=trim(compList(i))//"_mod:", &
        default=trim(compList(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      call ESMF_ConfigGetAttribute(config, compName, &
        label=trim(compList(i))//"_name:", &
        default=trim(compList(i)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

      if (is%wrap%verbosity .gt. 0) then
#ifdef COMP1_MOD
        call ESMF_LogWrite(trim(dname)//": COMP1_MOD is "// &
          STR(COMP1_MOD),ESMF_LOGMSG_INFO)
#endif
#ifdef COMP2_MOD
        call ESMF_LogWrite(trim(dname)//": COMP2_MOD is "// &
          STR(COMP2_MOD),ESMF_LOGMSG_INFO)
#endif
#ifdef COMP3_MOD
        call ESMF_LogWrite(trim(dname)//": COMP3_MOD is "// &
          STR(COMP3_MOD),ESMF_LOGMSG_INFO)
#endif
#ifdef COMP4_MOD
        call ESMF_LogWrite(trim(dname)//": COMP4_MOD is "// &
          STR(COMP4_MOD),ESMF_LOGMSG_INFO)
#endif
#ifdef COMP5_MOD
        call ESMF_LogWrite(trim(dname)//": COMP5_MOD is "// &
          STR(COMP5_MOD),ESMF_LOGMSG_INFO)
#endif
      endif

      select case (trim(compMod))
#ifdef COMP1_MOD
        case (STR(COMP1_MOD))
          ! SetServices for component 1
          call NUOPC_DriverAddComp(driver, trim(compName), COMP1_SS, &
            comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return ! bail out
#endif
#ifdef COMP2_MOD
        case (STR(COMP2_MOD))
          ! SetServices for component 2
          call NUOPC_DriverAddComp(driver, trim(compName), COMP2_SS, &
            comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return ! bail out
#endif
#ifdef COMP3_MOD
        case (STR(COMP3_MOD))
          ! SetServices for component 3
          call NUOPC_DriverAddComp(driver, trim(compName), COMP3_SS, &
            comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return ! bail out
#endif
#ifdef COMP4_MOD
        case (STR(COMP4_MOD))
          ! SetServices for component 4
          call NUOPC_DriverAddComp(driver, trim(compName), COMP4_SS, &
            comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return ! bail out
#endif
#ifdef COMP5_MOD
        case (STR(COMP5_MOD))
          ! SetServices for component 5
          call NUOPC_DriverAddComp(driver, trim(compName), COMP5_SS, &
            comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return ! bail out
#endif
        case default
          call ESMF_LogSetError(rcToCheck=ESMF_RC_NOT_FOUND, &
            msg=trim(compMod)//" was requested "// &
              "but is not available in the executable!", &
            method=METHOD, file=__FILE__, rcToReturn=rc)
          return ! bail out
      end select

      if (is%wrap%verbosity .gt. 0) then
        call ESMF_LogWrite(trim(dname)//": "//trim(compName)// &
          " added to the components.",ESMF_LOGMSG_INFO)
      endif

      ! read and ingest free format component attributes
      attrFF = NUOPC_FreeFormatCreate(config, &
        label=trim(compList(i))//"_attributes::", relaxedflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
      call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out
      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return ! bail out

    enddo

    deallocate(compList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of component list memory failed.", &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call CustomFieldSetup(driver, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "CustomFieldSetup"

  subroutine CustomFieldSetup(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)            :: dname
    type(model_internalstate_wrapper) :: is
    integer                           :: i

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! query the Driver for info
    call ESMF_GridCompGet(driver, name=dname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return ! bail out

    do i=1, size(CUSTOMFIELDLIST)

      if (.NOT.NUOPC_FieldDictionaryHasEntry(trim(CUSTOMFIELDLIST(i)))) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName=trim(CUSTOMFIELDLIST(i)), &
          canonicalUnits=trim(CUSTOMUNITSLIST(i)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return ! bail out
        if (is%wrap%verbosity .ge. 1) then
          call ESMF_LogWrite(trim(dname)//": Added "// &
            trim(CUSTOMFIELDLIST(i)), ESMF_LOGMSG_INFO)
        endif
      endif

    enddo

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

end module
