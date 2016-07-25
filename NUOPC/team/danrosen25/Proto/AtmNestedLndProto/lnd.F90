module LND

  !-----------------------------------------------------------------------------
  ! LND Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS            => SetServices, &
    model_label_DataInitialize  => label_DataInitialize, &
    model_label_SetClock        => label_SetClock, &
    model_label_SetRunClock     => label_SetRunClock, &
    model_label_Advance         => label_Advance, &
    model_label_Finalize        => label_Finalize
   
  implicit none
  
  private

  ! Field coupling exposed to NUOPC
  type FieldDesc
    character(len=64)   :: stdname =" "
    character(len=64)   :: shortname = " "
    character(len=64)   :: transferOffer = " "
    logical             :: import = .FALSE.
    logical             :: export = .FALSE.
  endtype FieldDesc

  ! Internal state to keep instance private data
  type InternalStateStruct
    integer                             :: verbosity = -1
    integer                             :: nnests = -1
    type(ESMF_Grid),allocatable         :: grids(:)
    type(ESMF_TimeInterval),allocatable :: timesteps(:)
    type(ESMF_TimeInterval),allocatable :: elapsedtimes(:)
    type(ESMF_Clock),allocatable        :: clocks(:)
    integer                             :: nfields = -1
    type(FieldDesc),allocatable         :: fields(:)
    integer                             :: slice = -1
    type(ESMF_State),allocatable        :: NStateImp(:)
    type(ESMF_State),allocatable        :: NStateExp(:)
  endtype

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  endtype

  public SetServices
  public InternalStateStruct
  public InternalState
 
  ! Sample configuration
  integer,parameter :: &
    verbosity = 255, &
    nnests = 3, &
    nfields = 3
!  integer,parameter,dimension(nnests) :: &
!    iCount = (/ 180,319,484 /), &
!    jCount = (/ 150,313,376 /)
  integer,parameter,dimension(nnests) :: &
    iCount = (/ 100,100,100 /), &
    jCount = (/ 10 ,10 ,10  /)
  real(ESMF_KIND_R8),parameter,dimension(nnests) :: &
    dt = (/ 10.D0, 30.D0, 60.D0 /)
!  real(ESMF_KIND_R8),parameter,dimension(nnests) :: &
!    minLat = (/ 13.90549, 22.54604, 26.77635 /), &
!    minLon = (/ 240.9351, 256.2747, 264.4102 /), &
!    maxLat = (/ 54.25622, 50.20378, 45.15566 /), &
!    maxLon = (/ 313.4248, 297.7253, 289.7091 /)
  real(ESMF_KIND_R8),parameter,dimension(nnests) :: &
    iMinCornerCoord = (/ 10._ESMF_KIND_R8,10._ESMF_KIND_R8,10._ESMF_KIND_R8 /), &
    iMaxCornerCoord = (/ 100._ESMF_KIND_R8,100._ESMF_KIND_R8,100._ESMF_KIND_R8 /), &
    jMinCornerCoord = (/ 20._ESMF_KIND_R8,20._ESMF_KIND_R8,20._ESMF_KIND_R8 /), &
    jMaxCornerCoord = (/ 200._ESMF_KIND_R8,200._ESMF_KIND_R8,200._ESMF_KIND_R8 /)
  type(FieldDesc),parameter,dimension(nfields) :: fields = &
    (/ FieldDesc( &
         "air_pressure_at_sea_level", &
         "pmsl", &
         "will provide", &
         .TRUE., &
         .FALSE.), &
       FieldDesc( &
         "surface_net_downward_shortwave_flux", &
         "rsns", &
         "will provide", &
         .TRUE., &
         .FALSE.), &
       FieldDesc( &
         "sea_surface_temperature", &
         "sst", &
         "will provide", &
         .FALSE., &
         .TRUE.) &
     /)
 
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(InternalState)   :: is
    integer               :: stat

    rc = ESMF_SUCCESS       
 
    ! -> allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    is%wrap%verbosity = verbosity

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! overwrite the default IPDv00 with IPDv02
    call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      userRoutine=ModelInitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    ! IPDv03p1 - NUOPC Model advertises the import and export fields. 
    !            (Specialized)
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=ModelInitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! IPDv03p3 - NUOPC Model realizes the import and export fields. 
    !            (Specialized)
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=ModelInitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!    ! IPDv03p4 - NUOPC Model checks for compatibility of the fields'
!    !            connected status.
!    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
!      phaseLabelList=(/"IPDv03p4"/), userRoutine=ModelInitializeP5, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    ! IPDv03p5 - NUOPC Model handles field data initialization and
!    !            timestamps the export fields.
!    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
!      phaseLabelList=(/"IPDv03p5"/), userRoutine=ModelInitializeP5, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
!    ! attach specializing method(s)
!    call NUOPC_CompSpecialize(model, specLabel=model_label_SetRunClock, &
!      specRoutine=ModelSetRunClock, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_DataInitialize, &
      specRoutine=ModelDataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=model_label_SetClock, &
      specRoutine=ModelSetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set model name so it becomes identifiable
    call ESMF_GridCompSet(model, name="LND", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail outend subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelInitializeP0(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: model
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    rc = ESMF_SUCCESS

    ! switch to IPDv00 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelInitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)  :: is
    integer              :: stat
    integer              :: nIndex, fIndex
    character(len=3)     :: nStr

    rc = ESMF_SUCCESS

    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    is%wrap%nnests = nnests
    allocate( &
      is%wrap%NStateImp(is%wrap%nnests), &
      is%wrap%NStateExp(is%wrap%nnests), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of nested state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! add namespace
    call NUOPC_AddNamespace(importState, &
      namespace="1", &
      nestedStateName="NestedStateImp_N1", &
      nestedState=is%wrap%NStateImp(1), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_AddNamespace(exportState, &
      namespace="1", &
      nestedStateName="NestedStateExp_N1", &
      nestedState=is%wrap%NStateExp(1), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do nIndex = 2, is%wrap%nnests
      if (nIndex > 999) then
        call ESMF_LogSetError(ESMF_FAILURE, &
          msg="Maximum nest size is 999.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif
      write (nStr,"(I0)") nIndex
      call NUOPC_AddNamespace(importState, &
        namespace=trim(nStr), &
        nestedStateName="NestedStateImp_N"//trim(nStr), &
        nestedState=is%wrap%NStateImp(nIndex), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_AddNamespace(exportState, &
        namespace=trim(nStr), &
        nestedStateName="NestedStateExp_N"//trim(nStr), &
        nestedState=is%wrap%NStateExp(nIndex), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    is%wrap%nfields = nfields
    allocate(is%wrap%fields(is%wrap%nfields), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of fields memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    is%wrap%fields = fields

    !!
    !! advertise import and export fields
    !!

    do nIndex = 1, is%wrap%nnests    
    do fIndex = 1, is%wrap%nfields
      if (is%wrap%fields(fIndex)%import) then
        call NUOPC_Advertise(is%wrap%NStateImp(nIndex), &
          StandardName=trim(is%wrap%fields(fIndex)%stdname), &
          name=trim(is%wrap%fields(fIndex)%shortname), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (is%wrap%fields(fIndex)%export) then
        call NUOPC_Advertise(is%wrap%NStateExp(nIndex), &
          StandardName=trim(is%wrap%fields(fIndex)%stdname), &
          name=trim(is%wrap%fields(fIndex)%shortname), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    enddo
    enddo

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ModelInitializeP3(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(InternalState)     :: is
    integer                 :: stat
    type(ESMF_Field)        :: field
    integer                 :: nIndex, fIndex
    logical                 :: impConn, expConn
    character(ESMF_MAXSTR)  :: logMsg
    
    rc = ESMF_SUCCESS

    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(is%wrap%grids(is%wrap%nnests), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of grids memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
 
    ! create a grid object for each nest
    do nIndex = 1, is%wrap%nnests

      is%wrap%grids(nIndex) = ESMF_GridCreateNoPeriDimUfrm( &
        maxIndex=(/iCount(nIndex),jCount(nIndex)/), &
        minCornerCoord=(/iMinCornerCoord(nIndex), jMinCornerCoord(nIndex)/), &
        maxCornerCoord=(/iMaxCornerCoord(nIndex), jMaxCornerCoord(nIndex)/), &
        coordSys=ESMF_COORDSYS_CART, &
        staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    do nIndex = 1, is%wrap%nnests
    do fIndex = 1, is%wrap%nfields
      if (is%wrap%fields(fIndex)%import) then
        impConn = NUOPC_IsConnected(is%wrap%NStateImp(nIndex), &
          fieldName=is%wrap%fields(fIndex)%shortname)
      else
        impConn = .FALSE.
      endif

      if (is%wrap%fields(fIndex)%export) then
        expConn = NUOPC_IsConnected(is%wrap%NStateExp(nIndex), &
          fieldName=is%wrap%fields(fIndex)%shortname)
      else
        expConn = .FALSE.
      endif

      if (impConn .or. expConn) then
        field = ESMF_FieldCreate(name=is%wrap%fields(fIndex)%shortname, &
          grid=is%wrap%grids(nIndex), typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out  
      endif

      if (impConn) then
        call NUOPC_Realize(is%wrap%NStateImp(nIndex), field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        write (logMsg, "(A,I0,A,A)") &
          "Field realized in import state(nest=",nIndex,"): ", &
          trim(is%wrap%fields(fIndex)%shortname)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, &
          line=__LINE__,file=__FILE__,rc=rc)
      elseif(is%wrap%fields(fIndex)%import) then
        call ESMF_StateRemove(is%wrap%NStateImp(nIndex), &
          (/trim(is%wrap%fields(fIndex)%shortname)/), &
          relaxedflag=.true.,rc=rc)
        write (logMsg, "(A,I0,A,A)") &
          "Field removed from import state(nest=",nIndex,"): ", &
          trim(is%wrap%fields(fIndex)%shortname)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, &
          line=__LINE__,file=__FILE__,rc=rc)
      endif

      if (expConn) then
        call NUOPC_Realize(is%wrap%NStateExp(nIndex), field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        write (logMsg, "(A,I0,A,A)") &
          "Field realized in export state(nest=",nIndex,"): ", &
          trim(is%wrap%fields(fIndex)%shortname)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, &
          line=__LINE__,file=__FILE__,rc=rc)
      elseif(is%wrap%fields(fIndex)%export) then
        call ESMF_StateRemove(is%wrap%NStateExp(nIndex), &
          (/trim(is%wrap%fields(fIndex)%shortname)/), &
          relaxedflag=.true.,rc=rc)
        write (logMsg, "(A,I0,A,A)") &
          "Field removed from export state(nest=",nIndex,"): ", &
          trim(is%wrap%fields(fIndex)%shortname)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, &
          line=__LINE__,file=__FILE__,rc=rc)
      endif
    enddo
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelDataInitialize(model, rc)
    type(ESMF_GridComp)   :: model
    integer, intent(out)  :: rc

    ! local variables
    type(InternalState)                    :: is
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    real(ESMF_KIND_R8), pointer            :: dataPtrR8D2(:,:)
    integer                                :: nIndex, iIndex
    integer                                :: stat
    type(ESMF_State)                       :: exportState

    rc = ESMF_SUCCESS

    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    is%wrap%slice = 0

    ! query the Component for its exportState
    call NUOPC_ModelGet(model, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
   
    do nIndex = 1, is%wrap%nnests

      ! initialize export fields to nest * 10
      call ESMF_StateGet(is%wrap%NStateExp(nIndex), &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate( &
        itemNameList(itemCount), &
        itemTypeList(itemCount), &
        stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of state item list memory failed.", &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_StateGet(is%wrap%NStateExp(nIndex), &
        itemNameList=itemNameList, itemTypeList=itemTypeList, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do iIndex = 1, itemCount
        if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(is%wrap%NStateExp(nIndex), &
            field=field, itemName=itemNameList(iIndex), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_FieldGet(field, farrayPtr=dataPtrR8D2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! initialize the entire array
          dataPtrR8D2 = nIndex * 10._ESMF_KIND_R8
          call NUOPC_SetAttribute(field, &
            name="Updated", value="true", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
      enddo

      deallocate(itemNameList, itemTypeList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of state item list memory failed.", &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    enddo

    if (NUOPC_IsUpdated(exportState)) then
      call NUOPC_CompAttributeSet(model, &
        name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelSetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)           :: is
    integer                       :: stat
    integer                       :: nIndex
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

    rc = ESMF_SUCCESS

    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate( &
      is%wrap%timesteps(is%wrap%nnests), &
      is%wrap%elapsedtimes(is%wrap%nnests), &
      is%wrap%clocks(is%wrap%nnests), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of clock and timestep memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query the Component for its clock
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do nIndex = 1, is%wrap%nnests
      call ESMF_TimeIntervalSet(is%wrap%timesteps(nIndex), &
        s_r8=dt(nIndex), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_TimeIntervalSet(is%wrap%elapsedtimes(nIndex), &
        s_r8=0._ESMF_KIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      is%wrap%clocks(nIndex) = ESMF_ClockCreate(clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_ClockSet(is%wrap%clocks(nIndex), &
        timeStep=is%wrap%timesteps(nIndex), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    call NUOPC_CompSetClock(model, clock, &
      is%wrap%timesteps(is%wrap%nnests), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelSetRunClock(model,rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)       :: is
    type(ESMF_Clock)          :: driverClock
    rc = ESMF_SUCCESS

    ! query Component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set component clock to inner nest's clock
    call ESMF_GridCompSet(model, &
      clock=is%wrap%clocks(is%wrap%nnests), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query component for driver clock
    call NUOPC_ModelGet(model, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check and set the model clock against the driver clock
    call NUOPC_CompCheckSetClock(model, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="NUOPC INCOMPATIBILITY DETECTED: between model and driver clocks", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(InternalState)           :: is
    character(len=64)             :: modelName
    type(ESMF_Clock)              :: modelClock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Time)               :: modelCurrTime
    type(ESMF_TimeInterval)       :: modelTimeStep
    integer                       :: nIndex
    character(len=3)              :: nStr
    integer                       :: stat

    rc = ESMF_SUCCESS
    
    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    is%wrap%slice = is%wrap%slice + 1

    ! query the component for its name
    call ESMF_GridCompGet(model, name=modelName,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=modelClock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(modelClock, options="currTime", &
      preString="------>Advancing "//trim(modelName)//" from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, &
      timeStep=modelTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_TimePrint(modelCurrTime + modelTimeStep, &
      preString="-------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do nIndex = 1, is%wrap%nnests
      if ( nIndex > 999) then
        call ESMF_LogSetError(ESMF_FAILURE, &
          msg="Maximum nest size is 999.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out 
      endif
      write (nStr,"(I0)") nIndex

      ! output import data
      call NUOPC_Write(is%wrap%NStateImp(nIndex), &
        fileNamePrefix=trim(modelName)//"_import_"//trim(nStr), &
        timeslice=is%wrap%slice, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out      

      is%wrap%elapsedtimes(nIndex) = is%wrap%elapsedtimes(nIndex) + modelTimeStep

      if ( is%wrap%elapsedtimes(nIndex) >= is%wrap%timesteps(nIndex) ) then
        call StateAdvance(is,is%wrap%NStateExp(nIndex),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_ClockAdvance(is%wrap%clocks(nIndex), &
          timestep=is%wrap%timesteps(nIndex),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        is%wrap%elapsedtimes(nIndex) = &
          is%wrap%elapsedtimes(nIndex) - is%wrap%timesteps(nIndex)
      endif
      ! output export data
      call NUOPC_Write(is%wrap%NStateExp(nIndex), &
        fileNamePrefix=trim(modelName)//"_export_"//trim(nStr), &
        timeslice=is%wrap%slice, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine StateAdvance(is, state, rc)
    type(InternalState)  :: is
    type(ESMF_State)     :: state
    integer, intent(out) :: rc

    ! local variables
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)    
    type(ESMF_Field)                       :: field
    real(ESMF_KIND_R8), pointer            :: dataPtrR8D2(:,:)
    integer                                :: nIndex, iIndex
    integer                                :: stat

    rc = ESMF_SUCCESS

    ! advance export fields
    call ESMF_StateGet(state, &
      itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(itemNameList(itemCount), &
      itemTypeList(itemCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_StateGet(state, &
      itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do iIndex = 1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD ) then
        call ESMF_StateGet(state, &
          field=field, itemName=itemNameList(iIndex), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_FieldGet(field, farrayPtr=dataPtrR8D2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! update the entire array
        dataPtrR8D2 = dataPtrR8D2 + 1
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(InternalState)  :: is
    integer              :: stat
    integer              :: nIndex

    rc = ESMF_SUCCESS

    ! -> get internal state from Component
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -> destroy objects inside of internal state and deallocate memory
    do nIndex=1, is%wrap%nnests
!      call ESMF_StateDestroy(is%wrap%NStateImp(nIndex), rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, &
!        file=__FILE__)) &
!        return
!      call ESMF_StateDestroy(is%wrap%NStateExp(nIndex), rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, &
!        file=__FILE__)) &
!        return
      call ESMF_ClockDestroy(is%wrap%clocks(nIndex), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
      call ESMF_GridDestroy(is%wrap%grids(nIndex), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    enddo
    deallocate( &
      is%wrap%grids, &
      is%wrap%timesteps, &
      is%wrap%elapsedtimes, &
      is%wrap%clocks, &
      is%wrap%fields, &
      is%wrap%NStateImp, &
      is%wrap%NStateExp, &
      stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of nest memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -> deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

end module
