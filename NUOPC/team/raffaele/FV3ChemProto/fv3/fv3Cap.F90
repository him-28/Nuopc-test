module fv3Cap

  use ESMF
  use NUOPC

  use NUOPC_Model, only: &
    model_routine_SS           => SetServices,          &
    model_label_Advance        => label_Advance,        &
    model_label_Finalize       => label_Finalize,       &
    model_label_CheckImport    => label_CheckImport

  use module_fcst_grid_comp,  only: fcstSS => SetServices, &
                                    fcstGrid, &
                                    numLevels, numTracers, &
                                    numSoilLayers

  use module_wrt_grid_comp,   only: wrtSS => SetServices

  use module_cplfields,       only: exportFields, &
                                    exportFieldsList, &
                                    exportFieldTypes


  implicit none

  type(ESMF_GridComp) :: fcstComp,  wrtComp
  type(ESMF_State)    :: fcstState, wrtState

contains

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    type(ESMF_Config)    :: config

    rc = ESMF_SUCCESS

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv01
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! checking the import fields is a bit more complex because of coldstart option
    call ESMF_MethodRemove(gcomp, model_label_CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_CheckImport, &
      specRoutine=NUOPC_NoOp, rc=rc)  !TODO: replace this with a real method
      !TODO: for now just disable all timestamp checking of import fields
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create, open, and set the config
    config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigLoadFile(config, "fv3.configure", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine InitializeP0

  !-----------------------------------------------------------------------------

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    integer :: item, itemcount, urc
    integer :: petcount, mype
    integer :: mpi_comm_atm
    integer :: num_pes_fcst, write_groups, wrttasks_per_group
    integer, dimension(:), allocatable :: petList, fcstPetList
    type(ESMF_VM) :: vm
    type(ESMF_Config) :: config

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(gcomp,vm=vm,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, mpiCommunicator=mpi_comm_atm,petCount=petcount, &
             localpet = mype,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- read # tasks for write component from config
    write_groups = 1
    wrttasks_per_group = 0

    call ESMF_GridCompGet(gcomp, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ConfigFindLabel(config, label="write_tasks:", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ConfigGetAttribute(config, wrttasks_per_group, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    write(6,'("FV3: Dedicate ",i0," PETs to writing data to disk.")') wrttasks_per_group

    num_pes_fcst = petcount - write_groups * wrttasks_per_group
    if (num_pes_fcst < 1) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
        msg="No PETs available for forecast component!",&
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    end if
    allocate(fcstPetList(num_pes_fcst))
    do item=1, num_pes_fcst
      fcstPetList(item) = item - 1
    enddo
    fcstComp = ESMF_GridCompCreate(petList=fcstPetList, name='fv3_fcst', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
    call ESMF_GridCompSetServices(fcstComp, fcstSS, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

! create fcst state
    fcstState = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

! call fcst Initialize (including creating fcstgrid and fcst fieldbundle)
    call ESMF_GridCompInitialize(fcstComp, exportState=fcstState,    &
         clock=clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! reconcile the fcstComp's import state
    call ESMF_StateReconcile(fcstState, attreconflag=ESMF_ATTRECONCILE_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! determine number elements in fcstState
    call ESMF_StateGet(fcstState, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (wrttasks_per_group > 0) then
      ! -- write component
      allocate(petList(wrttasks_per_group))

      itemCount = size(fcstPetList)
      do item = 1, wrttasks_per_group
        petList(item) = itemCount + item - 1
      end do

      wrtComp = ESMF_GridCompCreate(petList=petList, name='wrtComp', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

! call into wrtComp(i) SetServices
      call ESMF_GridCompSetServices(wrtComp, wrtSS, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

! create wrtstate(i)
      wrtState = ESMF_StateCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

! add the fcst FieldBundles to the wrtState(i) so write component can
! use this info to create mirror objects
      call ESMF_AttributeCopy(fcstState, wrtState, &
        attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

!   call ESMF_StateAdd(wrtState(i), fcstFB, rc=rc)
!   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__, &
!     file=__FILE__)) &
!     call ESMF_Finalize(endflag=ESMF_END_ABORT)

! call into wrtComp(i) Initialize
      call ESMF_GridCompInitialize(wrtComp, importState=wrtState, &
        clock=clock, phase=1, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

! remove fcst FieldBundles from the wrtState(i) because done with it
!   call ESMF_StateRemove(wrtState(i), fcstItemNameList, rc=rc)
!   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__, &
!     file=__FILE__)) &
!     call ESMF_Finalize(endflag=ESMF_END_ABORT)

! reconcile the wrtComp(i)'s export state
      call ESMF_StateReconcile(wrtState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! determine number elements in fcstState
      call ESMF_StateGet(wrtState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_AttributeCopy(fcstState, wrtState, &
        attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      deallocate(petList)
    end if

    deallocate(fcstPetList)

    if (ESMF_GridCompIsPetLocal(fcstComp, rc=rc)) then
      ! advertise import fields

      ! advertise export fields
      call NUOPC_Advertise(exportState, StandardNames=exportFieldsList, &
        SharePolicyField="share", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    end if

  end subroutine InitializeAdvertise

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables

    ! --- conditionally realize or remove Fields in importState and exportState -------------------
    rc = ESMF_SUCCESS

    if (ESMF_GridCompIsPetLocal(fcstComp, rc=rc)) then

      call realizeConnectedCplFields(exportState, fcstGrid, &
        exportFieldsList, exportFieldTypes, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__)) &
        return  ! bail out

    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine realizeConnectedCplFields(state, grid, fieldNames, fieldTypes, rc)

      type(ESMF_State),            intent(inout) :: state
      type(ESMF_Grid),                intent(in) :: grid
      character(len=*), dimension(:), intent(in) :: fieldNames, fieldTypes
      integer,                       intent(out) :: rc

      ! local variables
      integer          :: item
      type(ESMF_Field) :: field

      ! begin
      rc = ESMF_SUCCESS

      if (size(fieldNames) /= size(fieldTypes)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="fieldNames and fieldTypes must have same size.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      end if

      do item = 1, size(fieldNames)
        if (NUOPC_IsConnected(state, fieldName=trim(fieldNames(item)))) then
          select case (fieldTypes(item))
            case ('l','layer')
              field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                name=trim(fieldNames(item)), &
                ungriddedLBound=(/1/), ungriddedUBound=(/numLevels/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            case ('i','interface')
              field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                name=trim(fieldNames(item)), &
                ungriddedLBound=(/1/), ungriddedUBound=(/numLevels+1/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            case ('t','tracer')
              field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                name=trim(fieldNames(item)), &
                ungriddedLBound=(/1,1/), ungriddedUBound=(/numLevels, numTracers/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            case ('s','surface')
              field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                name=trim(fieldNames(item)), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            case ('g','soil')
              field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                name=trim(fieldNames(item)), &
                ungriddedLBound=(/1/), ungriddedUBound=(/numSoilLayers/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            case default
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="exportFieldType = '"//trim(fieldTypes(item))//"' not recognized", &
                line=__LINE__, file=__FILE__)
              return
          end select
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return
          ! -- save field
          exportFields(item) = field
        else
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/trim(fieldNames(item))/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        end if
      end do

    end subroutine realizeConnectedCplFields

  end subroutine InitializeRealize

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)                    :: gcomp
    integer, intent(out)                   :: rc

    ! -- local variables
    integer :: urc
    integer :: item
    type(ESMF_Clock) :: clock

    logical, save :: first = .false.
    character(len=ESMF_MAXSTR) :: name, value
    type(ESMF_Field) :: field

    ! -- begin
    rc = ESMF_SUCCESS

    print *,'Entering FV3_ADVANCE'

    ! Component internal Clock gets updated per NUOPC rules
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- run forecast model
    call ESMF_GridCompRun(fcstComp, exportState=fcstState, clock=clock,userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="leaving FV3_ADVANCE with clock current: ")

    call ESMF_ClockPrint(clock, options="startTime", &
      preString="leaving FV3_ADVANCE with clock start:   ")

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="leaving FV3_ADVANCE with clock stop:    ")

  end subroutine ModelAdvance


  subroutine ModelFinalize(gcomp, rc)
    type(ESMF_GridComp)                    :: gcomp
    integer, intent(out)                   :: rc

    ! -- local variables
    integer :: urc

    ! -- begin
    rc = ESMF_SUCCESS

    call ESMF_GridCompFinalize(fcstComp, exportState=fcstState, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_StateDestroy(fcstState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (ESMF_GridCompIsCreated(wrtComp)) then
      call ESMF_GridCompFinalize(wrtComp, importState=wrtState, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

    if (ESMF_StateIsCreated(wrtState)) then
      call ESMF_StateDestroy(wrtState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if

  end subroutine ModelFinalize

end module fv3Cap
