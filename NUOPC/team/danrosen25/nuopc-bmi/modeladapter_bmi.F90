module MODELADAPTER_BMI

  !-----------------------------------------------------------------------------
  ! MODEL Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance
  use bmif
  
  implicit none
  
  private
  
  public SetServices
  
  type(BMI_Model) :: bmodel

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
    
  end subroutine
  
  !-----------------------------------------------------------------------------



  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)     :: model
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock, compclock
    type(ESMF_Time)         :: reftime, startTime, stopTime
    type(ESMF_TimeInterval) :: stabilityTimeStep, interval
    integer, intent(out)    :: rc
    real                    :: start, end, step
    character(len=10)       :: units
    character(item_name_length), pointer    :: invarnames(:), outvarnames(:)
    integer                                     :: i

    rc = ESMF_SUCCESS

    call BMI_Initialize(bmodel,"") ! Initialize BMI without config file
    call BMIAdapter_PrintComponentInfo() ! Print BMI information after initializing
    call BMIAdapter_PrintAllVarInfo()

    call BMI_Get_time_step(bmodel,step)
    call BMI_Get_start_time(bmodel,start)
    call BMI_Get_end_time(bmodel,end)
    call BMI_Get_time_units (bmodel, units)

    if (units .eq. 's') then
      call ESMF_TimeIntervalSet(stabilityTimeStep, s = INT(step), rc=rc)
    else if(units .eq. 'm') then
      call ESMF_TimeIntervalSet(stabilityTimeStep, m = INT(step), rc=rc)
    else
      call ESMF_TimeIntervalSet(stabilityTimeStep, s = INT(step), rc=rc)
    end if
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    compclock = NUOPC_ClockInitialize(clock, stabilityTimeStep, rc)
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
    if(ESMF_LogFoundError(rcToCheck=rc,msg="Add Fields Error", &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

#define WITHIMPORTFIELDS_OFF
#ifdef WITHIMPORTFIELDS
    ! importable field(s): Get Input Var Names from BMI

    call BMI_Get_input_var_names(bmodel,invarnames)
    do i=1,SIZE(invarnames)
        call NUOPC_StateAdvertiseField(importState, &
            StandardName=trim(invarnames(i)), name=trim(invarnames(i)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    end do
#endif

    ! exportable field(s): Get Output Var Names from BMI
    call BMI_Get_output_var_names(bmodel,outvarnames)

    do i=1,SIZE(outvarnames)
        call NUOPC_StateAdvertiseField(exportState, &
            StandardName=trim(outvarnames(i)), name=trim(outvarnames(i)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    end do

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
    if (ESMF_LogFoundError(rcToCheck=rc, msg="BMIAdapter_SingleGridCreate failure.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

#ifdef WITHIMPORTFIELDS
    ! importable field: add BMI input variable names
    call BMI_Get_input_var_names(bmodel,invarnames)
    do i=1,size(invarnames)
        field = ESMF_FieldCreate(name=trim(invarnames(i)), grid=gridIn, &
          typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call NUOPC_StateRealizeField(importState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
    end do
#endif

    ! exportable field: add BMI output variable names
    call BMI_Get_output_var_names(bmodel,outvarnames)
    do i=1,size(outvarnames)
        field = ESMF_FieldCreate(name=trim(outvarnames(i)), grid=gridOut, &
          typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
    end do

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    real                          :: bmi_time

    call BMI_Update(bmodel) ! Update BMI
    call BMI_Get_current_time(bmodel,bmi_time)
    print *,"BMI_Update Complete - Time: ", bmi_time

    rc = ESMF_SUCCESS
    
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

  end subroutine

  !========================================
  ! Print BMI model information
  !========================================

  subroutine BMIAdapter_PrintComponentInfo()
    implicit none
    character(len=item_name_length), pointer        :: invarnames(:)
    character(len=item_name_length), pointer     :: outvarnames(:)
    character(len=component_name_length),pointer    :: compname
    real                            :: start
    real                            :: end
    real                            :: step
    integer :: i

    call BMI_Get_input_var_names(bmodel,invarnames)
    call BMI_Get_output_var_names(bmodel,outvarnames)
    call BMI_Get_component_name(bmodel,compname)
    call BMI_Get_time_step(bmodel,step)
    call BMI_Get_start_time(bmodel,start)
    call BMI_Get_end_time(bmodel,end)

    print *, "BMI Component Info"
    print *, "     Step: ",step
    print *, "     Start: ",start
    print *, "     End: ",end
    print *, "     # Input Variables: ",SIZE(invarnames)
    print *, "     Input Variable Names: ",invarnames
    print *, "     # Output Variables: ",SIZE(outvarnames)
    print *, "     Output Variable Names: ",outvarnames
    print *, "     Component Name: ",compname
  end subroutine

  !========================================
  ! Print all available info for a sigle
  ! variable
  !========================================

  subroutine BMIAdapter_PrintVarInfo(var_name,var_rank)
    implicit none
    character (len=item_name_length),pointer, intent (in) :: var_name
    integer, intent(in) :: var_rank
    integer             :: type
    character(len=10)   :: units, dict_units ! Assumed length for units string
    integer             :: gtype
    integer     :: gshape(1:var_rank)   ! Assumed shape for grid shape array
    real        :: gspacing(1:var_rank), gorigin(1:var_rank) ! Assumed shape for grid spacing array
    logical     :: in_dictionary
    integer     :: rc


    call BMI_Get_var_type (bmodel, var_name, type)
    call BMI_Get_var_units (bmodel, var_name, units)
    call BMI_Get_grid_type (bmodel, var_name, gtype)
    call BMI_Get_grid_shape (bmodel, var_name, gshape)
    call BMI_Get_grid_spacing (bmodel, var_name, gspacing)
    call BMI_Get_grid_origin (bmodel, var_name, gorigin)

    in_dictionary = NUOPC_FieldDictionaryHasEntry(var_name, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    print *, "     Type: ", type
    print *, "     Units: ", units
    print *, "     Rank: ", var_rank
    print *, "     Grid Type: ", gtype
    print *, "     Grid Shape: ", gshape
    print *, "     Grid Spacing: ", gspacing
    print *, "     Grid Origin: ", gorigin
    print *, "     In Dictionary: ", in_dictionary

    if (in_dictionary) then
        call NUOPC_FieldDictionaryGetEntry(var_name, canonicalUnits=dict_units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        print *,"     Dictionary Units: ",dict_units
    end if

  end subroutine

  !========================================
  ! Iterate over all input and output
  ! variables and print info.
  !========================================

  subroutine BMIAdapter_PrintAllVarInfo()
    implicit none
    character(len=item_name_length), pointer    :: invarnames(:), outvarnames(:)
    integer                                     :: i

    call BMI_Get_input_var_names(bmodel,invarnames)
    call BMI_Get_output_var_names(bmodel,outvarnames)

    do i=1,SIZE(invarnames)
        print *,"In Variable Info: ",invarnames(i)
        call BMIAdapter_PrintVarInfo(invarnames(i),BMIAdapter_GetRank(invarnames(i)))
    end do

    do i=1,SIZE(outvarnames)
        print *,"Out Variable Info: ",outvarnames(i)
        call BMIAdapter_PrintVarInfo(outvarnames(i),BMIAdapter_GetRank(invarnames(i)))
    end do

  end subroutine

  !========================================
  ! Add individual field to NUOPC Dictionary
  !========================================

  subroutine BMIAdapter_AddFieldToDictionary(var_name, rc)
    implicit none
    character (len=item_name_length),pointer, intent (in) :: var_name
    character(len=10)   :: units, dict_units ! Assumed length for units string
    integer                                     :: rc
    logical                                     :: in_dictionary

    rc = ESMF_SUCCESS
    call BMI_Get_var_units (bmodel, var_name, units)

    print *,"To be added: ",var_name," Length",LEN(var_name)

    in_dictionary = NUOPC_FieldDictionaryHasEntry(trim(var_name), rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    if (in_dictionary) then
        call NUOPC_FieldDictionaryGetEntry(trim(var_name), canonicalUnits=dict_units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        if (dict_units .ne. units) then
            rc=ESMF_RC_NOT_VALID
            return
        end if
    else
        call NUOPC_FieldDictionaryAddEntry(trim(var_name), canonicalUnits=units, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call ESMF_LogWrite("BMI Field Added <" // var_name // ":" // units // ">", ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__, &
            rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    end if

  end subroutine

  !========================================
  ! Add All Fields To Dictionary
  ! Iterate over all fields in input and
  ! output variable arrays.
  !========================================

  subroutine BMIAdapter_AddAllFieldsToDictionary(rc)
    implicit none
    character(len=item_name_length), pointer    :: invarnames(:), outvarnames(:)
    integer                                     :: i
    integer, intent(out)                        :: rc

    call BMI_Get_input_var_names(bmodel,invarnames)
    call BMI_Get_output_var_names(bmodel,outvarnames)

    do i=1,SIZE(invarnames)
        call BMIAdapter_AddFieldToDictionary(invarnames(i),rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
    end do
    do i=1,SIZE(outvarnames)
        call BMIAdapter_AddFieldToDictionary(outvarnames(i),rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
    end do
  end subroutine

  !========================================
  ! Generic BMI Wrapper Functions
  !========================================
    FUNCTION BMIAdapter_GetRank(var_name) result(rank)
        use bmif, only: Get_Rank => BMI_Get_var_rank
        implicit none
        INTEGER :: rank
        character (len=item_name_length),pointer, INTENT(in) :: var_name

        call Get_Rank (bmodel, var_name, rank)

    END FUNCTION BMIAdapter_GetRank

  !========================================
  ! Grid Creation Based on BMI
  !========================================

    FUNCTION BMIAdapter_SingleGridCreate(rc) result(return_grid)
        type(ESMF_Grid) :: return_grid
        real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max
        integer :: i_count, j_count, iterator1, iterator2
        integer,intent(out) :: rc
        character (item_name_length), pointer :: invarnames(:),outvarnames(:)
        real :: gridspacing(1:2)
        real :: gridorigin(1:2)
        integer :: gridshape(1:2)
        integer :: gridtype

        rc = ESMF_SUCCESS

        call BMI_Get_input_var_names (bmodel, invarnames)
        call BMI_Get_output_var_names (bmodel, outvarnames)

        ! If input fields and outputs field do not share the same grid
        ! then FAILURE.  Logic to create multiple field grids is not yet implemented
        do iterator1=1,SIZE(invarnames)
            do iterator2=1,SIZE(outvarnames)
                if (.NOT.(BMIAdapter_GridComparison(invarnames(iterator1),outvarnames(iterator2)))) then
                    rc = ESMF_RC_NOT_IMPL
                    return ! If variables do not share the same grid then return
                end if
            end do
        end do

        ! If grid is not uniform then FAILURE.  Logic to create non-uniform
        ! grids is not yet implemented
        call BMI_Get_grid_type(bmodel,invarnames(1),gridtype)
        if (gridtype .ne. BMI_GRID_TYPE_UNIFORM) then
            rc = ESMF_RC_NOT_IMPL
            return
        end if

        call BMI_Get_grid_origin (bmodel, invarnames(1), gridorigin)
        call BMI_Get_grid_spacing (bmodel, invarnames(1),gridspacing)
        call BMI_Get_grid_shape (bmodel,invarnames(1),gridshape)

        x_min = gridorigin(1)
        x_max = gridorigin(1)+gridspacing(1)*gridshape(1)
        y_min = gridorigin(2)
        y_max = gridorigin(2)+gridspacing(2)*gridshape(2)
        i_count = gridspacing(1)
        j_count = gridspacing(2)

        return_grid = NUOPC_GridCreateSimpleXY(x_min,y_min, x_max, y_max, &
            i_count, j_count, rc=rc)

        call ESMF_LogWrite("BMI Grid Created <NUOPCSimpleXY>", ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__, &
            rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    END FUNCTION BMIAdapter_SingleGridCreate

  !========================================
  ! Grid Comparison Function for BMI variables
  !========================================

    Function BMIAdapter_GridComparison(var_name_1,var_name_2) result (equivalent)
        character (item_name_length), intent(in) :: var_name_1,var_name_2
        real :: spacing_1(1:2), spacing_2(1:2)
        real :: origin_1(1:2), origin_2(1:2)
        integer :: shape_1(1:2), shape_2(1:2)
        integer :: gridtype_1, gridtype_2
        logical :: equivalent

        equivalent = .true.

        call BMI_Get_grid_origin (bmodel, var_name_1,origin_1)
        call BMI_Get_grid_origin (bmodel, var_name_2, origin_2)

        if(origin_1(1) .ne. origin_2 (1) .or. origin_1(2) .ne. origin_2(2)) then
            equivalent = .false.
            return
        end if

        call BMI_Get_grid_spacing (bmodel, var_name_1,spacing_1)
        call BMI_Get_grid_spacing (bmodel, var_name_2, spacing_2)

        if(spacing_1(1) .ne. spacing_2 (1) .or. spacing_1(2) .ne. spacing_2(2)) then
            equivalent = .false.
            return
        end if

        call BMI_Get_grid_shape (bmodel,var_name_1,shape_1)
        call BMI_Get_grid_shape (bmodel,var_name_2,shape_2)

        if(shape_1(1) .ne. shape_2 (1) .or. shape_1(2) .ne. shape_2(2)) then
            equivalent = .false.
            return
        end if

        call BMI_Get_grid_type(bmodel,var_name_2,gridtype_1)
        call BMI_Get_grid_type(bmodel,var_name_1,gridtype_2)

        if(gridtype_1 .ne. gridtype_2) then
            equivalent = .false.
            return
        end if

    End Function BMIAdapter_GridComparison

end module
