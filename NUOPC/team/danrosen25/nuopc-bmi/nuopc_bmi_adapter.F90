module NUOPC_BMI_ADAPTER

  !-----------------------------------------------------------------------------
  ! MODEL Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use bmif

  implicit none

  type(BMI_Model) :: bmodel ! The BMI compliant model configured by the configuration file or default model values

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  !########################################
  !# BMI Wrapper Routines                 #
  !# The following functions wrap a BMI   #
  !# model.                               #
  !########################################

  !========================================
  ! Print BMI model information
  !========================================

  subroutine BMIAdapter_PrintComponentInfo()
    implicit none
    character(len=item_name_length), pointer        :: invarnames(:)
    character(len=item_name_length), pointer     :: outvarnames(:)
    character(len=component_name_length),pointer    :: compname
    character(len=10)                               ::tunits
    real                            :: start
    real                            :: end
    real                            :: step
    integer :: i
    character(ESMF_MAXSTR) :: config_file
    integer :: rc

    rc = ESMF_SUCCESS

    call BMIAdapter_GetArgConfigFile(config_file,rc)
    call BMI_Get_input_var_names(bmodel,invarnames)
    call BMI_Get_output_var_names(bmodel,outvarnames)
    call BMI_Get_component_name(bmodel,compname)
    call BMI_Get_time_step(bmodel,step)
    call BMI_Get_start_time(bmodel,start)
    call BMI_Get_end_time(bmodel,end)
    call BMI_Get_time_units(bmodel,tunits)

    print *, "BMI Component Info"
    print *, "     Config File: ",trim(config_file)
    print *, "     Step: ",step
    print *, "     Start: ",start
    print *, "     End: ",end
    print *, "     Time Units: ",tunits
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
  ! Print Field Value
  !========================================

  SUBROUTINE BMIAdapter_PrintFieldArray(state,varname,rc)
    type(ESMF_State),intent(in) :: state
    character(*),intent(in) :: varname
    integer,intent(out) :: rc

    type(ESMF_Field) :: field
    type(ESMF_Array) :: array

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state,itemName=varname,field=field,rc=rc)
    if(rc .ne. ESMF_SUCCESS) return
    call ESMF_FieldGet(field, array=array, rc=rc)
    if(rc .ne. ESMF_SUCCESS) return
    call ESMF_ArrayPrint(array, rc=rc)
    if(rc .ne. ESMF_SUCCESS) return

  END SUBROUTINE

  !========================================
  ! Print BMI model current time
  !========================================

  SUBROUTINE BMIAdapter_PrintCurrentTime()
    real                          :: bmi_time

    call BMI_Get_current_time(bmodel,bmi_time)
    print *,"BMI Current Time: ", bmi_time

  END SUBROUTINE

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
  ! Command Line Config File
  !========================================

  SUBROUTINE BMIAdapter_GetArgConfigFile(config_file,rc)
    character(*),intent(out) :: config_file
    integer,intent(out) :: rc

    integer :: argindex, arglength

    rc = ESMF_SUCCESS

    call ESMF_UtilGetArgIndex("-bmiconfig", argindex=argindex, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

    if(argindex .ge. 0) then
        call ESMF_UtilGetArg(argindex+1, argvalue=config_file, arglength=arglength, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        if (arglength .gt. len(config_file)) then
            rc = ESMF_RC_ARG_VALUE
            return
        end if
    else
        rc = ESMF_RC_ARG_VALUE
    end if
  END SUBROUTINE

  !========================================
  ! Initialize BMI model
  !========================================

  SUBROUTINE BMIAdapter_Initialize(config_file,rc)
    character(*),intent(in) :: config_file
    integer,intent(out) :: rc

    character(component_name_length),pointer :: compname
    integer :: argindex, arglength

    rc = ESMF_SUCCESS

    call BMI_Initialize(bmodel,config_file) ! Initialize BMI without config file

    call BMIAdapter_PrintComponentInfo() ! Print BMI information after initializing
    call BMIAdapter_PrintAllVarInfo()
    call BMI_Get_component_name(bmodel,compname)
    call ESMF_LogWrite("BMI initialized <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

  END SUBROUTINE

  !========================================
  ! Update BMI model
  !========================================

  SUBROUTINE BMIAdapter_Update(rc)
    integer, intent(out) :: rc
    character(component_name_length),pointer :: compname

    rc = ESMF_SUCCESS

    call BMI_Update(bmodel) ! Update BMI
    call BMIAdapter_PrintCurrentTime() ! Print BMI model time after update
    call BMI_Get_component_name(bmodel,compname)
    call ESMF_LogWrite("BMI updated <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return
  END SUBROUTINE

  !========================================
  ! Finalize BMI model
  !========================================

  SUBROUTINE BMIAdapter_Finalize(rc)
    integer, intent(out) :: rc
    character(component_name_length),pointer :: compname

    rc = ESMF_SUCCESS

    call BMI_Finalize(bmodel) ! Finalize BMI
    call BMI_Get_component_name(bmodel,compname)
    call ESMF_LogWrite("BMI finalized <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
    if (rc .ne. ESMF_SUCCESS) return

  END SUBROUTINE

  !========================================
  ! Set Time Interval based on BMI model
  !========================================

  subroutine BMIAdapter_TimeIntervalSet(stabilityTimeStep, rc)
    type(ESMF_TimeInterval), intent(out) :: stabilityTimeStep
    integer, intent(out)    :: rc
    real                    :: step
    character(len=10)       :: units

    call BMI_Get_time_step(bmodel,step)
    call BMI_Get_time_units(bmodel,units)

    rc = ESMF_SUCCESS

    if (units .eq. 's') then
      call ESMF_TimeIntervalSet(stabilityTimeStep, s = INT(step), rc=rc)
    else if(units .eq. 'm') then
      call ESMF_TimeIntervalSet(stabilityTimeStep, m = INT(step), rc=rc)
    else
      rc = ESMF_RC_ARG_VALUE
    end if
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
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_LogWrite("BMI field added <" // trim(var_name) // ":" // trim(units) // ">", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
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
  ! Advertise all input fields to referenced state variable
  !========================================

    SUBROUTINE BMIAdapter_StateAdvertiseInputFields(importState,rc)
        type(ESMF_State)        :: importState
        integer, intent(out)    :: rc
        character(item_name_length), pointer    :: invarnames(:)
        integer                                     :: i

        ! importable field(s): Get Input Var Names from BMI
        rc = ESMF_SUCCESS

        call BMI_Get_input_var_names(bmodel,invarnames)

        do i=1,SIZE(invarnames)
            call NUOPC_StateAdvertiseField(importState, &
                StandardName=trim(invarnames(i)), name=trim(invarnames(i)), rc=rc)
            if (rc .ne. ESMF_SUCCESS) return  ! bail out
        end do
    END SUBROUTINE

  !========================================
  ! Advertise all output fields ot referenced state variable
  !========================================

    SUBROUTINE BMIAdapter_StateAdvertiseOutputFields(exportState,rc)
        type(ESMF_State)        :: exportState
        integer, intent(out)    :: rc
        character(item_name_length), pointer    ::  outvarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        ! exportable field(s): Get Output Var Names from BMI
        call BMI_Get_output_var_names(bmodel,outvarnames)

        do i=1,SIZE(outvarnames)
            call NUOPC_StateAdvertiseField(exportState, &
                StandardName=trim(outvarnames(i)), name=trim(outvarnames(i)), rc=rc)
            if (rc .ne. ESMF_SUCCESS) return ! bail out
        end do

    END SUBROUTINE

  !========================================
  !  Realize all input fields to import
  !========================================

    SUBROUTINE BMIAdapter_StateRealizeInputFields(importState,gridIn,rc)
        type(ESMF_State)     :: importState
        type(ESMF_Grid)         :: gridIn
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Field)        :: field
        character(item_name_length), pointer    :: invarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS


        ! importable field: add BMI input variable names
        call BMI_Get_input_var_names(bmodel,invarnames)

        do i=1,size(invarnames)
            field = ESMF_FieldCreate(name=trim(invarnames(i)), grid=gridIn, &
              typekind=ESMF_TYPEKIND_R8, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return

            call NUOPC_StateRealizeField(importState, field=field, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        end do

    END SUBROUTINE

    FUNCTION BMIAdapter_CreateField(grid,name,rc) result(field)
        type(ESMF_Field) :: field
        type(ESMF_Grid),intent(in) :: grid
        character(*),pointer,intent(in) :: name
        integer :: rc
        real,pointer:: bmi1d(:)
        real,pointer:: bmi2d(:,:)
        integer, dimension (2) :: gridshape

        call BMI_Get_grid_shape (bmodel, name, gridshape)

        rc = ESMF_SUCCESS

        call BMI_Get_Double(bmodel,name,bmi1d)
        bmi2d (1:gridshape(1), 1:gridshape(2)) => bmi1d
        !bmi2d = reshape(bmi1d,(/n,m/))

        field = ESMF_FieldCreate(grid, bmi2d, ESMF_INDEX_DELOCAL,name=name, rc=rc)
    END FUNCTION

  !========================================
  ! Realize all output fields to export
  !========================================
    SUBROUTINE BMIAdapter_StateRealizeOutputFields(exportState,gridOut,rc)
        type(ESMF_State)     :: exportState
        integer, intent(out) :: rc
        type(ESMF_Grid) :: gridOut

        ! local variables
        type(ESMF_Field)        :: field
        character(item_name_length), pointer    ::  outvarnames(:)
        integer                                     :: i
        integer :: vartype

        rc = ESMF_SUCCESS

        ! exportable field: add BMI output variable names
        call BMI_Get_output_var_names(bmodel,outvarnames)

        ! 26.6.9 ESMF_FieldCreate - Create a Field from Grid and Fortran array pointer

        do i=1,size(outvarnames)
            call BMI_Get_var_type(bmodel,outvarnames(i),vartype)
            field = BMIAdapter_CreateField(grid=gridOut,name=outvarnames(i),rc=rc)
            if (rc .ne. ESMF_SUCCESS) return

            call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        end do
    END SUBROUTINE

  !========================================
  ! Get variable rank / dimensions
  !========================================
    FUNCTION BMIAdapter_GetRank(var_name) result(rank)
        implicit none
        INTEGER :: rank
        character (len=item_name_length),pointer, INTENT(in) :: var_name

        call BMI_Get_var_rank (bmodel, var_name, rank)

    END FUNCTION BMIAdapter_GetRank

  !========================================
  ! Grid Creation Based on BMI
  !========================================

    FUNCTION BMIAdapter_GridCreate2D(varname,rc) result(return_grid)
        type(ESMF_Grid) :: return_grid
        integer, intent(out) :: rc
        character (*),pointer,intent(in) :: varname

        ! local variables
        integer :: x_min, x_max, y_min, y_max
        integer :: i_count, j_count
        real :: gridspacing(1:2)
        real :: gridorigin(1:2)
        integer :: gridshape(1:2)

        rc = ESMF_SUCCESS
        call BMI_Get_grid_origin (bmodel, varname, gridorigin)
        call BMI_Get_grid_spacing (bmodel, varname,gridspacing)
        call BMI_Get_grid_shape (bmodel,varname,gridshape)

        ! To be reviewed
        x_min = gridorigin(1)
        x_max = gridorigin(1)+gridshape(1)-1
        y_min = gridorigin(2)
        y_max = gridorigin(2)+gridshape(2)-1
        i_count = 1
        j_count = 1

        ! ESMF_GridCreateNoPeriDim - Create a Grid with no periodic dim and a regular distribution
        ! Temporarily no decomp
        return_grid = ESMF_GridCreateNoPeriDim(minIndex=(/x_min,y_min/), maxIndex=(/x_max,y_max/), &
          regDecomp=(/i_count,j_count/), name="bmi_uniform2d", rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_LogWrite("BMI grid created <bmi_uniform2d>", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

    END FUNCTION BMIAdapter_GridCreate2D

    FUNCTION BMIAdapter_SingleGridCreate(rc) result(return_grid)
        integer,intent(out) :: rc
        type(ESMF_Grid) :: return_grid

        ! local variables
        integer :: iterator1, iterator2
        character (item_name_length), pointer :: invarnames(:),outvarnames(:)
        integer :: gridtype, gridrank

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
        call BMI_Get_var_rank(bmodel,invarnames(1),gridrank)

        select case (gridtype)
            case(BMI_GRID_TYPE_UNIFORM)
                if(gridrank .eq. 2) then
                    return_grid = BMIAdapter_GridCreate2D(invarnames(1),rc)
                    if(rc .ne. ESMF_SUCCESS) return
                else
                    rc = ESMF_RC_NOT_IMPL
                    return
                end if
            case(BMI_GRID_TYPE_RECTILINEAR)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_STRUCTURED)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_UNSTRUCTURED)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_NUMBER)
                rc = ESMF_RC_NOT_IMPL
                return
            case(BMI_GRID_TYPE_UNKNOWN)
                rc = ESMF_RC_NOT_IMPL
                return
            case default
                rc = ESMF_RC_NOT_IMPL
                return
        end select

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
