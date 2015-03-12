module NuopcBmiAdapter

    use ESMF
    use NUOPC
    use BmiDefinitions

    implicit none

    private

    public BMIAdapter_SetProcedures, &
        BMIAdapter_Initialize, &
        BMIAdapter_Update, &
        BMIAdapter_Finalize, &
        BMIAdapter_StateRealizeOutputFields, &
        BMIAdapter_StateRealizeInputFields, &
        BMIAdapter_StateAdvertiseOutputFields, &
        BMIAdapter_StateAdvertiseInputFields, &
        BMIAdapter_PrintComponentInfo, &
        BMIAdapter_PrintAllVarInfo, &
        BMIAdapter_PrintCurrentTime, &
        BMIAdapter_SingleGridCreate, &
        BMIAdapter_ClockCreate, &
        BMIAdapter_AddAllFieldsToDictionary, &
        BMIAdapter_IsModelSet

    public bmiInitialize, &
        bmiUpdate, &
        bmiFinalize, &
        bmiGetStartTime, &
        bmiGetEndTime, &
        bmiGetCurrentTime, &
        bmiGetTimeStep, &
        bmiGetTimeUnits, &
        bmiGetVarType, &
        bmiGetVarUnits, &
        bmiGetVarRank, &
        bmiGetGridType, &
        bmiGetGridShape, &
        bmiGetGridSpacing, &
        bmiGetGridOrigin, &
        bmiGetDouble, &
        bmiGetDoubleAt, &
        bmiSetDouble, &
        bmiSetDoubleAt, &
        bmiGetInputVarNames, &
        bmiGetOutputVarNames, &
        bmiGetComponentName

    interface
        subroutine bmiInitialize(config_file)
            character(*), intent(in) :: config_file
        end subroutine
        subroutine bmiUpdate()
        end subroutine
        subroutine bmiFinalize()
        end subroutine
        subroutine bmiGetStartTime(start)
            real,intent(out) :: start
        end subroutine
        subroutine bmiGetEndTime(end)
            real, intent (out) :: end
        end subroutine
        subroutine bmiGetCurrentTime(time)
            real, intent (out) :: time
        end subroutine
        subroutine bmiGetTimeStep(dt)
            real, intent (out) :: dt
        end subroutine
        subroutine bmiGetTimeUnits(units)
            character (len=*), intent (out) :: units
        end subroutine
        subroutine bmiGetVarType(var_name, type)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: type
        end subroutine
        subroutine bmiGetVarUnits(var_name, units)
            character (len=*), intent (in) :: var_name
            character (len=*), intent (out) :: units
        end subroutine
        subroutine bmiGetVarRank(var_name, rank)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: rank
        end subroutine
        subroutine bmiGetGridType(var_name, type)
            character (len=*), intent (in) :: var_name
            integer, intent (out) :: type
        end subroutine
        subroutine bmiGetGridShape(var_name, shape)
            character (len=*), intent (in) :: var_name
            integer, dimension (:), intent (out) :: shape
        end subroutine
        subroutine bmiGetGridSpacing(var_name, spacing)
            character (len=*), intent (in) :: var_name
            real, dimension (:), intent (out) :: spacing
        end subroutine
        subroutine bmiGetGridOrigin(var_name, origin)
            character (len=*), intent (in) :: var_name
            real, dimension (:), intent (out) :: origin
        end subroutine
        subroutine bmiGetDouble(var_name, dest)
            character (len=*), intent (in) :: var_name
            real, pointer, intent (inout) :: dest(:)
        end subroutine
        subroutine bmiGetDoubleAt(var_name, dest, inds)
            character (len=*), intent (in) :: var_name
            real, pointer, intent (inout) :: dest(:)
            integer, intent (in) :: inds(:)
        end subroutine
        subroutine bmiSetDouble(var_name, src)
            character (len=*), intent (in) :: var_name
            real, intent (in) :: src (*)
        end subroutine
        subroutine bmiSetDoubleAt(var_name, inds, src)
            character (len=*), intent (in) :: var_name
            integer, intent (in) :: inds(:)
            real, intent (in) :: src (*)
        end subroutine
        subroutine bmiGetInputVarNames(names)
            character (*),pointer, intent (out) :: names(:)
        end subroutine
        subroutine bmiGetOutputVarNames(names)
            character (*),pointer, intent (out) :: names(:)
        end subroutine
        subroutine bmiGetComponentName(name)
            character (len=*),pointer, intent (out) :: name
        end subroutine
    end interface

    procedure(bmiInitialize), pointer :: pBmiInitialize => null()
    procedure(bmiUpdate), pointer :: pBmiUpdate => null()
    procedure(bmiFinalize), pointer :: pBmiFinalize => null()
    procedure(bmiGetStartTime), pointer :: pBmiGetStartTime => null()
    procedure(bmiGetEndTime), pointer :: pBmiGetEndTime => null()
    procedure(bmiGetCurrentTime), pointer :: pBmiGetCurrentTime => null()
    procedure(bmiGetTimeStep), pointer :: pBmiGetTimeStep => null()
    procedure(bmiGetTimeUnits), pointer :: pBmiGetTimeUnits => null()
    procedure(bmiGetVarType), pointer :: pBmiGetVarType => null()
    procedure(bmiGetVarUnits), pointer :: pBmiGetVarUnits => null()
    procedure(bmiGetVarRank), pointer :: pBmiGetVarRank => null()
    procedure(bmiGetGridType), pointer :: pBmiGetGridType => null()
    procedure(bmiGetGridShape), pointer :: pBmiGetGridShape => null()
    procedure(bmiGetGridSpacing), pointer :: pBmiGetGridSpacing => null()
    procedure(bmiGetGridOrigin), pointer :: pBmiGetGridOrigin => null()
    procedure(bmiGetDouble), pointer :: pBmiGetDouble => null()
    procedure(bmiGetDoubleAt), pointer :: pBmiGetDoubleAt => null()
    procedure(bmiSetDouble), pointer :: pBmiSetDouble => null()
    procedure(bmiSetDoubleAt), pointer :: pBmiSetDoubleAt => null()
    procedure(bmiGetInputVarNames), pointer :: pBmiGetInputVarNames => null()
    procedure(bmiGetOutputVarNames), pointer :: pBmiGetOutputVarNames => null()
    procedure(bmiGetComponentName), pointer :: pBmiGetComponentName => null()

!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------

    !########################################
    !# Section I:                           #
    !# BMI Adapter Setup Procedures         #
    !########################################

    subroutine BMIAdapter_SetProcedures(initialize,finalize,update, getStartTime, getEndTime, getCurrentTime, getTimeStep, getTimeUnits, getVarType, getVarUnits, getVarRank, getGridType, getGridShape, getGridSpacing, getGridOrigin, getDouble, getDoubleAt, setDouble, setDoubleAt, getInputVarNames, getOutputVarNames, getComponentName)
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

        pBmiInitialize => initialize
        pBmiUpdate => update
        pBmiFinalize => finalize
        pBmiGetTimeStep => getTimeStep
        pBmiGetStartTime => getStartTime
        pBmiGetEndTime => getEndTime
        pBmiGetCurrentTime => getCurrentTime
        pBmiGetTimeUnits => getTimeUnits
        pBmiGetVarType => getVarType
        pBmiGetVarUnits => getVarUnits
        pBmiGetVarRank => getVarRank
        pBmiGetGridType => getGridType
        pBmiGetGridShape => getGridShape
        pBmiGetGridSpacing => getGridSpacing
        pBmiGetGridOrigin => getGridOrigin
        pBmiGetDouble => getDouble
        pBmiGetDoubleAt => getDoubleAt
        pBmiSetDouble => setDouble
        pBmiSetDoubleAt => setDoubleAt
        pBmiGetInputVarNames => getInputVarNames
        pBmiGetOutputVarNames => getOutputVarNames
        pBmiGetComponentName => getComponentName

    end subroutine


    !#########################################
    !# Section II:                           #
    !# Model control flow adapter procedures #
    !#########################################

    !========================================
    ! Initialize BMI model
    !========================================

    SUBROUTINE BMIAdapter_Initialize(file,rc)
        character(*),intent(in) :: file
        integer,intent(out) :: rc

        character(BMI_MAXCOMPNAMESTR), pointer :: compname

        rc = ESMF_SUCCESS

        call pBmiInitialize(file)
        call pBmiGetComponentName(compname)
        call ESMF_LogWrite("BMI initialized <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

    END SUBROUTINE

    !========================================
    ! Update BMI model
    !========================================

    SUBROUTINE BMIAdapter_Update(rc)
        integer, intent(out) :: rc
        character(BMI_MAXCOMPNAMESTR),pointer :: compname

        rc = ESMF_SUCCESS

        call pBmiUpdate() ! Update BMI
        ! call BMIAdapter_PrintCurrentTime() ! Print BMI model time after update
        call pBmiGetComponentName(compname)
        call ESMF_LogWrite("BMI updated <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return
    END SUBROUTINE

    !========================================
    ! Finalize BMI model
    !========================================

    SUBROUTINE BMIAdapter_Finalize(rc)
        integer, intent(out) :: rc
        character(BMI_MAXCOMPNAMESTR),pointer :: compname

        rc = ESMF_SUCCESS

        call pBmiFinalize() ! Finalize BMI
        call pBmiGetComponentName(compname)
        call ESMF_LogWrite("BMI finalized <" // trim(compname) //">", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

    END SUBROUTINE

    !############################
    !# Section III:             #
    !# Clock Adapter Procedures #
    !############################

    !===========================
    ! Create Clock for BMI Model
    !===========================

    function BMIAdapter_ClockCreate(refClock, rc) result(clock)
        ! reference the driver clock
        type(ESMF_Clock),intent(in)  :: refClock
        ! instantiate a clock
        type(ESMF_Clock) :: clock

        ! instantiate time_step, start and stop times
        type(ESMF_TimeInterval) :: timeStep,startDelay,endReference
        type(ESMF_Time) :: refStartTime, startTime, stopTime

        ! local variables for Get methods
        real :: step,start,end
        character(len=10) :: units

        ! return code
        integer :: rc

        rc = ESMF_SUCCESS

        call pBmiGetTimeStep(step)
        call pBmiGetStartTime(start)
        call pBmiGetEndTime(end)
        call pBmiGetTimeUnits(units)

          ! Put warning in for truncation
        if (units .eq. 's') then
            call ESMF_TimeIntervalSet(timeStep, s = INT(step), rc=rc)
            call ESMF_TimeIntervalSet(startDelay, s = INT(start),rc=rc)
            call ESMF_TimeIntervalSet(endReference, s = INT(end),rc=rc)
        else if(units .eq. 'm') then
            call ESMF_TimeIntervalSet(timeStep, m = INT(step), rc=rc)
            call ESMF_TimeIntervalSet(startDelay, m = INT(start),rc=rc)
            call ESMF_TimeIntervalSet(endReference, m = INT(end),rc=rc)
        else
            rc = ESMF_RC_ARG_VALUE
        end if

        call ESMF_ClockGet(refClock,startTime=refStartTime,rc=rc)

        startTime = refStartTime + startDelay
        stopTime = refStartTime + endReference

        ! initialize the clock with the above values
        clock = ESMF_ClockCreate(timeStep, startTime=startTime, stopTime=stopTime, &
            name="bmiclock1", rc=rc)

    end function

    !####################
    !# Section IV:      #
    !# Field Procedures #
    !####################

    !========================================
    ! Add individual field to NUOPC Dictionary
    !========================================

    subroutine BMIAdapter_AddFieldToDictionary(var_name, rc)
        implicit none
        character (len=BMI_MAXVARNAMESTR), intent (in) :: var_name
        character(len=10)   :: units, dict_units ! Assumed length for units string
        integer                                     :: rc
        logical                                     :: in_dictionary

        rc = ESMF_SUCCESS

        call pBmiGetVarUnits ( var_name, units)

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
        character(len=BMI_MAXVARNAMESTR),pointer    :: invarnames(:), outvarnames(:)
        integer                                     :: i
        integer, intent(out)                        :: rc

        rc = ESMF_SUCCESS

        call pBmiGetInputVarNames(invarnames)
        call pBmiGetOutputVarNames(outvarnames)

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
        character(BMI_MAXVARNAMESTR),pointer    :: invarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        call pBmiGetInputVarNames(invarnames)

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
        character(BMI_MAXVARNAMESTR),pointer    ::  outvarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        ! exportable field(s): Get Output Var Names from BMI
        call pBmiGetOutputVarNames(outvarnames)

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
        character(BMI_MAXVARNAMESTR),pointer    :: invarnames(:)
        integer                                     :: i

        rc = ESMF_SUCCESS

        ! importable field: add BMI input variable names
        call pBmiGetInputVarNames(invarnames)

        do i=1,size(invarnames)
            field = ESMF_FieldCreate(name=trim(invarnames(i)), grid=gridIn, &
                typekind=ESMF_TYPEKIND_R8, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return

            call NUOPC_StateRealizeField(importState, field=field, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        end do

    END SUBROUTINE

    FUNCTION BMIAdapter_FieldCreate(grid,name,rc) result(field)
        type(ESMF_Field) :: field
        type(ESMF_Grid),intent(in) :: grid
        character(*),intent(in) :: name
        integer :: rc
        real,pointer:: bmi1d(:)
        real,pointer:: bmi2d(:,:)
        real,pointer:: arrayportion(:,:)
        integer, dimension (2) :: gridshape
        integer, dimension(2) :: gec

        rc = ESMF_SUCCESS

        call pBmiGetGridShape ( name, gridshape)

        call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
            exclusiveCount=gec, rc=rc)
        if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

        call pBmiGetDouble(name,bmi1d)

        bmi2d (1:gridshape(1), 1:gridshape(2)) => bmi1d
        arrayportion => bmi2d(1:gec(1),1:gec(2))
        !bmi2d = reshape(bmi1d,(/gec(1),gec(2)/))

        field = ESMF_FieldCreate(grid, farrayptr=arrayportion,name=name, rc=rc)
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
        character(BMI_MAXVARNAMESTR),pointer    ::  outvarnames(:)
        integer                                     :: i
        integer :: vartype

        rc = ESMF_SUCCESS

        ! exportable field: add BMI output variable names
        call pBmiGetOutputVarNames(outvarnames)

        ! 26.6.9 ESMF_FieldCreate - Create a Field from Grid and Fortran array pointer

        do i=1,size(outvarnames)
            call pBmiGetVarType(outvarnames(i),vartype)
            field = BMIAdapter_FieldCreate(grid=gridOut,name=outvarnames(i),rc=rc)
            if (rc .ne. ESMF_SUCCESS) return

            call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
            if (rc .ne. ESMF_SUCCESS) return
        end do
    END SUBROUTINE

    !========================================
    ! Get field rank / dimensions
    !========================================
    FUNCTION BMIAdapter_GetRank(var_name) result(rank)
        implicit none
        INTEGER :: rank
        character (len=BMI_MAXVARNAMESTR), INTENT(in) :: var_name

        call pBmiGetVarRank ( var_name, rank)

    END FUNCTION BMIAdapter_GetRank

    !###########################
    !# Section V:              #
    !# Grid Adapter Procedures #
    !###########################

    !========================================
    ! Grid Creation Based on BMI
    !========================================

    FUNCTION BMIAdapter_GridCreate2D(varname,rc) result(return_grid)
        type(ESMF_Grid) :: return_grid
        integer, intent(out) :: rc
        character (*),intent(in) :: varname

        ! local variables
        real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max
        !integer :: x_min, x_max, y_min, y_max
        integer :: i_count, j_count
        real :: gridspacing(1:2)
        real :: gridorigin(1:2)
        integer :: gridshape(1:2)

        rc = ESMF_SUCCESS

        call pBmiGetGridOrigin (varname, gridorigin)
        call pBmiGetGridSpacing (varname,gridspacing)
        call pBmiGetGridShape (varname,gridshape)

        ! To be reviewed
        x_min = gridorigin(1)
        x_max = gridorigin(1)+gridshape(1)-1
        y_min = gridorigin(2)
        y_max = gridorigin(2)+gridshape(2)-1
        i_count = gridshape(1)
        j_count = gridshape(2)

        ! ESMF_GridCreateNoPeriDim - Create a Grid with no periodic dim and a regular distribution
        ! Temporarily no decomp
        !return_grid = ESMF_GridCreateNoPeriDim(minIndex=(/x_min,y_min/), maxIndex=(/x_max,y_max/), &
        !    regDecomp=(/i_count,j_count/), name="bmi_uniform2d", rc=rc)
        return_grid = NUOPC_GridCreateSimpleXY(x_min, y_min, x_max, y_max, &
            i_count, j_count, rc)
        if (rc .ne. ESMF_SUCCESS) return
        call ESMF_LogWrite("BMI grid created <bmi_uniform2d>", ESMF_LOGMSG_INFO, rc=rc)
        if (rc .ne. ESMF_SUCCESS) return

    END FUNCTION BMIAdapter_GridCreate2D

    FUNCTION BMIAdapter_SingleGridCreate(rc) result(return_grid)
        integer,intent(out) :: rc
        type(ESMF_Grid) :: return_grid

        ! local variables
        integer :: iterator1, iterator2
        character (BMI_MAXVARNAMESTR),pointer :: invarnames(:),outvarnames(:)
        integer :: gridtype, gridrank

        rc = ESMF_SUCCESS

        call pBmiGetInputVarNames ( invarnames)
        call pBmiGetOutputVarNames ( outvarnames)

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
        call pBmiGetGridType(invarnames(1),gridtype)
        call pBmiGetVarRank(invarnames(1),gridrank)

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
        character (BMI_MAXVARNAMESTR), intent(in) :: var_name_1,var_name_2
        real :: spacing_1(1:2), spacing_2(1:2)
        real :: origin_1(1:2), origin_2(1:2)
        integer :: shape_1(1:2), shape_2(1:2)
        integer :: gridtype_1, gridtype_2
        logical :: equivalent

        equivalent = .true.

        call pBmiGetGridOrigin (var_name_1,origin_1)
        call pBmiGetGridOrigin (var_name_2, origin_2)

        if(origin_1(1) .ne. origin_2 (1) .or. origin_1(2) .ne. origin_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridSpacing (var_name_1,spacing_1)
        call pBmiGetGridSpacing (var_name_2, spacing_2)

        if(spacing_1(1) .ne. spacing_2 (1) .or. spacing_1(2) .ne. spacing_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridShape (var_name_1,shape_1)
        call pBmiGetGridShape (var_name_2,shape_2)

        if(shape_1(1) .ne. shape_2 (1) .or. shape_1(2) .ne. shape_2(2)) then
            equivalent = .false.
            return
        end if

        call pBmiGetGridType(var_name_2,gridtype_1)
        call pBmiGetGridType(var_name_1,gridtype_2)

        if(gridtype_1 .ne. gridtype_2) then
            equivalent = .false.
            return
        end if

    End Function BMIAdapter_GridComparison

    !##########################
    !# Section VI:            #
    !# OS Interface Procedures#
    !##########################

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
            config_file = ""
            rc = ESMF_RC_ARG_VALUE
        end if
    END SUBROUTINE

    !###########################
    !# Section VII:            #
    !# Standard Out Procedures #
    !###########################

    !=========================
    !=Print Model Information=
    !=========================

    subroutine BMIAdapter_PrintComponentInfo()
        implicit none
        character(len=BMI_MAXVARNAMESTR), pointer        :: invarnames(:)
        character(len=BMI_MAXVARNAMESTR), pointer     :: outvarnames(:)
        character(len=BMI_MAXCOMPNAMESTR),pointer    :: compname
        character(len=10)                               ::tunits
        real                            :: start
        real                            :: end
        real                            :: step
        integer :: i
        integer :: rc

        rc = ESMF_SUCCESS

        call pBmiGetInputVarNames(invarnames)
        call pBmiGetOutputVarNames(outvarnames)
        call pBmiGetComponentName(compname)
        call pBmiGetTimeStep(step)
        call pBmiGetStartTime(start)
        call pBmiGetEndTime(end)
        call pBmiGetTimeUnits(tunits)

        print *, "BMI Component Info"
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
        character (len=BMI_MAXVARNAMESTR), intent (in) :: var_name
        integer, intent(in) :: var_rank
        integer             :: type
        character(len=10)   :: units, dict_units ! Assumed length for units string
        integer             :: gtype
        integer     :: gshape(1:var_rank)   ! Assumed shape for grid shape array
        real        :: gspacing(1:var_rank), gorigin(1:var_rank) ! Assumed shape for grid spacing array
        logical     :: in_dictionary
        integer     :: rc

        call pBmiGetVarType ( var_name, type)
        call pBmiGetVarUnits ( var_name, units)
        call pBmiGetGridType ( var_name, gtype)
        call pBmiGetGridShape ( var_name, gshape)
        call pBmiGetGridSpacing ( var_name, gspacing)
        call pBmiGetGridOrigin ( var_name, gorigin)

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

    SUBROUTINE BMIAdapter_PrintCurrentTime(rc)
        real                          :: bmi_time
        integer,intent(out) :: rc

        rc = ESMF_SUCCESS

        call pBmiGetCurrentTime(bmi_time)
        print *,"BMI Current Time: ", bmi_time

    END SUBROUTINE

    !========================================
    ! Iterate over all input and output
    ! variables and print info.
    !========================================

    subroutine BMIAdapter_PrintAllVarInfo()
        implicit none
        character(len=BMI_MAXVARNAMESTR), pointer    :: invarnames(:), outvarnames(:)
        integer                                     :: i

        call pBmiGetInputVarNames(invarnames)
        call pBmiGetOutputVarNames(outvarnames)

        do i=1,SIZE(invarnames)
            print *,"In Variable Info: ",invarnames(i)
            call BMIAdapter_PrintVarInfo(invarnames(i),BMIAdapter_GetRank(invarnames(i)))
        end do

        do i=1,SIZE(outvarnames)
            print *,"Out Variable Info: ",outvarnames(i)
            call BMIAdapter_PrintVarInfo(outvarnames(i),BMIAdapter_GetRank(invarnames(i)))
        end do

    end subroutine

    !###############################
    !# Section VIII:               #
    !# Basic Model Interface Tests #
    !###############################

    logical function isModelSet()
        isModelSet = .false.

        if(associated(pBmiInitialize) .and. &
            associated(pBmiUpdate) .and. &
            associated(pBmiFinalize) .and. &
            associated(pBmiGetStartTime) .and. &
            associated(pBmiGetEndTime) .and. &
            associated(pBmiGetCurrentTime) .and. &
            associated(pBmiGetTimeStep) .and. &
            associated(pBmiGetTimeUnits) .and. &
            associated(pBmiGetVarType) .and. &
            associated(pBmiGetVarUnits) .and. &
            associated(pBmiGetVarRank) .and. &
            associated(pBmiGetGridType) .and. &
            associated(pBmiGetGridShape) .and. &
            associated(pBmiGetGridSpacing) .and. &
            associated(pBmiGetGridOrigin) .and. &
            associated(pBmiGetDouble) .and. &
            associated(pBmiGetDoubleAt) .and. &
            associated(pBmiSetDouble) .and. &
            associated(pBmiSetDoubleAt) .and. &
            associated(pBmiGetInputVarNames) .and. &
            associated(pBmiGetOutputVarNames) .and. &
            associated(pBmiGetComponentName)) then

            isModelSet = .true.

        endif

    end function

    subroutine BMIAdapter_isModelSet(rc)
        integer, intent(out) :: rc

        if (isModelSet()) then
            rc = ESMF_SUCCESS
        else
            rc = ESMF_RC_OBJ_INIT
        end if

    end subroutine

end module NuopcBmiAdapter
