! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component, most recent interface revision.
!
!
!\begin{verbatim}

#ifdef ESMF_TRACE
#define T_ENTER(region) call ESMF_TraceRegionEnter(region)
#define T_EXIT(region) call ESMF_TraceRegionExit(region)
#else
#define T_ENTER(region) 
#define T_EXIT(region)
#endif

#ifndef NPOINTS
#define NPOINTS 5000
#endif

#ifndef CONTIGUOUS
#define CONTIGUOUS 0
#endif

#ifdef DEBUG
#define _DPRINT2_(x,y) print *,x,y
#define _DPRINT3_(x,y,z) print *,x,y,z
#else
#define _DPRINT2_(x,y) !print *,x,y
#define _DPRINT3_(x,y,z) !print *,x,y,z
#endif

module user_model1

  ! ESMF Framework module
  use ESMF

  implicit none
    
  public userm1_register

  ! variable data arrays
  integer(ESMF_KIND_I8), allocatable, save :: tmp_data(:,:,:)
  integer(ESMF_KIND_I8), allocatable, save :: hum_data(:,:,:)
  integer(ESMF_KIND_I8), allocatable, save :: prs_data(:,:,:)

  ! global index size
  integer(ESMF_KIND_I4) :: gblcnt = NPOINTS

  contains

  subroutine userm1_register(comp, rc)
    type(ESMF_GridComp) :: comp
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)          :: vm
    integer                :: de_id

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Query component for VM and PET id
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMGet(vm, localPet=de_id, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    _DPRINT2_(de_id, "User Comp1 Register starting")

    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, userRoutine=user_init, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, userRoutine=user_run, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, userRoutine=user_final, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    _DPRINT2_(de_id, "Registered Initialize, Run, and Finalize routines")
    _DPRINT2_(de_id, "User Comp1 Register returning")
    
  end subroutine

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.
 
    
  subroutine user_init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)                      :: vm
    integer                            :: de_id, npets
    integer(ESMF_KIND_I4), allocatable :: indexList(:)
    type(ESMF_DistGrid)                :: distgrid
    type(ESMF_LocStream)               :: locs
    type(ESMF_Field)                   :: field(3)
    type(ESMF_FieldBundle)             :: fieldbundle
    integer(ESMF_KIND_I4)              :: eqlcnt
    integer(ESMF_KIND_I4)              :: lclcnt
    integer(ESMF_KIND_I4)              :: rmdcnt
    integer(ESMF_KIND_I4)              :: lclbeg
    integer(ESMF_KIND_I4)              :: lclend
    integer(ESMF_KIND_I4)              :: i

    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Query component for VM and PET id and PET count
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMGet(vm, localPet=de_id, petCount=npets, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    _DPRINT2_(de_id, "User Comp 1 Init starting")

    ! Check for correct number of PETs
    if ( npets .gt. gblcnt ) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
            msg="Too many PETs are allocated to user_model1",&
            line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
    endif

    T_ENTER("DATA_ALLOC")
    eqlcnt = gblcnt / npets
    rmdcnt = MOD(gblcnt, npets)
#if CONTIGUOUS == 1
    if ( de_id .eq. (npets-1) ) then
        lclcnt = eqlcnt + rmdcnt
    else
        lclcnt = eqlcnt
    endif
    lclbeg = (de_id * eqlcnt) + 1
    lclend = lclbeg + lclcnt - 1
    allocate(indexList(lclcnt))
    indexList = (/(i, i=lclbeg, lclend)/)
#else
    if ( de_id .lt. rmdcnt ) then
        lclcnt = eqlcnt + 1
    else
        lclcnt = eqlcnt
    endif
    lclbeg = de_id + 1
    lclend = lclbeg + (lclcnt-1) * npets
    allocate(indexList(lclcnt))
    indexList = (/(i, i=lclbeg, lclend, npets)/)
#endif
    allocate(tmp_data(2,lclcnt,4))
    allocate(hum_data(1,lclcnt,4))
    allocate(prs_data(2,lclcnt,4))
    T_EXIT("DATA_ALLOC")

    _DPRINT3_(de_id, "indexList = ", indexList)
    print *,"model1 ",de_id,"indexList(min) = ",indexList(1)
    print *,"model1 ",de_id,"indexList(max) = ",indexList(lclcnt)

    T_ENTER("DATA_INIT")
    if (lclcnt .gt. 0) then
        tmp_data(1,:,1) = indexList(:)*1*1
        tmp_data(1,:,2) = indexList(:)*1*2
        tmp_data(1,:,3) = indexList(:)*1*3
        tmp_data(1,:,4) = indexList(:)*1*4
        hum_data(1,:,1) = indexList(:)*10*1
        hum_data(1,:,2) = indexList(:)*10*2
        hum_data(1,:,3) = indexList(:)*10*3
        hum_data(1,:,4) = indexList(:)*10*4
        prs_data(1,:,1) = indexList(:)*100*1
        prs_data(1,:,2) = indexList(:)*100*2
        prs_data(1,:,3) = indexList(:)*100*3
        prs_data(1,:,4) = indexList(:)*100*4
        tmp_data(2,:,1) = 1*1
        tmp_data(2,:,2) = 1*2
        tmp_data(2,:,3) = 1*3
        tmp_data(2,:,4) = 1*4
        prs_data(2,:,1) = 100*1
        prs_data(2,:,2) = 100*2
        prs_data(2,:,3) = 100*3
        prs_data(2,:,4) = 100*4
    endif
    T_EXIT("DATA_INIT")

    T_ENTER("ESMF_OBJ_C")
    ! Add "temperature" "humidity" "pressure" fields to the export state.
    distgrid = ESMF_DistGridCreate(arbSeqIndexList=indexList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    deallocate(indexList)

    locs = ESMF_LocStreamCreate(distgrid=distgrid, &
        indexflag=ESMF_INDEX_DELOCAL, coordSys=ESMF_COORDSYS_CART, &
        name="Test LocStream", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field(1) = ESMF_FieldCreate(locs, tmp_data, &
        indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=(/2/), &
        name="temperature", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field(2) = ESMF_FieldCreate(locs, hum_data, &
        indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=(/2/), &
        name="humidity", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field(3) = ESMF_FieldCreate(locs, prs_data, &
        indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=(/2/), &
        name="pressure", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    fieldbundle = ESMF_FieldBundleCreate(fieldList=field, &
        name="fieldbundle data", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_StateAdd(exportState, fieldbundleList=(/fieldbundle/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    T_EXIT("ESMF_OBJ_C")

    _DPRINT2_(de_id, "User Comp1 Init returning")

  end subroutine user_init


!-------------------------------------------------------------------------
!   !  The Run routine where data is computed.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)          :: vm
    integer                :: de_id
   
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Query component for VM and PET id
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMGet(vm, localPet=de_id, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    _DPRINT2_(de_id, "User Comp1 Run starting")
    T_ENTER("RUN_NULL")

    T_EXIT("RUN_NULL")
    _DPRINT2_(de_id, "User Comp1 Run returning")

  end subroutine user_run


!-------------------------------------------------------------------------
!   !  The Finalization routine where things are deleted and cleaned up.
!   !
 
  subroutine user_final(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)          :: vm
    integer                :: de_id
    type(ESMF_Field)       :: field
    type(ESMF_FieldBundle) :: fieldbundle
    integer                :: k
    type(ESMF_LocStream)   :: locs
    
    ! Initialize return code
    rc = ESMF_SUCCESS

    ! Query component for VM and PET id
    call ESMF_GridCompGet(comp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_VMGet(vm, localPet=de_id, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    _DPRINT2_(de_id, "User Comp1 Final starting")

    T_ENTER("ESMF_OBJ_D")
    call ESMF_StateGet(exportState, "fieldbundle data", fieldbundle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldBundleGet(fieldbundle, locstream=locs, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do k = 1, 3
        call ESMF_FieldBundleGet(fieldbundle, fieldIndex=k, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_FieldDestroy(field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
    enddo
    call ESMF_FieldBundleDestroy(fieldbundle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LocStreamDestroy(locs, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateRemove(exportState, &
      itemNameList=(/"fieldbundle data"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    T_EXIT("ESMF_OBJ_D")

    T_ENTER("DATA_DEALLOC")
    deallocate(tmp_data)
    deallocate(hum_data)
    deallocate(prs_data)
    T_EXIT("DATA_DEALLOC")

    _DPRINT2_(de_id, "User Comp1 Final returning")

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
