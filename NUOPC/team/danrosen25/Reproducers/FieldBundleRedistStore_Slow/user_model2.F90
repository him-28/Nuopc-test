! $Id$
!
! Example/test code which shows User Component calls.

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

!
! !DESCRIPTION:
!  User-supplied Component
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
#define _DPRINT5_(v,w,x,y,z) print *,v,w,x,y,z
#else
#define _DPRINT2_(x,y) !print *,x,y
#define _DPRINT3_(x,y,z) !print *,x,y,z
#define _DPRINT5_(v,w,x,y,z) !print *,v,w,x,y,z
#endif

module user_model2

  ! ESMF Framework module
  use ESMF

  implicit none
    
  public userm2_register

  ! variable data arrays
  integer(ESMF_KIND_I8), allocatable, save :: tmp_data(:,:,:)
  integer(ESMF_KIND_I8), allocatable, save :: hum_data(:,:,:)
  integer(ESMF_KIND_I8), allocatable, save :: prs_data(:,:,:)

  ! global index size
  integer(ESMF_KIND_I4) :: gblcnt = NPOINTS

  contains

  subroutine userm2_register(comp, rc)
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

    _DPRINT2_(de_id, "User Comp2 Register starting")

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
    _DPRINT2_(de_id, "User Comp2 Register returning")
    
  end subroutine

!--------------------------------------------------------------------------------
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

    ! Query component for VM and create a layout with the right breakdown
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

    _DPRINT2_(de_id, "User Comp 2 Init starting")

    ! Check for correct number of PETs
    if ( npets .gt. gblcnt ) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
            msg="Too many PETs are allocated to user_model2",&
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
    print *,"model2 ",de_id,"indexList(min) = ",indexList(1)
    print *,"model2 ",de_id,"indexList(max) = ",indexList(lclcnt)

    T_ENTER("DATA_INIT")
    if (lclcnt .gt. 0) then
        tmp_data(:,:,:) = 0
        hum_data(:,:,:) = 0
        prs_data(:,:,:) = 0
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

    call ESMF_StateAdd(importState, fieldbundleList=(/fieldbundle/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    T_EXIT("ESMF_OBJ_C")

    _DPRINT2_(de_id, "User Comp2 Init returning")

  end subroutine user_init


!--------------------------------------------------------------------------------
!   !  The Run routine where data is validated.
!   !
 
  subroutine user_run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: comp
    type(ESMF_State) :: importState, exportState
    type(ESMF_Clock) :: clock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_VM)                      :: vm
    integer                            :: de_id
    type(ESMF_FieldBundle)             :: fieldbundle
    type(ESMF_LocStream)               :: locs
    type(ESMF_DistGrid)                :: distgrid
    logical                            :: arbIndex
    integer(ESMF_KIND_I4)              :: elementCount
    integer(ESMF_KIND_I4), allocatable :: indexList(:)
    type(ESMF_Field)                   :: field
    integer(ESMF_KIND_I8), pointer     :: fptr(:,:,:)
    integer(ESMF_KIND_I4)              :: i
    integer(ESMF_KIND_I4)              :: exlb(3), exub(3)
    
    ! Initialize return code
    rc = ESMF_SUCCESS

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

    _DPRINT2_(de_id, "User Comp2 Run starting")

    T_ENTER("RUN_CHECK")
    ! Get the destination Field from the import State
    call ESMF_StateGet(importState, itemName="fieldbundle data", fieldbundle=fieldbundle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
   
    ! Get the LocStream from the FieldBundle
    call ESMF_FieldBundleGet(fieldbundle, locstream=locs, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LocStreamGet(locs, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_DistGridGet(distgrid, localDe=0, arbSeqIndexFlag=arbIndex, &
          elementCount=elementCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(indexList(elementCount))

    call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=indexList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    nullify(fptr)
    call ESMF_FieldBundleGet(fieldbundle, fieldName="temperature", &
          field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exlb, exclusiveUBound=exub, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do i = exlb(2), exub(2)
          if(fptr(1,i,1) .ne. indexList(i)*1*1) then
            _DPRINT5_(de_id, "ERROR temp ", indexList(i), " val(1,i,1) =", fptr(1,i,1))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,2) .ne. indexList(i)*1*2) then
            _DPRINT5_(de_id, "ERROR temp ", indexList(i), " val(1,i,2) =", fptr(1,i,2))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,3) .ne. indexList(i)*1*3) then
            _DPRINT5_(de_id, "ERROR temp ", indexList(i), " val(1,i,3) =", fptr(1,i,3))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,4) .ne. indexList(i)*1*4) then
            _DPRINT5_(de_id, "ERROR temp ", indexList(i), " val(1,i,4) =", fptr(1,i,4))
            rc = ESMF_FAILURE
          endif
          if(fptr(2,i,1) .ne. 1*1) then
            _DPRINT5_(de_id, "ERROR temp ", indexList(i), " val(2,i,1) =", fptr(2,i,1))
            rc = ESMF_FAILURE
          endif
          if(fptr(2,i,2) .ne. 1*2) then
            _DPRINT5_(de_id, "ERROR temp ", indexList(i), " val(2,i,2) =", fptr(2,i,2))
            rc = ESMF_FAILURE
          endif
          if(fptr(2,i,3) .ne. 1*3) then
            _DPRINT5_(de_id, "ERROR temp ", indexList(i), " val(2,i,3) =", fptr(2,i,3))
            rc = ESMF_FAILURE
          endif
          if(fptr(2,i,4) .ne. 1*4) then
            _DPRINT5_(de_id, "ERROR temp ", indexList(i), " val(2,i,4) =", fptr(2,i,4))
            rc = ESMF_FAILURE
          endif
    enddo

    nullify(fptr)
    call ESMF_FieldBundleGet(fieldbundle, fieldName="humidity", &
          field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exlb, exclusiveUBound=exub, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do i = exlb(2), exub(2)
          if(fptr(1,i,1) .ne. indexList(i)*10*1) then
            _DPRINT5_(de_id, "ERROR humd ", indexList(i), " val(1,i,1) =", fptr(1,i,1))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,2) .ne. indexList(i)*10*2) then
            _DPRINT5_(de_id, "ERROR humd ", indexList(i), " val(1,i,2) =", fptr(1,i,2))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,3) .ne. indexList(i)*10*3) then
            _DPRINT5_(de_id, "ERROR humd ", indexList(i), " val(1,i,3) =", fptr(1,i,3))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,4) .ne. indexList(i)*10*4) then
            _DPRINT5_(de_id, "ERROR humd ", indexList(i), " val(1,i,4) =", fptr(1,i,4))
            rc = ESMF_FAILURE
          endif
    enddo

    nullify(fptr)
    call ESMF_FieldBundleGet(fieldbundle, fieldName="pressure", &
          field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, localDe=0, farrayPtr=fptr, &
          exclusiveLBound=exlb, exclusiveUBound=exub, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do i = exlb(2), exub(2)
          if(fptr(1,i,1) .ne. indexList(i)*100*1) then
            _DPRINT5_(de_id, "ERROR pres ", indexList(i), " val(1,i,1) =", fptr(1,i,1))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,2) .ne. indexList(i)*100*2) then
            _DPRINT5_(de_id, "ERROR pres ", indexList(i), " val(1,i,2) =", fptr(1,i,2))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,3) .ne. indexList(i)*100*3) then
            _DPRINT5_(de_id, "ERROR pres ", indexList(i), " val(1,i,3) =", fptr(1,i,3))
            rc = ESMF_FAILURE
          endif
          if(fptr(1,i,4) .ne. indexList(i)*100*4) then
            _DPRINT5_(de_id, "ERROR pres ", indexList(i), " val(1,i,4) =", fptr(1,i,4))
            rc = ESMF_FAILURE
          endif
          if(fptr(2,i,1) .ne. 100*1) then
            _DPRINT5_(de_id, "ERROR pres ", indexList(i), " val(2,i,1) =", fptr(2,i,1))
            rc = ESMF_FAILURE
          endif
          if(fptr(2,i,2) .ne. 100*2) then
            _DPRINT5_(de_id, "ERROR pres ", indexList(i), " val(2,i,2) =", fptr(2,i,2))
            rc = ESMF_FAILURE
          endif
          if(fptr(2,i,3) .ne. 100*3) then
            _DPRINT5_(de_id, "ERROR pres ", indexList(i), " val(2,i,3) =", fptr(2,i,3))
            rc = ESMF_FAILURE
          endif
          if(fptr(2,i,4) .ne. 100*4) then
            _DPRINT5_(de_id, "ERROR pres ", indexList(i), " val(2,i,4) =", fptr(2,i,4))
            rc = ESMF_FAILURE
          endif
    enddo

    deallocate(indexList)
    T_EXIT("RUN_CHECK")

    _DPRINT2_(de_id, "User Comp2 Run returning")

  end subroutine user_run


!--------------------------------------------------------------------------------
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
    type(ESMF_LocStream)   :: locs
    integer                :: k
    
    ! Initialize return code
    rc = ESMF_SUCCESS

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
    _DPRINT2_(de_id, "User Comp2 Final starting")

    T_ENTER("ESMF_OBJ_D")
    call ESMF_StateGet(importState, "fieldbundle data", fieldbundle, rc=rc)
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
    call ESMF_StateRemove(importState, &
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

    _DPRINT2_(de_id, "User Comp2 Final returning")

  end subroutine user_final

end module user_model2
!\end{verbatim}
