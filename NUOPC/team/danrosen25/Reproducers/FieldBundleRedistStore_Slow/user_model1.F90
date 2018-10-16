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

#ifndef NFIELDS
#define NFIELDS 40
#endif

#ifndef CONTIGUOUS
#define CONTIGUOUS 0
#endif

#ifdef DEBUG
#define _DPRINT2_(a,b) print *,a,b
#define _DPRINT3_(a,b,c) print *,a,b,c
#define _DPRINT4_(a,b,c,d) print *,a,b,c,d
#define _DPRINT5_(a,b,c,d,e) print *,a,b,c,d,e
#else
#define _DPRINT2_(a,b) !print *,a,b
#define _DPRINT3_(a,b,c) !print *,a,b,c
#define _DPRINT4_(a,b,c,d) !print *,a,b,c,d
#define _DPRINT5_(a,b,c,d,e) !print *,a,b,c,d,e
#endif

module user_model1

  ! ESMF Framework module
  use ESMF

  implicit none
    
  public userm1_register

  ! global index size
  integer(ESMF_KIND_I4) :: gblcnt = NPOINTS

  ! custom user field
  type type_user_field
      integer(ESMF_KIND_I8), allocatable :: fdata(:,:,:)
      character(len=32)                  :: fname = ""
      integer                            :: favgs = 1
      integer                            :: flvls = 1
      integer                            :: fnumb = 0
  end type type_user_field

  type(type_user_field),dimension(NFIELDS) :: flist

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
    type(ESMF_Field),allocatable       :: field(:)
    type(ESMF_FieldBundle)             :: fieldbundle
    integer(ESMF_KIND_I4)              :: eqlcnt
    integer(ESMF_KIND_I4)              :: lclcnt
    integer(ESMF_KIND_I4)              :: rmdcnt
    integer(ESMF_KIND_I4)              :: lclbeg
    integer(ESMF_KIND_I4)              :: lclend
    integer(ESMF_KIND_I4)              :: i,j,k
    character(len=32)                  :: fstr
    character(len=3)                   :: padding

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
    write(fstr,"(I32)") size(flist)
    write(padding,"(I3)") LEN_TRIM(ADJUSTL(fstr))
    do i=1, size(flist)
        write(fstr,"(I0."//TRIM(padding)//")") i
        flist(i)%fname = 'field'//ADJUSTL(fstr)
        flist(i)%fnumb = i
        if ( MOD(i,3) .eq. 0 ) then
            flist(i)%flvls = 4
        else
            flist(i)%flvls = 1
        endif
        if ( MOD(i,7) .eq. 0 ) then
            flist(i)%favgs = 2
        else
            flist(i)%favgs = 1
        endif
        allocate(flist(i)%fdata(flist(i)%favgs,lclcnt,flist(i)%flvls))
    enddo
    T_EXIT("DATA_ALLOC")

    T_ENTER("DATA_INIT")
    if (lclcnt .gt. 0) then
        do i=1, size(flist)
        do j=1, ubound(flist(i)%fdata,1)
        do k=1, ubound(flist(i)%fdata,3)
            if (j .eq. 1) then
                flist(i)%fdata(j,:,k) = indexList(:)*flist(i)%fnumb*k
            else
                flist(i)%fdata(j,:,k) = flist(i)%fnumb*k
            endif
        enddo
        enddo
        enddo
    endif
    T_EXIT("DATA_INIT")

    T_ENTER("ESMF_OBJ_C")
    ! Add "aa" "ab" "ac" fields to the export state.
    distgrid = ESMF_DistGridCreate(arbSeqIndexList=indexList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    print *, "Model1 ", de_id, "DistGrid contains ", gblcnt, " global points."
    print *, "Model1 ", de_id, "DistGrid contains ", lclcnt, " local points."
    print *, "Model1 ", de_id, "indexList(min) = ", indexList(1)
    print *, "Model1 ", de_id, "indexList(max) = ", indexList(lclcnt)
    _DPRINT4_("Model1 ", de_id, "indexList = ", indexList)

    deallocate(indexList)

    locs = ESMF_LocStreamCreate(distgrid=distgrid, &
        indexflag=ESMF_INDEX_DELOCAL, coordSys=ESMF_COORDSYS_CART, &
        name="Test LocStream", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(field(size(flist)))
    do i=1, size(field)
        field(i) = ESMF_FieldCreate(locs, flist(i)%fdata, &
            indexflag=ESMF_INDEX_DELOCAL, gridToFieldMap=(/2/), &
            name=flist(i)%fname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    enddo

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

    deallocate(field)
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
    integer                :: fcount
    integer                :: i
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
    call ESMF_FieldBundleGet(fieldbundle, locstream=locs, &
        fieldCount=fcount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    do i = 1, fcount
        call ESMF_FieldBundleGet(fieldbundle, fieldIndex=i, field=field, rc=rc)
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
    do i=1, size(flist)
        deallocate(flist(i)%fdata)
    enddo
    T_EXIT("DATA_DEALLOC")

    _DPRINT2_(de_id, "User Comp1 Final returning")

  end subroutine user_final


end module user_model1
    
!\end{verbatim}
