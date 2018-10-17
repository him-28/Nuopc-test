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
#define CONTIGUOUS 1
#endif

#ifndef NGAPS
#define NGAPS 50000
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

  ! number of gaps
  integer(ESMF_KIND_I4) :: gaps = NGAPS

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
    integer                            :: de_id,npets
    integer(ESMF_KIND_I4)              :: lclcnt,lclcntng
    integer(ESMF_KIND_I4), allocatable :: indexList(:)
    integer(ESMF_KIND_I4), allocatable :: indexListNoGaps(:)
    type(ESMF_DistGrid)                :: distgrid
    type(ESMF_LocStream)               :: locs
    type(ESMF_Field),allocatable       :: field(:)
    type(ESMF_FieldBundle)             :: fieldbundle
    integer(ESMF_KIND_I4)              :: eqlcnt
    integer(ESMF_KIND_I4)              :: rmdcnt
    integer(ESMF_KIND_I4)              :: incrmt
    integer(ESMF_KIND_I4)              :: lclbeg
    integer(ESMF_KIND_I4)              :: lclend
    integer(ESMF_KIND_I4)              :: i,j,k
    integer(ESMF_KIND_I8)              :: gapszs
    integer(ESMF_KIND_I8)              :: chkszs
    integer(ESMF_KIND_I4)              :: glbgapbnd(gaps,2)
    logical                            :: inclpt
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
        lclcntng = eqlcnt + rmdcnt
    else
        lclcntng = eqlcnt
    endif
    incrmt = 1
    lclbeg = (de_id * eqlcnt) + 1
    lclend = lclbeg + lclcntng - 1
#else
    if ( de_id .lt. rmdcnt ) then
        lclcntng = eqlcnt + 1
    else
        lclcntng = eqlcnt
    endif
    incrmt = npets
    lclbeg = de_id + 1
    lclend = lclbeg + (lclcntng-1) * npets
#endif

    allocate(indexListNoGaps(lclcntng))
    indexListNoGaps = (/(i, i=lclbeg, lclend, incrmt)/)

#if NGAPS > 0
    ! Check for correct number of gaps
    if ( gaps .gt. (gblcnt/3) ) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
            msg="Too many gaps are defined in user_model2",&
            line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
    endif
    gapszs = ( ( gblcnt * 6 ) / 10 ) / gaps
    chkszs = (gblcnt - ( gaps * gapszs )) / ( gaps + 1)
    glbgapbnd(1,:) = (/chkszs + 1,chkszs + gapszs/)
    do j=2, gaps
        glbgapbnd(j,1) = glbgapbnd(j-1,2) + chkszs + 1
        glbgapbnd(j,2) = glbgapbnd(j,1) + gapszs - 1
    enddo
    lclcnt = 0
    do i=1, size(indexListNoGaps)
        inclpt = .true.
        do j=1, gaps
            if ( indexListNoGaps(i) .ge. glbgapbnd(j,1) .AND. &
                 indexListNoGaps(i) .le. glbgapbnd(j,2) ) then
                inclpt = .false.
            endif
        enddo
        if (inclpt) lclcnt = lclcnt + 1
    enddo
    allocate(indexList(lclcnt))
    lclcnt = 0
    do i=1, size(indexListNoGaps)
        inclpt = .true.
        do j=1, gaps
            if ( indexListNoGaps(i) .ge. glbgapbnd(j,1) .AND. &
                 indexListNoGaps(i) .le. glbgapbnd(j,2) ) then
                inclpt = .false.
            endif
        enddo
        if (inclpt) then
            lclcnt = lclcnt + 1
            indexList(lclcnt) = indexListNoGaps(i)
        endif
    enddo
#else
    lclcnt = lclcntng
    allocate(indexList(lclcnt))
    indexList=indexListNoGaps
#endif
    deallocate(indexListNoGaps)

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

    if (de_id .eq. 0) print *, "Model1 global points ", gblcnt
    print *, "Model1 ", de_id, " Local excluded points ", (lclcntng-lclcnt)
    print *, "Model1 ", de_id, " Local included points ", lclcnt
    print *, "Model1 ", de_id, " Local minmax ", indexList(1),indexList(lclcnt)
    _DPRINT4_("Model1 ", de_id, " indexList ", indexList)
    T_EXIT("DATA_INIT")

    T_ENTER("ESMF_OBJ_C")
    ! Add fields to the export state.
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
