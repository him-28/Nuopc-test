#define FILENAME "util.F90"

module UTIL

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

  use ESMF

  implicit none

  private

  public FieldFill
  public FieldBundleFill
  public FieldWrite
  public FieldCopyData

! Mask codes
  integer, parameter :: MASK_INLAND_WATER =  -1
  integer, parameter :: MASK_WATER        =   0
  integer, parameter :: MASK_LAND         =   1
  integer, parameter :: MASK_FROZEN_WATER =   2
  integer, parameter :: MASK_FROZEN_LAND  =   3


  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine FieldFill(field, fill, rc)
    type(ESMF_Field)    :: field
    real(ESMF_KIND_R8)  :: fill
    integer             :: rc

    ! local variables
    integer                     :: ldecnt, lde
    integer                     :: tlb(2), tub(2)
    real(ESMF_KIND_R8), pointer :: fptr(:,:)
    integer                     :: localrc, stat

    rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, localDECount=ldecnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    do lde=0,ldecnt-1
      call ESMF_FieldGet(field, localDE=lde, farrayPtr=fptr, &
           totalLBound=tlb, totalUBound=tub, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      fptr(tlb(1):tub(1),tlb(2):tub(2)) = fill
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FieldBundleFill(fields, fill, rc)
    type(ESMF_FieldBundle) :: fields
    real(ESMF_KIND_R8)  :: fill
    integer             :: rc

    ! local variables
    integer                     :: fieldCount, i
    type(ESMF_Field), pointer   :: fieldList(:)
    integer                     :: ldecnt, lde
    integer                     :: tlb(2), tub(2)
    real(ESMF_KIND_R8), pointer :: fptr(:,:)
    integer                     :: localrc, stat

    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    if (fieldCount.eq.0) return

    allocate(fieldList(fieldCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of fieldList() failed.", &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    call ESMF_FieldBundleGet(fields, fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    do i = 1,fieldCount
      call ESMF_FieldGet(fieldList(i), localDECount=ldecnt, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      do lde=0,ldecnt-1
        call ESMF_FieldGet(fieldList(i), localDE=lde, farrayPtr=fptr, &
             totalLBound=tlb, totalUBound=tub, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=FILENAME)) return  ! bail out
        fptr(tlb(1):tub(1),tlb(2):tub(2)) = fill
      enddo
    enddo

    if (associated(fieldList)) deallocate(fieldList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FieldWrite(gcomp, field, rc)
    type(ESMF_GridComp) :: gcomp
    type(ESMF_Field)    :: field
    integer             :: rc

    ! local variables
    integer, parameter          :: rootPet = 0
    type(ESMF_VM)               :: vm
    integer                     :: iunit, nPets, lPet, i, j
    character(ESMF_MAXSTR)      :: cname, fname
    type(ESMF_Array)            :: array
    integer                     :: glb(2), gub(2), minIdx(2,1), maxIdx(2,1)
    real(ESMF_KIND_R8), pointer :: gptr(:,:)
    integer                     :: stat

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(gcomp, name=cname, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_VMGet(vm, petCount=nPets, localPet=lPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_FieldGet(field, name=fname, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    call ESMF_ArrayGet(array, minIndexPTile=minIdx, maxIndexPTile=maxIdx, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out
    glb(1) = minIdx(1,1)
    gub(1) = maxIdx(1,1)
    glb(2) = minIdx(2,1)
    gub(2) = maxIdx(2,1)

    if (lPet.eq.rootPet) then
      allocate(gptr(glb(1):gub(1),glb(2):gub(2)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of global array failed.", &
        line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    call ESMF_ArrayGather(array, gptr, rootPet, tile=1, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME, rcToReturn=rc)) return  ! bail out

    if (lPet.eq.rootPet) then
      call ESMF_UtilIOUnitGet(iunit, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      open(unit=iunit, file=trim(cname)//'_'//trim(fname), &
        form='formatted', action='write', status='replace')
      do j = glb(2),gub(2)
        write(iunit,'(100i1)') (nint(gptr(i,j)),i=glb(1),gub(1))
      enddo
      close(iunit)
      deallocate(gptr, stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Deallocation of global array failed.", &
        line=__LINE__, file=FILENAME)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
    call ESMF_VMBarrier(vm)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FieldCopyData(srcField, dstField, noData, rc)
    type(ESMF_Field)    :: srcField
    type(ESMF_Field)    :: dstField
    real(ESMF_KIND_R8)  :: noData
    integer             :: rc

    ! local variables
    integer                     :: ldecnt, lde
    integer                     :: tlb(2), tub(2)
    integer                     :: i, j
    real(ESMF_KIND_R8), pointer :: sptr(:,:), dptr(:,:)
    integer                     :: localrc, stat

    rc = ESMF_SUCCESS

    call ESMF_FieldGet(dstField, localDECount=ldecnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILENAME)) return  ! bail out

    do lde=0,ldecnt-1
      call ESMF_FieldGet(dstField, localDE=lde, farrayPtr=dptr, &
           totalLBound=tlb, totalUBound=tub, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      call ESMF_FieldGet(srcField, localDE=lde, farrayPtr=sptr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=FILENAME)) return  ! bail out
      do j = tlb(2),tub(2)
      do i = tlb(2),tub(2)
        if (sptr(i,j).lt.noData) dptr(i,j) = sptr(i,j)
      enddo
      enddo
    enddo

  end subroutine

end module
