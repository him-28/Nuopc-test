!-------------------------------------------------------------------------------
! COAMPS Field & Remapping Utilities
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_Futil.F90"
#include "COAMPS_Macros.h"
#define DEBUG_GET_EXTEND_SMM___disabled

module COAMPS_Futil

  use ESMF
  use NUOPC
  use COAMPS_GSRUMD

  implicit none

  private

  ! public methods
  public FieldRemapStore
  public FieldRemap
  public FieldRemapRelease
  public FieldBundleRemapStore
  public FieldBundleRemap
  public FieldBundleRemapRelease
  public FieldExtendStore
  public FieldExtend
  public FieldExtendRelease
  public FieldBundleExtendStore
  public FieldBundleExtend
  public FieldBundleExtendRelease
  public FieldFill
  public FieldBundleFill
  public FieldCopy
  public FieldBundleCopy
  public FieldCreate
  public FieldBundleCreate
  public FieldDestroy
  public FieldBundleDestroy
  public FieldAdd
  public FieldScale
  public RemapStatusCheck
  public RemapStatusWrite
  public FieldDebugWrite
  public PrintTimers

  ! missing value (public)
  real(ESMF_KIND_RX), parameter, public :: missingValue = real(999999,ESMF_KIND_RX)

  ! remap status values (public)
  integer, parameter, public :: REMAP_STATUS_NOT_SET     = 9
  integer, parameter, public :: REMAP_STATUS_MASKED_DST  = 0
  integer, parameter, public :: REMAP_STATUS_MASKED_SRC  = 1
  integer, parameter, public :: REMAP_STATUS_NOT_IN_SRC  = 2
  integer, parameter, public :: REMAP_STATUS_VALID_SRC   = 3
  integer, parameter, public :: REMAP_STATUS_EXTEND_FILL = 4

  ! field extend route handle type (public)
  type, public :: ExtendRouteHandle
    type(ESMF_RouteHandle), pointer :: rh(:) => null()
  end type

  ! Remap SMM type (private)
  type :: RemapSMM
    real(ESMF_KIND_RX),     pointer :: facList(:) => null()
    integer(ESMF_KIND_I4),  pointer :: facIdxList(:,:) => null()
  end type

  ! module interfaces
  interface FieldExtendStore
    module procedure FieldExtendStore_rs
    module procedure FieldExtendStore_it
  end interface
  interface FieldBundleExtendStore
    module procedure FieldBundleExtendStore_rs
    module procedure FieldBundleExtendStore_it
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldRemapStore - Store field remap route handle
! !INTERFACE:
  subroutine FieldRemapStore(srcField, dstField, remapRH, vm, &
    remapType, srcMaskValues, dstMaskValues, remapStatusField, cname, rc)
! !ARGUMENTS:
    type(ESMF_Field)                :: srcField
    type(ESMF_Field)                :: dstField
    type(ESMF_RouteHandle)          :: remapRH
    type(ESMF_VM)                   :: vm
    character(6),          optional :: remapType
    integer(ESMF_KIND_I4), optional :: srcMaskValues(:)
    integer(ESMF_KIND_I4), optional :: dstMaskValues(:)
    type(ESMF_Field),      optional :: remapStatusField
    character(*),          optional :: cname
    integer,               optional :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcField]
!     The source {\tt ESMF\_Field}.
!   \item[dstField]
!     The destination {\tt ESMF\_Field}.
!   \item[remapRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[vm]
!     The associated {\tt ESMF\_VM} object.
!   \item[{[remapType]}]
!     The type of remap.
!     Choices are: 'redist', 'bilinr', or 'bicubc'
!     If omitted, the default is 'bilinr'.
!   \item[{[srcMaskValues]}]
!     Array of source grid mask mask values.
!   \item[{[dstMaskValues]}]
!     Array of destination grid mask mask values.
!   \item[{[remapStatusField]}]
!     The remap status field defined on the destination grid.
!     The remap status values are:
!         REMAP_STATUS_NOT_SET     = 9
!         REMAP_STATUS_MASKED_DST  = 0
!         REMAP_STATUS_MASKED_SRC  = 1
!         REMAP_STATUS_NOT_IN_SRC  = 2
!         REMAP_STATUS_VALID_SRC   = 3
!         REMAP_STATUS_EXTEND_FILL = 4
!   \item[{[cname]}]
!     Character string to use a prefix for log file output.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(*), parameter :: mname='FieldRemapStore'
    integer :: localrc, stat
    type(RemapSMM) :: smm

    ! get remap SMM
    call GetRemapSMM(srcField, dstField, smm, vm, mname, cname=cname, &
      srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
      remapType=remapType, remapStatusField=remapStatusField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! store remap SMM
    call ESMF_FieldSMMStore(srcField, dstField, remapRH, &
      smm%facList, smm%facIdxList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! free memory for SMM
    deallocate(smm%facList, smm%facIdxList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=mname//': Deallocation of factor list failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldBundleRemapStore - Store field bundle remap route handle
! !INTERFACE:
  subroutine FieldBundleRemapStore(srcFields, dstFields, remapRH, vm, &
    remapType, srcMaskValues, dstMaskValues, remapStatusField, cname, rc)
! !ARGUMENTS:
    type(ESMF_FieldBundle)          :: srcFields
    type(ESMF_FieldBundle)          :: dstFields
    type(ESMF_RouteHandle)          :: remapRH
    type(ESMF_VM)                   :: vm
    character(6),          optional :: remapType
    integer(ESMF_KIND_I4), optional :: srcMaskValues(:)
    integer(ESMF_KIND_I4), optional :: dstMaskValues(:)
    type(ESMF_Field),      optional :: remapStatusField
    character(*),          optional :: cname
    integer,               optional :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcFields]
!     The source {\tt ESMF\_FieldBundle}.
!   \item[dstFields]
!     The destination {\tt ESMF\_FieldBundle}.
!   \item[remapRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[vm]
!     The associated {\tt ESMF\_VM} object.
!   \item[{[remapType]}]
!     The type of remap.
!     Choices are: 'redist', 'bilinr', or 'bicubc'
!     If omitted, the default is 'bilinr'.
!   \item[{[srcMaskValues]}]
!     Array of source grid mask mask values.
!   \item[{[dstMaskValues]}]
!     Array of destination grid mask mask values.
!   \item[{[remapStatusField]}]
!     The remap status field defined on the destination grid.
!     The remap status values are:
!         REMAP_STATUS_NOT_SET     = 9
!         REMAP_STATUS_MASKED_DST  = 0
!         REMAP_STATUS_MASKED_SRC  = 1
!         REMAP_STATUS_NOT_IN_SRC  = 2
!         REMAP_STATUS_VALID_SRC   = 3
!         REMAP_STATUS_EXTEND_FILL = 4
!   \item[{[cname]}]
!     Character string to use a prefix for log file output.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(*), parameter :: mname='FieldBundleRemapStore'
    integer :: localrc, stat
    type(RemapSMM) :: smm
    integer :: srcFieldCount, dstFieldCount
    type(ESMF_Field) :: srcField, dstField

    ! get/check field count
    call ESMF_FieldBundleGet(srcFields, fieldCount=srcFieldCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(dstFields, fieldCount=dstFieldCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (srcFieldCount.ne.dstFieldCount) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=mname//': srcFieldCount != dstFieldCount')
      return ! bail out
    endif
    if (srcFieldCount.eq.0) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=mname//': no fields in bundles')
      return ! bail out
    endif

    ! get first field in each bundle
    call ESMF_FieldBundleGet(srcFields, 1, srcField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldBundleGet(dstFields, 1, dstField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get remap SMM
    call GetRemapSMM(srcField, dstField, smm, vm, mname, cname=cname, &
      srcMaskValues=srcMaskValues, dstMaskValues=dstMaskValues, &
      remapType=remapType, remapStatusField=remapStatusField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! store remap SMM
    call ESMF_FieldBundleSMMStore(srcFields, dstFields, remapRH, &
      smm%facList, smm%facIdxList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! free memory for SMM
    deallocate(smm%facList, smm%facIdxList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=mname//': Deallocation of factor list failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldRemap - Execute field remap route handle
! !INTERFACE:
  subroutine FieldRemap(srcField, dstField, remapRH, &
    zeroRegion, checkFlag, rc)
! !ARGUMENTS:
    type(ESMF_Field)                :: srcField
    type(ESMF_Field)                :: dstField
    type(ESMF_RouteHandle)          :: remapRH
    type(ESMF_Region_Flag),optional :: zeroRegion
    logical               ,optional :: checkFlag
    integer,               optional :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcField]
!     The source {\tt ESMF\_Field}.
!   \item[dstField]
!     The destination {\tt ESMF\_Field}.
!   \item[remapRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[{[zeroRegion]}]
!     Zero destination region option.  See ESMF documentation.
!   \item[{[checkFlag]}]
!     Check flag option.  See ESMF documentation.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    ! NONE

    call ESMF_FieldSMM(srcField, dstField, remapRH, &
      zeroRegion=zeroRegion, checkFlag=checkFlag, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldBundleRemap - Execute field bundle remap route handle
! !INTERFACE:
  subroutine FieldBundleRemap(srcFields, dstFields, remapRH, &
! !ARGUMENTS:
    zeroRegion, checkFlag, rc)
    type(ESMF_FieldBundle)          :: srcFields
    type(ESMF_FieldBundle)          :: dstFields
    type(ESMF_RouteHandle)          :: remapRH
    type(ESMF_Region_Flag),optional :: zeroRegion
    logical               ,optional :: checkFlag
    integer,               optional :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcFields]
!     The source {\tt ESMF\_FieldBundle}.
!   \item[dstFields]
!     The destination {\tt ESMF\_FieldBundle}.
!   \item[remapRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[{[zeroRegion]}]
!     Zero destination region option.  See ESMF documentation.
!   \item[{[checkFlag]}]
!     Check flag option.  See ESMF documentation.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    ! NONE

    call ESMF_FieldBundleSMM(srcFields, dstFields, remapRH, &
      zeroRegion=zeroRegion, checkFlag=checkFlag, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldRemapRelease - Release field remap route handle
! !INTERFACE:
  subroutine FieldRemapRelease(remapRH, rc)
! !ARGUMENTS:
    type(ESMF_RouteHandle) :: remapRH
    integer, optional      :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \item[remapRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    ! NONE

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldSMMRelease(remapRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldBundleRemapRelease - Release field bundle remap route handle
! !INTERFACE:
  subroutine FieldBundleRemapRelease(remapRH, rc)
! !ARGUMENTS:
    type(ESMF_RouteHandle) :: remapRH
    integer, optional      :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \item[remapRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    ! NONE

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleSMMRelease(remapRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldExtendStore - Store field extend route handle based on an
!                               associated remap status field.
! !INTERFACE:
  ! Private name; call using FieldExtendStore()
  subroutine FieldExtendStore_rs(srcField, extendRH, vm, &
    remapStatusField, full, cname, rc)
! !ARGUMENTS:
    type(ESMF_Field)                 :: srcField
    type(ExtendRouteHandle)          :: extendRH
    type(ESMF_VM)                    :: vm
    type(ESMF_Field)                 :: remapStatusField
    logical,                optional :: full
    character(*),           optional :: cname
    integer,                optional :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcField]
!     The source {\tt ESMF\_Field}.
!   \item[extendRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[vm]
!     The associated {\tt ESMF\_VM} object.
!   \item[remapStatusField]
!     The associated remap status {\tt ESMF\_Field}.
!   \item[{[full]}]
!     Use full stencil option.
!   \item[{[cname]}]
!     Character string to use a prefix for log file output.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(*), parameter :: mname='FieldExtendStore'
    integer :: iter, stat
    type(RemapSMM), pointer :: smm(:)
    type(ESMF_Field)        :: dstField

    ! get SMM
    nullify(smm)
    call GetExtendSMM(srcField, smm, vm, mname, &
      remapStatusField=remapStatusField, full=full, cname=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! unassociated smm means zero iterations
    if (.not.associated(smm)) return

    ! allocate routeHandle
    allocate(extendRH%rh(ubound(smm,1)), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=mname//': Allocation of routeHandle array failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! create work field for routeHandle store
    dstField = FieldCreate(srcField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! store SMM in routeHandle
    do iter=1,ubound(smm,1)
      call ESMF_FieldSMMStore(srcField, dstField, extendRH%rh(iter), &
        smm(iter)%facList, smm(iter)%facIdxList, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! free memory for SMM
    do iter=1,ubound(smm,1)
      deallocate(smm(iter)%facList, smm(iter)%facIdxList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=mname//': Deallocation of smm factor list failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    enddo
    deallocate(smm, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=mname//': Deallocation of smm array failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! destroy work field
    call FieldDestroy(dstField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldExtendStore - Store field extend route handle based on
!                               number of iterations and mask values.
! !INTERFACE:
  ! Private name; call using FieldExtendStore()
  subroutine FieldExtendStore_it(srcField, extendRH, vm, &
    niter, mskval, full, cname, rc)
! !ARGUMENTS:
    type(ESMF_Field)                 :: srcField
    type(ExtendRouteHandle)          :: extendRH
    type(ESMF_VM)                    :: vm
    integer                          :: niter
    integer(ESMF_KIND_I4)            :: mskval(:)
    logical,                optional :: full
    character(*),           optional :: cname
    integer,                optional :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcField]
!     The source {\tt ESMF\_Field}.
!   \item[extendRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[vm]
!     The associated {\tt ESMF\_VM} object.
!   \item[niter]
!     The number of iterations.
!   \item[mskval]
!     Array of mask values.
!   \item[{[full]}]
!     Use full stencil option.
!   \item[{[cname]}]
!     Character string to use a prefix for log file output.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(*), parameter :: mname='FieldExtendStore'
    integer :: iter, stat
    type(RemapSMM), pointer :: smm(:)
    type(ESMF_Field)        :: dstField

    ! get SMM
    nullify(smm)
    call GetExtendSMM(srcField, smm, vm, mname, &
      niter=niter, mskval=mskval, full=full, cname=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! unassociated smm means zero iterations
    if (.not.associated(smm)) return

    ! allocate routeHandle
    allocate(extendRH%rh(ubound(smm,1)), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=mname//': Allocation of routeHandle array failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! create work field for routeHandle store
    dstField = FieldCreate(srcField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! store SMM in routeHandle
    do iter=1,ubound(smm,1)
      call ESMF_FieldSMMStore(srcField, dstField, extendRH%rh(iter), &
        smm(iter)%facList, smm(iter)%facIdxList, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! free memory for SMM
    do iter=1,ubound(smm,1)
      deallocate(smm(iter)%facList, smm(iter)%facIdxList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=mname//': Deallocation of smm factor list failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    enddo
    deallocate(smm, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=mname//': Deallocation of smm array failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! destroy work field
    call FieldDestroy(dstField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldBundleExtendStore - Store field bundle extend route handle
!                                     based on an associated remap status field.
! !INTERFACE:
  ! Private name; call using FieldBundleExtendStore()
  subroutine FieldBundleExtendStore_rs(srcFields, extendRH, vm, &
    remapStatusField, full, cname, rc)
! !ARGUMENTS:
    type(ESMF_FieldBundle)           :: srcFields
    type(ExtendRouteHandle)          :: extendRH
    type(ESMF_VM)                    :: vm
    type(ESMF_Field)                 :: remapStatusField
    logical,                optional :: full
    character(*),           optional :: cname
    integer,                optional :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcFields]
!     The source {\tt ESMF\_FieldBundle}.
!   \item[extendRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[vm]
!     The associated {\tt ESMF\_VM} object.
!   \item[remapStatusField]
!     The associated remap status {\tt ESMF\_Field}.
!   \item[{[full]}]
!     Use full stencil option.
!   \item[{[cname]}]
!     Character string to use a prefix for log file output.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(*), parameter :: mname='FieldBundleExtendStore'
    integer :: iter, stat
    type(RemapSMM), pointer :: smm(:)
    integer                 :: fieldCount
    type(ESMF_FieldBundle)  :: dstFields
    type(ESMF_Field)        :: srcField, dstField

    ! get fieldCount and return if no fields
    call ESMF_FieldBundleGet(srcFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (fieldCount.eq.0) return

    ! get first field
    call ESMF_FieldBundleGet(srcFields, 1, srcField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get SMM
    nullify(smm)
    call GetExtendSMM(srcField, smm, vm, mname, &
      remapStatusField=remapStatusField, full=full, cname=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! unassociated smm means zero iterations
    if (.not.associated(smm)) return

    ! allocate routeHandle
    allocate(extendRH%rh(ubound(smm,1)), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=mname//': Allocation of routeHandle array failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! create work field bundle for routeHandle store
    dstFields = FieldBundleCreate(srcFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! store SMM in routeHandle
    do iter=1,ubound(smm,1)
      call ESMF_FieldBundleSMMStore(srcFields, dstFields, extendRH%rh(iter), &
        smm(iter)%facList, smm(iter)%facIdxList, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! free memory for SMM
    do iter=1,ubound(smm,1)
      deallocate(smm(iter)%facList, smm(iter)%facIdxList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=mname//': Deallocation of smm factor list failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    enddo
    deallocate(smm, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=mname//': Deallocation of smm array failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! destroy work field bundle
    call FieldBundleDestroy(dstFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldBundleExtendStore - Store field bundle extend route handle
!                                     based on number of iterations and mask values.
! !INTERFACE:
  ! Private name; call using FieldBundleExtendStore()
  subroutine FieldBundleExtendStore_it(srcFields, extendRH, vm, &
    niter, mskval, full, cname, rc)
! !ARGUMENTS:
    type(ESMF_FieldBundle)           :: srcFields
    type(ExtendRouteHandle)          :: extendRH
    type(ESMF_VM)                    :: vm
    integer                          :: niter
    integer(ESMF_KIND_I4)            :: mskval(:)
    logical,                optional :: full
    character(*),           optional :: cname
    integer,                optional :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcFields]
!     The source {\tt ESMF\_FieldBundle}.
!   \item[extendRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[vm]
!     The associated {\tt ESMF\_VM} object.
!   \item[niter]
!     The number of iterations.
!   \item[mskval]
!     Array of mask values.
!   \item[{[full]}]
!     Use full stencil option.
!   \item[{[cname]}]
!     Character string to use a prefix for log file output.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(*), parameter :: mname='FieldBundleExtendStore'
    integer :: iter, stat
    type(RemapSMM), pointer :: smm(:)
    integer                 :: fieldCount
    type(ESMF_FieldBundle)  :: dstFields
    type(ESMF_Field)        :: srcField, dstField

    ! get fieldCount and return if no fields
    call ESMF_FieldBundleGet(srcFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (fieldCount.eq.0) return

    ! get first field
    call ESMF_FieldBundleGet(srcFields, 1, srcField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get SMM
    nullify(smm)
    call GetExtendSMM(srcField, smm, vm, mname, &
      niter=niter, mskval=mskval, full=full, cname=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! unassociated smm means zero iterations
    if (.not.associated(smm)) return

    ! allocate routeHandle
    allocate(extendRH%rh(ubound(smm,1)), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=mname//': Allocation of routeHandle array failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! create work field bundle for routeHandle store
    dstFields = FieldBundleCreate(srcFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! store SMM in routeHandle
    do iter=1,ubound(smm,1)
      call ESMF_FieldBundleSMMStore(srcFields, dstFields, extendRH%rh(iter), &
        smm(iter)%facList, smm(iter)%facIdxList, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    ! free memory for SMM
    do iter=1,ubound(smm,1)
      deallocate(smm(iter)%facList, smm(iter)%facIdxList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=mname//': Deallocation of smm factor list failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    enddo
    deallocate(smm, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=mname//': Deallocation of smm array failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! destroy work field bundle
    call FieldBundleDestroy(dstFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldExtend - Execute field extend route handle
! !INTERFACE:
  subroutine FieldExtend(srcField, extendRH, rc)
! !ARGUMENTS:
    type(ESMF_Field)       :: srcField
    type(ExtendRouteHandle):: extendRH
    integer, optional      :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcField]
!     The source {\tt ESMF\_Field}.
!   \item[extendRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                :: iter
    logical                :: checkFlag = .false.
    type(ESMF_Region_Flag) :: zeroRegion = ESMF_REGION_SELECT
    type(ESMF_Field)       :: dstField

    if (present(rc)) rc = ESMF_SUCCESS

    if (size(extendRH%rh).eq.0) return

    dstField = FieldCreate(srcField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    do iter=lbound(extendRH%rh,1),ubound(extendRH%rh,1)
      call FieldCopy(dstField, srcField, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_FieldSMM(dstField, srcField, extendRH%rh(iter), &
        zeroRegion=zeroRegion, checkFlag=checkFlag, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    call FieldDestroy(dstField, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldBundleExtend - Execute field bundle extend route handle
! !INTERFACE:
  subroutine FieldBundleExtend(srcFields, extendRH, rc)
! !ARGUMENTS:
    type(ESMF_FieldBundle) :: srcFields
    type(ExtendRouteHandle):: extendRH
    integer, optional      :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \begin{description}
!   \item[srcFields]
!     The source {\tt ESMF\_FieldBundle}.
!   \item[extendRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                :: fieldCount, iter
    logical                :: checkFlag = .false.
    type(ESMF_Region_Flag) :: zeroRegion = ESMF_REGION_SELECT
    type(ESMF_FieldBundle) :: dstFields

    if (present(rc)) rc = ESMF_SUCCESS

    if (size(extendRH%rh).eq.0) return

    call ESMF_FieldBundleGet(srcFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (fieldCount.eq.0) return

    dstFields = FieldBundleCreate(srcFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    do iter=lbound(extendRH%rh,1),ubound(extendRH%rh,1)
      call FieldBundleCopy(dstFields, srcFields, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_FieldBundleSMM(dstFields, srcFields, extendRH%rh(iter), &
        zeroRegion=zeroRegion, checkFlag=checkFlag, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    call FieldBundleDestroy(dstFields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldExtendRelease - Release field extend route handle
! !INTERFACE:
  subroutine FieldExtendRelease(extendRH, rc)
! !ARGUMENTS:
    type(ExtendRouteHandle):: extendRH
    integer, optional      :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \item[remapRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer :: iter

    if (present(rc)) rc = ESMF_SUCCESS

    if (size(extendRH%rh).eq.0) return

    do iter=lbound(extendRH%rh,1),ubound(extendRH%rh,1)
      call ESMF_FieldSMMRelease(extendRH%rh(iter), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    if (associated(extendRH%rh)) deallocate(extendRH%rh)

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldBundleExtendRelease - Release field bundle extend route handle
! !INTERFACE:
  subroutine FieldBundleExtendRelease(extendRH, rc)
! !ARGUMENTS:
    type(ExtendRouteHandle):: extendRH
    integer, optional      :: rc
! !DESCRIPTION:
!   insert description here
!
!   The arguments are:
!   \item[remapRH]
!     The {\tt ESMF\_RouteHandle} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer :: iter

    if (present(rc)) rc = ESMF_SUCCESS

    if (size(extendRH%rh).eq.0) return

    do iter=lbound(extendRH%rh,1),ubound(extendRH%rh,1)
      call ESMF_FieldBundleSMMRelease(extendRH%rh(iter), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    if (associated(extendRH%rh)) deallocate(extendRH%rh)

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldFill(dstField, dstFillVal, dstMaskVal, rc)
    type(ESMF_Field)                 :: dstField
    real(ESMF_KIND_RX)               :: dstFillVal
    integer(ESMF_KIND_I4), optional  :: dstMaskVal(:)
    integer,               optional  :: rc

    ! local variables
    integer :: ldecnt, lde, i, j, k
    integer :: dstDimCount, ndim, iund
    integer :: lb2(2), ub2(2)
    integer :: lb3(3), ub3(3)
    integer :: dstGridToFieldMap(2)
    type(ESMF_Grid) :: dstGrid
    type(ESMF_StaggerLoc) :: dstStgr
    type(ESMF_Array) :: dstMaskArray
    integer(ESMF_KIND_I4), pointer :: dstMask(:,:)
    real(ESMF_KIND_RX), pointer :: dptr2(:,:)
    real(ESMF_KIND_RX), pointer :: dptr3(:,:,:)

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(dstField, localDECount=ldecnt, grid=dstGrid, &
      staggerLoc=dstStgr, dimCount=dstDimCount, &
      gridToFieldMap=dstGridToFieldMap, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (dstDimCount.ne.2.and.dstDimCount.ne.3) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldFill: dimCount must be 2 or 3')
      return ! bail out
    endif
    ndim = dstDimCount

    if (dstGridToFieldMap(1).eq.1.and.dstGridToFieldMap(2).eq.2) then
      iund = 3
    elseif (dstGridToFieldMap(1).eq.1.and.dstGridToFieldMap(2).eq.3) then
      iund = 2
    else
      iund = 1
    endif

    if (present(dstMaskVal)) then
      call ESMF_GridGetItem(dstGrid, itemFlag=ESMF_GRIDITEM_MASK, &
        staggerLoc=dstStgr, array=dstMaskArray, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

    do lde=0,ldecnt-1

      if (ndim.eq.2) then
        call ESMF_FieldGet(dstField, localDE=lde, farrayPtr=dptr2, &
             exclusiveLBound=lb2, exclusiveUBound=ub2, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        call ESMF_FieldGet(dstField, localDE=lde, farrayPtr=dptr3, &
             exclusiveLBound=lb3, exclusiveUBound=ub3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif

      if (present(dstMaskVal)) then
        call ESMF_ArrayGet(dstMaskArray, localDE=lde, farrayPtr=dstMask, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif

      if (ndim.eq.2) then

        if (present(dstMaskVal)) then
          do j = lb2(2),ub2(2)
          do i = lb2(1),ub2(1)
            if (all(dstMask(i,j).ne.dstMaskVal)) dptr2(i,j) = dstFillVal
          enddo
          enddo
        else
          dptr2(lb2(1):ub2(1),lb2(2):ub2(2)) = dstFillVal
        endif

      else

        if (present(dstMaskVal)) then
          select case (iund)
          case (1)
            do j = lb3(3),ub3(3)
            do i = lb3(2),ub3(2)
              if (all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(lb3(1):ub3(1),i,j) = dstFillVal
            enddo
            enddo
          case (2)
            do j = lb3(3),ub3(3)
            do i = lb3(1),ub3(1)
              if (all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(i,lb3(2):ub3(2),j) = dstFillVal
            enddo
            enddo
          case (3)
            do j = lb3(2),ub3(2)
            do i = lb3(1),ub3(1)
              if (all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(i,j,lb3(3):ub3(3)) = dstFillVal
            enddo
            enddo
          end select
        else
          dptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3)) = dstFillVal
        endif

      endif

    enddo

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldBundleFill(dstFields, dstFillVal, dstMaskVal, rc)
    type(ESMF_FieldBundle)           :: dstFields
    real(ESMF_KIND_RX)               :: dstFillVal
    integer(ESMF_KIND_I4), optional  :: dstMaskVal(:)
    integer,               optional  :: rc

    ! local variables
    integer                     :: fieldCount, i
    type(ESMF_Field), pointer   :: dstFieldList(:)
    integer                     :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (fieldCount.eq.0) return

    allocate(dstFieldList(fieldCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of fieldList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    call ESMF_FieldBundleGet(dstFields, fieldList=dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    do i = 1,fieldCount
      call FieldFill(dstFieldList(i), dstFillVal, dstMaskVal=dstMaskVal, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    if (associated(dstFieldList)) deallocate(dstFieldList)

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldCopy(dstField, srcField, dstMaskVal, srcMaskVal, missingVal, rc)
    type(ESMF_Field)                 :: dstField
    type(ESMF_Field)                 :: srcField
    integer(ESMF_KIND_I4), optional  :: dstMaskVal(:)
    integer(ESMF_KIND_I4), optional  :: srcMaskVal(:)
    real(ESMF_KIND_RX),    optional  :: missingVal
    integer,               optional  :: rc

    ! local variables
    integer :: ldecnt, lde, i, j, k
    integer :: srcDimCount, dstDimCount, ndim, iund
    integer :: lb2(2), ub2(2)
    integer :: lb3(3), ub3(3)
    integer :: srcGridToFieldMap(2), dstGridToFieldMap(2)
    type(ESMF_Grid) :: srcGrid, dstGrid
    type(ESMF_StaggerLoc) :: srcStgr, dstStgr
    type(ESMF_Array) :: srcMaskArray, dstMaskArray
    integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
    real(ESMF_KIND_RX), pointer :: sptr2(:,:),   dptr2(:,:)
    real(ESMF_KIND_RX), pointer :: sptr3(:,:,:), dptr3(:,:,:)

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(missingVal)) then
      if (present(srcMaskVal).or.present(dstMaskVal)) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg='FieldCopy: missingVal specified with srcMaskVal '// &
          'or dstMaskVal not supported')
        return ! bail out
      endif
    endif

    call ESMF_FieldGet(srcField, localDECount=ldecnt, grid=srcGrid, &
      staggerLoc=srcStgr, dimCount=srcDimCount, &
      gridToFieldMap=srcGridToFieldMap, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (srcDimCount.ne.2.and.srcDimCount.ne.3) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldCopy: src dimCount must be 2 or 3')
      return ! bail out
    endif

    call ESMF_FieldGet(dstField, localDECount=ldecnt, grid=dstGrid, &
      staggerLoc=dstStgr, dimCount=dstDimCount, &
      gridToFieldMap=dstGridToFieldMap, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (dstDimCount.ne.2.and.dstDimCount.ne.3) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldCopy: dst dimCount must be 2 or 3')
      return ! bail out
    endif

    if (srcDimCount.ne.dstDimCount) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldCopy: src & dst dimCount must be equal')
      return ! bail out
    endif
    ndim = dstDimCount

    if (any(srcGridToFieldMap.ne.dstGridToFieldMap)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldCopy: src & dst gridToFieldMap must be equal')
      return ! bail out
    endif
    if (dstGridToFieldMap(1).eq.1.and.dstGridToFieldMap(2).eq.2) then
      iund = 3
    elseif (dstGridToFieldMap(1).eq.1.and.dstGridToFieldMap(2).eq.3) then
      iund = 2
    else
      iund = 1
    endif

    if (present(srcMaskVal)) then
      call ESMF_GridGetItem(srcGrid, itemFlag=ESMF_GRIDITEM_MASK, &
        staggerLoc=srcStgr, array=srcMaskArray, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

    if (present(dstMaskVal)) then
      call ESMF_GridGetItem(dstGrid, itemFlag=ESMF_GRIDITEM_MASK, &
        staggerLoc=dstStgr, array=dstMaskArray, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

    do lde=0,ldecnt-1

      if (ndim.eq.2) then
        call ESMF_FieldGet(srcField, localDE=lde, farrayPtr=sptr2, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        call ESMF_FieldGet(dstField, localDE=lde, farrayPtr=dptr2, &
             exclusiveLBound=lb2, exclusiveUBound=ub2, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        call ESMF_FieldGet(srcField, localDE=lde, farrayPtr=sptr3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        call ESMF_FieldGet(dstField, localDE=lde, farrayPtr=dptr3, &
             exclusiveLBound=lb3, exclusiveUBound=ub3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif

      if (present(srcMaskVal)) then
        call ESMF_ArrayGet(srcMaskArray, localDE=lde, farrayPtr=srcMask, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      if (present(dstMaskVal)) then
        call ESMF_ArrayGet(dstMaskArray, localDE=lde, farrayPtr=dstMask, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif

      if (ndim.eq.2) then

        if (present(srcMaskVal).and.present(dstMaskVal)) then
          do j = lb2(2),ub2(2)
          do i = lb2(1),ub2(1)
            if (all(srcMask(i,j).ne.srcMaskVal).and. &
                all(dstMask(i,j).ne.dstMaskVal)) dptr2(i,j) = sptr2(i,j)
          enddo
          enddo
        elseif (present(srcMaskVal)) then
          do j = lb2(2),ub2(2)
          do i = lb2(1),ub2(1)
            if (all(srcMask(i,j).ne.srcMaskVal)) dptr2(i,j) = sptr2(i,j)
          enddo
          enddo
        elseif (present(dstMaskVal)) then
          do j = lb2(2),ub2(2)
          do i = lb2(1),ub2(1)
            if (all(dstMask(i,j).ne.dstMaskVal)) dptr2(i,j) = sptr2(i,j)
          enddo
          enddo
        else
          if (present(missingVal)) then
            do j = lb2(2),ub2(2)
            do i = lb2(1),ub2(1)
              if (dptr2(i,j).eq.missingVal) dptr2(i,j) = sptr2(i,j)
            enddo
            enddo
          else
            dptr2(lb2(1):ub2(1),lb2(2):ub2(2)) = &
              sptr2(lb2(1):ub2(1),lb2(2):ub2(2))
          endif
        endif

      else

        if (present(srcMaskVal).and.present(dstMaskVal)) then
          select case (iund)
          case (1)
            do j = lb3(3),ub3(3)
            do i = lb3(2),ub3(2)
              if (all(srcMask(i,j).ne.srcMaskVal).and. &
                  all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(lb3(1):ub3(1),i,j) = sptr3(lb3(1):ub3(1),i,j)
            enddo
            enddo
          case (2)
            do j = lb3(3),ub3(3)
            do i = lb3(1),ub3(1)
              if (all(srcMask(i,j).ne.srcMaskVal).and. &
                  all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(i,lb3(2):ub3(2),j) = sptr3(i,lb3(2):ub3(2),j)
            enddo
            enddo
          case (3)
            do j = lb3(2),ub3(2)
            do i = lb3(1),ub3(1)
              if (all(srcMask(i,j).ne.srcMaskVal).and. &
                  all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(i,j,lb3(3):ub3(3)) = sptr3(i,j,lb3(3):ub3(3))
            enddo
            enddo
          end select
        elseif (present(srcMaskVal)) then
          select case (iund)
          case (1)
            do j = lb3(3),ub3(3)
            do i = lb3(2),ub3(2)
              if (all(srcMask(i,j).ne.srcMaskVal)) &
                dptr3(lb3(1):ub3(1),i,j) = sptr3(lb3(1):ub3(1),i,j)
            enddo
            enddo
          case (2)
            do j = lb3(3),ub3(3)
            do i = lb3(1),ub3(1)
              if (all(srcMask(i,j).ne.srcMaskVal)) &
                dptr3(i,lb3(2):ub3(2),j) = sptr3(i,lb3(2):ub3(2),j)
            enddo
            enddo
          case (3)
            do j = lb3(2),ub3(2)
            do i = lb3(1),ub3(1)
              if (all(srcMask(i,j).ne.srcMaskVal)) &
                dptr3(i,j,lb3(3):ub3(3)) = sptr3(i,j,lb3(3):ub3(3))
            enddo
            enddo
          end select
        elseif (present(dstMaskVal)) then
          select case (iund)
          case (1)
            do j = lb3(3),ub3(3)
            do i = lb3(2),ub3(2)
              if (all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(lb3(1):ub3(1),i,j) = sptr3(lb3(1):ub3(1),i,j)
            enddo
            enddo
          case (2)
            do j = lb3(3),ub3(3)
            do i = lb3(1),ub3(1)
              if (all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(i,lb3(2):ub3(2),j) = sptr3(i,lb3(2):ub3(2),j)
            enddo
            enddo
          case (3)
            do j = lb3(2),ub3(2)
            do i = lb3(1),ub3(1)
              if (all(dstMask(i,j).ne.dstMaskVal)) &
                dptr3(i,j,lb3(3):ub3(3)) = sptr3(i,j,lb3(3):ub3(3))
            enddo
            enddo
          end select
        else
          if (present(missingVal)) then
            do k = lb3(3),ub3(3)
            do j = lb3(2),ub3(2)
            do i = lb3(1),ub3(1)
              if (dptr3(i,j,k).eq.missingVal) dptr3(i,j,k) = sptr3(i,j,k)
            enddo
            enddo
            enddo
          else
            dptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3)) = &
              sptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3))
          endif
        endif

      endif

    enddo

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldBundleCopy(dstFields, srcFields, dstMaskVal, srcMaskVal, missingVal, rc)
    type(ESMF_FieldBundle)           :: dstFields
    type(ESMF_FieldBundle)           :: srcFields
    integer(ESMF_KIND_I4), optional  :: dstMaskVal(:)
    integer(ESMF_KIND_I4), optional  :: srcMaskVal(:)
    real(ESMF_KIND_RX),    optional  :: missingVal
    integer,               optional  :: rc

    ! local variables
    integer                     :: fieldCount, i
    type(ESMF_Field), pointer   :: srcFieldList(:)
    type(ESMF_Field), pointer   :: dstFieldList(:)
    integer                     :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(missingVal)) then
      if (present(srcMaskVal).or.present(dstMaskVal)) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg='FieldBundleCopy: missingVal specified with srcMaskVal '// &
          'or dstMaskVal not supported')
        return ! bail out
      endif
    endif

    call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (fieldCount.eq.0) return

    allocate(srcFieldList(fieldCount), dstFieldList(fieldCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of fieldList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    call ESMF_FieldBundleGet(srcFields, fieldList=srcFieldList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    call ESMF_FieldBundleGet(dstFields, fieldList=dstFieldList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    do i = 1,fieldCount
      call FieldCopy(dstFieldList(i), srcFieldList(i), &
        dstMaskVal=dstMaskVal, srcMaskVal=srcMaskVal, missingVal=missingVal, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    if (associated(srcFieldList)) deallocate(srcFieldList)
    if (associated(dstFieldList)) deallocate(dstFieldList)

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldCreate - Create deep copy of a field
! !INTERFACE:
  function FieldCreate(field, grid, rc) result(newField)
! !RETURN VALUE:
    type(ESMF_Field)          :: field
! !ARGUMENTS:
    type(ESMF_Grid), optional :: grid
    integer,         optional :: rc
    type(ESMF_Field)          :: newField
! !DESCRIPTION:
!   insert description here
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer, parameter     :: dimg=2
    integer, parameter     :: ldecnt=1
    integer                :: iter, dimf
    integer                :: g2fm(dimg), tlw(dimg,ldecnt), tuw(dimg,ldecnt)
    integer, allocatable   :: ulb(:), uub(:)
    type(ESMF_Grid)        :: fgrd
    type(ESMF_ArraySpec)   :: spec
    type(ESMF_StaggerLoc)  :: stgr
    type(ESMF_Field)       :: tmpfd
    character(ESMF_MAXSTR) :: fname

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, grid=fgrd, arraySpec=spec, dimCount=dimf, &
      staggerLoc=stgr, name=fname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldGet(field, gridToFieldMap=g2fm, totalLWidth=tlw, &
      totalUWidth=tuw, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (dimf.eq.dimg) then
      if (present(grid)) then
        newField = ESMF_FieldCreate(grid, spec, staggerLoc=stgr, &
          totalLWidth=tlw(:,1), totalUWidth=tuw(:,1), name=trim(fname), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        newField = ESMF_FieldCreate(fgrd, spec, staggerLoc=stgr, &
          totalLWidth=tlw(:,1), totalUWidth=tuw(:,1), name=trim(fname), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    else
      allocate(ulb(dimf-dimg), uub(dimf-dimg))
      call ESMF_FieldGet(field, gridToFieldMap=g2fm, ungriddedLBound=ulb, &
        ungriddedUBound=uub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (present(grid)) then
        newField = ESMF_FieldCreate(grid, spec, staggerLoc=stgr, &
          gridToFieldMap=g2fm, ungriddedLBound=ulb, ungriddedUBound=uub, &
          totalLWidth=tlw(:,1), totalUWidth=tuw(:,1), name=trim(fname), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        newField = ESMF_FieldCreate(fgrd, spec, staggerLoc=stgr, &
          gridToFieldMap=g2fm, ungriddedLBound=ulb, ungriddedUBound=uub, &
          totalLWidth=tlw(:,1), totalUWidth=tuw(:,1), name=trim(fname), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      deallocate(ulb, uub)
    endif

  end function
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
!BOP
! !IROUTINE: FieldBundleCreate - Create deep copy of a field bundle
! !INTERFACE:
  function FieldBundleCreate(fields, rc) result(newFields)
! !RETURN VALUE:
    type(ESMF_FieldBundle) :: fields
! !ARGUMENTS:
    integer, optional      :: rc
    type(ESMF_FieldBundle) :: newFields
! !DESCRIPTION:
!   insert description here
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    character(ESMF_MAXSTR)      :: bname
    integer                     :: fieldCount, i
    type(ESMF_Field), pointer   :: fieldList(:)
    type(ESMF_Grid)             :: grid
    type(ESMF_Field), pointer   :: newFieldList(:)
    integer                     :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fields, fieldCount=fieldCount, name=bname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    newFields = ESMF_FieldBundleCreate(name=trim(bname), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (fieldCount.eq.0) return

    allocate(fieldList(fieldCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of fieldList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call ESMF_FieldBundleGet(fields, fieldList=fieldList, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    allocate(newFieldList(fieldCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of newFieldList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    do i = 1,fieldCount
      newFieldList(i) = FieldCreate(fieldList(i), grid, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo
    call ESMF_FieldBundleAdd(newFields, newFieldList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (associated(fieldList)) deallocate(fieldList)
    if (associated(newFieldList)) deallocate(newFieldList)

  end function
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldDestroy(field, rc)
    type(ESMF_Field)       :: field
    integer, optional      :: rc

    ! local variables
    ! NONE

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldBundleDestroy(fields, rc)
    type(ESMF_FieldBundle) :: fields
    integer, optional      :: rc

    ! local variables
    integer                     :: fieldCount, i
    type(ESMF_Field), pointer   :: fieldList(:)
    integer                     :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(fields, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (fieldCount.eq.0) return

    allocate(fieldList(fieldCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of fieldList() failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call ESMF_FieldBundleGet(fields, fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    do i = 1,fieldCount
      call ESMF_FieldDestroy(fieldList(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo

    call ESMF_FieldBundleDestroy(fields, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    if (associated(fieldList)) deallocate(fieldList)

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldAdd(dstField, src1Field, src2Field, src1Fac, src2Fac, rc)
    type(ESMF_Field)             :: dstField
    type(ESMF_Field)             :: src1Field
    type(ESMF_Field)             :: src2Field
    real(ESMF_KIND_RX), optional :: src1Fac
    real(ESMF_KIND_RX), optional :: src2Fac
    integer,            optional :: rc

    ! local variables
    integer :: ldecnt, lde, i, j, k
    integer :: src1DimCount, src2DimCount, dstDimCount, ndim
    integer :: lb2(2), ub2(2)
    integer :: lb3(3), ub3(3)
    integer :: src1GridToFieldMap(2), src2GridToFieldMap(2)
    integer :: dstGridToFieldMap(2)
    type(ESMF_Grid) :: src1Grid, src2Grid, dstGrid
    type(ESMF_StaggerLoc) :: src1Stgr, src2Stgr, dstStgr
    real(ESMF_KIND_RX) :: s1fac, s2fac
    real(ESMF_KIND_RX), pointer :: s1ptr2(:,:),   s2ptr2(:,:),   dptr2(:,:)
    real(ESMF_KIND_RX), pointer :: s1ptr3(:,:,:), s2ptr3(:,:,:), dptr3(:,:,:)

    if (present(rc)) rc = ESMF_SUCCESS

    s1fac = 1
    if (present(src1Fac)) s1fac = src1Fac
    s2fac = 1
    if (present(src2Fac)) s2fac = src2Fac

    call ESMF_FieldGet(src1Field, localDECount=ldecnt, grid=src1Grid, &
      staggerLoc=src1Stgr, dimCount=src1DimCount, &
      gridToFieldMap=src1GridToFieldMap, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (src1DimCount.ne.2.and.src1DimCount.ne.3) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldAdd: src1 dimCount must be 2 or 3')
      return ! bail out
    endif

    call ESMF_FieldGet(src2Field, localDECount=ldecnt, grid=src2Grid, &
      staggerLoc=src2Stgr, dimCount=src2DimCount, &
      gridToFieldMap=src2GridToFieldMap, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (src2DimCount.ne.2.and.src2DimCount.ne.3) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldAdd: src2 dimCount must be 2 or 3')
      return ! bail out
    endif

    call ESMF_FieldGet(dstField, localDECount=ldecnt, grid=dstGrid, &
      staggerLoc=dstStgr, dimCount=dstDimCount, &
      gridToFieldMap=dstGridToFieldMap, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (dstDimCount.ne.2.and.dstDimCount.ne.3) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldAdd: dst dimCount must be 2 or 3')
      return ! bail out
    endif

    if (src1DimCount.ne.dstDimCount) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldAdd: src1 & dst dimCount must be equal')
      return ! bail out
    endif
    if (src2DimCount.ne.dstDimCount) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldAdd: src2 & dst dimCount must be equal')
      return ! bail out
    endif
    ndim = dstDimCount

    if (any(src1GridToFieldMap.ne.src2GridToFieldMap)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldAdd: src1 & src2 gridToFieldMap must be equal')
      return ! bail out
    endif
    if (any(src1GridToFieldMap.ne.dstGridToFieldMap)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldAdd: src1 & dst gridToFieldMap must be equal')
      return ! bail out
    endif
    if (any(src2GridToFieldMap.ne.dstGridToFieldMap)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldAdd: src2 & dst gridToFieldMap must be equal')
      return ! bail out
    endif

    do lde=0,ldecnt-1

      if (ndim.eq.2) then
        call ESMF_FieldGet(src1Field, localDE=lde, farrayPtr=s1ptr2, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        call ESMF_FieldGet(src2Field, localDE=lde, farrayPtr=s2ptr2, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        call ESMF_FieldGet(dstField, localDE=lde, farrayPtr=dptr2, &
             exclusiveLBound=lb2, exclusiveUBound=ub2, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        call ESMF_FieldGet(src1Field, localDE=lde, farrayPtr=s1ptr3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        call ESMF_FieldGet(src2Field, localDE=lde, farrayPtr=s2ptr3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        call ESMF_FieldGet(dstField, localDE=lde, farrayPtr=dptr3, &
             exclusiveLBound=lb3, exclusiveUBound=ub3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif

      if (ndim.eq.2) then

        dptr2(lb2(1):ub2(1),lb2(2):ub2(2)) = &
          s1fac*s1ptr2(lb2(1):ub2(1),lb2(2):ub2(2)) + &
          s2fac*s2ptr2(lb2(1):ub2(1),lb2(2):ub2(2))

      else

        dptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3)) = &
          s1fac*s1ptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3)) + &
          s2fac*s2ptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3))

      endif

    enddo

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldScale(field, scaleFac, addOffset, rc)
    type(ESMF_Field)   :: field
    real(ESMF_KIND_RX) :: scaleFac
    real(ESMF_KIND_RX) :: addOffset
    integer,  optional :: rc

    ! local variables
    integer :: ldecnt, lde, i, j, k
    integer :: dimCount, ndim
    integer :: lb2(2), ub2(2)
    integer :: lb3(3), ub3(3)
    type(ESMF_Grid) :: grid
    type(ESMF_StaggerLoc) :: stgr
    real(ESMF_KIND_RX), pointer :: ptr2(:,:)
    real(ESMF_KIND_RX), pointer :: ptr3(:,:,:)

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, localDECount=ldecnt, grid=grid, &
      staggerLoc=stgr, dimCount=dimCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (dimCount.ne.2.and.dimCount.ne.3) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldScale: dimCount must be 2 or 3')
      return ! bail out
    endif
    ndim = dimCount

    do lde=0,ldecnt-1

      if (ndim.eq.2) then
        call ESMF_FieldGet(field, localDE=lde, farrayPtr=ptr2, &
             exclusiveLBound=lb2, exclusiveUBound=ub2, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        call ESMF_FieldGet(field, localDE=lde, farrayPtr=ptr3, &
             exclusiveLBound=lb3, exclusiveUBound=ub3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif

      if (ndim.eq.2) then

        ptr2(lb2(1):ub2(1),lb2(2):ub2(2)) = &
          scaleFac*ptr2(lb2(1):ub2(1),lb2(2):ub2(2)) + addOffset

      else

        ptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3)) = &
          scaleFac*ptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3)) + addOffset

      endif

    enddo

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  function RemapStatusCheck(remapStatusField, statusToCheck, vm, rc) &
    result(numCells)
    type(ESMF_Field)       :: remapStatusField
    integer                :: statusToCheck
    type(ESMF_VM)          :: vm
    integer                :: rc
    integer                :: numCells

    ! local variables
    integer :: ldecnt, lde
    integer :: i, j, elb(2), eub(2)
    integer(ESMF_KIND_I4) :: ncntl, ncntg
    integer(ESMF_KIND_I4), pointer :: remapStatus(:,:)

    rc = ESMF_SUCCESS

    call ESMF_FieldGet(remapStatusField, localDECount=ldecnt, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ncntl = 0
    do lde=0,ldecnt-1
      call ESMF_FieldGet(remapStatusField, localDE=lde, farrayPtr=remapStatus, &
           exclusiveLBound=elb, exclusiveUBound=eub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      do j = elb(2),eub(2)
        do i = elb(1),eub(1)
          if (remapStatus(i,j).eq.statusToCheck) ncntl = ncntl+1
        enddo
      enddo
    enddo
    call ESMF_VMAllFullReduce(vm, (/ncntl/), ncntg, 1, ESMF_REDUCE_SUM, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    numCells = ncntg

  end function
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine RemapStatusWrite(field, cname, vm, rc)
    type(ESMF_Field)       :: field
    character(*)           :: cname
    type(ESMF_VM)          :: vm
    integer,      optional :: rc

    ! local variables
    integer                :: ndim
    character(ESMF_MAXSTR) :: fname
    type(ESMF_Array)       :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, name=fname, array=array, dimCount=ndim, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (ndim.ne.2) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='RemapStatusWrite: field dimCount must be 2')
      return ! bail out
    endif
    call ESMF_LogWrite(trim(cname)//': remapStatus: output: '//trim(fname), &
      ESMF_LOGMSG_INFO)

    call ArrayDebugWrite(array, vm, trim(fname), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine FieldDebugWrite(field, vm, suffix, rc)
    type(ESMF_Field)       :: field
    type(ESMF_VM)          :: vm
    character(*), optional :: suffix
    integer,      optional :: rc

    ! local variables
    integer                :: ndim
    character(ESMF_MAXSTR) :: fname
    type(ESMF_Array)       :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, name=fname, array=array, dimCount=ndim, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (ndim.ne.2) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FieldDebugWrite: field dimCount must be 2')
      return ! bail out
    endif

    if (present(suffix)) then
      call ArrayDebugWrite(array, vm, trim(fname)//'_'//trim(suffix), rc=rc)
    else
      call ArrayDebugWrite(array, vm, trim(fname), rc=rc)
    endif
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine ArrayDebugWrite(array, vm, aname, rc)
    type(ESMF_Array)       :: array
    type(ESMF_VM)          :: vm
    character(*)           :: aname
    integer,      optional :: rc

    ! local variables
    integer, parameter       :: rootPet = 0
    integer                  :: iunit, nPets, lPet, i, j
    integer                  :: glb(2), gub(2), minIdx(2,1), maxIdx(2,1)
    type(ESMF_TypeKind_Flag) :: tk
    integer(ESMF_KIND_I4), pointer :: iptr(:,:)
    real(ESMF_KIND_RX),    pointer :: rptr(:,:)
    integer                  :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_VMGet(vm, petCount=nPets, localPet=lPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    call ESMF_ArrayGet(array, typeKind=tk, &
      minIndexPTile=minIdx, maxIndexPTile=maxIdx, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    glb(1) = minIdx(1,1)
    gub(1) = maxIdx(1,1)
    glb(2) = minIdx(2,1)
    gub(2) = maxIdx(2,1)

    if (lPet.eq.rootPet) then
      if (tk.eq.ESMF_TYPEKIND_I4) then
        allocate(iptr(glb(1):gub(1),glb(2):gub(2)), stat=stat)
      else
        allocate(rptr(glb(1):gub(1),glb(2):gub(2)), stat=stat)
      endif
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg='Allocation of global array failed.', &
        CONTEXT)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    if (tk.eq.ESMF_TYPEKIND_I4) then
      call ESMF_ArrayGather(array, iptr, rootPet, tile=1, vm=vm, rc=rc)
    else
      call ESMF_ArrayGather(array, rptr, rootPet, tile=1, vm=vm, rc=rc)
    endif
    if (ESMF_LogFoundError(rc, PASSTHRU, rcToReturn=rc)) return ! bail out

    if (lPet.eq.rootPet) then
      call ESMF_UtilIOUnitGet(iunit, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      open (unit=iunit, file=trim(aname), &
        form='formatted', action='write', status='replace')
      if (aname(1:11).eq.'remapStatus') then
        write(iunit,'(a)') '------------------------------'
        write(iunit,'(a)') trim(aname)//' flags:'
        write(iunit,'(a,i2)') '     NOT_SET: ',REMAP_STATUS_NOT_SET
        write(iunit,'(a,i2)') '  MASKED_DST: ',REMAP_STATUS_MASKED_DST
        write(iunit,'(a,i2)') '  MASKED_SRC: ',REMAP_STATUS_MASKED_SRC
        write(iunit,'(a,i2)') '  NOT_IN_SRC: ',REMAP_STATUS_NOT_IN_SRC
        write(iunit,'(a,i2)') '   VALID_SRC: ',REMAP_STATUS_VALID_SRC
        write(iunit,'(a,i2)') ' EXTEND_FILL: ',REMAP_STATUS_EXTEND_FILL
        write(iunit,'(a)') '------------------------------'
      endif
      if (tk.eq.ESMF_TYPEKIND_I4) then
        do j = gub(2),glb(2),-1
          write(iunit,'(8000i1)') (iptr(i,j),i=glb(1),gub(1))
        enddo
      else
        do j = gub(2),glb(2),-1
          write(iunit,'(8000i1)') (nint(rptr(i,j)),i=glb(1),gub(1))
        enddo
      endif
      close (iunit)
      if (tk.eq.ESMF_TYPEKIND_I4) then
        deallocate(iptr, stat=stat)
      else
        deallocate(rptr, stat=stat)
      endif
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg='Deallocation of global array failed.', &
        CONTEXT)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
    call ESMF_VMBarrier(vm)

  end subroutine
  !-----------------------------------------------------------------------------

  ! Public method --------------------------------------------------------------
  subroutine PrintTimers(compName, wtnam, wtcnt, wtime)
    character(*)          :: compName
    character(*)          :: wtnam(:)
    integer(ESMF_KIND_I4) :: wtcnt(:)
    real(ESMF_KIND_R8)    :: wtime(:)

    ! local variables
    character(ESMF_MAXSTR) :: msg
    integer(ESMF_KIND_I4)  :: k

    write(msg,1) trim(compName),'timer','count','time'
    call ESMF_LogWrite(TRIM(msg), ESMF_LOGMSG_INFO)
    do k=lbound(wtcnt,1),ubound(wtcnt,1)
      write(msg,2) trim(compName),trim(wtnam(k)),wtcnt(k),wtime(k)
      call ESMF_LogWrite(TRIM(msg), ESMF_LOGMSG_INFO)
    enddo

1   format(a,': wtime: ',a20,a10,a14)
2   format(a,': wtime: ',a20,i10,e14.6)

  end subroutine
  !-----------------------------------------------------------------------------


  !=============================================================================
  ! Private Methods
  !=============================================================================

  ! Private method -------------------------------------------------------------
!BOPI
! !IROUTINE: GetRemapSMM - Compute a remap sparse matrix object
! !INTERFACE:
  subroutine GetRemapSMM(srcField, dstField, smm, vm, mname, &
    remapType, srcMaskValues, dstMaskValues, remapStatusField, cname, rc)
! !ARGUMENTS:
    type(ESMF_Field)                :: srcField
    type(ESMF_Field)                :: dstField
    type(RemapSMM)                  :: smm
    type(ESMF_VM)                   :: vm
    character(*)                    :: mname
    character(6),          optional :: remapType
    integer(ESMF_KIND_I4), optional :: srcMaskValues(:)
    integer(ESMF_KIND_I4), optional :: dstMaskValues(:)
    type(ESMF_Field),      optional :: remapStatusField
    character(*),          optional :: cname
    integer,               optional :: rc
! !DESCRIPTION:
!   insert description here
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    integer, parameter :: localDE=0
    logical, parameter :: ijg=.true.
    integer, parameter :: iclo=iclo_none, ncb=10
    real(8), parameter :: dcin=1d-1
    real(8), parameter ::  eps=1d-5
    logical :: llg
    character(ESMF_MAXSTR) :: msgString
    character(ESMF_MAXSTR) :: fname, gname
    character(6) :: rmptyp
    logical :: ingrid, lcomp
    integer :: localrc, stat
    integer :: nPets, lPet, nfac, nmap, mskc, istep
    integer :: m, n, i, j, k, ij, ii, jj
    integer :: ns
    integer, pointer :: is(:), js(:), ijs(:)
    real(8), pointer :: cs(:)
    real(8) :: lon, lat, ir, jr
    integer :: minIdx(2,1), maxIdx(2,1)
    integer :: srcGridDim, dstGridDim
    type(ESMF_Grid) :: srcGrid, dstGrid
    type(ESMF_Array) :: srcMsk, dstMsk
    type(ESMF_StaggerLoc) :: srcStgr, dstStgr
    type(ESMF_Index_Flag) :: srcIndexFlag, dstIndexFlag
    type(ESMF_CoordSys_Flag) :: srcCoordSysFlag, dstCoordSysFlag
    type(ESMF_Index_Flag) :: remapStatusIndexFlag
    type(ESMF_Array) :: dstLon, dstLat, dstArray
    real(ESMF_KIND_RX), pointer :: lon_d(:,:), lat_d(:,:)
    integer(ESMF_KIND_I4), pointer :: msk_d(:,:)
    integer(ESMF_KIND_I4), pointer :: remapStatus(:,:)
    logical, pointer :: lmsk_d(:,:)
    type(ESMF_Array) :: srcLon, srcLat
    real(ESMF_KIND_RX), pointer :: lon_s(:,:), lat_s(:,:)
    integer(ESMF_KIND_I4), pointer :: msk_s(:,:)
    logical, pointer :: lmsk_s(:,:)
    type(t_gsu) :: gsu_s
    integer, allocatable :: ix(:), iy(:)
    integer :: dstLdeCount
    integer :: glb_s(2), gub_s(2), nx_s, ny_s
    integer :: glb_d(2), gub_d(2), nx_d, ny_d
    integer :: elb_d(2), eub_d(2)
    integer(ESMF_KIND_I4) :: nfacl, nfacg

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(remapType)) then
      rmptyp = remapType
      select case (rmptyp)
      case ('redist')
      case ('bilinr')
      case ('bicubc')
      case default
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(mname)//': unsupported remapType option: '//rmptyp)
        return ! bail out
      endselect
    else
      rmptyp = 'bilinr'
    endif

    ! get vm info
    call ESMF_VMGet(vm, petCount=nPets, localPet=lPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get local DE count for destination field
    call ESMF_FieldGet(dstField, localDECount=dstLdeCount, name=fname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (dstLdeCount.gt.1) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': destination localDECount > 1 is not supported')
      return ! bail out
    endif

    ! get grid, stagger, and index flag for destination field
    call ESMF_FieldGet(dstField, grid=dstGrid, staggerLoc=dstStgr, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGet(dstGrid, indexFlag=dstIndexFlag, dimCount=dstGridDim, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (dstGridDim.ne.2) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': destination grid dimCount != 2 is not supported')
      return ! bail out
    endif
    if (dstIndexFlag.eq.ESMF_INDEX_USER) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': destination indexFlag = ESMF_INDEX_USER is not supported')
      return ! bail out
    endif

    ! index flag for remapStatus
    remapStatusIndexFlag = dstIndexFlag

    ! get grid, stagger, and index flag for source field
    call ESMF_FieldGet(srcField, grid=srcGrid, staggerLoc=srcStgr, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGet(srcGrid, indexFlag=srcIndexFlag, dimCount=srcGridDim, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (srcGridDim.ne.2) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': source grid dimCount != 2 is not supported')
      return ! bail out
    endif
    if (srcIndexFlag.eq.ESMF_INDEX_USER) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': source indexFlag = ESMF_INDEX_USER is not supported')
      return ! bail out
    endif

    ! check/set coordSys flag
    call ESMF_GridGet(dstGrid, coordSys=dstCoordSysFlag, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGet(srcGrid, coordSys=srcCoordSysFlag, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (srcCoordSysFlag.ne.dstCoordSysFlag) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': source coordSys != destination coordSys is not supported')
      return ! bail out
    endif
    if (srcCoordSysFlag.eq.ESMF_COORDSYS_SPH_RAD) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': coordSys = ESMF_COORDSYS_SPH_RAD is not supported')
      return ! bail out
    endif
    llg = srcCoordSysFlag.eq.ESMF_COORDSYS_SPH_DEG

    ! setup bounds for destination exclusive region
    ! (** bounds are in global index space **)
    elb_d = 0;  eub_d = 0;
    if (dstLdeCount.ne.0) then
      call ESMF_FieldGet(dstField, array=dstArray, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_ArrayGet(dstArray, 1, localDe=localDe, indexCount=m, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      allocate(ix(m), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ArrayGet(dstArray, 1, localDe=localDe, indexList=ix, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ArrayGet(dstArray, 2, localDe=localDe, indexCount=n, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      allocate(iy(n), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ArrayGet(dstArray, 2, localDe=localDe, indexList=iy, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      elb_d(1) = ix(1)
      eub_d(1) = ix(m)
      elb_d(2) = iy(1)
      eub_d(2) = iy(n)
      deallocate(ix,iy)
    endif !dstLdeCount.ne.0

    ! setup grid coordinates and land/sea mask for destination field
    call ESMF_GridGetCoord(dstGrid,coordDim=1,staggerLoc=dstStgr,array=dstLon,rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetCoord(dstGrid,coordDim=2,staggerLoc=dstStgr,array=dstLat,rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetItem(dstGrid, itemFlag=ESMF_GRIDITEM_MASK, &
      staggerLoc=dstStgr, array=dstMsk, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ArrayGet(dstMsk, minIndexPTile=minIdx, maxIndexPTile=maxIdx, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    glb_d(1) = minIdx(1,1)
    gub_d(1) = maxIdx(1,1)
    glb_d(2) = minIdx(2,1)
    gub_d(2) = maxIdx(2,1)
    nx_d = gub_d(1)-glb_d(1)+1
    ny_d = gub_d(2)-glb_d(2)+1
    if (present(remapStatusField)) then
      if (present(cname)) then
        remapStatusField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_I4, &
          indexFlag=remapStatusIndexFlag, staggerLoc=dstStgr, &
          name='remapStatus_'//trim(cname), rc=rc)
      else
        remapStatusField = ESMF_FieldCreate(dstGrid, ESMF_TYPEKIND_I4, &
          indexFlag=remapStatusIndexFlag, staggerLoc=dstStgr, &
          name='remapStatus', rc=rc)
      endif
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif
    if (dstLdeCount.ne.0) then
      call ESMF_ArrayGet(dstLon, localDE=localDe, farrayPtr=lon_d, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ArrayGet(dstLat, localDE=localDe, farrayPtr=lat_d, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ArrayGet(dstMsk, localDE=localDe, farrayPtr=msk_d, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      allocate(lmsk_d(elb_d(1):eub_d(1),elb_d(2):eub_d(2)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=trim(mname)//': Allocation of dst arrays failed.', &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (present(remapStatusField)) then
        call ESMF_FieldGet(remapStatusField,localDE=localDe,farrayPtr=remapStatus,rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      do jj=elb_d(2),eub_d(2)
        do ii=elb_d(1),eub_d(1)
          if (dstIndexFlag.eq.ESMF_INDEX_DELOCAL) then
            i = ii-elb_d(1)+1
            j = jj-elb_d(2)+1
          else
            i = ii
            j = jj
          endif
          lmsk_d(ii,jj) = .false.
          if (present(remapStatusField)) then
            if (remapStatusIndexFlag.eq.ESMF_INDEX_DELOCAL) then
              remapStatus(i ,j ) = REMAP_STATUS_NOT_SET
            else
              remapStatus(ii,jj) = REMAP_STATUS_NOT_SET
            endif
          endif
          if (present(dstMaskValues)) then
            dk_loop: do k=lbound(dstMaskValues,1),ubound(dstMaskValues,1)
              if (msk_d(i,j).eq.dstMaskValues(k)) then
                lmsk_d(ii,jj) = .true.
                if (present(remapStatusField)) then
                  if (remapStatusIndexFlag.eq.ESMF_INDEX_DELOCAL) then
                    remapStatus(i ,j ) = REMAP_STATUS_MASKED_DST
                  else
                    remapStatus(ii,jj) = REMAP_STATUS_MASKED_DST
                  endif
                endif
                exit dk_loop
            endif
          enddo dk_loop
          endif
        enddo
      enddo
    endif !dstLdeCount.ne.0

    ! setup grid coordinates and land/sea mask for source field
    call ESMF_GridGetCoord(srcGrid,coordDim=1,staggerLoc=srcStgr,array=srcLon,rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetCoord(srcGrid,coordDim=2,staggerLoc=srcStgr,array=srcLat,rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetItem(srcGrid, itemFlag=ESMF_GRIDITEM_MASK, &
      staggerLoc=srcStgr, array=srcMsk, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ArrayGet(srcMsk, minIndexPTile=minIdx, maxIndexPTile=maxIdx, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    glb_s(1) = minIdx(1,1)
    gub_s(1) = maxIdx(1,1)
    glb_s(2) = minIdx(2,1)
    gub_s(2) = maxIdx(2,1)
    nx_s = gub_s(1)-glb_s(1)+1
    ny_s = gub_s(2)-glb_s(2)+1
    allocate(lmsk_s(glb_s(1):gub_s(1),glb_s(2):gub_s(2)), &
              msk_s(glb_s(1):gub_s(1),glb_s(2):gub_s(2)), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=trim(mname)//': Allocation of src mask arrays failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    if (rmptyp.ne.'redist') then
      allocate(lon_s(glb_s(1):gub_s(1),glb_s(2):gub_s(2)), &
               lat_s(glb_s(1):gub_s(1),glb_s(2):gub_s(2)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=trim(mname)//': Allocation of src lon/lat arrays failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif
    do k=0,nPets-1
      if (rmptyp.ne.'redist') then
        call ESMF_ArrayGather(srcLon, lon_s, k, tile=1, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        call ESMF_ArrayGather(srcLat, lat_s, k, tile=1, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      call ESMF_ArrayGather(srcMsk, msk_s, k, tile=1, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    enddo
    do jj=glb_s(2),gub_s(2)
      do ii=glb_s(1),gub_s(1)
        lmsk_s(ii,jj) = .false.
        if (present(srcMaskValues)) then
          sk_loop: do k=lbound(srcMaskValues,1),ubound(srcMaskValues,1)
            if (msk_s(ii,jj).eq.srcMaskValues(k)) then
              lmsk_s(ii,jj) = .true.
              exit sk_loop
            endif
          enddo sk_loop
        endif
      enddo
    enddo

    ! create source grid search utility object
    if (rmptyp.ne.'redist') then
      gsu_s=w3gsuc(ijg, llg, iclo, lon_s, lat_s, ncb=ncb)
    else
      if (nx_d.ne.nx_s.or.ny_d.ne.ny_s) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(mname)//': redist requires source and dest grids to be the same size')
        write(msgString,'(a,2i8)') trim(mname)//': source grid dimensions: ',nx_s,ny_s
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
        write(msgString,'(a,2i8)') trim(mname)//': dest   grid dimensions: ',nx_d,ny_d
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
        return ! bail out
      endif
    endif

    ! compute remap factors (istep=1: count; istep=2: compute)
    step_loop: do istep=1,2
      lcomp=istep.eq.2
      if (istep.eq.2) then
        allocate(smm%facList(nfac), smm%facIdxList(2,nfac), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg=trim(mname)//': Allocation of factor list failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        smm%facList = 0.0
        smm%facIdxList = 0
      endif
      nfac=0
      nmap=0
      if (dstLdeCount.ne.0) then
        ! loop over destination points (ii & jj are global indices)
        jj_loop: do jj=elb_d(2),eub_d(2)
        ii_loop: do ii=elb_d(1),eub_d(1)
          if (lmsk_d(ii,jj)) cycle ii_loop
          if (dstIndexFlag.eq.ESMF_INDEX_DELOCAL) then
            i = ii-elb_d(1)+1
            j = jj-elb_d(2)+1
          else
            i = ii
            j = jj
          endif
          ij=(jj-glb_d(2))*nx_d+ii-glb_d(1)+1
          if (rmptyp.ne.'redist') then
            lon = lon_d(i,j)
            lat = lat_d(i,j)
            ! w3gfij will flag points as inside the grid (ingrid=t) that are outside the grid,
            ! but within "dcin" grid cell width distance of the grid
            ingrid = w3gfij(gsu_s, lon, lat, ir, jr, eps=eps, dcin=dcin)
            if (ingrid) then
              ! getrmp will flag points as inside the grid (ns>=0) that are outside the grid,
              ! but within "dcin" grid cell width distance of the grid
              call getrmp(lcomp,rmptyp,eps,dcin,glb_s,gub_s,lmsk_s,ir,jr,ns,is,js,ijs,cs)
            else
              ns = -1
            endif
          else
            if (lmsk_s(ii,jj)) then
              ns = 0
            else
              ns = 1
              allocate (is(ns),js(ns),ijs(ns),cs(ns))
              is(1) = ii
              js(1) = jj
              ijs(1)= ij
              cs(1) = 1d0
            endif
          endif
          if (ns.gt.0) then
            nmap = nmap + 1
            if (istep.eq.1) then
              nfac = nfac + ns
            else
              do k=1,ns
                nfac = nfac + 1
                smm%facList(nfac) = cs(k)
                smm%facIdxList(1,nfac) = ijs(k)
                smm%facIdxList(2,nfac) = ij
              enddo
              if (present(remapStatusField)) then
                if (remapStatusIndexFlag.eq.ESMF_INDEX_DELOCAL) then
                  remapStatus(i ,j ) = REMAP_STATUS_VALID_SRC
                else
                  remapStatus(ii,jj) = REMAP_STATUS_VALID_SRC
                endif
              endif
            endif
            deallocate (is,js,ijs,cs)
          elseif (ns.eq.0) then
            if (istep.eq.2) then
              if (present(remapStatusField)) then
                if (remapStatusIndexFlag.eq.ESMF_INDEX_DELOCAL) then
                  remapStatus(i ,j ) = REMAP_STATUS_MASKED_SRC
                else
                  remapStatus(ii,jj) = REMAP_STATUS_MASKED_SRC
                endif
              endif
            endif
          else
            if (istep.eq.2) then
              if (present(remapStatusField)) then
                if (remapStatusIndexFlag.eq.ESMF_INDEX_DELOCAL) then
                  remapStatus(i ,j ) = REMAP_STATUS_NOT_IN_SRC
                else
                  remapStatus(ii,jj) = REMAP_STATUS_NOT_IN_SRC
                endif
              endif
            endif
          endif
        enddo ii_loop
        enddo jj_loop
      endif !dstLdeCount.ne.0
    enddo step_loop
    nfacl = nfac
    call ESMF_VMAllFullReduce(vm, (/nfacl/), nfacg, 1, ESMF_REDUCE_SUM, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_VMBarrier(vm)
    if (present(cname)) then
      write(msgString,'(a,2(a,i10))') trim(cname)//': '//trim(mname)//': ', &
        'numLocFac = ',nfacl,', numGlbFac = ',nfacg
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    endif

    ! free memory for dst grid
    if (dstLdeCount.ne.0) then
      deallocate(lmsk_d, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(mname)//': Deallocation of dst arrays failed.', &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif !dstLdeCount.ne.0

    ! free memory for src grid
    call w3gsud(gsu_s)
    deallocate(lmsk_s, msk_s, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=trim(mname)//': Deallocation of src mask arrays failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    if (rmptyp.ne.'redist') then
      deallocate(lon_s, lat_s, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(mname)//': Deallocation of src lon/lat arrays failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  ! Private method -------------------------------------------------------------
!BOPI
! !IROUTINE: GetExtendSMM - Compute an extend sparse matrix object
! !INTERFACE:
  subroutine GetExtendSMM(srcField, smm, vm, mname, &
    remapStatusField, niter, mskval, full, cname, rc)
! !ARGUMENTS:
    type(ESMF_Field)                 :: srcField
    type(RemapSMM), pointer          :: smm(:)
    type(ESMF_VM)                    :: vm
    character(*)                     :: mname
    type(ESMF_Field),       optional :: remapStatusField
    integer,                optional :: niter
    integer(ESMF_KIND_I4),  optional :: mskval(:)
    logical,                optional :: full
    character(*),           optional :: cname
    integer,                optional :: rc
! !DESCRIPTION:
!   insert description here
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    integer, parameter :: localDE=0
    real(ESMF_KIND_RX), parameter :: zero=0.0, one=1.0
    integer, parameter :: maxiter=100
    integer, parameter :: nlvl=2
    integer, parameter :: nnbr=(2*nlvl+1)**2
    character(ESMF_MAXSTR) :: msgString
    logical :: fullStencil
    integer :: i, j, k, l, m, n, iter, stat
    integer :: ii, jj, kk, ll
    integer :: dimg, ldecnt, lde, lPet
    integer :: elb(2), eub(2)
    character(ESMF_MAXSTR) :: fname
    character(3)           :: suffix
    type(ESMF_Field)       :: fieldBmsk
    type(ESMF_Field)       :: fieldCmsk
    type(ESMF_RouteHandle) :: haloRH
    integer(ESMF_KIND_I4), pointer :: bmsk(:,:) => null()
    integer(ESMF_KIND_I4), pointer :: cmsk(:,:) => null()
    integer(ESMF_KIND_I4), pointer :: amsk(:,:) => null()
    integer(ESMF_KIND_I4), pointer :: remapStatus(:,:) => null()
    type(ESMF_Grid) :: grid
    type(ESMF_Array) :: array, arrayMask
    type(ESMF_StaggerLoc) :: stgr
    type(ESMF_Index_Flag) :: iflag
    integer :: hlw(2)=(/nlvl,nlvl/), huw(2)=(/nlvl,nlvl/)
    integer :: minIdx(2,1), maxIdx(2,1)
    integer :: glb(2), gub(2), nx, ny
    integer, allocatable :: ix(:), iy(:)
    integer :: nfac(maxiter), istep, ij, ns, is(nnbr), js(nnbr)
    integer(ESMF_KIND_I4) :: ncntl, ncntg
    integer(ESMF_KIND_I4) :: nfacl, nfacg
    integer :: nlcnt(maxiter), ngcnt(maxiter)
    integer :: nlfac(maxiter), ngfac(maxiter)
    real(ESMF_KIND_RX) :: cs(nnbr)

    if (present(rc)) rc = ESMF_SUCCESS

    ! trap unsupported args
    if (present(remapStatusField).and.present(niter)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(mname)//': remapStatusField and niter cannot both be specified')
      return ! bail out
    endif
    if (present(niter)) then
      if (niter.gt.maxiter) then
        write(msgString,'(a,2i6)') trim(mname)//': niter > maxiter: ',niter,maxiter
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
        return ! bail out
      endif
      if (.not.present(mskval)) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(mname)//': niter and mskval must be specified together')
        return ! bail out
      endif
    endif

    ! set full stencil flag
    if (present(full)) then
      fullStencil = full
    else
      fullStencil = .true.
    endif

    ! get VM info
    call ESMF_VMGet(vm, localPet=lPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get required attributes from field
    call ESMF_FieldGet(srcField, grid=grid, array=array, &
      staggerLoc=stgr, localDECount=ldecnt, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGet(grid, indexFlag=iflag, dimCount=dimg, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (present(niter)) then
      call ESMF_GridGetItem(grid, itemFlag=ESMF_GRIDITEM_MASK, &
        staggerLoc=stgr, array=arrayMask, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

    ! error traps
    if (dimg.ne.2) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': grid dimCount != 2 is not supported')
      return ! bail out
    endif
    if (iflag.eq.ESMF_INDEX_USER) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': indexFlag = ESMF_INDEX_USER is not supported')
      return ! bail out
    endif
    if (ldecnt.gt.1) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg=trim(mname)//': localDECount > 1 is not supported')
      return ! bail out
    endif

    ! get global bounds
    call ESMF_ArrayGet(array, minIndexPTile=minIdx, maxIndexPTile=maxIdx, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    glb(1) = minIdx(1,1)
    gub(1) = maxIdx(1,1)
    glb(2) = minIdx(2,1)
    gub(2) = maxIdx(2,1)
    nx = gub(1)-glb(1)+1
    ny = gub(2)-glb(2)+1

    ! get local bounds (global index)
    elb = 0;  eub = 0;
    if (ldecnt.ne.0) then
      call ESMF_ArrayGet(array, 1, localDe=localDe, indexCount=m, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      allocate(ix(m), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ArrayGet(array, 1, localDe=localDe, indexList=ix, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ArrayGet(array, 2, localDe=localDe, indexCount=n, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      allocate(iy(n), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_ArrayGet(array, 2, localDe=localDe, indexList=iy, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      elb(1) = ix(1)
      eub(1) = ix(m)
      elb(2) = iy(1)
      eub(2) = iy(n)
      deallocate(ix,iy)
    endif !ldecnt.ne.0

    ! create mask fields with halo required for extend iterations
    if (present(cname)) then
      fname = 'extendMask_'//trim(cname)
    else
      fname = 'extendMask'
    endif
    fieldBmsk = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I4, staggerLoc=stgr, &
      indexFlag=iflag, totalLWidth=hlw, totalUWidth=huw, name=trim(fname), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    fieldCmsk = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I4, staggerLoc=stgr, &
      indexFlag=iflag, totalLWidth=hlw, totalUWidth=huw, name=trim(fname), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! create halo route handle
    call ESMF_FieldHaloStore(fieldBmsk, routeHandle=haloRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! get data pointers
    if (ldecnt.ne.0) then
      call ESMF_FieldGet(fieldBmsk, localDE=localDE, farrayPtr=bmsk, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_FieldGet(fieldCmsk, localDE=localDE, farrayPtr=cmsk, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (present(remapStatusField)) then
        call ESMF_FieldGet(remapStatusField, localDE=localDe, &
          farrayPtr=remapStatus, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      if (present(niter)) then
        call ESMF_ArrayGet(arrayMask, localDE=localDE, farrayPtr=amsk, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    endif !ldecnt.ne.0

    ! report
    if (present(cname)) then
      write(msgString,'(a,a4,4(a,a10))') trim(cname)//': '//trim(mname)//': ', &
        'iter',' ','numLocExt',' ','numGlbExt',' ','numLocFac',' ','numGlbFac'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    endif

    ! two step loop:
    ! step 1: count number of iterations and number of factors
    ! step 2: allocate arrays, compute factors and store route handles
    istep_loop: do istep=1,2

    ! exit if nothing to extend
    if (istep.eq.2) then
      if (iter.eq.0) exit istep_loop
    endif

    ! allocate SMM array
    if (istep.eq.2) then
      allocate(smm(iter), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=trim(mname)//': Allocation of SMM array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
      do k=1,iter
        allocate(smm(k)%facList(nfac(k)), smm(k)%facIdxList(2,nfac(k)), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg=trim(mname)//': Allocation of SMM factor list failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        smm(k)%facList = zero
        smm(k)%facIdxList = 0
      enddo
    endif

    ! initialize work arrays
    ncntl = 0
    if (ldecnt.ne.0) then
      do jj=elb(2),eub(2)
        do ii=elb(1),eub(1)
          if (iflag.eq.ESMF_INDEX_DELOCAL) then
            i = ii-elb(1)+1
            j = jj-elb(2)+1
          else
            i = ii
            j = jj
          endif
          if (present(remapStatusField)) then
            cmsk(i,j) = remapStatus(i,j)
            if (cmsk(i,j).eq.REMAP_STATUS_MASKED_SRC) then
              ncntl = ncntl + 1
            endif
          else  !(present(niter))
            cmsk(i,j) = 1
            k_loop: do k=lbound(mskval,1),ubound(mskval,1)
              if (amsk(i,j).eq.mskval(k)) then
                cmsk(i,j) = 0
                ncntl = ncntl + 1
                exit k_loop
              endif
            enddo k_loop
          endif !(present(niter))
        enddo !ii
      enddo !jj
    endif !ldecnt.ne.0
    call ESMF_VMAllFullReduce(vm, (/ncntl/), ncntg, 1, ESMF_REDUCE_SUM, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! iterate to extend values for dst points mapped from masked src cells
    ! bmsk = mask indicating values that were filled as of previous iteration.
    ! cmsk = mask indicating values that have been filled.
    iter = 0
    iter_loop: do while (ncntg.ne.0)
      iter = iter + 1
      if (present(niter)) then
        if (iter.gt.niter) exit iter_loop
      endif
      if (iter.gt.maxiter) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(mname)//': ExtendStore: iter > maxiter!!!')
        return ! bail out
      endif
      ncntl = 0
      nfacl = 0

      ! update bmsk
      call ESMF_FieldCopy(fieldBmsk, fieldCmsk, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

      ! update halos
      call ESMF_FieldHalo(fieldBmsk, haloRH, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
#ifdef DEBUG_GET_EXTEND_SMM
      if (istep.eq.1) then
        write(suffix,'(i3.3)') iter
        call FieldDebugWrite(fieldBmsk, vm, suffix=suffix, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
#endif

      ! extend (exclusive points, ii & jj are global indices)
      if (ldecnt.ne.0) then
        jj_loop: do jj=elb(2),eub(2)
        ii_loop: do ii=elb(1),eub(1)
          if (iflag.eq.ESMF_INDEX_DELOCAL) then
            i = ii-elb(1)+1
            j = jj-elb(2)+1
          else
            i = ii
            j = jj
          endif

          if (present(remapStatusField)) then

            ! skip masked dst points
            if (cmsk(i,j).eq.REMAP_STATUS_MASKED_DST) cycle ii_loop
            ! extend data to dst points remapped from masked src
            if (cmsk(i,j).eq.REMAP_STATUS_MASKED_SRC) then
              ! sequential index for (ii,jj)
              ij=(jj-glb(2))*nx+ii-glb(1)+1
              ! check neighboring points and collect sums
              ns = 0
              if (fullStencil) then
                ! neighboring points include diagonal
                ! (kk & ll are global indices)
                do ll=max(glb(2),jj-nlvl),min(gub(2),jj+nlvl),1
                do kk=max(glb(1),ii-nlvl),min(gub(1),ii+nlvl),1
                  if (iflag.eq.ESMF_INDEX_DELOCAL) then
                    k = kk-elb(1)+1
                    l = ll-elb(2)+1
                  else
                    k = kk
                    l = ll
                  endif
                  if (bmsk(k,l).eq.REMAP_STATUS_VALID_SRC .or. &
                      bmsk(k,l).eq.REMAP_STATUS_EXTEND_FILL) then
                     ns = ns + 1
                     is(ns) = kk
                     js(ns) = ll
                  endif
                enddo !kk
                enddo !ll
              else !.not.fullStencil
                ! neighboring points do not include diagonal
                ! (kk & ll are global indices)
                ll = jj
                do kk=max(glb(1),ii-nlvl),min(gub(1),ii+nlvl),2
                  if (iflag.eq.ESMF_INDEX_DELOCAL) then
                    k = kk-elb(1)+1
                    l = ll-elb(2)+1
                  else
                    k = kk
                    l = ll
                  endif
                  if (bmsk(k,l).eq.REMAP_STATUS_VALID_SRC .or. &
                      bmsk(k,l).eq.REMAP_STATUS_EXTEND_FILL) then
                     ns = ns + 1
                     is(ns) = kk
                     js(ns) = ll
                  endif
                enddo !kk
                kk = ii
                do ll=max(glb(2),jj-nlvl),min(gub(2),jj+nlvl),2
                  if (iflag.eq.ESMF_INDEX_DELOCAL) then
                    k = kk-elb(1)+1
                    l = ll-elb(2)+1
                  else
                    k = kk
                    l = ll
                  endif
                  if (bmsk(k,l).eq.REMAP_STATUS_VALID_SRC .or. &
                      bmsk(k,l).eq.REMAP_STATUS_EXTEND_FILL) then
                     ns = ns + 1
                     is(ns) = kk
                     js(ns) = ll
                  endif
                enddo !ll
              endif !.not.fullStencil
              ! set as mean of data from surrounding valid points
              if (ns.gt.0) then
                ncntl = ncntl + 1
                cmsk(i,j) = REMAP_STATUS_EXTEND_FILL
                if (istep.eq.1) then
                  nfacl = nfacl + ns
                else
                  remapStatus(i,j) = REMAP_STATUS_EXTEND_FILL
                  do k=1,ns
                    nfacl = nfacl + 1
                    smm(iter)%facList(nfacl) = one/real(ns,ESMF_KIND_RX)
                    smm(iter)%facIdxList(1,nfacl) = (js(k)-glb(2))*nx+is(k)-glb(1)+1
                    smm(iter)%facIdxList(2,nfacl) = ij
                  enddo !k
                endif
              endif !ns.gt.0
            endif !(cmsk(i,j).eq.REMAP_STATUS_MASKED_SRC)

          else  !(present(niter))

            ! extend data from unmasked to masked
            if (cmsk(i,j).eq.0) then
              ! sequential index for (ii,jj)
              ij=(jj-glb(2))*nx+ii-glb(1)+1
              ! check neighboring points and collect sums
              ns = 0
              if (fullStencil) then
                ! neighboring points include diagonal
                ! (kk & ll are global indices)
                do ll=max(glb(2),jj-nlvl),min(gub(2),jj+nlvl),1
                do kk=max(glb(1),ii-nlvl),min(gub(1),ii+nlvl),1
                  if (iflag.eq.ESMF_INDEX_DELOCAL) then
                    k = kk-elb(1)+1
                    l = ll-elb(2)+1
                  else
                    k = kk
                    l = ll
                  endif
                  if (bmsk(k,l).eq.1) then
                     ns = ns + 1
                     is(ns) = kk
                     js(ns) = ll
                  endif
                enddo !kk
                enddo !ll
              else !.not.fullStencil
                ! neighboring points do not include diagonal
                ! (kk & ll are global indices)
                ll = jj
                do kk=max(glb(1),ii-nlvl),min(gub(1),ii+nlvl),2
                  if (iflag.eq.ESMF_INDEX_DELOCAL) then
                    k = kk-elb(1)+1
                    l = ll-elb(2)+1
                  else
                    k = kk
                    l = ll
                  endif
                  if (bmsk(k,l).eq.1) then
                     ns = ns + 1
                     is(ns) = kk
                     js(ns) = ll
                  endif
                enddo !kk
                kk = ii
                do ll=max(glb(2),jj-nlvl),min(gub(2),jj+nlvl),2
                  if (iflag.eq.ESMF_INDEX_DELOCAL) then
                    k = kk-elb(1)+1
                    l = ll-elb(2)+1
                  else
                    k = kk
                    l = ll
                  endif
                  if (bmsk(k,l).eq.1) then
                     ns = ns + 1
                     is(ns) = kk
                     js(ns) = ll
                  endif
                enddo !ll
              endif !.not.fullStencil
              ! set as mean of data from surrounding valid points
              if (ns.gt.0) then
                ncntl = ncntl + 1
                cmsk(i,j) = 1
                if (istep.eq.1) then
                  nfacl = nfacl + ns
                else
                  do k=1,ns
                    nfacl = nfacl + 1
                    smm(iter)%facList(nfacl) = one/real(ns,ESMF_KIND_RX)
                    smm(iter)%facIdxList(1,nfacl) = (js(k)-glb(2))*nx+is(k)-glb(1)+1
                    smm(iter)%facIdxList(2,nfacl) = ij
                  enddo !k
                endif
              endif !ns.gt.0
            endif !(cmsk(i,j).eq.0)

          endif !(present(niter))

        enddo ii_loop
        enddo jj_loop
      endif !ldecnt.ne.0
      nfac(iter) = nfacl

      ! update global extend count
      call ESMF_VMAllFullReduce(vm, (/ncntl/), ncntg, 1, ESMF_REDUCE_SUM, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_VMAllFullReduce(vm, (/nfacl/), nfacg, 1, ESMF_REDUCE_SUM, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_VMBarrier(vm)
      nlcnt(iter) = ncntl
      ngcnt(iter) = ncntg
      nlfac(iter) = nfacl
      ngfac(iter) = nfacg

      ! report
      if (istep.eq.1) then
      if (present(cname)) then
        write(msgString,'(a,i4,4(a,i10))') trim(cname)//': '//trim(mname)//': ', &
          iter,' ',nlcnt(iter),' ',ngcnt(iter),' ',nlfac(iter),' ',ngfac(iter)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
      endif
      endif

      ! adjust iter based on ncntg
      if (ncntg.eq.0) iter = iter - 1

    enddo iter_loop

    enddo istep_loop

    ! report
!   if (present(cname)) then
!     write(msgString,'(a,a,i4)') trim(cname)//': '//trim(mname)//': ', &
!       'numIter: ',iter
!     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
!     write(msgString,'(a,a4,4(a,a10))') trim(cname)//': '//trim(mname)//': ', &
!       'iter',' ','numLocExt',' ','numGlbExt',' ','numLocFac',' ','numGlbFac'
!     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
!     do i = 1,iter
!       write(msgString,'(a,i4,4(a,i10))') trim(cname)//': '//trim(mname)//': ', &
!         i,' ',nlcnt(i),' ',ngcnt(i),' ',nlfac(i),' ',ngfac(i)
!       call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
!     enddo
!   endif

    ! destroy work fields and release halo route handle
    call ESMF_FieldDestroy(fieldBmsk, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldDestroy(fieldCmsk, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldHaloRelease(haloRH, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  ! Private method -------------------------------------------------------------
!BOPI
! !IROUTINE: getrmp - compute remap factors for a given point (ir,jr)
! !INTERFACE:
  subroutine getrmp(lcmp,rtyp,eps,dcin,lb,ub,mask,ir,jr,ns,is,js,ijs,cs)
! !ARGUMENTS:
    logical :: lcmp !true: compute coefficients
    character(6) :: rtyp !type of interpolation: 'redist', 'bilinr' or 'bicubc'
    ! eps is a small non-zero tolerance for checking point coincidence
    real(8) :: eps
    ! dcin is the distance (in source grid intervals) that a point being
    ! interpolated is allowed to lie outside the source grid for valid
    ! interpolation.  The is to accomodate points that lie just outside the
    ! source grid.  Note that points outside the source grid will be
    ! extrapolated, and will not be very accurate if dcin is very large.
    real(8) :: dcin
    integer :: lb(2),ub(2)
    logical :: mask(lb(1):ub(1),lb(2):ub(2))
    real(8) :: ir,jr
    integer :: ns
    integer,pointer :: is(:),js(:),ijs(:)
    real(8),pointer :: cs(:)
! !DESCRIPTION:
!   insert description here
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    real(8), parameter :: one=1d0
    integer :: nlvl,nz
    logical,pointer :: lz(:)
    integer,pointer :: iz(:),jz(:)
    real(8),pointer :: cz(:)
    integer :: i,j,k,ic(4),jc(4),mcs
    logical :: doblc

    ! if target point lies outside domain, then return with ns<0
    if ( (ir.lt.real(lb(1),8)-dcin).or. &
         (ir.gt.real(ub(1),8)+dcin).or. &
         (jr.lt.real(lb(2),8)-dcin).or. &
         (jr.gt.real(ub(2),8)+dcin) ) then
      ns = -1
      return
    endif

    ! corner points of grid cell containing target point
    i=max(lb(1),min(int(real(ir,4)),ub(1)-1))
    j=max(lb(2),min(int(real(jr,4)),ub(2)-1))
    ic(1)=i  ;  jc(1)=j  ;
    ic(2)=i+1;  jc(2)=j  ;
    ic(3)=i+1;  jc(3)=j+1;
    ic(4)=i  ;  jc(4)=j+1;

    ! if target point is coincident with an unmasked source grid
    ! cell point, then set the coefficient to 1 and return
    do k=1,4
      if ( abs(real(ic(k),8)-ir).le.eps .and. &
           abs(real(jc(k),8)-jr).le.eps .and. &
           .not.mask(ic(k),jc(k)) ) exit
    enddo
    if (k.le.4) then
      ns = 1
      allocate (is(ns),js(ns),ijs(ns),cs(ns))
      is(1) = ic(k);  js(1) = jc(k);  cs(1) = one;
      ijs = (js-lb(2))*(ub(1)-lb(1)+1)+is-lb(1)+1
      return
    endif

    ! count number of masked points in source cell
    mcs = 0
    do k=1,4
      if (mask(ic(k),jc(k))) mcs = mcs+1
    enddo

    ! compute interpolation
    select case (rtyp)
    case ('redist')
      if (.not.mask(nint(ir),nint(jr))) then
        nz = 1
        allocate (lz(nz),iz(nz),jz(nz),cz(nz))
        lz(1) = .true.
        iz(1) = nint(ir)
        jz(1) = nint(jr)
        cz(1) = one
      else
        ns = 0
        return
      endif
    case ('bilinr')
      if (mcs.eq.0) then
        call getblc(lcmp,lb,ub,ir,jr,nz,lz,iz,jz,cz)
      else
        ns = 0
        return
      endif
    case ('bicubc')
      if (mcs.eq.0) then
        call getbcc(lcmp,lb,ub,ir,jr,nz,lz,iz,jz,cz)
        doblc = .false.
        do k=1,nz
          if (lz(k)) then
            if (mask(iz(k),jz(k))) then
              doblc = .true.
              exit
            endif
          endif
        enddo
        if (doblc) then
          deallocate (lz,iz,jz,cz)
          call getblc(lcmp,lb,ub,ir,jr,nz,lz,iz,jz,cz)
        endif
      else
        ns = 0
        return
      endif
    case default
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg='getrmp: unsupported remap type: '//rtyp)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endselect

    ! load into return arrays
    ns = 0
    do k=1,nz
      if (lz(k)) then
        ns = ns + 1
      endif
    enddo
    if (ns.ne.0) then
      allocate (is(ns),js(ns),ijs(ns),cs(ns))
      ns = 0
      do k=1,nz
        if (lz(k)) then
          ns = ns + 1
          is(ns) = iz(k)
          js(ns) = jz(k)
          cs(ns) = cz(k)
        endif
      enddo
      ijs = (js-lb(2))*(ub(1)-lb(1)+1)+is-lb(1)+1
    endif

    ! release work arrays
    deallocate (lz,iz,jz,cz)

  end subroutine
  !-----------------------------------------------------------------------------

  ! Private method -------------------------------------------------------------
!BOPI
! !IROUTINE: getblc - compute bilinear remap factors for a given point (ir,jr)
! !INTERFACE:
  subroutine getblc(lcmp,lb,ub,ir,jr,ns,ls,is,js,cs)
! !ARGUMENTS:
    logical :: lcmp !true: compute coefficients
    integer :: lb(2),ub(2)
    real(8) :: ir,jr
    integer :: ns
    logical,pointer :: ls(:)
    integer,pointer :: is(:),js(:)
    real(8),pointer :: cs(:)
! !DESCRIPTION:
! compute source points and coefficients for bilinear interpolation
! (ir,jr) must satisfy: lb(1)<=ir<=ub(1) && lb(2)<=jr<=ub(2)
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    integer :: i,j,k,l
    real(8) :: t,u

    ! set number of interpolation points and allocate arrays
    ns = 4
    allocate(ls(ns),is(ns),js(ns),cs(ns))
    ls=.true.
    cs=0d0

    ! lower-left corner point of grid cell containing target point
    i=max(lb(1),min(int(real(ir,4)),ub(1)-1))
    j=max(lb(2),min(int(real(jr,4)),ub(2)-1))

    ! 4 source points for the bilinear interpolation
    !   (4)------------------(3)
    !    |                    |
    !    |                    |
    !    |                    |
    !   (1)------------------(2)
    is(1)=i  ;  js(1)=j  ;
    is(2)=i+1;  js(2)=j  ;
    is(3)=i+1;  js(3)=j+1;
    is(4)=i  ;  js(4)=j+1;

    ! calculate bilinear interpolation coefficients
    if (lcmp) then
      t=ir-real(i,8)
      u=jr-real(j,8)
      cs(1)=(1d0-t)*(1d0-u)
      cs(2)=t*(1d0-u)
      cs(3)=t*u
      cs(4)=(1d0-t)*u
    endif

  end subroutine
  !-----------------------------------------------------------------------------

  ! Private method -------------------------------------------------------------
!BOPI
! !IROUTINE: getbcc - compute bicubic remap factors for a given point (ir,jr)
! !INTERFACE:
  subroutine getbcc(lcmp,lb,ub,ir,jr,ns,ls,is,js,cs)
! !ARGUMENTS:
    logical :: lcmp !true: compute coefficients
    integer :: lb(2),ub(2)
    real(8) :: ir,jr
    integer :: ns
    logical,pointer :: ls(:)
    integer,pointer :: is(:),js(:)
    real(8),pointer :: cs(:)
! !DESCRIPTION:
! compute source points and coefficients for bicubic interpolation
! (ir,jr) must satisfy: lb(1)<=ir<=ub(1) && lb(2)<=jr<=ub(2)
!
!EOPI
  !-----------------------------------------------------------------------------
    ! local variables
    integer :: i,j,k,l
    real(8) :: t,u
    integer :: it,ju,itju
    real(8),dimension(16) :: A
    real(8),parameter :: idx=0.5
    real(8),parameter :: idy=0.5
    integer,dimension(16,16) :: Aw = reshape &
    !  A1  A2  A3  A4  A5  A6  A7  A8  A9  A10 A11 A12 A13 A14 A15 A16
    ((/1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
       0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0, &
      -3,  0,  0,  3,  0,  0,  0,  0, -2,  0,  0, -1,  0,  0,  0,  0, &
       2,  0,  0, -2,  0,  0,  0,  0,  1,  0,  0,  1,  0,  0,  0,  0, &
       0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0, &
       0,  0,  0,  0, -3,  0,  0,  3,  0,  0,  0,  0, -2,  0,  0, -1, &
       0,  0,  0,  0,  2,  0,  0, -2,  0,  0,  0,  0,  1,  0,  0,  1, &
      -3,  3,  0,  0, -2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
       0,  0,  0,  0,  0,  0,  0,  0, -3,  3,  0,  0, -2, -1,  0,  0, &
       9, -9,  9, -9,  6,  3, -3, -6,  6, -6, -3,  3,  4,  2,  1,  2, &
      -6,  6, -6,  6, -4, -2,  2,  4, -3,  3,  3, -3, -2, -1, -1, -2, &
       2, -2,  0,  0,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
       0,  0,  0,  0,  0,  0,  0,  0,  2, -2,  0,  0,  1,  1,  0,  0, &
      -6,  6, -6,  6, -3, -3,  3,  3, -4,  4,  2, -2, -2, -2, -1, -1, &
       4, -4,  4, -4,  2,  2, -2, -2,  2, -2, -2,  2,  1,  1,  1,  1  &
      /),(/16,16/))

    ! set number of interpolation points and allocate arrays
    ns = 16
    allocate(ls(ns),is(ns),js(ns),cs(ns))
    ls=.true.
    cs=0d0

    ! lower-left corner point of grid cell containing target point
    i=max(lb(1),min(int(real(ir,4)),ub(1)-1))
    j=max(lb(2),min(int(real(jr,4)),ub(2)-1))

    ! 16 source points for the bicubic interpolation
    ! The additional points are needed to construct derivatives (centered in space).
    ! If boundary points are not available one sided finite differences are used.
    !
    !   (14)...(13)...(12)...(11)
    !    .      .      .      .
    !    .      .      .      .
    !    .      .      .      .
    !   (15)   (4)----(3)    (10)
    !    .      |      |      .
    !    .      |      |      .
    !    .      |      |      .
    !   (16)   (1)----(2)    (9)
    !    .      .      .      .
    !    .      .      .      .
    !    .      .      .      .
    !   (5)....(6)....(7)....(8)
    is( 1)=i  ;  js( 1)=j  ;
    is( 2)=i+1;  js( 2)=j  ;
    is( 3)=i+1;  js( 3)=j+1;
    is( 4)=i  ;  js( 4)=j+1;
    is( 5)=i-1;  js( 5)=j-1;
    is( 6)=i  ;  js( 6)=j-1;
    is( 7)=i+1;  js( 7)=j-1;
    is( 8)=i+2;  js( 8)=j-1;
    is( 9)=i+2;  js( 9)=j  ;
    is(10)=i+2;  js(10)=j+1;
    is(11)=i+2;  js(11)=j+2;
    is(12)=i+1;  js(12)=j+2;
    is(13)=i  ;  js(13)=j+2;
    is(14)=i-1;  js(14)=j+2;
    is(15)=i-1;  js(15)=j+1;
    is(16)=i-1;  js(16)=j  ;

    ! setup table of bicubic coefficients
    if (lcmp) then
      t=ir-real(i,8)
      u=jr-real(j,8)
      do k=1,16
        A(k)=0d0
        do it=0,3
          do ju=0,3
            itju=it*4+ju+1
            A(k)=A(k)+real(Aw(k,itju),8)*(t**it)*(u**ju)
          enddo
        enddo
      enddo
    endif

    ! calculate bicubic interpolation coefficients
    ! ALL points available
    if ((i.gt.lb(1)  ).and.(j.gt.lb(2)  ).and. &
        (i.lt.ub(1)-1).and.(j.lt.ub(2)-1)) then
      if (lcmp) then
        cs(1)=A(1)-A(6)*IDX-A(12)*IDY+A(15)*IDX*IDY;
        cs(2)=A(2)+A(5)*IDX-A(11)*IDY-A(16)*IDX*IDY;
        cs(3)=A(3)+A(8)*IDX+A(10)*IDY+A(13)*IDX*IDY;
        cs(4)=A(4)-A(7)*IDX+A(9)*IDY-A(14)*IDX*IDY;
        cs(5)=A(13)*IDX*IDY;
        cs(6)=-A(9)*IDY+A(14)*IDX*IDY;
        cs(7)=-A(10)*IDY-A(13)*IDX*IDY;
        cs(8)=-A(14)*IDX*IDY;
        cs(9)=A(6)*IDX-A(15)*IDX*IDY;
        cs(10)=A(7)*IDX+A(14)*IDX*IDY;
        cs(11)=A(15)*IDX*IDY;
        cs(12)=A(11)*IDY+A(16)*IDX*IDY;
        cs(13)=A(12)*IDY-A(15)*IDX*IDY;
        cs(14)=-A(16)*IDX*IDY;
        cs(15)=-A(8)*IDX-A(13)*IDX*IDY;
        cs(16)=-A(5)*IDX+A(16)*IDX*IDY;   
      endif
    ! SW point
    elseif ((i.eq.lb(1)).and.(j.eq.lb(2))) then
      if (lcmp) then
       cs(1)=A(1)-3*A(5)*IDX-A(6)*IDX-3*A(9)*IDX-A(12)*IDY+9*A(13)*IDX*IDY+3*A(14)*IDX*IDY+A(15)*IDX*IDY+3*A(16)*IDX*IDY
       cs(2)=A(2)+4*A(5)*IDX-3*A(10)*IDY-A(11)*IDY-12*A(13)*IDX*IDY-4*A(16)*IDX*IDY
       cs(3)=A(3)+4*A(8)*IDX+4*A(10)*IDX+16*A(13)*IDX*IDY
       cs(4)=A(4)-A(7)*IDX-3*A(8)*IDX+4*A(9)*IDY-12*A(13)*IDX*IDY-4*A(14)*IDX*IDY
       cs(9)=-A(5)*IDX+A(6)*IDX+3*A(13)*IDX*IDY-3*A(14)*IDX*IDY-A(15)*IDX*IDY+A(16)*IDX*IDY
       cs(10)=A(7)*IDX-A(8)*IDX-4*A(13)*IDX*IDY+4*A(14)*IDX*IDY
       cs(11)=A(13)*IDX*IDY-A(14)*IDX*IDY+A(15)*IDX*IDY-A(16)*IDX*IDY
       cs(12)=-A(10)*IDY+A(11)*IDY-4*A(13)*IDX*IDY+4*A(16)*IDX*IDY
       cs(13)=-A(9)*IDY+A(12)*IDY+3*A(13)*IDX*IDY+A(14)*IDX*IDY-A(15)*IDX*IDY-3*A(16)*IDX*IDY
      endif
      ls(5)=.false.
      ls(6)=.false.
      ls(7)=.false.
      ls(8)=.false.
      ls(14)=.false.
      ls(15)=.false.
      ls(16)=.false.
    ! S points
    elseif ((i.gt.lb(1)).and.(i.lt.ub(1)-1).and.(j.eq.lb(2))) then
      if (lcmp) then
        cs(1)=A(1)-A(6)*IDX-3*A(9)*IDY-A(12)*IDY+3*A(14)*IDX*IDY+A(15)*IDX*IDY
        cs(2)=A(2)+A(5)*IDX-3*A(10)*IDY-A(11)*IDY-3*A(13)*IDX*IDY-A(16)*IDX*IDY
        cs(3)=A(3)+A(8)*IDX+4*A(10)*IDY+4*A(13)*IDX*IDY
        cs(4)=A(4)-A(7)*IDX+4*A(9)*IDY-4*A(14)*IDX*IDY
        cs(9)=A(6)*IDX-3*A(14)*IDX*IDY-A(15)*IDX*IDY
        cs(10)=A(7)*IDX+4*A(14)*IDX*IDY
        cs(11)=-A(14)*IDX*IDY+A(15)*IDX*IDY
        cs(12)=-A(10)*IDY+A(11)*IDY-A(13)*IDX*IDY+A(16)*IDX*IDY
        cs(13)=-A(9)*IDY+A(12)*IDY+A(14)*IDX*IDY-A(15)*IDX*IDY
        cs(14)=A(13)*IDX*IDY-A(16)*IDX*IDY
        cs(15)=-A(8)*IDX-4*A(13)*IDX*IDY
        cs(16)=-A(5)*IDX+3*A(13)*IDX*IDY+A(16)*IDX*IDY
      endif
      ls(5)=.false.
      ls(6)=.false.
      ls(7)=.false.
      ls(8)=.false.
    ! SE point
    elseif ((i.eq.ub(1)-1).and.(j.eq.lb(2))) then
      if (lcmp) then
        cs(1)=A(1)-4*A(6)*IDX-3*A(9)*IDY-A(12)*IDY+12*A(14)*IDX*IDY+4*A(15)*IDX*IDY
        cs(2)=A(2)+A(5)*IDX+3*A(6)*IDX-3*A(10)*IDY-A(11)*IDY-3*A(13)*IDX*IDY-9*A(14)*IDX*IDY-3*A(15)*IDX*IDY-A(16)*IDX*IDY
        cs(3)=A(3)+3*A(7)*IDX+A(8)*IDX+4*A(10)*IDY+4*A(13)*IDX*IDY+12*A(14)*IDX*IDY
        cs(4)=A(4)-4*A(7)*IDX+4*A(9)*IDY-16*A(14)*IDX*IDY
        cs(12)=-A(10)*IDY+A(11)*IDY-A(13)*IDX*IDY-3*A(14)*IDX*IDY+3*A(15)*IDX*IDY+A(16)*IDX*IDY
        cs(13)=-A(9)*IDY+A(12)*IDY+4*A(14)*IDX*IDY-4*A(15)*IDX*IDY
        cs(14)=IDX*IDY*(A(13)-A(14)+A(15)-A(16))
        cs(15)=A(7)*IDX-A(8)*IDX+IDX*IDY*(-4*A(13)+4*A(14))
        cs(16)=-A(5)*IDX+A(6)*IDX+IDX*IDY*(3*A(13)-3*A(14)-A(15)+A(16))
      endif
      ls(5)=.false.
      ls(6)=.false.
      ls(7)=.false.
      ls(8)=.false.
      ls(9)=.false.
      ls(10)=.false.
      ls(11)=.false.
    ! E points
    elseif ((i.eq.ub(1)-1).and.(j.gt.lb(2)).and.(j.lt.ub(2)-1)) then
      if (lcmp) then
        cs(1)=A(1)-4*A(6)*IDX-A(12)*IDY+4*A(15)*IDX*IDY
        cs(2)=A(2)+A(5)*IDX+3*A(6)*IDX-A(11)*IDY+IDX*IDY*(-3*A(15)-A(16))
        cs(3)=A(3)+3*A(7)*IDX+A(8)*IDX+A(10)*IDY+IDX*IDY*(A(13)+3*A(14))
        cs(4)=A(4)-4*A(7)*IDX+A(9)*IDY-4*A(14)*IDX*IDY
        cs(5)=IDX*IDY*(A(13)-A(14))
        cs(6)=-A(9)*IDY+4*A(14)*IDX*IDY
        cs(7)=-A(10)*IDY-A(13)*IDX*IDY-3*A(14)*IDX*IDY
        cs(12)=A(11)*IDY+IDX*IDY*(3*A(15)+A(16))
        cs(13)=A(12)*IDY-4*A(15)*IDX*IDY
        cs(14)=IDX*IDY*(A(15)-A(16))
        cs(15)=A(7)*IDX-A(8)*IDX-A(13)*IDX*IDY+A(14)*IDX*IDY
        cs(16)=-A(5)*IDX+A(6)*IDX-A(15)*IDX*IDY+A(16)*IDX*IDY
      endif
      ls(8)=.false.
      ls(9)=.false.
      ls(10)=.false.
      ls(11)=.false.
    ! NE point
    elseif ((i.eq.ub(1)-1).and.(j.eq.ub(2)-1)) then
      if (lcmp) then
        cs(1)=A(1)-4*A(6)*IDX-4*A(12)*IDY+16*A(15)*IDX*IDY
        cs(2)=A(2)+A(5)*IDX+3*A(6)*IDX-4*A(11)*IDY+IDX*IDY*(-12*A(15)-4*A(16))
        cs(3)=A(3)+3*A(7)*IDX+A(8)*IDX+A(10)*IDY+3*A(11)*IDY+IDX*IDY*(A(13)+3*A(14)+9*A(15)+3*A(16))
        cs(4)=A(4)-4*A(7)*IDX+A(9)*IDX+3*A(12)*IDY+IDX*IDY*(-4*A(14)-12*A(15))
        cs(5)=IDX*IDY*(A(13)-A(14)+A(15)-A(16))
        cs(6)=-A(9)*IDY+A(12)*IDY+IDX*IDY*(4*A(14)-4*A(15))
        cs(7)=-A(10)*IDY+A(11)*IDY+IDX*IDY*(-A(13)-3*A(14)+3*A(15)+A(16))
        cs(15)=A(7)*IDX-A(8)*IDX+IDX*IDY*(-A(13)+A(14)+3*A(15)-3*A(16))
        cs(16)=-A(5)*IDX+A(6)*IDX+IDX*IDY*(-4*A(15)+4*A(16))
      endif
      ls(8)=.false.
      ls(9)=.false.
      ls(10)=.false.
      ls(11)=.false.
      ls(12)=.false.
      ls(13)=.false.
      ls(14)=.false.
    ! N points
    elseif ((i.gt.lb(1)).and.(i.lt.ub(1)-1).and.(j.eq.ub(2)-1)) then
      if (lcmp) then
        cs(1)=A(1)-A(6)*IDX-4*A(12)*IDY+4*A(15)*IDX*IDY
        cs(2)=A(2)+A(5)*IDX-4*A(11)*IDY-4*A(16)*IDX*IDY
        cs(3)=A(3)+A(8)*IDX+A(10)*IDY+3*A(11)*IDY+IDX*IDY*(A(13)+3*A(16))
        cs(4)=A(4)-A(7)*IDX+A(9)*IDY+3*A(12)*IDY+IDX*IDY*(-A(14)-3*A(15))
        cs(5)=IDX*IDY*(A(13)-A(16))
        cs(6)=-A(9)*IDY+A(12)*IDY+IDX*IDY*(A(14)-A(15))
        cs(7)=-A(10)*IDY+A(11)*IDY+IDX*IDY*(-A(13)+A(16))
        cs(8)=IDX*IDY*(-A(14)+A(15))
        cs(9)=A(6)*IDX-4*A(15)*IDX*IDY
        cs(10)=A(7)*IDX+IDX*IDY*(A(14)+3*A(15))
        cs(15)=-A(8)*IDX+IDX*IDY*(-A(13)-3*A(16))
        cs(16)=-A(5)*IDX+4*A(16)*IDX*IDY
      endif
      ls(11)=.false.
      ls(12)=.false.
      ls(13)=.false.
      ls(14)=.false.
    ! NW point
    elseif ((i.eq.lb(1)).and.(j.eq.ub(2)-1)) then
      if (lcmp) then
        cs(1)=A(1)-3*A(5)*IDX-A(6)*IDX-4*A(12)*IDY+IDX*IDY*(4*A(15)+12*A(16))
        cs(2)=A(2)+4*A(5)*IDX-4*A(11)*IDY-16*A(16)*IDX*IDY
        cs(3)=A(3)+4*A(8)*IDX+A(10)*IDY+3*A(11)*IDY+IDX*IDY*(4*A(13)+12*A(16))
        cs(4)=A(4)-A(7)*IDX-3*A(8)*IDX+A(9)*IDY+3*A(12)*IDY+IDX*IDY*(-3*A(13)-A(14)-3*A(15)-9*A(16))
        cs(6)=-A(9)*IDX+A(12)*IDX+IDX*IDY*(3*A(13)+A(14)-A(15)-3*A(16))
        cs(7)=-A(10)*IDY+A(11)*IDY+IDX*IDY*(-4*A(13)+4*A(16))
        cs(8)=IDX*IDY*(A(13)-A(14)+A(15)-A(16))
        cs(9)=-A(5)*IDX+A(6)*IDX+IDX*IDY*(-4*A(15)+4*A(16))
        cs(10)=A(7)*IDX-A(8)*IDX+IDX*IDY*(-A(13)+A(14)+3*A(15)-3*A(16))
      endif
      ls(5)=.false.
      ls(11)=.false.
      ls(12)=.false.
      ls(13)=.false.
      ls(14)=.false.
      ls(15)=.false.
      ls(16)=.false.
    ! W points
    elseif ((i.eq.lb(1)).and.(j.gt.lb(2)).and.(j.lt.ub(2)-1)) then
      if (lcmp) then
        cs(1)=A(1)-3*A(5)*IDX-A(6)*IDX-A(12)*IDY+IDX*IDY*(A(15)+3*A(16))
        cs(2)=A(2)+4*A(5)*IDX-A(11)*IDY-4*A(16)*IDX*IDY
        cs(3)=A(3)+4*A(8)*IDX+A(10)*IDY+4*A(13)*IDX*IDY
        cs(4)=A(4)-A(7)*IDX-3*A(8)*IDX+A(9)*IDY+IDX*IDY*(-3*A(13)-A(14))
        cs(6)=-A(9)*IDY+IDX*IDY*(3*A(13)+A(14))
        cs(7)=-A(10)*IDY-4*A(13)*IDX*IDY
        cs(8)=IDX*IDY*(A(13)-A(14))
        cs(9)=-A(5)*IDX+A(6)*IDX+IDX*IDY*(-A(15)+A(16))
        cs(10)=A(7)*IDX-A(8)*IDX+IDX*IDY*(-A(13)+A(14))
        cs(11)=IDX*IDY*(A(15)-A(16))
        cs(12)=A(11)*IDY+4*A(16)*IDX*IDY
        cs(13)=A(12)*IDY+IDX*IDY*(-A(15)-3*A(16))
      endif
      ls(5)=.false.
      ls(14)=.false.
      ls(15)=.false.
      ls(16)=.false.
    endif

  end subroutine
  !-----------------------------------------------------------------------------

end module
