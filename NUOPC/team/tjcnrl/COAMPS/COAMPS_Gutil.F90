!-------------------------------------------------------------------------------
! COAMPS Grid Utilities Module
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_Gutil.F90"
#include "COAMPS_Macros.h"


module COAMPS_Gutil

  use ESMF
  use COAMPS_Gdem

  implicit none

  private

  ! public methods
  public GridCreate
  public GridSetMask
  public GridIsCovered
  public CGridCreate
  public CGridPrint
  public CGridCoord
  public CGridIJ2LL
  public CGridLL2IJ
  public FFileCreate
  public FFileSetDims
  public FFilePrint
  public FFileName
  public FFileSetTime
  public FFileRead
  public FFileWrite
  public FFileWriteGrid

  ! COAMPS grid structure type (public)
  type, public :: CGrid
    logical      :: ijg
    character(1) :: cfluid
    integer      :: nest
    integer      :: nproj
    integer      :: iref, jref
    integer      :: m, n, l
    real(8)      :: alnnt, phnt1, phnt2
    real(8)      :: rlon, rlat
    real(8)      :: delx, dely
    real(8)      :: rnest
  end type

  ! Flat-file structure type (public)
  type, public :: FFile
    character(6)      :: fldnam     ! field name
    character(24)     :: lvltyp     ! level type
    real(4)           :: rlev(2)    ! min/max levels
    integer           :: nest       ! nest identifier
    character(1)      :: cfluid     ! fluid type
    integer           :: dims(3)    ! grid dimensions
    integer           :: dtgv(4)    ! date-time group vector [YYYY,MM,DD,hh]
    integer           :: tauv(3)    ! tau vector [hhhh,mm,ss]
    character(7)      :: outtyp     ! output type
  end type

  ! private data
  real(8), parameter :: pi   = 3.14159265358979323846d0
  real(8), parameter :: pi2  = 2d0*pi
  real(8), parameter :: pi4  = 4d0*pi
  real(8), parameter :: pi3h = 3d0*pi/2d0
  real(8), parameter :: pio2 = pi/2d0
  real(8), parameter :: pio4 = pi/4d0
  real(8), parameter :: d2r  = pi/180d0
  real(8), parameter :: r2d  = 1d0/d2r
  real(8), parameter :: d360 = 360d0
  real(8), parameter :: d270 = 270d0
  real(8), parameter :: d180 = 180d0
  real(8), parameter :: d90  =  90d0
  real(8), parameter :: zero = 0.0d0
  real(8), parameter :: half = 0.5d0
  real(8), parameter :: one  = 1.0d0
  real(8), parameter :: two  = 2.0d0
  real(8), parameter :: four = 4.0d0
  real(8), parameter :: rearth = 4.d7/pi2
  real(8), parameter :: d2m  = rearth*d2r
  real(8), parameter :: m2d  = 1d0/d2m
  real(8), parameter :: omega4 = 4d0*pi/86400d0
  character(ESMF_MAXSTR) :: msgString

  ! module interfaces
  interface GridSetMask
    module procedure GridSetMask_FFile
    module procedure GridSetMask_Gdem
    module procedure GridSetMask_Const
  end interface
  interface GridIsCovered
    module procedure GridIsCovered_Gridnl
    module procedure GridIsCovered_CGrid
  end interface
  interface CGridCreate
    module procedure CGridCreate_Gridnl
    module procedure CGridCreate_Datahd
    module procedure CGridCreate_Args_r4
    module procedure CGridCreate_Args_r8
  end interface
  interface CGridCoord
    module procedure CGridCoord_r4
    module procedure CGridCoord_r8
  end interface
  interface CGridIJ2LL
    module procedure CGridIJ2LL_array2_r4
    module procedure CGridIJ2LL_array2_r8
    module procedure CGridIJ2LL_array1_r4
    module procedure CGridIJ2LL_array1_r8
    module procedure CGridIJ2LL_point_r4
    module procedure CGridIJ2LL_point_r8
  end interface
  interface CGridLL2IJ
    module procedure CGridLL2IJ_array2_r4
    module procedure CGridLL2IJ_array2_r8
    module procedure CGridLL2IJ_array1_r4
    module procedure CGridLL2IJ_array1_r8
    module procedure CGridLL2IJ_point_r4
    module procedure CGridLL2IJ_point_r8
  end interface
  interface FFileCreate
    module procedure FFileCreate_CGrid
    module procedure FFileCreate_Array
    module procedure FFileCreate_Field
  end interface
  interface FFileSetDims
    module procedure FFileSetDims_Array
    module procedure FFileSetDims_Field
  end interface
  interface FFileRead
    module procedure FFileRead_Array
    module procedure FFileRead_Field
  end interface
  interface FFileWrite
    module procedure FFileWrite_Array
    module procedure FFileWrite_Field
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function GridCreate(cgs, dataDir, vm, dtgTime, tauTime, read_coord, rc) &
    result (grid)
    type(CGrid)           :: cgs
    character(*)          :: dataDir
    type(ESMF_VM)         :: vm
    type(ESMF_Time)       :: dtgTime
    type(ESMF_Time)       :: tauTime
    logical     ,optional :: read_coord
    integer     ,optional :: rc
    type(ESMF_Grid)       :: grid

    ! local variables
    integer, parameter             :: lde=0
    integer                        :: localrc, stat
    type(FFile)                    :: ffs
    logical                        :: rcoord
    integer                        :: i, j, nPets, n
    integer                        :: decomp(2), lb(2), ub(2)
    type(ESMF_ArraySpec)           :: arraySpec
    type(ESMF_DistGrid)            :: distGrid
    type(ESMF_Field)               :: field
    real(ESMF_KIND_RX),    pointer :: lon(:,:), lat(:,:)
    character(6)                   :: fldnam
    character(24)                  :: lvltyp='surface'
    real(4)                        :: rlev(2)=(/0.0,0.0/)
    character(7)                   :: outtyp='datafld'
    character(6)                   :: lonpfx='grdlon'
    character(6)                   :: latpfx='grdlat'

    if (present(rc)) rc = ESMF_SUCCESS

    ! handle optional args
    if (present(read_coord)) then
      rcoord = read_coord
    else
      rcoord = .false.
    endif

    ! get VM info
    call ESMF_VMGet(vm, petCount=nPets, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! create arraySpec
    call ESMF_ArraySpecSet(arraySpec, rank=2, typeKind=ESMF_TYPEKIND_RX, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! create distGrid
    do n=int(sqrt(real(nPets))),2,-1
      if (mod(nPets,n).eq.0) exit
    enddo
    if (cgs%m.gt.cgs%n) then
      decomp = (/nPets/n,n/)
    else
      decomp = (/n,nPets/n/)
    endif
    distGrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/cgs%m,cgs%n/), &
      regDecomp=decomp, indexFlag=ESMF_INDEX_GLOBAL, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! create grid
    grid = ESMF_GridCreate(distGrid, coordTypeKind=ESMF_TYPEKIND_RX, &
      indexFlag=ESMF_INDEX_GLOBAL, coordSys=ESMF_COORDSYS_SPH_DEG, &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set grid coord
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetCoord(grid, 1, localDE=lde, &
      exclusiveLBound=lb, exclusiveUBound=ub, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=lon, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetCoord(grid, 2, localDE=lde, &
      exclusiveLBound=lb, exclusiveUBound=ub, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=lat, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (rcoord) then
      fldnam = lonpfx; ffs = FFileCreate(cgs, fldnam, lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      field = ESMF_FieldCreate(grid, lon, dataCopyFlag=ESMF_DATACOPY_REFERENCE, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call FFileRead(ffs, field, vm, trim(dataDir), dtgTime, tauTime, outtyp, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_FieldDestroy(field, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      fldnam = latpfx; ffs = FFileCreate(cgs, fldnam, lvltyp, rlev, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      field = ESMF_FieldCreate(grid, lat, dataCopyFlag=ESMF_DATACOPY_REFERENCE, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call FFileRead(ffs, field, vm, trim(dataDir), dtgTime, tauTime, outtyp, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_FieldDestroy(field, rc=rc)
    else
      call CGridCoord(cgs, lb(1), ub(1), lb(2), ub(2), lon, lat, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    endif

  end function

  !-----------------------------------------------------------------------------

  subroutine GridSetMask_FFile(grid, cgs, dataDir, vm, dtgTime, tauTime, &
    mask_prefix, rc)
    type(ESMF_Grid)       :: grid
    type(CGrid)           :: cgs
    character(*)          :: dataDir
    type(ESMF_VM)         :: vm
    type(ESMF_Time)       :: dtgTime
    type(ESMF_Time)       :: tauTime
    character(6),optional :: mask_prefix
    integer     ,optional :: rc

    ! local variables
    integer, parameter             :: lde=0
    integer                        :: localrc, stat
    type(FFile)                    :: ffs
    integer                        :: i, j, n
    integer                        :: lb(2), ub(2)
    type(ESMF_ArraySpec)           :: arraySpec
    type(ESMF_Field)               :: field
    real(ESMF_KIND_RX),    pointer :: amsk(:,:)
    integer(ESMF_KIND_I4), pointer :: mask(:,:)
    character(6)                   :: fldnam
    character(24)                  :: lvltyp='surface'
    real(4)                        :: rlev(2)=(/0.0,0.0/)
    character(7)                   :: outtyp='datafld'
    character(6)                   :: lndpfx='lndsea'

    if (present(rc)) rc = ESMF_SUCCESS

    ! handle optional args
    if (present(mask_prefix)) then
      lndpfx = mask_prefix
    endif

    ! create arraySpec
    call ESMF_ArraySpecSet(arraySpec, rank=2, typeKind=ESMF_TYPEKIND_RX, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! add grid mask, get pointer and bounds
    call ESMF_GridAddItem(grid, ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, localDE=lde, &
      exclusiveLBound=lb, exclusiveUBound=ub, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=mask, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! read mask from file
    field = ESMF_FieldCreate(grid, arraySpec, indexFlag=ESMF_INDEX_GLOBAL, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_FieldGet(field, lde, amsk, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    fldnam = lndpfx; ffs = FFileCreate(cgs, fldnam, lvltyp, rlev, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call FFileRead(ffs, field, vm, trim(dataDir), dtgTime, tauTime, outtyp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set integer mask
    do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        mask(i,j) = nint(amsk(i,j))
      enddo
    enddo

    ! clean up
    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine GridSetMask_Gdem(grid, gdemDir, rc)
    type(ESMF_Grid)       :: grid
    character(*)          :: gdemDir
    integer     ,optional :: rc

    ! local variables
    integer, parameter             :: lde=0
    integer                        :: localrc, stat
    integer                        :: i, j, n
    integer                        :: lb(2), ub(2)
    real(ESMF_KIND_RX),    pointer :: lon(:,:), lat(:,:)
    real              ,    pointer :: topo(:,:), lons(:,:), lats(:,:)
    integer(ESMF_KIND_I4), pointer :: mask(:,:)

    if (present(rc)) rc = ESMF_SUCCESS

    ! get pointers to grid coord
    call ESMF_GridGetCoord(grid, 1, localDE=lde, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=lon, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetCoord(grid, 2, localDE=lde, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=lat, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! add grid mask, get pointer and bounds
    call ESMF_GridAddItem(grid, ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, localDE=lde, &
      exclusiveLBound=lb, exclusiveUBound=ub, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=mask, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! allocate work arrays
    allocate (topo(lb(1):ub(1),lb(2):ub(2)), &
              lons(lb(1):ub(1),lb(2):ub(2)), &
              lats(lb(1):ub(1),lb(2):ub(2)), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of work arrays failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

    ! COAMPS GLOBE DEM requires longitudes in [-180,180]
    do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        lons(i,j) = lon(i,j)
        if (lons(i,j).gt.d180) lons(i,j)=lons(i,j)-d360
        lats(i,j) = lat(i,j)
      enddo
    enddo

    ! get topo from GLOBE DEM
    n = (ub(2)-lb(2)+1)*(ub(1)-lb(1)+1)
    call GDEM_Init(trim(gdemDir), 0, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call GDEM_Get(n, lons, lats, -999., topo, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call GDEM_Final(rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set integer mask
    do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        if (topo(i,j).gt.zero) then
          mask(i,j) = COAMPS_MASK_LAND
        else
          mask(i,j) = COAMPS_MASK_WATER
        endif
      enddo
    enddo

    ! clean up
    deallocate (topo, lons, lats, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of work arrays failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine GridSetMask_Const(grid, mskval, rc)
    type(ESMF_Grid)       :: grid
    integer               :: mskval
    integer     ,optional :: rc

    ! local variables
    integer, parameter             :: lde=0
    integer                        :: localrc, stat
    integer                        :: i, j, n
    integer                        :: lb(2), ub(2)
    integer(ESMF_KIND_I4), pointer :: mask(:,:)

    if (present(rc)) rc = ESMF_SUCCESS

    ! add grid mask, get pointer and bounds
    call ESMF_GridAddItem(grid, ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, localDE=lde, &
      exclusiveLBound=lb, exclusiveUBound=ub, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=mask, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set integer mask
    do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        mask(i,j) = mskval
      enddo
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  function GridIsCovered_CGrid(srcCgs, dstCgs, rc) result (cover)
    type(CGrid)           :: srcCgs
    type(CGrid)           :: dstCgs
    integer     ,optional :: rc
    logical               :: cover

    ! local variables
    integer               :: localrc, stat
    integer               :: i, j, nb
    real(ESMF_KIND_RX)    :: bimin, bimax, bjmin, bjmax
    real(ESMF_KIND_RX), allocatable  :: blon(:),blat(:),bi(:),bj(:)
    ! dcin is the distance (in source grid intervals) that a point being
    ! interpolated is allowed to lie outside the source grid for valid
    ! interpolation.  The is to accomodate points that lie just outside the
    ! source grid.  Note that points outside the source grid will be
    ! extrapolated, and will not be very accurate if dcin is very large.
    real(8), parameter :: dcin = 1d-1

    if (present(rc)) rc = ESMF_SUCCESS
!   write(*,'(a)') 'srcCgs:'
!   call CGridPrint(srcCgs)
!   write(*,'(a)') 'dstCgs:'
!   call CGridPrint(dstCgs)

    ! setup destination grid boundary point arrays
    nb = 2*(dstCgs%m+dstCgs%n-2)
    allocate (blon(nb), blat(nb), bi(nb), bj(nb), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of grid boundary point arrays failed.", &
      CONTEXT, rcToReturn=rc)) return ! bail out
    nb = 0
    do j = 1,dstCgs%n,dstCgs%n-1
      do i = 1,dstCgs%m
        nb = nb+1
        bi(nb) = i
        bj(nb) = j
      enddo
    enddo
    do j = 2,dstCgs%n-1
      do i = 1,dstCgs%m,dstCgs%m-1
        nb = nb+1
        bi(nb) = i
        bj(nb) = j
      enddo
    enddo
!   write(*,'(a,4e14.6)') 'bndy src i/j min/max: ', &
!     minval(bi),maxval(bi),minval(bj),maxval(bj)
    call CGridIJ2LL(dstCgs,bi,bj,blon,blat,rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
!   write(*,'(a,4e14.6)') 'bndy dst lon/lat min/max: ', &
!     minval(blon),maxval(blon),minval(blat),maxval(blat)

    ! compute destination grid boundary points in source grid index space
    call CGridLL2IJ(srcCgs,blon,blat,bi,bj,rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
!   write(*,'(a,4e14.6)') 'bndy dst i/j min/max: ', &
!     minval(bi),maxval(bi),minval(bj),maxval(bj)

    ! check if destination grid is covered by source grid
    bimin = minval(bi)
    bimax = maxval(bi)
    bjmin = minval(bj)
    bjmax = maxval(bj)
    cover = bimin.ge.1-dcin.and.bimin.le.srcCgs%m+dcin.and. &
            bimax.ge.1-dcin.and.bimax.le.srcCgs%m+dcin.and. &
            bjmin.ge.1-dcin.and.bjmin.le.srcCgs%n+dcin.and. &
            bjmax.ge.1-dcin.and.bjmax.le.srcCgs%n+dcin

    ! clean up
    deallocate (blon, blat, bi, bj, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of grid boundary point arrays failed.", &
      CONTEXT, rcToReturn=rc)) return ! bail out

  end function

  !-----------------------------------------------------------------------------

  function GridIsCovered_Gridnl(srcGridnl, srcNest, dstGridnl, dstNest, rc) result (cover)
    character(*)          :: srcGridnl
    integer               :: srcNest
    character(*)          :: dstGridnl
    integer               :: dstNest
    integer     ,optional :: rc
    logical               :: cover

    ! local variables
    integer               :: localrc, stat
    type(CGrid)           :: srcCgs
    character(1)          :: srcFluid='z' !fluid type doesn't matter
    type(CGrid)           :: dstCgs
    character(1)          :: dstFluid='z' !fluid type doesn't matter

    if (present(rc)) rc = ESMF_SUCCESS

    ! create src grid data structure
    srcCgs = CGridCreate(srcGridnl, srcFluid, srcNest, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! create dst grid data structure
    dstCgs = CGridCreate(dstGridnl, dstFluid, dstNest, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! check if destination grid is covered by source grid
    cover = GridIsCovered(srcCgs, dstCgs, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end function

  !-----------------------------------------------------------------------------

  function CGridCreate_Gridnl(gnlFile, cfluid, nest, ijg, rc) result (cgs)
    character(*) :: gnlFile
    character(1) :: cfluid
    integer      :: nest
    logical, optional :: ijg
    integer, optional :: rc
    type(CGrid)  :: cgs

    ! local variables
    integer :: iunit, stat
    logical :: lopened
    character(64) :: msg=' '
    integer :: nn, np, nrx0, nry0, nn0
    real(8) :: delxy
    ! grid namelist definition and default values (from gridnl.F)
    integer, parameter :: MAX_GRIDS=7
    real(8),dimension(MAX_GRIDS) :: delx=81000., dely=81000.
    integer,dimension(MAX_GRIDS) :: ii=1, iref=1, jj=1, jref=1
    logical,dimension(MAX_GRIDS) :: lnmove=.false.
    integer,dimension(MAX_GRIDS) :: m=61,n=61,npgrid=(/1,1,2,3,4,5,6/)
    integer :: kkl, kka=30, kko=1, kkom=1, kkosm=0, nnest=1, nproj=2
    real(8) :: alnnt=240., phnt1=60., phnt2=30., rlat=42.5, rlon=16.5
    real(8) :: rnest=3.
    namelist /gridnl/ m,n,kka,kko,kkom,kkosm,nproj,rlat,rlon,alnnt, &
      ii,iref,jj,jref,lnmove,nnest,npgrid,phnt1,phnt2,delx,dely,rnest

    if (present(rc)) rc = ESMF_SUCCESS

    ! find available file unit
    call ESMF_UtilIOUnitGet(iunit, rc=rc)
    if (rc.ne.ESMF_SUCCESS) return

    ! read gridnl
    open (unit=iunit, file=trim(gnlFile), form='formatted', action='read', &
      status='old', iostat=stat, iomsg=msg)
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(gnlFile), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif
    read (iunit, nml=gridnl, iostat=stat, iomsg=msg)
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(gnlFile), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif
    close (iunit, iostat=stat, iomsg=msg)
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(gnlFile), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif
    if (nest.gt.nnest) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg='CGridCreate: nest > nnest is not allowed', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! fill in values for all nests
    do np = 1,nnest
      do nn = 2,nnest
        if (npgrid(nn).eq.np) then
          delx(nn) = delx(np)/rnest
          dely(nn) = dely(np)/rnest
          iref(nn) = (iref(np)-ii(nn))*int(rnest) + 1
          jref(nn) = (jref(np)-jj(nn))*int(rnest) + 1
        endif
      enddo
    enddo

    ! identify highest resolution nest
    nn0 = 1
    delxy = delx(nn0)*dely(nn0)
    do nn = 2,nnest
      if (delx(nn)*dely(nn).lt.delxy) then
        nn0 = nn
        delxy = delx(nn)*dely(nn)
      endif
    enddo 
    nrx0 = delx(1)/delx(nn0)
    nry0 = dely(1)/dely(nn0)

    ! set nest independent grid parameters
    if (present(ijg)) then
      cgs%ijg  = ijg
    else
      cgs%ijg  = .true.
    endif
    cgs%cfluid = cfluid
    cgs%rnest  = rnest
    cgs%nest   = nest
    cgs%nproj  = nproj
    cgs%alnnt  = alnnt
    cgs%phnt1  = phnt1
    cgs%phnt2  = phnt2
    cgs%rlon   = rlon
    cgs%rlat   = rlat
    select case (cfluid)
    case ('a')
      cgs%l = kka
    case ('o')
      cgs%l = kko
    case default
      cgs%l = 1
    end select

    ! set nest dependent grid parameters
    if (nest.eq.0) then
      cgs%m    = nrx0*(m(1)-1) + 1
      cgs%n    = nry0*(n(1)-1) + 1
      cgs%delx = delx(1)/real(nrx0)
      cgs%dely = dely(1)/real(nry0)
      cgs%iref = nrx0*(iref(1)-1) + 1
      cgs%jref = nry0*(jref(1)-1) + 1
    else
      cgs%m    = m(nest)
      cgs%n    = n(nest)
      cgs%delx = delx(nest)
      cgs%dely = dely(nest)
      cgs%iref = iref(nest)
      cgs%jref = jref(nest)
    endif

  end function

  !-----------------------------------------------------------------------------

  function CGridCreate_Datahd(dir, cfluid, dtgTime, nest, ijg, rc) result (cgs)
    character(*)      :: dir
    character(1)      :: cfluid
    type(ESMF_Time)   :: dtgTime
    integer           :: nest
    logical, optional :: ijg
    integer, optional :: rc
    type(CGrid)       :: cgs

    ! local variables
    integer            :: iunit, stat
    logical            :: lopened
    character(64)      :: msg=' '
    type(FFile)        :: ffs
    character(256)     :: dfn
    character(64)      :: ffn
    real(4) :: datahd(2000)
    integer :: llen, nn, np, nrx0, nry0, nn0
    real(8) :: delxy
!   grid namelist definition and default values (from gridnl.F)
    integer, parameter :: MAX_GRIDS=7
    real(8),dimension(MAX_GRIDS) :: delx=81000., dely=81000.
    integer,dimension(MAX_GRIDS) :: ii=1, iref=1, jj=1, jref=1
    logical,dimension(MAX_GRIDS) :: lnmove=.false.
    integer,dimension(MAX_GRIDS) :: m=61,n=61,npgrid=(/1,1,2,3,4,5,6/)
    integer :: kkl, kka=30, kko=1, kkom=1, kkosm=0, nnest=1, nproj=2
    real(8) :: alnnt=240., phnt1=60., phnt2=30., rlat=42.5, rlon=16.5
    real(8) :: rnest=3.

    if (present(rc)) rc = ESMF_SUCCESS

    ! set file name
    ffs%fldnam = 'datahd'
    ffs%lvltyp = 'surface'
    ffs%rlev   = (/0.0,0.0/)
    ffs%nest   = 1
    ffs%cfluid = cfluid
    ffs%dims(1)= 2000
    ffs%dims(2)= 1
    ffs%dims(3)= 1
    call ESMF_TimeGet(dtgTime, yy=ffs%dtgv(1), mm=ffs%dtgv(2), &
      dd=ffs%dtgv(3), h=ffs%dtgv(4), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    ffs%tauv(1) = 0
    ffs%tauv(2) = 0
    ffs%tauv(3) = 0
    ffs%outtyp = 'infofld'
    ffn = FFileName(ffs, rc=rc)
    dfn = trim(dir)//'/'//ffn

    ! find available file unit
    call ESMF_UtilIOUnitGet(iunit, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! read datahd
    inquire (iolength=llen) datahd
    open (unit=iunit, file=trim(dfn), form='unformatted', access='direct', &
      recl=llen, action='read', status='old', iostat=stat, iomsg=msg)
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif
    read (iunit, rec=1, iostat=stat, iomsg=msg) datahd
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif
    close (iunit, iostat=stat, iomsg=msg)
    if (stat.ne.0) then
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
        CONTEXT, rcToReturn=rc)
      call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! extract data from datahd
    nnest = datahd(11)
      kkl = datahd( 2)
    nproj = datahd( 3)
    phnt1 = datahd( 4)
    phnt2 = datahd( 5)
    alnnt = datahd( 6)
     rlat = datahd( 7)
     rlon = datahd( 8)
    do nn = 1,nnest
      np=30+(nn-1)*30
         m(nn) = nint(datahd(np+0))
         n(nn) = nint(datahd(np+1))
        ii(nn) = nint(datahd(np+2))
        jj(nn) = nint(datahd(np+3))
      iref(nn) = nint(datahd(np+4))
      jref(nn) = nint(datahd(np+5))
      delx(nn) =      datahd(np+7)
      dely(nn) =      datahd(np+8)
    enddo
    if (nest.gt.nnest) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg='CGridCreate: nest > nnest is not allowed', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! fill in values for all nests
    do np = 1,nnest
      do nn = 2,nnest
        if (npgrid(nn).eq.np) then
          delx(nn) = delx(np)/rnest
          dely(nn) = dely(np)/rnest
          iref(nn) = (iref(np)-ii(nn))*int(rnest) + 1
          jref(nn) = (jref(np)-jj(nn))*int(rnest) + 1
        endif
      enddo
    enddo

    ! identify highest resolution nest
    nn0 = 1
    delxy = delx(nn0)*dely(nn0)
    do nn = 2,nnest
      if (delx(nn)*dely(nn).lt.delxy) then
        nn0 = nn
        delxy = delx(nn)*dely(nn)
      endif
    enddo 
    nrx0 = delx(1)/delx(nn0)
    nry0 = dely(1)/dely(nn0)

    ! set nest independent grid parameters
    if (present(ijg)) then
      cgs%ijg  = ijg
    else
      cgs%ijg  = .true.
    endif
    cgs%cfluid = cfluid
    cgs%rnest  = rnest
    cgs%nest   = nest
    cgs%nproj  = nproj
    cgs%alnnt  = alnnt
    cgs%phnt1  = phnt1
    cgs%phnt2  = phnt2
    cgs%rlon   = rlon
    cgs%rlat   = rlat
    select case (cfluid)
    case ('a')
      cgs%l = kka
    case ('o')
      cgs%l = kko
    case default
      cgs%l = 1
    end select

    ! set nest dependent grid parameters
    if (nest.eq.0) then
      cgs%m    = nrx0*(m(1)-1) + 1
      cgs%n    = nry0*(n(1)-1) + 1
      cgs%delx = delx(1)/real(nrx0)
      cgs%dely = dely(1)/real(nry0)
      cgs%iref = nrx0*(iref(1)-1) + 1
      cgs%jref = nry0*(jref(1)-1) + 1
    else
      cgs%m    = m(nest)
      cgs%n    = n(nest)
      cgs%delx = delx(nest)
      cgs%dely = dely(nest)
      cgs%iref = iref(nest)
      cgs%jref = jref(nest)
    endif

  end function

  !-----------------------------------------------------------------------------

  function CGridCreate_Args_r4(nproj, iref, jref, m, n, &
           alnnt, phnt1, phnt2, rlon, rlat, delx, dely, ijg, rc) result (cgs)
    integer      :: nproj
    integer      :: iref, jref
    integer      :: m, n
    real(4)      :: alnnt, phnt1, phnt2
    real(4)      :: rlon, rlat
    real(4)      :: delx, dely
    logical, optional :: ijg
    integer, optional :: rc
    type(CGrid)  :: cgs

    ! local variables
    ! none

    if (present(rc)) rc = ESMF_SUCCESS

    ! set nest independent grid parameters
    if (present(ijg)) then
      cgs%ijg  = ijg
    else
      cgs%ijg  = .true.
    endif
    cgs%cfluid = 'w'
    cgs%nproj  = nproj
    cgs%nest   = 1
    cgs%rnest  = 3.
    cgs%iref   = iref
    cgs%jref   = jref
    cgs%m      = m
    cgs%n      = n
    cgs%l      = 1
    cgs%alnnt  = alnnt
    cgs%phnt1  = phnt1
    cgs%phnt2  = phnt2
    cgs%rlon   = rlon
    cgs%rlat   = rlat
    cgs%delx   = delx
    cgs%dely   = dely

  end function

  !-----------------------------------------------------------------------------

  function CGridCreate_Args_r8(nproj, iref, jref, m, n, &
           alnnt, phnt1, phnt2, rlon, rlat, delx, dely, ijg, rc) result (cgs)
    integer      :: nproj
    integer      :: iref, jref
    integer      :: m, n
    real(8)      :: alnnt, phnt1, phnt2
    real(8)      :: rlon, rlat
    real(8)      :: delx, dely
    logical, optional :: ijg
    integer, optional :: rc
    type(CGrid)  :: cgs

    ! local variables
    ! none

    if (present(rc)) rc = ESMF_SUCCESS

    ! set nest independent grid parameters
    if (present(ijg)) then
      cgs%ijg  = ijg
    else
      cgs%ijg  = .true.
    endif
    cgs%cfluid = 'w'
    cgs%nproj  = nproj
    cgs%nest   = 1
    cgs%rnest  = 3.
    cgs%iref   = iref
    cgs%jref   = jref
    cgs%m      = m
    cgs%n      = n
    cgs%l      = 1
    cgs%alnnt  = alnnt
    cgs%phnt1  = phnt1
    cgs%phnt2  = phnt2
    cgs%rlon   = rlon
    cgs%rlat   = rlat
    cgs%delx   = delx
    cgs%dely   = dely

  end function

  !-----------------------------------------------------------------------------

  subroutine CGridPrint(cgs)
    type(CGrid)  :: cgs

    ! local variables
    ! none

    write(*,'(a,1l1)')     'CGridPrint:         ijg: ',cgs%ijg
    write(*,'(a,1a1)')     'CGridPrint:      cfluid: ',cgs%cfluid
    write(*,'(a,1i1)')     'CGridPrint:        nest: ',cgs%nest
    write(*,'(a,1i1)')     'CGridPrint:       nproj: ',cgs%nproj
    write(*,'(a,2i5)')     'CGridPrint:   iref,jref: ',cgs%iref,cgs%jref
    write(*,'(a,3i6)')     'CGridPrint:     m, n, l: ',cgs%m,cgs%n,cgs%l
    write(*,'(a,1e24.16)') 'CGridPrint:       alnnt: ',cgs%alnnt
    write(*,'(a,2e24.16)') 'CGridPrint: phnt1,phnt2: ',cgs%phnt1,cgs%phnt2
    write(*,'(a,2e24.16)') 'CGridPrint:   rlon,rlat: ',cgs%rlon,cgs%rlat
    write(*,'(a,2e24.16)') 'CGridPrint:   delx,dely: ',cgs%delx,cgs%dely
    write(*,'(a,1e24.16)') 'CGridPrint:       rnest: ',cgs%rnest

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridCoord_r4(cgs,lb1,ub1,lb2,ub2,grdlon,grdlat,dx,dy,grdrot,rc)
    !***********************************************************************
    ! Adapted from coampslib/grid.F & grdij.F
    ! Single precision interface
    !***********************************************************************
    type(CGrid)      :: cgs
    integer          :: lb1, ub1, lb2, ub2
    real(4)          :: grdlon (lb1:ub1,lb2:ub2) ! longitude
    real(4)          :: grdlat (lb1:ub1,lb2:ub2) ! latitude
    real(4),optional :: dx     (lb1:ub1,lb2:ub2) ! grid cell width in x-direction
    real(4),optional :: dy     (lb1:ub1,lb2:ub2) ! grid cell width in y-direction
    real(4),optional :: grdrot (lb1:ub1,lb2:ub2) ! grid cell rotation angle
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: imin,imax,jmin,jmax
    real(8) :: glon,glat,grot
    real(8) :: angle,cn1,cn2,cn3,cn4,cn5,cn6,cnx,cny,con1,con2,deg,dlon,gcon
    real(8) :: ogcon,rih,rr,x,xih,xx,y,yih,yy,grdi,grdj,distx,disty,hx,hy

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridCoord: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    if (cgs%ijg) then
      imin = lb1
      imax = ub1
      jmin = lb2
      jmax = ub2
    else
      imin = lb2
      imax = ub2
      jmin = lb1
      jmax = ub1
    endif

    ! Set grdrot to zero
    if (present(grdrot)) grdrot(:,:) = zero

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      gcon=zero
      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      do j = jmin, jmax
        do i = imin, imax
          grdi=real(i,8)
          grdj=real(j,8)
          rr=rih+(grdj-jref)*dely
          glat=(two*atan(exp(rr/con2))-pio2)*r2d
          glon=reflon+(grdi-iref)*r2d*delx/con2
          if (glon.gt.d360) glon=glon-d360
          if (glon.lt.zero) glon=glon+d360
          if (cgs%ijg) then
            grdlon(i,j) = glon
            grdlat(i,j) = glat
          else
            grdlon(j,i) = glon
            grdlat(j,i) = glat
          endif
      enddo
      enddo

      ! Calculate dx & dy
      if (present(dx).and.present(dy)) then
      do j = jmin, jmax
        do i = imin, imax
          if (cgs%ijg) then
            deg=grdlat(i,j)*d2r
            hx=cos(deg)/con1
            hy=hx
            dx(i,j)=delx*hx
            dy(i,j)=dely*hy
          else
            deg=grdlat(j,i)*d2r
            hx=cos(deg)/con1
            hy=hx
            dx(j,i)=delx*hx
            dy(j,i)=dely*hy
          endif
        enddo
      enddo
      endif

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do j = jmin, jmax
        do i = imin, imax
          grdi=real(i,8)
          grdj=real(j,8)
          distx=(grdi-real(iref,8))*delx
          disty=(grdj-real(jref,8))*dely
          x=xih+distx
          y=yih+disty
          rr=sqrt(x*x+y*y)
          glat=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
          xx= x
          yy=-y*ihem
          if (yy.eq.zero) then
            if (xx.le.zero) then
              angle=-d90
            elseif (xx.gt.zero) then
              angle= d90
            endif
          else
            angle=atan2(xx,yy)*r2d
          endif
          glon=stdlon+angle*ogcon
          if (glon.gt.d360) glon=glon-d360
          if (glon.lt.zero) glon=glon+d360
          if (cgs%ijg) then
            grdlon(i,j) = glon
            grdlat(i,j) = glat
          else
            grdlon(j,i) = glon
            grdlat(j,i) = glat
          endif
        enddo
      enddo

      ! Calculate angle between grid north and true north at every grid point.
      if (present(grdrot)) then
      do j = jmin, jmax
        do i = imin, imax
          if (cgs%ijg) then
            dlon=stdlon-grdlon(i,j)
          else
            dlon=stdlon-grdlon(j,i)
          endif
          if (abs(dlon).gt.d180) dlon=dlon-sign(d360,dlon)
          grot=dlon*gcon
          if (cgs%ijg) then
            grdrot(i,j) = grot
          else
            grdrot(j,i) = grot
          endif
        enddo
      enddo
      endif

      ! Calculate dx & dy
      if (present(dx).and.present(dy)) then
      do j = jmin, jmax
        do i = imin, imax
          if (cgs%ijg) then
            glat = grdlat(i,j)
          else
            glat = grdlat(j,i)
          endif
          deg = (d90-glat*real(ihem,8))*d2r
          cn5=sin(deg)
          deg=deg*half
          cn6=tan(deg)
          if (nproj .eq. 2) then
            hx=cn5/cn1*(cn6/cn3)**(-gcon)
          else
            hx=(one+sin(abs(glat)*d2r))/(one+sin(abs(stdlt1)*d2r))
          endif
          hy=hx
          if (cgs%ijg) then
            dx(i,j)=delx*hx
            dy(i,j)=dely*hy
          else
            dx(j,i)=delx*hx
            dy(j,i)=dely*hy
          endif
        enddo
      enddo
      endif

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      gcon=two
      cnx=delx*m2d
      cny=dely*m2d
      do j = jmin, jmax
        do i = imin, imax
          grdi=real(i,8)
          grdj=real(j,8)
          glat=(grdj-jref)*cny+reflat
          glon=(grdi-iref)*cnx+reflon
          if (glon.gt.d360) glon=glon-d360
          if (glon.lt.zero) glon=glon+d360
          if (cgs%ijg) then
            grdlon(i,j) = glon
            grdlat(i,j) = glat
          else
            grdlon(j,i) = glon
            grdlat(j,i) = glat
          endif
        enddo
      enddo

      ! Calculate dx & dy
      if (present(dx).and.present(dy)) then
        dx=delx
        dy=dely
      endif

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      gcon=3.d0
      cnx=delx
      cny=dely
      do j = jmin, jmax
        do i = imin, imax
          grdi=real(i,8)
          grdj=real(j,8)
          glat=(grdj-jref)*cny+reflat
          glon=(grdi-iref)*cnx+reflon
          if (glon.gt.d360) glon=glon-d360
          if (glon.lt.zero) glon=glon+d360
          if (cgs%ijg) then
            grdlon(i,j) = glon
            grdlat(i,j) = glat
          else
            grdlon(j,i) = glon
            grdlat(j,i) = glat
          endif
        enddo
      enddo

      ! Calculate dx & dy
      if (present(dx).and.present(dy)) then
      cnx=delx*d2m
      cny=dely*d2m
      do j = jmin, jmax
        do i = imin, imax
          if (cgs%ijg) then
            deg=grdlat(i,j)*d2r
            hx=cos(deg)
            if (hx.lt.zero) hx=zero
            hy=one
            dx(i,j)=cnx*hx
            dy(i,j)=cny*hy
          else
            deg=grdlat(j,i)*d2r
            hx=cos(deg)
            if (hx.lt.zero) hx=zero
            hy=one
            dx(j,i)=cnx*hx
            dy(j,i)=cny*hy
          endif
        enddo
      enddo
      endif

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridCoord:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridCoord_r8(cgs,lb1,ub1,lb2,ub2,grdlon,grdlat,dx,dy,grdrot,rc)
    !***********************************************************************
    ! Adapted from coampslib/grid.F & grdij.F
    ! Double precision interface
    !***********************************************************************
    type(CGrid)      :: cgs
    integer          :: lb1, ub1, lb2, ub2
    real(8)          :: grdlon (lb1:ub1,lb2:ub2) ! longitude
    real(8)          :: grdlat (lb1:ub1,lb2:ub2) ! latitude
    real(8),optional :: dx     (lb1:ub1,lb2:ub2) ! grid cell width in x-direction
    real(8),optional :: dy     (lb1:ub1,lb2:ub2) ! grid cell width in y-direction
    real(8),optional :: grdrot (lb1:ub1,lb2:ub2) ! grid cell rotation angle
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: imin,imax,jmin,jmax
    real(8) :: glon,glat,grot
    real(8) :: angle,cn1,cn2,cn3,cn4,cn5,cn6,cnx,cny,con1,con2,deg,dlon,gcon
    real(8) :: ogcon,rih,rr,x,xih,xx,y,yih,yy,grdi,grdj,distx,disty,hx,hy

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridCoord: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    if (cgs%ijg) then
      imin = lb1
      imax = ub1
      jmin = lb2
      jmax = ub2
    else
      imin = lb2
      imax = ub2
      jmin = lb1
      jmax = ub1
    endif

    ! Set grdrot to zero
    if (present(grdrot)) grdrot(:,:) = zero

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      gcon=zero
      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      do j = jmin, jmax
        do i = imin, imax
          grdi=real(i,8)
          grdj=real(j,8)
          rr=rih+(grdj-jref)*dely
          glat=(two*atan(exp(rr/con2))-pio2)*r2d
          glon=reflon+(grdi-iref)*r2d*delx/con2
          if (glon.gt.d360) glon=glon-d360
          if (glon.lt.zero) glon=glon+d360
          if (cgs%ijg) then
            grdlon(i,j) = glon
            grdlat(i,j) = glat
          else
            grdlon(j,i) = glon
            grdlat(j,i) = glat
          endif
        enddo
      enddo

      ! Calculate dx & dy
      if (present(dx).and.present(dy)) then
      do j = jmin, jmax
        do i = imin, imax
          if (cgs%ijg) then
            deg=grdlat(i,j)*d2r
            hx=cos(deg)/con1
            hy=hx
            dx(i,j)=delx*hx
            dy(i,j)=dely*hy
          else
            deg=grdlat(j,i)*d2r
            hx=cos(deg)/con1
            hy=hx
            dx(j,i)=delx*hx
            dy(j,i)=dely*hy
          endif
        enddo
      enddo
      endif

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do j = jmin, jmax
        do i = imin, imax
          grdi=real(i,8)
          grdj=real(j,8)
          distx=(grdi-real(iref,8))*delx
          disty=(grdj-real(jref,8))*dely
          x=xih+distx
          y=yih+disty
          rr=sqrt(x*x+y*y)
          glat=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
          xx= x
          yy=-y*ihem
          if (yy.eq.zero) then
            if (xx.le.zero) then
              angle=-d90
            elseif (xx.gt.zero) then
              angle= d90
            endif
          else
            angle=atan2(xx,yy)*r2d
          endif
          glon=stdlon+angle*ogcon
          if (glon.gt.d360) glon=glon-d360
          if (glon.lt.zero) glon=glon+d360
          if (cgs%ijg) then
            grdlon(i,j) = glon
            grdlat(i,j) = glat
          else
            grdlon(j,i) = glon
            grdlat(j,i) = glat
          endif
        enddo
      enddo

      ! Calculate angle between grid north and true north at every grid point.
      if (present(grdrot)) then
      do j = jmin, jmax
        do i = imin, imax
          if (cgs%ijg) then
            dlon=stdlon-grdlon(i,j)
          else
            dlon=stdlon-grdlon(j,i)
          endif
          if (abs(dlon).gt.d180) dlon=dlon-sign(d360,dlon)
          grot=dlon*gcon
          if (cgs%ijg) then
            grdrot(i,j) = grot
          else
            grdrot(j,i) = grot
          endif
        enddo
      enddo
      endif

      ! Calculate dx & dy
      if (present(dx).and.present(dy)) then
      do j = jmin, jmax
        do i = imin, imax
          if (cgs%ijg) then
            glat = grdlat(i,j)
          else
            glat = grdlat(j,i)
          endif
          deg = (d90-glat*real(ihem,8))*d2r
          cn5=sin(deg)
          deg=deg*half
          cn6=tan(deg)
          if (nproj .eq. 2) then
            hx=cn5/cn1*(cn6/cn3)**(-gcon)
          else
            hx=(one+sin(abs(glat)*d2r))/(one+sin(abs(stdlt1)*d2r))
          endif
          hy=hx
          if (cgs%ijg) then
            dx(i,j)=delx*hx
            dy(i,j)=dely*hy
          else
            dx(j,i)=delx*hx
            dy(j,i)=dely*hy
          endif
        enddo
      enddo
      endif

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      gcon=two
      cnx=delx*m2d
      cny=dely*m2d
      do j = jmin, jmax
        do i = imin, imax
          grdi=real(i,8)
          grdj=real(j,8)
          glat=(grdj-jref)*cny+reflat
          glon=(grdi-iref)*cnx+reflon
          if (glon.gt.d360) glon=glon-d360
          if (glon.lt.zero) glon=glon+d360
          if (cgs%ijg) then
            grdlon(i,j) = glon
            grdlat(i,j) = glat
          else
            grdlon(j,i) = glon
            grdlat(j,i) = glat
          endif
        enddo
      enddo

      ! Calculate dx & dy
      if (present(dx).and.present(dy)) then
        dx=delx
        dy=dely
      endif

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      gcon=3.d0
      cnx=delx
      cny=dely
      do j = jmin, jmax
        do i = imin, imax
          grdi=real(i,8)
          grdj=real(j,8)
          glat=(grdj-jref)*cny+reflat
          glon=(grdi-iref)*cnx+reflon
          if (glon.gt.d360) glon=glon-d360
          if (glon.lt.zero) glon=glon+d360
          if (cgs%ijg) then
            grdlon(i,j) = glon
            grdlat(i,j) = glat
          else
            grdlon(j,i) = glon
            grdlat(j,i) = glat
          endif
        enddo
      enddo

      ! Calculate dx & dy
      if (present(dx).and.present(dy)) then
      cnx=delx*d2m
      cny=dely*d2m
      do j = jmin, jmax
        do i = imin, imax
          if (cgs%ijg) then
            deg=grdlat(i,j)*d2r
            hx=cos(deg)
            if (hx.lt.zero) hx=zero
            hy=one
            dx(i,j)=cnx*hx
            dy(i,j)=cny*hy
          else
            deg=grdlat(j,i)*d2r
            hx=cos(deg)
            if (hx.lt.zero) hx=zero
            hy=one
            dx(j,i)=cnx*hx
            dy(j,i)=cny*hy
          endif
        enddo
      enddo
      endif

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridCoord:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridIJ2LL_array2_r4(cgs,grdi,grdj,grdlon,grdlat,rc)
    !***********************************************************************
    ! Adapted from coampslib/ij2ll.F
    ! Single precision 2d array interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(4)          :: grdi   (:,:)
    real(4)          :: grdj   (:,:)
    real(4)          :: grdlon (:,:)
    real(4)          :: grdlat (:,:)
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rr,x,xih,xx,y,yih,yy

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridIJ2LL: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    if (cgs%ijg) then
      lb1 = lbound(grdi,1)
      ub1 = ubound(grdi,1)
      lb2 = lbound(grdi,2)
      ub2 = ubound(grdi,2)
    else
      lb1 = lbound(grdi,2)
      ub1 = ubound(grdi,2)
      lb2 = lbound(grdi,1)
      ub2 = ubound(grdi,1)
    endif

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        rr=rih+(grdj(i,j)-jref)*dely
        grdlat(i,j)=(two*atan(exp(rr/con2))-pio2)*r2d
        grdlon(i,j)=reflon+(grdi(i,j)-iref)*r2d*delx/con2
        if (grdlon(i,j).gt.d360) grdlon(i,j)=grdlon(i,j)-d360
        if (grdlon(i,j).lt.zero) grdlon(i,j)=grdlon(i,j)+d360
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        rr=rih+(grdj(j,i)-jref)*dely
        grdlat(j,i)=(two*atan(exp(rr/con2))-pio2)*r2d
        grdlon(j,i)=reflon+(grdi(j,i)-iref)*r2d*delx/con2
        if (grdlon(j,i).gt.d360) grdlon(j,i)=grdlon(j,i)-d360
        if (grdlon(j,i).lt.zero) grdlon(j,i)=grdlon(j,i)+d360
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        x=xih+(grdi(i,j)-iref)*delx
        y=yih+(grdj(i,j)-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat(i,j)=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.zero) then
          if (xx.le.zero) then
            angle=-d90
          elseif (xx.gt.zero) then
            angle=d90
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon(i,j)=stdlon+angle*ogcon
        deg=grdlat(i,j)*d2r
        if (grdlon(i,j).gt.d360) grdlon(i,j)=grdlon(i,j)-d360
        if (grdlon(i,j).lt.zero) grdlon(i,j)=grdlon(i,j)+d360
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        x=xih+(grdi(j,i)-iref)*delx
        y=yih+(grdj(j,i)-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat(j,i)=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.zero) then
          if (xx.le.zero) then
            angle=-d90
          elseif (xx.gt.zero) then
            angle=d90
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon(j,i)=stdlon+angle*ogcon
        deg=grdlat(j,i)*d2r
        if (grdlon(j,i).gt.d360) grdlon(j,i)=grdlon(j,i)-d360
        if (grdlon(j,i).lt.zero) grdlon(j,i)=grdlon(j,i)+d360
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        grdlat(i,j)=(grdj(i,j)-jref)*cny+reflat
        grdlon(i,j)=(grdi(i,j)-iref)*cnx+reflon
        if (grdlon(i,j).gt.d360) grdlon(i,j)=grdlon(i,j)-d360
        if (grdlon(i,j).lt.zero) grdlon(i,j)=grdlon(i,j)+d360
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        grdlat(j,i)=(grdj(j,i)-jref)*cny+reflat
        grdlon(j,i)=(grdi(j,i)-iref)*cnx+reflon
        if (grdlon(j,i).gt.d360) grdlon(j,i)=grdlon(j,i)-d360
        if (grdlon(j,i).lt.zero) grdlon(j,i)=grdlon(j,i)+d360
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        grdlat(i,j)=(grdj(i,j)-jref)*cny+reflat
        grdlon(i,j)=(grdi(i,j)-iref)*cnx+reflon
        if (grdlon(i,j).gt.d360) grdlon(i,j)=grdlon(i,j)-d360
        if (grdlon(i,j).lt.zero) grdlon(i,j)=grdlon(i,j)+d360
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        grdlat(j,i)=(grdj(j,i)-jref)*cny+reflat
        grdlon(j,i)=(grdi(j,i)-iref)*cnx+reflon
        if (grdlon(j,i).gt.d360) grdlon(j,i)=grdlon(j,i)-d360
        if (grdlon(j,i).lt.zero) grdlon(j,i)=grdlon(j,i)+d360
      enddo
      enddo
      endif

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridIJ2LL:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridIJ2LL_array2_r8(cgs,grdi,grdj,grdlon,grdlat,rc)
    !***********************************************************************
    ! Adapted from coampslib/ij2ll.F
    ! Double precision 2d array interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(8)          :: grdi   (:,:)
    real(8)          :: grdj   (:,:)
    real(8)          :: grdlon (:,:)
    real(8)          :: grdlat (:,:)
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rr,x,xih,xx,y,yih,yy

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridIJ2LL: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    if (cgs%ijg) then
      lb1 = lbound(grdi,1)
      ub1 = ubound(grdi,1)
      lb2 = lbound(grdi,2)
      ub2 = ubound(grdi,2)
    else
      lb1 = lbound(grdi,2)
      ub1 = ubound(grdi,2)
      lb2 = lbound(grdi,1)
      ub2 = ubound(grdi,1)
    endif

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        rr=rih+(grdj(i,j)-jref)*dely
        grdlat(i,j)=(two*atan(exp(rr/con2))-pio2)*r2d
        grdlon(i,j)=reflon+(grdi(i,j)-iref)*r2d*delx/con2
        if (grdlon(i,j).gt.d360) grdlon(i,j)=grdlon(i,j)-d360
        if (grdlon(i,j).lt.zero) grdlon(i,j)=grdlon(i,j)+d360
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        rr=rih+(grdj(j,i)-jref)*dely
        grdlat(j,i)=(two*atan(exp(rr/con2))-pio2)*r2d
        grdlon(j,i)=reflon+(grdi(j,i)-iref)*r2d*delx/con2
        if (grdlon(j,i).gt.d360) grdlon(j,i)=grdlon(j,i)-d360
        if (grdlon(j,i).lt.zero) grdlon(j,i)=grdlon(j,i)+d360
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        x=xih+(grdi(i,j)-iref)*delx
        y=yih+(grdj(i,j)-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat(i,j)=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.zero) then
          if (xx.le.zero) then
            angle=-d90
          elseif (xx.gt.zero) then
            angle=d90
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon(i,j)=stdlon+angle*ogcon
        deg=grdlat(i,j)*d2r
        if (grdlon(i,j).gt.d360) grdlon(i,j)=grdlon(i,j)-d360
        if (grdlon(i,j).lt.zero) grdlon(i,j)=grdlon(i,j)+d360
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        x=xih+(grdi(j,i)-iref)*delx
        y=yih+(grdj(j,i)-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat(j,i)=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.zero) then
          if (xx.le.zero) then
            angle=-d90
          elseif (xx.gt.zero) then
            angle=d90
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon(j,i)=stdlon+angle*ogcon
        deg=grdlat(j,i)*d2r
        if (grdlon(j,i).gt.d360) grdlon(j,i)=grdlon(j,i)-d360
        if (grdlon(j,i).lt.zero) grdlon(j,i)=grdlon(j,i)+d360
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        grdlat(i,j)=(grdj(i,j)-jref)*cny+reflat
        grdlon(i,j)=(grdi(i,j)-iref)*cnx+reflon
        if (grdlon(i,j).gt.d360) grdlon(i,j)=grdlon(i,j)-d360
        if (grdlon(i,j).lt.zero) grdlon(i,j)=grdlon(i,j)+d360
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        grdlat(j,i)=(grdj(j,i)-jref)*cny+reflat
        grdlon(j,i)=(grdi(j,i)-iref)*cnx+reflon
        if (grdlon(j,i).gt.d360) grdlon(j,i)=grdlon(j,i)-d360
        if (grdlon(j,i).lt.zero) grdlon(j,i)=grdlon(j,i)+d360
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        grdlat(i,j)=(grdj(i,j)-jref)*cny+reflat
        grdlon(i,j)=(grdi(i,j)-iref)*cnx+reflon
        if (grdlon(i,j).gt.d360) grdlon(i,j)=grdlon(i,j)-d360
        if (grdlon(i,j).lt.zero) grdlon(i,j)=grdlon(i,j)+d360
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        grdlat(j,i)=(grdj(j,i)-jref)*cny+reflat
        grdlon(j,i)=(grdi(j,i)-iref)*cnx+reflon
        if (grdlon(j,i).gt.d360) grdlon(j,i)=grdlon(j,i)-d360
        if (grdlon(j,i).lt.zero) grdlon(j,i)=grdlon(j,i)+d360
      enddo
      enddo
      endif

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridIJ2LL:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridIJ2LL_array1_r4(cgs,grdi,grdj,grdlon,grdlat,rc)
    !***********************************************************************
    ! Adapted from coampslib/ij2ll.F
    ! Single precision 1d array interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(4)          :: grdi   (:)
    real(4)          :: grdj   (:)
    real(4)          :: grdlon (:)
    real(4)          :: grdlat (:)
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rr,x,xih,xx,y,yih,yy

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridIJ2LL: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    lb1 = lbound(grdi,1)
    ub1 = ubound(grdi,1)

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      do i = lb1, ub1
        rr=rih+(grdj(i)-jref)*dely
        grdlat(i)=(two*atan(exp(rr/con2))-pio2)*r2d
        grdlon(i)=reflon+(grdi(i)-iref)*r2d*delx/con2
        if (grdlon(i).gt.d360) grdlon(i)=grdlon(i)-d360
        if (grdlon(i).lt.zero) grdlon(i)=grdlon(i)+d360
      enddo

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do i = lb1, ub1
        x=xih+(grdi(i)-iref)*delx
        y=yih+(grdj(i)-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat(i)=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.zero) then
          if (xx.le.zero) then
            angle=-d90
          elseif (xx.gt.zero) then
            angle=d90
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon(i)=stdlon+angle*ogcon
        deg=grdlat(i)*d2r
        if (grdlon(i).gt.d360) grdlon(i)=grdlon(i)-d360
        if (grdlon(i).lt.zero) grdlon(i)=grdlon(i)+d360
      enddo

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
      do i = lb1, ub1
        grdlat(i)=(grdj(i)-jref)*cny+reflat
        grdlon(i)=(grdi(i)-iref)*cnx+reflon
        if (grdlon(i).gt.d360) grdlon(i)=grdlon(i)-d360
        if (grdlon(i).lt.zero) grdlon(i)=grdlon(i)+d360
      enddo

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
      do i = lb1, ub1
        grdlat(i)=(grdj(i)-jref)*cny+reflat
        grdlon(i)=(grdi(i)-iref)*cnx+reflon
        if (grdlon(i).gt.d360) grdlon(i)=grdlon(i)-d360
        if (grdlon(i).lt.zero) grdlon(i)=grdlon(i)+d360
      enddo

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridIJ2LL:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridIJ2LL_array1_r8(cgs,grdi,grdj,grdlon,grdlat,rc)
    !***********************************************************************
    ! Adapted from coampslib/ij2ll.F
    ! Double precision 1d array interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(8)          :: grdi   (:)
    real(8)          :: grdj   (:)
    real(8)          :: grdlon (:)
    real(8)          :: grdlat (:)
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rr,x,xih,xx,y,yih,yy

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridIJ2LL: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    lb1 = lbound(grdi,1)
    ub1 = ubound(grdi,1)

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      do i = lb1, ub1
        rr=rih+(grdj(i)-jref)*dely
        grdlat(i)=(two*atan(exp(rr/con2))-pio2)*r2d
        grdlon(i)=reflon+(grdi(i)-iref)*r2d*delx/con2
        if (grdlon(i).gt.d360) grdlon(i)=grdlon(i)-d360
        if (grdlon(i).lt.zero) grdlon(i)=grdlon(i)+d360
      enddo

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do i = lb1, ub1
        x=xih+(grdi(i)-iref)*delx
        y=yih+(grdj(i)-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat(i)=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.zero) then
          if (xx.le.zero) then
            angle=-d90
          elseif (xx.gt.zero) then
            angle=d90
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon(i)=stdlon+angle*ogcon
        deg=grdlat(i)*d2r
        if (grdlon(i).gt.d360) grdlon(i)=grdlon(i)-d360
        if (grdlon(i).lt.zero) grdlon(i)=grdlon(i)+d360
      enddo

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
      do i = lb1, ub1
        grdlat(i)=(grdj(i)-jref)*cny+reflat
        grdlon(i)=(grdi(i)-iref)*cnx+reflon
        if (grdlon(i).gt.d360) grdlon(i)=grdlon(i)-d360
        if (grdlon(i).lt.zero) grdlon(i)=grdlon(i)+d360
      enddo

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
      do i = lb1, ub1
        grdlat(i)=(grdj(i)-jref)*cny+reflat
        grdlon(i)=(grdi(i)-iref)*cnx+reflon
        if (grdlon(i).gt.d360) grdlon(i)=grdlon(i)-d360
        if (grdlon(i).lt.zero) grdlon(i)=grdlon(i)+d360
      enddo

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridIJ2LL:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridIJ2LL_point_r4(cgs,grdi,grdj,grdlon,grdlat,rc)
    !***********************************************************************
    ! Adapted from coampslib/ij2ll.F
    ! Single precision point interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(4)          :: grdi
    real(4)          :: grdj
    real(4)          :: grdlon
    real(4)          :: grdlat
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rr,x,xih,xx,y,yih,yy

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridIJ2LL: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
        rr=rih+(grdj-jref)*dely
        grdlat=(two*atan(exp(rr/con2))-pio2)*r2d
        grdlon=reflon+(grdi-iref)*r2d*delx/con2
        if (grdlon.gt.d360) grdlon=grdlon-d360
        if (grdlon.lt.zero) grdlon=grdlon+d360

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

        x=xih+(grdi-iref)*delx
        y=yih+(grdj-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.zero) then
          if (xx.le.zero) then
            angle=-d90
          elseif (xx.gt.zero) then
            angle=d90
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon=stdlon+angle*ogcon
        deg=grdlat*d2r
        if (grdlon.gt.d360) grdlon=grdlon-d360
        if (grdlon.lt.zero) grdlon=grdlon+d360

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
        grdlat=(grdj-jref)*cny+reflat
        grdlon=(grdi-iref)*cnx+reflon
        if (grdlon.gt.d360) grdlon=grdlon-d360
        if (grdlon.lt.zero) grdlon=grdlon+d360

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
        grdlat=(grdj-jref)*cny+reflat
        grdlon=(grdi-iref)*cnx+reflon
        if (grdlon.gt.d360) grdlon=grdlon-d360
        if (grdlon.lt.zero) grdlon=grdlon+d360

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridIJ2LL:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridIJ2LL_point_r8(cgs,grdi,grdj,grdlon,grdlat,rc)
    !***********************************************************************
    ! Adapted from coampslib/ij2ll.F
    ! Double precision point interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(8)          :: grdi
    real(8)          :: grdj
    real(8)          :: grdlon
    real(8)          :: grdlat
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rr,x,xih,xx,y,yih,yy

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridIJ2LL: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
        rr=rih+(grdj-jref)*dely
        grdlat=(two*atan(exp(rr/con2))-pio2)*r2d
        grdlon=reflon+(grdi-iref)*r2d*delx/con2
        if (grdlon.gt.d360) grdlon=grdlon-d360
        if (grdlon.lt.zero) grdlon=grdlon+d360

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

        x=xih+(grdi-iref)*delx
        y=yih+(grdj-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat=r2d*(pio2-two*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.zero) then
          if (xx.le.zero) then
            angle=-d90
          elseif (xx.gt.zero) then
            angle=d90
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon=stdlon+angle*ogcon
        deg=grdlat*d2r
        if (grdlon.gt.d360) grdlon=grdlon-d360
        if (grdlon.lt.zero) grdlon=grdlon+d360

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
        grdlat=(grdj-jref)*cny+reflat
        grdlon=(grdi-iref)*cnx+reflon
        if (grdlon.gt.d360) grdlon=grdlon-d360
        if (grdlon.lt.zero) grdlon=grdlon+d360

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
        grdlat=(grdj-jref)*cny+reflat
        grdlon=(grdi-iref)*cnx+reflon
        if (grdlon.gt.d360) grdlon=grdlon-d360
        if (grdlon.lt.zero) grdlon=grdlon+d360

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridIJ2LL:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridLL2IJ_array2_r4(cgs,grdlon,grdlat,grdi,grdj,rc)
    !***********************************************************************
    ! Adapted from coampslib/ll2ij.F
    ! Single precision 2d array interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(4)          :: grdlon (:,:)
    real(4)          :: grdlat (:,:)
    real(4)          :: grdi   (:,:)
    real(4)          :: grdj   (:,:)
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rrih,x,xih,y,yih
    real(8) :: check,alnfix,alon

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridLL2IJ: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    if (cgs%ijg) then
      lb1 = lbound(grdlon,1)
      ub1 = ubound(grdlon,1)
      lb2 = lbound(grdlon,2)
      ub2 = ubound(grdlon,2)
    else
      lb1 = lbound(grdlon,2)
      ub1 = ubound(grdlon,2)
      lb2 = lbound(grdlon,1)
      ub2 = ubound(grdlon,1)
    endif

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        alon=grdlon(i,j)+d180-reflon
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        grdi(i,j)=iref+(alon-d180)*con2/(r2d*delx)
        deg=grdlat(i,j)*d2r+pio2
        deg=deg*half
        grdj(i,j)=jref+(con2*log(tan(deg))-rih)/dely
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        alon=grdlon(j,i)+d180-reflon
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        grdi(j,i)=iref+(alon-d180)*con2/(r2d*delx)
        deg=grdlat(j,i)*d2r+pio2
        deg=deg*half
        grdj(j,i)=jref+(con2*log(tan(deg))-rih)/dely
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        deg=(d90-grdlat(i,j)*ihem)*half*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=d180-stdlon
        alnfix=stdlon+check
        alon=grdlon(i,j)+check
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi(i,j)=iref+(x-xih)/delx
        grdj(i,j)=jref+(y-yih)/dely
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        deg=(d90-grdlat(j,i)*ihem)*half*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=d180-stdlon
        alnfix=stdlon+check
        alon=grdlon(j,i)+check
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi(j,i)=iref+(x-xih)/delx
        grdj(j,i)=jref+(y-yih)/dely
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        grdi(i,j)=iref+(grdlon(i,j)-reflon)/cnx
        grdj(i,j)=jref+(grdlat(i,j)-reflat)/cny
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        grdi(j,i)=iref+(grdlon(j,i)-reflon)/cnx
        grdj(j,i)=jref+(grdlat(j,i)-reflat)/cny
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        grdi(i,j)=iref+(grdlon(i,j)-reflon)/cnx
        grdj(i,j)=jref+(grdlat(i,j)-reflat)/cny
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        grdi(j,i)=iref+(grdlon(j,i)-reflon)/cnx
        grdj(j,i)=jref+(grdlat(j,i)-reflat)/cny
      enddo
      enddo
      endif

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridLL2IJ:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridLL2IJ_array2_r8(cgs,grdlon,grdlat,grdi,grdj,rc)
    !***********************************************************************
    ! Adapted from coampslib/ll2ij.F
    ! Double precision 2d array interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(8)          :: grdlon (:,:)
    real(8)          :: grdlat (:,:)
    real(8)          :: grdi   (:,:)
    real(8)          :: grdj   (:,:)
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rrih,x,xih,y,yih
    real(8) :: check,alnfix,alon

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridLL2IJ: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    if (cgs%ijg) then
      lb1 = lbound(grdlon,1)
      ub1 = ubound(grdlon,1)
      lb2 = lbound(grdlon,2)
      ub2 = ubound(grdlon,2)
    else
      lb1 = lbound(grdlon,2)
      ub1 = ubound(grdlon,2)
      lb2 = lbound(grdlon,1)
      ub2 = ubound(grdlon,1)
    endif

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        alon=grdlon(i,j)+d180-reflon
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        grdi(i,j)=iref+(alon-d180)*con2/(r2d*delx)
        deg=grdlat(i,j)*d2r+pio2
        deg=deg*half
        grdj(i,j)=jref+(con2*log(tan(deg))-rih)/dely
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        alon=grdlon(j,i)+d180-reflon
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        grdi(j,i)=iref+(alon-d180)*con2/(r2d*delx)
        deg=grdlat(j,i)*d2r+pio2
        deg=deg*half
        grdj(j,i)=jref+(con2*log(tan(deg))-rih)/dely
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        deg=(d90-grdlat(i,j)*ihem)*half*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=d180-stdlon
        alnfix=stdlon+check
        alon=grdlon(i,j)+check
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi(i,j)=iref+(x-xih)/delx
        grdj(i,j)=jref+(y-yih)/dely
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        deg=(d90-grdlat(j,i)*ihem)*half*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=d180-stdlon
        alnfix=stdlon+check
        alon=grdlon(j,i)+check
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi(j,i)=iref+(x-xih)/delx
        grdj(j,i)=jref+(y-yih)/dely
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        grdi(i,j)=iref+(grdlon(i,j)-reflon)/cnx
        grdj(i,j)=jref+(grdlat(i,j)-reflat)/cny
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        grdi(j,i)=iref+(grdlon(j,i)-reflon)/cnx
        grdj(j,i)=jref+(grdlat(j,i)-reflat)/cny
      enddo
      enddo
      endif

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
      if (cgs%ijg) then
      do j = lb2, ub2
      do i = lb1, ub1
        grdi(i,j)=iref+(grdlon(i,j)-reflon)/cnx
        grdj(i,j)=jref+(grdlat(i,j)-reflat)/cny
      enddo
      enddo
      else
      do i = lb2, ub2
      do j = lb1, ub1
        grdi(j,i)=iref+(grdlon(j,i)-reflon)/cnx
        grdj(j,i)=jref+(grdlat(j,i)-reflat)/cny
      enddo
      enddo
      endif

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridLL2IJ:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridLL2IJ_array1_r4(cgs,grdlon,grdlat,grdi,grdj,rc)
    !***********************************************************************
    ! Adapted from coampslib/ll2ij.F
    ! Single precision 1d array interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(4)          :: grdlon (:)
    real(4)          :: grdlat (:)
    real(4)          :: grdi   (:)
    real(4)          :: grdj   (:)
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rrih,x,xih,y,yih
    real(8) :: check,alnfix,alon

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridLL2IJ: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    lb1 = lbound(grdlon,1)
    ub1 = ubound(grdlon,1)

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      do i = lb1, ub1
        alon=grdlon(i)+d180-reflon
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        grdi(i)=iref+(alon-d180)*con2/(r2d*delx)
        deg=grdlat(i)*d2r+pio2
        deg=deg*half
        grdj(i)=jref+(con2*log(tan(deg))-rih)/dely
      enddo

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do i = lb1, ub1
        deg=(d90-grdlat(i)*ihem)*half*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=d180-stdlon
        alnfix=stdlon+check
        alon=grdlon(i)+check
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi(i)=iref+(x-xih)/delx
        grdj(i)=jref+(y-yih)/dely
      enddo

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
      do i = lb1, ub1
        grdi(i)=iref+(grdlon(i)-reflon)/cnx
        grdj(i)=jref+(grdlat(i)-reflat)/cny
      enddo

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
      do i = lb1, ub1
        grdi(i)=iref+(grdlon(i)-reflon)/cnx
        grdj(i)=jref+(grdlat(i)-reflat)/cny
      enddo

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridLL2IJ:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridLL2IJ_array1_r8(cgs,grdlon,grdlat,grdi,grdj,rc)
    !***********************************************************************
    ! Adapted from coampslib/ll2ij.F
    ! Double precision 1d array interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(8)          :: grdlon (:)
    real(8)          :: grdlat (:)
    real(8)          :: grdi   (:)
    real(8)          :: grdj   (:)
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rrih,x,xih,y,yih
    real(8) :: check,alnfix,alon

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridLL2IJ: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    ! array bounds
    lb1 = lbound(grdlon,1)
    ub1 = ubound(grdlon,1)

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
      do i = lb1, ub1
        alon=grdlon(i)+d180-reflon
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        grdi(i)=iref+(alon-d180)*con2/(r2d*delx)
        deg=grdlat(i)*d2r+pio2
        deg=deg*half
        grdj(i)=jref+(con2*log(tan(deg))-rih)/dely
      enddo

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do i = lb1, ub1
        deg=(d90-grdlat(i)*ihem)*half*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=d180-stdlon
        alnfix=stdlon+check
        alon=grdlon(i)+check
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi(i)=iref+(x-xih)/delx
        grdj(i)=jref+(y-yih)/dely
      enddo

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
      do i = lb1, ub1
        grdi(i)=iref+(grdlon(i)-reflon)/cnx
        grdj(i)=jref+(grdlat(i)-reflat)/cny
      enddo

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
      do i = lb1, ub1
        grdi(i)=iref+(grdlon(i)-reflon)/cnx
        grdj(i)=jref+(grdlat(i)-reflat)/cny
      enddo

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridLL2IJ:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridLL2IJ_point_r4(cgs,grdlon,grdlat,grdi,grdj,rc)
    !***********************************************************************
    ! Adapted from coampslib/ll2ij.F
    ! Single precision point interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(4)          :: grdlon
    real(4)          :: grdlat
    real(4)          :: grdi
    real(4)          :: grdj
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rrih,x,xih,y,yih
    real(8) :: check,alnfix,alon

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridLL2IJ: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
        alon=grdlon+d180-reflon
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        grdi=iref+(alon-d180)*con2/(r2d*delx)
        deg=grdlat*d2r+pio2
        deg=deg*half
        grdj=jref+(con2*log(tan(deg))-rih)/dely

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

        deg=(d90-grdlat*ihem)*half*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=d180-stdlon
        alnfix=stdlon+check
        alon=grdlon+check
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi=iref+(x-xih)/delx
        grdj=jref+(y-yih)/dely

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
        grdi=iref+(grdlon-reflon)/cnx
        grdj=jref+(grdlat-reflat)/cny

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
        grdi=iref+(grdlon-reflon)/cnx
        grdj=jref+(grdlat-reflat)/cny

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridLL2IJ:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridLL2IJ_point_r8(cgs,grdlon,grdlat,grdi,grdj,rc)
    !***********************************************************************
    ! Adapted from coampslib/ll2ij.F
    ! Double precision point interface
    !***********************************************************************
    type(CGrid)      :: cgs
    real(8)          :: grdlon
    real(8)          :: grdlat
    real(8)          :: grdi
    real(8)          :: grdj
    integer,optional :: rc

    ! local variables
    integer :: i,j,ihem
    integer :: lb1,ub1,lb2,ub2
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,deg,gcon
    real(8) :: ogcon,rih,rrih,x,xih,y,yih
    real(8) :: check,alnfix,alon

    ! local grid reference
    integer :: nproj
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    nproj  = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref   = cgs%iref
    jref   = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx   = cgs%delx
    dely   = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure nproj is an acceptable value
    if (nproj.lt.1.or.nproj.gt.5) then
      print 800
      print 805,nproj
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, msg='CGridLL2IJ: unsupported nproj', &
        CONTEXT, rcToReturn=rc)
      return ! bail out
    endif

    !
    !***********************************************************************
    !          mercator projection (nproj=1)
    !***********************************************************************
    !
    if (nproj.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=rearth*con1
      deg=reflat*half*d2r
      rih=con2*log(tan(pio4+deg))
        alon=grdlon+d180-reflon
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        grdi=iref+(alon-d180)*con2/(r2d*delx)
        deg=grdlat*d2r+pio2
        deg=deg*half
        grdj=jref+(con2*log(tan(deg))-rih)/dely

    !
    !***********************************************************************
    !          lambert conformal (nproj=2) or
    !          polar stereographic (nproj=3)
    !***********************************************************************
    !
    elseif (nproj.eq.2.or.nproj.eq.3) then

      if (nproj.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((d90-abs(stdlt1))*d2r)) &
               -log(sin((d90-abs(stdlt2))*d2r))) &
              /(log(tan((d90-abs(stdlt1))*half*d2r)) &
               -log(tan((d90-abs(stdlt2))*half*d2r)))
        endif
      else
        gcon=one
      endif
      ogcon=one/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(d90-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=rearth*cn1*ogcon
      deg=deg*half
      cn3=tan(deg)
      deg=(d90-abs(reflat))*half*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

        deg=(d90-grdlat*ihem)*half*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=d180-stdlon
        alnfix=stdlon+check
        alon=grdlon+check
        if (alon.lt.zero) alon=alon+d360
        if (alon.gt.d360) alon=alon-d360
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi=iref+(x-xih)/delx
        grdj=jref+(y-yih)/dely

    !
    !***********************************************************************
    !          analytic grid (nproj=4) (delx/dely in meters)
    !***********************************************************************
    !
    elseif (nproj.eq.4) then

      cnx=delx*m2d
      cny=dely*m2d
        grdi=iref+(grdlon-reflon)/cnx
        grdj=jref+(grdlat-reflat)/cny

    !
    !***********************************************************************
    !          spherical grid (nproj=5) (delx/dely in degrees)
    !***********************************************************************
    !
    elseif (nproj.eq.5) then

      cnx=delx
      cny=dely
        grdi=iref+(grdlon-reflon)/cnx
        grdj=jref+(grdlat-reflat)/cny

    endif
    !
    !********************************************************************
    !          format statements
    !********************************************************************
    !
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridLL2IJ:',/ &
            ,'   nproj must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  function FFileCreate_CGrid(cgs, fldnam, lvltyp, rlev, rc) result (ffs)
    type(CGrid)       :: cgs
    character(6)      :: fldnam     ! field name
    character(24)     :: lvltyp     ! level type
    real(4)           :: rlev(2)    ! min/max levels
    integer, optional :: rc
    type(FFile)       :: ffs

    ! local variables
    ! none

    if (present(rc)) rc = ESMF_SUCCESS

    ffs%fldnam = fldnam
    ffs%lvltyp = lvltyp
    ffs%rlev   = rlev
    ffs%nest   = cgs%nest
    ffs%cfluid = cgs%cfluid
    ffs%dims(1)= cgs%m
    ffs%dims(2)= cgs%n
    ffs%dims(3)= 1

  end function

  !-----------------------------------------------------------------------------

  function FFileCreate_Array(array, nest, cfluid, fldnam, lvltyp, rlev, rc) result (ffs)
    type(ESMF_Array)  :: array
    integer           :: nest       ! nest identifier
    character(1)      :: cfluid     ! fluid type
    character(6)      :: fldnam     ! field name
    character(24)     :: lvltyp     ! level type
    real(4)           :: rlev(2)    ! min/max levels
    integer, optional :: rc
    type(FFile)       :: ffs

    ! local variables
    integer           :: minIdx(2,1), maxIdx(2,1)
    integer           :: udlb(1), udub(1)
    integer           :: ndim, dims(3)

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array, minIndexPTile=minIdx, maxIndexPTile=maxIdx, &
      rank=ndim, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (.not.(ndim.eq.2.or.ndim.eq.3)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FFileCreate: array rank must be 2 or 3')
      return ! bail out
    endif
    dims(1) = maxIdx(1,1)-minIdx(1,1)+1
    dims(2) = maxIdx(2,1)-minIdx(2,1)+1
    dims(3) = 1

    if (ndim.eq.3) then
      call ESMF_ArrayGet(array, undistLBound=udlb, undistUBound=udub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      dims(3) = udub(1)-udlb(1)+1
    endif

    ffs%fldnam = fldnam
    ffs%lvltyp = lvltyp
    ffs%rlev   = rlev
    ffs%nest   = nest
    ffs%cfluid = cfluid
    ffs%dims   = dims

  end function

  !-----------------------------------------------------------------------------

  function FFileCreate_Field(field, nest, cfluid, fldnam, lvltyp, rlev, rc) result (ffs)
    type(ESMF_Field)  :: field
    integer           :: nest       ! nest identifier
    character(1)      :: cfluid     ! fluid type
    character(6)      :: fldnam     ! field name
    character(24)     :: lvltyp     ! level type
    real(4)           :: rlev(2)    ! min/max levels
    integer, optional :: rc
    type(FFile)       :: ffs

    ! local variables
    type(ESMF_Array)  :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, array=array, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ffs = FFileCreate(array, nest, cfluid, fldnam, lvltyp, rlev, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end function

  !-----------------------------------------------------------------------------

  subroutine FFileSetDims_Array(ffs, array, rc)
    type(FFile)       :: ffs
    type(ESMF_Array)  :: array
    integer, optional :: rc

    ! local variables
    integer           :: minIdx(2,1), maxIdx(2,1)
    integer           :: udlb(1), udub(1)
    integer           :: ndim, dims(3)

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array, minIndexPTile=minIdx, maxIndexPTile=maxIdx, &
      rank=ndim, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (.not.(ndim.eq.2.or.ndim.eq.3)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FFileSetDims: array rank must be 2 or 3')
      return ! bail out
    endif
    dims(1) = maxIdx(1,1)-minIdx(1,1)+1
    dims(2) = maxIdx(2,1)-minIdx(2,1)+1
    dims(3) = 1

    if (ndim.eq.3) then
      call ESMF_ArrayGet(array, undistLBound=udlb, undistUBound=udub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      dims(3) = udub(1)-udlb(1)+1
    endif

    ffs%dims   = dims

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FFileSetDims_Field(ffs, field, rc)
    type(FFile)       :: ffs
    type(ESMF_Field)  :: field
    integer, optional :: rc

    ! local variables
    type(ESMF_Array)  :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, array=array, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    call FFileSetDims(ffs, array, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FFilePrint(ffs)
    type(FFile)  :: ffs

    ! local variables
    ! none

    write(*,'(a,1a6)')    'FFilePrint: fldnam: ',ffs%fldnam
    write(*,'(a,1a24)')   'FFilePrint: lvltyp: ',ffs%lvltyp
    write(*,'(a,2e14.6)') 'FFilePrint:   rlev: ',ffs%rlev(:)
    write(*,'(a,1i1)')    'FFilePrint:   nest: ',ffs%nest
    write(*,'(a,1a1)')    'FFilePrint: cfluid: ',ffs%cfluid
    write(*,'(a,3i6)')    'FFilePrint:   dims: ',ffs%dims(:)
    write(*,'(a,4i4)')    'FFilePrint:   dtgv: ',ffs%dtgv(:)
    write(*,'(a,3i4)')    'FFilePrint:   tauv: ',ffs%tauv(:)
    write(*,'(a,1a7)')    'FFilePrint: outtyp: ',ffs%outtyp

  end subroutine

  !-----------------------------------------------------------------------------

  function FFileName(ffs, rc) result (ffn)
    type(FFile)        :: ffs
    integer, optional  :: rc
    character(64)      :: ffn

    ! local variables
    character(32)      :: mdltyp = 'coamps'
    character(3)       :: cltype
    integer            :: ilev1, ilev2, irem1
    character(10)      :: cdtg

    if (present(rc)) rc = ESMF_SUCCESS

! 64 character flat-file name reference
! seatmp_sfc_000000_000000_1o0254x0308_2003020112_00000000_fcstfld
! 1234567890123456789012345678901234567890123456789012345678901234

    ilev1 = ffs%rlev(1)
    ilev2 = ffs%rlev(2)
    irem1 = 0

    write(cdtg,'(i4.4,3i2.2)') ffs%dtgv(1),ffs%dtgv(2),ffs%dtgv(3),ffs%dtgv(4)

    if (ffs%lvltyp.eq.'isbr_lvl'.or.ffs%lvltyp.eq.'dpth_sfc') then
      cltype = 'pre'
      if (ffs%rlev(1).lt.1.0) then
        ilev1 = 0
        irem1 = ffs%rlev(1)*10
      elseif (ffs%rlev(1).gt.1001.0) then
        irem1 = ffs%rlev(1)*10
        irem1 = irem1-ilev1*10
        irem1 = 2
      endif
    elseif (ffs%lvltyp.eq.'sgma_lvl') then
      cltype = 'sig'
      if (ffs%rlev(1).le.1.0) then
        ilev1 = ffs%rlev(1)*10000
      endif
    elseif (ffs%lvltyp.eq.'msl') then
      cltype = 'msl'
    elseif (ffs%lvltyp.eq.'ht_sfc') then
      cltype = 'zht'
    elseif (ffs%lvltyp.eq.'surface') then
      cltype = 'sfc'
    elseif (ffs%lvltyp.eq.'marn_lvl') then
      cltype = 'msl'
    elseif (ffs%lvltyp.eq.'trpp_lvl') then
      cltype = 'top'
    elseif (ffs%lvltyp.eq.'isth_lvl') then
      cltype = 'ist'
    else
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FFileName: unknown lvltyp: '//trim(ffs%lvltyp))
      return ! bail out
    endif

    if (mdltyp(1:6).eq.'NOGAPS'.or.mdltyp(1:6).eq.'nogaps') then
      if (ffs%lvltyp.eq.'isbr_lvl'.and.ffs%rlev(1).gt.1001.0) then
        write(ffn,826) ffs%fldnam,cltype,ilev1,irem1,ilev2, &
          ffs%dims(1),ffs%dims(2),cdtg,ffs%tauv(1),ffs%tauv(2),ffs%tauv(3),ffs%outtyp
      else
        write(ffn,825) ffs%fldnam,cltype,ilev1,ilev2, &
          ffs%dims(1),ffs%dims(2),cdtg,ffs%tauv(1),ffs%tauv(2),ffs%tauv(3),ffs%outtyp
      endif
    else
      write(ffn,830) ffs%fldnam,cltype,ilev1,ilev2,ffs%nest,ffs%cfluid, &
        ffs%dims(1),ffs%dims(2),cdtg,ffs%tauv(1),ffs%tauv(2),ffs%tauv(3),ffs%outtyp
    endif

825 format (a6,'_',a3,'_',i4.4,'.0','_',i4.4,'.0','_','glob', &
      i3,'x',i3,'_',a10,'_',i4.4,2i2.2,'_',a7)
826 format (a6,'_',a3,'_',i4.4,'.',i1,'_',i4.4,'.0','_','glob', &
      i3,'x',i3,'_',a10,'_',i4.4,2i2.2,'_',a7)
830 format (a6,'_',a3,'_',i6.6,'_',i6.6,'_',i1,a1, &
      i4.4,'x',i4.4,'_',a10,'_',i4.4,2i2.2,'_',a7)

  end function

  !-----------------------------------------------------------------------------

  subroutine FFileSetTime(ffs, dtgTime, tauTime, rc)
    type(FFile)       :: ffs
    type(ESMF_Time)   :: dtgTime
    type(ESMF_Time)   :: tauTime
    integer, optional :: rc

    ! local variables
    ! none

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_TimeGet(dtgTime, yy=ffs%dtgv(1), mm=ffs%dtgv(2), &
      dd=ffs%dtgv(3), h=ffs%dtgv(4), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_TimeIntervalGet(tauTime-dtgTime, h=ffs%tauv(1), m=ffs%tauv(2), &
      s=ffs%tauv(3), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FFileRead_Array(ffs, array, vm, dir, dtgTime, tauTime, outtyp, rc)
    type(FFile)       :: ffs
    type(ESMF_Array)  :: array
    type(ESMF_VM)     :: vm
    character(*)      :: dir
    type(ESMF_Time)   :: dtgTime
    type(ESMF_Time)   :: tauTime
    character(7)      :: outtyp
    integer, optional :: rc

    ! local variables
    integer, parameter :: rootPet = 0
    integer, parameter :: tile = 1
    integer, parameter :: lde = 0
    integer            :: stat, iunit, llen, nPets, lPet, ndim
    character(64)      :: msg=' '
    character(256)     :: dfn
    character(64)      :: ffn
    real(ESMF_KIND_R4), pointer :: rbuf2(:,:)
    real(ESMF_KIND_RX), pointer :: bptr2(:,:), rptr2(:,:), rptr3(:,:,:)
    integer(ESMF_KIND_I4), pointer :: iptr2(:,:), iptr3(:,:,:)
    type(ESMF_TypeKind_Flag) :: tk
    type(ESMF_DistGrid)  :: distGrid
    type(ESMF_ArraySpec) :: arraySpec
    type(ESMF_Array)     :: brray
    integer              :: i, j, k, udlb(1), udub(1)
    integer              :: lb(2), ub(2), elb(2,1), eub(2,1)

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array, rank=ndim, typeKind=tk, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (.not.(ndim.eq.2.or.ndim.eq.3)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FFileRead: array rank must be 2 or 3')
      return ! bail out
    endif

    call ESMF_VMGet(vm, petCount=nPets, localPet=lPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    call FFileSetTime(ffs, dtgTime, tauTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ffs%outtyp = outtyp
    ffn = FFileName(ffs, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    dfn = trim(dir)//'/'//ffn
    if (lPet.eq.rootPet) then
      write(*,'(a)') 'FFileRead: '//trim(dfn)
    endif

    if (ndim.eq.3) then
      if (tk.eq.ESMF_TYPEKIND_I4) then
        call ESMF_ArrayGet(array, localDE=lde, farrayPtr=iptr3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        call ESMF_ArrayGet(array, localDE=lde, farrayPtr=rptr3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      call ESMF_ArrayGet(array, distGrid=distGrid, &
        undistLBound=udlb, undistUBound=udub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_ArraySpecSet(arraySpec, rank=2, typeKind=ESMF_TYPEKIND_RX, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      brray = ESMF_ArrayCreate(distGrid, arraySpec, indexFlag=ESMF_INDEX_GLOBAL, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_ArrayGet(brray, localDE=lde, farrayPtr=bptr2, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_ArrayGet(brray, exclusiveLBound=elb, exclusiveUBound=eub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      lb(:) = elb(:,1)
      ub(:) = eub(:,1)
    endif

    if (lPet.eq.rootPet) then
      allocate (rbuf2(ffs%dims(1),ffs%dims(2)), &
                rptr2(ffs%dims(1),ffs%dims(2)), &
                iptr2(ffs%dims(1),ffs%dims(2)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of global arrays failed.", &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_UtilIOUnitGet(iunit, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      inquire (iolength=llen) rbuf2
      open (unit=iunit, file=trim(dfn), form='unformatted', access='direct', &
        recl=llen, action='read', status='old', iostat=stat, iomsg=msg)
      if (stat.ne.0) then
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
          CONTEXT, rcToReturn=rc)
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
          CONTEXT, rcToReturn=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    if (ndim.eq.3) then
      do k=udlb(1),udub(1)
        if (lPet.eq.rootPet) then
          read (iunit, rec=k-udlb(1)+1, iostat=stat, iomsg=msg) rbuf2
          if (stat.ne.0) then
            call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
              CONTEXT, rcToReturn=rc)
            call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
              CONTEXT, rcToReturn=rc)
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
          endif
          rptr2(:,:) = rbuf2(:,:)
        endif
        call ESMF_ArrayScatter(brray, rptr2, rootPet, tile=tile, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        if (tk.eq.ESMF_TYPEKIND_I4) then
          iptr3(lb(1):ub(1),lb(2):ub(2),k) = nint(bptr2(lb(1):ub(1),lb(2):ub(2)))
        else
          rptr3(lb(1):ub(1),lb(2):ub(2),k) = bptr2(lb(1):ub(1),lb(2):ub(2))
        endif
      enddo
      call ESMF_ArrayDestroy(brray, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    else
      if (lPet.eq.rootPet) then
        read (iunit, rec=1, iostat=stat, iomsg=msg) rbuf2
        if (stat.ne.0) then
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
            CONTEXT, rcToReturn=rc)
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
            CONTEXT, rcToReturn=rc)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
        if (tk.eq.ESMF_TYPEKIND_I4) then
          iptr2(:,:) = nint(rbuf2(:,:))
        else
          rptr2(:,:) = rbuf2(:,:)
        endif
      endif
      if (tk.eq.ESMF_TYPEKIND_I4) then
        call ESMF_ArrayScatter(array, iptr2, rootPet, tile=tile, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        call ESMF_ArrayScatter(array, rptr2, rootPet, tile=tile, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    endif

    if (lPet.eq.rootPet) then
      close (iunit, iostat=stat, iomsg=msg)
      if (stat.ne.0) then
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
          CONTEXT, rcToReturn=rc)
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
          CONTEXT, rcToReturn=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      deallocate (rbuf2, rptr2, iptr2, stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Deallocation of global arrays failed.", &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
    call ESMF_VMBarrier(vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FFileRead_Field(ffs, field, vm, dir, dtgTime, tauTime, outtyp, rc)
    type(FFile)       :: ffs
    type(ESMF_Field)  :: field
    type(ESMF_VM)     :: vm
    character(*)      :: dir
    type(ESMF_Time)   :: dtgTime
    type(ESMF_Time)   :: tauTime
    character(7)      :: outtyp
    integer, optional :: rc

    ! local variables
    type(ESMF_Array)   :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, array=array, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    call FFileRead(ffs, array, vm, dir, dtgTime, tauTime, outtyp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FFileWrite_Array(ffs, array, vm, dir, dtgTime, tauTime, outtyp, rc)
    type(FFile)       :: ffs
    type(ESMF_Array)  :: array
    type(ESMF_VM)     :: vm
    character(*)      :: dir
    type(ESMF_Time)   :: dtgTime
    type(ESMF_Time)   :: tauTime
    character(7)      :: outtyp
    integer, optional :: rc

    ! local variables
    integer, parameter :: rootPet = 0
    integer, parameter :: tile = 1
    integer, parameter :: lde = 0
    integer            :: stat, iunit, llen, nPets, lPet, ndim
    character(64)      :: msg=' '
    character(256)     :: dfn
    character(64)      :: ffn
    real(ESMF_KIND_R4), pointer :: rbuf2(:,:)
    real(ESMF_KIND_RX), pointer :: bptr2(:,:), rptr2(:,:), rptr3(:,:,:)
    integer(ESMF_KIND_I4), pointer :: iptr2(:,:), iptr3(:,:,:)
    type(ESMF_TypeKind_Flag) :: tk
    type(ESMF_DistGrid)  :: distGrid
    type(ESMF_ArraySpec) :: arraySpec
    type(ESMF_Array)     :: brray
    integer              :: i, j, k, udlb(1), udub(1)
    integer              :: lb(2), ub(2), elb(2,1), eub(2,1)

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_ArrayGet(array, rank=ndim, typeKind=tk, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (.not.(ndim.eq.2.or.ndim.eq.3)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='FFileWrite: array rank must be 2 or 3')
      return ! bail out
    endif

    call ESMF_VMGet(vm, petCount=nPets, localPet=lPet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    call FFileSetTime(ffs, dtgTime, tauTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ffs%outtyp = outtyp
    ffn = FFileName(ffs, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    dfn = trim(dir)//'/'//ffn
    if (lPet.eq.rootPet) then
      write(*,'(a)') 'FFileWrite: '//trim(dfn)
    endif

    if (ndim.eq.3) then
      if (tk.eq.ESMF_TYPEKIND_I4) then
        call ESMF_ArrayGet(array, localDE=lde, farrayPtr=iptr3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        call ESMF_ArrayGet(array, localDE=lde, farrayPtr=rptr3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      call ESMF_ArrayGet(array, distGrid=distGrid, &
        undistLBound=udlb, undistUBound=udub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_ArraySpecSet(arraySpec, rank=2, typeKind=ESMF_TYPEKIND_RX, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      brray = ESMF_ArrayCreate(distGrid, arraySpec, indexFlag=ESMF_INDEX_GLOBAL, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_ArrayGet(brray, localDE=lde, farrayPtr=bptr2, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_ArrayGet(brray, exclusiveLBound=elb, exclusiveUBound=eub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      lb(:) = elb(:,1)
      ub(:) = eub(:,1)
    endif

    if (lPet.eq.rootPet) then
      allocate (rbuf2(ffs%dims(1),ffs%dims(2)), &
                rptr2(ffs%dims(1),ffs%dims(2)), &
                iptr2(ffs%dims(1),ffs%dims(2)), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of global arrays failed.", &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_UtilIOUnitGet(iunit, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      inquire (iolength=llen) rbuf2
      open (unit=iunit, file=trim(dfn), form='unformatted', access='direct', &
        recl=llen, action='write', status='replace', iostat=stat, iomsg=msg)
      if (stat.ne.0) then
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
          CONTEXT, rcToReturn=rc)
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
          CONTEXT, rcToReturn=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    if (ndim.eq.3) then
      do k=udlb(1),udub(1)
        if (tk.eq.ESMF_TYPEKIND_I4) then
          bptr2(lb(1):ub(1),lb(2):ub(2)) = iptr3(lb(1):ub(1),lb(2):ub(2),k)
        else
          bptr2(lb(1):ub(1),lb(2):ub(2)) = rptr3(lb(1):ub(1),lb(2):ub(2),k)
        endif
        call ESMF_ArrayGather(brray, rptr2, rootPet, tile=tile, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
        if (lPet.eq.rootPet) then
          rbuf2(:,:) = rptr2(:,:)
          write (iunit, rec=k-udlb(1)+1, iostat=stat, iomsg=msg) rbuf2
          if (stat.ne.0) then
            call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
              CONTEXT, rcToReturn=rc)
            call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
              CONTEXT, rcToReturn=rc)
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
          endif
        endif
      enddo
      call ESMF_ArrayDestroy(brray, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    else
      if (tk.eq.ESMF_TYPEKIND_I4) then
        call ESMF_ArrayGather(array, iptr2, rootPet, tile=tile, vm=vm, rc=rc)
      else
        call ESMF_ArrayGather(array, rptr2, rootPet, tile=tile, vm=vm, rc=rc)
      endif
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (lPet.eq.rootPet) then
        if (tk.eq.ESMF_TYPEKIND_I4) then
          rbuf2(:,:) = iptr2(:,:)
        else
          rbuf2(:,:) = rptr2(:,:)
        endif
        write (iunit, rec=1, iostat=stat, iomsg=msg) rbuf2
        if (stat.ne.0) then
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
            CONTEXT, rcToReturn=rc)
          call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
            CONTEXT, rcToReturn=rc)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif
      endif
    endif

    if (lPet.eq.rootPet) then
      close (iunit, iostat=stat, iomsg=msg)
      if (stat.ne.0) then
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), &
          CONTEXT, rcToReturn=rc)
        call ESMF_LogSetError(ESMF_FAILURE, msg=trim(dfn), &
          CONTEXT, rcToReturn=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      deallocate (rbuf2, rptr2, iptr2, stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Deallocation of global arrays failed.", &
        CONTEXT, rcToReturn=rc)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
    call ESMF_VMBarrier(vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FFileWrite_Field(ffs, field, vm, dir, dtgTime, tauTime, outtyp, rc)
    type(FFile)       :: ffs
    type(ESMF_Field)  :: field
    type(ESMF_VM)     :: vm
    character(*)      :: dir
    type(ESMF_Time)   :: dtgTime
    type(ESMF_Time)   :: tauTime
    character(7)      :: outtyp
    integer, optional :: rc

    ! local variables
    type(ESMF_Array)   :: array

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, array=array, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    call FFileWrite(ffs, array, vm, dir, dtgTime, tauTime, outtyp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine FFileWriteGrid(grid, vm, dir, nest, cfluid, dtgTime, tauTime, opt, rc)
    type(ESMF_Grid)   :: grid
    type(ESMF_VM)     :: vm
    character(*)      :: dir
    integer           :: nest
    character(1)      :: cfluid
    type(ESMF_Time)   :: dtgTime
    type(ESMF_Time)   :: tauTime
    character(4), optional :: opt !'coor', 'mask', 'both'
    integer, optional :: rc

    ! local variables
    integer, parameter :: lde=0
    character(4)       :: sopt
    character(6)       :: fldnam
    character(24)      :: lvltyp='surface'
    real(4)            :: rlev(2)=(/0.0,0.0/)
    character(7)       :: outtyp='datafld'
    type(FFile)        :: ffs
    real(ESMF_KIND_RX), pointer :: rptr(:,:)
    integer(ESMF_KIND_I4), pointer :: iptr(:,:)
    type(ESMF_Field)   :: field

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(opt)) then
      sopt = opt
    else
      sopt = 'both'
    endif

    if (sopt.eq.'coor'.or.sopt.eq.'both') then

      fldnam = 'grdlon'
      call ESMF_GridGetCoord(grid, 1, localDE=lde, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=rptr, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      field = ESMF_FieldCreate(grid, rptr, dataCopyFlag=ESMF_DATACOPY_REFERENCE, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, name=fldnam, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      ffs = FFileCreate(field, nest, cfluid, fldnam, lvltyp, rlev, rc=rc)
      call FFileWrite(ffs, field, vm, trim(dir), dtgTime, tauTime, outtyp, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_FieldDestroy(field, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

      fldnam = 'grdlat'
      call ESMF_GridGetCoord(grid, 2, localDE=lde, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=rptr, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      field = ESMF_FieldCreate(grid, rptr, dataCopyFlag=ESMF_DATACOPY_REFERENCE, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, name=fldnam, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      ffs = FFileCreate(field, nest, cfluid, fldnam, lvltyp, rlev, rc=rc)
      call FFileWrite(ffs, field, vm, trim(dir), dtgTime, tauTime, outtyp, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_FieldDestroy(field, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    endif

    if (sopt.eq.'mask'.or.sopt.eq.'both') then

      fldnam = 'lndsea'
      call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK, localDE=lde, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=iptr, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      field = ESMF_FieldCreate(grid, iptr, dataCopyFlag=ESMF_DATACOPY_REFERENCE, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, name=fldnam, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      ffs = FFileCreate(field, nest, cfluid, fldnam, lvltyp, rlev, rc=rc)
      call FFileWrite(ffs, field, vm, trim(dir), dtgTime, tauTime, outtyp, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      call ESMF_FieldDestroy(field, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    endif

  end subroutine

end module
