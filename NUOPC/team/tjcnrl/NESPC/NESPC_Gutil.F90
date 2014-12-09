!-------------------------------------------------------------------------------
! NAVY ESPC Grid Utilities Module
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "NESPC_Gutil.F90"
#include "NESPC_Macros.h"


module NESPC_Gutil

  use ESMF
  use NESPC_Gdem

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

  ! grid structure type (public)
  type, public :: CGrid
    character(1) :: cfluid
    integer      :: nest
    integer      :: nproj
    integer      :: iref, jref
    integer      :: m, n, l
    real(4)      :: alnnt, phnt1, phnt2
    real(4)      :: rlon, rlat
    real(4)      :: delx, dely
    real(4)      :: rnest
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
  character(ESMF_MAXSTR)         :: msgString

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
    real(4), parameter             :: zero=0., r180=180., r360=360.
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

    ! GLOBE DEM requires longitudes in [-180,180]
    do j=lb(2),ub(2)
      do i=lb(1),ub(1)
        lons(i,j) = lon(i,j)
        if (lons(i,j).gt.r180) lons(i,j)=lons(i,j)-r360
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
          mask(i,j) = NESPC_MASK_LAND
        else
          mask(i,j) = NESPC_MASK_WATER
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
    real(ESMF_KIND_RX)    :: eps = 0.1
    integer               :: localrc, stat
    integer               :: i, j, nb
    real(ESMF_KIND_RX)    :: bimin, bimax, bjmin, bjmax
    real(ESMF_KIND_RX), allocatable  :: blon(:),blat(:),bi(:),bj(:)

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
    call CGridIJ2LL(dstCgs,nb,bi,bj,blon,blat,rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
!   write(*,'(a,4e14.6)') 'bndy dst lon/lat min/max: ', &
!     minval(blon),maxval(blon),minval(blat),maxval(blat)

    ! compute destination grid boundary points in source grid index space
    call CGridLL2IJ(srcCgs,nb,blon,blat,bi,bj,rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
!   write(*,'(a,4e14.6)') 'bndy dst i/j min/max: ', &
!     minval(bi),maxval(bi),minval(bj),maxval(bj)

    ! check if destination grid is covered by source grid
    bimin = minval(bi)
    bimax = maxval(bi)
    bjmin = minval(bj)
    bjmax = maxval(bj)
    cover = bimin.ge.1-eps.and.bimin.le.srcCgs%m+eps.and. &
            bimax.ge.1-eps.and.bimax.le.srcCgs%m+eps.and. &
            bjmin.ge.1-eps.and.bjmin.le.srcCgs%n+eps.and. &
            bjmax.ge.1-eps.and.bjmax.le.srcCgs%n+eps

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
    srcCgs = CGridCreate(srcGridnl, srcFluid, srcNest, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! create dst grid data structure
    dstCgs = CGridCreate(dstGridnl, dstFluid, dstNest, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! check if destination grid is covered by source grid
    cover = GridIsCovered(srcCgs, dstCgs, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end function

  !-----------------------------------------------------------------------------

  function CGridCreate_Gridnl(gnlFile, cfluid, nest, rc) result (cgs)
    character(*) :: gnlFile
    character(1) :: cfluid
    integer      :: nest
    integer, optional :: rc
    type(CGrid)  :: cgs

    ! local variables
    real(4), parameter :: zero = 0.0, d360 = 360.0
    integer :: iunit, stat
    logical :: lopened
    character(64) :: msg=' '
    integer :: nn, np, nrx0, nry0, nn0
    real(4) :: delxy
    real(8) :: onedeg,pi,radius
!   grid namelist definition and default values (from gridnl.F)
    integer, parameter :: MAX_GRIDS=7
    real(4),dimension(MAX_GRIDS) :: delx=81000., dely=81000.
    integer,dimension(MAX_GRIDS) :: ii=1, iref=1, jj=1, jref=1
    logical,dimension(MAX_GRIDS) :: lnmove=.false.
    integer,dimension(MAX_GRIDS) :: m=61,n=61,npgrid=(/1,1,2,3,4,5,6/)
    integer :: kkl, kka=30, kko=1, kkom=1, kkosm=0, nnest=1, nproj=2
    real(4) :: alnnt=240., phnt1=60., phnt2=30., rlat=42.5, rlon=16.5
    real(4) :: rnest=3.
    namelist /gridnl/ m,n,kka,kko,kkom,kkosm,nproj,rlat,rlon,alnnt, &
      ii,iref,jj,jref,lnmove,nnest,npgrid,phnt1,phnt2,delx,dely,rnest

    if (present(rc)) rc = ESMF_SUCCESS

!...local constants
    pi=4.d0*atan(1.d0)
    radius=6371229.d0
    onedeg=radius*2.d0*pi/360.d0

!...find available file unit
    call ESMF_UtilIOUnitGet(iunit, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

!...read gridnl
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

!...shift rlon and allnt
    if (rlon.lt.zero) rlon = d360+rlon
    if (alnnt.lt.zero) alnnt = d360+alnnt

!...fill in values for all nests
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

!...convert grid spacing from degree to km for spherical projection
    if (nproj.eq.5) then
      delx = delx*onedeg
      dely = dely*onedeg
    endif

!...identify highest resolution nest
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

!...set nest independent grid parameters
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

!...set nest dependent grid parameters
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

  function CGridCreate_Datahd(dir, cfluid, dtgTime, nest, rc) result (cgs)
    character(*)      :: dir
    character(1)      :: cfluid
    type(ESMF_Time)   :: dtgTime
    integer           :: nest
    integer, optional :: rc
    type(CGrid)       :: cgs

    ! local variables
    real(4), parameter :: zero = 0.0, d360 = 360.0
    integer            :: iunit, stat
    logical            :: lopened
    character(64)      :: msg=' '
    type(FFile)        :: ffs
    character(256)     :: dfn
    character(64)      :: ffn
    real(ESMF_KIND_R4) :: datahd(2000)
    integer :: llen, nn, np, nrx0, nry0, nn0
    real(4) :: delxy
    real(8) :: onedeg,pi,radius
!   grid namelist definition and default values (from gridnl.F)
    integer, parameter :: MAX_GRIDS=7
    real(4),dimension(MAX_GRIDS) :: delx=81000., dely=81000.
    integer,dimension(MAX_GRIDS) :: ii=1, iref=1, jj=1, jref=1
    logical,dimension(MAX_GRIDS) :: lnmove=.false.
    integer,dimension(MAX_GRIDS) :: m=61,n=61,npgrid=(/1,1,2,3,4,5,6/)
    integer :: kkl, kka=30, kko=1, kkom=1, kkosm=0, nnest=1, nproj=2
    real(4) :: alnnt=240., phnt1=60., phnt2=30., rlat=42.5, rlon=16.5
    real(4) :: rnest=3.

    if (present(rc)) rc = ESMF_SUCCESS

!...local constants
    pi=4.d0*atan(1.d0)
    radius=6371229.d0
    onedeg=radius*2.d0*pi/360.d0

!...set file name
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

!...get available io unit
    call ESMF_UtilIOUnitGet(iunit, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

!...read datahd
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

!...extract data from datahd
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

!...convert grid spacing from degree to km for spherical projection
    if (nproj.eq.5) then
      delx = delx*onedeg
      dely = dely*onedeg
    endif

!...shift rlon and allnt
    if (rlon.lt.zero) rlon = d360+rlon
    if (alnnt.lt.zero) alnnt = d360+alnnt

!...identify highest resolution nest
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

!...set nest independent grid parameters
    cgs%cfluid = cfluid
    cgs%rnest  = rnest
    cgs%nest   = nest
    cgs%nproj  = nproj
    cgs%alnnt  = alnnt
    cgs%phnt1  = phnt1
    cgs%phnt2  = phnt2
    cgs%rlon   = rlon
    cgs%rlat   = rlat
    cgs%l      = kkl

!...set nest dependent grid parameters
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

  subroutine CGridPrint(cgs)
    type(CGrid)  :: cgs

    ! local variables
    ! none

    write(*,'(a,1a1)')    'CGridPrint:      cfluid: ',cgs%cfluid
    write(*,'(a,1i1)')    'CGridPrint:        nest: ',cgs%nest
    write(*,'(a,1i1)')    'CGridPrint:       nproj: ',cgs%nproj
    write(*,'(a,2i5)')    'CGridPrint:   iref,jref: ',cgs%iref,cgs%jref
    write(*,'(a,3i6)')    'CGridPrint:     m, n, l: ',cgs%m,cgs%n,cgs%l
    write(*,'(a,1e14.6)') 'CGridPrint:       alnnt: ',cgs%alnnt
    write(*,'(a,2e14.6)') 'CGridPrint: phnt1,phnt2: ',cgs%phnt1,cgs%phnt2
    write(*,'(a,2e14.6)') 'CGridPrint:   rlon,rlat: ',cgs%rlon,cgs%rlat
    write(*,'(a,2e14.6)') 'CGridPrint:   delx,dely: ',cgs%delx,cgs%dely
    write(*,'(a,1e14.6)') 'CGridPrint:       rnest: ',cgs%rnest

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridCoord(cgs,imin,imax,jmin,jmax,grdlon,grdlat,grdrot,rc)
    !***********************************************************************
    ! Adapted from coampslib/grid.F & grdij.F
    !***********************************************************************
    type(CGrid)        :: cgs
    integer            :: imin,imax,jmin,jmax
    real(ESMF_KIND_RX) :: grdlon (imin:imax,jmin:jmax)
    real(ESMF_KIND_RX) :: grdlat (imin:imax,jmin:jmax)
    real(ESMF_KIND_RX), optional :: grdrot (imin:imax,jmin:jmax)
    integer, optional  :: rc

    ! local variables
    integer :: i,j,ihem
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,d2r,deg,gcon
    real(8) :: ogcon,omega4,onedeg,pi,pi2,pi4,r2d,radius,rih,rr,x,xih,xx,y,yih,yy
    real(8) :: grdi   (imin:imax,jmin:jmax)
    real(8) :: grdj   (imin:imax,jmin:jmax)
    real(8) :: distx  (imin:imax,jmin:jmax)
    real(8) :: disty  (imin:imax,jmin:jmax)
    real(8) :: xpos   (imin:imax,jmin:jmax)
    real(8) :: ypos   (imin:imax,jmin:jmax)

    ! local grid reference
    integer :: igrid
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    igrid = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref = cgs%iref
    jref = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx = cgs%delx
    dely = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure igrid is an acceptable value
    if (igrid.lt.1.or.igrid.gt.5) then
      print 800
      print 805,igrid
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='CGridCoord: unsupported igrid')
      return ! bail out
    endif

    ! local constants
    pi=4.d0*atan(1.d0)
    pi2=pi/2.d0
    pi4=pi/4.d0
    d2r=pi/180.d0
    r2d=180.d0/pi
    radius=6371229.d0
    omega4=4.d0*pi/86400.d0
    onedeg=radius*2.d0*pi/360.d0

    ! Set grdij
    do j = jmin, jmax
      do i = imin, imax
        grdi(i,j)=real(i,8)
        grdj(i,j)=real(j,8)
      enddo
    enddo

    ! Set grdrot to 0.0
    if (present(grdrot)) then
    do j = jmin, jmax
      do i = imin, imax
        grdrot(i,j)=0.0
      enddo
    enddo
    endif

    ! compute distances on grid
    do j = jmin, jmax   
      do i = imin, imax
        distx(i,j)=(grdi(i,j)-real(iref,8))*delx
        disty(i,j)=(grdj(i,j)-real(jref,8))*dely
       enddo
    enddo

    !
    !***********************************************************************
    !          mercator projection (igrid=1)
    !***********************************************************************
    !
    if (igrid.eq.1) then

      gcon=0.d0
      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=radius*con1
      deg=reflat*0.5d0*d2r
      rih=con2*log(tan(pi4+deg))
      do j = jmin, jmax
        do i = imin, imax
          xpos(i,j)=1.d0
          ypos(i,j)=1.d0
          rr=rih+(grdj(i,j)-jref)*dely
          grdlat(i,j)=(2.d0*atan(exp(rr/con2))-pi2)*r2d
          grdlon(i,j)=reflon+(grdi(i,j)-iref)*r2d*delx/con2
          if (grdlon(i,j).gt.360.d0) grdlon(i,j)=grdlon(i,j)-360.d0
          if (grdlon(i,j).lt.  0.d0) grdlon(i,j)=grdlon(i,j)+360.d0
        enddo
      enddo

      return
    !
    !***********************************************************************
    !          lambert conformal (igrid=2) or
    !          polar stereographic (igrid=3)
    !***********************************************************************
    !
    elseif (igrid.eq.2.or.igrid.eq.3) then

      if (igrid.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((90.d0-abs(stdlt1))*d2r)) &
               -log(sin((90.d0-abs(stdlt2))*d2r))) &
              /(log(tan((90.d0-abs(stdlt1))*0.5d0*d2r)) &
               -log(tan((90.d0-abs(stdlt2))*0.5d0*d2r)))
        endif
      else
        gcon=1.d0
      endif
      ogcon=1.d0/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(90.d0-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=radius*cn1*ogcon
      deg=deg*0.5d0
      cn3=tan(deg)
      deg=(90.d0-abs(reflat))*0.5d0*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do j = jmin, jmax
        do i = imin, imax
          x=xih+distx(i,j)
          y=yih+disty(i,j)
          xpos(i,j)=x*ihem
          ypos(i,j)=y*ihem
          rr=sqrt(x*x+y*y)
          grdlat(i,j)=r2d*(pi2-2.d0*atan(cn3*(rr/cn2)**ogcon))*ihem
          xx= x
          yy=-y*ihem
          if (yy.eq.0.d0) then
            if (xx.le.0.d0) then
              angle=-90.d0
            elseif (xx.gt.0.d0) then
              angle=90.d0
            endif
          else
            angle=atan2(xx,yy)*r2d
          endif
          grdlon(i,j)=stdlon+angle*ogcon
          deg=grdlat(i,j)*d2r
          if (grdlon(i,j).gt.360.d0) grdlon(i,j)=grdlon(i,j)-360.d0
          if (grdlon(i,j).lt.  0.d0) grdlon(i,j)=grdlon(i,j)+360.d0
        enddo
      enddo

      ! Calculate angle between grid north and true north at every grid point.
      if (present(grdrot)) then
      do j = jmin, jmax
        do i = imin, imax
          grdrot(i,j)=(stdlon-grdlon(i,j))*gcon
          ! account for crossing greenwich meridian
          !  case 1: standard longitude is east of Meridian, (between 0 and 90E)
          !          and the grid longitude is west of Meridian (between 90W and Meridian)
          if (stdlon .ge. 0.d0 .and. stdlon .le. 90.d0 .and. &
            grdlon(i,j) .ge. 270.d0 .and. grdlon(i,j) .lt. 360.d0) &
          then
            grdrot(i,j)=(stdlon+360.d0-grdlon(i,j))*gcon
          endif
          !  case 2: standard longitude is west of Meridian, (between 90W and Meridian)
          !          and the grid longitude is east of Meridian (between 0 and 90E)
          if (stdlon .ge. 270.d0 .and. stdlon .lt. 360.d0 .and. &
            grdlon(i,j) .ge. 0.d0 .and. grdlon(i,j) .le. 90.d0) &
          then
            grdrot(i,j)=(stdlon-grdlon(i,j)-360.d0)*gcon
          endif            
        enddo
      enddo
      endif

      return
    !
    !***********************************************************************
    !          analytic grid (igrid=4)
    !***********************************************************************
    !
    elseif (igrid.eq.4) then

      gcon=2.d0
      cnx=delx/onedeg
      cny=dely/onedeg
      do j = jmin, jmax
        do i = imin, imax
          xpos(i,j)=1.d0
          ypos(i,j)=1.d0
          grdlat(i,j)=(grdj(i,j)-jref)*cny+reflat
          grdlon(i,j)=(grdi(i,j)-iref)*cnx+reflon
          if (grdlon(i,j).gt.360.d0) grdlon(i,j)=grdlon(i,j)-360.d0
          if (grdlon(i,j).lt.  0.d0) grdlon(i,j)=grdlon(i,j)+360.d0
        enddo
      enddo

      return
    !
    !***********************************************************************
    !          spherical grid (igrid=5)
    !***********************************************************************
    !
    elseif (igrid.eq.5) then

      gcon=3.d0
      cnx=delx/onedeg
      cny=dely/onedeg
      do j = jmin, jmax
        do i = imin, imax
          xpos(i,j)=1.d0
          ypos(i,j)=1.d0
          grdlat(i,j)=(grdj(i,j)-jref)*cny+reflat
          grdlon(i,j)=(grdi(i,j)-iref)*cnx+reflon
          if (grdlon(i,j).gt.360.d0) grdlon(i,j)=grdlon(i,j)-360.d0
          if (grdlon(i,j).lt.  0.d0) grdlon(i,j)=grdlon(i,j)+360.d0
        enddo
      enddo

      return

    endif
!
!********************************************************************
!          format statements
!********************************************************************
!
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridCoord:',/ &
            ,'   igrid must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridIJ2LL(cgs,npts,grdi,grdj,grdlon,grdlat,rc)
    !***********************************************************************
    ! Adapted from coampslib/ij2ll.F
    !***********************************************************************
    type(CGrid)        :: cgs
    integer            :: npts
    real(ESMF_KIND_RX) :: grdi   (npts)
    real(ESMF_KIND_RX) :: grdj   (npts)
    real(ESMF_KIND_RX) :: grdlon (npts)
    real(ESMF_KIND_RX) :: grdlat (npts)
    integer, optional  :: rc

    ! local variables
    integer :: i,j,ihem
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,d2r,deg,gcon
    real(8) :: ogcon,omega4,onedeg,pi,pi2,pi4,r2d,radius,rih,rr,x,xih,xx,y,yih,yy

    ! local grid reference
    integer :: igrid
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    igrid = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref = cgs%iref
    jref = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx = cgs%delx
    dely = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure igrid is an acceptable value
    if (igrid.lt.1.or.igrid.gt.5) then
      print 800
      print 805,igrid
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='CGridIJ2LL: unsupported igrid')
      return ! bail out
    endif

    ! local constants
    pi=4.d0*atan(1.d0)
    pi2=pi/2.d0
    pi4=pi/4.d0
    d2r=pi/180.d0
    r2d=180.d0/pi
    radius=6371229.d0
    omega4=4.d0*pi/86400.d0
    onedeg=radius*2.d0*pi/360.d0

    !
    !***********************************************************************
    !          mercator projection (igrid=1)
    !***********************************************************************
    !
    if (igrid.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=radius*con1
      deg=reflat*0.5d0*d2r
      rih=con2*log(tan(pi4+deg))
      do i = 1, npts
        rr=rih+(grdj(i)-jref)*dely
        grdlat(i)=(2.d0*atan(exp(rr/con2))-pi2)*r2d
        grdlon(i)=reflon+(grdi(i)-iref)*r2d*delx/con2
        if (grdlon(i).gt.360.d0) grdlon(i)=grdlon(i)-360.d0
        if (grdlon(i).lt.  0.d0) grdlon(i)=grdlon(i)+360.d0
      enddo

      return
    !
    !***********************************************************************
    !          lambert conformal (igrid=2) or
    !          polar stereographic (igrid=3)
    !***********************************************************************
    !
    elseif (igrid.eq.2.or.igrid.eq.3) then

      if (igrid.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((90.d0-abs(stdlt1))*d2r)) &
               -log(sin((90.d0-abs(stdlt2))*d2r))) &
              /(log(tan((90.d0-abs(stdlt1))*0.5d0*d2r)) &
               -log(tan((90.d0-abs(stdlt2))*0.5d0*d2r)))
        endif
      else
        gcon=1.d0
      endif
      ogcon=1.d0/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(90.d0-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=radius*cn1*ogcon
      deg=deg*0.5d0
      cn3=tan(deg)
      deg=(90.d0-abs(reflat))*0.5d0*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do i = 1, npts
        x=xih+(grdi(i)-iref)*delx
        y=yih+(grdj(i)-jref)*dely
        rr=sqrt(x*x+y*y)
        grdlat(i)=r2d*(pi2-2.d0*atan(cn3*(rr/cn2)**ogcon))*ihem
        xx= x
        yy=-y*ihem
        if (yy.eq.0.d0) then
          if (xx.le.0.d0) then
            angle=-90.d0
          elseif (xx.gt.0.d0) then
            angle=90.d0
          endif
        else
          angle=atan2(xx,yy)*r2d
        endif
        grdlon(i)=stdlon+angle*ogcon
        deg=grdlat(i)*d2r
        if (grdlon(i).gt.360.d0) grdlon(i)=grdlon(i)-360.d0
        if (grdlon(i).lt.  0.d0) grdlon(i)=grdlon(i)+360.d0
      enddo

      return
    !
    !***********************************************************************
    !          analytic grid (igrid=4)
    !***********************************************************************
    !
    elseif (igrid.eq.4) then

      cnx=delx/onedeg
      cny=dely/onedeg
      do i = 1, npts
        grdlat(i)=(grdj(i)-jref)*cny+reflat
        grdlon(i)=(grdi(i)-iref)*cnx+reflon
        if (grdlon(i).gt.360.d0) grdlon(i)=grdlon(i)-360.d0
        if (grdlon(i).lt.  0.d0) grdlon(i)=grdlon(i)+360.d0
      enddo

      return
    !
    !***********************************************************************
    !          spherical grid (igrid=5)
    !***********************************************************************
    !
    elseif (igrid.eq.5) then

      cnx=delx/onedeg
      cny=dely/onedeg
      do i = 1, npts
        grdlat(i)=(grdj(i)-jref)*cny+reflat
        grdlon(i)=(grdi(i)-iref)*cnx+reflon
        if (grdlon(i).gt.360.d0) grdlon(i)=grdlon(i)-360.d0
        if (grdlon(i).lt.  0.d0) grdlon(i)=grdlon(i)+360.d0
      enddo

      return

    endif
!
!********************************************************************
!          format statements
!********************************************************************
!
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridIJ2LL:',/ &
            ,'   igrid must be one of the following values:',// &
            ,'            1: mercator projection',/ &
            ,'            2: lambert conformal projection',/ &
            ,'            3: polar steographic projection',/ &
            ,'            4: cartesian coordinates',/ &
            ,'            5: spherical projection',// &
            ,' Your entry was:',i6,', Correct and try again',/)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CGridLL2IJ(cgs,npts,grdlon,grdlat,grdi,grdj,rc)
    !***********************************************************************
    ! Adapted from coampslib/ll2ij.F
    !***********************************************************************
    type(CGrid)        :: cgs
    integer            :: npts
    real(ESMF_KIND_RX) :: grdlon (npts)
    real(ESMF_KIND_RX) :: grdlat (npts)
    real(ESMF_KIND_RX) :: grdi   (npts)
    real(ESMF_KIND_RX) :: grdj   (npts)
    integer, optional  :: rc

    ! local variables
    integer :: i,j,ihem
    real(8) :: angle,cn1,cn2,cn3,cn4,cnx,cny,con1,con2,d2r,deg,gcon
    real(8) :: ogcon,omega4,onedeg,pi,pi2,pi4,r2d,radius,rih,rrih,x,xih,y,yih
    real(8) :: check,alnfix,alon

    ! local grid reference
    integer :: igrid
    real(8) :: reflat,reflon
    integer :: iref,jref
    real(8) :: stdlt1,stdlt2,stdlon
    real(8) :: delx,dely
    igrid = cgs%nproj
    reflat = cgs%rlat
    reflon = cgs%rlon
    iref = cgs%iref
    jref = cgs%jref
    stdlt1 = cgs%phnt1
    stdlt2 = cgs%phnt2
    stdlon = cgs%alnnt
    delx = cgs%delx
    dely = cgs%dely

    if (present(rc)) rc = ESMF_SUCCESS

    ! make sure igrid is an acceptable value
    if (igrid.lt.1.or.igrid.gt.5) then
      print 800
      print 805,igrid
      print 800
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
      msg='CGridLL2IJ: unsupported igrid')
      return ! bail out
    endif

    ! local constants
    pi=4.d0*atan(1.d0)
    pi2=pi/2.d0
    pi4=pi/4.d0
    d2r=pi/180.d0
    r2d=180.d0/pi
    radius=6371229.d0
    omega4=4.d0*pi/86400.d0
    onedeg=radius*2.d0*pi/360.d0

    !
    !***********************************************************************
    !          mercator projection (igrid=1)
    !***********************************************************************
    !
    if (igrid.eq.1) then

      deg=abs(stdlt1)*d2r
      con1=cos(deg)
      con2=radius*con1
      deg=reflat*0.5d0*d2r
      rih=con2*log(tan(pi4+deg))
      do i = 1, npts
        alon=grdlon(i)+180.d0-reflon
        if (alon.lt.  0.d0) alon=alon+360.d0
        if (alon.gt.360.d0) alon=alon-360.d0
        grdi(i)=iref+(alon-180.d0)*con2/(r2d*delx)
        deg=grdlat(i)*d2r+pi2
        deg=deg*0.5d0
        grdj(i)=jref+(con2*log(tan(deg))-rih)/dely
      enddo

      return
    !
    !***********************************************************************
    !          lambert conformal (igrid=2) or
    !          polar stereographic (igrid=3)
    !***********************************************************************
    !
    elseif (igrid.eq.2.or.igrid.eq.3) then

      if (igrid.eq.2) then
        if (stdlt1.eq.stdlt2) then
          gcon=sin(abs(stdlt1)*d2r)
        else
          gcon=(log(sin((90.d0-abs(stdlt1))*d2r)) &
               -log(sin((90.d0-abs(stdlt2))*d2r))) &
              /(log(tan((90.d0-abs(stdlt1))*0.5d0*d2r)) &
               -log(tan((90.d0-abs(stdlt2))*0.5d0*d2r)))
        endif
      else
        gcon=1.d0
      endif
      ogcon=1.d0/gcon
      ihem=nint(abs(stdlt1)/stdlt1)
      deg=(90.d0-abs(stdlt1))*d2r
      cn1=sin(deg)
      cn2=radius*cn1*ogcon
      deg=deg*0.5d0
      cn3=tan(deg)
      deg=(90.d0-abs(reflat))*0.5d0*d2r
      cn4=tan(deg)
      rih=cn2*(cn4/cn3)**gcon
      deg=(reflon-stdlon)*d2r*gcon
      xih= rih*sin(deg)
      yih=-rih*cos(deg)*ihem

      do i = 1, npts
        deg=(90.d0-grdlat(i)*ihem)*0.5d0*d2r
        cn4=tan(deg)
        rrih=cn2*(cn4/cn3)**gcon
        check=180.d0-stdlon
        alnfix=stdlon+check
        alon=grdlon(i)+check
        if (alon.lt.  0.d0) alon=alon+360.d0
        if (alon.gt.360.d0) alon=alon-360.d0
        deg=(alon-alnfix)*gcon*d2r
        x= rrih*sin(deg)
        y=-rrih*cos(deg)*ihem
        grdi(i)=iref+(x-xih)/delx
        grdj(i)=jref+(y-yih)/dely
      enddo

      return
    !
    !***********************************************************************
    !          analytic grid (igrid=4)
    !***********************************************************************
    !
    elseif (igrid.eq.4) then

      cnx=delx/onedeg
      cny=dely/onedeg
      do i = 1, npts
        grdi(i)=iref+(grdlon(i)-reflon)/cnx
        grdj(i)=jref+(grdlat(i)-reflat)/cny
      enddo

      return
    !
    !***********************************************************************
    !          spherical grid (igrid=5)
    !***********************************************************************
    !
    elseif (igrid.eq.5) then

      cnx=delx/onedeg
      cny=dely/onedeg
      do i = 1, npts
        grdi(i)=iref+(grdlon(i)-reflon)/cnx
        grdj(i)=jref+(grdlat(i)-reflat)/cny
      enddo

      return

    endif
!
!********************************************************************
!          format statements
!********************************************************************
!
800 format(/,' ',72('-'),/)
805 format(/,' ERROR from subroutine CGridLL2IJ:',/ &
            ,'   igrid must be one of the following values:',// &
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
