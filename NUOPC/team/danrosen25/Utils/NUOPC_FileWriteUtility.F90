#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "NUOPC_FileWriteUtility.F90"
#define MODNAME "NUOPC_FileWriteUtility"

module NUOPC_FileWriteUtility
  use ESMF
  use NUOPC

  implicit none

  private

  public :: NUOPC_FileWriteGrid
  public :: NUOPC_FileWriteMapgrid
  public :: NUOPC_MAPGRID_GLOBAL
  public :: NUOPC_MAPGRID_CONUS
  public :: NUOPC_MAPGRID_IRENE
  public :: NUOPC_MAPGRID_FRONTRANGE
  public :: NUOPC_MAPGRID_PROTO100200

  integer,parameter :: NUOPC_MAPGRID_GLOBAL = 1
  integer,parameter :: NUOPC_MAPGRID_CONUS = 2
  integer,parameter :: NUOPC_MAPGRID_IRENE = 3
  integer,parameter :: NUOPC_MAPGRID_FRONTRANGE = 4
  integer,parameter :: NUOPC_MAPGRID_PROTO100200 = 5

  type MapGridDesc
    character(len=16) :: mapGridName
    real              :: minLat
    real              :: maxLat
    real              :: minLon
    real              :: maxLon
  endtype MapGridDesc

  type(MapGridDesc),parameter,dimension(5) :: MapGrid = &
    (/ MapGridDesc( &
         "GLOBAL", &
         -90.0, &
         90.0, &
         -180.0, &
         -180.0), &
       MapGridDesc( &
         "CONUS", &
         18.0, &
         49.0, &
         -125.0, &
         -62.5), &
       MapGridDesc( &
         "IRENE", &
         13.0, &
         55.0, &
         -120.0, &
         -46.0), &
       MapGridDesc( &
         "FRONTRANGE", &
         38.5, &
         41.0, &
         -107.0, &
         -103.5), &
       MapGridDesc( &
         "PROTO100200", &
         0.0, &
         100.0, &
         0.0, &
         200.5) &
     /) 

contains

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

  subroutine NUOPC_FileWriteGrid(grid,prefix,mapGridList,rc)
    ! ARGUMENTS
    type(ESMF_Grid),intent(in)           :: grid
    character(len=*),intent(in),optional :: prefix
    integer,intent(in),optional          :: mapGridList(:)    
    integer,intent(out),optional         :: rc

    ! LOCAL VARIABLES
    character(len=64)               :: lprefix
    character(len=64)               :: gridFile
    type(ESMF_Array)                :: array
    type(ESMF_ArrayBundle)          :: arrayBundle
    logical                         :: isPresent

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(prefix)) then
      lprefix = trim(prefix)
    else
      call ESMF_GridGet(grid,name=lprefix,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    arrayBundle = ESMF_ArrayBundleCreate(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    write (gridFile,"(A)") trim(lprefix)//".nc"

    ! -- centers --

    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      isPresent=isPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (isPresent) then
      call ESMF_GridGetCoord(grid, coordDim=1, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="lon_center", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayBundleAdd(arrayBundle,(/array/),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(grid, coordDim=2, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="lat_center", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayBundleAdd(arrayBundle,(/array/),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    ! -- corners --

    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
      isPresent=isPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (isPresent) then
      call ESMF_GridGetCoord(grid, coordDim=1, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_STDERRORCHECK(rc)) then
        call ESMF_ArraySet(array, name="lon_corner", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_ArrayBundleAdd(arrayBundle,(/array/),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
      call ESMF_GridGetCoord(grid, coordDim=2, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_STDERRORCHECK(rc)) then
        call ESMF_ArraySet(array, name="lat_corner", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_ArrayBundleAdd(arrayBundle,(/array/),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
    endif

    ! -- mask --

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="mask", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayBundleAdd(arrayBundle,(/array/),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    ! -- area --

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="area", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayBundleAdd(arrayBundle,(/array/),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
   endif

    call ESMF_ArrayBundleWrite(arrayBundle, &
     fileName=trim(gridFile),rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_ArrayBundleDestroy(arrayBundle,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (present(mapGridList)) then
      call NUOPC_FileWriteMapGrid(trim(gridFile),mapGridList,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

  end subroutine

  subroutine NUOPC_FileWriteMapGrid(gridFile,mapGridList,title,gridName,rc)
    ! ARGUMENTS
    character(len=*),intent(in)          :: gridFile
    integer,intent(in)                   :: mapGridList(:)
    character(len=*),intent(in),optional :: title
    character(len=*),intent(in),optional :: gridName
    integer,intent(out),optional         :: rc

    ! LOCAL VARIABLES
    character(len=64)               :: ltitle
    character(len=64)               :: lgridName
    character(len=64)               :: mapGridFile
    character(len=64)               :: mapGridName
    integer                         :: mIndex
    integer                         :: markExt
    integer                         :: fUnit
    integer                         :: stat

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(gridName)) then
      lgridName = trim(gridName)
    else
      markExt = index(gridFile,".",back=.TRUE.)
      if (markExt > 1) then
        lgridName = gridFile(1:markExt-1)
      else
        lgridName = trim(gridFile)
      endif
    endif

    if (present(title)) then
      ltitle = trim(title)
    else
      ltitle = trim(lgridName)
    endif


    do mIndex=1,size(mapGridList)
      selectcase(mapGridList(mIndex))
        case (NUOPC_MAPGRID_GLOBAL,NUOPC_MAPGRID_CONUS,NUOPC_MAPGRID_IRENE, &
        NUOPC_MAPGRID_FRONTRANGE,NUOPC_MAPGRID_PROTO100200)
          mapGridName = trim(MapGrid(mapGridList(mIndex))%mapGridName)
        case default
          mapGridName = 'UKNOWN'
      endselect

      mapGridFile = trim(lgridName)// &
        "_"//trim(mapGridName)//".ncl"

      call ESMF_UtilIOUnitGet(fUnit, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      open (fUnit,file=trim(mapGridFile),action="write", &
        status="new",iostat=stat)
      if (stat /= 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
          msg="Cound not open "//trim(mapGridFile)//".", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      endif

      write (fUnit,"(A)") 'begin'
      write (fUnit,"(A)") '  in = addfile("'//trim(gridFile)//'","r")'
      write (fUnit,"(A)") '  wks = gsn_open_wks("png","'// &
        trim(lgridName)//'_'//trim(mapGridName)//'")'
      write (fUnit,"(A)") '  res              = True'
      write (fUnit,"(A)") '  res@gsnMaximize  = True'
      write (fUnit,"(A)") '  res@gsnDraw      = False'
      write (fUnit,"(A)") '  res@gsnFrame     = False'
      write (fUnit,"(A)") '  res@tiMainString = "'// &
        trim(ltitle)//' '//trim(mapGridName)//'"'
      write (fUnit,"(A)") '  res@pmTickMarkDisplayMode = "Always"'
      write (fUnit,"(A)") '! '//trim(mapGridName)//' Map Grid'
      if (mapGridList(mIndex) > 1 .AND. mapGridList(mIndex) <= size(MapGrid)) then
        write (fUnit,"(A,F0.3)") &
          '  res@mpMinLatF    = ',MapGrid(mapGridList(mIndex))%minLat
        write (fUnit,"(A,F0.3)") &
          '  res@mpMaxLatF    = ',MapGrid(mapGridList(mIndex))%maxLat
        write (fUnit,"(A,F0.3)") &
          '  res@mpMinLonF    = ',MapGrid(mapGridList(mIndex))%minLon
        write (fUnit,"(A,F0.3)") &
          '  res@mpMaxLonF    = ',MapGrid(mapGridList(mIndex))%minLon
      endif
      write (fUnit,"(A)") '  map = gsn_csm_map_ce(wks,res)'
      write (fUnit,"(A)") '  hgt = in->lon_center(:,:)'
      write (fUnit,"(A)") '  hgt@lat2d = in->lat_center(:,:)'
      write (fUnit,"(A)") '  hgt@lon2d = in->lon_center(:,:)'
      write (fUnit,"(A)") '  pres                   = True'
      write (fUnit,"(A)") '  pres@gsnCoordsAsLines  = True'
      write (fUnit,"(A)") '  gsn_coordinates(wks,map,hgt,pres)'
      write (fUnit,"(A)") 'end'

      close (fUnit,iostat=stat)
      if (stat /= 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
          msg="Cound not close "//trim(mapGridFile)//".", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      endif

    enddo

  end subroutine    

end module
