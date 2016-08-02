#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "NUOPC_FileWriteUtility.F90"
#define MODNAME "NUOPC_FileWriteUtility"

module NUOPC_FileWriteUtility
  use ESMF
  use NUOPC

  implicit none

  private

  public :: NUOPC_FileWriteGrid
  public :: NUOPC_FileWriteMapNCL
  public :: NUOPC_MAP_GLOBAL
  public :: NUOPC_MAP_CONUS
  public :: NUOPC_MAP_IRENE
  public :: NUOPC_MAP_FRONTRANGE
  public :: NUOPC_MAP_PROTO100200

  integer,parameter :: NUOPC_MAP_GLOBAL = 1
  integer,parameter :: NUOPC_MAP_CONUS = 2
  integer,parameter :: NUOPC_MAP_IRENE = 3
  integer,parameter :: NUOPC_MAP_FRONTRANGE = 4
  integer,parameter :: NUOPC_MAP_PROTO100200 = 5

  type MapDesc
    character(len=16) :: mapName
    real              :: minLat
    real              :: maxLat
    real              :: minLon
    real              :: maxLon
  endtype MapDesc

  type(MapDesc),parameter,dimension(5) :: MapGrid = &
    (/ MapDesc( &
         "GLOBAL", &
         -90.0, &
         90.0, &
         -180.0, &
         -180.0), &
       MapDesc( &
         "CONUS", &
         18.0, &
         49.0, &
         -125.0, &
         -62.5), &
       MapDesc( &
         "IRENE", &
         13.0, &
         55.0, &
         -120.0, &
         -46.0), &
       MapDesc( &
         "FRONTRANGE", &
         38.5, &
         41.0, &
         -107.0, &
         -103.5), &
       MapDesc( &
         "PROTO100200", &
         0.0, &
         100.0, &
         0.0, &
         200.0) &
     /) 

contains

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

  subroutine NUOPC_FileWriteGrid(grid,fileName,mapList,rc)
    ! ARGUMENTS
    type(ESMF_Grid),intent(in)           :: grid
    character(len=*),intent(in),optional :: fileName
    integer,intent(in),optional          :: mapList(:)    
    integer,intent(out),optional         :: rc

    ! LOCAL VARIABLES
    character(len=64)               :: lfileName
    character(len=64)               :: gridName
    type(ESMF_Array)                :: array
    type(ESMF_ArrayBundle)          :: arrayBundle
    logical                         :: isPresent
    integer                         :: dimCount
    integer                         :: dimIndex
    integer,allocatable             :: coordDimCount(:)
    integer                         :: coordDimMax
    integer                         :: stat
    logical                         :: corners

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_GridGet(grid, name=gridName, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (present(fileName)) then
      lfileName = trim(fileName)
    else
      lfileName = trim(gridName)//".nc"
    endif

    arrayBundle = ESMF_ArrayBundleCreate(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

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
      corners = .TRUE.
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
    else
      corners = .FALSE.
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
     fileName=trim(lfileName),rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_ArrayBundleDestroy(arrayBundle,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (present(mapList)) then
      call ESMF_GridGet(grid,dimCount=dimCount,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      ! allocate coordDim info accord. to dimCount and tileCount
      allocate(coordDimCount(dimCount), &
        stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of coordinate dimensions memory failed.", &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out

      ! get coordDim info
      call ESMF_GridGet(grid, coordDimCount=coordDimCount, &
        rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      coordDimMax = 0
      do dimIndex=1,dimCount
        coordDimMax = MAX(coordDimMax,coordDimCount(dimIndex))
      enddo

      if (coordDimMax == 1) then
        call NUOPC_FileWriteMapNCL(trim(lfileName),mapList, &
          repeatCoord=.TRUE.,corners=corners,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      else
        call NUOPC_FileWriteMapNCL(trim(lfileName),mapList, &
          corners=corners,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
   endif

  end subroutine

  subroutine NUOPC_FileWriteMapNCL(gridFile,mapList,title,gridName, &
  repeatCoord,corners,rc)
    ! ARGUMENTS
    character(len=*),intent(in)          :: gridFile
    integer,intent(in)                   :: mapList(:)
    character(len=*),intent(in),optional :: title
    character(len=*),intent(in),optional :: gridName
    logical,intent(in),optional          :: repeatCoord
    logical,intent(in),optional          :: corners
    integer,intent(out),optional         :: rc

    ! LOCAL VARIABLES
    type(ESMF_VM)                   :: vm
    character(len=64)               :: ltitle
    character(len=64)               :: lgridName
    logical                         :: lrepeatCoord
    logical                         :: lcorners
    character(len=64)               :: nclFile
    character(len=64)               :: mapName
    integer                         :: mIndex
    integer                         :: markExt
    integer                         :: fUnit
    integer                         :: stat
    integer                         :: lpe
    character(len=10)               :: lat
    character(len=10)               :: lon

    if (present(rc)) rc = ESMF_SUCCESS

    ! Get current VM and pet number
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_VMGet(vm, localPet=lpe, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (lpe /= 0) return

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

    if (present(repeatCoord)) then
      lrepeatCoord = repeatCoord
    else
      lrepeatCoord = .FALSE.
    endif

    if (present(corners)) then
      lcorners = corners
    else
      lcorners = .FALSE.
    endif

    if (lcorners) then
      lat = 'lat_corner'
      lon = 'lon_corner'
    else
      lat = 'lat_center'
      lon = 'lon_center'
    endif

    do mIndex=1,size(mapList)
      selectcase(mapList(mIndex))
        case (NUOPC_MAP_GLOBAL,NUOPC_MAP_CONUS,NUOPC_MAP_IRENE, &
        NUOPC_MAP_FRONTRANGE,NUOPC_MAP_PROTO100200)
          mapName = trim(MapGrid(mapList(mIndex))%mapName)
        case default
          mapName = 'UKNOWN'
      endselect

      nclFile = trim(lgridName)// &
        "_"//trim(mapName)//".ncl"

      call ESMF_UtilIOUnitGet(fUnit, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      open (fUnit,file=trim(nclFile),action="write", &
        status="new",iostat=stat)
      if (stat /= 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN,   &
          msg="Cound not open "//trim(nclFile)//".", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      endif

      write (fUnit,"(A)") '; NUOPC_FileWriteMapNCL used to generate this file'
      write (fUnit,"(A)") '; execute ncl <this file> to generate grid png file'
      write (fUnit,"(A)") 'begin'
      write (fUnit,"(A)") '  in = addfile("'//trim(gridFile)//'","r")'
      write (fUnit,"(A)") '  wks = gsn_open_wks("png","'// &
        trim(lgridName)//'_'//trim(mapName)//'")'
      write (fUnit,"(A)") '  res              = True'
      write (fUnit,"(A)") '  res@gsnMaximize  = True'
      write (fUnit,"(A)") '  res@gsnDraw      = False'
      write (fUnit,"(A)") '  res@gsnFrame     = False'
      write (fUnit,"(A)") '  res@tiMainString = "'// &
        trim(ltitle)//' '//trim(mapName)//'"'
      write (fUnit,"(A)") '  res@pmTickMarkDisplayMode = "Always"'
      write (fUnit,"(A)") '; '//trim(mapName)//' Map Grid'
      if (mapList(mIndex) > 1 .AND. mapList(mIndex) <= size(MapGrid)) then
        write (fUnit,"(A,F0.3)") &
          '  res@mpMinLatF    = ',MapGrid(mapList(mIndex))%minLat
        write (fUnit,"(A,F0.3)") &
          '  res@mpMaxLatF    = ',MapGrid(mapList(mIndex))%maxLat
        write (fUnit,"(A,F0.3)") &
          '  res@mpMinLonF    = ',MapGrid(mapList(mIndex))%minLon
        write (fUnit,"(A,F0.3)") &
          '  res@mpMaxLonF    = ',MapGrid(mapList(mIndex))%maxLon
      endif
      write (fUnit,"(A)") '  map = gsn_csm_map_ce(wks,res)'
      write (fUnit,"(A)") '  hgt = in->lon_center(:,:)'
      write (fUnit,"(A)") '  dimlon = getfilevardimsizes(in,"'//trim(lon)//'")'
      write (fUnit,"(A)") '  dimlat = getfilevardimsizes(in,"'//trim(lat)//'")'
      if (lrepeatCoord) then
        write (fUnit,"(A)") '  hgt@lat2d = conform_dims((/dimlon(0),dimlat(1)/),'// &
          'in->lat_center(:,0),1)'
        write (fUnit,"(A)") '  hgt@lon2d = conform_dims((/dimlon(0),dimlat(1)/),'// &
          'in->lon_center(0,:),0)'
      else
        write (fUnit,"(A)") '  hgt@lat2d = in->'//trim(lat)//'(:,:)'
        write (fUnit,"(A)") '  hgt@lon2d = in->'//trim(lon)//'(:,:)'
      endif
      write (fUnit,"(A)") '  pres                   = True'
      if (lcorners) then
        write (fUnit,"(A)") '  pres@gsnCoordsAsLines  = True'
      else
        write (fUnit,"(A)") '  pres@gsnCoordsAsLines  = False'
        write (fUnit,"(A)") '  if (dimlon(0)*dimlat(1) .gt. 99) then'
        write (fUnit,"(A)") '    pres@gsMarkerIndex = 1'
        write (fUnit,"(A)") '  end if'
      endif
      write (fUnit,"(A)") '  gsn_coordinates(wks,map,hgt,pres)'
      write (fUnit,"(A)") 'end'

      close (fUnit,iostat=stat)
      if (stat /= 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_CLOSE,   &
          msg="Cound not close "//trim(nclFile)//".", &
          line=__LINE__, file=FILENAME, rcToReturn=rc)
        return
      endif

    enddo

  end subroutine    

end module
