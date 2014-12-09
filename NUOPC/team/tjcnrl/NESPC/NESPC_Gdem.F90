!-------------------------------------------------------------------------------
! NAVY ESPC Gdem Module
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "NESPC_Gdem.F90"
#include "NESPC_Macros.h"


MODULE NESPC_Gdem

  USE ESMF

  IMPLICIT NONE
  PRIVATE

  !
  ! status and error codes
  !
  INTEGER, PARAMETER   :: SUCCESS          =  0
  INTEGER, PARAMETER   :: FAIL             = -1
  INTEGER, PARAMETER   :: DATAFILE_MISSING =  1
  INTEGER, PARAMETER   :: INVALID_LAT_LON  =  2
  INTEGER, PARAMETER   :: OUT_OF_MEMORY    =  3

  !
  ! Binary Interpolation flag: interpolate the 4 enclosing 
  ! neigbhoring cell to the specified lat/lon location.
  !
  LOGICAL, PARAMETER   :: USE_BINLINEAR_INTERP =  .true. 

  !
  ! File handle to used for reading metadata
  !
  INTEGER :: FH_META

  !
  ! The base unit to use for file I/O
  INTEGER, PARAMETER   ::  FILE_UNIT_BASE = 30
  !
  ! Default metafile name.
  !
  CHARACTER (len=*), PARAMETER    :: METADATA_FILE='metadata.txt'

  !
  ! Structure holding information about individual tile.
  !
  TYPE tile_info_t
      
      INTEGER                   nb_col_per_row
      INTEGER                   nb_row_per_tile
      REAL                      lat
      REAL                      lon
      REAL                      inc_lon
      REAL                      inc_lat
      CHARACTER (LEN=256)       fname

  END TYPE tile_info_t
 
  !
  ! Structure holding information about the topo data.
  !
  TYPE topo_info_t

      REAL                    start_lon
      REAL                    start_lat
      INTEGER                 nb_tiles_per_row     ! number of tile along lon axis for topo region
      INTEGER                 nb_tiles_per_col     ! number of tile along lat axis for topo region
      INTEGER                 ocean

  END TYPE topo_info_t

  !
  ! Structure holding bounding lon/lat about a tile (all values are stored in seconds)
  !
  TYPE tile_box_t

      REAL                    start_lon
      REAL                    start_lat
      REAL                    end_lon
      REAL                    end_lat

  END TYPE tile_box_t

  !
  ! Structure to hold the elevation data in memory. This 
  ! structure is dimensioned to N by M tiles
  !
  TYPE elv_data_t
      
      INTEGER (2), pointer, dimension(:,:)    ::     elv

  END TYPE elv_data_t

  !
  ! The declaration of topo data structure and module variables
  !
  TYPE (tile_info_t), allocatable, dimension(:,:) :: tile_info
  TYPE (topo_info_t)                              :: topo_info
  TYPE (tile_box_t),  allocatable, dimension(:,:) :: tile_box
  TYPE (elv_data_t),  allocatable, dimension(:,:) :: elv_data
  INTEGER, allocatable, dimension(:,:)            :: file_handles
  CHARACTER (len=320)                             :: topo_dir_path
  INTEGER                                         :: m_debug_level
  LOGICAL                                         :: init_flag

  !
  ! Public methods
  !
  PUBLIC GDEM_Init
  PUBLIC GDEM_Final
  PUBLIC GDEM_Get


CONTAINS


! ------------------------------------------------------------
!
!  subroutine to initialize GLOBE DEM data set.
!  
SUBROUTINE GDEM_Init(dir_location, debug_level, rc)
! 

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  CHARACTER (len=*), INTENT(in) :: dir_location  ! location of the globe dem database.
  INTEGER,           INTENT(in) :: debug_level   
  INTEGER, OPTIONAL :: rc
  !
  ! Local variables
  !
  INTEGER          :: str_len                   !
  INTEGER          :: istat
  INTEGER          :: i, j       
!- End of header ------------------------------------------------------

  if (present(rc)) rc = ESMF_SUCCESS

  !
  ! Initialize the module variables
  !
  str_len=len(topo_dir_path)
  topo_dir_path(1:str_len) = ' '
  call ESMF_UtilIOUnitGet(FH_META, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  !
  ! Read the metadata information
  !
  call topo_load_metadata(dir_location, istat)
  if ( istat .ne. SUCCESS ) then
       print*, 'Error encountered while initializing GLOBE terrain database.'
       print*, 'Unable to load metadata information from directory: ',dir_location
       print*, 'Please check the dir path (specified by the namelist variable -> dsdted ) is correct '
       print*, 'and that terrain metadata file [metadata.txt] exists in that directory.'
       call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
         msg='Error encounted while initializing GLOBE terrain database', CONTEXT)
       return ! bail out
  endif

  !
  ! intialized the file handls for reading tiles
  ! 
  allocate(file_handles( topo_info%nb_tiles_per_row, topo_info%nb_tiles_per_col ),stat=istat)
  if ( istat .ne. 0 ) then
      print*, 'Error: Can not allocate memory for file_handles array.'
      if (ESMF_LogFoundAllocError(statusToCheck=istat, &
        msg="Allocation of memory for file_handles failed.", &
        CONTEXT)) return ! bail out
  endif


  do j=1, topo_info%nb_tiles_per_col
     do i=1, topo_info%nb_tiles_per_row
        file_handles(i,j) = -1
     enddo
  enddo

  !
  !  allocate memory structure to hold elvation data
  !
  allocate(elv_data( topo_info%nb_tiles_per_row, topo_info%nb_tiles_per_col ),stat=istat)
  if ( istat .ne. 0 ) then
      print*, 'Error: Can not allocate memory for elv data array.'
      if (ESMF_LogFoundAllocError(statusToCheck=istat, &
        msg="Allocation of memory for elv data array failed.", &
        CONTEXT)) return ! bail out
  endif

  m_debug_level=debug_level
  init_flag = .true. 
  return

END SUBROUTINE


! ------------------------------------------------------------
!
!  subroutine to close GLOBE DEM data set and free up resouce.
!  
SUBROUTINE GDEM_Final(rc)

  ! Subroutine arguments
  INTEGER, OPTIONAL :: rc

  !
  ! Local variables
  !
  INTEGER          :: i, j , istat      
!- End of header ------------------------------------------------------

  if (present(rc)) rc = ESMF_SUCCESS

  do j=1, topo_info%nb_tiles_per_col
     do i=1, topo_info%nb_tiles_per_row
        if ( file_handles(i,j) .ne. -1 ) then
           close(file_handles(i,j))
           if ( m_debug_level .gt. 0 ) &
           print*, 'closing topo database, close file unit: ',file_handles(i,j)
           file_handles(i,j)=-1
           !
           ! Free up buffer memory
           ! 
           deallocate(elv_data(i,j)%elv,stat=istat)
        endif
     enddo
  enddo

  !
  !  Deallocate memory 
  !
  deallocate (file_handles,stat=istat)
  deallocate (tile_info,stat=istat)
  deallocate (tile_box,stat=istat)
  deallocate (elv_data,stat=istat)
  init_flag = .false. 
  return

END SUBROUTINE


! ------------------------------------------------------------
!
!  subroutine to read GLOBE elevation data at the specified lat/lon locations. 
!  
SUBROUTINE GDEM_Get(nb, lons, lats, maskout, result, istat)


  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  INTEGER, INTENT(in) :: nb             ! Number of lat/lon locations 
  REAL, INTENT(in) ::  maskout          ! value to mask out the lat/lon 
                                        ! locations, i.e, if the specfied 
                                        ! lat/lon is equal to the maskout 
                                        ! value, then no elevation value 
                                        ! will be retrived for that locatio.

  ! Array  arguments with intent(in):
  REAL, INTENT(in) :: lons(nb)          ! -180(W) to 180 (E)
  REAL, INTENT(in) :: lats(nb)          !  -90(S)  to 90 (N)
  
  ! Array  arguments with intent(out):
  REAL, INTENT(out)   :: result(nb)     ! Array holding the elevation values.
                                        
  INTEGER,INTENT(out) :: istat          ! Return status
                   
  ! Local scalars:
  INTEGER      :: i                     ! 
  INTEGER      :: ipos                  ! i index of the tile 
  INTEGER      :: jpos                  ! j index of the tile 
  INTEGER      :: icol                  ! 
  INTEGER      :: jrow                  !
  
!- End of header ------------------------------------------------------

  if ( .not. init_flag  ) then
     print*, 'Exit module for reading GLOBE elevation data, module is not initialized.'
     call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=istat, &
       msg='GLOBE module is not initialized', CONTEXT)
     return ! bail out
  endif

  if ( USE_BINLINEAR_INTERP ) then
  !
  ! Use bilinear interpolation
  ! 

     do i = 1, nb
        if ( lons(i) .ne. maskout .and. lats(i) .ne. maskout ) then
        !
        ! Map lat/lon value to i,j and tile location
        !
           call topo_get_interp_elv_fm_mem(lons(i),lats(i),result(i),istat)
           if ( istat .ne. SUCCESS ) then
               print*, 'Invalid lat/lon values in the input, topo data processing stopped'
               call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=istat, &
                 msg='Invalid lat/lon values in the input', CONTEXT)
               return ! bail out
           endif
        else
            result(i)=0.0
        endif
     enddo
     istat = SUCCESS
     return

  else

  !
  ! No interpolation, get the elevation from topo database
  !
     do i = 1, nb
        if ( lons(i) .ne. maskout .and. lats(i) .ne. maskout ) then
        !
        ! Map lat/lon value to i,j and tile location
        !
           call map_lon_lat_to_cell(lons(i),lats(i),ipos,jpos,icol,jrow,istat)
        !
        ! Read the elevation value from the database
        ! 
           if ( istat .eq. SUCCESS ) then
               call topo_get_elv_fm_mem(ipos,jpos,icol,jrow,result(i),istat)
           else
               if ( istat .eq. INVALID_LAT_LON ) then
                  print*, 'Invalid lat/lon values in the input, topo data processing stopped'
                  call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=istat, &
                    msg='Invalid lat/lon values in the input', CONTEXT)
                  return ! bail out
               endif
           endif
        else
           result(i)=0.0
        endif
     enddo
     istat = ESMF_SUCCESS
     return

  endif

END SUBROUTINE


! ------------------------------------------------------------
!
!  internal subroutine to read GLOBE DEM tiles
!  
SUBROUTINE TOPO_READ_ELV(ipos, jpos, col, row, result, istat)

  ! Subroutine arguments

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  INTEGER, INTENT(IN)   :: ipos          ! specifies the i position of the GLOBE tile.
  INTEGER, INTENT(IN)   :: jpos          ! specifies the j position of the GLOBE tile. 
  INTEGER, INTENT(IN)   :: col           ! column index of the specified tile.
  INTEGER, INTENT(IN)   :: row           !row index of the specified tile.
  
  ! Array  arguments with intent(out):
  REAL, INTENT(out)   :: result          ! elevation value on return.
                                         ! 
  INTEGER,INTENT(out) :: istat           ! return status value.
                                         ! 
  ! Local scalars:
  INTEGER (2)  :: val_int16              ! 2-byte integer 
  INTEGER      :: ilen                   !
  INTEGER      :: slen                   !
  INTEGER      :: dlen                   !
  INTEGER      :: record_len             !
  INTEGER      :: ios                    ! io error flag
  INTEGER      :: fpos                   ! the position of read ptr in the tile
                                         !
  CHARACTER (len=321)  :: fname          ! Pathname to the topo file
                                         ! max len 320 ->
                                         ! dirpath (256) + fname(64)
                                         ! + pathname separator
  INTEGER      :: fh                     ! File handle (unit)
!- End of header ------------------------------------------------------


  !
  ! Get the file handle for the tile 
  !
  fh = file_handles(ipos,jpos)
  ! 
  if ( fh == -1 ) then 
      !
      ! build the filename for the tile and open the file.
      !
      slen=len_trim(tile_info(ipos,jpos)%fname)
      dlen=len_trim(topo_dir_path)
      ilen=dlen + 1 + slen
      fname(1:ilen) = topo_dir_path(1:dlen) // &
                       '/'          // &
                     tile_info(ipos,jpos)%fname(1:slen)


      ! Get the size of record length to used for 2 byte integer
      INQUIRE(iolength = record_len) val_int16

      !
      ! set the file handle value ( a base offset + position of 
      ! the tile in the elevation data set.
      !
!     fh= FILE_UNIT_BASE + ( jpos - 1 ) * topo_info%nb_tiles_per_row + ipos
      call ESMF_UtilIOUnitGet(fh, rc=istat)
      if (ESMF_LogFoundError(istat, PASSTHRU)) return ! bail out

      OPEN(unit=fh,file=fname(1:ilen),access='direct',  &
              status='old', action='read', iostat=ios,     &
              form='unformatted',recl=record_len)

      if ( ios /= 0 ) then
          print *, 'Error, can not open topo file ',fname(1:ilen)
          istat = DATAFILE_MISSING
          return
      else
          !
          ! Save the file handle for the next read
          !
          file_handles(ipos,jpos)=fh
          if ( m_debug_level .gt. 0 ) &
          print*, 'file handle: ', fh, 'open tile: ',fname(1:ilen)
      endif
  endif

  fpos = ( ( row - 1 ) * tile_info(ipos,jpos)%nb_col_per_row ) + col

  read(fh,rec=fpos) val_int16

  if ( val_int16 .eq. topo_info%ocean ) then
     result=0.0
  else
     result = val_int16
  endif
    
  istat = SUCCESS
  return

END SUBROUTINE TOPO_READ_ELV


! ------------------------------------------------------------
!
!  internal subroutine to read GLOBE DEM tiles into memory.
!  
SUBROUTINE TOPO_GET_ELV_FM_MEM(ipos, jpos, col, row, result, istat)

  ! Subroutine arguments

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  INTEGER, INTENT(IN)   :: ipos
  INTEGER, INTENT(IN)   :: jpos
  INTEGER, INTENT(IN)   :: col
  INTEGER, INTENT(IN)   :: row
  
  ! Array  arguments with intent(out):
  REAL, INTENT(out)   :: result       ! elevation value on return.
                                      !  
  INTEGER,INTENT(out) :: istat        ! return status value.
                                      ! 
  ! Local scalars:
  INTEGER (2)  :: val_int16           ! 2-byte integer 
!- End of header ------------------------------------------------------

  !
  ! Load the elevation data from the specified tile into an internal
  ! module structure 'elv_data'. 'elv_data' is the structure
  ! declared in mod_topo_defs.mod, it is used to store elevation data
  ! in memory.
  !
  call topo_load_elv_fm_tile(ipos,jpos,istat) 

  !
  ! Get the elevation value from 'elv_data' structure.
  ! 
  val_int16=elv_data(ipos,jpos)%elv(col,row)

  if ( val_int16 .eq. topo_info%ocean ) then
     result=0.0
  else
     result = val_int16
  endif
    
  istat = SUCCESS
  return

! 100 istat=FAIL
!     return

END SUBROUTINE TOPO_GET_ELV_FM_MEM


! ------------------------------------------------------------
!
!  internal subroutine to read GLOBE DEM tile into memory. if the specified tile
!  has already been loaded in the memory, then the function returns silently.
!  
SUBROUTINE TOPO_LOAD_ELV_FM_TILE(ipos, jpos, istat)

  ! Subroutine arguments

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  INTEGER, INTENT(IN)   :: ipos              ! specifies the i pos and
  INTEGER, INTENT(IN)   :: jpos              ! j pos of the tiles.

  INTEGER,INTENT(out) :: istat               ! Error status flag 
                                             !
  ! Local scalars:
  INTEGER (2)  :: val_int16                  ! 2-byte integer (used for 
                                             ! querying the record length 
                                             ! of FORTRAN direct access file.)
  INTEGER      :: ilen                       !
  INTEGER      :: slen                       !
  INTEGER      :: dlen                       !
  INTEGER      :: record_len                 ! Record length (in byte) for 
                                             ! the FORTRAN direct access file.
  INTEGER      :: ios                        ! io error flag
                                             !
  CHARACTER (len=321)  :: fname              ! Pathname to the tile file
                                             ! max len 320 ->
                                             ! dirpath (256) + fname(64)
                                             ! + pathname separator
  INTEGER      :: fh                         ! File handle (unit)
!- End of header ------------------------------------------------------


  !
  ! Get the file handle for the tile 
  !
  fh = file_handles(ipos,jpos)
  ! 
  ! If the already in memory, do nothing.
  !
  if ( fh .ne. -1 ) then 
      istat = SUCCESS
      return
  else
      !
      ! build the filename for the tile and open the file.
      !
      slen=len_trim(tile_info(ipos,jpos)%fname)
      dlen=len_trim(topo_dir_path)
      ilen=dlen + 1 + slen
      fname(1:ilen) = topo_dir_path(1:dlen) // &
                       '/'          // &
                     tile_info(ipos,jpos)%fname(1:slen)


      ! Get the size of record length to used for 2 byte integer
      INQUIRE(iolength = record_len) val_int16

      !
      ! set the file handle value ( a base offset + position of 
      ! the tile in the elevation data set.
      !
      fh= FILE_UNIT_BASE + ( jpos - 1 ) * topo_info%nb_tiles_per_row + ipos
      !
      ! compute the size of the file 
      !
      record_len=record_len * tile_info(ipos,jpos)%nb_col_per_row *  &
                 tile_info(ipos,jpos)%nb_row_per_tile

      OPEN(unit=fh,file=fname(1:ilen),access='direct',  &
              status='old', action='read', iostat=ios,     &
              form='unformatted',recl=record_len)

      if ( ios /= 0 ) then
          print *, 'Error, can not open topo file ',fname(1:ilen)
          istat = DATAFILE_MISSING
          return
      else
          !
          ! Save the file handle for the next read
          !
          file_handles(ipos,jpos)=fh
          if ( m_debug_level .gt. 0 ) &
          print*, 'file handle: ', fh, 'open tile: ',fname(1:ilen)

          !
          ! allocating memory to store the elevation data.
          !
          allocate(elv_data(ipos,jpos)%elv(tile_info(ipos,jpos)%nb_col_per_row,   &
                                           tile_info(ipos,jpos)%nb_row_per_tile), &
                   stat=istat) 
          if ( istat .ne. 0 ) then
              write(*,*) 'Error: Can not allocate memory for elv data.'
              call exit(1)
          endif
          !
          ! load the tile content into memory
          !  
          read(fh,rec=1, err=100) elv_data(ipos,jpos)%elv 
          istat = SUCCESS
          return
      endif
  endif

100 istat=FAIL
    return

END SUBROUTINE TOPO_LOAD_ELV_FM_TILE


! ------------------------------------------------------------
!
!  Internal subroutine to get linearly interpolated
!  elevation value at specified longitude/latiude point
!  from  GLOBE DEM.
!  
SUBROUTINE TOPO_GET_INTERP_ELV_FM_MEM(rlon, rlat, result, istat)

  ! Subroutine arguments

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  REAL,    INTENT(IN)   :: rlon       ! specifies the longitude value 
                                      ! (in degree) the grid point to 
                                      ! obtain terrain elevation
  REAL,    INTENT(IN)   :: rlat       ! specifies the latitude value 
                                      ! (in degree) the grid point to 
                                      ! obtain terrain elevation
  
  ! Scalar arguments with intent(out):
  REAL, INTENT(out)   :: result       ! interpolated terrain elevation value 
                                      ! at specified rlon/rlat location

  INTEGER,INTENT(out) :: istat        ! status value.
                                      ! 
  ! Local array variables:
  REAL      :: lons(1:4)      ! longitude values of the 4 enclosing grid points
  REAL      :: lats(1:4)      ! latitude values of the 4 enclosing grid points
  REAL      :: topodata(1:4)  ! terrain elevation values for the 4 enclosing grid points                     
  INTEGER   :: ipos(1:4)      ! specify the i indices of the tile array location
                              ! for the given enclosing grid points. 
  INTEGER   :: jpos(1:4)      ! specify the j indices of the tile array location
                              ! for the given enclosing grid points. 
  INTEGER   :: icols(1:4)     ! column indices (in the specified tile) for the 
                              ! 4 enclosing grid points.
  INTEGER   :: jrows(1:4)     ! row indices (in the specified tile) for the 
                              ! 4 enclosing grid points.

  ! Local scalars:
  REAL      :: lon            ! specify the longitude (-180 to 180) value (in seconds)
                              ! of the grid point to obtain the terrain elevation.
  REAL      :: lat            ! specify the latitude (-180 to 180) value (in seconds)
                              ! of the grid point to obtain the terrain elevation.
  INTEGER   :: i              ! loop index.               !
  INTEGER   :: nb             ! number of enclosing grid points.

!- End of header ------------------------------------------------------

!
!  convert input lon/lat from degree to second.
!
   lon=rlon*3600.0
   lat=rlat*3600.0
!
!  Find the 4 neighboring cells that contains the specified lat/lon values 
!
  call get_neighbor_cells(lon, lat,  &
                          nb, ipos, jpos, icols, jrows, lons, lats, istat)
  if ( istat .eq. SUCCESS ) then
     result=0.0
     do i = 1, nb
        call topo_get_elv_fm_mem(ipos(i),jpos(i),icols(i),jrows(i),topodata(i),istat)
     enddo
  else
     return
  endif

  !
  ! interpolate the neighboring cells to the spcified lat/lon location
  !
  call binary_intrp(lon,lat,nb,lons,lats,topodata,result)

  istat = SUCCESS
  return

END SUBROUTINE TOPO_GET_INTERP_ELV_FM_MEM


! ------------------------------------------------------------
!
!  internal subroutine to read GLOBE DEM meta data.
!  
SUBROUTINE TOPO_LOAD_METADATA(dir_location, istat)

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  CHARACTER (len=*), INTENT(in) :: dir_location  ! specifies the directory path to the
                                                 ! GLOBE DEM files.
                                                 ! 
  !
  ! Scalar arguments with intent(out):
  INTEGER, INTENT(out)          :: istat
  !
  ! Local variables
  !
  CHARACTER (len=321)          :: fname         ! Pathname to the metafile
                                                ! max len 320 ->
                                                ! dirpath (256) + fname(64)
                                                ! + pathname separator

  CHARACTER (len=256)          :: line          ! char buffer to hold 
                                                ! input string
  CHARACTER (len=32)           :: cbuf 

  INTEGER                      :: str_len       ! Len of the metafile name
  INTEGER                      :: dir_len       ! Len of the dir pathname

  INTEGER                      :: i
  INTEGER                      :: j

  REAL                         :: rval          ! variable to store 
                                                ! input value

  LOGICAL                      :: file_is_open  !

  INTEGER                      :: ios           ! I/O status flag

!- End of header ------------------------------------------------------

  ! 
  ! build the file pathname by concating the directory path 
  ! and metafile name
  !
  dir_len=len_trim(dir_location)

  !
  !  check if the directory pathname ends with a '/'
  !
  if ( dir_location(dir_len:dir_len) .eq. '/' ) then
      str_len = dir_len + len(METADATA_FILE)
      fname(1 : str_len ) = dir_location(1:dir_len) // &
                            METADATA_FILE
      !
      ! Save the directory path with out the ending '/'.
      !
      topo_dir_path(1:dir_len-1)=dir_location(1:dir_len-1)
  else
      str_len = dir_len + 1 + len(METADATA_FILE)
      fname(1 : str_len ) = dir_location(1:dir_len) // &
                            '/'                      // &
                            METADATA_FILE
      !
      ! Save the directory path to the topo data files.
      !
      topo_dir_path(1:dir_len)=dir_location(1:dir_len) 
  endif
  !
  ! Check that this file handle has not been used.
  ! 
  INQUIRE(FH_META,OPENED=file_is_open)
  if ( file_is_open ) then
   close(FH_META)
   file_is_open=.false.
  endif

  if ( .not. (file_is_open) ) then

      OPEN(unit=FH_META,file=fname(1:str_len),  &
           status='old', action='read', iostat=ios,     &
           form='formatted')

      if ( ios /= 0 ) then
         print*, 'Error: metadata file for terrain elevation data not found.'
         print*, 'Can not open: ', fname(1:str_len)
         print*, 'IO status: ', ios
         istat=DATAFILE_MISSING
         return
      endif

  else
      write(*,*) 'Error: file handle (unit=',FH_META,') is already used'
      call exit(1)
  endif

  ! loop until end of file
  LOOP_READ_TOPO_INFO : do while (ios == 0  .And. line(1:3) /= 'END' )

      read(FH_META, '(a)', iostat=ios) line
      If (ios /= 0) Then
          print*, 'Error: encounter while reading meta file: ', &
                  fname(1:str_len)
          istat=FAIL
          return
      Endif
 
      line=adjustl(line)
      ! skip comment line
      if ( line(1:1) .eq. '#' ) then
          cycle
      endif

      !
      ! Parse the line
      !
      read(line,*,iostat=ios) cbuf, rval

      Select Case ( cbuf )

      !-------------------------------------------------------
      Case ('start_lon')
          topo_info%start_lon = rval
          if ( m_debug_level .gt. 0 ) &
          print*,'start_lon= ',topo_info%start_lon

      Case ('start_lat')
          topo_info%start_lat = rval
          if ( m_debug_level .gt. 0 ) &
          print*,'start_lat= ', topo_info%start_lat 
          
      Case ('nb_tiles_per_row')
          topo_info%nb_tiles_per_row = rval
          if ( m_debug_level .gt. 0 ) &
          print*,'nb_tiles along longitude axis ', topo_info%nb_tiles_per_row

      Case ('nb_tiles_per_col')
          topo_info%nb_tiles_per_col = rval
          if ( m_debug_level .gt. 0 ) &
          print*,'nb_tiles along latitude axis ', topo_info%nb_tiles_per_col
          
      Case ('ocean')
          topo_info%ocean = rval
          if ( m_debug_level .gt. 0 ) &
          print*,'ocean= ', topo_info%ocean          
          !
          ! Done reading topo info, now exit the loop and process the tile info next.
          !
          exit LOOP_READ_TOPO_INFO
     
      Case default
          str_len=len_trim(line)
          if ( m_debug_level .gt. 0 ) &
          print *, line(1:str_len)

      End Select

  enddo LOOP_READ_TOPO_INFO

  !
  ! Allocate memory to hold the tile info.
  !
  allocate(tile_info( topo_info%nb_tiles_per_row, topo_info%nb_tiles_per_col ),stat=istat)
  if ( istat .ne. 0 ) then
     write(*,*) 'Error: Can not allocate memory for tile_info array.'
     call exit(1)
  endif
  
  i=1
  j=1
  do while (ios == 0  .and. j .le. topo_info%nb_tiles_per_col )

      read(FH_META, '(a)', iostat=ios) line
      If (ios /= 0) Then
        write(*,*) 'Error: encounter while reading meta file: ', &
                  fname(1:str_len)
        call exit(1)
      Endif
 
      line=adjustl(line)
      ! skip comment line
      if ( line(1:1) .eq. '#' ) then
          cycle
      endif

      !
      ! Parse the line
      !
      read(line,*,iostat=ios) tile_info(i,j)%nb_col_per_row, &
                              tile_info(i,j)%nb_row_per_tile, &
                              tile_info(i,j)%lon, &
                              tile_info(i,j)%lat, &
                              tile_info(i,j)%inc_lon, &
                              tile_info(i,j)%inc_lat, &
                              tile_info(i,j)%fname
      if ( ios /= 0 ) then
         print*, 'Error: encounter while reading meta file: ', &
                  fname(1:str_len)
         istat=FAIL
         return
      else
         tile_info(i,j)%fname=trim(tile_info(i,j)%fname)
         !
         ! increment the i,j counter 
         !
         i=i+1
         if ( i .gt. topo_info%nb_tiles_per_row ) then
            i=1
            j=j+1
         endif
      endif   
  enddo

  !
  ! print out tile info
  !
  if ( m_debug_level .gt. 0 ) then
     do j=1, topo_info%nb_tiles_per_col
        do i=1, topo_info%nb_tiles_per_row
           print *, tile_info(i,j)%nb_col_per_row, &
                tile_info(i,j)%nb_row_per_tile, &
                tile_info(i,j)%lon, &
                tile_info(i,j)%lat, &
                tile_info(i,j)%inc_lon, &
                tile_info(i,j)%inc_lat, &
                tile_info(i,j)%fname
        enddo
     enddo
  endif

  !
  ! Allocate memory to hold the tile box
  !
  allocate(tile_box( topo_info%nb_tiles_per_row, topo_info%nb_tiles_per_col ),stat=istat)
  if ( istat .ne. 0 ) then
     write(*,*) 'Error: Can not allocate memory for tile_box array.'
     call exit(1)
  endif

  !
  ! Compute the lat/lon bounds of the each tile
  !
  do j=1, topo_info%nb_tiles_per_col
     do i=1, topo_info%nb_tiles_per_row
        tile_box(i,j)%start_lon = tile_info(i,j)%lon * 3600.0
        tile_box(i,j)%start_lat = tile_info(i,j)%lat * 3600.0
        tile_box(i,j)%end_lon = tile_box(i,j)%start_lon + &
                                (tile_info(i,j)%inc_lon * tile_info(i,j)%nb_col_per_row)
        tile_box(i,j)%end_lat = tile_box(i,j)%start_lat - &
                                (tile_info(i,j)%inc_lat * tile_info(i,j)%nb_row_per_tile)

        if ( m_debug_level .gt. 0 ) then
           print *, tile_box(i,j)%start_lon/3600.0, &
                  tile_box(i,j)%start_lat/3600.0, &
                  tile_box(i,j)%end_lon/3600.0, &
                  tile_box(i,j)%end_lat/3600.0
        endif

     enddo
  enddo

  istat = SUCCESS
  return

END SUBROUTINE TOPO_LOAD_METADATA


! ------------------------------------------------------------
!
!  internal subroutines to map lat/lon value to a specific tile
!  and i,j location in that tile. Lat/lon inputs are has unit of degree.
!  
SUBROUTINE MAP_LON_LAT_TO_CELL(rlon, rlat, ipos, jpos, icol, jrow, istat)


  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  REAL, INTENT(in)     :: rlon                 ! longitude (-180(w)  to 180 (e) ) 
  REAL, INTENT(in)     :: rlat                 ! latitude  (-90 to 90 )
  INTEGER, INTENT(out) :: ipos                 ! i index of the tile which lon/lat position is located
  INTEGER, INTENT(out) :: jpos                 ! j index of the tile which lon/lat position is located
  INTEGER, INTENT(out) :: icol                 ! the i index for the specified lon value
  INTEGER, INTENT(out) :: jrow                 ! the j index for the specified lat value.
  INTEGER, INTENT(out) :: istat                ! status flag: 
                                               !      SUCCESS
                                               !      INVALID_LAT_LON
  
  ! 
  ! Local scalars:

  REAL         :: lon
  REAL         :: lat

!- End of header ------------------------------------------------------

  !
  ! convert input lon/lat value to seconds
  !
  lon=rlon * 3600.0
  lat=rlat * 3600.0

  call lon_lat_to_cell(lon, lat, ipos, jpos, icol, jrow, istat)

  return

END SUBROUTINE MAP_LON_LAT_TO_CELL


! ------------------------------------------------------------
!
!  internal subroutines to map lat/lon value to a specific tile
!  and i,j location in that tile. Lat/lon inputs are has unit of second.
!  
SUBROUTINE LON_LAT_TO_CELL(rlon, rlat, ipos, jpos, icol, jrow, istat)

  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  REAL, INTENT(in)     :: rlon                 ! longitude (-180(w)  to 180 (e) ) 
  REAL, INTENT(in)     :: rlat                 ! latitude  (-90 to 90 )
  INTEGER, INTENT(out) :: ipos                 ! i index of the tile which lon/lat position is located
  INTEGER, INTENT(out) :: jpos                 ! j index of the tile which lon/lat position is located
  INTEGER, INTENT(out) :: icol                 ! the i index for the specified lon value
  INTEGER, INTENT(out) :: jrow                 ! the j index for the specified lat value.
  INTEGER, INTENT(out) :: istat                ! status flag: 
                                               !      SUCCESS
                                               !      INVALID_LAT_LON
  
  ! 
  ! Local scalars:
  REAL         :: gval                         ! the height (in second) of the tile
  INTEGER      :: i                            !
  INTEGER      :: j                            !
!- End of header ------------------------------------------------------

  do i = 1, topo_info%nb_tiles_per_row
      !
      !  found the ipos of the tile which this lon/lat value belongs
      !
      if ( rlon .ge. tile_box(i,1)%start_lon .and. rlon .lt. tile_box(i,1)%end_lon ) then
          ipos=i
          !
          ! now look for jpos of the tile which this lat value belongs
          !
          do j = 1, topo_info%nb_tiles_per_col
              if ( rlat .le. tile_box(1,j)%start_lat .and. rlat .gt. tile_box(1,j)%end_lat ) then
                 jpos=j
                 !
                 !  now find the i and j cell position corresponds to the lat/lon value
                 !
                 gval=(rlon-tile_box(ipos,jpos)%start_lon) / tile_info(ipos,jpos)%inc_lon 
                 icol=int(gval)+1

                 gval=(tile_box(ipos,jpos)%start_lat-rlat) / tile_info(ipos,jpos)%inc_lat
                 jrow=int(gval)+1       
          
                 istat=SUCCESS
                 return
              endif
          enddo  
      endif
  enddo  

  istat = INVALID_LAT_LON
  return

END SUBROUTINE LON_LAT_TO_CELL


! ------------------------------------------------------------
!
!  internal subroutines to compute tile location, i and j of 4 
!  nearest (enclosing) neighbors of the specified lat/lon grid point.
!  
SUBROUTINE GET_NEIGHBOR_CELLS(rlon, rlat, nb_cells, &
                              ipos, jpos, icol, jrow, lons, lats, &
                              istat)


  ! Subroutine arguments
  ! Scalar arguments with intent(in):
  REAL, INTENT(in)     :: rlon                 ! longitude (-180 * 3600 (w)  to 180 * 3600 (e) ) in seconds
  REAL, INTENT(in)     :: rlat                 ! latitude  (-90 * 3600 (s) to 90 * 3600(n) ) in seconds

  ! Arguments with intent(out) 
  !
  INTEGER, INTENT(out) :: nb_cells             ! number of elements in the jpos/ipos
                                               ! icol/jrow arrays

  INTEGER, INTENT(out) :: ipos(1:4)            ! the i indices into the tile array 
                                               ! for the given enclosing grid points. 

  INTEGER, INTENT(out) :: jpos(1:4)            ! the j indices into the tile array 
                                               ! for the given enclosing grid points. 

  INTEGER, INTENT(out) :: icol(1:4)            ! column indices (in the specified tile) for 
                                               ! the enclosing grid points.

  INTEGER, INTENT(out) :: jrow(1:4)            ! row indices (in the specified tile) 
                                               ! for the enclosing grid points.

  REAL, INTENT(out)    :: lons(1:4)            ! longitude values of the surronding neighbor cells 
  REAL, INTENT(out)    :: lats(1:4)            ! latitude values of the surronding neighbor cells
  
  INTEGER, INTENT(out) :: istat                ! status flag: 
                                               !      SUCCESS
                                               !      INVALID_LAT_LON
  
  ! 
  ! Local scalars:
!  REAL         :: gval                         ! 
!  REAL         :: lon                          !
!  REAL         :: lat
!  INTEGER      :: i                            !
!  INTEGER      :: j                            !
  REAL         :: cell_lon  ! the longitude (-180 to 180) value (in seconds)
                            ! at the center of the cell which the specified grid
                            ! is located.
  REAL         :: cell_lat  ! the latitude (90 to 90) value (in seconds)
                            ! at the center of the cell which the specified grid
                            ! is located
  INTEGER      :: i_pos     ! i index into tile array for the specified longitude point 
  INTEGER      :: j_pos     ! j index into tile array for the specified latitude point 
  INTEGER      :: i_col     ! column position in the tile for the specified longitude point
  INTEGER      :: j_row     ! row position in the tile for the specified latitude point

!- End of header ------------------------------------------------------

  !
  !  Get the tile location and i,j position of the cell 
  ! 
  call lon_lat_to_cell(rlon, rlat, i_pos, j_pos, i_col, j_row, istat)

  if ( istat .eq. SUCCESS ) then
      !
      ! compute the lat/lon value at the center of the cell which
      ! the specified lat/lon is located (all unit are in seconds)
      !
      cell_lon = tile_box(i_pos,j_pos)%start_lon + i_col*tile_info(i_pos,j_pos)%inc_lon -  &
                 tile_info(i_pos,j_pos)%inc_lon/2.0 

      cell_lat = tile_box(i_pos,j_pos)%start_lat - j_row*tile_info(i_pos,j_pos)%inc_lat + &
                 tile_info(i_pos,j_pos)%inc_lat/2.0
      ! print*,'==============================='
      ! print*,'cell lon= ',cell_lon/3600.0, 'cell_lat= ', cell_lat/3600.0
  else
      print*, 'Invalid lon/lat values in the input', rlon/3600.0, rlat/3600.0
      istat=INVALID_LAT_LON
      return
  endif

  !
  ! determine the relative offset of the specified lat/lon from
  ! the cell center lat/lon values.  We use the information to 
  ! determine the exepcted lat/lon values of the surrounding cells.
  ! 
  !

  !
  ! case 1, the lat/lon falls in the lower right quadrant of the 
  ! the current cell, this case also catch the lat/lon that
  ! is located at the center of the given cell.
  !
  if ( rlon .ge. cell_lon .and. rlat .le. cell_lat ) then
     lons(1) = cell_lon
     lats(1) = cell_lat
     lons(2) = cell_lon + tile_info(i_pos,j_pos)%inc_lon
     lats(2) = cell_lat
     lons(3) = cell_lon
     lats(3) = cell_lat - tile_info(i_pos,j_pos)%inc_lat
     lons(4) = cell_lon + tile_info(i_pos,j_pos)%inc_lon
     lats(4) = cell_lat - tile_info(i_pos,j_pos)%inc_lat

     !
     ! compute the tile location and i,j indexes for the neighboring cells
     !
     ipos(1)=i_pos
     jpos(1)=j_pos
     icol(1)=i_col
     jrow(1)=j_row
     call lon_lat_to_cell(lons(2),lats(2),ipos(2),jpos(2),icol(2),jrow(2),istat)
     call lon_lat_to_cell(lons(3),lats(3),ipos(3),jpos(3),icol(3),jrow(3),istat)
     call lon_lat_to_cell(lons(4),lats(4),ipos(4),jpos(4),icol(4),jrow(4),istat)
     nb_cells=4
     istat = SUCCESS
     ! print*, 'case 1'
     return
  endif

  !
  ! case 2, the lat/lon falls in the upper right quadrant of the 
  ! the current cell
  !
  if ( rlon .ge. cell_lon .and. rlat .gt. cell_lat ) then
     lons(1) = cell_lon
     lats(1) = cell_lat + tile_info(i_pos,j_pos)%inc_lat
     lons(2) = cell_lon + tile_info(i_pos,j_pos)%inc_lon
     lats(2) = cell_lat + tile_info(i_pos,j_pos)%inc_lat
     lons(3) = cell_lon
     lats(3) = cell_lat 
     lons(4) = cell_lon + tile_info(i_pos,j_pos)%inc_lon
     lats(4) = cell_lat 

     !
     ! compute the tile location and i,j indexes for the neighboring cells
     !
     ipos(3)=i_pos
     jpos(3)=j_pos
     icol(3)=i_col
     jrow(3)=j_row
     call lon_lat_to_cell(lons(1),lats(1),ipos(1),jpos(1),icol(1),jrow(1),istat)
     call lon_lat_to_cell(lons(2),lats(2),ipos(2),jpos(2),icol(2),jrow(2),istat)
     call lon_lat_to_cell(lons(4),lats(4),ipos(4),jpos(4),icol(4),jrow(4),istat)
     nb_cells=4
     istat = SUCCESS 
     ! print*, 'case 2'
     return
  endif

  !
  ! case 3, the lat/lon falls in the lower left quadrant of the 
  ! the current cell
  !
  if ( rlon .lt. cell_lon .and. rlat .le. cell_lat ) then
     lons(1) = cell_lon - tile_info(i_pos,j_pos)%inc_lon
     lats(1) = cell_lat 
     lons(2) = cell_lon 
     lats(2) = cell_lat 
     lons(3) = cell_lon - tile_info(i_pos,j_pos)%inc_lon 
     lats(3) = cell_lat - tile_info(i_pos,j_pos)%inc_lat
     lons(4) = cell_lon 
     lats(4) = cell_lat - tile_info(i_pos,j_pos)%inc_lat

     !
     ! compute the tile location and i,j indexes for the neighboring cells
     !
     ipos(2)=i_pos
     jpos(2)=j_pos
     icol(2)=i_col
     jrow(2)=j_row
     call lon_lat_to_cell(lons(1),lats(1),ipos(1),jpos(1),icol(1),jrow(1),istat)
     call lon_lat_to_cell(lons(3),lats(3),ipos(3),jpos(3),icol(3),jrow(3),istat)
     call lon_lat_to_cell(lons(4),lats(4),ipos(4),jpos(4),icol(4),jrow(4),istat)
     nb_cells=4
     istat = SUCCESS
     ! print*, 'case 3'
     return
  endif

  !
  ! case 4, the lat/lon falls in the upper left quadrant of the 
  ! the current cell
  !
  if ( rlon .lt. cell_lon .and. rlat .gt. cell_lat ) then
     lons(1) = cell_lon - tile_info(i_pos,j_pos)%inc_lon
     lats(1) = cell_lat + tile_info(i_pos,j_pos)%inc_lat
     lons(2) = cell_lon 
     lats(2) = cell_lat + tile_info(i_pos,j_pos)%inc_lat
     lons(3) = cell_lon - tile_info(i_pos,j_pos)%inc_lon
     lats(3) = cell_lat 
     lons(4) = cell_lon 
     lats(4) = cell_lat

     !
     ! compute the tile location and i,j indexes for the neighboring cells
     !
     ipos(4)=i_pos
     jpos(4)=j_pos
     icol(4)=i_col
     jrow(4)=j_row
     call lon_lat_to_cell(lons(1),lats(1),ipos(1),jpos(1),icol(1),jrow(1),istat)
     call lon_lat_to_cell(lons(2),lats(2),ipos(2),jpos(2),icol(2),jrow(2),istat)
     call lon_lat_to_cell(lons(3),lats(3),ipos(3),jpos(3),icol(3),jrow(3),istat)
     nb_cells=4
     istat = SUCCESS 
     ! print*, 'case 4'
     return
  endif

  istat = INVALID_LAT_LON
  return

END SUBROUTINE GET_NEIGHBOR_CELLS


!------------------------------------------------------------
!
!     bintrp:  bilinearly interpolates the elvation value to 
!              the model grid locations
!
SUBROUTINE BINARY_INTRP(rlon,rlat,nb,lons,lats,topodata,result)
  !
  IMPLICIT NONE

  !
  !
  ! Subroutine arguments
  ! arguments with intent(in)
  !
  REAL, INTENT(in)      :: rlon           ! the lon of location to interp to 
  REAL, INTENT(in)      :: rlat           ! the lat of location to interp to
  INTEGER, INTENT(in)   :: nb             ! number of elements in the input arrays
  
  REAL, INTENT(in)      :: lons(1:nb)     ! lon of the elevation points used 
                                          ! in the interpolation  
  REAL, INTENT(in)      :: lats(1:nb)     ! lat of the elevation points used 
                                          ! in the interpolation  
  REAL, INTENT(in)      :: topodata(1:nb) ! elevation data used in the interpolation

  !
  ! arguments with intent(out)
  !
  REAL, INTENT(out)     :: result         ! the interpolated value.

  ! 
  ! Local scalars:
  !
  REAL                  :: ri
  REAL                  :: rj
  REAL                  :: delta_lon
  REAL                  :: delta_lat
  !
  ! - End of header --------------------------------------------
  ! 
  !
  !     perform bilinear-interpolations at observation sites
  !
  !   The input points are      1------2
  !                             |      |
  !                             |   x  |
  !                             |      |
  !                             3------4
  !
  !  and x is the location to interpret to inside the box
  !
  delta_lon = lons(2) - lons(1)
  delta_lat = lats(1) - lats(3)

  ri = (rlon - lons(1)) / delta_lon
  rj = (lats(1) - rlat) / delta_lat

  result = topodata(1) * (1.0-ri) * (1.0-rj)  +  &        
           topodata(2) *    ri    * (1.0-rj)  +  &        
           topodata(3) * (1.0-ri) *    rj     +  &        
           topodata(4) *    ri    *    rj

  return

END SUBROUTINE BINARY_INTRP


END MODULE
