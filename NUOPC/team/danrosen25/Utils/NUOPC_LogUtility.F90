#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "NUOPC_LogUtility.F90"
#define MODNAME "NUOPC_LogUtility"

module NUOPC_LogUtility
  use ESMF
  use NUOPC

  implicit none

  private

  public :: NUOPC_LogStateList
  public :: NUOPC_LogState
  public :: NUOPC_LogFieldConnections
  public :: NUOPC_LogGrid
  public :: NUOPC_LogFieldList
  public :: NUOPC_LogField
  public :: NUOPC_LogFieldValue
  public :: NUOPC_LogArrayValue
  public :: NUOPC_LogCplList

  contains

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogStateList(stateList,nestedFlag,label,rc)
    type(ESMF_State),intent(in)          :: stateList(:)
    logical,intent(in),optional          :: nestedFlag
    character(len=*),intent(in),optional :: label
    integer, intent(out),optional        :: rc

    ! local variables
    character(len=64) :: llabel
    integer           :: sIndex

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'NUOPC_LogStateList'
    endif

    do sIndex=1, size(stateList)
      call NUOPC_LogState(stateList(sIndex),nestedFlag,label,rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogState(state,nestedFlag,label,rc)
    type(ESMF_State),intent(in)  :: state
    logical,intent(in),optional  :: nestedFlag
    character(len=*),optional    :: label
    integer,intent(out),optional :: rc

    ! local variables
    character(len=64)                     :: llabel
    integer                               :: stat
    integer                               :: itemCount
    character(len=64),allocatable         :: itemNameList(:)
    type(ESMF_StateItem_Flag),allocatable :: itemTypeList(:)
    character(len=64)                     :: stateName
    character(len=12)                     :: itemTypeStr
    integer                               :: iIndex
    character(len=ESMF_MAXSTR)           :: logMsg

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'NUOPC_LogState'
    endif

    call ESMF_StateGet(state, nestedFlag=nestedFlag, &
      itemCount=itemCount, name=stateName, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out  
   
    if (itemCount > 0 ) then
      allocate(itemNameList(itemCount),itemTypeList(itemCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of item list memory failed.", &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out
      call ESMF_StateGet(state, nestedFlag=.TRUE., &
        itemNameList=itemNameList,itemTypeList=itemTypeList, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    else
      write (logMsg,"(A,A)") trim(llabel)//": ", &
        trim(stateName)//" is empty."
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, &
          line=__LINE__, file=FILENAME)
    endif

    do iIndex=1, itemCount
      if (itemTypeList(iIndex) == ESMF_STATEITEM_ARRAY) then
        itemTypeStr = 'ARRAY'
      elseif (itemTypeList(iIndex) == ESMF_STATEITEM_ARRAYBUNDLE) then
        itemTypeStr = 'ARRAYBUNDLE'
      elseif (itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        itemTypeStr = 'FIELD'
      elseif (itemTypeList(iIndex) == ESMF_STATEITEM_FIELDBUNDLE) then
        itemTypeStr = 'FIELDBUNDLE'
      elseif (itemTypeList(iIndex) == ESMF_STATEITEM_ROUTEHANDLE) then
        itemTypeStr = 'ROUTEHANDLE'
      elseif (itemTypeList(iIndex) == ESMF_STATEITEM_STATE) then
        itemTypeStr = 'STATE'
      else
        itemTypeStr = 'UNKNOWN'
      endif

      write (logMsg,"(A,A,(A,I0,A,I0,A),A)") trim(llabel)//": ", &
        trim(stateName), &
        " StateItem(",iIndex," of ",itemCount,") ", &
        trim(itemNameList(iIndex))//"["//itemTypeStr//"]"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, &
        line=__LINE__, file=FILENAME)
    enddo

    if (itemCount > 0) then
      deallocate(itemNameList,itemTypeList,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of item list memory failed.", &
        line=__LINE__, file=FILENAME)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogFieldConnections(state,nestedFlag,label,rc)
    ! ARGUMENTS
    type(ESMF_State), intent(in)            :: state
    logical, intent(in), optional           :: nestedFlag
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc
    ! LOCAL VARIABLES
    character(len=64)                       :: llabel
    character(len=64), allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    character(len=64)                       :: stateName
    integer                                 :: iIndex, itemCount
    integer                                 :: stat
    character(len=ESMF_MAXSTR)              :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS
    if(present(label)) then
      llabel = trim(label)
    else
      llabel = 'NUOPC_LogFieldConnections'
    endif

    call ESMF_StateGet(state, itemCount=itemCount, &
      nestedFlag=nestedFlag,name=stateName,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(itemNameList(itemCount),itemTypeList(itemCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of item name and type list memory failed.", &
      line=__LINE__, file=FILENAME)) &
      return  ! bail out

    call ESMF_StateGet(state, itemNameList=itemNameList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    do iIndex=1, itemCount
      if (itemTypeList(iIndex) /= ESMF_STATEITEM_FIELD) then
        write (logMsg,"(A,A,(A,I0,A,I0,A),A)") trim(llabel)//": ", &
          trim(stateName), &
          "(",iIndex," of ",itemCount,") ", &
          trim(itemNameList(iIndex))//" is not a field."
      elseif (NUOPC_IsConnected(state, fieldName=itemNameList(iIndex))) then
        write (logMsg,"(A,A,(A,I0,A,I0,A),A)") trim(llabel)//": ", &
          trim(stateName), &
          "(",iIndex," of ",itemCount,") ", &
          trim(itemNameList(iIndex))//" is connected."
      else
        write (logMsg,"(A,A,(A,I0,A,I0,A),A)") trim(llabel)//": ", &
          trim(stateName), &
          "(",iIndex," of ",itemCount,") ", &
          trim(itemNameList(iIndex))//" is not connected."
      endif
    enddo

    deallocate(itemNameList,itemTypeList,stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Deallocation of item name and type list memory failed.", &
      line=__LINE__, file=FILENAME)) &
      return  ! bail out

    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, line=__LINE__,file=FILENAME)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogGrid(grid,label,rc)
    ! ARGUMENTS
    type(ESMF_Grid), intent(in)            :: grid
    character(len=*), intent(in), optional :: label
    integer, intent(out), optional         :: rc

    ! LOCAL VARIABLES
    character(len=64)           :: llabel
    character(len=64)           :: gridName
    type(ESMF_DistGrid)         :: distgrid
    character(len=64)           :: transferAction
    integer                     :: localDeCount
    integer                     :: dimCount, tileCount
    integer                     :: dimIndex, tileIndex
    integer, allocatable        :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer                     :: stat
    character(len=ESMF_MAXSTR)  :: logMsg

    if (present(rc)) rc = ESMF_SUCCESS
    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'NUOPC_LogGrid'
    endif

    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, name=gridName, &
      localDeCount=localDeCount, distgrid=distgrid, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " local decomposition count=",localDeCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, line=__LINE__, file=FILENAME)

    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " dimension count=",dimCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, line=__LINE__, file=FILENAME)
    write (logMsg,"(A,A,(A,I0))") trim(llabel)//": ", &
      trim(gridName), &
      " tile count=",tileCount
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, line=__LINE__, file=FILENAME)

    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of index array memory failed.", &
      line=__LINE__, file=FILENAME)) &
      return  ! bail out

    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
       maxIndexPTile=maxIndexPTile, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    do tileIndex=1,tileCount
    do dimIndex=1,dimCount
      write (logMsg,"(A,A,A,4(I0,A))") trim(llabel)//": ", &
        trim(gridName), &       
        " (tile,dim,minIndexPTile,maxIndexPTile)=(", &
        tileIndex,",",dimIndex,",", &
        minIndexPTile(dimIndex,tileIndex),",", &
        maxIndexPTile(dimIndex,tileIndex),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, line=__LINE__, file=FILENAME)
    enddo
    enddo

    deallocate(minIndexPTile, maxIndexPTile,stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Deallocation of index array memory failed.", &
      line=__LINE__, file=FILENAME)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogFieldList(fieldList,label,rc)
    type(ESMF_Field),intent(in)          :: fieldList(:)
    character(len=*),intent(in),optional :: label
    integer,intent(out),optional         :: rc

    ! LOCAL VARIABLES
    character(len=64) :: llabel
    integer           :: fIndex

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'NUOPC_LogFieldList'
    endif

    do fIndex=1,size(fieldList)
      call NUOPC_LogField(fieldList(fIndex),llabel,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogField(field,label,rc)
    type(ESMF_Field)                     :: field
    character(len=*),intent(in),optional :: label
    integer,intent(out),optional         :: rc

    ! local variables
    character(len=64)            :: llabel
    character(ESMF_MAXSTR)       :: logMsg
    integer                      :: stat
    type(ESMF_FieldStatus_Flag)  :: fieldStatus
    character(len=10)            :: fieldStatusStr
    character(ESMF_MAXSTR)       :: fieldName
    type(ESMF_GeomType_Flag)     :: fieldGeomtype
    character(len=10)            :: fieldGeomtypeStr
    character(len=64)            :: fieldConsumerConn
    character(len=64)            :: fieldTransferOffer
    character(len=64)            :: fieldTransferAction

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'NUOPC_LogField'
    endif

    call ESMF_FieldGet(field,status=fieldStatus,name=fieldName,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (fieldStatus == ESMF_FIELDSTATUS_EMPTY) then
      fieldStatusStr = 'EMPTY'
    elseif (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
      fieldStatusStr = 'GRIDSET'
    elseif (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
      fieldStatusStr = 'COMPLETE'
    else
      fieldStatusStr = 'UNKNOWN'
    endif

    if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE .OR. &
    fieldStatus == ESMF_FIELDSTATUS_GRIDSET ) then
      call ESMF_FieldGet(field, geomtype=fieldGeomtype,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      if (fieldGeomtype == ESMF_GEOMTYPE_GRID) then
        fieldGeomtypeStr = 'GRID'
      elseif (fieldGeomtype == ESMF_GEOMTYPE_MESH) then
        fieldGeomtypeStr = 'MESH'
      elseif (fieldGeomtype == ESMF_GEOMTYPE_XGRID) then
        fieldGeomtypeStr = 'XGRID'
      elseif (fieldGeomtype == ESMF_GEOMTYPE_LOCSTREAM) then
        fieldGeomtypeStr = 'LOCSTREAM'
      else
        fieldGeomtypeStr = 'UNKNOWN'
      endif
    else
      fieldGeomtypeStr = 'NOTSET'
    endif

    call NUOPC_GetAttribute(field, name="ConsumerConnection", &
      value=fieldConsumerConn, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_GetAttribute(field, name="TransferOfferGeomObject", &
      value=fieldTransferOffer, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
      value=fieldTransferAction, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (logMsg,"(A,A,(2A))") trim(llabel)//": ", &
      trim(fieldName), &
      " status=",trim(fieldStatusStr)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, &
      line=__LINE__, file=FILENAME)
    write (logMsg,"(A,A,(2A))") trim(llabel)//": ", &
      trim(fieldName), &
      " geomtype=",trim(fieldGeomtypeStr)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, &
      line=__LINE__, file=FILENAME)
    write (logMsg,"(A,A,(2A))") trim(llabel)//": ", &
      trim(fieldName), &
      " consumerconn=",trim(fieldConsumerConn)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, &
      line=__LINE__, file=FILENAME)
    write (logMsg,"(A,A,2(2A))") trim(llabel)//": ", &
      trim(fieldName), &
      " transferOffer=",trim(fieldTransferOffer), &
      " transferAction=",trim(fieldTransferAction)
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, &
      line=__LINE__, file=FILENAME)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogFieldValue(field, label, rc)
    ! ARGUMENTS
    type(ESMF_Field), intent(in)            :: field
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    type(ESMF_Array)  :: array
    character(len=64) :: llabel
    character(len=64) :: fieldName

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = "NUOPC_LogFieldValue"
    endif

    call ESMF_FieldGet(field,array=array,name=fieldName,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return    

    call NUOPC_LogArrayValue(array,fieldName,llabel,rc)
    if (ESMF_STDERRORCHECK(rc)) return

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogArrayValue(array, fieldName, label, rc)
    ! ARGUMENTS
    type(ESMF_Array), intent(in)            :: array
    character(len=*), intent(in), optional  :: fieldName
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    character(len=130)          :: llabel
    type(ESMF_TypeKind_Flag)    :: typekind
    integer                     :: rank
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_1D(:)
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_2D(:,:)
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_3D(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_1D(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_2D(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_3D(:,:,:)
    character(len=ESMF_MAXSTR)  :: logMsg

    if(present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      if (present(fieldName)) then
        llabel = trim(label)//" "//trim(fieldName)
      else
        llabel = trim(label)
      endif
    else
      if (present(fieldName)) then
        llabel = "NUOPC_LogArrayValue"//" "//trim(fieldName)
      else
        llabel = "NUOPC_LogArrayValue"
      endif
    endif

    call ESMF_ArrayGet(array, typekind=typekind,rank=rank, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (typekind == ESMF_TYPEKIND_R4) then
      if (rank == 1) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_1D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(logMsg,'(A,A,3(F0.3,A))') trim(llabel)//": ", &
          "(min,max,sum)=(", &
          minval(dataPtr_R4_1D),",", &
          maxval(dataPtr_R4_1D),",", &
          sum(dataPtr_R4_1D),")"
      elseif (rank == 2) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_2D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(logMsg,'(A,A,3(F0.3,A))') trim(llabel)//": ", &
          "(min,max,sum)=(", &
          minval(dataPtr_R4_2D),",", &
          maxval(dataPtr_R4_2D),",", &
          sum(dataPtr_R4_2D),")"
      elseif (rank == 3) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_3D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(logMsg,'(A,A,3(F0.3,A))') trim(llabel)//": ", &
          "(min,max,sum)=(", &
          minval(dataPtr_R4_3D),",", &
          maxval(dataPtr_R4_3D),",", &
          sum(dataPtr_R4_3D),")"
      else
        write(logMsg,"(A,A)") trim(llabel)//": ", &
          "rank out of NUOPC_LogArrayValue range."
      endif
    elseif (typekind == ESMF_TYPEKIND_R8) then
      if (rank == 1) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_1D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(logMsg,'(A,A,3(F0.3,A))') trim(llabel)//": ", &
          "(min,max,sum)=(", &
          minval(dataPtr_R8_1D),",", &
          maxval(dataPtr_R8_1D),",", &
          sum(dataPtr_R8_1D),")"
      elseif (rank == 2) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_2D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(logMsg,'(A,A,3(F0.3,A))') trim(llabel)//": ", &
          "(min,max,sum)=(", &
          minval(dataPtr_R8_2D),",", &
          maxval(dataPtr_R8_2D),",", &
          sum(dataPtr_R8_2D),")"
      elseif (rank == 3) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_3D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(logMsg,'(A,A,3(F0.3,A))') trim(llabel)//": ", &
          "(min,max,sum)=(", &
          minval(dataPtr_R8_3D),",", &
          maxval(dataPtr_R8_3D),",", &
          sum(dataPtr_R8_3D),")"
      else
        write(logMsg,"(A,A)") trim(llabel)//": ", &
          "rank out of NUOPC_LogArrayValue range."
      endif
    else
      write(logMsg,"(A,A)") trim(llabel)//": ", &
        "typekind out of NUOPC_LogArrayValue range."
    endif

    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, line=__LINE__, file=FILENAME)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine NUOPC_LogCplList(cplcomp,label,rc)
    type(ESMF_CplComp),intent(in)        :: cplcomp
    character(len=*),intent(in),optional :: label
    integer, intent(out),optional        :: rc

    ! local variables
    character(len=64)                   :: llabel
    character(ESMF_MAXSTR), allocatable :: cplList(:)
    integer                             :: cplListSize
    integer                             :: cIndex
    integer                             :: stat
    character(len=64)                   :: name
    character(ESMF_MAXSTR)              :: logMsg

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(label)) then
      llabel = trim(label)
    else
      llabel = 'NUOPC_LogCplList'
    endif

    ! query the CplComp for info
    call ESMF_CplCompGet(cplcomp, name=name, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! get the CplList Attribute
    call NUOPC_CompAttributeGet(cplcomp, name="CplList", &
      itemCount=cplListSize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (cplListSize>0) then
      allocate(cplList(cplListSize), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of internal CplList memory failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
      call NUOPC_CompAttributeGet(cplcomp, name="CplList", valueList=cplList, &
        rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
   else
     write (logMsg,"(A,A,A)") trim(llabel)//": ", &
       trim(name), &
       " CplList is empty."
     call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, &
        line=__LINE__, file=FILENAME)
   endif

    ! main loop over all entries in the cplList
    do cIndex=1, cplListSize
      write (logMsg,"(A,A,(A,I0,A,I0,A),A)") trim(llabel)//": ", &
        trim(name), &
        " CplListItem(",cIndex, " of ", cplListSize, ")=", &
        trim(cplList(cIndex))
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, &
        line=__LINE__, file=FILENAME)
    enddo

    ! clean-up
    if (cplListSize>0) then
      deallocate(cplList,stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Deallocation of internal CplList memory failed.", &
        line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
        return  ! bail out
    endif

  end subroutine

end module
