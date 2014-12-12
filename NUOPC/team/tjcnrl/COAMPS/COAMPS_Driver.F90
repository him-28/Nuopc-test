!-------------------------------------------------------------------------------
! COAMPS Driver Component
!-------------------------------------------------------------------------------
! CONFIGURATION IDENTIFICATION $HeadURL$
! CONFIGURATION IDENTIFICATION @(#)$Id$
!-------------------------------------------------------------------------------
#define FILENAME "COAMPS_Driver.F90"
#include "COAMPS_Macros.h"

!-------------------------------------------------------------------------------
! Default included component modules
!-------------------------------------------------------------------------------
#define FRONT_DUMB COAMPS_Mdumb
#define FRONT_CON  COAMPS_Connector
#undef  FRONT_MED

#define FRONT_ATM0 COAMPS_Mdata
#define  TYPE_ATM0 "data"
#define FRONT_OCN0 COAMPS_Mdata
#define  TYPE_OCN0 "data"
#define FRONT_WAV0 COAMPS_Mdata
#define  TYPE_WAV0 "data"
#undef  FRONT_ICE0
#define  TYPE_ICE0 "data"
#undef  FRONT_LND0
#define  TYPE_LND0 "data"

#undef  FRONT_ABG0
#define  TYPE_ABG0 "data"
#define FRONT_OBG0 COAMPS_Mdata
#define  TYPE_OBG0 "data"
#define FRONT_WBG0 COAMPS_Mdata
#define  TYPE_WBG0 "data"


!-------------------------------------------------------------------------------
! User included component modules
!-------------------------------------------------------------------------------
#ifdef  FRONT_ATM1
#ifndef  TYPE_ATM1
#error "CPP Macro TYPE_ATM1 is required when FRONT_ATM1 is defined"
#endif
#else
#define  TYPE_ATM1 "none"
#endif
#ifdef  FRONT_ATM2
#ifndef  TYPE_ATM2
#error "CPP Macro TYPE_ATM2 is required when FRONT_ATM2 is defined"
#endif
#else
#define  TYPE_ATM2 "none"
#endif

#ifdef  FRONT_OCN1
#ifndef  TYPE_OCN1
#error "CPP Macro TYPE_OCN1 is required when FRONT_OCN1 is defined"
#endif
#else
#define  TYPE_OCN1 "none"
#endif
#ifdef  FRONT_OCN2
#ifndef  TYPE_OCN2
#error "CPP Macro TYPE_OCN2 is required when FRONT_OCN2 is defined"
#endif
#else
#define  TYPE_OCN2 "none"
#endif

#ifdef  FRONT_WAV1
#ifndef  TYPE_WAV1
#error "CPP Macro TYPE_WAV1 is required when FRONT_WAV1 is defined"
#endif
#else
#define  TYPE_WAV1 "none"
#endif
#ifdef  FRONT_WAV2
#ifndef  TYPE_WAV2
#error "CPP Macro TYPE_WAV2 is required when FRONT_WAV2 is defined"
#endif
#else
#define  TYPE_WAV2 "none"
#endif

#ifdef  FRONT_ICE1
#ifndef  TYPE_ICE1
#error "CPP Macro TYPE_ICE1 is required when FRONT_ICE1 is defined"
#endif
#else
#define  TYPE_ICE1 "none"
#endif
#ifdef  FRONT_ICE2
#ifndef  TYPE_ICE2
#error "CPP Macro TYPE_ICE2 is required when FRONT_ICE2 is defined"
#endif
#else
#define  TYPE_ICE2 "none"
#endif

#ifdef  FRONT_LND1
#ifndef  TYPE_LND1
#error "CPP Macro TYPE_LND1 is required when FRONT_LND1 is defined"
#endif
#else
#define  TYPE_LND1 "none"
#endif
#ifdef  FRONT_LND2
#ifndef  TYPE_LND2
#error "CPP Macro TYPE_LND2 is required when FRONT_LND2 is defined"
#endif
#else
#define  TYPE_LND2 "none"
#endif


module FRONT_DRM

  use ESMF
  use NUOPC
  use NUOPC_Driver, only: &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_SetRunSequence   => label_SetRunSequence, &
    driver_label_Finalize         => label_Finalize, &
    NUOPC_DriverAddComp, NUOPC_DriverGetComp, &
    NUOPC_DriverNewRunSequence, NUOPC_DriverAddRunElement
  use COAMPS_Futil, only: missingValue

  use  FRONT_CON , only: cplSS  => SetServices
#ifdef FRONT_MED
  use  FRONT_MED , only: medSS  => SetServices
#else
  use  FRONT_DUMB, only: medSS  => SetServices
#endif

  ! ATM component modules
#ifdef FRONT_ATM0
  use  FRONT_ATM0, only: atm0SS => SetServices
#else
  use  FRONT_DUMB, only: atm0SS => SetServices
#endif
#ifdef FRONT_ATM1
  use  FRONT_ATM1, only: atm1SS => SetServices
#else
  use  FRONT_DUMB, only: atm1SS => SetServices
#endif
#ifdef FRONT_ATM2
  use  FRONT_ATM2, only: atm2SS => SetServices
#else
  use  FRONT_DUMB, only: atm2SS => SetServices
#endif

  ! OCN component modules
#ifdef FRONT_OCN0
  use  FRONT_OCN0, only: ocn0SS => SetServices
#else
  use  FRONT_DUMB, only: ocn0SS => SetServices
#endif
#ifdef FRONT_OCN1
  use  FRONT_OCN1, only: ocn1SS => SetServices
#else
  use  FRONT_DUMB, only: ocn1SS => SetServices
#endif
#ifdef FRONT_OCN2
  use  FRONT_OCN2, only: ocn2SS => SetServices
#else
  use  FRONT_DUMB, only: ocn2SS => SetServices
#endif

  ! WAV component modules
#ifdef FRONT_WAV0
  use  FRONT_WAV0, only: wav0SS => SetServices
#else
  use  FRONT_DUMB, only: wav0SS => SetServices
#endif
#ifdef FRONT_WAV1
  use  FRONT_WAV1, only: wav1SS => SetServices
#else
  use  FRONT_DUMB, only: wav1SS => SetServices
#endif
#ifdef FRONT_WAV2
  use  FRONT_WAV2, only: wav2SS => SetServices
#else
  use  FRONT_DUMB, only: wav2SS => SetServices
#endif

  ! ICE component modules
#ifdef FRONT_ICE0
  use  FRONT_ICE0, only: ice0SS => SetServices
#else
  use  FRONT_DUMB, only: ice0SS => SetServices
#endif
#ifdef FRONT_ICE1
  use  FRONT_ICE1, only: ice1SS => SetServices
#else
  use  FRONT_DUMB, only: ice1SS => SetServices
#endif
#ifdef FRONT_ICE2
  use  FRONT_ICE2, only: ice2SS => SetServices
#else
  use  FRONT_DUMB, only: ice2SS => SetServices
#endif

  ! LND component modules
#ifdef FRONT_LND0
  use  FRONT_LND0, only: lnd0SS => SetServices
#else
  use  FRONT_DUMB, only: lnd0SS => SetServices
#endif
#ifdef FRONT_LND1
  use  FRONT_LND1, only: lnd1SS => SetServices
#else
  use  FRONT_DUMB, only: lnd1SS => SetServices
#endif
#ifdef FRONT_LND2
  use  FRONT_LND2, only: lnd2SS => SetServices
#else
  use  FRONT_DUMB, only: lnd2SS => SetServices
#endif

#ifdef FRONT_ABG0
  use  FRONT_ABG0, only: abg0SS => SetServices
#else
  use  FRONT_DUMB, only: abg0SS => SetServices
#endif
#ifdef FRONT_OBG0
  use  FRONT_OBG0, only: obg0SS => SetServices
#else
  use  FRONT_DUMB, only: obg0SS => SetServices
#endif
#ifdef FRONT_WBG0
  use  FRONT_WBG0, only: wbg0SS => SetServices
#else
  use  FRONT_DUMB, only: wbg0SS => SetServices
#endif

  implicit none
  save
  private

  public SetServices

  integer     , parameter :: maxModCount = 9
  logical     , parameter :: defaultVerbose = .false.
  logical     , parameter :: defaultModActive = .false.
  character(6), parameter :: defaultConType = 'bilinr'
  character(*), parameter :: label_InternalState = 'InternalState'
  integer     , parameter :: run_prep_phase = 2
  integer     , parameter :: run_post_phase = 3

  type type_PL
    integer, pointer :: p(:)
  end type

  type type_InternalStateStruct
    logical      :: verbose
    integer      :: modCount=0
    integer      :: modFgdCount=0
    integer      :: med=0, atm=0, ocn=0, wav=0, ice=0, lnd=0
    integer      :: abg=0, obg=0, wbg=0
    character(3) :: modName(0:maxModCount)
    character(8) :: modType(0:2,0:maxModCount)
    integer      :: modTndx(0:maxModCount)
    logical      :: modActive(0:maxModCount)
    type(type_PL):: modPetList(0:maxModCount)
    character(10):: conName(0:maxModCount,0:maxModCount)
    character(6) :: conType(0:maxModCount,0:maxModCount)
    logical      :: conActive(0:maxModCount,0:maxModCount)
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: modFgdCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    integer      ,pointer              :: abg, obg, wbg
    character(3) ,pointer              :: modName(:)
    character(8) ,pointer              :: modType(:,:)
    integer      ,pointer              :: modTndx(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    character(6) ,pointer              :: conType(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    logical                            :: configIsPresent
    integer                            :: i, j, k

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! config is required
    call ESMF_GridCompGet(driver, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (.not.configIsPresent) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config')
      return ! bail out
    endif

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      CONTEXT, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    modFgdCount => is%wrap%modFgdCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    abg => is%wrap%abg
    obg => is%wrap%obg
    wbg => is%wrap%wbg
    modName => is%wrap%modName
    modType => is%wrap%modType
    modTndx => is%wrap%modTndx
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conType => is%wrap%conType
    conActive => is%wrap%conActive

    ! initialize
    modCount = 0
    modFgdCount = 0
    med = 0
    atm = 0
    ocn = 0
    wav = 0
    ice = 0
    lnd = 0
    abg = 0
    obg = 0
    wbg = 0
    modActive = .false.
    conActive = .false.
    modType(:,:) = 'none'
    modTndx(:) = 0

    ! *** report compiled modules, set model count, model index mapping, and model names ***

    ! CON component
#ifdef FRONT_CON
    call ESMF_LogWrite(trim(cname)//': compiled with    CON  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without CON  module', ESMF_LOGMSG_INFO)
#endif

    ! MED component
#if defined(FRONT_MED)
    modCount = modCount + 1
    med = modCount
    modName(med) = 'MED'
    call ESMF_LogWrite(trim(cname)//': compiled with    MED  module', ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without MED  module', ESMF_LOGMSG_INFO)
#endif

    ! ATM component
#if defined(FRONT_ATM0) || defined(FRONT_ATM1) || defined(FRONT_ATM2)
    modCount       = modCount + 1
    atm            = modCount
    modName(atm)   = 'ATM'
    modType(0,atm) = trim(TYPE_ATM0)
    modType(1,atm) = trim(TYPE_ATM1)
    modType(2,atm) = trim(TYPE_ATM2)
#endif
#ifdef FRONT_ATM0
    call ESMF_LogWrite(trim(cname)//': compiled with    ATM0 module'// &
      ', model type: '//trim(TYPE_ATM0), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ATM0 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_ATM1
    call ESMF_LogWrite(trim(cname)//': compiled with    ATM1 module'// &
      ', model type: '//trim(TYPE_ATM1), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ATM1 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_ATM2
    call ESMF_LogWrite(trim(cname)//': compiled with    ATM2 module'// &
      ', model type: '//trim(TYPE_ATM2), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ATM2 module', ESMF_LOGMSG_INFO)
#endif

    ! OCN component
#if defined(FRONT_OCN0) || defined(FRONT_OCN1) || defined(FRONT_OCN2)
    modCount       = modCount + 1
    ocn            = modCount
    modName(ocn)   = 'OCN'
    modType(0,ocn) = trim(TYPE_OCN0)
    modType(1,ocn) = trim(TYPE_OCN1)
    modType(2,ocn) = trim(TYPE_OCN2)
#endif
#ifdef FRONT_OCN0
    call ESMF_LogWrite(trim(cname)//': compiled with    OCN0 module'// &
      ', model type: '//trim(TYPE_OCN0), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without OCN0 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_OCN1
    call ESMF_LogWrite(trim(cname)//': compiled with    OCN1 module'// &
      ', model type: '//trim(TYPE_OCN1), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without OCN1 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_OCN2
    call ESMF_LogWrite(trim(cname)//': compiled with    OCN2 module'// &
      ', model type: '//trim(TYPE_OCN2), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without OCN2 module', ESMF_LOGMSG_INFO)
#endif

    ! WAV component
#if defined(FRONT_WAV0) || defined(FRONT_WAV1) || defined(FRONT_WAV2)
    modCount       = modCount + 1
    wav            = modCount
    modName(wav)   = 'WAV'
    modType(0,wav) = trim(TYPE_WAV0)
    modType(1,wav) = trim(TYPE_WAV1)
    modType(2,wav) = trim(TYPE_WAV2)
#endif
#ifdef FRONT_WAV0
    call ESMF_LogWrite(trim(cname)//': compiled with    WAV0 module'// &
      ', model type: '//trim(TYPE_WAV0), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without WAV0 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_WAV1
    call ESMF_LogWrite(trim(cname)//': compiled with    WAV1 module'// &
      ', model type: '//trim(TYPE_WAV1), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without WAV1 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_WAV2
    call ESMF_LogWrite(trim(cname)//': compiled with    WAV2 module'// &
      ', model type: '//trim(TYPE_WAV2), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without WAV2 module', ESMF_LOGMSG_INFO)
#endif

    ! ICE component
#if defined(FRONT_ICE0) || defined(FRONT_ICE1) || defined(FRONT_ICE2)
    modCount       = modCount + 1
    ice            = modCount
    modName(ice)   = 'ICE'
    modType(0,ice) = trim(TYPE_ICE0)
    modType(1,ice) = trim(TYPE_ICE1)
    modType(2,ice) = trim(TYPE_ICE2)
#endif
#ifdef FRONT_ICE0
    call ESMF_LogWrite(trim(cname)//': compiled with    ICE0 module'// &
      ', model type: '//trim(TYPE_ICE0), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ICE0 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_ICE1
    call ESMF_LogWrite(trim(cname)//': compiled with    ICE1 module'// &
      ', model type: '//trim(TYPE_ICE1), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ICE1 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_ICE2
    call ESMF_LogWrite(trim(cname)//': compiled with    ICE2 module'// &
      ', model type: '//trim(TYPE_ICE2), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ICE2 module', ESMF_LOGMSG_INFO)
#endif

    ! LND component
#if defined(FRONT_LND0) || defined(FRONT_LND1) || defined(FRONT_LND2)
    modCount       = modCount + 1
    lnd            = modCount
    modName(lnd)   = 'LND'
    modType(0,lnd) = trim(TYPE_LND0)
    modType(1,lnd) = trim(TYPE_LND1)
    modType(2,lnd) = trim(TYPE_LND2)
#endif
#ifdef FRONT_LND0
    call ESMF_LogWrite(trim(cname)//': compiled with    LND0 module'// &
      ', model type: '//trim(TYPE_LND0), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without LND0 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_LND1
    call ESMF_LogWrite(trim(cname)//': compiled with    LND1 module'// &
      ', model type: '//trim(TYPE_LND1), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without LND1 module', ESMF_LOGMSG_INFO)
#endif
#ifdef FRONT_LND2
    call ESMF_LogWrite(trim(cname)//': compiled with    LND2 module'// &
      ', model type: '//trim(TYPE_LND2), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without LND2 module', ESMF_LOGMSG_INFO)
#endif

    ! set number of foreground components
    modFgdCount = modCount

    ! ABG (atmosphere background) component
#if defined(FRONT_ABG0)
    modCount       = modCount + 1
    abg            = modCount
    modName(abg)   = 'ABG'
    modType(0,abg) = trim(TYPE_ABG0)
    call ESMF_LogWrite(trim(cname)//': compiled with    ABG0 module'// &
      ', model type: '//trim(TYPE_ABG0), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without ABG0 module', ESMF_LOGMSG_INFO)
#endif

    ! OBG (ocean background) component
#if defined(FRONT_OBG0)
    modCount       = modCount + 1
    obg            = modCount
    modName(obg)   = 'OBG'
    modType(0,obg) = trim(TYPE_OBG0)
    call ESMF_LogWrite(trim(cname)//': compiled with    OBG0 module'// &
      ', model type: '//trim(TYPE_OBG0), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without OBG0 module', ESMF_LOGMSG_INFO)
#endif

    ! WBG (wave background) component
#if defined(FRONT_WBG0)
    modCount       = modCount + 1
    wbg            = modCount
    modName(wbg)   = 'WBG'
    modType(0,wbg) = trim(TYPE_WBG0)
    call ESMF_LogWrite(trim(cname)//': compiled with    WBG0 module'// &
      ', model type: '//trim(TYPE_WBG0), ESMF_LOGMSG_INFO)
#else
    call ESMF_LogWrite(trim(cname)//': compiled without WBG0 module', ESMF_LOGMSG_INFO)
#endif

    ! report model indexing
    do i = 1,modCount
      write(msgString,'(a,i0)') trim(cname)//': '//modName(i)//' model index: ',i
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set entry points for prep- and post-run methods
    call ESMF_GridCompSetEntryPoint(driver, ESMF_METHOD_RUN, &
      userRoutine=RunPrep, phase=run_prep_phase, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridCompSetEntryPoint(driver, ESMF_METHOD_RUN, &
      userRoutine=RunPost, phase=run_post_phase, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: modFgdCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    integer      ,pointer              :: abg, obg, wbg
    character(3) ,pointer              :: modName(:)
    character(8) ,pointer              :: modType(:,:)
    integer      ,pointer              :: modTndx(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    character(6) ,pointer              :: conType(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k
    character(ESMF_MAXSTR)             :: verbosity
    character(ESMF_MAXSTR)             :: label
    type(ESMF_GridComp)                :: modComp(maxModCount)
    type(ESMF_CplComp)                 :: conComp(maxModCount,maxModCount)
    integer(ESMF_KIND_I4)              :: time(6)
    type(ESMF_Time)                    :: startTime
    type(ESMF_TimeInterval)            :: runDuration
    type(ESMF_TimeInterval)            :: timeStep
    type(ESMF_TimeInterval)            :: zeroTimeInterval
    type(ESMF_Clock)                   :: internalClock
    character(8)                       :: modTypeString
    integer, parameter                 :: numAttr = 5
    character(ESMF_MAXSTR)             :: attrList(numAttr)
    character(ESMF_MAXSTR)             :: attrAtmBackground
    character(ESMF_MAXSTR)             :: attrOcnBackground
    character(ESMF_MAXSTR)             :: attrWavBackground

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    modFgdCount => is%wrap%modFgdCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    abg => is%wrap%abg
    obg => is%wrap%obg
    wbg => is%wrap%wbg
    modName => is%wrap%modName
    modType => is%wrap%modType
    modTndx => is%wrap%modTndx
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conType => is%wrap%conType
    conActive => is%wrap%conActive

    ! process config for verbose
    label = 'verbose:'
    call ESMF_ConfigGetAttribute(config, verbose, default=defaultVerbose, &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (verbose) then
      verbosity = 'high'
    else
      verbosity = 'low'
    endif

    ! process config for modActive
    do i = 1,modCount
      label = modName(i)//'_active:'
      call ESMF_ConfigGetAttribute(config, modActive(i), default=defaultModActive, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! report active/inactive models
      if (modActive(i)) then
        write(msgString,'(a)') trim(cname)//': '//modName(i)//' is     active'
      else
        write(msgString,'(a)') trim(cname)//': '//modName(i)//' is not active'
      endif
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! process config to set modTndx for foreground models
    ! when more than one model type is available, MOD_type is required
    do i = 1,modFgdCount
      if (.not.modActive(i)) cycle
      if (all(modType(:,i).eq.'none')) cycle
      label = modName(i)//'_type:'
      call ESMF_ConfigGetAttribute(config, modTypeString, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      do j = lbound(modType,dim=1),ubound(modType,dim=1)
        if (trim(modTypeString).eq.trim(modType(j,i))) then
          modTndx(i) = j
          exit
        endif
      enddo
      if (j.gt.ubound(modType,dim=1)) then
        write(msgString,'(a)') trim(cname)//': '//modName(i)// &
        ': Model type not supported: '//trim(modTypeString)
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
        return ! bail out
      endif
      write(msgString,'(a)') trim(cname)//': '//modName(i)// &
      ' type: '//trim(modType(modTndx(i),i))
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! set connector names (use same as NUOPC)
    do j = 1,modCount
    do i = 1,modCount
      conName(i,j) = modName(i)//'-TO-'//modName(j)
    enddo
    enddo

    ! set active connectors
    if (modActive(med)) then
      ! mediator is active
      ! * active model to mediator connections
      do i = med+1,modCount
        conActive(i,med) = modActive(i)
      enddo
      ! * mediator to active foreground model connections
      do i = med+1,modFgdCount
        conActive(med,i) = modActive(i)
      enddo
      ! * no model to model connections (except WAV-TO-OCN)
      conActive(wav,ocn) = modActive(wav).and.modActive(ocn)
    else
      ! mediator is not active
      ! * active foreground model to active foreground model connections
      do j = med+1,modFgdCount
      do i = med+1,modFgdCount
        if (i.eq.j) cycle
        conActive(i,j) = modActive(i).and.modActive(j)
        conActive(j,i) = modActive(j).and.modActive(i)
      enddo
      enddo
      ! * active background model to active foreground model connections
      do j = med+1,modFgdCount
      do i = modFgdCount+1,modCount
        if (i.eq.j) cycle
        if (i.eq.obg.and.j.eq.ocn) cycle
        if (i.eq.wbg.and.j.eq.wav) cycle
        conActive(i,j) = modActive(i).and.modActive(j)
      enddo
      enddo
    endif

    ! report active connections
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      write(msgString,'(a)') trim(cname)//': '//conName(i,j)//' connector is active'
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    enddo

    ! process config for conType
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      select case (conName(i,j))
      case default
      label = conName(i,j)//'_type:'
      call ESMF_ConfigGetAttribute(config, conType(i,j), default=defaultConType, &
        label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        call ESMF_LogWrite(trim(cname)//': ESMF_ConfigGetAttribute: '// &
          trim(label), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      endselect
      select case (conType(i,j))
      case ('memcpy','redist','bilinr','bicubc')
      case default
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': Connector type not supported: '//conType(i,j))
        return ! bail out
      endselect
      write(msgString,'(a)') trim(cname)//': '//conName(i,j)//' type: '//conType(i,j)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo
    enddo

    ! define COAMPS component level attributes
    attrList(1) = 'MissingValue'
    attrList(2) = 'AtmBackground' ! 'none', 'model', or 'mediator'
    if (modActive(abg)) then
      if (modActive(med)) then
        attrAtmBackground = 'mediator'
      else
        attrAtmBackground = 'model'
      endif
    else
      attrAtmBackground = 'none'
    endif
    attrList(3) = 'OcnBackground' ! 'none', 'model', or 'mediator'
    if (modActive(obg)) then
      if (modActive(med)) then
        attrOcnBackground = 'mediator'
      else
        attrOcnBackground = 'model'
      endif
    else
      attrOcnBackground = 'none'
    endif
    attrList(4) = 'WavBackground' ! 'none', 'model', or 'mediator'
    if (modActive(wbg)) then
      if (modActive(med)) then
        attrWavBackground = 'mediator'
      else
        attrWavBackground = 'model'
      endif
    else
      attrWavBackground = 'none'
    endif
    attrList(5) = 'ConnectorType' ! 'memcpy', 'redist', 'bilinr', or 'bicubc'

    ! process config for pet lists
    call SetModelPetLists(driver, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! SetServices for active models
    ! The model setServices function reference must be compile-time valid
    ! so that CPP macros are not required here.
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      select case (modName(i))
      case ('MED')
        call NUOPC_DriverAddComp(driver, modName(i), medSS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      case ('ATM')
        select case (modTndx(i))
        case (0)
          call NUOPC_DriverAddComp(driver, modName(i), atm0SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (1)
          call NUOPC_DriverAddComp(driver, modName(i), atm1SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (2)
          call NUOPC_DriverAddComp(driver, modName(i), atm2SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        endselect
      case ('OCN')
        select case (modTndx(i))
        case (0)
          call NUOPC_DriverAddComp(driver, modName(i), ocn0SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (1)
          call NUOPC_DriverAddComp(driver, modName(i), ocn1SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (2)
          call NUOPC_DriverAddComp(driver, modName(i), ocn2SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        endselect
      case ('WAV')
        select case (modTndx(i))
        case (0)
          call NUOPC_DriverAddComp(driver, modName(i), wav0SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (1)
          call NUOPC_DriverAddComp(driver, modName(i), wav1SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (2)
          call NUOPC_DriverAddComp(driver, modName(i), wav2SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        endselect
      case ('ICE')
        select case (modTndx(i))
        case (0)
          call NUOPC_DriverAddComp(driver, modName(i), ice0SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (1)
          call NUOPC_DriverAddComp(driver, modName(i), ice1SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (2)
          call NUOPC_DriverAddComp(driver, modName(i), ice2SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        endselect
      case ('LND')
        select case (modTndx(i))
        case (0)
          call NUOPC_DriverAddComp(driver, modName(i), lnd0SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (1)
          call NUOPC_DriverAddComp(driver, modName(i), lnd1SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        case (2)
          call NUOPC_DriverAddComp(driver, modName(i), lnd2SS, &
            petList=modPetList(i)%p, comp=modComp(i), rc=rc)
        endselect
      case ('ABG')
        call NUOPC_DriverAddComp(driver, modName(i), abg0SS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      case ('OBG')
        call NUOPC_DriverAddComp(driver, modName(i), obg0SS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      case ('WBG')
        call NUOPC_DriverAddComp(driver, modName(i), wbg0SS, &
          petList=modPetList(i)%p, comp=modComp(i), rc=rc)
      endselect
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'NUOPC_DriverAddComp: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo

    ! set component attributes for active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      ! set config
      call ESMF_GridCompSet(modComp(i), config=config, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'Set config: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! set verbosity
      call NUOPC_CompAttributeSet(modComp(i), name='Verbosity', value=trim(verbosity), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'NUOPC_CompAttributeSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! add COAMPS Attribute package
      call ESMF_AttributeAdd(modComp(i), convention='COAMPS', purpose='General', &
        attrList=attrList, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeAdd: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! set COAMPS Attributes
      call ESMF_AttributeSet(modComp(i), name='AtmBackground', &
        value=trim(attrAtmBackground), convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_AttributeSet(modComp(i), name='OcnBackground', &
        value=trim(attrOcnBackground), convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_AttributeSet(modComp(i), name='WavBackground', &
        value=trim(attrWavBackground), convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_AttributeSet(modComp(i), name='MissingValue', &
        value=missingValue, convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'ESMF_AttributeSet: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo

    ! SetServices for active connectors
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      call NUOPC_DriverAddComp(driver, modName(i), modName(j), cplSS, comp=conComp(i,j), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'NUOPC_DriverAddComp: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo
    enddo

    ! set connector component attributes for active connectors
    do j = 1,modCount
    do i = 1,modCount
      if (.not.conActive(i,j)) cycle
      ! set config
      call ESMF_CplCompSet(conComp(i,j), config=config, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'Set Config: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! set verbosity
      call NUOPC_CompAttributeSet(conComp(i,j), name='Verbosity', value=trim(verbosity), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'NUOPC_CompAttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! add COAMPS Attribute package
      call ESMF_AttributeAdd(conComp(i,j), convention='COAMPS', purpose='General', &
        attrList=attrList, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeAdd: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      ! set COAMPS Attributes
      call ESMF_AttributeSet(conComp(i,j), name='AtmBackground', &
        value=trim(attrAtmBackground), convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_AttributeSet(conComp(i,j), name='OcnBackground', &
        value=trim(attrOcnBackground), convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_AttributeSet(conComp(i,j), name='WavBackground', &
        value=trim(attrWavBackground), convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_AttributeSet(conComp(i,j), name='MissingValue', &
        value=missingValue, convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_AttributeSet(conComp(i,j), name='ConnectorType', &
        value=conType(i,j), convention='COAMPS', purpose='General', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        write(msgString,'(a,2i2,a)') 'ESMF_AttributeSet: ',i,j,', '//conName(i,j)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo
    enddo

    ! process config for required timeStep input
    label = 'time_step:'
    call ESMF_ConfigGetAttribute(config, time(4:6), count=3, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' hh mm ss')
      return ! bail out
    endif
    write(msgString,'(a,3(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=4,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeIntervalSet(timeStep, h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! process config for required startTime input
    label = 'start_time:'
    call ESMF_ConfigGetAttribute(config, time, count=6, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' YYYY MM DD hh mm ss')
      return ! bail out
    endif
    write(msgString,'(a,6(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=1,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeSet(startTime, yy=time(1), mm=time(2), dd=time(3), &
      h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! process config for required runDuration input
    label = 'run_duration:'
    call ESMF_ConfigGetAttribute(config, time(4:6), count=3, label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': missing required config input: '// &
        trim(label)//' hh mm ss')
      return ! bail out
    endif
    write(msgString,'(a,3(a,i0))') trim(cname)//': '//trim(label),(' ',time(k),k=4,6)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimeIntervalSet(runDuration, h=time(4), m=time(5), s=time(6), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! check that simulation time is multiple of timeStep
    call ESMF_TimeIntervalSet(zeroTimeInterval, h=0, m=0, s=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (mod(runDuration,timeStep) .ne. zeroTimeInterval) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
        msg=trim(cname)//': run duration is not a multiple of timeStep')
      return ! bail out
    endif

#ifdef ADJOINT
    ! compiled as adjoint, so reinterpret time inputs
    startTime   = startTime + runDuration
    runDuration = -runDuration
    timeStep    = -timeStep
#endif

    ! create/set the driver clock
    internalClock = ESMF_ClockCreate(name=trim(cname)//'_clock', &
      timeStep=timeStep, startTime=startTime, runDuration=runDuration, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelPetLists(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: modFgdCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    integer      ,pointer              :: abg, obg, wbg
    character(3) ,pointer              :: modName(:)
    character(8) ,pointer              :: modType(:,:)
    integer      ,pointer              :: modTndx(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    character(6) ,pointer              :: conType(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k
    integer                            :: l, m, n, p
    integer                            :: k1, k2
    integer                            :: modStart
    integer                            :: petCount, npet
    integer                            :: modPetCount(maxModCount)
    character(ESMF_MAXSTR)             :: label
    character(ESMF_MAXSTR)             :: petLayoutOption
    character(ESMF_MAXSTR)             :: mbgPetAssignment
    logical                            :: isPresent
    integer     , allocatable          :: list(:), ncol(:)

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    modFgdCount => is%wrap%modFgdCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    abg => is%wrap%abg
    obg => is%wrap%obg
    wbg => is%wrap%wbg
    modName => is%wrap%modName
    modType => is%wrap%modType
    modTndx => is%wrap%modTndx
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conType => is%wrap%conType
    conActive => is%wrap%conActive

    ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! process config for pet_layout_option:
    label = 'pet_layout_option:'
    call ESMF_ConfigGetAttribute(config, petLayoutOption, default='sequential', &
      label=trim(label), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_LogWrite(trim(cname)//': '//trim(label)//' '//trim(petLayoutOption), &
    ESMF_LOGMSG_INFO)

    ! set the model petLists based on petLayoutOption
    select case (trim(petLayoutOption))

    ! pet_layout_option: sequential
    !   * active models defined on pet_count pets
    !   * MED defined on pet_count pets
    !   * requires pet_count input
    case ('sequential')
      label='pet_count:'
      call ESMF_ConfigGetAttribute(config, npet, label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
          ' = sequential')
        return ! bail out
      endif
      if (npet.lt.1.or.npet.gt.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': '//trim(label)//' must be > 0 and <= # PETs')
        return ! bail out
      endif
      do i = 1,modCount
        if (.not.modActive(i)) cycle
        allocate(modPetList(i)%p(npet), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list array failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        do j = 1,npet
          modPetList(i)%p(j) = j-1
        enddo
      enddo

    ! pet_layout_option: concurrent
    !   * active models defined on non-overlapping sets of PETs
    !   * requires <MOD>_pet_count input for active models
    !   * MED_pet_count optional, default is MED defined on all PETs
    !   * requires \sum(<MOD>_pet_count) <= petCount
    case ('concurrent')
      modStart = 1
      if (modActive(med)) then
        label=modName(med)//'_pet_count:'
        call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
        if (.not.isPresent.or.rc.ne.ESMF_SUCCESS) then
          modPetCount(med) = petCount
          if (verbose) then
            write(msgString,'(a,i0)') trim(cname)//': '// &
              modName(med)//' PET count: ',modPetCount(med)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          endif
          allocate(modPetList(med)%p(petCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg='Allocation of '//modName(med)//' PET list array failed.', &
            CONTEXT, rcToReturn=rc)) return ! bail out
          do j = 1,petCount
            modPetList(med)%p(j) = j-1
          enddo
          modStart = med+1
        endif
      endif
      npet = 0
      do i = modStart,modFgdCount
        if (.not.modActive(i)) cycle
        label=modName(i)//'_pet_count:'
        call ESMF_ConfigGetAttribute(config, modPetCount(i), label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
            ' = concurrent and '//modName(i)//' is active')
          return ! bail out
        endif
        if (modPetCount(i).lt.1.or.modPetCount(i).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' must be > 0 and < # PETs')
          return ! bail out
        endif
        npet = npet + modPetCount(i)
      enddo
      if (npet.gt.petCount) then
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
          msg=trim(cname)//': pet_layout_option = concurrent requires'// &
          ' \sum(<MOD>_pet_count) <= # PETs for active models')
        return ! bail out
      endif
      npet = 0
      do i = modStart,modFgdCount
        if (.not.modActive(i)) cycle
        allocate(modPetList(i)%p(modPetCount(i)), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list array failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        do j = 1,modPetCount(i)
          modPetList(i)%p(j) = npet
          npet = npet + 1
        enddo
        if (verbose) then
          write(msgString,'(a,i0)') trim(cname)//': '// &
            modName(i)//' PET count: ',modPetCount(i)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          n = 10
          m = ceiling(real(modPetCount(i))/real(n))
          write(msgString,'(a)') trim(cname)//': '//modName(i)//' PET list:'
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          do l = 1,m
            k1 = min((l-1)*n+1,modPetCount(i))
            k2 = min((l-1)*n+n,modPetCount(i))
            write(msgString,'(a,100i7)') trim(cname)//': ', (modPetList(i)%p(k),k=k1,k2)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          enddo
        endif
      enddo

    ! pet_layout_option: specified
    !   * active models defined on specified sets of PETs
    !   * requires <MOD>_pet_list input for active models
    !   * MED_pet_list optional, default is MED defined on all PETs
    !   * requires min(<MOD>_pet_list) >= 0 && max(<MOD>_pet_list) < petCount
    case ('specified')
      modStart = 1
      if (modActive(med)) then
        label=modName(med)//'_pet_list::'
        call ESMF_ConfigFindLabel(config, label=trim(label), isPresent=isPresent, rc=rc)
        if (.not.isPresent.or.rc.ne.ESMF_SUCCESS) then
          modPetCount(med) = petCount
          if (verbose) then
            write(msgString,'(a,i0)') trim(cname)//': '// &
              modName(med)//' PET count: ',modPetCount(med)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          endif
          allocate(modPetList(med)%p(petCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg='Allocation of '//modName(med)//' PET list array failed.', &
            CONTEXT, rcToReturn=rc)) return ! bail out
          do j = 1,petCount
            modPetList(i)%p(j) = j-1
          enddo
          modStart = med+1
        endif
      endif
      do i = modStart,modFgdCount
        if (.not.modActive(i)) cycle
        label=modName(i)//'_pet_list::'
        call ESMF_ConfigGetDim(config, m, n, label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
            ' = specified and '//modName(i)//' is active')
          return ! bail out
        endif
        if (verbose) then
          write(msgString,'(a,i0)') trim(cname)//': '// &
            trim(label)//' table number of rows: ',m
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          write(msgString,'(a,i0)') trim(cname)//': '// &
            trim(label)//' table max number of columns: ',n
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
        endif
        allocate(list(n), ncol(m), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list table failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        do p = 1,2
          if (p.eq.2) then
            if (verbose) then
              write(msgString,'(a,i0)') trim(cname)//': '// &
                modName(i)//' PET count: ',modPetCount(i)
              call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
            endif
            if (modPetCount(i).lt.1.or.modPetCount(i).gt.petCount) then
              call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
                msg=trim(cname)//': '//modName(i)// &
                ' PET count must be > 0 and <= # PETs')
              return ! bail out
            endif
            allocate(modPetList(i)%p(modPetCount(i)), stat=stat)
            if (ESMF_LogFoundAllocError(statusToCheck=stat, &
              msg='Allocation of '//modName(i)//' PET list array failed.', &
              CONTEXT, rcToReturn=rc)) return ! bail out
          endif
          call ESMF_ConfigFindLabel(config, trim(label), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) then
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
              msg=trim(cname)//': '//trim(label)//' is required when pet_layout_option'// &
              ' = specified and '//modName(i)//' is active')
            return ! bail out
          endif
          modPetCount(i) = 0
          do l=1,m
            call ESMF_ConfigNextLine(config, rc=rc)
            if (ESMF_LogFoundError(rc, PASSTHRU)) then
              write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                ' next line ',l,' failed'
              call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
              return ! bail out
            endif
            if (p.eq.1) then
              ncol(l) = ESMF_ConfigGetLen(config, rc=rc)
              if (ESMF_LogFoundError(rc, PASSTHRU)) then
                write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                  ' get length ',l,' failed'
                call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
                return ! bail out
              endif
              modPetCount(i) = modPetCount(i) + ncol(l)
            else
              call ESMF_ConfigGetAttribute(config, list(1:ncol(l)), rc=rc)
              if (ESMF_LogFoundError(rc, PASSTHRU)) then
                write(msgString,'(a,i0,a)') trim(cname)//': '//trim(label)// &
                  ' get row ',l,' failed'
                call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, msg=trim(msgString))
                return ! bail out
              endif
              modPetCount(i) = modPetCount(i) + ncol(l)
              modPetList(i)%p(modPetCount(i)-ncol(l)+1:modPetCount(i)) = list(1:ncol(l))
            endif
          enddo
        enddo
        deallocate(list, ncol, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg='Deallocation of '//modName(i)//' PET list table failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        if (verbose) then
          n = 10
          m = ceiling(real(modPetCount(i))/real(n))
          write(msgString,'(a)') trim(cname)//': '//modName(i)//' PET list:'
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          do l = 1,m
            k1 = min((l-1)*n+1,modPetCount(i))
            k2 = min((l-1)*n+n,modPetCount(i))
            write(msgString,'(a,100i7)') trim(cname)//': ', (modPetList(i)%p(k),k=k1,k2)
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
          enddo
        endif
        if (minval(modPetList(i)%p).lt.0.or.maxval(modPetList(i)%p).ge.petCount) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//modName(i)//' PET list ids must be > 0 and < # PETs')
          return ! bail out
        endif
        do j = 1,modPetCount(i)
          if (count(modPetList(i)%p.eq.modPetList(i)%p(j)).gt.1) then
            call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
              msg=trim(cname)//': '//modName(i)//' PET list has duplicate entries')
            return ! bail out
          endif
        enddo
      enddo

    ! unsupported pet_layout_option:
    case default
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
       msg=trim(cname)//': pet_layout_option not supported: '//trim(petLayoutOption))
      return ! bail out
    endselect

    ! pet lists for background components
    if (trim(petLayoutOption).ne.'sequential') then
      do i = modFgdCount+1,modCount
        if (.not.modActive(i)) cycle
        label=modName(i)//'_pet_assignment:'
        call ESMF_ConfigGetAttribute(config, mbgPetAssignment, label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) then
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
            msg=trim(cname)//': '//trim(label)//' is required when '//modName(i)//' is active')
          return ! bail out
        endif
        select case (trim(mbgPetAssignment))
        case ('MED')
          j = med
        case ('ATM')
          j = atm
        case ('OCN')
          j = ocn
        case ('WAV')
          j = wav
        case ('ICE')
          j = ice
        case ('LND')
          j = lnd
        case default
          call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
           msg=trim(cname)//': '//trim(label)//' not supported: '//trim(mbgPetAssignment))
          return ! bail out
        endselect
        allocate(modPetList(i)%p(modPetCount(j)), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg='Allocation of '//modName(i)//' PET list array failed.', &
          CONTEXT, rcToReturn=rc)) return ! bail out
        modPetList(i)%p(:) = modPetList(j)%p(:)
      enddo
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: modFgdCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    integer      ,pointer              :: abg, obg, wbg
    character(3) ,pointer              :: modName(:)
    character(8) ,pointer              :: modType(:,:)
    integer      ,pointer              :: modTndx(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    character(6) ,pointer              :: conType(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    modFgdCount => is%wrap%modFgdCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    abg => is%wrap%abg
    obg => is%wrap%obg
    wbg => is%wrap%wbg
    modName => is%wrap%modName
    modType => is%wrap%modType
    modTndx => is%wrap%modTndx
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conType => is%wrap%conType
    conActive => is%wrap%conActive

    ! override the default run sequence defined by the generic Driver
    call NUOPC_DriverNewRunSequence(driver, slotCount=1, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (modActive(med)) then
      ! *** run sequence with mediator ***
      ! 1: connect active models to mediator
      do i = med+1,modCount
        if (.not.conActive(i,med)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), modName(med), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
      ! 2: advance mediator
      call NUOPC_DriverAddRunElement(driver, 1, modName(med), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      ! 3: connect mediator to active models
      do j = med+1,modCount
        if (.not.conActive(med,j)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(med), modName(j), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
      ! 4: connect active WAV to active OCN
      if (conActive(wav,ocn)) then
        call NUOPC_DriverAddRunElement(driver, 1, modName(wav), modName(ocn), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      ! 5: advance active models
      do i = med+1,modCount
        if (.not.modActive(i)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
    else
      ! *** run sequence without mediator ***
      ! 1: connect active background models to active foreground models
      do j = med+1,modFgdCount
      do i = modFgdCount+1,modCount
        if (i.eq.j) cycle
        if (.not.conActive(i,j)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), modName(j), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
      enddo
      ! 2: connect active foreground models to active foreground models
      do j = med+1,modFgdCount
      do i = med+1,modFgdCount
        if (i.eq.j) cycle
        if (.not.conActive(i,j)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), modName(j), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
      enddo
      ! 3: advance active models
      do i = med+1,modCount
        if (.not.modActive(i)) cycle
        call NUOPC_DriverAddRunElement(driver, 1, modName(i), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      enddo
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RunPrep(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    character(3) ,pointer              :: modName(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k
    type(ESMF_GridComp)                :: modComp(maxModCount)
    type(ESMF_Clock)                   :: internalClock
    type(ESMF_Time)                    :: startTime
    integer(ESMF_KIND_I8)              :: zero = 0

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered RunPrep', ESMF_LOGMSG_INFO)

    ! reset internal clock
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ClockGet(internalClock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    call ESMF_ClockSet(internalClock, currTime=startTime, advanceCount=zero, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! invoke RunPrep for active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      call NUOPC_DriverGetComp(driver, modName(i), comp=modComp(i), rc=rc)
      if (ESMF_LogFoundError( rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'DriverGetComp: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_GridCompRun(modComp(i), phase=run_prep_phase, userRc=lrc, rc=rc)
      if (ESMF_LogFoundError( rc, PASSTHRU) .or. &
          ESMF_LogFoundError(lrc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'RunPrep: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving RunPrep', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RunPost(driver, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical      ,pointer              :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    character(3) ,pointer              :: modName(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k
    type(ESMF_GridComp)                :: modComp(maxModCount)
    type(ESMF_Clock)                   :: internalClock

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query the component for its config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose => is%wrap%verbose
    modCount => is%wrap%modCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    modName => is%wrap%modName
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conActive => is%wrap%conActive

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered RunPost', ESMF_LOGMSG_INFO)

    ! invoke RunPost for active models
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      call NUOPC_DriverGetComp(driver, modName(i), comp=modComp(i), rc=rc)
      if (ESMF_LogFoundError( rc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'DriverGetComp: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
      call ESMF_GridCompRun(modComp(i), phase=run_post_phase, userRc=lrc, rc=rc)
      if (ESMF_LogFoundError( rc, PASSTHRU) .or. &
          ESMF_LogFoundError(lrc, PASSTHRU)) then
        write(msgString,'(a,1i2,a)') 'RunPost: ',i,', '//modName(i)
        call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
        return ! bail out
      endif
    enddo

1   if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving RunPost', ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Finalize(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)             :: cname
    type(ESMF_Config)                  :: config
    type(type_InternalState)           :: is
    logical                            :: verbose
    integer      ,pointer              :: modCount
    integer      ,pointer              :: modFgdCount
    integer      ,pointer              :: med, atm, ocn, wav, ice, lnd
    integer      ,pointer              :: abg, obg, wbg
    character(3) ,pointer              :: modName(:)
    character(8) ,pointer              :: modType(:,:)
    integer      ,pointer              :: modTndx(:)
    logical      ,pointer              :: modActive(:)
    type(type_PL),pointer              :: modPetList(:)
    character(10),pointer              :: conName(:,:)
    character(6) ,pointer              :: conType(:,:)
    logical      ,pointer              :: conActive(:,:)
    character(ESMF_MAXSTR)             :: msgString
    integer                            :: lrc, stat
    integer                            :: i, j, k

    rc = ESMF_SUCCESS

    ! query the component for its name
    call ESMF_GridCompGet(driver, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! query component for internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out

    ! set local pointers for internal state members
    verbose = is%wrap%verbose
    modCount => is%wrap%modCount
    modFgdCount => is%wrap%modFgdCount
    med => is%wrap%med
    atm => is%wrap%atm
    ocn => is%wrap%ocn
    wav => is%wrap%wav
    ice => is%wrap%ice
    lnd => is%wrap%lnd
    abg => is%wrap%abg
    obg => is%wrap%obg
    wbg => is%wrap%wbg
    modName => is%wrap%modName
    modType => is%wrap%modType
    modTndx => is%wrap%modTndx
    modActive => is%wrap%modActive
    modPetList => is%wrap%modPetList
    conName => is%wrap%conName
    conType => is%wrap%conType
    conActive => is%wrap%conActive

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': entered Finalize', ESMF_LOGMSG_INFO)

    ! clean up internal state
    do i = 1,modCount
      if (.not.modActive(i)) cycle
      deallocate(modPetList(i)%p, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg='Deallocation of '//modName(i)//' PET list array failed.', &
        CONTEXT, rcToReturn=rc)) return ! bail out
    enddo

    if (verbose) &
    call ESMF_LogWrite(trim(cname)//': leaving Finalize', ESMF_LOGMSG_INFO)

  end subroutine

end module
