! $Id$
!
! Reproducer code FieldBundleRedistStore_Slow

!-------------------------------------------------------------------------
!ESMF_MULTI_PROC_SYSTEM_TEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! ESMF Reproducer FieldBundleRedistStore_Slow.
!
!\begin{verbatim}

    program esmTest
#define ESMF_METHOD "program esmTest"

    ! ESMF Framework module
    use ESMF

    use user_model1, only : userm1_register
    use user_model2, only : userm2_register
    use user_coupler, only : usercpl_register

    implicit none

    ! Local variables
    integer :: pet_id, npets, rc, localrc, userrc
    integer :: srcPetCnt, dstPetCnt
    integer, allocatable :: petList1(:)
    integer, allocatable :: petList2(:)
    integer, allocatable :: petList3(:)
    integer :: i
    character(len=ESMF_MAXSTR) :: cname1, cname2, cplname
    type(ESMF_VM):: vm
    type(ESMF_State) :: c1exp, c2imp
    type(ESMF_GridComp) :: comp1, comp2
    type(ESMF_CplComp) :: cpl

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message, and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

    ! Initialize return code
    rc = ESMF_SUCCESS


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "-------------------------------------------------------- "
    print *, "Start of System Test FieldBundleRedistStore_Slow:"
    print *, "-------------------------------------------------------- "

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize framework and get back default global VM
    call ESMF_Initialize(vm=vm, defaultlogfilename="ESMF_Test.Log", &
                        logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPet=pet_id, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Check for correct number of PETs
    if ( npets < 4 ) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD,&
        msg="This system test does not run on fewer than 4 PETs.",&
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      call ESMF_Finalize(rc=rc, endflag=ESMF_END_ABORT)
    endif

    dstPetCnt = 2 + (npets/50) + (npets/100)
    srcPetCnt = npets - dstPetCnt

    ! Create the 2 model components and coupler
    cname1 = "user model 1"
    allocate(petList1(srcPetCnt))
    petList1 = (/(i, i=0, (srcPetCnt-1))/)
    comp1 = ESMF_GridCompCreate(name=cname1, petList=petList1, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "Created component ", trim(cname1), "rc =", rc
    !  call ESMF_GridCompPrint(comp1, "", rc)
    deallocate(petList1)

    cname2 = "user model 2"
    allocate(petList2(dstPetCnt))
    petList2 = (/(i, i=srcPetCnt, (npets-1))/)
    comp2 = ESMF_GridCompCreate(name=cname2, petList=petList2, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "Created component ", trim(cname2), "rc =", rc
    !  call ESMF_GridCompPrint(comp2, "", rc)
    deallocate(petList2)

    cplname = "user one-way coupler"
    allocate(petList3(npets))
    petList3 = (/(i, i=0, (npets-1))/)
    cpl = ESMF_CplCompCreate(name=cplname, petList=petList3, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "Created component ", trim(cplname), ", rc =", rc
    !  call ESMF_CplCompPrint(cpl, "", rc)
    deallocate(petList3)

    print *, "Comp Creates finished"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    call ESMF_GridCompSetServices(comp1, userRoutine=userm1_register, &
      userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "Comp SetServices finished, rc= ", rc, userrc

    call ESMF_GridCompSetServices(comp2, userRoutine=userm2_register, &
      userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "Comp SetServices finished, rc= ", rc, userrc

    call ESMF_CplCompSetServices(cpl, userRoutine=usercpl_register, &
      userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    print *, "Comp SetServices finished, rc= ", rc, userrc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                            name="Gregorian", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! initialize start time to 5/01/2003
    call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! initialize stop time to 5/02/2003
    call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=1, h=6, &
                      calendar=gregorianCalendar, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! initialize the clock with the above values
    clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
                             name="Clock 1", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    c1exp = ESMF_StateCreate(name="comp1 export",  &
                             stateintent=ESMF_STATEINTENT_EXPORT, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Comp 1 Initialize finished, rc =", rc
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    c2imp = ESMF_StateCreate(name="comp2 import",  &
                             stateintent=ESMF_STATEINTENT_IMPORT, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompInitialize(comp2, importState=c2imp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Comp 2 Initialize finished, rc =", rc
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! note that the coupler's import is comp1's export
    call ESMF_CplCompInitialize(cpl, importState=c1exp, &
      exportState=c2imp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Coupler Initialize finished, rc =", rc
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    do while (.not. ESMF_ClockIsStopTime(clock, rc=rc))

      call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, &
        userRc=userrc, rc=localrc)
      print *, "Comp 1 Run returned, rc =", rc
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_CplCompRun(cpl, importState=c1exp, &
        exportState=c2imp, clock=clock, &
        userRc=userrc, rc=localrc)
      print *, "Coupler Run returned, rc =", rc
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_GridCompRun(comp2, importState=c2imp, clock=clock, &
        userRc=userrc, rc=localrc)
      print *, "Comp 2 Run returned, rc =", rc
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_ClockAdvance(clock, rc=localrc)
      !call ESMF_ClockPrint(clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

    enddo

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

    call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Comp 1 Finalize finished, rc =", rc, userrc
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompFinalize(comp2, importState=c2imp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Comp 2 Finalize finished, rc =", rc, userrc
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_CplCompFinalize(cpl, importState=c1exp, &
      exportState=c2imp, clock=clock, &
      userRc=userrc, rc=localrc)
    print *, "Coupler Finalize finished, rc =", rc, userrc
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=userrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)


    print *, "------------------------------------------------------------"
    print *, "------------------------------------------------------------"
    print *, "Test finished, pet_id = ", pet_id
    print *, "------------------------------------------------------------"
    print *, "------------------------------------------------------------"

    print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

    call ESMF_StateDestroy(c1exp, rc=rc)
    call ESMF_StateDestroy(c2imp, rc=rc)

    call ESMF_ClockDestroy(clock, rc=rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)

    call ESMF_GridCompDestroy(comp1, rc=rc)
    call ESMF_GridCompDestroy(comp2, rc=rc)
    call ESMF_CplCompDestroy(cpl, rc=rc)

    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10  print *, "System Test FieldBundleRedistStore_Slow complete."


    ! Normal ESMF Test output
    write(failMsg, *) "System Test failure"
    write(testname, *) "System Test FieldBundleRedistStore_Slow: LocStream based FieldBundleRedist"

    if (rc .ne. ESMF_SUCCESS) then
      ! Separate message to console, for quick confirmation of success/failure
      if (rc .eq. ESMF_SUCCESS) then
        write(finalMsg, *) "SUCCESS: LocStream based FieldBundleRedist test finished correctly."
      else
        write(finalMsg, *) "System Test did not succeed.  Error code ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif

    call ESMF_Finalize(rc=rc)

    end program esmTest

!\end{verbatim}

