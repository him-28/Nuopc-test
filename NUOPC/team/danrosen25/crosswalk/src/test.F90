program testMapUtility
    implicit none

    character(10) :: out
    integer :: rc
    integer :: errorCount, testCount

    errorCount = 0
    testCount = 0

    print *,"============"
    print *,"=Start Test="
    print *,"============"

    call loadMapping("config",rc)
    testCount = testCount +1
    if(rc .ne. 0) then
        call printError(rc)
        errorCount = errorCount + 1
    end if

    call csdmsToCF("",out,rc)
    testCount = testCount + 1
    if(rc .ne. 0) then
        call printError(rc)
        errorCount = errorCount + 1
    end if

    call cfToCSDMS("",out,rc)
    testCount = testCount +1
    if(rc .ne. 0) then
        call printError(rc)
        errorCount = errorCount + 1
    end if

    print *,"  RESULTS"
    print *,"    Failed: ",errorCount
    print *,"    Passed: ",(testCount-errorCount)

    if (errorCount .gt. 0) then
        print *,"  *** TEST FAILED ***"
    else
        print *,"  *** TEST PASSED ***"
    end if

    print *,"============"
    print *,"==End Test=="
    print *,"============"

    call exit(errorCount)

end program
