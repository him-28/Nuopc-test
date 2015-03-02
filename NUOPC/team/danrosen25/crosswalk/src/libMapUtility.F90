
module snmHeader
    integer, parameter :: SNM_SUCCESS = 0
    integer, parameter :: SNM_ERROR_NOFILE = 1
    integer, parameter :: SNM_ERROR_NOCSDMS = 2
    integer, parameter :: SNM_ERROR_NOCF = 3
    integer, parameter :: SNM_ERROR_STRLEN = 4
    integer, parameter :: SNM_MAXSTR = 128
end module

    subroutine loadMapping(file,rc)
        use snmHeader
        implicit none
        ! Dummy Variables
        character(*),intent(in) :: file
        integer,intent(out),optional :: rc
        ! Local Variables
        character(SNM_MAXSTR) :: readline
        logical :: fexists

        if(present(rc)) rc = SNM_SUCCESS

        if(file .eq. "") then
            return
        end if

        inquire(file=file, exist=fexists)
        if(.not. fexists) then
            if(present(rc)) rc = SNM_ERROR_NOFILE
            return
        end if

        open(15,file=file)
        read(15,*) readline
        close(15)

    end subroutine

    subroutine csdmsToCF(from,to,rc)
        use snmHeader
        implicit none
        ! Dummy Variables
        character(*),intent(in) :: from
        character(*),intent(out) :: to
        integer,intent(out),optional :: rc
        ! Local Variables
        character(SNM_MAXSTR) :: lto = "out_standard_name"

        if(present(rc)) rc = SNM_SUCCESS
        to = lto

    end subroutine

    subroutine cfToCsdms(from,to,rc)
        use snmHeader
        implicit none
        ! Dummy Variables
        character(*),intent(in) :: from
        character(*),intent(out) :: to
        integer,intent(out),optional :: rc
        ! Local Variables
        character(SNM_MAXSTR) :: lto = "out_standard_name"

        if(present(rc)) rc = SNM_SUCCESS
        to = lto

    end subroutine

    subroutine printError(errorCode)
        use snmHeader
        implicit none
        ! Dummy Variables
        integer, intent(in) :: errorCode

        select case(errorCode)
            case(SNM_SUCCESS)
                print *,"SUCCESS: No Error"
            case(SNM_ERROR_NOFILE)
                print *,"ERROR: File Not Found"
            case(SNM_ERROR_NOCSDMS)
                print *,"ERROR: No CSDMS standard name entry"
            case(SNM_ERROR_NOCF)
                print *,"ERROR: No CF standard name entry"
            case(SNM_ERROR_STRLEN)
                print *,"ERROR: String length out of bounds"
            case default
                print *,"ERROR: Unknown"
        end select

    end subroutine
