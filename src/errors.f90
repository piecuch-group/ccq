module errors

    ! Module holding error handling routines

    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit

    implicit none

contains

    subroutine stop_all(var, msg)

        ! Abort everything and exit program with an
        ! error message

        ! In:
        !   var: location of the error
        !   msg: error message

        character(len=*), intent(in) :: var
        character(len=*), intent(in) :: msg

        write(error_unit, '(a)') trim(var//' '//msg)
        ! Exit program with error code 1
        call exit(1)

    end subroutine stop_all

end module errors