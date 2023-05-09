module checking

    ! Helper routines for testing whether an event was a success.

    use const, only: int_64
    implicit none

    interface check_allocate
        module procedure check_allocate_int_32
        module procedure check_allocate_int_64
    end interface
    contains

        subroutine check_allocate_int_32(array_name, array_size, ierr)

            ! Check whether an allocation was successful.

            ! In:
            !    array_name: name of array being allocated.
            !    array_size: number of elements being allocated.
            !    ierr: error code returned by the allocate statement.

            use utils, only: int_fmt
            use const, only: int_32
            use errors, only: stop_all

            character(*), intent(in) :: array_name
            integer(int_32), intent(in) :: array_size
            integer, intent(in) :: ierr

            if (array_size < 0 .or. ierr /= 0) then
                ! Error in allocating array.
                write (6,'(1X,a25,1X,a,1X,a9,1X,'//int_fmt(array_size,1)//')') &
                    'Error in allocating array',trim(array_name),'with size',array_size
                write (6,*) 'ierr',ierr
                call stop_all('check_allocate_int_32','Allocation error')
            end if

        end subroutine check_allocate_int_32

        subroutine check_allocate_int_64(array_name, array_size, ierr)

            ! Check whether an allocation was successful.

            ! In:
            !    array_name: name of array being allocated.
            !    array_size: number of elements being allocated.
            !    ierr: error code returned by the allocate statement.

            use utils, only: int_fmt
            use const, only: int_64
            use errors, only: stop_all

            character(*), intent(in) :: array_name
            integer(int_64), intent(in) :: array_size
            integer, intent(in) :: ierr

            if (array_size < 0 .or. ierr /= 0) then
                ! Error in allocating array.
                write (6,'(1X,a25,1X,a,1X,a9,1X,'//int_fmt(array_size,1)//')') &
                    'Error in allocating array',trim(array_name),'with size',array_size
                write (6,*) 'ierr',ierr
                call stop_all('check_allocate','Allocation error')
            end if

        end subroutine check_allocate_int_64

        subroutine check_deallocate(array_name, ierr)

            ! Check whether a deallocation was successful.

            ! In:
            !    array_name: name of array being deallocated.
            !    ierr: error code returned by the deallocate statement.

            use utils, only: int_fmt
            use errors, only: stop_all

            character(*), intent(in) :: array_name
            integer, intent(in) :: ierr

            if (ierr /= 0) then
                ! Error in deallocating array.
                write (6,'(1X,a25,1X,a,1X,a9)') 'Error in deallocating array',trim(array_name)
                call stop_all('check_deallocate','Deallocation error')
            end if

        end subroutine check_deallocate

end module checking
