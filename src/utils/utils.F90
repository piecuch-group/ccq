! Module borrowed and adapted from HANDE
! https://github.com/hande-qmc/hande

module utils

! Various utilities and tools...

implicit none

interface int_fmt
    !module procedure int_fmt_int_32
    module procedure int_fmt_int_64
end interface int_fmt
interface tri_ind
    !module procedure tri_ind_int_32
    module procedure tri_ind_int_64
end interface tri_ind


contains

! --- Combinatorics ---

    elemental function binom_i(m, n) result(binom)

        ! ACM Algorithm 160 translated to Fortran.
        ! Returns the binomial coefficient ^mC_n: the number of
        ! combinations of m things taken n at a time.

        integer :: binom
        integer, intent(in) :: m, n
        integer :: p,i, n1

        n1 = n
        p = m - n1
        if (n1 < p) then
            p = n1
            n1 = m - p
        end if
        binom = n1 + 1
        if (p == 0) binom = 1
        do i = 2, p
            binom = (binom*(n1+i))/i
        end do

    end function binom_i

    elemental function binom_r(m, n) result(binom)

        ! ACM Algorithm 160 translated to Fortran.
        ! Returns the binomial coefficient ^mC_n: the number of
        ! combinations of m things taken n at a time.

        use const, only: dp

        real(dp) :: binom
        integer, intent(in) :: m, n
        integer :: p,i, n1

        n1 = n
        p = m - n1
        if (n1 < p) then
            p = n1
            n1 = m - p
        end if
        binom = n1 + 1
        if (p == 0) binom = 1
        do i = 2, p
            binom = (binom*(n1+i))/i
        end do

    end function binom_r

    elemental function factorial(n) result(fac)

        ! In:
        !   n: integer
        ! Returns:
        !   factorial of n, n!

        ! WARNING:
        ! This does *not* safeguard against integer overflow.  As such, it is
        ! only suitably for 0 <= n <= 12.  Investigating using log(n!) or the
        ! Gamma function if required for larger values.

        integer :: fac
        integer, intent(in) :: n

        integer :: i

        fac = 1
        do i = 2, n
            fac = fac*i
        end do

    end function factorial

    elemental function factorial_combination_1(m,n) result (combination)

        ! Given m and n input, this function returns
        ! combination = n!m!/(n+m+1)!
        ! Required to calculate amplitudes of the Neel singlet
        ! trial function for the Heisenberg model

        use const, only: p

        real(p) :: combination
        integer, intent(in) :: m, n
        integer :: m1, n1, i

        ! Choose m1 to be the larger of m and n
        if (m >= n) then
            m1 = m
            n1 = n
        else
            m1 = n
            n1 = m
        end if
        combination = 1
        do i = 1, n1
            combination = combination * i/(i+m1)
        end do
        combination = combination/(m1+n1+1)

    end function factorial_combination_1

    subroutine next_comb(nitems, inds, r, done)

        ! Generate the next combination of a selection of r items
        ! in the inds array. This subroutine was translated from
        ! python's itertools example.
        ! https://docs.python.org/3.7/library/itertools.html#itertools.combinations

        ! In:
        !    nitems: total number of items
        !    r: subset of items to choose
        ! In/Out:
        !    inds: index combination
        !    done: whether the last combination has been reached

        integer, intent(in) :: nitems
        integer, intent(inout) :: inds(:)
        integer, intent(in) :: r
        logical, intent(out) :: done

        integer :: i, j

        done = .false.

        do i=r, 1, -1
            if (inds(i) /= i + nitems - r) exit

            if (i == 1) then
                done = .true.
                return
            endif
        enddo

        inds(i) = inds(i) + 1
        do j=i+1, r
            inds(j) = inds(j-1) + 1
        enddo

    end subroutine next_comb

    subroutine combs(items, r, comb)

        integer, intent(in) :: items(:)
        integer, intent(in) :: r
        integer, allocatable, intent(out) :: comb(:,:)

        integer, allocatable :: inds(:)
        integer :: n
        integer :: i, j, k, indx
        integer :: n_combs

        n = size(items)

        n_combs = binom_i(n, r)

        allocate(comb(r, n_combs))

        allocate(inds(n))

        do i=1, r
            inds(i) = i
        enddo

        indx = 1
        do k=1, r
            comb(k,indx) = items(inds(k))
        enddo

        outer: do
            do i=r, 1, -1
                if (inds(i) /= i + n - r) exit
                if (i == 1) exit outer
            enddo
            inds(i) = inds(i) + 1
            do j=i+1, r
                inds(j) = inds(j-1) + 1
            enddo

            indx = indx + 1
            do k=1, r
                comb(k,indx) = items(inds(k))
            enddo
        enddo outer

        deallocate(inds)

    end subroutine combs

!--- format statement formatting ---

!    elemental function int_fmt_int_32(i, padding) result(fmt1)
!
!        ! In:
!        !    i: an integer
!        !    padding (optional): amount of padding to add to format statement.
!        !        Default: 2.
!        ! Returns:
!        !    fmt1: a format statement for an integer field which will hold
!        !        i perfectly plus an amount of padding.
!
!        ! This does take i/o formatting to a slightly OCD level addmittedly...
!
!        use const, only: dp
!
!        character(4) :: fmt1
!        integer, intent(in) :: i
!        integer, intent(in), optional :: padding
!        real(dp) :: logi
!
!        if (i == 0 .or. i==1) then
!            logi = 1.0
!        else
!            logi = log10(real(abs(i)+1,dp))
!        end if
!        if (i < 0) logi = logi + 1
!
!        fmt1 = int_fmt_helper(logi, padding)
!
!    end function int_fmt_int_32

    elemental function int_fmt_int_64(i, padding) result(fmt1)

        ! In:
        !    i: a long integer
        !    padding (optional): amount of padding to add to format statement.
        !        Default: 2.
        ! Returns:
        !    fmt1: a format statement for an integer field which will hold
        !        i perfectly plus an amount of padding.

        ! This does take i/o formatting to a slightly OCD level addmittedly...

        use const, only: int_64, dp

        character(4) :: fmt1
        integer(int_64), intent(in) :: i
        integer, intent(in), optional :: padding
        real(dp) :: logi

        if (i == 0 .or. i==1) then
            logi = 1.0
        else
            logi = log10(real(abs(i)+1,dp))
        end if
        if (i < 0) logi = logi + 1

        fmt1 = int_fmt_helper(logi, padding)

    end function int_fmt_int_64

    elemental function int_fmt_helper(logi, padding) result(fmt1)

        ! In:
        !    logi: log10 of an integer.
        !    padding (optional): amount of padding to add to format statement.
        !        The default amount is 2.  The padding is used to include the
        !        sign if i is negative.
        ! Returns:
        !    fmt1: a format statement for an real field using the G format
        !       statement which will hold i perfectly plus an amount of padding.

        use const, only: dp

        character(4) :: fmt1
        real(dp), intent(in) :: logi
        integer, intent(in), optional :: padding
        integer :: p

        if (present(padding)) then
            p = padding
        else
            p = 2
        end if

        p = ceiling(logi+p)

        if (p < 10) then
            write (fmt1,'("i",i1)') p
        else if (p < 100) then
            write (fmt1,'("i",i2)') p
        else
            ! By this point we'll have hit integer overflow (for 32- and 64-bit
            ! integers) anyway...
            write (fmt1,'("i",i3)') p
        end if

    end function int_fmt_helper

! --- File names and file handling ---

    function get_free_unit() result(free_unit)

        ! Returns:
        !    The first free file unit above 10 and less than or equal to
        !    the paramater max_unit (currently set to 200).

        use errors, only: stop_all

        integer, parameter :: max_unit = 100
        integer :: free_unit
        integer :: i
        logical :: t_open, t_exist

        do i = 10, max_unit
            inquire(unit=i, opened=t_open, exist=t_exist)
            if (.not.t_open .and. t_exist) then
                free_unit = i
                exit
            end if
        end do
        if (i == max_unit+1) call stop_all('get_free_unit','Cannot find a free unit below max_unit.')

    end function get_free_unit

    elemental subroutine append_ext(stem, n, s)

        ! Returns stem.n in s.

        character(*), intent(in) :: stem
        integer, intent(in) :: n
        character(*), intent(out) :: s
        character(10) :: ext

        write (ext,'('//int_fmt(n,0)//')') n
        s = stem//'.'//ext

    end subroutine append_ext

   subroutine get_unique_filename(stem, suffix, tnext, istart, filename, id, reduce)

        ! Find a filename which is either the "newest" or the next to be used.
        ! The filename is assumed to be stem.xsuffix, where x is an integer.

        ! In:
        !    stem: stem of the filename.
        !    suffix: suffix of filename
        !    tnext: the next unused filename is found if true, else the
        !        filename is set to be stem.x where stem.x exists and stem.x+1
        !        doesn't and x is greater than istart.
        !    istart: the integer of the first x value to check.
        !        If istart is negative, then the filename is set to be stem.x,
        !        where x = |istart+1|.  This overrides everything else.
        !    reduce (optional): Ensure that all processes get the same file id if
        !        running in parallel.  Defaults to true.
        ! Out:
        !    filename.
        !    id (optional): value of x used in the filename.

        character(*), intent(in) :: stem, suffix
        logical, intent(in) :: tnext
        integer, intent(in) :: istart
        character(*), intent(out) :: filename
        integer, optional, intent(out) :: id
        logical, optional, intent(in) :: reduce

        integer :: i
        logical :: exists

        if (istart >= 0) then

            i = istart
            exists = .true.
            do while (exists)
                call append_ext(stem, i, filename)
                if (present(id)) id = i
                filename = trim(filename)//suffix
                inquire(file=filename,exist=exists)
                i = i + 1
            end do

            if (.not.tnext) then
                ! actually want the last file which existed.
                ! this will return stem.istart if stem.istart doesn't exist.
                i = max(istart,i - 2)
                call append_ext(stem, i, filename)
                if (present(id)) id = i
                filename = trim(filename)//suffix
            end if

        else
            ! Have been asked for a specific file.
            call append_ext(stem, abs(istart+1), filename)
            if (present(id)) id = abs(istart+1)
            filename = trim(filename)//suffix
        end if

    end subroutine get_unique_filename

! --- Array indexing ---

    elemental function tri_ind_int_64(i,j) result(indx)

        ! Find the index corresponding to the (i,j)-th element of a lower
        ! triangular array.  This maps:
        !
        !   1,1                   1
        !   2,1 2,2               2  3
        !   3,1 3,2 3,3       to  4  5  6
        !   4,1 4,2 4,3 4,4       7  8  9 10
        !
        ! WARNING:
        ! We assume that i >= j.  It is the programmer's responsibility to check
        ! this and re-order i and j if required.
        !
        ! In:
        !    i: (1-indexed) row index
        !    j: (1-indexed) column index
        ! Returns:
        !    A combined (1-indexed) index for the corresponding element in
        !    a lower triangular array.

        use const, only: int_64
        integer(int_64) :: indx
        integer(int_64) :: tmp
        integer(int_64), intent(in) :: i, j

        !The unsigned integer is there to avoid overflow with >2^30 integrals.
        tmp=i
        tmp=i*(i-1)
        indx = tmp/2 + j

    end function tri_ind_int_64

    elemental function tri_ind_int_32(i,j) result(indx)

        ! Find the index corresponding to the (i,j)-th element of a lower
        ! triangular array.  This maps:
        !
        !   1,1                   1
        !   2,1 2,2               2  3
        !   3,1 3,2 3,3       to  4  5  6
        !   4,1 4,2 4,3 4,4       7  8  9 10
        !
        ! WARNING:
        ! We assume that i >= j.  It is the programmer's responsibility to check
        ! this and re-order i and j if required.
        !
        ! In:
        !    i: (1-indexed) row index
        !    j: (1-indexed) column index
        ! Returns:
        !    A combined (1-indexed) index for the corresponding element in
        !    a lower triangular array.

        use const, only: int_32, int_64
        integer(int_64) :: indx
        integer(int_64) :: tmp
        integer(int_32), intent(in) :: i, j

        !The 64-bit is there to avoid overflow with >2^30 integrals.
        tmp = i
        tmp = tmp * (i-1)
        indx = tmp/2 + j

    end function tri_ind_int_32

    elemental function tri_ind_reorder(i,j) result(indx)

        ! Find the index corresponding to the (i,j)-th element of a lower
        ! triangular array.
        !
        ! We assume that i >= j.  If this is not the case (i.e. (i,j) refers to
        ! an element in the upper triangular array) then the index of the
        ! transpose element (i.e. (j,i)) is returned.
        !
        ! This maps:
        !
        !   1,1 1,2 1,3 1,4        1  2  4  7
        !   2,1 2,2 2,3 2,4        2  3  5  8
        !   3,1 3,2 3,3 3,4   to   4  5  6  9
        !   4,1 4,2 4,3 4,4        7  8  9 10
        !
        ! In:
        !    i: (1-indexed) index
        !    j: (1-indexed) index
        ! Returns:
        !    A combined (1-indexed) index for the corresponding element in
        !    a lower triangular array.

        use const, only: int_64

        integer(int_64) :: indx
        integer, intent(in) :: i, j

        if (i>=j) then
            indx = tri_ind(i,j)
        else
            indx = tri_ind(j,i)
        end if

    end function tri_ind_reorder

! --- String conversion ---

    pure function fstring_to_carray(string_f03) result(array_c)

        ! Convert a Fortran string into a C string.  The result can be passed to
        ! C procedures which require a *char object, which corresponds to an
        ! array in Fortran.

        ! In:
        !    string_f03: Fortran character string.
        ! Returns:
        !    The equivalent (null-terminated) C string in a character array.

        use, intrinsic :: iso_c_binding, only: c_char, c_null_char

        character(len=*), intent(in) :: string_f03
        character(kind=c_char) :: array_c(len(string_f03)+1)

        integer :: i

        do i = 1, len(string_f03)
            array_c(i) = string_f03(i:i)
        end do
        array_c(i) = c_null_char

    end function fstring_to_carray

    pure function carray_to_fstring(array_c) result(string_f03)

        ! Convert a C string into a Fortran string.  The input can be from a
        ! C procedure which uses a *char object (which corresponds to an array
        ! in Fortran) and the output is the equivalent standard Fortran string.

        ! In:
        !    array_c: null-terminated C string in a character array.
        ! Returns:
        !    The equivalent Fortran character string (without null termination).

        use, intrinsic :: iso_c_binding, only: c_char, c_null_char

        character(kind=c_char), intent(in) :: array_c(:)
        character(len=size(array_c)-1) :: string_f03

        integer :: i

        ! Don't copy any (presumably garbage) from beyond the null character.
        string_f03 = ''
        do i = 1, size(array_c)-1
            if (array_c(i) == c_null_char) exit
            string_f03(i:i) = array_c(i)
        end do

    end function carray_to_fstring

    function count_file_lines(file_path) result(line_count)

        ! Count the number of lines in a file.

        ! In:
        !    file_path: path to the file
        ! Returns:
        !    The number of lines in the file

        use const, only: tmp_unit

        integer :: line_count
        character(len=*), intent(in) :: file_path

        integer :: ios

        line_count = 0
        open(tmp_unit, file=trim(file_path), status="old")

        do
            read(tmp_unit, *, iostat=ios)
            if (ios /= 0) exit
            line_count = line_count + 1
        enddo

        close(tmp_unit)

    end function count_file_lines

!--- System output ---

    function get_walltime_sec() result(res)

        use const, only: int_64, dp

        real(dp) :: res
        integer(int_64) :: cnt, cnt_rate, cnt_max

        call system_clock(cnt, cnt_rate, cnt_max)

        res = real(cnt / cnt_rate, dp)

    end function get_walltime_sec

!--- Informative output ---

    subroutine print_matrix(matrix)

        ! Print out a given real matrix in a neat format.

        ! In:
        !    matrix: The matrix which is to be output to the screen.

        use const, only: p

        real(p), intent(in) :: matrix(:,:)
        integer :: i,j

        do i = 1, ubound(matrix,1)
            do j = 1, ubound(matrix,2)
                write(6, '(es17.10,2X)', advance = 'no') matrix(i,j)
            end do
            write(6,'(1X)', advance = 'yes')
        end do

    end subroutine print_matrix

end module utils
