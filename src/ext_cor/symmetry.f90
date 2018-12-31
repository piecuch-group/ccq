module symmetry

    ! Module for point-group symmetry considerations

    implicit none
    integer :: mo_sym(1:200)
    !integer, dimension(8,8,4) :: char_table
    integer :: char_table(8,8)
    integer :: group_dim
    integer :: target_sym = 1
    character(len=5) :: pg_sym_name

contains


    subroutine read_sym(filename, orbs)

        ! Read symmetry file and choose character tables
        ! In:
        !   filename: file containing molecular orbital symmetries

        use, intrinsic :: iso_fortran_env, only: error_unit
        integer, intent(in) :: orbs
        character(len=*), intent(in) :: filename

        integer :: i
        integer :: i_sym_char
        integer :: sym_irrep
        integer :: char_tables(8,8,4)
        integer :: group_dims(4)

        logical :: t_exists
        !character(len=3) :: sym_irrep_str

        inquire(file=filename, exist=t_exists)
        if (.not. t_exists) then
            write(error_unit, '(3a)') 'Symmetry file, ', trim(filename),', does not exist.'
            call exit(1)
        endif

        char_tables = reshape(&
             (/1,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,&
             0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,&
             0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,&
             0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,&

             1,1,0,0,0,0,0,0, 1,-1,0,0,0,0,0,0,&
             0,0,0,0,0,0,0,0, 0, 0,0,0,0,0,0,0,&
             0,0,0,0,0,0,0,0, 0, 0,0,0,0,0,0,0,&
             0,0,0,0,0,0,0,0, 0, 0,0,0,0,0,0,0,&

             1, 1,1, 1,0,0,0,0, 1, 1,-1,-1,0,0,0,0,&
             1,-1,1,-1,0,0,0,0, 1,-1,-1, 1,0,0,0,0,&
             0, 0,0, 0,0,0,0,0, 0, 0, 0, 0,0,0,0,0,&
             0, 0,0, 0,0,0,0,0, 0, 0, 0, 0,0,0,0,0,&

             1, 1, 1, 1, 1, 1, 1, 1,   1, 1,-1,-1, 1, 1,-1,-1,&
             1,-1, 1,-1, 1,-1, 1,-1,   1,-1,-1, 1, 1,-1,-1, 1,&
             1, 1, 1, 1,-1,-1,-1,-1,   1, 1,-1,-1,-1,-1, 1, 1,&
             1,-1, 1,-1,-1, 1,-1, 1,   1,-1,-1, 1,-1, 1, 1,-1/), &
             (/8,8,4/))

        group_dims = (/1, 2, 4, 8/)


        open(unit=105,file=filename,status="old")

        read(105, *) i_sym_char

        select case (i_sym_char)
        case (1)
            pg_sym_name = "C1"

        case (2)
            pg_sym_name = "CS"

        case (3)
            pg_sym_name = "C2V"

        case (4)
            pg_sym_name = "D2H"

        case default
            write(error_unit, '(a)') 'Point-group symmetry not supported'
            call exit(1)
        end select

        group_dim = group_dims(i_sym_char)
        char_table = char_tables(:,:,i_sym_char)

        do i=1, orbs
            read(105, *) sym_irrep
            mo_sym(i) = sym_irrep
        enddo

        close(105)

    end subroutine read_sym

    function is_sym(ex_orbs, norbs)
        integer, intent(in) :: ex_orbs(8)
        integer, intent(in) :: norbs
        integer :: tmp_sym(8)
        integer :: test_ag = 1
        integer :: i, i_sym
        logical :: is_sym

        tmp_sym = 1
        do i_sym=1, group_dim
            do i=1, norbs
                tmp_sym(i_sym) = tmp_sym(i_sym) * char_table(mo_sym(ex_orbs(i)),i_sym)
            enddo
        enddo

        is_sym = .true.
        !print *, 'Debug PG', ex_orbs(1:norbs)
        do i_sym=1, group_dim
            !print '(2i4)', tmp_sym(i_sym), char_table(target_sym,i_sym)
            if (tmp_sym(i_sym) /= char_table(target_sym,i_sym)) then
                is_sym = .false.
                exit
            endif
        enddo
        !print *, is_sym

    end function is_sym

end module symmetry
