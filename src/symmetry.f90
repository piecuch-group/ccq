module symmetry

    ! Module for point-group symmetry considerations

    implicit none

    integer :: mo_sym(1:200)
    integer :: char_table(8,8)
    integer :: group_dim
    integer :: target_sym = 1
    character(len=5) :: pg_sym_name

contains

    subroutine read_sym(filename, orbs)

        ! Read symmetry file and choose character tables

        ! In:
        !   filename: file containing molecular orbital symmetries

        use const, only: sym_unit
        use errors, only: stop_all

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
            call stop_all('read_sym', 'Symmetry file, '//trim(filename)//', does not exist.')
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


        open(sym_unit,file=filename,status="old")

        read(sym_unit, *) i_sym_char

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
            call stop_all('read_sym', 'Point-group symmetry not supported')

        end select

        group_dim = group_dims(i_sym_char)
        char_table = char_tables(:,:,i_sym_char)

        do i=1, orbs
            read(sym_unit, *) sym_irrep
            mo_sym(i) = sym_irrep
        enddo

        close(sym_unit)

    end subroutine read_sym

    function is_sym(ex_orbs, norbs)

        ! Check whether the existation is fully symmetric

        ! In:
        !   ex_orbs: orbitals involved in the excitation
        !   norbs: number of orbitals in the excitation.
        !          norbs/2 is the excitation rank

        ! Out:
        !   is_sym: logical value. If true, the excitation
        !           is fully symmetric.

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
        do i_sym=1, group_dim
            if (tmp_sym(i_sym) /= char_table(target_sym,i_sym)) then
                is_sym = .false.
                exit
            endif
        enddo

    end function is_sym

end module symmetry