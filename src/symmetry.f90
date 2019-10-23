module symmetry

    ! Module for point-group symmetry calculations

    implicit none

    integer, private :: mo_sym(1:200)
    integer, private :: char_table(8,8)
    integer, private :: group_dim
    integer, private :: target_sym = 1
    character(len=5), private :: pg_sym_name

contains

    subroutine read_sym(filename, orb_syms, point_group, orbs)

        ! Read symmetry file and choose character tables

        ! In:
        !   filename: file containing molecular orbital symmetries

        use const, only: sym_unit
        use errors, only: stop_all

        character(len=*), intent(in) :: filename
        integer, allocatable, intent(in out) :: orb_syms(:)
        character(len=*), intent(in out) :: point_group
        integer, intent(in) :: orbs

        integer :: i
        integer :: i_sym_char
        integer :: sym_irrep
        integer :: char_tables(8,8,4)
        integer :: group_dims(4)

        logical :: t_exists
        !character(len=3) :: sym_irrep_str

        if (.not. allocated(orb_syms)) &
            allocate(orb_syms(200))

        inquire(file=filename, exist=t_exists)
        if (.not. t_exists) then
            !call stop_all('read_sym', 'ERROR: Symmetry file, '//trim(filename)//', does not exist.')

            pg_sym_name = 'C1'
            point_group = pg_sym_name

            do i=1, orbs
                mo_sym(i) = 1
            enddo
            orb_syms = mo_sym

            return


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
            call stop_all('read_sym', 'ERROR: Point-group symmetry not supported')

        end select

        point_group = pg_sym_name

        group_dim = group_dims(i_sym_char)
        char_table = char_tables(:,:,i_sym_char)

        do i=1, orbs
            read(sym_unit, *) sym_irrep
            mo_sym(i) = sym_irrep
        enddo
        orb_syms = mo_sym

        close(sym_unit)

    end subroutine read_sym


    ! [TODO] lousy way of writing a dictionary. Should change.
    function map_irrep(irrep, point_group) result(irrep_num)

        integer :: irrep_num
        character(len=*), intent(in) :: irrep
        character(len=*), intent(in) :: point_group

        select case(trim(point_group))

        case ('C1')
            select case(trim(irrep))
            case ('A')
                irrep_num = 1
            end select

        case ('CS')
            select case(trim(irrep))
            case ("A'")
                irrep_num = 1

            case ('A"')
                irrep_num = 2
            end select

        case ('C2V')
            select case(trim(irrep))
            case ('A1')
                irrep_num = 1

            case ('A2')
                irrep_num = 2

            case ('B1')
                irrep_num = 3

            case ('B2')
                irrep_num = 4

            end select

        case ('D2H')
            select case(trim(irrep))
            case ('Ag')
                irrep_num = 1

            case ('B1g')
                irrep_num = 2

            case ('B2g')
                irrep_num = 3

            case ('B3g')
                irrep_num = 4

            case ('Au')
                irrep_num = 5

            case ('B1u')
                irrep_num = 6

            case ('B2u')
                irrep_num = 7

            case ('B3u')
                irrep_num = 8


            end select

        end select


    end function map_irrep

    function reverse_map_irrep(irrep_num, point_group) result(irrep)

        character(len=5) :: irrep
        integer, intent(in) :: irrep_num
        character(len=*), intent(in) :: point_group

        select case(trim(point_group))

        case ('C1')
            select case(irrep_num)
            case (1)
                irrep = 'A'
            end select

        case ('CS')
            select case(irrep_num)
            case (1)
                irrep = "A'"

            case (2)
                irrep = 'A"'

            end select

        case ('C2V')
            select case(irrep_num)
            case (1)
                irrep = "A1"

            case (2)
                irrep = "A2"

            case (3)
                irrep = "B1"

            case (4)
                irrep = "B2"

            end select

        case ('D2H')
            select case(irrep_num)
            case (1)
                irrep = "Ag"

            case (2)
                irrep = "B1g"

            case (3)
                irrep = "B2g"

            case (4)
                irrep = "B3g"

            case (5)
                irrep = "Au"

            case (6)
                irrep = "B1u"

            case (7)
                irrep = "B2u"

            case (8)
                irrep = "B3u"

            end select

        end select


    end function reverse_map_irrep

    ! [TODO] optimize this
    function is_sym(ex_orbs, norbs)

        ! Check whether the excitation is fully symmetric

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
