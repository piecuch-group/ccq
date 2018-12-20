module process_ci

    ! Module for proccesing CI vectors

    use const, only: dp
    use sys_data
    use symmetry
    use printing, only: abort_cc
    !use printing, only: print_conf_occupation, print_conf_number, abort_cc

    implicit none

    ! We need two different ways of computing phase factors
    abstract interface
        function i_excitation_sign(excitation_rank, from, to) result(det_sign)
            integer, intent(in) :: excitation_rank
            integer, intent(in) :: from(4)
            integer, intent(in) :: to(4)
            integer :: det_sign
        end function i_excitation_sign

        function i_convert_spin(i, spin) result(i_spin)
            integer, intent(in) :: i
            integer, intent(in) :: spin
            integer :: i_spin
        end function i_convert_spin
    end interface

    type excit_t
        real(dp) :: coef
        integer :: rank_a
        integer :: rank_b
        integer :: from_a(4), to_a(4)
        integer :: from_b(4), to_b(4)
    end type excit_t

contains

    subroutine decode_determinant(qmc_coef, conf, excitation)
        integer, intent(in) :: qmc_coef
        integer, intent(in) :: conf(:)
        type(excit_t), intent(inout) :: excitation


        ! These scratch arrays to generate configurations on
        integer, allocatable :: pos_a(:), pos_b(:)
        logical :: is_hf = .true.
        ! alpha and beta index
        integer :: i, idx
        integer :: inda, indb

        ! Set initial alpha configuration
        is_hf = .true.
        do i=1, nele
            if (conf(i) /= i) then
                is_hf = .false.
                exit
            endif
        enddo

        if (is_hf) then
            excitation%coef = real(qmc_coef,kind=8)
            excitation%rank_a = 0
            excitation%rank_b = 0
        else
            allocate(pos_a(occ_a), pos_b(occ_b))

            ! Load orbitals to symmetry testing array
            ! [TODO] this should be implemented better
            inda = 1
            indb = 1
            do i=1, nele
                if (mod(conf(i), 2) == 1) then
                    pos_a(inda) = (conf(i) / 2) + 1
                    inda = inda + 1
                else
                    pos_b(indb) = conf(i) / 2
                    indb = indb + 1
                endif
            enddo

            !if (config_debug) then
            !call print_conf_occupation(155, pos_a, pos_b, real(line(2), &
            !kind=8))
            !endif

            call spin_integrate('q', pos_a, pos_b, real(qmc_coef,kind=8), excitation)
            deallocate(pos_a, pos_b)
        endif

    end subroutine decode_determinant

    subroutine find_fciqmc_c3(filename, config_debug, &
            c1_a, c1_b, c2_aa, c2_ab, c2_bb, &
            c3_aaa, c3_aab, c3_abb, c3_bbb, coef_norm)

        ! Translate configurations to excitations for FCIQMC coefficients
        !
        ! In:
        !   filename: file containing FCIQMC walkers per determinant
        !   config_debug: if true, write configurations in occupation form
        ! In/Out:
        !   c*: arrays containing the spin-integrated coefficients based on excitation rank

        use const, only: dp

        ! [TODO] make this a type and pass it nicely
        real(kind=8), allocatable, intent(inout) :: c1_a(:,:)
        real(kind=8), allocatable, intent(inout) :: c1_b(:,:)

        real(kind=8), allocatable, intent(inout) :: c2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c2_bb(:,:,:,:)

        real(kind=8), allocatable, intent(inout) :: c3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c3_bbb(:,:,:,:,:,:)

        logical, intent(in) :: config_debug

        ! Renormalization number
        real(dp), intent(inout) :: coef_norm

        ! Aux variable
        type(excit_t) :: excitation
        character(len=*), intent(in) :: filename
        integer :: excit_rank
        integer :: ios
        integer :: line(2)
        integer :: conf(nele)
        logical :: is_hf = .true.



        ! Run over all alpha combinations
        !if (config_debug) then
        !    open(unit=155, file='occ_configs.log', status='unknown')
        !endif
        coef_norm = 0.0d0

        open(unit=105, file=trim(filename), status='old')

        do
            read(105, *, iostat=ios) line, conf
            if (ios /= 0) exit

            call decode_determinant(line(2), conf, excitation)

            associate(excit_rank_a=>excitation%rank_a, coef=>excitation%coef, &
                    to_a=>excitation%to_a, to_b=>excitation%to_b, &
                    from_a=>excitation%from_a, from_b=>excitation%from_b)

                excit_rank = excit_rank_a + excitation%rank_b

                ! convert_spin_ptr options: 1 -> alpha from, 2 -> beta from, 3 -> alpha to, 4 -> beta to
                select case (excit_rank)
                case (0)
                    coef_norm = coef

                case (1)
                    if (excit_rank_a == 1) then
                        c1_a(to_a(1), from_a(1)) = coef

                    else
                        c1_b(to_b(1), from_b(1)) = coef
                    endif

                case (2)
                    if (excit_rank_a == 2) then
                        c2_aa(to_a(1), to_a(2), from_a(1), from_a(2)) = coef
                    elseif (excit_rank_a == 1) then
                        c2_ab(to_a(1), to_b(1), from_a(1), from_b(1)) = coef
                    else
                        c2_bb(to_b(1), to_b(2), from_b(1), from_b(2)) = coef
                    endif

                case(3)
                    if (excit_rank_a == 3) then
                        c3_aaa(to_a(1), to_a(2), to_a(3), from_a(1), from_a(2), from_a(3)) = &
                            coef
                    elseif (excit_rank_a == 2) then
                        c3_aab(to_a(1), to_a(2), to_b(1), from_a(1), from_a(2), from_b(1)) = &
                            coef
                    elseif (excit_rank_a == 1) then
                        c3_abb(to_a(1), to_b(1), to_b(2), from_a(1), from_b(1), from_b(2)) = &
                            coef
                    else
                        c3_bbb(to_b(1), to_b(2), to_b(3), from_b(1), from_b(2), from_b(3)) = &
                            coef
                    endif

                end select
            end associate
        enddo
        close(105)

        if (coef_norm == 0.0d0) then
            call abort_cc('Hartree--Fock coefficient not found!')
        endif

        ! Renormalize
        c1_a = c1_a / coef_norm
        c1_b = c1_b / coef_norm
        c2_aa = c2_aa / coef_norm
        c2_ab = c2_ab / coef_norm
        c2_bb = c2_bb / coef_norm
        c3_aaa = c3_aaa / coef_norm
        c3_aab = c3_aab / coef_norm
        c3_abb = c3_abb / coef_norm
        c3_bbb = c3_bbb / coef_norm

        call antisymmetrize(c2_aa, c2_ab, c2_bb, c3_aaa, c3_aab, c3_abb, c3_bbb)

    end subroutine find_fciqmc_c3

    subroutine find_fciqmc_c4_aaaa(filename, config_debug, coef_norm, c4_aaaa)

        ! Translate configurations to excitations for FCIQMC coefficients
        !
        ! In:
        !   filename: file containing FCIQMC walkers per determinant
        !   config_debug: if true, write configurations in occupation form
        ! In/Out:
        !   c*: arrays containing the spin-integrated coefficients based on excitation rank

        ! [TODO] make this a type and pass it nicely
        real(kind=8), allocatable, intent(inout) :: c4_aaaa(:,:,:,:,:,:,:,:)
        real(dp), intent(in) :: coef_norm

        logical, intent(in) :: config_debug

        ! These scratch arrays to generate configurations on

        ! Aux variable
        character(len=*), intent(in) :: filename
        type(excit_t) :: excitation
        integer :: excit_rank
        integer :: ios
        integer :: line(2)
        integer :: conf(nele)

        ! Run over all alpha combinations
        open(unit=105, file=trim(filename), status='old')

        do
            read(105, *, iostat=ios) line, conf
            if (ios /= 0) exit

            call decode_determinant(line(2), conf, excitation)

            associate(excit_rank_a=>excitation%rank_a, coef=>excitation%coef, &
                    to_a=>excitation%to_a, to_b=>excitation%to_b, &
                    from_a=>excitation%from_a, from_b=>excitation%from_b)

                excit_rank = excit_rank_a + excitation%rank_b
                if (excit_rank /= 4) cycle

                if (excit_rank_a == 4) then
                    c4_aaaa(to_a(1), to_a(2), to_a(3), to_a(4), &
                        from_a(1), from_a(2), from_a(3), from_a(4)) = &
                        coef
                endif

            end associate
        enddo

        close(105)
        ! Renormalize
        c4_aaaa = c4_aaaa / coef_norm

    end subroutine find_fciqmc_c4_aaaa

    subroutine find_fciqmc_c4_aaab(filename, config_debug, coef_norm, c4_aaab)

        ! Translate configurations to excitations for FCIQMC coefficients
        !
        ! In:
        !   filename: file containing FCIQMC walkers per determinant
        !   config_debug: if true, write configurations in occupation form
        ! In/Out:
        !   c*: arrays containing the spin-integrated coefficients based on excitation rank

        ! [TODO] make this a type and pass it nicely
        real(kind=8), allocatable, intent(inout) :: c4_aaab(:,:,:,:,:,:,:,:)
        real(dp), intent(in) :: coef_norm

        logical, intent(in) :: config_debug

        ! These scratch arrays to generate configurations on

        ! Aux variable
        character(len=*), intent(in) :: filename
        type(excit_t) :: excitation
        integer :: excit_rank
        integer :: ios
        integer :: line(2)
        integer :: conf(nele)

        ! Run over all alpha combinations
        open(unit=105, file=trim(filename), status='old')

        do
            read(105, *, iostat=ios) line, conf
            if (ios /= 0) exit

            call decode_determinant(line(2), conf, excitation)
            associate(excit_rank_a=>excitation%rank_a, coef=>excitation%coef, &
                    to_a=>excitation%to_a, to_b=>excitation%to_b, &
                    from_a=>excitation%from_a, from_b=>excitation%from_b)

                excit_rank = excit_rank_a + excitation%rank_b
                if (excit_rank /= 4) cycle

                if (excit_rank_a == 3) then
                    c4_aaab(to_a(1), to_a(2), to_a(3), to_b(1), &
                        from_a(1), from_a(2), from_a(3), from_b(1)) = &
                        coef
                endif

            end associate
        enddo

        close(105)

        ! Renormalize
        c4_aaab = c4_aaab / coef_norm

    end subroutine find_fciqmc_c4_aaab

    subroutine find_fciqmc_c4_aabb(filename, config_debug, coef_norm, c4_aabb)

        ! Translate configurations to excitations for FCIQMC coefficients
        !
        ! In:
        !   filename: file containing FCIQMC walkers per determinant
        !   config_debug: if true, write configurations in occupation form
        ! In/Out:
        !   c*: arrays containing the spin-integrated coefficients based on excitation rank

        ! [TODO] make this a type and pass it nicely
        real(kind=8), allocatable, intent(inout) :: c4_aabb(:,:,:,:,:,:,:,:)
        real(dp), intent(in) :: coef_norm

        logical, intent(in) :: config_debug

        ! These scratch arrays to generate configurations on

        ! Aux variable
        character(len=*), intent(in) :: filename
        type(excit_t) :: excitation
        integer :: excit_rank
        integer :: ios
        integer :: line(2)
        integer :: conf(nele)

        ! Run over all alpha combinations
        open(unit=105, file=trim(filename), status='old')

        do
            read(105, *, iostat=ios) line, conf
            if (ios /= 0) exit

            call decode_determinant(line(2), conf, excitation)
            associate(excit_rank_a=>excitation%rank_a, coef=>excitation%coef, &
                    to_a=>excitation%to_a, to_b=>excitation%to_b, &
                    from_a=>excitation%from_a, from_b=>excitation%from_b)

                excit_rank = excit_rank_a + excitation%rank_b
                if (excit_rank /= 4) cycle

                if (excit_rank_a == 2) then
                    c4_aabb(to_a(1), to_a(2), to_b(1), to_b(2), &
                        from_a(1), from_a(2), from_b(1), from_b(2)) = &
                        coef
                endif

            end associate
        enddo

        close(105)
        ! Renormalize
        c4_aabb = c4_aabb / coef_norm

    end subroutine find_fciqmc_c4_aabb

    subroutine find_fciqmc_c4_abbb(filename, config_debug, coef_norm, c4_abbb)

        ! Translate configurations to excitations for FCIQMC coefficients
        !
        ! In:
        !   filename: file containing FCIQMC walkers per determinant
        !   config_debug: if true, write configurations in occupation form
        ! In/Out:
        !   c*: arrays containing the spin-integrated coefficients based on excitation rank

        ! [TODO] make this a type and pass it nicely
        real(kind=8), allocatable, intent(inout) :: c4_abbb(:,:,:,:,:,:,:,:)
        real(dp), intent(in) :: coef_norm

        logical, intent(in) :: config_debug

        ! These scratch arrays to generate configurations on

        ! Aux variable
        character(len=*), intent(in) :: filename
        type(excit_t) :: excitation
        integer :: excit_rank
        integer :: ios
        integer :: line(2)
        integer :: conf(nele)

        ! Run over all alpha combinations
        open(unit=105, file=trim(filename), status='old')

        do
            read(105, *, iostat=ios) line, conf
            if (ios /= 0) exit

            call decode_determinant(line(2), conf, excitation)
            associate(excit_rank_a=>excitation%rank_a, coef=>excitation%coef, &
                    to_a=>excitation%to_a, to_b=>excitation%to_b, &
                    from_a=>excitation%from_a, from_b=>excitation%from_b)

                excit_rank = excit_rank_a + excitation%rank_b
                if (excit_rank /= 4) cycle

                if (excit_rank_a == 1) then
                    c4_abbb(to_a(1), to_b(1), to_b(2), to_b(3), &
                        from_a(1), from_b(1), from_b(2), from_b(3)) = &
                        coef
                endif

            end associate
        enddo

        close(105)
        ! Renormalize
        c4_abbb = c4_abbb / coef_norm

    end subroutine find_fciqmc_c4_abbb

    subroutine find_fciqmc_c4_bbbb(filename, config_debug, coef_norm, c4_bbbb)

        ! Translate configurations to excitations for FCIQMC coefficients
        !
        ! In:
        !   filename: file containing FCIQMC walkers per determinant
        !   config_debug: if true, write configurations in occupation form
        ! In/Out:
        !   c*: arrays containing the spin-integrated coefficients based on excitation rank

        ! [TODO] make this a type and pass it nicely
        real(kind=8), allocatable, intent(inout) :: c4_bbbb(:,:,:,:,:,:,:,:)
        real(dp), intent(in) :: coef_norm

        logical, intent(in) :: config_debug

        ! These scratch arrays to generate configurations on

        ! Aux variable
        character(len=*), intent(in) :: filename
        type(excit_t) :: excitation
        integer :: excit_rank
        integer :: ios
        integer :: line(2)
        integer :: conf(nele)

        ! Run over all alpha combinations
        open(unit=105, file=trim(filename), status='old')

        do
            read(105, *, iostat=ios) line, conf
            if (ios /= 0) exit

            call decode_determinant(line(2), conf, excitation)
            associate(excit_rank_a=>excitation%rank_a, coef=>excitation%coef, &
                    to_a=>excitation%to_a, to_b=>excitation%to_b, &
                    from_a=>excitation%from_a, from_b=>excitation%from_b)

                excit_rank = excit_rank_a + excitation%rank_b
                if (excit_rank /= 4) cycle

                if (excit_rank_a == 0) then
                    c4_bbbb(to_b(1), to_b(2), to_b(3), to_b(4), &
                        from_b(1), from_b(2), from_b(3), from_b(4)) = &
                        coef
                endif

            end associate
        enddo

        close(105)
        ! Renormalize
        c4_bbbb = c4_bbbb / coef_norm

    end subroutine find_fciqmc_c4_bbbb

    subroutine spin_integrate(read_type, pos_a, pos_b, coef, excitation)

        ! Fill in spin-integrated CI coefficient arrays
        !
        ! In:
        !   read_type: type of occupation representation. Depends on the type of CI.
        !   pos_a: alpha occupied orbitals
        !   pos_b: beta occupied orbitals
        !   coef: CI coefficient
        ! In/Out:
        !   c*: arrays containing the spin-integrated coefficients based on excitation rank
        use const, only: dp

        character(len=*), intent(in) :: read_type
        real(dp), intent(in) :: coef
        integer, allocatable, intent(in) :: pos_a(:), pos_b(:)
        type(excit_t), intent(inout) :: excitation

        integer :: i, j, w
        integer :: excit_rank, excit_rank_a, excit_rank_b
        integer :: from_a(4), to_a(4)
        integer :: from_b(4), to_b(4)
        integer :: from_spin(4), to_spin(4)

        procedure(i_excitation_sign), pointer :: excitation_sign_ptr => null()
        procedure(i_convert_spin), pointer :: convert_spin_ptr => null()

        ! Select the right phase factor function for a given the CI type
        select case (read_type)
        case ('g')
            excitation_sign_ptr => excitation_sign_gam
            convert_spin_ptr => convert_spin_gam

        case ('q')
            excitation_sign_ptr => excitation_sign_qmc
            convert_spin_ptr => convert_spin_qmc
        end select


        ! Get alpha excitation
        call find_excitation(pos_a, occ_a, excit_rank_a, from_a, to_a)
        ! Get beta excitation
        call find_excitation(pos_b, occ_b, excit_rank_b, from_b, to_b)

        excitation%from_a = from_a
        excitation%from_b = from_b
        excitation%to_a = to_a
        excitation%to_b = to_b
        excitation%rank_a = excit_rank_a
        excitation%rank_b = excit_rank_b

        ! WARNING: Remember that a < b < c < ..., i < j < k < ...
        ! e.g.  c2_aa(a,b,i,j)

        excit_rank = excit_rank_a + excit_rank_b
        select case (excit_rank)
        ! convert_spin_ptr options: 1 -> alpha from, 2 -> beta from, 3 -> alpha to, 4 -> beta to
    case (1)
        if (excit_rank_a == 1) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        else
            from_spin(1) = convert_spin_ptr(from_b(1), 2)
            to_spin(1) = convert_spin_ptr(to_b(1), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        endif

    case (2)
        if (excit_rank_a == 2) then

            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_a(2), 1)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_a(2), 3)

            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        elseif (excit_rank_a == 1) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_b(1), 2)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_b(1), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        else
            from_spin(1) = convert_spin_ptr(from_b(1), 2)
            from_spin(2) = convert_spin_ptr(from_b(2), 2)
            to_spin(1) = convert_spin_ptr(to_b(1), 4)
            to_spin(2) = convert_spin_ptr(to_b(2), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        endif

    case(3)
        if (excit_rank_a == 3) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_a(2), 1)
            from_spin(3) = convert_spin_ptr(from_a(3), 1)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_a(2), 3)
            to_spin(3) = convert_spin_ptr(to_a(3), 3)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        elseif (excit_rank_a == 2) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_a(2), 1)
            from_spin(3) = convert_spin_ptr(from_b(1), 2)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_a(2), 3)
            to_spin(3) = convert_spin_ptr(to_b(1), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        elseif (excit_rank_a == 1) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_b(1), 2)
            from_spin(3) = convert_spin_ptr(from_b(2), 2)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_b(1), 4)
            to_spin(3) = convert_spin_ptr(to_b(2), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        else
            from_spin(1) = convert_spin_ptr(from_b(1), 2)
            from_spin(2) = convert_spin_ptr(from_b(2), 2)
            from_spin(3) = convert_spin_ptr(from_b(3), 2)
            to_spin(1) = convert_spin_ptr(to_b(1), 4)
            to_spin(2) = convert_spin_ptr(to_b(2), 4)
            to_spin(3) = convert_spin_ptr(to_b(3), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        endif

    case(4)
        if (excit_rank_a == 4) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_a(2), 1)
            from_spin(3) = convert_spin_ptr(from_a(3), 1)
            from_spin(4) = convert_spin_ptr(from_a(4), 1)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_a(2), 3)
            to_spin(3) = convert_spin_ptr(to_a(3), 3)
            to_spin(4) = convert_spin_ptr(to_a(4), 3)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        elseif (excit_rank_a == 3) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_a(2), 1)
            from_spin(3) = convert_spin_ptr(from_a(3), 1)
            from_spin(4) = convert_spin_ptr(from_b(1), 2)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_a(2), 3)
            to_spin(3) = convert_spin_ptr(to_a(3), 3)
            to_spin(4) = convert_spin_ptr(to_b(1), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        elseif (excit_rank_a == 2) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_a(2), 1)
            from_spin(3) = convert_spin_ptr(from_b(1), 2)
            from_spin(4) = convert_spin_ptr(from_b(2), 2)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_a(2), 3)
            to_spin(3) = convert_spin_ptr(to_b(1), 4)
            to_spin(4) = convert_spin_ptr(to_b(2), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)

        elseif (excit_rank_a == 1) then
            from_spin(1) = convert_spin_ptr(from_a(1), 1)
            from_spin(2) = convert_spin_ptr(from_b(1), 2)
            from_spin(3) = convert_spin_ptr(from_b(2), 2)
            from_spin(4) = convert_spin_ptr(from_b(3), 2)
            to_spin(1) = convert_spin_ptr(to_a(1), 3)
            to_spin(2) = convert_spin_ptr(to_b(1), 4)
            to_spin(3) = convert_spin_ptr(to_b(2), 4)
            to_spin(4) = convert_spin_ptr(to_b(3), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        else
            from_spin(1) = convert_spin_ptr(from_b(1), 2)
            from_spin(2) = convert_spin_ptr(from_b(2), 2)
            from_spin(3) = convert_spin_ptr(from_b(3), 2)
            from_spin(4) = convert_spin_ptr(from_b(4), 2)
            to_spin(1) = convert_spin_ptr(to_b(1), 4)
            to_spin(2) = convert_spin_ptr(to_b(2), 4)
            to_spin(3) = convert_spin_ptr(to_b(3), 4)
            to_spin(4) = convert_spin_ptr(to_b(4), 4)
            excitation%coef = &
                coef * excitation_sign_ptr(excit_rank, from_spin, to_spin)
        endif

    end select

end subroutine spin_integrate

subroutine antisymmetrize(c2_aa, c2_ab, c2_bb, &
        c3_aaa, c3_aab, c3_abb, c3_bbb)

    ! Antisymmetrize arrays
    !
    ! In/Out:
    !   c*: CI or CC arrays

    real(kind=8), allocatable, intent(inout) :: c2_aa(:,:,:,:)
    real(kind=8), allocatable, intent(inout) :: c2_ab(:,:,:,:)
    real(kind=8), allocatable, intent(inout) :: c2_bb(:,:,:,:)

    real(kind=8), allocatable, intent(inout) :: c3_aaa(:,:,:,:,:,:)
    real(kind=8), allocatable, intent(inout) :: c3_aab(:,:,:,:,:,:)
    real(kind=8), allocatable, intent(inout) :: c3_abb(:,:,:,:,:,:)
    real(kind=8), allocatable, intent(inout) :: c3_bbb(:,:,:,:,:,:)

    integer :: a, b, c
    integer :: i, j, k
    do i=froz+1, occ_a
        do j=i+1, occ_a
            do a=occ_a+1, total
                do b=a+1, total
                    c2_aa(a,b,j,i)=-c2_aa(a,b,i,j) !(ij)
                    c2_aa(b,a,i,j)=-c2_aa(a,b,i,j) !(ab)
                    c2_aa(b,a,j,i)=c2_aa(a,b,i,j) !(ab)(ij)
                enddo
            enddo
        enddo
    enddo

    do i=froz+1, occ_b
        do j=i+1, occ_b
            do a=occ_b+1, total
                do b=a+1, total
                    c2_bb(a,b,j,i)=-c2_bb(a,b,i,j) !(ij)
                    c2_bb(b,a,i,j)=-c2_bb(a,b,i,j) !(ab)
                    c2_bb(b,a,j,i)=c2_bb(a,b,i,j) !(ab)(ij)
                enddo
            enddo
        enddo
    enddo

    do i=froz+1, occ_a
        do j=i+1, occ_a
            do k=j+1, occ_a
                do a=occ_a+1, total
                    do b=a+1, total
                        do c=b+1, total
                            c3_aaa(a,b,c,j,i,k)=-c3_aaa(a,b,c,i,j,k) !(ij)
                            c3_aaa(a,b,c,k,j,i)=-c3_aaa(a,b,c,i,j,k) !(ik)
                            c3_aaa(a,b,c,i,k,j)=-c3_aaa(a,b,c,i,j,k) !(jk)
                            c3_aaa(a,b,c,j,k,i)=c3_aaa(a,b,c,i,j,k) !(ijk)
                            c3_aaa(a,b,c,k,i,j)=c3_aaa(a,b,c,i,j,k) !(ikj)
                            c3_aaa(b,a,c,i,j,k)=-c3_aaa(a,b,c,i,j,k) !(ab)
                            c3_aaa(b,a,c,j,i,k)=c3_aaa(a,b,c,i,j,k) !(ab)(ij)
                            c3_aaa(b,a,c,k,j,i)=c3_aaa(a,b,c,i,j,k) !(ab)(ik)
                            c3_aaa(b,a,c,i,k,j)=c3_aaa(a,b,c,i,j,k) !(ab)(jk)
                            c3_aaa(b,a,c,j,k,i)=-c3_aaa(a,b,c,i,j,k) !(ab)(ijk)
                            c3_aaa(b,a,c,k,i,j)=-c3_aaa(a,b,c,i,j,k) !(ab)(ikj)
                            c3_aaa(c,b,a,i,j,k)=-c3_aaa(a,b,c,i,j,k) !(ac)
                            c3_aaa(c,b,a,j,i,k)=c3_aaa(a,b,c,i,j,k) !(ac)(ij)
                            c3_aaa(c,b,a,k,j,i)=c3_aaa(a,b,c,i,j,k) !(ac)(ik)
                            c3_aaa(c,b,a,i,k,j)=c3_aaa(a,b,c,i,j,k) !(ac)(jk)
                            c3_aaa(c,b,a,j,k,i)=-c3_aaa(a,b,c,i,j,k) !(ac)(ijk)
                            c3_aaa(c,b,a,k,i,j)=-c3_aaa(a,b,c,i,j,k) !(ac)(ikj)
                            c3_aaa(a,c,b,i,j,k)=-c3_aaa(a,b,c,i,j,k) !(bc)
                            c3_aaa(a,c,b,j,i,k)=c3_aaa(a,b,c,i,j,k) !(bc)(ij)
                            c3_aaa(a,c,b,k,j,i)=c3_aaa(a,b,c,i,j,k) !(bc)(ik)
                            c3_aaa(a,c,b,i,k,j)=c3_aaa(a,b,c,i,j,k) !(bc)(jk)
                            c3_aaa(a,c,b,j,k,i)=-c3_aaa(a,b,c,i,j,k) !(bc)(ijk)
                            c3_aaa(a,c,b,k,i,j)=-c3_aaa(a,b,c,i,j,k) !(bc)(ikj)
                            c3_aaa(b,c,a,i,j,k)=c3_aaa(a,b,c,i,j,k) !(abc)
                            c3_aaa(b,c,a,j,i,k)=-c3_aaa(a,b,c,i,j,k) !(abc)(ij)
                            c3_aaa(b,c,a,k,j,i)=-c3_aaa(a,b,c,i,j,k) !(abc)(ik)
                            c3_aaa(b,c,a,i,k,j)=-c3_aaa(a,b,c,i,j,k) !(abc)(jk)
                            c3_aaa(b,c,a,j,k,i)=c3_aaa(a,b,c,i,j,k) !(abc)(ijk)
                            c3_aaa(b,c,a,k,i,j)=c3_aaa(a,b,c,i,j,k) !(abc)(ikj)
                            c3_aaa(c,a,b,i,j,k)=c3_aaa(a,b,c,i,j,k) !(acb)
                            c3_aaa(c,a,b,j,i,k)=-c3_aaa(a,b,c,i,j,k) !(acb)(ij)
                            c3_aaa(c,a,b,k,j,i)=-c3_aaa(a,b,c,i,j,k) !(acb)(ik)
                            c3_aaa(c,a,b,i,k,j)=-c3_aaa(a,b,c,i,j,k) !(acb)(jk)
                            c3_aaa(c,a,b,j,k,i)=c3_aaa(a,b,c,i,j,k) !(acb)(ijk)
                            c3_aaa(c,a,b,k,i,j)=c3_aaa(a,b,c,i,j,k) !(acb)(ikj)
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo

    do i=froz+1, occ_a
        do j=i+1, occ_a
            do k=froz+1, occ_b
                do a=occ_a+1, total
                    do b=a+1, total
                        do c=occ_b+1, total
                            c3_aab(a,b,c,j,i,k)=-c3_aab(a,b,c,i,j,k) !(ij)
                            c3_aab(b,a,c,i,j,k)=-c3_aab(a,b,c,i,j,k) !(ab)
                            c3_aab(b,a,c,j,i,k)=c3_aab(a,b,c,i,j,k) !(ab)(ij)
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo

    do i=froz+1, occ_a
        do j=froz+1, occ_b
            do k=j+1, occ_b
                do a=occ_a+1, total
                    do b=occ_b+1, total
                        do c=b+1, total
                            c3_abb(a,b,c,i,k,j)=-c3_abb(a,b,c,i,j,k) !(jk)
                            c3_abb(a,c,b,i,j,k)=-c3_abb(a,b,c,i,j,k) !(bc)
                            c3_abb(a,c,b,i,k,j)=c3_abb(a,b,c,i,j,k) !(bc)(jk)
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo

    do i=froz+1, occ_b
        do j=i+1, occ_b
            do k=j+1, occ_b
                do a=occ_b+1, total
                    do b=a+1, total
                        do c=b+1, total
                            c3_bbb(a,b,c,j,i,k)=-c3_bbb(a,b,c,i,j,k) !(ij)
                            c3_bbb(a,b,c,k,j,i)=-c3_bbb(a,b,c,i,j,k) !(ik)
                            c3_bbb(a,b,c,i,k,j)=-c3_bbb(a,b,c,i,j,k) !(jk)
                            c3_bbb(a,b,c,j,k,i)=c3_bbb(a,b,c,i,j,k) !(ijk)
                            c3_bbb(a,b,c,k,i,j)=c3_bbb(a,b,c,i,j,k) !(ikj)
                            c3_bbb(b,a,c,i,j,k)=-c3_bbb(a,b,c,i,j,k) !(ab)
                            c3_bbb(b,a,c,j,i,k)=c3_bbb(a,b,c,i,j,k) !(ab)(ij)
                            c3_bbb(b,a,c,k,j,i)=c3_bbb(a,b,c,i,j,k) !(ab)(ik)
                            c3_bbb(b,a,c,i,k,j)=c3_bbb(a,b,c,i,j,k) !(ab)(jk)
                            c3_bbb(b,a,c,j,k,i)=-c3_bbb(a,b,c,i,j,k) !(ab)(ijk)
                            c3_bbb(b,a,c,k,i,j)=-c3_bbb(a,b,c,i,j,k) !(ab)(ikj)
                            c3_bbb(c,b,a,i,j,k)=-c3_bbb(a,b,c,i,j,k) !(ac)
                            c3_bbb(c,b,a,j,i,k)=c3_bbb(a,b,c,i,j,k) !(ac)(ij)
                            c3_bbb(c,b,a,k,j,i)=c3_bbb(a,b,c,i,j,k) !(ac)(ik)
                            c3_bbb(c,b,a,i,k,j)=c3_bbb(a,b,c,i,j,k) !(ac)(jk)
                            c3_bbb(c,b,a,j,k,i)=-c3_bbb(a,b,c,i,j,k) !(ac)(ijk)
                            c3_bbb(c,b,a,k,i,j)=-c3_bbb(a,b,c,i,j,k) !(ac)(ikj)
                            c3_bbb(a,c,b,i,j,k)=-c3_bbb(a,b,c,i,j,k) !(bc)
                            c3_bbb(a,c,b,j,i,k)=c3_bbb(a,b,c,i,j,k) !(bc)(ij)
                            c3_bbb(a,c,b,k,j,i)=c3_bbb(a,b,c,i,j,k) !(bc)(ik)
                            c3_bbb(a,c,b,i,k,j)=c3_bbb(a,b,c,i,j,k) !(bc)(jk)
                            c3_bbb(a,c,b,j,k,i)=-c3_bbb(a,b,c,i,j,k) !(bc)(ijk)
                            c3_bbb(a,c,b,k,i,j)=-c3_bbb(a,b,c,i,j,k) !(bc)(ikj)
                            c3_bbb(b,c,a,i,j,k)=c3_bbb(a,b,c,i,j,k) !(abc)
                            c3_bbb(b,c,a,j,i,k)=-c3_bbb(a,b,c,i,j,k) !(abc)(ij)
                            c3_bbb(b,c,a,k,j,i)=-c3_bbb(a,b,c,i,j,k) !(abc)(ik)
                            c3_bbb(b,c,a,i,k,j)=-c3_bbb(a,b,c,i,j,k) !(abc)(jk)
                            c3_bbb(b,c,a,j,k,i)=c3_bbb(a,b,c,i,j,k) !(abc)(ijk)
                            c3_bbb(b,c,a,k,i,j)=c3_bbb(a,b,c,i,j,k) !(abc)(ikj)
                            c3_bbb(c,a,b,i,j,k)=c3_bbb(a,b,c,i,j,k) !(acb)
                            c3_bbb(c,a,b,j,i,k)=-c3_bbb(a,b,c,i,j,k) !(acb)(ij)
                            c3_bbb(c,a,b,k,j,i)=-c3_bbb(a,b,c,i,j,k) !(acb)(ik)
                            c3_bbb(c,a,b,i,k,j)=-c3_bbb(a,b,c,i,j,k) !(acb)(jk)
                            c3_bbb(c,a,b,j,k,i)=c3_bbb(a,b,c,i,j,k) !(acb)(ijk)
                            c3_bbb(c,a,b,k,i,j)=c3_bbb(a,b,c,i,j,k) !(acb)(ikj)
                        enddo
                    enddo
                enddo
            enddo
        enddo
    enddo

end subroutine antisymmetrize

subroutine find_excitation(pos, pos_len, excit_rank, &
        from, to)

    ! Find excitation for an occupation number representation
    !
    ! In:
    !   pos: occupied orbitals
    !   pos_len: number of orbitals
    !   excit_rank: excitation rank
    ! In/Out:
    !   from: orbitals exciting from
    !   to: orbitals exciting to

    integer, allocatable, intent(in) :: pos(:)
    integer, intent(in) :: pos_len

    integer, intent(inout) :: excit_rank
    integer, intent(inout) :: from(4), to(4)

    integer :: i, j, w
    logical :: found_unexcit

    ! Find orbitals where electrons are excited into
    excit_rank = 0
    do i=1, pos_len
        if (pos(i) > pos_len) then
            excit_rank = excit_rank + 1
            to(excit_rank) = pos(i)
        endif
    enddo

    ! Find orbitals where electrons are excited from
    if (excit_rank > 0) then
        w = 1
        do j=1, pos_len
            found_unexcit = .false.
            do i=1, pos_len - excit_rank
                if (pos(i) == j) then
                    found_unexcit = .true.
                    exit
                endif
            enddo

            if (.not. found_unexcit) then
                from(w) = j
                w = w + 1
            endif

        enddo
    endif

end subroutine find_excitation

function excitation_sign_gam(excitation_rank, from, to) result(det_sign)

    ! Find phase factor of GAMESS determinants. These determinants are
    ! given such that all alpha electrons are given first and
    ! beta electrons, second.
    !
    ! In:
    !   excitation_rank: rank of the excited determinant
    !   from: orbitals exciting from
    !   to: orbitals exciting to
    ! Out:
    !   det_sign: phase factor

    integer, intent(in) :: excitation_rank
    integer, intent(in) :: from(4)
    integer, intent(in) :: to(4)
    integer :: temp_ref(120)

    integer :: det_sign
    integer :: i, idx
    integer :: occ_sum

    ! Create reference determinant
    temp_ref = 0
    do i=1, occ_a
        temp_ref(i) = 1
    enddo

    do i=total+1, total+occ_b
        temp_ref(i) = 1
    enddo

    ! Occupied permutation counter
    occ_sum = 0

    ! Loop over all excitations
    do idx=1, excitation_rank
        do i=1, from(idx) - 1
            if (temp_ref(i) == 1) then
                occ_sum = occ_sum + 1
            endif
        enddo

        temp_ref(from(idx)) = 0

        do i=1, to(idx) - 1
            if (temp_ref(i) == 1) then
                occ_sum = occ_sum + 1
            endif
        enddo

        temp_ref(to(idx)) = 1
    enddo

    det_sign = (-1) ** occ_sum

end function excitation_sign_gam

function excitation_sign_qmc(excitation_rank, from, to) result(det_sign)

    ! Find phase factor of FCIQMC determinants. These determinants are
    ! given such that odd numbers contain alpha electrons and even numbers
    ! contain beta electrons.
    !
    ! In:
    !   excitation_rank: rank of the excited determinant
    !   from: orbitals exciting from
    !   to: orbitals exciting to
    ! Out:
    !   det_sign: phase factor

    integer, intent(in) :: excitation_rank
    integer, intent(in) :: from(4)
    integer, intent(in) :: to(4)
    integer :: temp_ref(250)

    integer :: det_sign
    integer :: i, idx
    integer :: occ_sum

    ! Create reference determinant
    temp_ref = 0
    do i=1, nele
        temp_ref(i) = 1
    enddo

    ! Occupied permutation counter
    occ_sum = 0

    ! Loop over all excitations
    do idx=1, excitation_rank
        do i=1, from(idx) - 1
            if (temp_ref(i) == 1) then
                occ_sum = occ_sum + 1
            endif
        enddo

        temp_ref(from(idx)) = 0

        do i=1, to(idx) - 1
            if (temp_ref(i) == 1) then
                occ_sum = occ_sum + 1
            endif
        enddo

        temp_ref(to(idx)) = 1
    enddo

    det_sign = (-1) ** occ_sum

end function excitation_sign_qmc


subroutine advance_elec(elec_pos, nele)

    ! Calculate a determinant configuration.
    ! Taken from GAMESS aldeci.src ADVANC
    !
    ! In:
    !   nele: number of electrons
    ! In/Out:
    !   elec_pos: spin-orbital configuration

    integer, intent(in) :: nele
    integer, allocatable, intent(inout) :: elec_pos(:)
    integer :: i, j

    if (elec_pos(nele) == total) then
        do i=nele-1, 1, -1
            if (elec_pos(i+1) - elec_pos(i) > 1) then
                elec_pos(i) = elec_pos(i) + 1
                ! Shift inside the gap
                do j=i+1, nele
                    elec_pos(j) = elec_pos(j-1) + 1
                enddo

                return
            endif
        enddo
    endif

    elec_pos(nele) = elec_pos(nele) + 1
end subroutine advance_elec

function convert_spin_gam(i, spin) result(i_spin)

    ! Convert the spin representation of GAMESS type configurations,
    ! where alpha electrons come first and beta, second.
    !
    ! In:
    !   i: orbital number
    !   spin: spin/orbital type type. Explanation below
    ! Out:
    !   i_spin: occupation number representation of the GAMESS form
    !       (i.e. alpha first, beta second)

    integer, intent(in) :: i
    integer, intent(in) :: spin

    integer :: i_spin

    if (spin == 1) then
        ! Alpha case, from
        i_spin = i
    elseif (spin == 2) then
        ! Beta case from
        i_spin = i + total
    elseif (spin == 3) then
        ! Alpha case to
        i_spin = i
    elseif (spin == 4) then
        ! Beta case to
        i_spin = i + total
    endif

end function convert_spin_gam

function convert_spin_qmc(i, spin) result(i_spin)

    ! Convert the spin representation of FCIQMC type configurations,
    ! where alpha electrons are odd and beta are even.
    !
    ! In:
    !   i: orbital number
    !   spin: spin/orbital type type. Explanation below
    ! Out:
    !   i_spin: occupation number representation of the FCIQMC form
    !       (i.e. alpha odd, beta even)

    integer, intent(in) :: i
    integer, intent(in) :: spin

    integer :: i_spin

    if (spin == 1) then
        ! Alpha case, from
        i_spin = 2 * (i - 1) + 1
    elseif (spin == 2) then
        ! Beta case from
        i_spin = 2 * i
    elseif (spin == 3) then
        ! Alpha case to
        i_spin = 2 * (i - 1) + 1
    elseif (spin == 4) then
        ! Beta case to
        i_spin = 2 * i
    endif

end function convert_spin_qmc

end module process_ci
