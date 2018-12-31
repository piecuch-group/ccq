module determinant

    use const, only: dp
    use sys_data

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
contains

    subroutine decode_determinant(qmc_coef, conf, excitation)

        use ext_cor_types, only: excit_t

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
            excitation%coef = real(qmc_coef,dp)
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

            call spin_integrate('q', pos_a, pos_b, real(qmc_coef,dp), excitation)
            deallocate(pos_a, pos_b)
        endif

    end subroutine decode_determinant

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
        use ext_cor_types, only: excit_t

        character(len=*), intent(in) :: read_type
        real(dp), intent(in) :: coef
        integer, allocatable, intent(in) :: pos_a(:), pos_b(:)
        type(excit_t), intent(inout) :: excitation

        integer :: i, j, w
        integer :: excit_rank, excit_rank_a, excit_rank_b
        integer :: from_a(4), to_a(4)
        integer :: from_b(4), to_b(4)
        integer :: from_spin(4), to_spin(4)
        ! [TODO] use associate
        integer :: excit_sign

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
        ! convert_spin_ptr options: 1 -> alpha from, 2 -> beta from, 3 -> alpha to, 4 -> beta to
        select case (excit_rank)
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
                excit_sign = excitation_sign_ptr(excit_rank, from_spin, to_spin)
                excitation%coef = &
                    coef * excit_sign
            elseif (excit_rank_a == 3) then
                from_spin(1) = convert_spin_ptr(from_a(1), 1)
                from_spin(2) = convert_spin_ptr(from_a(2), 1)
                from_spin(3) = convert_spin_ptr(from_a(3), 1)
                from_spin(4) = convert_spin_ptr(from_b(1), 2)
                to_spin(1) = convert_spin_ptr(to_a(1), 3)
                to_spin(2) = convert_spin_ptr(to_a(2), 3)
                to_spin(3) = convert_spin_ptr(to_a(3), 3)
                to_spin(4) = convert_spin_ptr(to_b(1), 4)
                excit_sign = excitation_sign_ptr(excit_rank, from_spin, to_spin)
                excitation%coef = &
                    coef * excit_sign
            elseif (excit_rank_a == 2) then
                from_spin(1) = convert_spin_ptr(from_a(1), 1)
                from_spin(2) = convert_spin_ptr(from_a(2), 1)
                from_spin(3) = convert_spin_ptr(from_b(1), 2)
                from_spin(4) = convert_spin_ptr(from_b(2), 2)
                to_spin(1) = convert_spin_ptr(to_a(1), 3)
                to_spin(2) = convert_spin_ptr(to_a(2), 3)
                to_spin(3) = convert_spin_ptr(to_b(1), 4)
                to_spin(4) = convert_spin_ptr(to_b(2), 4)
                excit_sign = excitation_sign_ptr(excit_rank, from_spin, to_spin)
                excitation%coef = &
                    coef * excit_sign

            elseif (excit_rank_a == 1) then
                from_spin(1) = convert_spin_ptr(from_a(1), 1)
                from_spin(2) = convert_spin_ptr(from_b(1), 2)
                from_spin(3) = convert_spin_ptr(from_b(2), 2)
                from_spin(4) = convert_spin_ptr(from_b(3), 2)
                to_spin(1) = convert_spin_ptr(to_a(1), 3)
                to_spin(2) = convert_spin_ptr(to_b(1), 4)
                to_spin(3) = convert_spin_ptr(to_b(2), 4)
                to_spin(4) = convert_spin_ptr(to_b(3), 4)
                excit_sign = excitation_sign_ptr(excit_rank, from_spin, to_spin)
                excitation%coef = &
                    coef * excit_sign
            else
                from_spin(1) = convert_spin_ptr(from_b(1), 2)
                from_spin(2) = convert_spin_ptr(from_b(2), 2)
                from_spin(3) = convert_spin_ptr(from_b(3), 2)
                from_spin(4) = convert_spin_ptr(from_b(4), 2)
                to_spin(1) = convert_spin_ptr(to_b(1), 4)
                to_spin(2) = convert_spin_ptr(to_b(2), 4)
                to_spin(3) = convert_spin_ptr(to_b(3), 4)
                to_spin(4) = convert_spin_ptr(to_b(4), 4)
                excit_sign = excitation_sign_ptr(excit_rank, from_spin, to_spin)
                excitation%coef = &
                    coef * excit_sign
            endif
            excitation%excit_sign = excit_sign

        end select

    end subroutine spin_integrate

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

end module determinant
