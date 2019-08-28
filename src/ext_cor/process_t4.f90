module process_t4

    implicit none

contains

    subroutine gen_doubles_conf(sys, confs, f_ref)

        use const, only: i0
        use utils, only: combs, binom_i
        use checking, only: check_allocate, check_deallocate
        use determinants, only: encode_det
        use excitations, only: excit_t, create_excited_det
        use symmetry, only: is_sym
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        integer(i0), allocatable, intent(out) :: confs(:,:)
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)

        integer :: elecs(sys%nel), virts(sys%nvirt)
        integer, allocatable :: combs_elecs(:,:), combs_virts(:,:)
        integer(i0), allocatable :: tmp_confs(:,:)

        integer, parameter :: r = 2
        type(excit_t) :: excit
        integer(i0) :: f_tmp(sys%basis%string_len)

        integer :: ierr
        integer :: ex_orbs(8)
        integer :: ind_el, ind_conf

        integer :: a, b
        integer :: i, j, k, l

        integer :: spin_elecs, spin_virts
        integer :: ndoubles

        ! Set connection excitation level
        excit%nexcit = r

        ndoubles = binom_i(sys%nel-sys%froz*2, r) * binom_i(sys%nvirt, r)
        allocate(tmp_confs(sys%basis%string_len, ndoubles), stat=ierr)
        call check_allocate('tmp_confs', ndoubles, ierr)

        ind_conf = 0
        do a=sys%nel+1, sys%orbs*2
            do b=a+1, sys%orbs*2

                excit%to_orb(1:2) = [a, b]

                do i=sys%froz*2+1, sys%nel
                    do j=i+1, sys%nel

                        excit%from_orb(1:2) = [i, j]

                        if (check_spin(excit, r) .and. check_sym(excit, r)) then
                            call create_excited_det(sys%basis, f_ref, excit, f_tmp)
                            ind_conf = ind_conf + 1
                            tmp_confs(:,ind_conf) = f_tmp
                        endif

                    enddo
                enddo

            enddo
        enddo

        ! Compact configurations array
        allocate(confs(sys%basis%string_len, ind_conf), stat=ierr)
        call check_allocate('confs', ind_conf, ierr)
        confs(:,:) = tmp_confs(:,1:ind_conf)
        deallocate(tmp_confs)
        call check_deallocate('tmp_confs', ierr)


    end subroutine gen_doubles_conf

    pure subroutine update_doubles_projection(sys, doubles_conf, t4_conf, t4_amp, &
            doubles_projection, doubles_conf_hash, f_ref)

        use const, only: p, i0
        use det_hash
        use excitations, only: excit_t, get_excitation_level, get_excitation, create_excited_det, find_excitation_permutation2
        use hmat, only: get_v
        use system, only: sys_t
        use utils, only: next_comb

        type(sys_t), intent(in) :: sys
        integer(i0), intent(in) :: doubles_conf(:,:)
        integer(i0), intent(in) :: t4_conf(sys%basis%string_len)
        real(p), intent(in) :: t4_amp
        real(p), intent(inout) :: doubles_projection(:)
        type(dictionary_t), intent(in) :: doubles_conf_hash
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)

        integer(i0) :: f_doub(sys%basis%string_len)

        !  Local variables
        integer, parameter :: n = 4
        integer, parameter :: r = 2
        integer :: inds_from(r), inds_to(r)
        integer :: doubles_nconf
        integer :: iconf
        integer :: idx
        integer :: i, j, a, b
        integer :: nexcit
        type(excit_t) :: excit, excit_t4
        real(p) :: h_element

        logical :: done


        excit_t4 = get_excitation(sys%nel, sys%basis, f_ref, t4_conf)


        associate(from=>excit_t4%from_orb, to=>excit_t4%to_orb)

            do idx=1, r
                inds_from(idx) = idx
            enddo

            from_loop: do

                do idx=1, r
                    inds_to(idx) = idx
                    ! Inverting excitation
                    excit%to_orb(idx) = from(inds_from(idx))
                enddo


                to_loop: do

                    do idx=1, r
                        ! Inverting excitation
                        excit%from_orb(idx) = to(inds_to(idx))
                    enddo

                    ! Calculate matrix element. Note that only double excitations
                    ! are allowed, thus only twobody integrals are needed (Slater rules).
                    a = excit%from_orb(1)
                    b = excit%from_orb(2)
                    i = excit%to_orb(1)
                    j = excit%to_orb(2)

                    excit%nexcit = 2

                    h_element = get_v(sys%ints%v_aa, sys%ints%v_ab, sys%ints%v_bb, i, j, a, b)

                    if (h_element /= 0.0_p) then

                        call create_excited_det(sys%basis, t4_conf, excit, f_doub)
                        call find_excitation_permutation2(sys%basis%excit_mask, t4_conf, excit)

                        ! Apply the appropriate permutation parity
                        ! [TODO]
                        if (excit%perm) h_element = -h_element

                        iconf = get_val(f_doub, doubles_conf_hash%dict_size, doubles_conf_hash)

                        ! Update projection on doubles
                        doubles_projection(iconf) = doubles_projection(iconf) + (h_element * t4_amp)

                    endif

                    ! Get next combination of to orbitals
                    call next_comb(n, inds_to, r, done)
                    if (done) exit to_loop

                enddo to_loop

                ! Get next combination of to orbitals
                call next_comb(n, inds_from, r, done)
                if (done) exit from_loop

            enddo from_loop

        end associate

    end subroutine update_doubles_projection

    subroutine update_t2_cluster(sys, ext_cor, f_ref)

        use const, only: p, i0
        use excitations, only: excit_t, get_excitation_spin_integrate, get_excitation_level
        use ext_cor_types, only: ext_cor_t
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(ext_cor_t), intent(inout) :: ext_cor
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)
        type(excit_t) :: excit
        integer :: nocc_a, nocc_b, nunocc_a, nunocc_b
        integer :: e_sign

        integer :: i

        nocc_a = sys%occ_a - sys%froz
        nocc_b = sys%occ_b - sys%froz
        nunocc_a = sys%orbs - sys%occ_a
        nunocc_b = sys%orbs - sys%occ_b

        if (.not. allocated(ext_cor%t2a)) &
            allocate(ext_cor%t2a(nunocc_a, nunocc_a, nocc_a, nocc_a))
        ext_cor%t2a = 0.0_p
        if (.not. allocated(ext_cor%t2b)) &
            allocate(ext_cor%t2b(nunocc_b, nunocc_a, nocc_b, nocc_a))
        ext_cor%t2b = 0.0_p
        if (.not. allocated(ext_cor%t2c)) &
            allocate(ext_cor%t2c(nunocc_b, nunocc_b, nocc_b, nocc_b))
        ext_cor%t2c = 0.0_p

        associate(from_a=>excit%from_a, from_b=>excit%from_b, &
                to_a=>excit%to_a, to_b=>excit%to_b)


            do i=1, ext_cor%doubles_nconf
                ! Compute excitation
                excit = get_excitation_spin_integrate(sys%nel, sys%basis, f_ref, ext_cor%doubles_conf(:,i))
                ! Get excitation sign
                e_sign = 1
                if (excit%perm) e_sign = -1

                ! Shift numbers to match the system's array


                from_a = from_a - sys%froz
                from_b = from_b - sys%froz
                to_a = to_a - sys%occ_a
                to_b = to_b - sys%occ_b

                select case (excit%nexcit_alpha)

                case(2)
                    ext_cor%t2a(to_a(2), to_a(1), from_a(2), from_a(1)) = &
                        ext_cor%doubles_proj(i) * e_sign

                case(1)
                    ext_cor%t2b(to_b(1), to_a(1), from_b(1), from_a(1)) = &
                        ext_cor%doubles_proj(i) * e_sign

                case(0)
                    ext_cor%t2c(to_b(2), to_b(1), from_b(2), from_b(1)) = &
                        ext_cor%doubles_proj(i) * e_sign

                end select

            enddo

        end associate

    end subroutine update_t2_cluster

    function check_exists(f_t4, c4_confs, cnt_c4) result(res)

        use bit_utils, only: count_set_bits
        use const, only: i0

        logical :: res
        integer(i0), intent(in) :: f_t4(:)
        integer(i0), allocatable, intent(in) :: c4_confs(:,:)
        integer, intent(in) :: cnt_c4

        integer :: i
        integer :: level

        res = .false.
        do i=1, cnt_c4

            level = sum(count_set_bits(ieor(f_t4, c4_confs(:,i))))
            if (level == 0) then
                res = .true.
                return
            endif

        enddo

    end function check_exists

    function check_spin(excit, r) result(res)

        use excitations, only: excit_t

        logical :: res
        type(excit_t), intent(in) :: excit
        integer, intent(in) :: r

        integer :: spin_elecs, spin_virts
        integer :: i

        spin_elecs = 0
        spin_virts = 0
        do i=1, r
            spin_elecs = spin_elecs + mod(excit%from_orb(i), 2)
            spin_virts = spin_virts + mod(excit%to_orb(i), 2)
        enddo

        if (spin_elecs /= spin_virts) then
            res = .false.
        else
            res = .true.
        endif

    end function check_spin

    function check_sym(excit, r) result(res)

        use excitations, only: excit_t
        use symmetry, only: is_sym

        logical :: res
        type(excit_t), intent(in) :: excit
        integer, intent(in) :: r

        integer :: i
        integer :: ex_orbs(8)


        do i=1, r
            ex_orbs(i) = int((excit%from_orb(i) + 1) / 2)
        enddo
        do i=r+1, 2*r
            ex_orbs(i) = int((excit%to_orb(i-r) + 1) / 2)
        enddo

        res = is_sym(ex_orbs, 2*r)

    end function check_sym

end module process_t4
