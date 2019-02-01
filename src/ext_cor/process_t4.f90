module process_t4

    implicit none

contains

    subroutine gen_doubles_conf(sys, confs, f_ref)

        use const, only: i0
        use utils, only: combs
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
        integer :: i, j, k, l

        integer :: spin_elecs, spin_virts
        integer :: n_combs_elecs, n_combs_virts

        ! Set connection excitation level
        excit%nexcit = r

        ! Get electron combinations
        do i=1, sys%nel
            elecs(i) = i
        enddo
        call combs(elecs, r, combs_elecs)

        do i=1, sys%nvirt
            virts(i) = sys%nel + i
        enddo
        call combs(virts, r, combs_virts)

        n_combs_elecs = size(combs_elecs, 2)
        n_combs_virts = size(combs_virts, 2)

        allocate(tmp_confs(sys%basis%string_len, n_combs_elecs * n_combs_virts), stat=ierr)
        call check_allocate('tmp_confs', n_combs_elecs * n_combs_virts, ierr)

        ind_conf = 1
        do i=1, n_combs_elecs
            do j=1, n_combs_virts

                ! Check whether spin matches excitation
                spin_elecs = 0
                spin_virts = 0
                do k=1, r
                    spin_elecs = spin_elecs + mod(combs_elecs(k,i), 2)
                    spin_virts = spin_virts + mod(combs_virts(k,j), 2)
                enddo
                if (spin_elecs /= spin_virts) cycle

                ! Check symmetry
                do k=1, r
                    ex_orbs(k) = int((combs_elecs(k,i) + 1) / 2)
                enddo
                do k=r+1, 2*r
                    ex_orbs(k) = int((combs_virts(k-r,j) + 1) / 2)
                enddo
                if (.not. is_sym(ex_orbs, 2*r)) cycle

                excit%from_orb(1:r) = combs_elecs(:,i)
                excit%to_orb(1:r) = combs_virts(:,j)
                call create_excited_det(sys%basis, f_ref, excit, f_tmp)

                tmp_confs(:,ind_conf) = f_tmp
                ind_conf = ind_conf + 1

            enddo
        enddo

        ! Compact configurations array
        allocate(confs(sys%basis%string_len, ind_conf-1), stat=ierr)
        call check_allocate('confs', ind_conf - 1, ierr)
        confs(:,:) = tmp_confs(:,1:ind_conf-1)
        deallocate(tmp_confs)
        call check_deallocate('tmp_confs', ierr)


    end subroutine gen_doubles_conf

    subroutine contract_t4(sys, cc, f_ref, f_t4, c_vec, c4_coef, coef_norm)

        use const, only: i0, p
        use cluster_analysis, only: analyze_t4_aaaa, analyze_t4_aaab, analyze_t4_aabb, &
            analyze_t4_abbb, analyze_t4_bbbb
        use excitations, only: excit_t, get_excitation_spin_integrate
        use ext_cor_types, only: vec3_t
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)
        integer(i0), intent(in) :: f_t4(sys%basis%string_len)
        integer, intent(in) :: c4_coef
        real(p), intent(in) :: coef_norm
        type(vec3_t), intent(in) :: c_vec

        integer :: nconf

        integer :: ios
        type(excit_t) :: excit
        integer :: excit_sign
        real(p) :: renorm_coef
        real(p) :: t4_amp


        nconf = size(cc%ext_cor%doubles_conf, 2)
        excit = get_excitation_spin_integrate(sys%nel, sys%basis, f_ref, f_t4)
        if (excit%perm) then
            excit_sign = -1
        else
            excit_sign = 1
        endif
        renorm_coef =  real(c4_coef, p) / coef_norm * excit_sign

        select case(excit%nexcit_alpha)

        ! Obtain t4_amp from the cluster analysis of the Cs
        case(4)
            call analyze_t4_aaaa(c_vec, excit, renorm_coef, t4_amp)

        case(3)
            call analyze_t4_aaab(c_vec, excit, renorm_coef, t4_amp)

        case(2)
            call analyze_t4_aabb(c_vec, excit, renorm_coef, t4_amp)

        case(1)
            call analyze_t4_abbb(c_vec, excit, renorm_coef, t4_amp)

        case(0)
            call analyze_t4_bbbb(c_vec, excit, renorm_coef, t4_amp)

        end select

        ! Fix sign before the projection in Slater determinant space.
        ! As a note, this is kind of tricky. One has to think about the different
        ! references as you translate between excitation and Slater
        ! determinant space. In the former, the phase factors are calculated with
        ! respect to the structure of the of the excitation (e.g. |D_ijk^abc> where
        ! i<j<k and a<b<c and all indices are alpha spin or |D_ijk^abc> where i<j,k and
        ! a<b,c where i,j and a,b are alpha spin and k and c are beta spin) which means
        ! that one must pay attention to how the determinants are defined in the theory.

        ! In the later, the phase factors depend on how the occupation numbers are
        ! defined in the the determinant string (e.g. by numerical order or by an
        ! alpha-beta, alpha-beta, etc. structure).

        t4_amp = t4_amp * excit_sign

        if (t4_amp /= 0.0_p) then
            call update_proj_t2(sys, cc, f_t4, t4_amp)
        endif


    end subroutine contract_t4

    subroutine update_proj_t2(sys, cc, t4_conf, t4_amp)

        use const, only: p, i0
        use excitations, only: excit_t, get_excitation_level, get_excitation
        use hmat, only: get_v
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc
        integer(i0), intent(in) :: t4_conf(sys%basis%string_len)
        real(p), intent(in) :: t4_amp

        !  Local variables
        integer :: iconf
        integer :: i, j, a, b
        integer :: nexcit
        type(excit_t) :: excit
        real(p) :: h_element

        ! Read configurations
        associate(doubles_conf=>cc%ext_cor%doubles_conf, doubles_proj=>cc%ext_cor%doubles_proj)

            do iconf=1, size(doubles_conf,2)

                ! Cycle if not a double excitation. Only double excitations
                ! are allowed between doubly and quadruply excited determinants.
                nexcit = get_excitation_level(doubles_conf(:,iconf), t4_conf)
                if (nexcit /= 2) cycle

                ! Get excitation information
                excit = get_excitation(sys%nel, sys%basis, doubles_conf(:,iconf), t4_conf)

                ! Calculate matrix element. Note that only double excitations
                ! are allowed, thus only twobody integrals are needed (Slater rules).
                i = excit%from_orb(1)
                j = excit%from_orb(2)
                a = excit%to_orb(1)
                b = excit%to_orb(2)

                h_element = get_v(sys%ints%v_aa, sys%ints%v_ab, sys%ints%v_bb, i, j, a, b)

                ! Apply the appropriate permutation parity
                if (excit%perm) h_element = -h_element

                ! Update projection on doubles
                doubles_proj(iconf) = doubles_proj(iconf) + (h_element * t4_amp)
            enddo

        end associate

    end subroutine update_proj_t2

    subroutine update_t2_cluster(sys, ext_cor, f_ref)

        use const, only: p, i0
        use excitations, only: excit_t, get_excitation_spin_integrate
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


            do i=1, size(ext_cor%doubles_proj, 1)
                ! Compute excitation
                excit = get_excitation_spin_integrate(sys%nel, sys%basis, f_ref, ext_cor%doubles_conf(:,i))
                ! Get excitation sign
                e_sign = 1
                if (excit%perm) e_sign = -1

                ! Shift numbers to match the system's array
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

    subroutine find_disc_t4(sys, cc, f_ref, c4_cnt, c4_hash, c_vec)

        use const, only: i0, p
        use determinants, only: decode_det
        use excitations, only: excit_t, create_excited_det
        use ext_cor_types, only: vec3_t
        use system, only: sys_t
        use cc_types, only: cc_t
        use utils, only: next_comb

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc
        integer, intent(in) :: c4_cnt
        !integer(i0), allocatable, intent(in) :: c4_confs(:,:)
        type(dictionary_t), intent(in) :: c4_hash
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)
        type(vec3_t), intent(in) :: c_vec

        integer(i0) :: f_t4(sys%basis%string_len)
        type(excit_t) :: excit
        integer, parameter :: r = 4
        integer :: elecs(sys%nel-sys%froz*2), virts(sys%nvirt)
        integer :: inds_elecs(r), inds_virts(r)

        integer :: i
        logical :: done
        ! Debug
        integer :: cnt_eqs = 0
        integer :: cnt = 0
        logical :: t_spin, t_sym, t_exist
        integer :: occ_list(sys%nel)

        ! Set excitation rank
        excit%nexcit = r

        ! Set electrons array
        do i=1, sys%nel-sys%froz*2
            elecs(i) = i + sys%froz*2
        enddo

        ! Set virts array
        do i=1, sys%nvirt
            virts(i) = sys%nel + i
        enddo

        ! Init arrays
        do i=1, r
            inds_elecs(i) = i
        enddo

        elecs_loop: do

            do i=1, r
                inds_virts(i) = i
            enddo
            do i=1, r
                excit%from_orb(i) = elecs(inds_elecs(i))
            enddo

            virts_loop: do

                do i=1, r
                    excit%to_orb(i) = virts(inds_virts(i))
                enddo

                ! Check whether spin matches excitation
                ! Check symmetry
                t_spin = check_spin(excit, r)
                t_sym = check_sym(excit, r)
                if (t_spin .and. t_sym) then
                    ! Check existence
                    call create_excited_det(sys%basis, f_ref, excit, f_t4)
                    ! [TODO] replace check exists with something that doesn't have linear scaling (say hash table)
                    !t_exist = check_exists(f_t4, c4_confs, c4_cnt)
                    t_exist = c4_hash%get(f_t4) /= 0
                    if (.not. t_exist) then
                        ! Contract
                        !call decode_det(sys%basis, f_t4, occ_list)
                        !print '(6i4)', occ_list
                        call contract_t4(sys, cc, f_ref, f_t4, c_vec, 0, 1.0_p)
                        cnt = cnt + 1
                    endif
                endif

                ! Get next combination of virtual orbitals
                call next_comb(sys%nvirt, inds_virts, r, done)
                if (done) exit virts_loop
            enddo virts_loop

            ! Get next combination of occupied orbitals
            call next_comb(sys%nel-sys%froz*2, inds_elecs, r, done)
            if (done) exit elecs_loop
        enddo elecs_loop

        !print '(a,i15)', 'all quads', cnt

    end subroutine find_disc_t4


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
