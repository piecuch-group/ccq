module process_ci

    ! Module for proccesing CI vectors

    use const, only: dp
    use sys_data
    use symmetry
    use printing, only: abort_cc
    !use printing, only: print_conf_occupation, print_conf_number, abort_cc

    implicit none



contains

    subroutine gen_twobody_confs(confs)

        use combinatorics, only: combinations
        use symmetry, only: is_sym

        integer, allocatable, intent(out) :: confs(:,:)
        integer, allocatable :: elecs(:), virts(:)
        integer, allocatable :: combs_elecs(:,:), combs_virts(:,:)
        integer, allocatable :: tmp_confs(:,:)

        integer :: ex_orbs(8)
        integer :: ind_el, ind_conf
        integer :: i, j, k, l

        integer :: spin_elecs, spin_virts
        integer :: n_combs_elecs, n_combs_virts

        ! Get electron combinations
        allocate(elecs(nele))
        do i=1, nele
            elecs(i) = i
        enddo
        call combinations(elecs, 2, combs_elecs)

        allocate(virts(unocc))
        do i=1, unocc
            virts(i) = nele + i
        enddo
        call combinations(virts, 2, combs_virts)

        n_combs_elecs = size(combs_elecs, 2)
        n_combs_virts = size(combs_virts, 2)

        allocate(tmp_confs(nele, n_combs_elecs * n_combs_virts))

        ind_conf = 1
        do i=1, n_combs_elecs
            do j=1, n_combs_virts

                ! Check whether spin matches excitation
                spin_elecs = 0
                spin_virts = 0
                do k=1, 2
                    spin_elecs = spin_elecs + mod(combs_elecs(k,i), 2)
                    spin_virts = spin_virts + mod(combs_virts(k,j), 2)
                enddo
                if (spin_elecs /= spin_virts) cycle

                ! Check symmetry
                do k=1, 2
                    ex_orbs(k) = int((combs_elecs(k,i) + 1) / 2)
                enddo
                do k=3, 4
                    ex_orbs(k) = int((combs_virts(k-2,j) + 1) / 2)
                enddo
                if (.not. is_sym(ex_orbs, 4)) cycle

                ind_el = 1
                loop_el: do k=1,nele
                    do l=1, 2
                        if (combs_elecs(l,i) == k) cycle loop_el
                    enddo
                    tmp_confs(ind_el, ind_conf) = k
                    ind_el = ind_el + 1
                enddo loop_el
                tmp_confs(ind_el, ind_conf) = combs_virts(1,j)
                tmp_confs(ind_el + 1, ind_conf) = combs_virts(2,j)
                ind_conf = ind_conf + 1
            enddo
        enddo

        allocate(confs(nele, ind_conf-1))

        confs(:,:) = tmp_confs(:,1:ind_conf-1)

        deallocate(tmp_confs)


    end subroutine gen_twobody_confs


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
        use determinant, only: decode_determinant
        use ext_cor_types, only: excit_t

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

    subroutine find_disc_t4(cnt_c4, c4_confs, c_vec, ints, twobody_occ, twobody_proj)

        use ext_cor_types, only: c_vec_t
        use system, only: ints_t

        integer, intent(in) :: cnt_c4
        integer, intent(in) :: c4_confs(:,:)
        type(c_vec_t), intent(in) :: c_vec
        type(ints_t), intent(in) :: ints
        integer, allocatable, intent(in) :: twobody_occ(:,:)
        real(dp), allocatable, intent(inout) :: twobody_proj(:)

        integer, allocatable :: elecs(:)
        integer, parameter :: r = 4
        integer :: i, j, k, l, m, indx
        integer :: n_combs
        integer :: inds_elecs(r), inds_virts(r)

        ! Debug
        integer :: cnt_eqs = 0

        !n_combs_elecs = ncr(nele, r)
        !n_combs_virts = ncr(unocc, r)

        ! Set electrons array
        allocate(elecs(nele))
        do i=1, nele
            elecs(i) = i
        enddo


        do i=1, r
            inds_elecs(i) = i
        enddo

        !print '(/a/)', 'Start disc'
        !open(997, file='test_4_deters', status='unknown')

        call loop_virts_4(cnt_c4, c4_confs, c_vec, ints, twobody_occ, twobody_proj, &
            elecs, inds_elecs, r)


        elecs_loop: do
            do i=r, 1, -1
                if (inds_elecs(i) /= i + nele - r) exit
                if (i == 1) exit elecs_loop
            enddo
            inds_elecs(i) = inds_elecs(i) + 1
            do j=i+1, r
                inds_elecs(j) = inds_elecs(j-1) + 1
            enddo
            !print *, 'elecs loop', i

            call loop_virts_4(cnt_c4, c4_confs, c_vec, ints, twobody_occ, twobody_proj, &
                elecs, inds_elecs, r)


        enddo elecs_loop

        !close(997)

        !print '(a,2i12)', 'cnt_eqs, cnt_c4', cnt_eqs, cnt_c4
        deallocate(elecs)

    end subroutine find_disc_t4

    subroutine loop_virts_4(cnt_c4, c4_confs, c_vec, ints, twobody_occ, twobody_proj, &
            elecs, inds_elecs, r)

        use ext_cor_types, only: c_vec_t
        use system, only: ints_t

        integer, intent(in) :: cnt_c4
        integer, intent(in) :: c4_confs(:,:)
        type(c_vec_t), intent(in) :: c_vec
        type(ints_t), intent(in) :: ints
        integer, allocatable, intent(in) :: twobody_occ(:,:)
        real(dp), allocatable, intent(inout) :: twobody_proj(:)
        integer, allocatable, intent(in) :: elecs(:)
        integer, intent(in) :: inds_elecs(:)
        integer, intent(in) :: r

        integer, allocatable :: virts(:), tmp_conf(:)
        integer :: inds_virts(r)

        integer :: i, j
        logical :: t_spin, t_sym, is_c4, t_exists

        ! Debug
        integer :: cnt_eqs

        ! Set virtuals array
        allocate(virts(unocc))
        do i=1, unocc
            virts(i) = nele + i
        enddo

        do i=1, r
            inds_virts(i) = i
        enddo

        allocate(tmp_conf(nele))

        ! Check if already exists
        call elec_virt_to_conf(elecs, inds_elecs, virts, inds_virts, r, tmp_conf)
        ! Check whether spin matches excitation
        t_spin = check_spin(elecs, inds_elecs, virts, inds_virts, r)
        ! Check symmetry
        t_sym = check_sym(elecs, inds_elecs, virts, inds_virts, r)

        t_exists = check_exists(cnt_c4, c4_confs, tmp_conf)
        !if (t_exists) cnt_eqs = cnt_eqs + 1

        if (.not. t_exists .and. t_spin .and. t_sym) then
            ! Contract
            call contract_t4(0, tmp_conf, 1.0_dp, c_vec, ints, twobody_occ, twobody_proj, is_c4)
        endif

        virts_loop: do
            do i=r, 1, -1
                if (inds_virts(i) /= i + unocc - r) exit
                if (i == 1) exit virts_loop
            enddo
            inds_virts(i) = inds_virts(i) + 1
            do j=i+1, r
                inds_virts(j) = inds_virts(j-1) + 1
            enddo

            call elec_virt_to_conf(elecs, inds_elecs, virts, inds_virts, r, tmp_conf)
            !print *, tmp_conf

            ! Check whether spin matches excitation
            t_spin = check_spin(elecs, inds_elecs, virts, inds_virts, r)
            if (.not. t_spin) cycle virts_loop

            ! Check symmetry
            t_sym = check_sym(elecs, inds_elecs, virts, inds_virts, r)
            if (.not. t_sym) cycle virts_loop

            ! Check if already exists
            t_exists = check_exists(cnt_c4, c4_confs, tmp_conf)
            !write(997, '(6i4)') tmp_conf

            if (t_exists) cycle virts_loop

            ! Contract
            call contract_t4(0, tmp_conf, 1.0_dp, c_vec, ints, twobody_occ, twobody_proj, is_c4)

        enddo virts_loop

        deallocate(tmp_conf, virts)

    end subroutine loop_virts_4

    subroutine elec_virt_to_conf(elecs, inds_elecs, virts, inds_virts, r, tmp_conf)

        integer, intent(in) :: elecs(:), virts(:)
        integer, intent(in) :: inds_elecs(:), inds_virts(:)
        integer, intent(in) :: r
        integer, intent(inout) :: tmp_conf(:)

        integer :: ind_el, i, j


        ind_el = 1
        loop_el: do i=1,nele
            do j=1, r
                if (elecs(inds_elecs(j)) == i) cycle loop_el
            enddo
            tmp_conf(ind_el) = i
            ind_el = ind_el + 1
        enddo loop_el

        do i=1, r
            tmp_conf(ind_el) = virts(inds_virts(i))
            ind_el = ind_el + 1
        enddo


    end subroutine elec_virt_to_conf

    function check_exists(cnt_c4, c4_confs, tmp_conf) result(res)
        integer, intent(in) :: cnt_c4
        integer, intent(in) :: c4_confs(:,:)
        integer, intent(in) :: tmp_conf(:)

        integer :: i, j
        logical :: res
        logical :: tmp_check

        res = .false.
        do i=1, cnt_c4
            tmp_check = .true.
            do j=1, size(tmp_conf)
                if (tmp_conf(j) /= c4_confs(j,i)) then
                    tmp_check = .false.
                    exit
                endif
            enddo
            if (tmp_check) then
                res = .true.
                return
            endif
        enddo

    end function check_exists

    function check_spin(elecs, inds_elecs, virts, inds_virts, r) result(res)

        integer, intent(in) :: elecs(:), virts(:)
        integer, intent(in) :: inds_elecs(:), inds_virts(:)
        integer, intent(in) :: r

        integer :: spin_elecs, spin_virts
        integer :: i

        logical :: res

        spin_elecs = 0
        spin_virts = 0
        do i=1, r
            spin_elecs = spin_elecs + mod(elecs(inds_elecs(i)), 2)
            spin_virts = spin_virts + mod(virts(inds_virts(i)), 2)
        enddo

        if (spin_elecs /= spin_virts) then
            res = .false.
        else
            res = .true.
        endif

    end function check_spin

    function check_sym(elecs, inds_elecs, virts, inds_virts, r) result(res)

        integer, intent(in) :: elecs(:), virts(:)
        integer, intent(in) :: inds_elecs(:), inds_virts(:)
        integer, intent(in) :: r

        integer :: spin_elecs, spin_virts
        integer :: i
        integer :: ex_orbs(8)

        logical :: res

        do i=1, r
            ex_orbs(i) = int((elecs(inds_elecs(i)) + 1) / 2)
        enddo
        do i=r+1, 2*r
            ex_orbs(i) = int((virts(inds_virts(i-r)) + 1) / 2)
        enddo

        res = is_sym(ex_orbs, 2*r)

    end function check_sym

    subroutine get_ext_cor_4(filename, coef_norm, c_vec, ints, twobody_confs, twobody_proj)
        use determinant, only: decode_determinant
        use directed_t4_analysis, only: analyze_t4_aaaa, analyze_t4_aaab, analyze_t4_aabb, &
            analyze_t4_abbb, analyze_t4_bbbb
        use hmatrix, only: num_confs_to_occs, update_proj_t2
        use ext_cor_types, only: c_vec_t, excit_t
        use printing, only: io
        use system, only: ints_t

        character(len=*), intent(in) :: filename
        real(dp), intent(in) :: coef_norm
        type(c_vec_t), intent(in) :: c_vec
        type(ints_t), intent(in) :: ints
        integer, allocatable, intent(out) :: twobody_confs(:,:)
        real(dp), allocatable, intent(out) :: twobody_proj(:)

        integer, parameter :: init_c4 = 100000
        integer, allocatable :: c4_confs(:,:)
        integer, allocatable :: twobody_occ(:,:)

        integer :: nconf
        integer :: line(2), c4_coef
        integer :: c4_conf(nele)
        integer :: cnt_c4 = 0
        integer :: ios
        logical :: is_c4

        allocate(c4_confs(nele, init_c4))

        write(io, '(4x,a)') '=> Creating twobody configurations'
        call gen_twobody_confs(twobody_confs)
        nconf = size(twobody_confs, 2)
        call num_confs_to_occs(twobody_confs, nconf, nsorb, nele, twobody_occ)

        allocate(twobody_proj(nconf))
        twobody_proj = 0.0_dp

        ! Run over all alpha combinations
        open(unit=105, file=trim(filename), status='old')

        write(io, '(4x,a)') '=> Starting loop over stochastic quadruply excited determinants'
        do
            read(105, *, iostat=ios) line, c4_conf
            if (ios /= 0) exit

            c4_coef = line(2)

            call contract_t4(c4_coef, c4_conf, coef_norm, c_vec, ints, twobody_occ, twobody_proj, is_c4)
            if (is_c4) then
                cnt_c4 = cnt_c4 + 1
                c4_confs(:,cnt_c4) = c4_conf
            endif
        enddo

        close(105)

        write(io, '(4x,a)') '=> Starting loop over disconnected quadruply excited determinants'
        call find_disc_t4(cnt_c4, c4_confs, c_vec, ints, twobody_occ, twobody_proj)

    end subroutine get_ext_cor_4

    !subroutine contract_t4(filename, coef_norm, c_vec, ints, twobody_confs, twobody_proj)
    subroutine contract_t4(c4_coef, c4_conf, coef_norm, c_vec, ints, twobody_occ, twobody_proj, is_c4)

        use determinant, only: decode_determinant
        use directed_t4_analysis, only: analyze_t4_aaaa, analyze_t4_aaab, analyze_t4_aabb, &
            analyze_t4_abbb, analyze_t4_bbbb
        use hmatrix, only: update_proj_t2
        use ext_cor_types, only: c_vec_t, excit_t
        use system, only: ints_t

        integer, intent(in) :: c4_coef
        integer, intent(in) :: c4_conf(:)
        real(dp), intent(in) :: coef_norm
        type(c_vec_t), intent(in) :: c_vec
        type(ints_t), intent(in) :: ints
        integer, allocatable, intent(in) :: twobody_occ(:,:)
        real(dp), allocatable, intent(inout) :: twobody_proj(:)
        logical, intent(inout) :: is_c4

        integer :: nconf

        integer :: ios
        type(excit_t) :: excitation
        integer :: to(4), from(4)
        integer :: excit_rank
        real(dp) :: renorm_coef
        real(dp) :: t4
        !real(dp) :: coef_sign


        !print '(6i4)', c4_conf
        !print '(i4)', c4_coef
        call decode_determinant(c4_coef, c4_conf, excitation)
        nconf = size(twobody_occ, 2)

        associate(excit_rank_a=>excitation%rank_a, coef=>excitation%coef, &
                to_a=>excitation%to_a, to_b=>excitation%to_b, &
                from_a=>excitation%from_a, from_b=>excitation%from_b)

            !coef_sign = coef / real(c4_coef, dp)
            !coef_sign = 1.0_dp

            excit_rank = excit_rank_a + excitation%rank_b
            if (excit_rank == 4) then

                is_c4 = .true.

                renorm_coef =  coef / coef_norm
                if (excit_rank_a == 4) then
                    to = to_a
                    from = from_a
                    call analyze_t4_aaaa(c_vec, from, to, renorm_coef, t4)

                elseif (excit_rank_a == 3) then
                    to(1:3) = to_a(1:3)
                    from(1:3) = from_a(1:3)
                    to(4) = to_b(1)
                    from(4) = from_b(1)
                    call analyze_t4_aaab(c_vec, from, to, renorm_coef, t4)

                elseif (excit_rank_a == 2) then
                    to(1:2) = to_a(1:2)
                    from(1:2) = from_a(1:2)
                    to(3:4) = to_b(1:2)
                    from(3:4) = from_b(1:2)
                    call analyze_t4_aabb(c_vec, from, to, renorm_coef, t4)

                elseif (excit_rank_a == 1) then
                    to(1) = to_a(1)
                    from(1) = from_a(1)
                    to(2:4) = to_b(1:3)
                    from(2:4) = from_b(1:3)
                    call analyze_t4_abbb(c_vec, from, to, renorm_coef, t4)

                elseif (excit_rank_a == 0) then
                    to = to_b
                    from = from_b
                    call analyze_t4_bbbb(c_vec, from, to, renorm_coef, t4)

                endif

                t4 = t4 * excitation%excit_sign
                !print *, renorm_coef, t4

                if (t4 /= 0.0_dp) then

                    call update_proj_t2(nele, nsorb, ints, &
                        nconf, twobody_occ, &
                        c4_conf, t4, &
                        twobody_proj)
                endif

            else
                is_c4 = .false.
            endif

        end associate

    end subroutine contract_t4

    subroutine find_fciqmc_c4_aaaa(filename, config_debug, coef_norm, c4_aaaa)

        ! Translate configurations to excitations for FCIQMC coefficients
        !
        ! In:
        !   filename: file containing FCIQMC walkers per determinant
        !   config_debug: if true, write configurations in occupation form
        ! In/Out:
        !   c*: arrays containing the spin-integrated coefficients based on excitation rank

        use determinant, only: decode_determinant
        use ext_cor_types, only: excit_t

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
        use determinant, only: decode_determinant
        use ext_cor_types, only: excit_t
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
        use determinant, only: decode_determinant
        use ext_cor_types, only: excit_t
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
        use determinant, only: decode_determinant
        use ext_cor_types, only: excit_t
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
        use determinant, only: decode_determinant
        use ext_cor_types, only: excit_t
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


end module process_ci
