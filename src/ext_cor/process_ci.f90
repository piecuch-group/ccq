module process_ci

    ! Module for proccesing CI vectors
    implicit none

contains

    subroutine find_fciqmc_c3(sys, f_ref, filename, c_vec, coef_norm)

        ! Translate configurations and walker populations to excitations and
        ! intermediately-renormalized CI coefficients
        !
        ! In:
        !   sys: system information
        !   f_ref: refernce determinant in bitform (see HANDE)
        !   filename: file containing FCIQMC walkers per determinant
        ! In/Out:
        !   c_vec: arrays containing spin-integrated CI coefficients obtained from FCIQMC
        !   coef_norm: normalization coefficient (is equal to the amount of walkers on the
        !              HF determinant.

        use const, only: dp, walk_unit, i0
        use determinants, only: encode_det
        use excitations, only: excit_t, get_excitation_spin_integrate, get_excitation_level
        use ext_cor_types, only: vec3_t
        use printing, only: abort_cc
        use system, only: sys_t

        ! Interface variables
        type(sys_t), intent(in) :: sys
        character(len=*), intent(in) :: filename
        type(vec3_t), intent(inout) :: c_vec
        real(dp), intent(inout) :: coef_norm
        integer(i0), intent(in)  :: f_ref(sys%basis%string_len)

        ! Temporary walker determinant holder
        integer(i0) :: f_walk(sys%basis%string_len)

        ! Aux variables
        type(excit_t) :: excit
        integer :: excit_rank
        real(dp) :: coef
        integer :: ios
        integer :: line(2)
        integer :: occ_list(sys%nel)

        logical :: t_exists

        character(len=30) :: deb_fmt

        coef_norm = 0.0d0

        ! Loop over determinants in the walker file
        inquire(file=trim(filename), exist=t_exists)
        if (.not. t_exists) call abort_cc("RUNTIME ERROR: Walker file not found")

        open(walk_unit, file=trim(filename), status='old')
        do
            read(walk_unit, *, iostat=ios) line, occ_list
            if (ios /= 0) exit

            ! Encode occupation list into bitform representation
            call encode_det(sys%basis, occ_list, f_walk)
            excit_rank = get_excitation_level(f_ref, f_walk)
            !print '(8i4)', excit_rank, occ_list, line(2)

            ! We are not interested in higher-than-quadruply excited determinants
            if (excit_rank >= 4) cycle

            ! Get excitation (from HF) and spin integrate
            excit = get_excitation_spin_integrate(sys%nel, sys%basis, f_ref, f_walk)
            ! Check permutation sign. This part is tricky.
            if (excit%perm) then
                coef = -real(line(2), dp)
            else
                coef = real(line(2), dp)
            endif

            associate(excit_rank_a=>excit%nexcit_alpha, from_a=>excit%from_a, from_b=>excit%from_b, &
                    to_a=>excit%to_a, to_b=>excit%to_b)

                ! Debug

                !if (excit_rank /= 0) then
                !    write(deb_fmt, '(a,i0,a)') '(', 2*excit_rank, 'i4, f18.10)'
                !    print deb_fmt, excit%from_orb(1:excit_rank), excit%to_orb(1:excit_rank), coef
                !endif
                !print '(6i4,f18.2,l8)', occ_list, coef, excit%perm

                ! Sort coefficients by spin case
                select case (excit_rank)
                case (0)
                    ! HF determinant
                    coef_norm = coef

                case (1)
                    ! All singles
                    if (excit_rank_a == 1) then
                        c_vec%o1_a(to_a(1), from_a(1)) = coef
                    else
                        c_vec%o1_b(to_b(1), from_b(1)) = coef
                    endif

                case (2)
                    ! All doubles
                    if (excit_rank_a == 2) then
                        c_vec%o2_aa(to_a(1), to_a(2), from_a(1), from_a(2)) = coef
                    elseif (excit_rank_a == 1) then
                        c_vec%o2_ab(to_a(1), to_b(1), from_a(1), from_b(1)) = coef
                    else
                        c_vec%o2_bb(to_b(1), to_b(2), from_b(1), from_b(2)) = coef
                    endif

                case(3)
                    ! All triples
                    if (excit_rank_a == 3) then
                        c_vec%o3_aaa(to_a(1), to_a(2), to_a(3), from_a(1), from_a(2), from_a(3)) = coef
                    elseif (excit_rank_a == 2) then
                        c_vec%o3_aab(to_a(1), to_a(2), to_b(1), from_a(1), from_a(2), from_b(1)) = coef
                    elseif (excit_rank_a == 1) then
                        c_vec%o3_abb(to_a(1), to_b(1), to_b(2), from_a(1), from_b(1), from_b(2)) = coef
                    else
                        c_vec%o3_bbb(to_b(1), to_b(2), to_b(3), from_b(1), from_b(2), from_b(3)) = coef
                    endif

                    ! We are skipping quadruples becasue they are too expensive.
                    ! We will compute them in a different way in the next section.

                end select
            end associate

        enddo
        close(walk_unit)

        ! Check whether we have the HF coefficient. Otherwise exit with error.
        if (coef_norm == 0.0d0) then
            call abort_cc('RUNTIME ERROR: Hartree-Fock coefficient not found in routine: find_fciqmc_c3')
        endif

        ! Renormalize coeffcients to the intermediate renormalization (useful for CC)
        c_vec%o1_a = c_vec%o1_a / coef_norm
        c_vec%o1_b = c_vec%o1_b / coef_norm
        c_vec%o2_aa = c_vec%o2_aa / coef_norm
        c_vec%o2_ab = c_vec%o2_ab / coef_norm
        c_vec%o2_bb = c_vec%o2_bb / coef_norm
        c_vec%o3_aaa = c_vec%o3_aaa / coef_norm
        c_vec%o3_aab = c_vec%o3_aab / coef_norm
        c_vec%o3_abb = c_vec%o3_abb / coef_norm
        c_vec%o3_bbb = c_vec%o3_bbb / coef_norm

    end subroutine find_fciqmc_c3

end module process_ci
