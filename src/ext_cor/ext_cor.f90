module external_correction

    ! Module that deals with externally corrected coupled-cluster
    ! methods. It is based on reading a FCI wave function, computed
    ! with an external method, and producing T3 and T4 to be
    ! subsequently introduced in CCSD-like equations.

    implicit none

contains

    subroutine external_correction_driver(sys, run, cc)

        ! Generates CC vector with ampltitudes obtained from an external method. This routine is the main driver of external
        ! correction feature.

        ! In:
        !   sys: system information including molecular data, integrals, etc.
        !   run: runtime options

        ! In/Out:
        !   cc: coupled-cluster information. On exit, cc contains and updated t vector (cc%t_vec)
        !       and all cc%ext_cor variables populated

        use const, only: p, dp, i0
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use printing, only: io, print_date

        use determinants, only: gen_f_ref
        use energy, only: calculate_unsorted_energy

        use contract_t3, only: drive_t3_contraction
        use contract_t4, only: process_fciqmc_c4, process_fciqmc_c4_h5, &
            update_t2_cluster, drive_t4_contraction

        use ext_cor_types, only: vec3_t, alloc_vec3_t, dealloc_vec3_t, alloc_out_arrays
        use cluster_analysis, only: cluster_analysis_up_t3

        use hdf5_io, only: write_ext_cor_vecs, read_popsfile_h5
        use utils, only: get_wall_time

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(in out) :: cc

        type(vec3_t) :: c_vec, t_vec
        integer(i0) :: f_ref(sys%basis%string_len)

        real(p) :: coef_norm

        real(dp) :: start_time, end_time

        integer(i0), allocatable :: dets(:,:)
        real(p), allocatable :: coefs(:,:)
        integer :: list_size


        ! Initialize data required for the cluster analysis
        call gen_f_ref(sys, f_ref)

        ! Starting cluster analysis
        write(io, '(a)') 'Cluster analysis'
        write(io, '(a)') '----------------'
        call print_date('  cluster analysis started on:')

        ! Allocate C and T vectors up to three body terms
        write(io, '(2x,a)') '=> Starting cluster analysis up to three body terms'
        write(io, '(4x,a)') '=> Initializing C and T arrays'
        call alloc_vec3_t(sys, c_vec)
        call alloc_vec3_t(sys, t_vec)

        ! Read FCIQMC coefficients of up to triply excited determinants
        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations on:"
        write(io, '(8x,a)') trim(run%ext_cor_file)

        if (run%ext_cor_file_h5) then
            call read_popsfile_h5(run%ext_cor_file, dets, coefs, list_size)
            call parse_fciqmc_c3_h5(sys, f_ref, dets, coefs, list_size, c_vec, coef_norm)
        else
            call parse_fciqmc_c3(sys, f_ref, run%ext_cor_file, c_vec, coef_norm)
        endif

        ! Cluster analyze up to three body components
        write(io, '(4x,a)') "=> Starting cluster analysis"
        ! [TODO] decide whether to allow RHF
        call cluster_analysis_up_t3(sys, c_vec, t_vec, .false.)
        ! T1 and C1 have a one to one mapping
        t_vec%o1_a = c_vec%o1_a
        t_vec%o1_b = c_vec%o1_b
        call dealloc_vec3_t(c_vec)

        ! Copy the new T vector into the CC vector for the next calculation
        write(io, '(4x,a/)') "=> Writing amplitudes"
        call write_up_t3(sys, cc, t_vec, run%ext_cor_sd)
        call dealloc_vec3_t(t_vec)
        ! Save T2 MC for DCMC calculations
        cc%acc%t2_mc = cc%t_vec(cc%pos(3):cc%pos(6)-1)



        ! T3 and T4 contractions
        ! ----------------------

        ! Initialize output contracted arrays from the external correction.
        ! These will hold all the information from the projections of the
        ! contractions between T3 and T4 on singles and doubles.
        call alloc_out_arrays(sys, cc%ext_cor)

        ! Contract T3 with H and generate intermediates for
        ! the <ijab| (HT1T3)C |phi> terms
        write(io, '(2x,a)') &
             '=> Generating <ia|[H_N,T3]|phi> and <ijab|[H_N,T3]|phi> intermediates'
        start_time = get_wall_time()
        call drive_t3_contraction(sys, cc)
        end_time = get_wall_time()
        write(io, '(4x,a,f10.2,a/)') '=> This step took', end_time - start_time, ' seconds'


        ! Generate T4 and contract with V on the fly
        write(io, '(2x,a)') '=> Generating <ijab|[V_N,T4]|phi> intermediate'
        if (run%ext_cor_file_h5) then
            call process_fciqmc_c4_h5(sys, cc, f_ref, dets, coefs, list_size, coef_norm)
        else
            call process_fciqmc_c4(sys, cc, f_ref, run%ext_cor_file, coef_norm)
        endif
        call update_t2_cluster(sys, cc%ext_cor, f_ref)

        write(io, '(4x,a)') '=> Contracting non-linear terms'
        start_time = get_wall_time()
        call drive_t4_contraction(sys, cc)
        end_time = get_wall_time()
        write(io, '(4x,a,f10.2,a)') '=> Contraction took', end_time - start_time, ' seconds'



        ! Write externally corrected methods to HDF5, just in case
        write(io, '(4x,a)') '=> Writing externally corrected data to HDF5'
        call write_ext_cor_vecs(run, cc)

        ! Calculate externally corrected correlation energy
        cc%en_cor = calculate_unsorted_energy(sys, cc)
        write(io, '(/2x,a27,2x,f16.10/)') 'External correlation energy', cc%en_cor

        ! End of cluster analysis driven external correction
        call print_date('  cluster analysis ended on:')
        write(io, '(a)') ''

    end subroutine external_correction_driver

    subroutine parse_fciqmc_c3(sys, f_ref, filename, c_vec, coef_norm)

        ! Translate configurations and walker populations to excitations and
        ! intermediately-renormalized CI coefficients

        ! In:
        !   sys: system information
        !   f_ref: refernce determinant in bitform (see HANDE)
        !   filename: file containing FCIQMC walkers per determinant

        ! In/Out:
        !   c_vec: arrays containing spin-integrated CI coefficients obtained from FCIQMC
        !   coef_norm: normalization coefficient (is equal to the amount of walkers on the
        !              HF determinant.

        use const, only: p, walk_unit, i0
        use system, only: sys_t

        use determinants, only: encode_det
        use excitations, only: excit_t, get_excitation_spin_integrate, get_excitation_level, &
            shift_occ_list
        use ext_cor_types, only: vec3_t

        use errors, only: stop_all
        use cc_utils, only: antisymmetrize

        ! Interface variables
        type(sys_t), intent(in) :: sys
        character(len=*), intent(in) :: filename
        type(vec3_t), intent(inout) :: c_vec
        real(p), intent(inout) :: coef_norm
        integer(i0), intent(in)  :: f_ref(sys%basis%string_len)

        ! Temporary walker determinant holder
        integer(i0) :: f_walk(sys%basis%string_len)

        ! Aux variables
        type(excit_t) :: excit
        integer :: excit_rank
        real(p) :: coef
        real(p) :: read_coef
        integer :: ios
        integer :: c_id
        integer :: occ_list(sys%nel)

        logical :: t_exists


        coef_norm = 0.0_p

        ! Loop over determinants in the walker file
        inquire(file=trim(filename), exist=t_exists)
        if (.not. t_exists) call stop_all('find_fciqmc_c3', "RUNTIME ERROR: Walker file not found")

        open(walk_unit, file=trim(filename), status='old')
        do
            read(walk_unit, *, iostat=ios) c_id, read_coef, occ_list(1:sys%nel-sys%froz*2)
            if (ios /= 0) exit

            ! Encode occupation list into bitform representation
            call shift_occ_list(sys%froz*2, sys%nel, occ_list)
            call encode_det(sys%basis, occ_list, f_walk)
            excit_rank = get_excitation_level(f_ref, f_walk)
            !print '(8i4)', excit_rank, occ_list, line(2)

            ! We are not interested in higher-than-quadruply excited determinants
            if (excit_rank >= 4) cycle

            ! Get excitation (from HF) and spin integrate
            excit = get_excitation_spin_integrate(sys%nel, sys%basis, f_ref, f_walk)
            ! Check permutation sign. This part is tricky.
            if (excit%perm) then
                coef = -real(read_coef, p)
            else
                coef = real(read_coef, p)
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
            call stop_all('find_fciqmc_c3', 'RUNTIME ERROR: Hartree-Fock coefficient not found in routine: find_fciqmc_c3')
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

        call antisymmetrize(sys, c_vec)

    end subroutine parse_fciqmc_c3


    subroutine parse_fciqmc_c3_h5(sys, f_ref, dets_in, coefs_in, list_size, c_vec, coef_norm)

        ! Translate configurations and walker populations to excitations and
        ! intermediately-renormalized CI coefficients

        ! In:
        !   sys: system information
        !   f_ref: refernce determinant in bitform (see HANDE)
        !   filename: file containing FCIQMC walkers per determinant

        ! In/Out:
        !   c_vec: arrays containing spin-integrated CI coefficients obtained from FCIQMC
        !   coef_norm: normalization coefficient (is equal to the amount of walkers on the
        !              HF determinant.

        use const, only: p, walk_unit, i0
        use system, only: sys_t

        use determinants, only: encode_det
        use excitations, only: excit_t, get_excitation_spin_integrate, get_excitation_level, &
            shift_occ_list
        use ext_cor_types, only: vec3_t

        use errors, only: stop_all
        use cc_utils, only: antisymmetrize

        ! Interface variables
        type(sys_t), intent(in) :: sys
        integer(i0), intent(in)  :: f_ref(sys%basis%string_len)
        integer(i0), intent(in) :: dets_in(:,:)
        real(p), intent(in) :: coefs_in(:,:)
        integer, intent(in) :: list_size
        type(vec3_t), intent(inout) :: c_vec
        real(p), intent(inout) :: coef_norm

        ! Temporary walker determinant holder
        integer(i0) :: f_walk(sys%basis%string_len)

        ! Aux variables
        type(excit_t) :: excit
        integer :: excit_rank
        real(p) :: coef
        real(p) :: read_coef

        integer :: idx


        coef_norm = 0.0_p

        ! Loop over determinants in the walker file
        do idx=1, list_size

            f_walk = dets_in(:, idx)
            read_coef = coefs_in(1, idx)

            excit_rank = get_excitation_level(f_ref, f_walk)

            ! We are not interested in higher-than-quadruply excited determinants
            if (excit_rank >= 4) cycle

            ! Get excitation (from HF) and spin integrate
            excit = get_excitation_spin_integrate(sys%nel, sys%basis, f_ref, f_walk)
            ! Check permutation sign. This part is tricky.
            if (excit%perm) then
                coef = -real(read_coef, p)
            else
                coef = real(read_coef, p)
            endif

            associate(excit_rank_a=>excit%nexcit_alpha, from_a=>excit%from_a, from_b=>excit%from_b, &
                    to_a=>excit%to_a, to_b=>excit%to_b)

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
            call stop_all('find_fciqmc_c3', 'RUNTIME ERROR: Hartree-Fock coefficient not found in routine: find_fciqmc_c3')
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

        call antisymmetrize(sys, c_vec)

    end subroutine parse_fciqmc_c3_h5

    subroutine write_up_t3(sys, cc, t_vec, print_doubles)

        ! Write the CC files required for externally corrected CC calculations.

        ! In:
        !   sys: system information
        !   t_vec: cluster analyzed amplitudes up to triples
        !   print_doubles: if true writes down single and double amplitudes as well

        ! In/Out:
        !   cc: coupled-cluster information with updated T vector (cc%t_vec) amplitudes

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t
        use ext_cor_types, only: vec3_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout), target :: cc
        type(vec3_t), intent(in) :: t_vec
        logical, intent(in) :: print_doubles


        integer :: i, j, k
        integer :: a, b, c
        real(p), pointer :: t3(:) => null()

        integer :: indx

        t3 => cc%t_vec

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

            if (print_doubles) then
                indx = 1

                ! T1A
                do i=froz+1,occ_a
                    do a=occ_a+1,total
                        t3(indx) = t_vec%o1_a(a,i)
                        indx = indx + 1
                    enddo
                enddo

                ! T1B
                do i=froz+1,occ_b
                    do a=occ_b+1,total
                        t3(indx) = t_vec%o1_b(a,i)
                        indx = indx + 1
                    enddo
                enddo

                ! T2A
                do i=froz+1,occ_a
                    do j=froz+1,occ_a
                        do a=occ_a+1,total
                            do b=occ_a+1,total
                                t3(indx) = t_vec%o2_aa(a,b,i,j)
                                indx = indx + 1
                            enddo
                        enddo
                    enddo
                enddo

                ! T2B
                do i=froz+1,occ_a
                    do j=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_b+1,total
                                t3(indx) = t_vec%o2_ab(a,b,i,j)
                                indx = indx + 1
                            enddo
                        enddo
                    enddo
                enddo

                ! T2C
                do i=froz+1,occ_b
                    do j=froz+1,occ_b
                        do a=occ_b+1,total
                            do b=occ_b+1,total
                                t3(indx) = t_vec%o2_bb(a,b,i,j)
                                indx = indx + 1
                            enddo
                        enddo
                    enddo
                enddo

            else
                indx = cc%pos(6)
            endif

            ! T3A
            do i=froz+1,occ_a
                do j=froz+1,occ_a
                    do k=froz+1,occ_a
                        do a=occ_a+1,total
                            do b=occ_a+1,total
                                do c=occ_a+1,total
                                    t3(indx) = t_vec%o3_aaa(a,b,c,i,j,k)
                                    indx = indx + 1
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            ! T3B
            do i=froz+1,occ_a
                do j=froz+1,occ_a
                    do k=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_a+1,total
                                do c=occ_b+1,total
                                    t3(indx) = t_vec%o3_aab(a,b,c,i,j,k)
                                    indx = indx + 1
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            ! T3C
            do i=froz+1,occ_a
                do j=froz+1,occ_b
                    do k=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_b+1,total
                                do c=occ_b+1,total
                                    t3(indx) = t_vec%o3_abb(a,b,c,i,j,k)
                                    indx = indx + 1
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            ! T3D
            do i=froz+1,occ_b
                do j=froz+1,occ_b
                    do k=froz+1,occ_b
                        do a=occ_b+1,total
                            do b=occ_b+1,total
                                do c=occ_b+1,total
                                    t3(indx) = t_vec%o3_bbb(a,b,c,i,j,k)
                                    indx = indx + 1
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

        end associate

    end subroutine write_up_t3

end module external_correction
