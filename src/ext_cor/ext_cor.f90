module external_correction

    implicit none

contains

    subroutine alloc_vec3_t(sys, vec3)

        ! Initialize a vector with up to three-body components

        ! In:
        !    sys: system information
        ! In/Out:
        !    vec3: vector with up to three-body components

        use const, only: p
        use ext_cor_types, only: vec3_t
        use system, only: sys_t

        type(sys_t), intent(in) :: sys
        type(vec3_t), intent(inout) :: vec3

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

            allocate(vec3%o1_a(occ_a+1:total,froz+1:occ_a))
            allocate(vec3%o1_b(occ_b+1:total,froz+1:occ_b))

            allocate(vec3%o2_aa(occ_a+1:total,occ_a+1:total,froz+1:occ_a,froz+1:occ_a))
            allocate(vec3%o2_ab(occ_a+1:total,occ_b+1:total,froz+1:occ_a,froz+1:occ_b))
            allocate(vec3%o2_bb(occ_b+1:total,occ_b+1:total,froz+1:occ_b,froz+1:occ_b))

            allocate(vec3%o3_aaa(occ_a+1:total,occ_a+1:total,occ_a+1:total, &
                froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
            allocate(vec3%o3_aab(occ_a+1:total,occ_a+1:total,occ_b+1:total, &
                froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
            allocate(vec3%o3_abb(occ_a+1:total,occ_b+1:total,occ_b+1:total, &
                froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
            allocate(vec3%o3_bbb(occ_b+1:total,occ_b+1:total,occ_b+1:total, &
                froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))

            vec3%o1_a = 0.0_p
            vec3%o1_b = 0.0_p
            vec3%o2_aa = 0.0_p
            vec3%o2_ab = 0.0_p
            vec3%o2_bb = 0.0_p
            vec3%o3_aaa = 0.0_p
            vec3%o3_aab = 0.0_p
            vec3%o3_abb = 0.0_p
            vec3%o3_bbb = 0.0_p

        end associate

    end subroutine alloc_vec3_t

    subroutine dealloc_vec3_t(vec3)

        ! Deallocate a vector with up to three-body components

        ! In/Out:
        !    vec3: vector with up to three-body components

        use ext_cor_types, only: vec3_t

        type(vec3_t), intent(inout) :: vec3

        deallocate(vec3%o1_a)
        deallocate(vec3%o1_b)

        deallocate(vec3%o2_aa)
        deallocate(vec3%o2_ab)
        deallocate(vec3%o2_bb)

        deallocate(vec3%o3_aaa)
        deallocate(vec3%o3_aab)
        deallocate(vec3%o3_abb)
        deallocate(vec3%o3_bbb)

    end subroutine dealloc_vec3_t

    subroutine ext_cor_driver(sys, run, cc)

        ! Generates CC vector with ampltitudes obtained from an external method. This routine is the main driver of external
        ! correction feature.

        ! In:
        !   sys: system information including molecular data, integrals, etc.
        !   run: runtime options
        ! In/Out:
        !   cc: coupled-cluster information. On exit, cc contains and updated t vector (cc%t_vec)
        !       and all cc%ext_cor variables populated

        use const, only: p, i0
        use determinants, only: gen_f_ref
        use energy, only: calculate_unsorted_energy
        use ext_cor_types, only: vec3_t
        use process_t4, only: update_t2_cluster
        use printing, only: io, print_date
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use symmetry, only: read_sym
        use cc_utils, only: antisymmetrize
        use cluster_analysis, only: analyze_t3

        ! [TMPDEBUG]
        use ext_cor_types, only: ext_cor_t

        type(ext_cor_t) :: test_ext

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc
        type(vec3_t) :: c_vec, t_vec
        integer(i0) :: f_ref(sys%basis%string_len)

        real(p) :: coef_norm

        logical :: rm_dscnctd = .false.
        integer :: i_err


        ! Initialize data required for the cluster analysis
        call read_sym(run%sym_file, sys%orbs)
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
        call parse_fciqmc_c3(sys, f_ref, run%ext_cor_file, c_vec, coef_norm)

        ! Cluster analyze up to three body components
        write(io, '(4x,a)') "=> Starting cluster analysis"
        call antisymmetrize(sys, c_vec)
        !call analyze_t3(sys, c_vec, t_vec, run%rhf, rm_dscnctd)
        call analyze_t3(sys, c_vec, t_vec, .false., rm_dscnctd)
        call antisymmetrize(sys, t_vec)

        ! Copy the new T vector into the CC vector for the next calculation
        write(io, '(4x,a/)') "=> Writing amplitudes"
        t_vec%o1_a = c_vec%o1_a
        t_vec%o1_b = c_vec%o1_b
        call write_t3(sys, cc, t_vec, run%ext_cor_sd)
        call dealloc_vec3_t(t_vec)
        ! Save T2 MC for DCMC calculations
        cc%acc%t2_mc = cc%t_vec(cc%pos(3):cc%pos(6)-1)

        ! Generate T4 and contract with V on the fly
        write(io, '(2x,a)') '=> Generating <ijab|[V_N,T4]|0> intermediate'
        call ext_cor_4(sys, cc, f_ref, run%ext_cor_file, coef_norm, c_vec)
        call dealloc_vec3_t(c_vec)

        ! [TODO] might not be needed
        ! Write to CC vector
        write(io, '(4x,a)') '=> Updating T2 files'
        call update_t2_cluster(sys, cc%ext_cor, f_ref)
        call substract_disc(sys, cc)

        cc%en_cor = calculate_unsorted_energy(sys, cc)
        write(io, '(/2x,a27,2x,f16.10/)') 'External correlation energy', cc%en_cor

        call print_date('  cluster analysis ended on:')
        write(io, '(a)') ''

    end subroutine ext_cor_driver

    subroutine parse_fciqmc_c3(sys, f_ref, filename, c_vec, coef_norm)

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

        use const, only: p, walk_unit, i0
        use determinants, only: encode_det
        use excitations, only: excit_t, get_excitation_spin_integrate, get_excitation_level, &
            shift_occ_list
        use ext_cor_types, only: vec3_t
        use errors, only: stop_all
        use system, only: sys_t

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
        integer :: line(2)
        integer :: c_id
        integer :: occ_list(sys%nel)

        logical :: t_exists

        character(len=30) :: deb_fmt

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

    end subroutine parse_fciqmc_c3

    subroutine substract_disc(sys, cc)

        use const, only: i0, p
        use contract_doubles_ext_cor, only: drive_doubles_contraction
        use system, only: sys_t
        use cc_types, only: cc_t
        use cc_utils, only: antisymmetrize_t2
        use printing, only: io
        use utils, only: get_wall_time

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc

        integer :: i, j, a, b
        integer :: ii, jj, aa, bb

        real(p) :: start_time, end_time

        real(p), allocatable :: v2a(:,:,:,:)
        real(p), allocatable :: v2b(:,:,:,:)
        real(p), allocatable :: v2c(:,:,:,:)


        write(io, '(4x,a)') '=> Contracting non-linear terms'
        start_time = get_wall_time()
        call drive_doubles_contraction(sys, cc, v2a, v2b, v2c)
        end_time = get_wall_time()
        write(io, '(4x,a,f10.2,a)') '=> Contraction took', end_time - start_time, ' seconds'

        call antisymmetrize_t2(sys, cc%ext_cor%t2a, cc%ext_cor%t2c)
        call antisymmetrize_t2(sys, v2a, v2c)

        do i=sys%froz+1, sys%occ_a
            do j=sys%froz+1, sys%occ_a
                do a=sys%occ_a+1, sys%orbs
                    do b=sys%occ_a+1, sys%orbs

                        ii = i - sys%froz
                        jj = j - sys%froz
                        aa = a - sys%occ_a
                        bb = b - sys%occ_a

                        cc%ext_cor%t2a(bb,aa,jj,ii) = cc%ext_cor%t2a(bb,aa,jj,ii) - v2a(b,a,j,i)

                    enddo
                enddo
            enddo
        enddo

        do i=sys%froz+1, sys%occ_a
            do j=sys%froz+1, sys%occ_b
                do a=sys%occ_a+1, sys%orbs
                    do b=sys%occ_b+1, sys%orbs

                        ii = i - sys%froz
                        jj = j - sys%froz
                        aa = a - sys%occ_a
                        bb = b - sys%occ_b

                        cc%ext_cor%t2b(bb,aa,jj,ii) = cc%ext_cor%t2b(bb,aa,jj,ii) - v2b(b,a,j,i)

                    enddo
                enddo
            enddo
        enddo

        do i=sys%froz+1, sys%occ_b
            do j=sys%froz+1, sys%occ_b
                do a=sys%occ_b+1, sys%orbs
                    do b=sys%occ_b+1, sys%orbs

                        ii = i - sys%froz
                        jj = j - sys%froz
                        aa = a - sys%occ_b
                        bb = b - sys%occ_b

                        cc%ext_cor%t2c(bb,aa,jj,ii) = cc%ext_cor%t2c(bb,aa,jj,ii) - v2c(b,a,j,i)

                    enddo
                enddo
            enddo
        enddo

    end subroutine substract_disc

    subroutine ext_cor_4(sys, cc, f_ref, filename, coef_norm, c_vec)

        use const, only: i0, p, walk_unit
        use checking, only: check_allocate, check_deallocate
        use determinants, only: encode_det
        use det_hash
        use excitations, only: excit_t, get_excitation_level, shift_occ_list
        use ext_cor_types, only: vec3_t
        use printing, only: io
        use system, only: sys_t
        use cc_types, only: cc_t
        use process_t4, only: gen_doubles_conf, &
            update_doubles_projection, update_doubles_projection_old
        use utils, only: get_wall_time

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout) :: cc
        integer(i0), intent(in) :: f_ref(sys%basis%string_len)
        character(len=*), intent(in) :: filename
        real(p), intent(in) :: coef_norm
        type(vec3_t), intent(in) :: c_vec

        type(dictionary_t) :: doubles_conf_hash

        real(p) :: c4_amp

        real(p) :: prev_time

        real(p) :: read_coef
        integer :: idx
        integer :: occ_list(sys%nel)
        integer(i0) :: f_t4(sys%basis%string_len)
        integer(i0) :: f_doub(sys%basis%string_len)
        integer :: doubles_nconf
        integer :: cnt_c4 = 0
        integer :: c_id
        integer :: ios
        integer :: ierr
        integer :: excit_rank


        ! Generate doubly excited determinants required to calculate the
        ! matrix elements of <ijab | [V_N, T_4] |phi>
        ! [TODO] we might be able to move this?
        write(io, '(4x,a)') '=> Creating twobody configurations'
        call gen_doubles_conf(sys, cc%ext_cor%doubles_conf, f_ref)
        doubles_nconf = size(cc%ext_cor%doubles_conf, 2)
        write(io, '(8x,a,i8,a)') '=> ', doubles_nconf,' double excitations'
        cc%ext_cor%doubles_nconf = doubles_nconf

        ! Create a hash table for finding indices by mapping determinants
        ! to indices
        call doubles_conf_hash%init(doubles_nconf)
        do idx=1, doubles_nconf
            f_doub = cc%ext_cor%doubles_conf(:,idx)
            call doubles_conf_hash%set(f_doub, idx)
        enddo

        ! Allocate the array that holds the  <ijab | [V_N, T_4] |phi>
        ! projection.
        allocate(cc%ext_cor%doubles_proj(doubles_nconf), stat=ierr)
        cc%ext_cor%doubles_proj = 0.0_p

        ! Load all quadruply excited determinants in CIQMC
        open(walk_unit, file=trim(filename), status='old')

        write(io, '(4x,a)') '=> Starting loop over stochastic quadruply excited determinants'
        prev_time = get_wall_time()
        do

            ! Read Slater determinant
            read(walk_unit, *, iostat=ios) c_id, read_coef, occ_list(1:sys%nel-sys%froz*2)
            if (ios /= 0) exit

            ! Shift occupation list due to frozen orbitals that
            ! need to be taken into account for the CC code
            call shift_occ_list(sys%froz*2, sys%nel, occ_list)
            call encode_det(sys%basis, occ_list, f_t4)
            excit_rank = get_excitation_level(f_ref, f_t4)

            ! Skip non quadruply excited determinants
            if (excit_rank /= 4) cycle

            ! Update the array holding the projections <ijab | [V_N, C_4] |phi>
            c4_amp = read_coef / coef_norm
            if (c4_amp /= 0.0_p) then
                cnt_c4 = cnt_c4 + 1
                call update_doubles_projection_old(sys, cc%ext_cor%doubles_conf, &
                    f_t4, c4_amp, cc%ext_cor%doubles_proj)
            endif

        enddo
        close(walk_unit)

        write(io, '(8x,a,i8,a,f8.2,a)') '=>', cnt_c4, ' amplitudes proccesed in ', &
            get_wall_time()-prev_time, ' seconds'

    end subroutine ext_cor_4

    subroutine write_t3(sys, cc, t_vec, print_doubles)

        ! Write the CC files required for externally corrected CC calculations.
        !
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


        integer :: i, j, k, l
        integer :: a, b, c, d
        real(p), pointer :: t3(:) => null()

        integer :: indx

        t3 => cc%t_vec
        print *, size(cc%t_vec)

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

    end subroutine write_t3

end module external_correction
