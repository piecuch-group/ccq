module cluster_analysis

    ! Cluster analysis module

    implicit none

contains

    subroutine cluster_analysis_driver_opt(sys, run, cc)

        ! Drives the cluster analysis

        ! In:
        !   sys: system information including molecular data, integrals, etc.
        !   run: runtime options
        ! In/Out:
        !   cc: coupled-cluster information. On exit, cc contains and updated t vector (cc%t_vec)
        !       and all cc%ext_cor variables populated

        use const, only: dp, i0
        use determinants, only: gen_f_ref
        use energy, only: calculate_unsorted_energy
        use ext_cor, only: alloc_vec3_t, dealloc_vec3_t
        use ext_cor_types, only: vec3_t
        use t4_generation, only: update_t2_cluster, ext_cor_4
        use process_ci, only: find_fciqmc_c3
        use printing, only: io, print_date
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use symmetry, only: read_sym
        use utilities, only: antisymmetrize

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc
        type(vec3_t) :: c_vec, t_vec
        integer(i0) :: f_ref(sys%basis%string_len)

        real(dp) :: coef_norm

        logical :: rm_dscnctd = .false.
        integer :: i_err

        call read_sym(run%sym_file, sys%orbs)

        write(io, '(a)') 'Cluster analysis'
        write(io, '(a)') '----------------'
        call gen_f_ref(sys, f_ref)

        call print_date('  cluster analysis started on:')

        ! Allocate vectors
        write(io, '(2x,a)') '=> Up to C3'
        write(io, '(4x,a)') '=> Initializing arrays'
        call alloc_vec3_t(sys, c_vec)
        call alloc_vec3_t(sys, t_vec)

        ! Read up to C3
        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations on:"
        write(io, '(6x,a)') trim(run%ext_cor_file)
        call find_fciqmc_c3(sys, f_ref, run%ext_cor_file, c_vec, coef_norm)

        ! Cluster analyze
        write(io, '(4x,a)') "=> Starting cluster analysis"
        call antisymmetrize(sys, c_vec)
        !call analyze_t3(sys, c_vec, t_vec, run%rhf, rm_dscnctd)
        call analyze_t3(sys, c_vec, t_vec, .false., rm_dscnctd)
        call antisymmetrize(sys, t_vec)

        ! Put information in T vec
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
        write(io, '(4x,a/)') '=> Writing T2 files'
        call update_t2_cluster(sys, cc%ext_cor, f_ref)

        cc%en_cor = calculate_unsorted_energy(sys, cc)
        write(io, '(2x,a27,2x,f16.10/)') 'External correlation energy', cc%en_cor

        call print_date('  cluster analysis ended on:')
        write(io, '(a)') ''

    end subroutine cluster_analysis_driver_opt

    subroutine analyze_t3(sys, c_vec, t_vec, rhf, rm_dscnctd)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use const, only: dp
        use ext_cor_types, only: vec3_t
        use symmetry, only: is_sym
        use system, only: sys_t
        use utilities, only: t3_aab_to_t3_abb

        type(sys_t), intent(in) :: sys
        ! CI coefficients
        type(vec3_t), intent(in) :: c_vec
        ! T amplitudes
        type(vec3_t), intent(inout) :: t_vec

        logical, intent(in) :: rhf
        logical, intent(in) :: rm_dscnctd


        ! Place holders
        real(dp) :: c1c3, c12c2, c22, c13
        integer :: ex_orbs(8)

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d
        integer :: indx


        ex_orbs = 0

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, total=>sys%orbs)

            do i=froz+1,occ_a
                do j=i+1,occ_a
                    do a=occ_a+1,total
                        do b=a+1,total
                            if (rm_dscnctd .and. c_vec%o2_aa(a,b,i,j) == 0.0) cycle
                            t_vec%o2_aa(a,b,i,j)=(c_vec%o2_aa(a,b,i,j) &
                                -c_vec%o1_a(a,i)*c_vec%o1_a(b,j) &
                                +c_vec%o1_a(a,j)*c_vec%o1_a(b,i))
                        enddo
                    enddo
                enddo
            enddo


            do i=froz+1,occ_a
                do j=froz+1,occ_b
                    do a=occ_a+1,total
                        do b=occ_b+1,total
                            if (rm_dscnctd .and. c_vec%o2_ab(a,b,i,j) == 0.0) cycle
                            t_vec%o2_ab(a,b,i,j)=(c_vec%o2_ab(a,b,i,j) &
                                -c_vec%o1_a(a,i)*c_vec%o1_b(b,j))
                        enddo
                    enddo
                enddo
            enddo

            if (rhf) then
                t_vec%o2_bb = t_vec%o2_aa
            else
                do i=froz+1,occ_b
                    do j=i+1,occ_b
                        do a=occ_b+1,total
                            do b=a+1,total
                                if (rm_dscnctd .and. c_vec%o2_bb(a,b,i,j) == 0.0) cycle
                                t_vec%o2_bb(a,b,i,j)=(c_vec%o2_bb(a,b,i,j) &
                                    -c_vec%o1_b(a,i)*c_vec%o1_b(b,j) &
                                    +c_vec%o1_b(a,j)*c_vec%o1_b(b,i))
                            enddo
                        enddo
                    enddo
                enddo
            endif

            do i=froz+1,occ_a
                do j=i+1,occ_a
                    do k=j+1,occ_a
                        do a=occ_a+1,total
                            do b=a+1,total
                                do c=b+1,total

                                    ex_orbs(1) = a
                                    ex_orbs(2) = b
                                    ex_orbs(3) = c
                                    ex_orbs(4) = i
                                    ex_orbs(5) = j
                                    ex_orbs(6) = k

                                    if (.not. is_sym(ex_orbs, 6)) cycle
                                    if (rm_dscnctd .and. c_vec%o3_aaa(a,b,c,i,j,k) == 0.0) cycle
                                    t_vec%o3_aaa(a,b,c,i,j,k)=(c_vec%o3_aaa(a,b,c,i,j,k) &
                                        -c_vec%o1_a(a,i)*c_vec%o2_aa(b,c,j,k) &
                                        +c_vec%o1_a(a,j)*c_vec%o2_aa(b,c,i,k) &
                                        +c_vec%o1_a(a,k)*c_vec%o2_aa(b,c,j,i) &
                                        +c_vec%o1_a(b,i)*c_vec%o2_aa(a,c,j,k) &
                                        +c_vec%o1_a(c,i)*c_vec%o2_aa(b,a,j,k) &
                                        -c_vec%o1_a(b,j)*c_vec%o2_aa(a,c,i,k) &
                                        -c_vec%o1_a(c,j)*c_vec%o2_aa(b,a,i,k) &
                                        -c_vec%o1_a(b,k)*c_vec%o2_aa(a,c,j,i) &
                                        -c_vec%o1_a(c,k)*c_vec%o2_aa(b,a,j,i) &
                                        +2*(c_vec%o1_a(a,i)*c_vec%o1_a(b,j)*c_vec%o1_a(c,k) &
                                        -c_vec%o1_a(a,j)*c_vec%o1_a(b,i)*c_vec%o1_a(c,k) &
                                        -c_vec%o1_a(a,k)*c_vec%o1_a(b,j)*c_vec%o1_a(c,i) &
                                        -c_vec%o1_a(a,i)*c_vec%o1_a(b,k)*c_vec%o1_a(c,j) &
                                        +c_vec%o1_a(a,j)*c_vec%o1_a(b,k)*c_vec%o1_a(c,i) &
                                        +c_vec%o1_a(a,k)*c_vec%o1_a(b,i)*c_vec%o1_a(c,j)))
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            do i=froz+1,occ_a
                do j=i+1,occ_a
                    do k=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=a+1,total
                                do c=occ_b+1,total

                                    ex_orbs(1) = a
                                    ex_orbs(2) = b
                                    ex_orbs(3) = c
                                    ex_orbs(4) = i
                                    ex_orbs(5) = j
                                    ex_orbs(6) = k

                                    if (.not. is_sym(ex_orbs, 6)) cycle
                                    if (rm_dscnctd .and. c_vec%o3_aab(a,b,c,i,j,k) == 0.0) cycle
                                    t_vec%o3_aab(a,b,c,i,j,k)=(c_vec%o3_aab(a,b,c,i,j,k) &
                                        -c_vec%o1_a(a,i)*c_vec%o2_ab(b,c,j,k) &
                                        +c_vec%o1_a(a,j)*c_vec%o2_ab(b,c,i,k) &
                                        +c_vec%o1_a(b,i)*c_vec%o2_ab(a,c,j,k) &
                                        -c_vec%o1_a(b,j)*c_vec%o2_ab(a,c,i,k) &
                                        -c_vec%o1_b(c,k)*c_vec%o2_aa(a,b,i,j) &
                                        +2*(c_vec%o1_a(a,i)*c_vec%o1_a(b,j)*c_vec%o1_b(c,k) &
                                        -c_vec%o1_a(a,j)*c_vec%o1_a(b,i)*c_vec%o1_b(c,k)))
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo

            if (rhf) then
                call t3_aab_to_t3_abb(sys, t_vec%o3_aab, t_vec%o3_abb)
                t_vec%o3_bbb = t_vec%o3_aaa
            else

                do i=froz+1,occ_a
                    do j=froz+1,occ_b
                        do k=j+1,occ_b
                            do a=occ_a+1,total
                                do b=occ_b+1,total
                                    do c=b+1,total

                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = i
                                        ex_orbs(5) = j
                                        ex_orbs(6) = k

                                        if (.not. is_sym(ex_orbs, 6)) cycle
                                        if (rm_dscnctd .and. c_vec%o3_abb(a,b,c,i,j,k) == 0.0) cycle
                                        t_vec%o3_abb(a,b,c,i,j,k)=(c_vec%o3_abb(a,b,c,i,j,k) &
                                            -c_vec%o1_a(a,i)*c_vec%o2_bb(b,c,j,k) &
                                            -c_vec%o1_b(c,k)*c_vec%o2_ab(a,b,i,j) &
                                            +c_vec%o1_b(c,j)*c_vec%o2_ab(a,b,i,k) &
                                            +c_vec%o1_b(b,k)*c_vec%o2_ab(a,c,i,j) &
                                            -c_vec%o1_b(b,j)*c_vec%o2_ab(a,c,i,k) &
                                            +2*(c_vec%o1_a(a,i)*c_vec%o1_b(b,j)*c_vec%o1_b(c,k) &
                                            -c_vec%o1_a(a,i)*c_vec%o1_b(b,k)*c_vec%o1_b(c,j)))
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo

                do i=froz+1,occ_b
                    do j=i+1,occ_b
                        do k=j+1,occ_b
                            do a=occ_b+1,total
                                do b=a+1,total
                                    do c=b+1,total

                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = i
                                        ex_orbs(5) = j
                                        ex_orbs(6) = k

                                        if (.not. is_sym(ex_orbs, 6)) cycle
                                        if (rm_dscnctd .and. c_vec%o3_bbb(a,b,c,i,j,k) == 0.0) cycle
                                        t_vec%o3_bbb(a,b,c,i,j,k)=(c_vec%o3_bbb(a,b,c,i,j,k) &
                                            -c_vec%o1_b(a,i)*c_vec%o2_bb(b,c,j,k) &
                                            +c_vec%o1_b(a,j)*c_vec%o2_bb(b,c,i,k) &
                                            +c_vec%o1_b(a,k)*c_vec%o2_bb(b,c,j,i) &
                                            +c_vec%o1_b(b,i)*c_vec%o2_bb(a,c,j,k) &
                                            +c_vec%o1_b(c,i)*c_vec%o2_bb(b,a,j,k) &
                                            -c_vec%o1_b(b,j)*c_vec%o2_bb(a,c,i,k) &
                                            -c_vec%o1_b(c,j)*c_vec%o2_bb(b,a,i,k) &
                                            -c_vec%o1_b(b,k)*c_vec%o2_bb(a,c,j,i) &
                                            -c_vec%o1_b(c,k)*c_vec%o2_bb(b,a,j,i) &
                                            +2*(c_vec%o1_b(a,i)*c_vec%o1_b(b,j)*c_vec%o1_b(c,k) &
                                            -c_vec%o1_b(a,j)*c_vec%o1_b(b,i)*c_vec%o1_b(c,k) &
                                            -c_vec%o1_b(a,k)*c_vec%o1_b(b,j)*c_vec%o1_b(c,i) &
                                            -c_vec%o1_b(a,i)*c_vec%o1_b(b,k)*c_vec%o1_b(c,j) &
                                            +c_vec%o1_b(a,j)*c_vec%o1_b(b,k)*c_vec%o1_b(c,i) &
                                            +c_vec%o1_b(a,k)*c_vec%o1_b(b,i)*c_vec%o1_b(c,j)))
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            endif

        end associate

    end subroutine analyze_t3

    subroutine write_t3(sys, cc, t_vec, print_doubles)

        ! Write the CC files required for externally corrected CC calculations.
        !
        ! In:
        !   sys: system information
        !   t_vec: cluster analyzed amplitudes up to triples
        !   print_doubles: if true writes down single and double amplitudes as well
        ! In/Out:
        !   cc: coupled-cluster information with updated T vector (cc%t_vec) amplitudes

        use const, only: dp
        use system, only: sys_t
        use cc_types, only: cc_t
        use ext_cor_types, only: vec3_t

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(inout), target :: cc
        type(vec3_t), intent(in) :: t_vec
        logical, intent(in) :: print_doubles


        integer :: i, j, k, l
        integer :: a, b, c, d
        real(dp), pointer :: t3(:) => null()

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

    end subroutine write_t3

end module cluster_analysis
