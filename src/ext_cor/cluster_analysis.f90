module cluster_analysis

    ! Module for performing cluster analysis.

    implicit none

contains

    subroutine cluster_analysis_driver_opt(sys, run, cc)

        use const, only: dp
        use ext_cor_types, only: c_vec_t
        use hmatrix, only: ext_cor_update_t2_cluster
        use process_ci, only: find_fciqmc_c3, get_ext_cor_4, antisymmetrize
        use printing, only: io
        use system, only: sys_t, run_t, cc_t
        use symmetry, only: read_sym
        use sys_data

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc
        type(c_vec_t) :: c_vec

        real(kind=8), allocatable :: t2_aa(:,:,:,:)
        real(kind=8), allocatable :: t2_ab(:,:,:,:)
        real(kind=8), allocatable :: t2_bb(:,:,:,:)

        real(kind=8), allocatable :: t3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable :: t3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable :: t3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable :: t3_bbb(:,:,:,:,:,:)

        integer, allocatable :: twobody_confs(:,:)
        real(dp), allocatable :: twobody_proj(:)

        real(dp) :: coef_norm

        logical :: print_doubles = .true.
        logical :: jun_moe = .false.
        logical :: closed_shell = .false.
        logical :: config_debug = .false.
        logical :: from_moe = .false.
        logical :: rm_dscnctd = .false.
        integer :: i_err

        call read_sym('sym.out', sys%orbs)
        !if (sys%occ_a == sys%occ_b) closed_shell = .true.
        froz = sys%froz
        occ_a = sys%occ_a
        occ_b = sys%occ_b
        nele = occ_a + occ_b
        total = sys%orbs
        unocc = total * 2 - nele
        nsorb = (total * 2) - (froz * 2)
        nocc_a = sys%occ_a - sys%froz
        nocc_b = sys%occ_b - sys%froz
        nunocc_a = sys%orbs - sys%occ_a
        nunocc_b = sys%orbs - sys%occ_b
        ntotal = cc%t_size
        ntotal_moe = cc%t_size
        i_t1a = cc%pos(1)
        i_t1b = cc%pos(2)
        i_t2a = cc%pos(3)
        i_t2b = cc%pos(4)
        i_t2c = cc%pos(5)
        i_t3a = cc%pos(6)
        i_t3b = cc%pos(7)
        i_t3c = cc%pos(8)
        i_t3d = cc%pos(9)

        write(io, '(a)') 'Cluster analysis'
        write(io, '(a)') '----------------'
        write(io, '(2x,a)') '=> Allocating arrays'

        allocate(c_vec%c1_a(occ_a+1:total,froz+1:occ_a))
        allocate(c_vec%c1_b(occ_b+1:total,froz+1:occ_b), stat=i_err)

        allocate(c_vec%c2_aa(occ_a+1:total,occ_a+1:total,froz+1:occ_a,froz+1:occ_a))
        allocate(c_vec%c2_ab(occ_a+1:total,occ_b+1:total,froz+1:occ_a,froz+1:occ_b))
        allocate(c_vec%c2_bb(occ_b+1:total,occ_b+1:total,froz+1:occ_b,froz+1:occ_b))

        allocate(c_vec%c3_aaa(occ_a+1:total,occ_a+1:total,occ_a+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        allocate(c_vec%c3_aab(occ_a+1:total,occ_a+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
        allocate(c_vec%c3_abb(occ_a+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
        allocate(c_vec%c3_bbb(occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))


        allocate(t2_aa(occ_a+1:total,occ_a+1:total,froz+1:occ_a,froz+1:occ_a))
        allocate(t2_ab(occ_a+1:total,occ_b+1:total,froz+1:occ_a,froz+1:occ_b))
        allocate(t2_bb(occ_b+1:total,occ_b+1:total,froz+1:occ_b,froz+1:occ_b))

        allocate(t3_aaa(occ_a+1:total,occ_a+1:total,occ_a+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        allocate(t3_aab(occ_a+1:total,occ_a+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
        allocate(t3_abb(occ_a+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
        allocate(t3_bbb(occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))


        write(io, '(2x,a)') '=> Up to C3'
        write(io, '(4x,a)') '=> Initializing arrays'
        c_vec%c1_a = 0.0d0
        c_vec%c1_b = 0.0d0
        c_vec%c2_aa = 0.0d0
        c_vec%c2_ab = 0.0d0
        c_vec%c2_bb = 0.0d0
        c_vec%c3_aaa = 0.0d0
        c_vec%c3_aab = 0.0d0
        c_vec%c3_abb = 0.0d0
        c_vec%c3_bbb = 0.0d0

        t2_aa = 0.0d0
        t2_ab = 0.0d0
        t2_bb = 0.0d0
        t3_aaa = 0.0d0
        t3_aab = 0.0d0
        t3_abb = 0.0d0
        t3_bbb = 0.0d0

        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations on:"
        write(io, '(6x,a)') trim(run%ext_cor_file)
        call find_fciqmc_c3(run%ext_cor_file, config_debug, &
            c_vec%c1_a, c_vec%c1_b, c_vec%c2_aa, c_vec%c2_ab, c_vec%c2_bb, &
            c_vec%c3_aaa, c_vec%c3_aab, c_vec%c3_abb, c_vec%c3_bbb, coef_norm)

        write(io, '(4x,a)') "=> Starting cluster analysis"
        call analyze_t3(from_moe, closed_shell, rm_dscnctd, &
            c_vec%c1_a, c_vec%c1_b, c_vec%c2_aa, c_vec%c2_ab, c_vec%c2_bb, &
            c_vec%c3_aaa, c_vec%c3_aab, c_vec%c3_abb, c_vec%c3_bbb, &
            t2_aa, t2_ab, t2_bb, &
            t3_aaa, t3_aab, t3_abb, t3_bbb)
        call antisymmetrize(t2_aa, t2_ab, t2_bb, &
            t3_aaa, t3_aab, t3_abb, t3_bbb)

        write(io, '(4x,a/)') "=> Writing amplitudes"
        call write_t3(cc, print_doubles, c_vec%c1_a, c_vec%c1_b, &
            t2_aa, t2_ab, t2_bb, &
            t3_aaa, t3_aab, t3_abb, t3_bbb)

        deallocate(t2_aa)
        deallocate(t2_ab)
        deallocate(t2_bb)

        deallocate(t3_aaa)
        deallocate(t3_aab)
        deallocate(t3_abb)
        deallocate(t3_bbb)

        write(io, '(2x,a)') '=> Generating <ijab|[V_N,T4]|0> intermediate'
        call get_ext_cor_4(run%ext_cor_file, coef_norm, c_vec, sys%ints, cc%ext_cor%twobody_confs, cc%ext_cor%twobody_proj)

        deallocate(c_vec%c1_a)
        deallocate(c_vec%c1_b)

        deallocate(c_vec%c2_aa)
        deallocate(c_vec%c2_ab)
        deallocate(c_vec%c2_bb)

        deallocate(c_vec%c3_aaa)
        deallocate(c_vec%c3_aab)
        deallocate(c_vec%c3_abb)
        deallocate(c_vec%c3_bbb)

        write(io, '(4x,a/)') '=> Writing T2 files'
        call ext_cor_update_t2_cluster(sys, cc%ext_cor)


    end subroutine cluster_analysis_driver_opt

    subroutine cluster_analysis_driver(sys, run, cc)

        use const, only: dp
        use process_ci, only: find_fciqmc_c3, &
            find_fciqmc_c4_aaaa, find_fciqmc_c4_aaab, &
            find_fciqmc_c4_aabb, find_fciqmc_c4_abbb, find_fciqmc_c4_bbbb, antisymmetrize
        use printing, only: io
        use system, only: sys_t, run_t, cc_t
        use symmetry, only: read_sym
        use sys_data

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout) :: cc

        real(kind=8), allocatable :: c1_a(:,:)
        real(kind=8), allocatable :: c1_b(:,:)

        real(kind=8), allocatable :: c2_aa(:,:,:,:)
        real(kind=8), allocatable :: c2_ab(:,:,:,:)
        real(kind=8), allocatable :: c2_bb(:,:,:,:)

        real(kind=8), allocatable :: c3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable :: c3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable :: c3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable :: c3_bbb(:,:,:,:,:,:)

        real(kind=8), allocatable :: c4_aaaa(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: c4_aaab(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: c4_aabb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: c4_abbb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: c4_bbbb(:,:,:,:,:,:,:,:)

        real(kind=8), allocatable :: t1_a(:,:)
        real(kind=8), allocatable :: t1_b(:,:)

        real(kind=8), allocatable :: t2_aa(:,:,:,:)
        real(kind=8), allocatable :: t2_ab(:,:,:,:)
        real(kind=8), allocatable :: t2_bb(:,:,:,:)

        real(kind=8), allocatable :: t3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable :: t3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable :: t3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable :: t3_bbb(:,:,:,:,:,:)

        real(kind=8), allocatable :: t4_aaaa(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_aaab(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_aabb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_abbb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_bbbb(:,:,:,:,:,:,:,:)

        real(dp) :: coef_norm

        logical :: print_doubles = .true.
        logical :: jun_moe = .false.
        logical :: closed_shell = .false.
        logical :: config_debug = .false.
        logical :: from_moe = .false.
        logical :: rm_dscnctd = .false.
        integer :: i_err

        call read_sym('sym.out', sys%orbs)
        !if (sys%occ_a == sys%occ_b) closed_shell = .true.
        froz = sys%froz
        occ_a = sys%occ_a
        occ_b = sys%occ_b
        nele = occ_a + occ_b
        total = sys%orbs
        unocc = total * 2 - nele
        nsorb = (total * 2) - (froz * 2)
        nocc_a = sys%occ_a - sys%froz
        nocc_b = sys%occ_b - sys%froz
        nunocc_a = sys%orbs - sys%occ_a
        nunocc_b = sys%orbs - sys%occ_b
        ntotal = cc%t_size
        ntotal_moe = cc%t_size
        i_t1a = cc%pos(1)
        i_t1b = cc%pos(2)
        i_t2a = cc%pos(3)
        i_t2b = cc%pos(4)
        i_t2c = cc%pos(5)
        i_t3a = cc%pos(6)
        i_t3b = cc%pos(7)
        i_t3c = cc%pos(8)
        i_t3d = cc%pos(9)

        write(io, '(a)') 'Cluster analysis'
        write(io, '(a)') '----------------'
        write(io, '(2x,a)') '=> Allocating arrays'
        allocate(c1_a(occ_a+1:total,froz+1:occ_a))
        allocate(c1_b(occ_b+1:total,froz+1:occ_b), stat=i_err)

        allocate(c2_aa(occ_a+1:total,occ_a+1:total,froz+1:occ_a,froz+1:occ_a))
        allocate(c2_ab(occ_a+1:total,occ_b+1:total,froz+1:occ_a,froz+1:occ_b))
        allocate(c2_bb(occ_b+1:total,occ_b+1:total,froz+1:occ_b,froz+1:occ_b))

        allocate(c3_aaa(occ_a+1:total,occ_a+1:total,occ_a+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        allocate(c3_aab(occ_a+1:total,occ_a+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
        allocate(c3_abb(occ_a+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
        allocate(c3_bbb(occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))


        allocate(t1_a(occ_a+1:total,froz+1:occ_a))
        allocate(t1_b(occ_b+1:total,froz+1:occ_b))

        allocate(t2_aa(occ_a+1:total,occ_a+1:total,froz+1:occ_a,froz+1:occ_a))
        allocate(t2_ab(occ_a+1:total,occ_b+1:total,froz+1:occ_a,froz+1:occ_b))
        allocate(t2_bb(occ_b+1:total,occ_b+1:total,froz+1:occ_b,froz+1:occ_b))

        allocate(t3_aaa(occ_a+1:total,occ_a+1:total,occ_a+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        allocate(t3_aab(occ_a+1:total,occ_a+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
        allocate(t3_abb(occ_a+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
        allocate(t3_bbb(occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))


        write(io, '(2x,a)') '=> Up to C3'
        write(io, '(4x,a)') '=> Initializing arrays'
        c1_a = 0.0d0
        c1_b = 0.0d0
        c2_aa = 0.0d0
        c2_ab = 0.0d0
        c2_bb = 0.0d0
        c3_aaa = 0.0d0
        c3_aab = 0.0d0
        c3_abb = 0.0d0
        c3_bbb = 0.0d0

        t1_a = 0.0d0
        t1_b = 0.0d0
        t2_aa = 0.0d0
        t2_ab = 0.0d0
        t2_bb = 0.0d0
        t3_aaa = 0.0d0
        t3_aab = 0.0d0
        t3_abb = 0.0d0
        t3_bbb = 0.0d0

        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations on:"
        write(io, '(6x,a)') trim(run%ext_cor_file)
        call find_fciqmc_c3(run%ext_cor_file, config_debug, &
            c1_a, c1_b, c2_aa, c2_ab, c2_bb, &
            c3_aaa, c3_aab, c3_abb, c3_bbb, coef_norm)

        write(io, '(4x,a)') "=> Starting cluster analysis"
        call analyze_t3(from_moe, closed_shell, rm_dscnctd, &
            c1_a, c1_b, c2_aa, c2_ab, c2_bb, &
            c3_aaa, c3_aab, c3_abb, c3_bbb, &
            t2_aa, t2_ab, t2_bb, &
            t3_aaa, t3_aab, t3_abb, t3_bbb)
        call antisymmetrize(t2_aa, t2_ab, t2_bb, &
            t3_aaa, t3_aab, t3_abb, t3_bbb)

        write(io, '(4x,a/)') "=> Writing amplitudes"
        call write_t3(cc, print_doubles, c1_a, c1_b, &
            t2_aa, t2_ab, t2_bb, &
            t3_aaa, t3_aab, t3_abb, t3_bbb)

        deallocate(t1_a)
        deallocate(t1_b)

        deallocate(t2_aa)
        deallocate(t2_ab)
        deallocate(t2_bb)

        deallocate(t3_aaa)
        deallocate(t3_aab)
        deallocate(t3_abb)
        deallocate(t3_bbb)



        write(io, '(2x,a)') '=> C4_aaaa'
        write(io, '(4x,a)') '=> Initializing C4 array'
        allocate(c4_aaaa(occ_a+1:total,occ_a+1:total,occ_a+1:total,occ_a+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        c4_aaaa = 0.0d0
        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations"
        call find_fciqmc_c4_aaaa(trim(run%ext_cor_file), config_debug, coef_norm, c4_aaaa)
        write(io, '(4x,a)') '=> Initializing T4 array'
        allocate(t4_aaaa(occ_a+1:total,occ_a+1:total,occ_a+1:total,occ_a+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        t4_aaaa = 0.0d0
        write(io, '(4x,a)') "=> Starting cluster analysis"
        call analyze_t4_aaaa(rm_dscnctd, c1_a, c2_aa, c3_aaa, c4_aaaa, t4_aaaa)
        deallocate(c4_aaaa)
        write(io, '(4x,a/)') "=> Writing amplitudes"
        call write_t4_aaaa(t4_aaaa)
        deallocate(t4_aaaa)


        write(io, '(2x,a)') '=> C4_aaab'
        write(io, '(4x,a)') '=> Initializing C4 array'
        allocate(c4_aaab(occ_a+1:total,occ_a+1:total,occ_a+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
        c4_aaab = 0.0d0
        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations"
        call find_fciqmc_c4_aaab(trim(run%ext_cor_file), config_debug, coef_norm, c4_aaab)
        write(io, '(4x,a)') '=> Initializing T4 array'
        allocate(t4_aaab(occ_a+1:total,occ_a+1:total,occ_a+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
        t4_aaab = 0.0d0
        write(io, '(4x,a)') "=> Starting cluster analysis"
        call analyze_t4_aaab(rm_dscnctd, c1_a, c1_b, c2_aa, c2_ab, c3_aaa, c3_aab, c4_aaab, t4_aaab)
        deallocate(c4_aaab)
        write(io, '(4x,a/)') "=> Writing amplitudes"
        call write_t4_aaab(t4_aaab)
        deallocate(t4_aaab)

        write(io, '(2x,a)') '=> C4_aabb'
        write(io, '(4x,a)') '=> Initializing C4 array'
        allocate(c4_aabb(occ_a+1:total,occ_a+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
        c4_aabb = 0.0d0
        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations"
        call find_fciqmc_c4_aabb(trim(run%ext_cor_file), config_debug, coef_norm, c4_aabb)
        write(io, '(4x,a)') '=> Initializing T4 array'
        allocate(t4_aabb(occ_a+1:total,occ_a+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
        t4_aabb = 0.0d0
        write(io, '(4x,a)') "=> Starting cluster analysis"
        call analyze_t4_aabb(rm_dscnctd, c1_a, c1_b, c2_aa, c2_ab, c2_bb, c3_aab, c3_abb, c4_aabb, t4_aabb)
        deallocate(c4_aabb)
        write(io, '(4x,a/)') "=> Writing amplitudes"
        call write_t4_aabb(t4_aabb)
        deallocate(t4_aabb)

        write(io, '(2x,a)') '=> C4_abbb'
        write(io, '(4x,a)') '=> Initializing C4 array'
        allocate(c4_abbb(occ_a+1:total,occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
        c4_abbb = 0.0d0
        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations"
        call find_fciqmc_c4_abbb(trim(run%ext_cor_file), config_debug, coef_norm, c4_abbb)
        write(io, '(4x,a)') '=> Initializing T4 array'
        allocate(t4_abbb(occ_a+1:total,occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
        t4_abbb = 0.0d0
        write(io, '(4x,a)') "=> Starting cluster analysis"
        call analyze_t4_abbb(rm_dscnctd, c1_a, c1_b, c2_ab, c2_bb, c3_abb, c3_bbb, c4_abbb, t4_abbb)
        deallocate(c4_abbb)
        write(io, '(4x,a/)') "=> Writing amplitudes"
        call write_t4_abbb(t4_abbb)
        deallocate(t4_abbb)

        write(io, '(2x,a)') '=> C4_bbbb'
        write(io, '(4x,a)') '=> Initializing C4 array'
        allocate(c4_bbbb(occ_b+1:total,occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_b,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
        c4_bbbb = 0.0d0
        write(io, '(4x,a)') "=> Reading and decoding FCIQMC vector configurations"
        call find_fciqmc_c4_bbbb(trim(run%ext_cor_file), config_debug, coef_norm, c4_bbbb)
        write(io, '(4x,a)') '=> Initializing T4 array'
        allocate(t4_bbbb(occ_b+1:total,occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_b,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
        t4_bbbb = 0.0d0
        write(io, '(4x,a)') "=> Starting cluster analysis"
        call analyze_t4_bbbb(rm_dscnctd, c1_b, c2_bb, c3_bbb, c4_bbbb, t4_bbbb)
        deallocate(c4_bbbb)
        write(io, '(4x,a/)') "=> Writing amplitudes"
        call write_t4_bbbb(t4_bbbb)
        deallocate(t4_bbbb)

        deallocate(c1_a)
        deallocate(c1_b)

        deallocate(c2_aa)
        deallocate(c2_ab)
        deallocate(c2_bb)

        deallocate(c3_aaa)
        deallocate(c3_aab)
        deallocate(c3_abb)
        deallocate(c3_bbb)


    end subroutine cluster_analysis_driver

    subroutine test_energy_c(c1_a, c1_b, c2_aa, c2_ab, c2_bb)

        ! Compute the projective energy using CI coefficients and one- and two-body integrals
        !
        ! In:
        !   c*: one- and two-body CI coefficients

        use sys_data

        real(kind=8), allocatable, intent(in) :: c1_a(:,:)
        real(kind=8), allocatable, intent(in) :: c1_b(:,:)

        real(kind=8), allocatable, intent(in) :: c2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c2_bb(:,:,:,:)

        real(kind=8), allocatable :: fn_a(:,:), fn_b(:,:)
        real(kind=8), allocatable :: vn_aa(:,:,:,:)
        real(kind=8), allocatable :: vn_ab(:,:,:,:)
        real(kind=8), allocatable :: vn_bb(:,:,:,:)

        real(kind=8) :: energy_1a, energy_1b, energy_2aa, energy_2ab, energy_2bb
        real(kind=8) :: e_repul

        integer :: i, j, a, b

        allocate(fn_a(froz+1:total, froz+1:total))
        allocate(fn_b(froz+1:total, froz+1:total))
        allocate(vn_aa(froz+1:total, froz+1:total, froz+1:total, froz+1:total))
        allocate(vn_ab(froz+1:total, froz+1:total, froz+1:total, froz+1:total))
        allocate(vn_bb(froz+1:total, froz+1:total, froz+1:total, froz+1:total))

        call read_integrals(fn_a, fn_b, vn_aa, vn_ab, vn_bb, e_repul)

        energy_1a = 0.0d0
        energy_1b = 0.0d0
        energy_2aa = 0.0d0
        energy_2ab = 0.0d0
        energy_2bb = 0.0d0

        do i=froz+1, occ_a
            do a=occ_a+1, total
                energy_1a = energy_1a + fn_a(a,i) * c1_a(a,i)
            enddo
        enddo

        do i=froz+1, occ_b
            do a=occ_b+1, total
                energy_1b = energy_1b + fn_b(a,i) * c1_b(a,i)
            enddo
        enddo

        do i=froz+1, occ_a
            do j=froz+1, occ_a
                do a=occ_a+1, total
                    do b=occ_a+1, total
                        energy_2aa = energy_2aa + 0.25 * vn_aa(a,b,i,j) * c2_aa(a,b,i,j)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1, occ_a
            do j=froz+1, occ_b
                do a=occ_a+1, total
                    do b=occ_b+1, total
                        energy_2ab = energy_2ab + vn_ab(a,b,i,j) * c2_ab(a,b,i,j)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1, occ_b
            do j=froz+1, occ_b
                do a=occ_b+1, total
                    do b=occ_b+1, total
                        energy_2bb = energy_2bb + 0.25 * vn_bb(a,b,i,j) * c2_bb(a,b,i,j)
                    enddo
                enddo
            enddo
        enddo

        print '(/a)', 'Energy from C vector'
        print '(a/)', '--------------------'
        print '(a15,f24.10)', 'C1 a', energy_1a
        print '(a15,f24.10)', 'C1 b', energy_1b
        print '(a15,f24.10)', 'C2 aa', energy_2aa
        print '(a15,f24.10)', 'C2 ab', energy_2ab
        print '(a15,f24.10)', 'C2 bb', energy_2bb
        print '(a15,f24.10/)', 'Total', energy_1a + energy_1b + energy_2aa + &
            energy_2ab + energy_2bb

        deallocate(fn_a)
        deallocate(fn_b)
        deallocate(vn_aa)
        deallocate(vn_ab)
        deallocate(vn_bb)


    end subroutine test_energy_c

    subroutine test_energy_t(t1_a, t1_b, t2_aa, t2_ab, t2_bb)

        ! Compute the projective energy using CC coefficients and one- and two-body integrals
        !
        ! In:
        !   t*: one- and two-body T coefficients

        use sys_data

        real(kind=8), allocatable, intent(in) :: t1_a(:,:)
        real(kind=8), allocatable, intent(in) :: t1_b(:,:)

        real(kind=8), allocatable, intent(in) :: t2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: t2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: t2_bb(:,:,:,:)

        real(kind=8), allocatable :: fn_a(:,:), fn_b(:,:)
        real(kind=8), allocatable :: vn_aa(:,:,:,:)
        real(kind=8), allocatable :: vn_ab(:,:,:,:)
        real(kind=8), allocatable :: vn_bb(:,:,:,:)

        real(kind=8) :: energy_1a, energy_1b
        real(kind=8) :: energy_1aa, energy_1ab, energy_1bb
        real(kind=8) :: energy_2aa, energy_2ab, energy_2bb
        real(kind=8) :: e_repul

        integer :: i, j, a, b

        allocate(fn_a(froz+1:total, froz+1:total))
        allocate(fn_b(froz+1:total, froz+1:total))
        allocate(vn_aa(froz+1:total, froz+1:total, froz+1:total, froz+1:total))
        allocate(vn_ab(froz+1:total, froz+1:total, froz+1:total, froz+1:total))
        allocate(vn_bb(froz+1:total, froz+1:total, froz+1:total, froz+1:total))

        call read_integrals(fn_a, fn_b, vn_aa, vn_ab, vn_bb, e_repul)

        energy_1a = 0.0d0
        energy_1b = 0.0d0
        energy_1aa = 0.0d0
        energy_1ab = 0.0d0
        energy_1bb = 0.0d0
        energy_2aa = 0.0d0
        energy_2ab = 0.0d0
        energy_2bb = 0.0d0

        do i=froz+1, occ_a
            do a=occ_a+1, total
                energy_1a = energy_1a + fn_a(a,i) * t1_a(a,i)
            enddo
        enddo

        do i=froz+1, occ_b
            do a=occ_b+1, total
                energy_1b = energy_1b + fn_b(a,i) * t1_b(a,i)
            enddo
        enddo

        do i=froz+1, occ_a
            do j=froz+1, occ_a
                do a=occ_a+1, total
                    do b=occ_a+1, total
                        energy_1aa = energy_1aa + 0.5 * vn_aa(a,b,i,j) * t1_a(a,i) * t1_a(b,j)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1, occ_a
            do j=froz+1, occ_b
                do a=occ_a+1, total
                    do b=occ_b+1, total
                        energy_1ab = energy_1ab + vn_ab(a,b,i,j) * t1_a(a,i) * t1_b(b,j)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1, occ_b
            do j=froz+1, occ_b
                do a=occ_b+1, total
                    do b=occ_b+1, total
                        energy_1bb = energy_1bb + 0.5 * vn_bb(a,b,i,j) * t1_b(a,i) * t1_b(b,j)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1, occ_a
            do j=froz+1, occ_a
                do a=occ_a+1, total
                    do b=occ_a+1, total
                        energy_2aa = energy_2aa + 0.25 * vn_aa(a,b,i,j) * t2_aa(a,b,i,j)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1, occ_a
            do j=froz+1, occ_b
                do a=occ_a+1, total
                    do b=occ_b+1, total
                        energy_2ab = energy_2ab + vn_ab(a,b,i,j) * t2_ab(a,b,i,j)
                    enddo
                enddo
            enddo
        enddo

        do i=froz+1, occ_b
            do j=froz+1, occ_b
                do a=occ_b+1, total
                    do b=occ_b+1, total
                        energy_2bb = energy_2bb + 0.25 * vn_bb(a,b,i,j) * t2_bb(a,b,i,j)
                    enddo
                enddo
            enddo
        enddo

        print '(/a)', 'Energy from T vector'
        print '(a/)', '--------------------'
        print '(a15,f24.10)', 'T1 a', energy_1a
        print '(a15,f24.10)', 'T1 b', energy_1b
        print '(a15,f24.10)', 'T1^2 aa', energy_1aa
        print '(a15,f24.10)', 'T1^2 ab', energy_1ab
        print '(a15,f24.10)', 'T1^2 bb', energy_1bb
        print '(a15,f24.10)', 'T2 aa', energy_2aa
        print '(a15,f24.10)', 'T2 ab', energy_2ab
        print '(a15,f24.10)', 'T2 bb', energy_2bb
        print '(a15,f24.10/)', 'Total', energy_1a + energy_1b + &
            energy_1aa + energy_1ab + energy_1bb + &
            energy_2aa + energy_2ab + energy_2bb

        deallocate(fn_a)
        deallocate(fn_b)
        deallocate(vn_aa)
        deallocate(vn_ab)
        deallocate(vn_bb)


    end subroutine test_energy_t

    subroutine analyze_t3(load_singles_doubles, closed_shell, rm_dscnctd, &
            c1_a, c1_b, c2_aa, c2_ab, c2_bb, &
            c3_aaa, c3_aab, c3_abb, c3_bbb, &
            t2_aa, t2_ab, t2_bb, &
            t3_aaa, t3_aab, t3_abb, t3_bbb)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use sys_data
        use symmetry
        use utilities, only: t3_aab_to_t3_abb

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        integer :: indx

        integer :: ex_orbs(100)

        logical, intent(in) :: load_singles_doubles
        logical, intent(in) :: closed_shell
        logical, intent(in) :: rm_dscnctd

        ! Place holders
        real(kind=8) :: c1c3, c12c2, c22, c13

        ! CI coefficients
        real(kind=8), allocatable, intent(in) :: c1_a(:,:)
        real(kind=8), allocatable, intent(in) :: c1_b(:,:)

        real(kind=8), allocatable, intent(in) :: c2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c2_bb(:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c3_bbb(:,:,:,:,:,:)

        ! T amplitudes
        real(kind=8), allocatable, intent(inout) :: t2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: t2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: t2_bb(:,:,:,:)

        real(kind=8), allocatable, intent(inout) :: t3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: t3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: t3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: t3_bbb(:,:,:,:,:,:)

        ex_orbs = 0

        if (.not. load_singles_doubles) then
            do i=froz+1,occ_a
                do j=i+1,occ_a
                    do a=occ_a+1,total
                        do b=a+1,total
                            if (rm_dscnctd .and. c2_aa(a,b,i,j) == 0.0) cycle
                            t2_aa(a,b,i,j)=(c2_aa(a,b,i,j) &
                                -c1_a(a,i)*c1_a(b,j) &
                                +c1_a(a,j)*c1_a(b,i))
                        enddo
                    enddo
                enddo
            enddo


            do i=froz+1,occ_a
                do j=froz+1,occ_b
                    do a=occ_a+1,total
                        do b=occ_b+1,total
                            if (rm_dscnctd .and. c2_ab(a,b,i,j) == 0.0) cycle
                            t2_ab(a,b,i,j)=(c2_ab(a,b,i,j) &
                                -c1_a(a,i)*c1_b(b,j))
                        enddo
                    enddo
                enddo
            enddo

            if (closed_shell) then
                t2_bb = t2_aa
            else
                do i=froz+1,occ_b
                    do j=i+1,occ_b
                        do a=occ_b+1,total
                            do b=a+1,total
                                if (rm_dscnctd .and. c2_bb(a,b,i,j) == 0.0) cycle
                                t2_bb(a,b,i,j)=(c2_bb(a,b,i,j) &
                                    -c1_b(a,i)*c1_b(b,j) &
                                    +c1_b(a,j)*c1_b(b,i))
                            enddo
                        enddo
                    enddo
                enddo
            endif
        else
            t2_aa = c2_aa
            t2_ab = c2_ab
            t2_bb = c2_bb
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
                                if (rm_dscnctd .and. c3_aaa(a,b,c,i,j,k) == 0.0) cycle
                                t3_aaa(a,b,c,i,j,k)=(c3_aaa(a,b,c,i,j,k) &
                                    -c1_a(a,i)*c2_aa(b,c,j,k) &
                                    +c1_a(a,j)*c2_aa(b,c,i,k) &
                                    +c1_a(a,k)*c2_aa(b,c,j,i) &
                                    +c1_a(b,i)*c2_aa(a,c,j,k) &
                                    +c1_a(c,i)*c2_aa(b,a,j,k) &
                                    -c1_a(b,j)*c2_aa(a,c,i,k) &
                                    -c1_a(c,j)*c2_aa(b,a,i,k) &
                                    -c1_a(b,k)*c2_aa(a,c,j,i) &
                                    -c1_a(c,k)*c2_aa(b,a,j,i) &
                                    +2*(c1_a(a,i)*c1_a(b,j)*c1_a(c,k) &
                                    -c1_a(a,j)*c1_a(b,i)*c1_a(c,k) &
                                    -c1_a(a,k)*c1_a(b,j)*c1_a(c,i) &
                                    -c1_a(a,i)*c1_a(b,k)*c1_a(c,j) &
                                    +c1_a(a,j)*c1_a(b,k)*c1_a(c,i) &
                                    +c1_a(a,k)*c1_a(b,i)*c1_a(c,j)))
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
                                if (rm_dscnctd .and. c3_aab(a,b,c,i,j,k) == 0.0) cycle
                                t3_aab(a,b,c,i,j,k)=(c3_aab(a,b,c,i,j,k) &
                                    -c1_a(a,i)*c2_ab(b,c,j,k) &
                                    +c1_a(a,j)*c2_ab(b,c,i,k) &
                                    +c1_a(b,i)*c2_ab(a,c,j,k) &
                                    -c1_a(b,j)*c2_ab(a,c,i,k) &
                                    -c1_b(c,k)*c2_aa(a,b,i,j) &
                                    +2*(c1_a(a,i)*c1_a(b,j)*c1_b(c,k) &
                                    -c1_a(a,j)*c1_a(b,i)*c1_b(c,k)))
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

        if (closed_shell) then
            call t3_aab_to_t3_abb(t3_aab, t3_abb)
            t3_bbb = t3_aaa
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
                                    if (rm_dscnctd .and. c3_abb(a,b,c,i,j,k) == 0.0) cycle
                                    t3_abb(a,b,c,i,j,k)=(c3_abb(a,b,c,i,j,k) &
                                        -c1_a(a,i)*c2_bb(b,c,j,k) &
                                        -c1_b(c,k)*c2_ab(a,b,i,j) &
                                        +c1_b(c,j)*c2_ab(a,b,i,k) &
                                        +c1_b(b,k)*c2_ab(a,c,i,j) &
                                        -c1_b(b,j)*c2_ab(a,c,i,k) &
                                        +2*(c1_a(a,i)*c1_b(b,j)*c1_b(c,k) &
                                        -c1_a(a,i)*c1_b(b,k)*c1_b(c,j)))
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
                                    if (rm_dscnctd .and. c3_bbb(a,b,c,i,j,k) == 0.0) cycle
                                    t3_bbb(a,b,c,i,j,k)=(c3_bbb(a,b,c,i,j,k) &
                                        -c1_b(a,i)*c2_bb(b,c,j,k) &
                                        +c1_b(a,j)*c2_bb(b,c,i,k) &
                                        +c1_b(a,k)*c2_bb(b,c,j,i) &
                                        +c1_b(b,i)*c2_bb(a,c,j,k) &
                                        +c1_b(c,i)*c2_bb(b,a,j,k) &
                                        -c1_b(b,j)*c2_bb(a,c,i,k) &
                                        -c1_b(c,j)*c2_bb(b,a,i,k) &
                                        -c1_b(b,k)*c2_bb(a,c,j,i) &
                                        -c1_b(c,k)*c2_bb(b,a,j,i) &
                                        +2*(c1_b(a,i)*c1_b(b,j)*c1_b(c,k) &
                                        -c1_b(a,j)*c1_b(b,i)*c1_b(c,k) &
                                        -c1_b(a,k)*c1_b(b,j)*c1_b(c,i) &
                                        -c1_b(a,i)*c1_b(b,k)*c1_b(c,j) &
                                        +c1_b(a,j)*c1_b(b,k)*c1_b(c,i) &
                                        +c1_b(a,k)*c1_b(b,i)*c1_b(c,j)))
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        endif

    end subroutine analyze_t3

    subroutine analyze_t4_aaaa(rm_dscnctd, &
            c1_a, c2_aa, &
            c3_aaa, &
            c4_aaaa, &
            t4_aaaa)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use sys_data
        use symmetry
        use utilities, only: t3_aab_to_t3_abb, t4_aaab_to_t4_abbb

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        integer :: indx

        integer :: ex_orbs(100)

        logical, intent(in) :: rm_dscnctd

        ! Place holders
        real(kind=8) :: c1c3, c12c2, c22, c13

        ! CI coefficients
        real(kind=8), allocatable, intent(in) :: c1_a(:,:)

        real(kind=8), allocatable, intent(in) :: c2_aa(:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c3_aaa(:,:,:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c4_aaaa(:,:,:,:,:,:,:,:)

        ! T amplitudes
        real(kind=8), allocatable, intent(inout) :: t4_aaaa(:,:,:,:,:,:,:,:)

        ex_orbs = 0

        do i=froz+1,occ_a
            do j=i+1,occ_a
                do k=j+1,occ_a
                    do l=k+1,occ_a
                        do a=occ_a+1,total
                            do b=a+1,total
                                do c=b+1,total
                                    do d=c+1,total

                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = d
                                        ex_orbs(5) = i
                                        ex_orbs(6) = j
                                        ex_orbs(7) = k
                                        ex_orbs(8) = l

                                        if (.not. is_sym(ex_orbs, 8)) cycle
                                        if (rm_dscnctd .and. c4_aaaa(a,b,c,d,i,j,k,l) == 0.0) cycle
                                        t4_aaaa(a,b,c,d,i,j,k,l) = &
                                            (c4_aaaa(a,b,c,d,i,j,k,l) &
                                            ! -C_1 * C_3
                                        -c1_a(a,i)*c3_aaa(b,c,d,j,k,l) &
                                            +c1_a(a,j)*c3_aaa(b,c,d,i,k,l) &
                                            +c1_a(a,k)*c3_aaa(b,c,d,j,i,l) &
                                            +c1_a(a,l)*c3_aaa(b,c,d,j,k,i) &
                                            +c1_a(b,i)*c3_aaa(a,c,d,j,k,l) &
                                            +c1_a(c,i)*c3_aaa(b,a,d,j,k,l) &
                                            +c1_a(d,i)*c3_aaa(b,c,a,j,k,l) &
                                            -c1_a(b,j)*c3_aaa(a,c,d,i,k,l) &
                                            -c1_a(c,j)*c3_aaa(b,a,d,i,k,l) &
                                            -c1_a(d,j)*c3_aaa(b,c,a,i,k,l) &
                                            -c1_a(b,k)*c3_aaa(a,c,d,j,i,l) &
                                            -c1_a(c,k)*c3_aaa(b,a,d,j,i,l) &
                                            -c1_a(d,k)*c3_aaa(b,c,a,j,i,l) &
                                            -c1_a(b,l)*c3_aaa(a,c,d,j,k,i) &
                                            -c1_a(c,l)*c3_aaa(b,a,d,j,k,i) &
                                            -c1_a(d,l)*c3_aaa(b,c,a,j,k,i) &
                                            ! 2 * 1/2 C_1^2 * C_2
                                        +2*(c1_a(a,i)*c1_a(b,j)*c2_aa(c,d,k,l) & !1
                                            -c1_a(a,k)*c1_a(b,j)*c2_aa(c,d,i,l) & !(ik)
                                            -c1_a(a,l)*c1_a(b,j)*c2_aa(c,d,k,i) & !(il)
                                            -c1_a(a,j)*c1_a(b,i)*c2_aa(c,d,k,l) & !(ij)
                                            -c1_a(a,i)*c1_a(b,k)*c2_aa(c,d,j,l) & !(jk)
                                            -c1_a(a,i)*c1_a(b,l)*c2_aa(c,d,k,j) & !(jl)
                                            -c1_a(c,i)*c1_a(b,j)*c2_aa(a,d,k,l) & !(ac)
                                            -c1_a(d,i)*c1_a(b,j)*c2_aa(c,a,k,l) & !(ad)
                                            -c1_a(a,i)*c1_a(c,j)*c2_aa(b,d,k,l) & !(bc)
                                            -c1_a(a,i)*c1_a(d,j)*c2_aa(c,b,k,l) & !(bd)
                                            +c1_a(c,i)*c1_a(d,j)*c2_aa(a,b,k,l) & !(ac)(bd)
                                            +c1_a(c,k)*c1_a(b,j)*c2_aa(a,d,i,l) & !(ac)(ik)
                                            +c1_a(d,k)*c1_a(b,j)*c2_aa(c,a,i,l) & !(ad)(ik)
                                            +c1_a(a,k)*c1_a(c,j)*c2_aa(b,d,i,l) & !(bc)(ik)
                                            +c1_a(a,k)*c1_a(d,j)*c2_aa(c,b,i,l) & !(bd)(ik)
                                            +c1_a(c,l)*c1_a(b,j)*c2_aa(a,d,k,i) & !(ac)(il)
                                            +c1_a(d,l)*c1_a(b,j)*c2_aa(c,a,k,i) & !(ad)(il)
                                            +c1_a(a,l)*c1_a(c,j)*c2_aa(b,d,k,i) & !(bc)(il)
                                            +c1_a(a,l)*c1_a(d,j)*c2_aa(c,b,k,i) & !(bd)(il)
                                            +c1_a(c,j)*c1_a(b,i)*c2_aa(a,d,k,l) & !(ac)(ij)
                                            +c1_a(d,j)*c1_a(b,i)*c2_aa(c,a,k,l) & !(ad)(ij)
                                            +c1_a(a,j)*c1_a(c,i)*c2_aa(b,d,k,l) & !(bc)(ij)
                                            +c1_a(a,j)*c1_a(d,i)*c2_aa(c,b,k,l) & !(bd)(ij)
                                            +c1_a(c,i)*c1_a(b,k)*c2_aa(a,d,j,l) & !(ac)(jk)
                                            +c1_a(d,i)*c1_a(b,k)*c2_aa(c,a,j,l) & !(ad)(jk)
                                            +c1_a(a,i)*c1_a(c,k)*c2_aa(b,d,j,l) & !(bc)(jk)
                                            +c1_a(a,i)*c1_a(d,k)*c2_aa(c,b,j,l) & !(bd)(jk)
                                            +c1_a(c,i)*c1_a(b,l)*c2_aa(a,d,k,j) & !(ac)(jl)
                                            +c1_a(d,i)*c1_a(b,l)*c2_aa(c,a,k,j) & !(ad)(jl)
                                            +c1_a(a,i)*c1_a(c,l)*c2_aa(b,d,k,j) & !(bc)(jl)
                                            +c1_a(a,i)*c1_a(d,l)*c2_aa(c,b,k,j) & !(bd)(jl)
                                            +c1_a(a,j)*c1_a(b,k)*c2_aa(c,d,i,l) & !(ijk)
                                            +c1_a(a,k)*c1_a(b,i)*c2_aa(c,d,j,l) & !(ikj)
                                            +c1_a(a,j)*c1_a(b,l)*c2_aa(c,d,k,i) & !(ijl)
                                            +c1_a(a,l)*c1_a(b,i)*c2_aa(c,d,k,j) & !(ilj)
                                            -c1_a(a,l)*c1_a(b,k)*c2_aa(c,d,i,j) & !(kilj)
                                            -c1_a(c,k)*c1_a(d,j)*c2_aa(a,b,i,l) & !(ac)(bd)(ik)
                                            -c1_a(c,l)*c1_a(d,j)*c2_aa(a,b,k,i) & !(ac)(bd)(il)
                                            -c1_a(c,j)*c1_a(d,i)*c2_aa(a,b,k,l) & !(ac)(bd)(ij)
                                            -c1_a(c,i)*c1_a(d,k)*c2_aa(a,b,j,l) & !(ac)(bd)(jk)
                                            -c1_a(c,i)*c1_a(d,l)*c2_aa(a,b,k,j) & !(ac)(bd)(jl)
                                            -c1_a(c,j)*c1_a(b,k)*c2_aa(a,d,i,l) & !(ac)(ijk)
                                            -c1_a(d,j)*c1_a(b,k)*c2_aa(c,a,i,l) & !(ad)(ijk)
                                            -c1_a(a,j)*c1_a(c,k)*c2_aa(b,d,i,l) & !(bc)(ijk)
                                            -c1_a(a,j)*c1_a(d,k)*c2_aa(c,b,i,l) & !(bd)(ijk)
                                            -c1_a(c,k)*c1_a(b,i)*c2_aa(a,d,j,l) & !(ac)(ikj)
                                            -c1_a(d,k)*c1_a(b,i)*c2_aa(c,a,j,l) & !(ad)(ikj)
                                            -c1_a(a,k)*c1_a(c,i)*c2_aa(b,d,j,l) & !(bc)(ikj)
                                            -c1_a(a,k)*c1_a(d,i)*c2_aa(c,b,j,l) & !(bd)(ikj)
                                            -c1_a(c,j)*c1_a(b,l)*c2_aa(a,d,k,i) & !(ac)(ijl)
                                            -c1_a(d,j)*c1_a(b,l)*c2_aa(c,a,k,i) & !(ad)(ijl)
                                            -c1_a(a,j)*c1_a(c,l)*c2_aa(b,d,k,i) & !(bc)(ijl)
                                            -c1_a(a,j)*c1_a(d,l)*c2_aa(c,b,k,i) & !(bd)(ijl)
                                            -c1_a(c,l)*c1_a(b,i)*c2_aa(a,d,k,j) & !(ac)(ilj)
                                            -c1_a(d,l)*c1_a(b,i)*c2_aa(c,a,k,j) & !(ad)(ilj)
                                            -c1_a(a,l)*c1_a(c,i)*c2_aa(b,d,k,j) & !(bc)(ilj)
                                            -c1_a(a,l)*c1_a(d,i)*c2_aa(c,b,k,j) & !(bd)(ilj)
                                            +c1_a(c,j)*c1_a(d,k)*c2_aa(a,b,i,l) & !(ac)(bd)(ijk)
                                            +c1_a(c,k)*c1_a(d,i)*c2_aa(a,b,j,l) & !(ac)(bd)(ikj)
                                            +c1_a(c,j)*c1_a(d,l)*c2_aa(a,b,k,i) & !(ac)(bd)(ijl)
                                            +c1_a(c,l)*c1_a(d,i)*c2_aa(a,b,k,j) & !(ac)(bd)(ilj)
                                            +c1_a(c,l)*c1_a(b,k)*c2_aa(a,d,i,j) & !(ac)(kilj)
                                            +c1_a(d,l)*c1_a(b,k)*c2_aa(c,a,i,j) & !(ad)(kilj)
                                            +c1_a(a,l)*c1_a(c,k)*c2_aa(b,d,i,j) & !(bc)(kilj)
                                            +c1_a(a,l)*c1_a(d,k)*c2_aa(c,b,i,j) & !(bd)(kilj)
                                            -c1_a(c,l)*c1_a(d,k)*c2_aa(a,b,i,j) & !(ac)(bd)(kilj)
                                            -c1_a(c,k)*c1_a(b,l)*c2_aa(a,d,i,j) & !(ac)(ik)(jl)
                                            -c1_a(d,k)*c1_a(b,l)*c2_aa(c,a,i,j) & !(ad)(ik)(jl)
                                            -c1_a(a,k)*c1_a(c,l)*c2_aa(b,d,i,j) & !(bc)(ik)(jl)
                                            -c1_a(a,k)*c1_a(d,l)*c2_aa(c,b,i,j) & !(bd)(ik)(jl)
                                            +c1_a(c,k)*c1_a(d,l)*c2_aa(a,b,i,j) & !(ac)(bd)(ik)(jl)
                                            +c1_a(a,k)*c1_a(b,l)*c2_aa(c,d,i,j)) & !(ik)(jl)
                                            -c2_aa(a,b,i,j)*c2_aa(c,d,k,l) &
                                            +c2_aa(a,b,k,j)*c2_aa(c,d,i,l) &
                                            +c2_aa(a,b,l,j)*c2_aa(c,d,k,i) &
                                            +c2_aa(a,b,i,k)*c2_aa(c,d,j,l) &
                                            +c2_aa(a,b,i,l)*c2_aa(c,d,k,j) &
                                            +c2_aa(c,b,i,j)*c2_aa(a,d,k,l) &
                                            +c2_aa(d,b,i,j)*c2_aa(c,a,k,l) &
                                            -c2_aa(a,b,k,l)*c2_aa(c,d,i,j) &
                                            -c2_aa(c,b,k,j)*c2_aa(a,d,i,l) &
                                            -c2_aa(c,b,l,j)*c2_aa(a,d,k,i) &
                                            -c2_aa(c,b,i,k)*c2_aa(a,d,j,l) &
                                            -c2_aa(c,b,i,l)*c2_aa(a,d,k,j) &
                                            -c2_aa(d,b,k,j)*c2_aa(c,a,i,l) &
                                            -c2_aa(d,b,l,j)*c2_aa(c,a,k,i) &
                                            -c2_aa(d,b,i,k)*c2_aa(c,a,j,l) &
                                            -c2_aa(d,b,i,l)*c2_aa(c,a,k,j) &
                                            +c2_aa(c,b,k,l)*c2_aa(a,d,i,j) &
                                            +c2_aa(d,b,k,l)*c2_aa(c,a,i,j) &
                                            -6*(c1_a(a,i)*c1_a(b,j)*c1_a(c,k)*c1_a(d,l) &
                                            -c1_a(a,j)*c1_a(b,i)*c1_a(c,k)*c1_a(d,l) &
                                            -c1_a(a,k)*c1_a(b,j)*c1_a(c,i)*c1_a(d,l) &
                                            -c1_a(a,l)*c1_a(b,j)*c1_a(c,k)*c1_a(d,i) &
                                            -c1_a(a,i)*c1_a(b,k)*c1_a(c,j)*c1_a(d,l) &
                                            -c1_a(a,i)*c1_a(b,l)*c1_a(c,k)*c1_a(d,j) &
                                            -c1_a(a,i)*c1_a(b,j)*c1_a(c,l)*c1_a(d,k) &
                                            +c1_a(a,j)*c1_a(b,k)*c1_a(c,i)*c1_a(d,l) &
                                            +c1_a(a,k)*c1_a(b,i)*c1_a(c,j)*c1_a(d,l) &
                                            +c1_a(a,j)*c1_a(b,l)*c1_a(c,k)*c1_a(d,i) &
                                            +c1_a(a,l)*c1_a(b,i)*c1_a(c,k)*c1_a(d,j) &
                                            +c1_a(a,k)*c1_a(b,j)*c1_a(c,l)*c1_a(d,i) &
                                            +c1_a(a,l)*c1_a(b,j)*c1_a(c,i)*c1_a(d,k) &
                                            +c1_a(a,i)*c1_a(b,k)*c1_a(c,l)*c1_a(d,j) &
                                            +c1_a(a,i)*c1_a(b,l)*c1_a(c,j)*c1_a(d,k) &
                                            +c1_a(a,j)*c1_a(b,i)*c1_a(c,l)*c1_a(d,k) &
                                            +c1_a(a,k)*c1_a(b,l)*c1_a(c,i)*c1_a(d,j) &
                                            +c1_a(a,l)*c1_a(b,k)*c1_a(c,j)*c1_a(d,i) &
                                            -c1_a(a,j)*c1_a(b,k)*c1_a(c,l)*c1_a(d,i) &
                                            -c1_a(a,j)*c1_a(b,l)*c1_a(c,i)*c1_a(d,k) &
                                            -c1_a(a,k)*c1_a(b,l)*c1_a(c,j)*c1_a(d,i) &
                                            -c1_a(a,k)*c1_a(b,i)*c1_a(c,l)*c1_a(d,j) &
                                            -c1_a(a,l)*c1_a(b,i)*c1_a(c,j)*c1_a(d,k) &
                                            -c1_a(a,l)*c1_a(b,k)*c1_a(c,i)*c1_a(d,j)))
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine analyze_t4_aaaa

    subroutine analyze_t4_aaab(rm_dscnctd, &
            c1_a, c1_b, c2_aa, c2_ab, &
            c3_aaa, c3_aab, &
            c4_aaab, &
            t4_aaab)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use sys_data
        use symmetry
        use utilities, only: t3_aab_to_t3_abb, t4_aaab_to_t4_abbb

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        integer :: indx

        integer :: ex_orbs(100)

        logical, intent(in) :: rm_dscnctd

        ! Place holders
        real(kind=8) :: c1c3, c12c2, c22, c13

        ! CI coefficients
        real(kind=8), allocatable, intent(in) :: c1_a(:,:)
        real(kind=8), allocatable, intent(in) :: c1_b(:,:)

        real(kind=8), allocatable, intent(in) :: c2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c2_ab(:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c3_aab(:,:,:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c4_aaab(:,:,:,:,:,:,:,:)

        ! T amplitudes
        real(kind=8), allocatable, intent(inout) :: t4_aaab(:,:,:,:,:,:,:,:)

        ex_orbs = 0

        do i=froz+1,occ_a
            do j=i+1,occ_a
                do k=j+1,occ_a
                    do l=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=a+1,total
                                do c=b+1,total
                                    do d=occ_b+1,total
                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = d
                                        ex_orbs(5) = i
                                        ex_orbs(6) = j
                                        ex_orbs(7) = k
                                        ex_orbs(8) = l
                                        if (.not. is_sym(ex_orbs, 8)) cycle
                                        if (rm_dscnctd .and. c4_aaab(a,b,c,d,i,j,k,l) == 0.0) cycle
                                        t4_aaab(a,b,c,d,i,j,k,l)=(c4_aaab(a,b,c,d,i,j,k,l) &
                                            -c1_a(a,i)*c3_aab(b,c,d,j,k,l) &
                                            +c1_a(a,j)*c3_aab(b,c,d,i,k,l) &
                                            +c1_a(a,k)*c3_aab(b,c,d,j,i,l) &
                                            +c1_a(b,i)*c3_aab(a,c,d,j,k,l) &
                                            +c1_a(c,i)*c3_aab(b,a,d,j,k,l) &
                                            -c1_a(b,j)*c3_aab(a,c,d,i,k,l) &
                                            -c1_a(c,j)*c3_aab(b,a,d,i,k,l) &
                                            -c1_a(b,k)*c3_aab(a,c,d,j,i,l) &
                                            -c1_a(c,k)*c3_aab(b,a,d,j,i,l) &
                                            -c1_b(d,l)*c3_aaa(a,b,c,i,j,k) &
                                            +2*(c1_a(a,i)*c1_b(d,l)*c2_aa(b,c,j,k) &
                                            -c1_a(a,j)*c1_b(d,l)*c2_aa(b,c,i,k) &
                                            -c1_a(a,k)*c1_b(d,l)*c2_aa(b,c,j,i) &
                                            -c1_a(b,i)*c1_b(d,l)*c2_aa(a,c,j,k) &
                                            -c1_a(c,i)*c1_b(d,l)*c2_aa(b,a,j,k) &
                                            +c1_a(b,j)*c1_b(d,l)*c2_aa(a,c,i,k) &
                                            +c1_a(c,j)*c1_b(d,l)*c2_aa(b,a,i,k) &
                                            +c1_a(b,k)*c1_b(d,l)*c2_aa(a,c,j,i) &
                                            +c1_a(c,k)*c1_b(d,l)*c2_aa(b,a,j,i) &
                                            +c1_a(a,i)*c1_a(b,j)*c2_ab(c,d,k,l) &
                                            -c1_a(a,j)*c1_a(b,i)*c2_ab(c,d,k,l) &
                                            -c1_a(a,k)*c1_a(b,j)*c2_ab(c,d,i,l) &
                                            -c1_a(a,i)*c1_a(b,k)*c2_ab(c,d,j,l) &
                                            -c1_a(c,i)*c1_a(b,j)*c2_ab(a,d,k,l) &
                                            -c1_a(a,i)*c1_a(c,j)*c2_ab(b,d,k,l) &
                                            +c1_a(a,j)*c1_a(b,k)*c2_ab(c,d,i,l) &
                                            +c1_a(a,k)*c1_a(b,i)*c2_ab(c,d,j,l) &
                                            +c1_a(c,j)*c1_a(b,i)*c2_ab(a,d,k,l) &
                                            +c1_a(a,j)*c1_a(c,i)*c2_ab(b,d,k,l) &
                                            +c1_a(c,k)*c1_a(b,j)*c2_ab(a,d,i,l) &
                                            +c1_a(a,k)*c1_a(c,j)*c2_ab(b,d,i,l) &
                                            +c1_a(c,i)*c1_a(b,k)*c2_ab(a,d,j,l) &
                                            +c1_a(a,i)*c1_a(c,k)*c2_ab(b,d,j,l) &
                                            -c1_a(c,j)*c1_a(b,k)*c2_ab(a,d,i,l) &
                                            -c1_a(a,j)*c1_a(c,k)*c2_ab(b,d,i,l) &
                                            -c1_a(c,k)*c1_a(b,i)*c2_ab(a,d,j,l) &
                                            -c1_a(a,k)*c1_a(c,i)*c2_ab(b,d,j,l)) &
                                            -c2_aa(a,b,i,j)*c2_ab(c,d,k,l) &
                                            +c2_aa(a,b,k,j)*c2_ab(c,d,i,l) &
                                            +c2_aa(a,b,i,k)*c2_ab(c,d,j,l) &
                                            +c2_aa(c,b,i,j)*c2_ab(a,d,k,l) &
                                            +c2_aa(a,c,i,j)*c2_ab(b,d,k,l) &
                                            -c2_aa(c,b,k,j)*c2_ab(a,d,i,l) &
                                            -c2_aa(a,c,k,j)*c2_ab(b,d,i,l) &
                                            -c2_aa(c,b,i,k)*c2_ab(a,d,j,l) &
                                            -c2_aa(a,c,i,k)*c2_ab(b,d,j,l) &
                                            -6*(c1_a(a,i)*c1_a(b,j)*c1_a(c,k)*c1_b(d,l) &
                                            -c1_a(a,j)*c1_a(b,i)*c1_a(c,k)*c1_b(d,l) &
                                            -c1_a(a,k)*c1_a(b,j)*c1_a(c,i)*c1_b(d,l) &
                                            -c1_a(a,i)*c1_a(b,k)*c1_a(c,j)*c1_b(d,l) &
                                            +c1_a(a,j)*c1_a(b,k)*c1_a(c,i)*c1_b(d,l) &
                                            +c1_a(a,k)*c1_a(b,i)*c1_a(c,j)*c1_b(d,l)))
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine analyze_t4_aaab

    subroutine analyze_t4_aabb(rm_dscnctd, &
            c1_a, c1_b, c2_aa, c2_ab, c2_bb, &
            c3_aab, c3_abb, &
            c4_aabb, &
            t4_aabb)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use sys_data
        use symmetry
        use utilities, only: t3_aab_to_t3_abb, t4_aaab_to_t4_abbb

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        integer :: indx

        integer :: ex_orbs(100)

        logical, intent(in) :: rm_dscnctd

        ! Place holders
        real(kind=8) :: c1c3, c12c2, c22, c13

        ! CI coefficients
        real(kind=8), allocatable, intent(in) :: c1_a(:,:)
        real(kind=8), allocatable, intent(in) :: c1_b(:,:)

        real(kind=8), allocatable, intent(in) :: c2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c2_bb(:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c3_abb(:,:,:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c4_aabb(:,:,:,:,:,:,:,:)

        ! T amplitudes
        real(kind=8), allocatable, intent(inout) :: t4_aabb(:,:,:,:,:,:,:,:)

        ex_orbs = 0


        do i=froz+1,occ_a
            do j=i+1,occ_a
                do k=froz+1,occ_b
                    do l=k+1,occ_b
                        do a=occ_a+1,total
                            do b=a+1,total
                                do c=occ_b+1,total
                                    do d=c+1,total
                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = d
                                        ex_orbs(5) = i
                                        ex_orbs(6) = j
                                        ex_orbs(7) = k
                                        ex_orbs(8) = l
                                        if (.not. is_sym(ex_orbs, 8)) cycle
                                        if (rm_dscnctd .and. c4_aabb(a,b,c,d,i,j,k,l) == 0.0) cycle
                                        c1c3 = (-c1_a(a,i)*c3_abb(b,c,d,j,k,l) &
                                            +c1_a(a,j)*c3_abb(b,c,d,i,k,l) &
                                            +c1_a(b,i)*c3_abb(a,c,d,j,k,l) &
                                            -c1_a(b,j)*c3_abb(a,c,d,i,k,l) &
                                            -c1_b(d,l)*c3_aab(a,b,c,i,j,k) &
                                            +c1_b(d,k)*c3_aab(a,b,c,i,j,l) &
                                            +c1_b(c,l)*c3_aab(a,b,d,i,j,k) &
                                            -c1_b(c,k)*c3_aab(a,b,d,i,j,l))

                                        c12c2 = 2*(c1_a(a,i)*c1_a(b,j)*c2_bb(c,d,k,l) &
                                            -c1_a(a,j)*c1_a(b,i)*c2_bb(c,d,k,l) &
                                            +c1_a(a,i)*c1_b(c,k)*c2_ab(b,d,j,l) &
                                            -c1_a(a,j)*c1_b(c,k)*c2_ab(b,d,i,l) &
                                            -c1_a(a,i)*c1_b(c,l)*c2_ab(b,d,j,k) &
                                            -c1_a(b,i)*c1_b(c,k)*c2_ab(a,d,j,l) &
                                            -c1_a(a,i)*c1_b(d,k)*c2_ab(b,c,j,l) &
                                            +c1_a(a,j)*c1_b(c,l)*c2_ab(b,d,i,k) &
                                            +c1_a(b,j)*c1_b(c,k)*c2_ab(a,d,i,l) &
                                            +c1_a(a,j)*c1_b(d,k)*c2_ab(b,c,i,l) &
                                            +c1_a(b,i)*c1_b(c,l)*c2_ab(a,d,j,k) &
                                            +c1_a(a,i)*c1_b(d,l)*c2_ab(b,c,j,k) &
                                            +c1_a(b,i)*c1_b(d,k)*c2_ab(a,c,j,l) &
                                            -c1_a(b,j)*c1_b(c,l)*c2_ab(a,d,i,k) &
                                            -c1_a(a,j)*c1_b(d,l)*c2_ab(b,c,i,k) &
                                            -c1_a(b,j)*c1_b(d,k)*c2_ab(a,c,i,l) &
                                            -c1_a(b,i)*c1_b(d,l)*c2_ab(a,c,j,k) &
                                            +c1_a(b,j)*c1_b(d,l)*c2_ab(a,c,i,k) &
                                            +c1_b(c,k)*c1_b(d,l)*c2_aa(a,b,i,j) &
                                            -c1_b(c,l)*c1_b(d,k)*c2_aa(a,b,i,j))

                                        c22 = (-c2_ab(a,c,i,k)*c2_ab(b,d,j,l) &
                                            +c2_ab(a,c,j,k)*c2_ab(b,d,i,l) &
                                            +c2_ab(a,c,i,l)*c2_ab(b,d,j,k) &
                                            +c2_ab(b,c,i,k)*c2_ab(a,d,j,l) &
                                            -c2_ab(a,c,j,l)*c2_ab(b,d,i,k) &
                                            -c2_ab(b,c,j,k)*c2_ab(a,d,i,l) &
                                            -c2_ab(b,c,i,l)*c2_ab(a,d,j,k) &
                                            +c2_ab(b,c,j,l)*c2_ab(a,d,i,k) &
                                            -c2_aa(a,b,i,j)*c2_bb(c,d,k,l))

                                        c13 = (-6*(c1_a(a,i)*c1_a(b,j)*c1_b(c,k)*c1_b(d,l) &
                                            -c1_a(a,j)*c1_a(b,i)*c1_b(c,k)*c1_b(d,l) &
                                            -c1_a(a,i)*c1_a(b,j)*c1_b(c,l)*c1_b(d,k) &
                                            +c1_a(a,j)*c1_a(b,i)*c1_b(c,l)*c1_b(d,k)))

                                        ! Debuggin
                                        !print '(8i3)', a, b, c, d, i, j, k, l
                                        !if (c4_aabb(a,b,c,d,i,j,k,l) /= 0.0d0) then
                                        !    print '(f13.10)', c4_aabb(a,b,c,d,i,j,k,l)
                                        !    print '(f13.10)', c1c3
                                        !    print '(f13.10)', c12c2
                                        !    print '(f13.10)', c22
                                        !    print '(f13.10)', c13
                                        !endif


                                        t4_aabb(a,b,c,d,i,j,k,l)=c4_aabb(a,b,c,d,i,j,k,l) + c1c3 + c12c2 + c22 + c13
                                        !print '(f13.10)', t4_aabb(a,b,c,d,i,j,k,l)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine analyze_t4_aabb

    subroutine analyze_t4_abbb(rm_dscnctd, &
            c1_a, c1_b, c2_ab, c2_bb, &
            c3_abb, c3_bbb, &
            c4_abbb, &
            t4_abbb)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use sys_data
        use symmetry
        use utilities, only: t3_aab_to_t3_abb, t4_aaab_to_t4_abbb

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        integer :: indx

        integer :: ex_orbs(100)

        logical, intent(in) :: rm_dscnctd

        ! Place holders
        real(kind=8) :: c1c3, c12c2, c22, c13

        ! CI coefficients
        real(kind=8), allocatable, intent(in) :: c1_a(:,:)
        real(kind=8), allocatable, intent(in) :: c1_b(:,:)

        real(kind=8), allocatable, intent(in) :: c2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c2_bb(:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: c3_bbb(:,:,:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c4_abbb(:,:,:,:,:,:,:,:)

        ! T amplitudes
        real(kind=8), allocatable, intent(inout) :: t4_abbb(:,:,:,:,:,:,:,:)

        ex_orbs = 0

        do i=froz+1,occ_a
            do j=froz+1,occ_b
                do k=j+1,occ_b
                    do l=k+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_b+1,total
                                do c=b+1,total
                                    do d=c+1,total
                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = d
                                        ex_orbs(5) = i
                                        ex_orbs(6) = j
                                        ex_orbs(7) = k
                                        ex_orbs(8) = l
                                        if (.not. is_sym(ex_orbs, 8)) cycle
                                        if (rm_dscnctd .and. c4_abbb(a,b,c,d,i,j,k,l) == 0.0) cycle

                                        t4_abbb(a,b,c,d,i,j,k,l)=(c4_abbb(a,b,c,d,i,j,k,l) &
                                            -c1_a(a,i)*c3_bbb(b,c,d,j,k,l) &
                                            -c1_b(d,l)*c3_abb(a,b,c,i,j,k) &
                                            +c1_b(d,j)*c3_abb(a,b,c,i,l,k) &
                                            +c1_b(d,k)*c3_abb(a,b,c,i,j,l) &
                                            +c1_b(b,l)*c3_abb(a,d,c,i,j,k) &
                                            +c1_b(c,l)*c3_abb(a,b,d,i,j,k) &
                                            -c1_b(b,j)*c3_abb(a,d,c,i,l,k) &
                                            -c1_b(c,j)*c3_abb(a,b,d,i,l,k) &
                                            -c1_b(b,k)*c3_abb(a,d,c,i,j,l) &
                                            -c1_b(c,k)*c3_abb(a,b,d,i,j,l) &
                                            +2*(c1_b(c,k)*c1_b(d,l)*c2_ab(a,b,i,j) &
                                            -c1_b(c,j)*c1_b(d,l)*c2_ab(a,b,i,k) &
                                            -c1_b(c,k)*c1_b(d,j)*c2_ab(a,b,i,l) &
                                            -c1_b(c,l)*c1_b(d,k)*c2_ab(a,b,i,j) &
                                            -c1_b(b,k)*c1_b(d,l)*c2_ab(a,c,i,j) &
                                            -c1_b(c,k)*c1_b(b,l)*c2_ab(a,d,i,j) &
                                            +c1_b(c,l)*c1_b(d,j)*c2_ab(a,b,i,k) &
                                            +c1_b(c,j)*c1_b(d,k)*c2_ab(a,b,i,l) &
                                            +c1_b(b,j)*c1_b(d,l)*c2_ab(a,c,i,k) &
                                            +c1_b(c,j)*c1_b(b,l)*c2_ab(a,d,i,k) &
                                            +c1_b(b,k)*c1_b(d,j)*c2_ab(a,c,i,l) &
                                            +c1_b(c,k)*c1_b(b,j)*c2_ab(a,d,i,l) &
                                            +c1_b(b,l)*c1_b(d,k)*c2_ab(a,c,i,j) &
                                            +c1_b(c,l)*c1_b(b,k)*c2_ab(a,d,i,j) &
                                            -c1_b(b,l)*c1_b(d,j)*c2_ab(a,c,i,k) &
                                            -c1_b(c,l)*c1_b(b,j)*c2_ab(a,d,i,k) &
                                            -c1_b(b,j)*c1_b(d,k)*c2_ab(a,c,i,l) &
                                            -c1_b(c,j)*c1_b(b,k)*c2_ab(a,d,i,l) &
                                            +c1_a(a,i)*c1_b(b,j)*c2_bb(c,d,k,l) &
                                            -c1_a(a,i)*c1_b(b,k)*c2_bb(c,d,j,l) &
                                            -c1_a(a,i)*c1_b(b,l)*c2_bb(c,d,k,j) &
                                            -c1_a(a,i)*c1_b(c,j)*c2_bb(b,d,k,l) &
                                            -c1_a(a,i)*c1_b(d,j)*c2_bb(c,b,k,l) &
                                            +c1_a(a,i)*c1_b(c,k)*c2_bb(b,d,j,l) &
                                            +c1_a(a,i)*c1_b(d,k)*c2_bb(c,b,j,l) &
                                            +c1_a(a,i)*c1_b(c,l)*c2_bb(b,d,k,j) &
                                            +c1_a(a,i)*c1_b(d,l)*c2_bb(c,b,k,j)) &
                                            -c2_ab(a,b,i,j)*c2_bb(c,d,k,l) &
                                            +c2_ab(a,b,i,k)*c2_bb(c,d,j,l) &
                                            +c2_ab(a,b,i,l)*c2_bb(c,d,k,j) &
                                            +c2_ab(a,c,i,j)*c2_bb(b,d,k,l) &
                                            +c2_ab(a,d,i,j)*c2_bb(c,b,k,l) &
                                            -c2_ab(a,c,i,k)*c2_bb(b,d,j,l) &
                                            -c2_ab(a,d,i,k)*c2_bb(c,b,j,l) &
                                            -c2_ab(a,c,i,l)*c2_bb(b,d,k,j) &
                                            -c2_ab(a,d,i,l)*c2_bb(c,b,k,j) &
                                            -6*(c1_a(a,i)*c1_b(b,j)*c1_b(c,k)*c1_b(d,l) &
                                            -c1_a(a,i)*c1_b(b,k)*c1_b(c,j)*c1_b(d,l) &
                                            -c1_a(a,i)*c1_b(b,l)*c1_b(c,k)*c1_b(d,j) &
                                            -c1_a(a,i)*c1_b(b,j)*c1_b(c,l)*c1_b(d,k) &
                                            +c1_a(a,i)*c1_b(b,k)*c1_b(c,l)*c1_b(d,j) &
                                            +c1_a(a,i)*c1_b(b,l)*c1_b(c,j)*c1_b(d,k)))

                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine analyze_t4_abbb

    subroutine analyze_t4_bbbb(rm_dscnctd, &
            c1_b, c2_bb, &
            c3_bbb, &
            c4_bbbb, &
            t4_bbbb)

        ! Perform the cluster analysis
        !
        ! In:
        !   c*: all CI coeffcients up to quadruples
        !
        ! In/Out:
        !   t*: all cluster amplitudes up to quadruples

        use sys_data
        use symmetry
        use utilities, only: t3_aab_to_t3_abb, t4_aaab_to_t4_abbb

        ! Indices
        integer :: i, j, k, l
        integer :: a, b, c, d

        integer :: indx

        integer :: ex_orbs(100)

        logical, intent(in) :: rm_dscnctd

        ! Place holders
        real(kind=8) :: c1c3, c12c2, c22, c13

        ! CI coefficients
        real(kind=8), allocatable, intent(in) :: c1_b(:,:)

        real(kind=8), allocatable, intent(in) :: c2_bb(:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c3_bbb(:,:,:,:,:,:)

        real(kind=8), allocatable, intent(in) :: c4_bbbb(:,:,:,:,:,:,:,:)

        ! T amplitudes
        real(kind=8), allocatable, intent(inout) :: t4_bbbb(:,:,:,:,:,:,:,:)

        ex_orbs = 0

        do i=froz+1,occ_b
            do j=i+1,occ_b
                do k=j+1,occ_b
                    do l=k+1,occ_b
                        do a=occ_b+1,total
                            do b=a+1,total
                                do c=b+1,total
                                    do d=c+1,total
                                        ex_orbs(1) = a
                                        ex_orbs(2) = b
                                        ex_orbs(3) = c
                                        ex_orbs(4) = d
                                        ex_orbs(5) = i
                                        ex_orbs(6) = j
                                        ex_orbs(7) = k
                                        ex_orbs(8) = l
                                        if (.not. is_sym(ex_orbs, 8)) cycle
                                        if (rm_dscnctd .and. c4_bbbb(a,b,c,d,i,j,k,l) == 0.0) cycle
                                        t4_bbbb(a,b,c,d,i,j,k,l) = &
                                            (c4_bbbb(a,b,c,d,i,j,k,l) &
                                            -c1_b(a,i)*c3_bbb(b,c,d,j,k,l) &
                                            +c1_b(a,j)*c3_bbb(b,c,d,i,k,l) &
                                            +c1_b(a,k)*c3_bbb(b,c,d,j,i,l) &
                                            +c1_b(a,l)*c3_bbb(b,c,d,j,k,i) &
                                            +c1_b(b,i)*c3_bbb(a,c,d,j,k,l) &
                                            +c1_b(c,i)*c3_bbb(b,a,d,j,k,l) &
                                            +c1_b(d,i)*c3_bbb(b,c,a,j,k,l) &
                                            -c1_b(b,j)*c3_bbb(a,c,d,i,k,l) &
                                            -c1_b(c,j)*c3_bbb(b,a,d,i,k,l) &
                                            -c1_b(d,j)*c3_bbb(b,c,a,i,k,l) &
                                            -c1_b(b,k)*c3_bbb(a,c,d,j,i,l) &
                                            -c1_b(c,k)*c3_bbb(b,a,d,j,i,l) &
                                            -c1_b(d,k)*c3_bbb(b,c,a,j,i,l) &
                                            -c1_b(b,l)*c3_bbb(a,c,d,j,k,i) &
                                            -c1_b(c,l)*c3_bbb(b,a,d,j,k,i) &
                                            -c1_b(d,l)*c3_bbb(b,c,a,j,k,i) &
                                            +2*(c1_b(a,i)*c1_b(b,j)*c2_bb(c,d,k,l) & !1
                                            -c1_b(a,k)*c1_b(b,j)*c2_bb(c,d,i,l) & !(ik)
                                            -c1_b(a,l)*c1_b(b,j)*c2_bb(c,d,k,i) & !(il)
                                            -c1_b(a,j)*c1_b(b,i)*c2_bb(c,d,k,l) & !(ij)
                                            -c1_b(a,i)*c1_b(b,k)*c2_bb(c,d,j,l) & !(jk)
                                            -c1_b(a,i)*c1_b(b,l)*c2_bb(c,d,k,j) & !(jl)
                                            -c1_b(c,i)*c1_b(b,j)*c2_bb(a,d,k,l) & !(ac)
                                            -c1_b(d,i)*c1_b(b,j)*c2_bb(c,a,k,l) & !(ad)
                                            -c1_b(a,i)*c1_b(c,j)*c2_bb(b,d,k,l) & !(bc)
                                            -c1_b(a,i)*c1_b(d,j)*c2_bb(c,b,k,l) & !(bd)
                                            +c1_b(c,i)*c1_b(d,j)*c2_bb(a,b,k,l) & !(ac)(bd)
                                            +c1_b(c,k)*c1_b(b,j)*c2_bb(a,d,i,l) & !(ac)(ik)
                                            +c1_b(d,k)*c1_b(b,j)*c2_bb(c,a,i,l) & !(ad)(ik)
                                            +c1_b(a,k)*c1_b(c,j)*c2_bb(b,d,i,l) & !(bc)(ik)
                                            +c1_b(a,k)*c1_b(d,j)*c2_bb(c,b,i,l) & !(bd)(ik)
                                            +c1_b(c,l)*c1_b(b,j)*c2_bb(a,d,k,i) & !(ac)(il)
                                            +c1_b(d,l)*c1_b(b,j)*c2_bb(c,a,k,i) & !(ad)(il)
                                            +c1_b(a,l)*c1_b(c,j)*c2_bb(b,d,k,i) & !(bc)(il)
                                            +c1_b(a,l)*c1_b(d,j)*c2_bb(c,b,k,i) & !(bd)(il)
                                            +c1_b(c,j)*c1_b(b,i)*c2_bb(a,d,k,l) & !(ac)(ij)
                                            +c1_b(d,j)*c1_b(b,i)*c2_bb(c,a,k,l) & !(ad)(ij)
                                            +c1_b(a,j)*c1_b(c,i)*c2_bb(b,d,k,l) & !(bc)(ij)
                                            +c1_b(a,j)*c1_b(d,i)*c2_bb(c,b,k,l) & !(bd)(ij)
                                            +c1_b(c,i)*c1_b(b,k)*c2_bb(a,d,j,l) & !(ac)(jk)
                                            +c1_b(d,i)*c1_b(b,k)*c2_bb(c,a,j,l) & !(ad)(jk)
                                            +c1_b(a,i)*c1_b(c,k)*c2_bb(b,d,j,l) & !(bc)(jk)
                                            +c1_b(a,i)*c1_b(d,k)*c2_bb(c,b,j,l) & !(bd)(jk)
                                            +c1_b(c,i)*c1_b(b,l)*c2_bb(a,d,k,j) & !(ac)(jl)
                                            +c1_b(d,i)*c1_b(b,l)*c2_bb(c,a,k,j) & !(ad)(jl)
                                            +c1_b(a,i)*c1_b(c,l)*c2_bb(b,d,k,j) & !(bc)(jl)
                                            +c1_b(a,i)*c1_b(d,l)*c2_bb(c,b,k,j) & !(bd)(jl)
                                            +c1_b(a,j)*c1_b(b,k)*c2_bb(c,d,i,l) & !(ijk)
                                            +c1_b(a,k)*c1_b(b,i)*c2_bb(c,d,j,l) & !(ikj)
                                            +c1_b(a,j)*c1_b(b,l)*c2_bb(c,d,k,i) & !(ijl)
                                            +c1_b(a,l)*c1_b(b,i)*c2_bb(c,d,k,j) & !(ilj)
                                            -c1_b(a,l)*c1_b(b,k)*c2_bb(c,d,i,j) & !(kilj)
                                            -c1_b(c,k)*c1_b(d,j)*c2_bb(a,b,i,l) & !(ac)(bd)(ik)
                                            -c1_b(c,l)*c1_b(d,j)*c2_bb(a,b,k,i) & !(ac)(bd)(il)
                                            -c1_b(c,j)*c1_b(d,i)*c2_bb(a,b,k,l) & !(ac)(bd)(ij)
                                            -c1_b(c,i)*c1_b(d,k)*c2_bb(a,b,j,l) & !(ac)(bd)(jk)
                                            -c1_b(c,i)*c1_b(d,l)*c2_bb(a,b,k,j) & !(ac)(bd)(jl)
                                            -c1_b(c,j)*c1_b(b,k)*c2_bb(a,d,i,l) & !(ac)(ijk)
                                            -c1_b(d,j)*c1_b(b,k)*c2_bb(c,a,i,l) & !(ad)(ijk)
                                            -c1_b(a,j)*c1_b(c,k)*c2_bb(b,d,i,l) & !(bc)(ijk)
                                            -c1_b(a,j)*c1_b(d,k)*c2_bb(c,b,i,l) & !(bd)(ijk)
                                            -c1_b(c,k)*c1_b(b,i)*c2_bb(a,d,j,l) & !(ac)(ikj)
                                            -c1_b(d,k)*c1_b(b,i)*c2_bb(c,a,j,l) & !(ad)(ikj)
                                            -c1_b(a,k)*c1_b(c,i)*c2_bb(b,d,j,l) & !(bc)(ikj)
                                            -c1_b(a,k)*c1_b(d,i)*c2_bb(c,b,j,l) & !(bd)(ikj)
                                            -c1_b(c,j)*c1_b(b,l)*c2_bb(a,d,k,i) & !(ac)(ijl)
                                            -c1_b(d,j)*c1_b(b,l)*c2_bb(c,a,k,i) & !(ad)(ijl)
                                            -c1_b(a,j)*c1_b(c,l)*c2_bb(b,d,k,i) & !(bc)(ijl)
                                            -c1_b(a,j)*c1_b(d,l)*c2_bb(c,b,k,i) & !(bd)(ijl)
                                            -c1_b(c,l)*c1_b(b,i)*c2_bb(a,d,k,j) & !(ac)(ilj)
                                            -c1_b(d,l)*c1_b(b,i)*c2_bb(c,a,k,j) & !(ad)(ilj)
                                            -c1_b(a,l)*c1_b(c,i)*c2_bb(b,d,k,j) & !(bc)(ilj)
                                            -c1_b(a,l)*c1_b(d,i)*c2_bb(c,b,k,j) & !(bd)(ilj)
                                            +c1_b(c,j)*c1_b(d,k)*c2_bb(a,b,i,l) & !(ac)(bd)(ijk)
                                            +c1_b(c,k)*c1_b(d,i)*c2_bb(a,b,j,l) & !(ac)(bd)(ikj)
                                            +c1_b(c,j)*c1_b(d,l)*c2_bb(a,b,k,i) & !(ac)(bd)(ijl)
                                            +c1_b(c,l)*c1_b(d,i)*c2_bb(a,b,k,j) & !(ac)(bd)(ilj)
                                            +c1_b(c,l)*c1_b(b,k)*c2_bb(a,d,i,j) & !(ac)(kilj)
                                            +c1_b(d,l)*c1_b(b,k)*c2_bb(c,a,i,j) & !(ad)(kilj)
                                            +c1_b(a,l)*c1_b(c,k)*c2_bb(b,d,i,j) & !(bc)(kilj)
                                            +c1_b(a,l)*c1_b(d,k)*c2_bb(c,b,i,j) & !(bd)(kilj)
                                            -c1_b(c,l)*c1_b(d,k)*c2_bb(a,b,i,j) & !(ac)(bd)(kilj)
                                            -c1_b(c,k)*c1_b(b,l)*c2_bb(a,d,i,j) & !(ac)(ik)(jl)
                                            -c1_b(d,k)*c1_b(b,l)*c2_bb(c,a,i,j) & !(ad)(ik)(jl)
                                            -c1_b(a,k)*c1_b(c,l)*c2_bb(b,d,i,j) & !(bc)(ik)(jl)
                                            -c1_b(a,k)*c1_b(d,l)*c2_bb(c,b,i,j) & !(bd)(ik)(jl)
                                            +c1_b(c,k)*c1_b(d,l)*c2_bb(a,b,i,j) & !(ac)(bd)(ik)(jl)
                                            +c1_b(a,k)*c1_b(b,l)*c2_bb(c,d,i,j)) & !(ik)(jl)
                                            -c2_bb(a,b,i,j)*c2_bb(c,d,k,l) &
                                            +c2_bb(a,b,k,j)*c2_bb(c,d,i,l) &
                                            +c2_bb(a,b,l,j)*c2_bb(c,d,k,i) &
                                            +c2_bb(a,b,i,k)*c2_bb(c,d,j,l) &
                                            +c2_bb(a,b,i,l)*c2_bb(c,d,k,j) &
                                            +c2_bb(c,b,i,j)*c2_bb(a,d,k,l) &
                                            +c2_bb(d,b,i,j)*c2_bb(c,a,k,l) &
                                            -c2_bb(a,b,k,l)*c2_bb(c,d,i,j) &
                                            -c2_bb(c,b,k,j)*c2_bb(a,d,i,l) &
                                            -c2_bb(c,b,l,j)*c2_bb(a,d,k,i) &
                                            -c2_bb(c,b,i,k)*c2_bb(a,d,j,l) &
                                            -c2_bb(c,b,i,l)*c2_bb(a,d,k,j) &
                                            -c2_bb(d,b,k,j)*c2_bb(c,a,i,l) &
                                            -c2_bb(d,b,l,j)*c2_bb(c,a,k,i) &
                                            -c2_bb(d,b,i,k)*c2_bb(c,a,j,l) &
                                            -c2_bb(d,b,i,l)*c2_bb(c,a,k,j) &
                                            +c2_bb(c,b,k,l)*c2_bb(a,d,i,j) &
                                            +c2_bb(d,b,k,l)*c2_bb(c,a,i,j) &
                                            -6*(c1_b(a,i)*c1_b(b,j)*c1_b(c,k)*c1_b(d,l) &
                                            -c1_b(a,j)*c1_b(b,i)*c1_b(c,k)*c1_b(d,l) &
                                            -c1_b(a,k)*c1_b(b,j)*c1_b(c,i)*c1_b(d,l) &
                                            -c1_b(a,l)*c1_b(b,j)*c1_b(c,k)*c1_b(d,i) &
                                            -c1_b(a,i)*c1_b(b,k)*c1_b(c,j)*c1_b(d,l) &
                                            -c1_b(a,i)*c1_b(b,l)*c1_b(c,k)*c1_b(d,j) &
                                            -c1_b(a,i)*c1_b(b,j)*c1_b(c,l)*c1_b(d,k) &
                                            +c1_b(a,j)*c1_b(b,k)*c1_b(c,i)*c1_b(d,l) &
                                            +c1_b(a,k)*c1_b(b,i)*c1_b(c,j)*c1_b(d,l) &
                                            +c1_b(a,j)*c1_b(b,l)*c1_b(c,k)*c1_b(d,i) &
                                            +c1_b(a,l)*c1_b(b,i)*c1_b(c,k)*c1_b(d,j) &
                                            +c1_b(a,k)*c1_b(b,j)*c1_b(c,l)*c1_b(d,i) &
                                            +c1_b(a,l)*c1_b(b,j)*c1_b(c,i)*c1_b(d,k) &
                                            +c1_b(a,i)*c1_b(b,k)*c1_b(c,l)*c1_b(d,j) &
                                            +c1_b(a,i)*c1_b(b,l)*c1_b(c,j)*c1_b(d,k) &
                                            +c1_b(a,j)*c1_b(b,i)*c1_b(c,l)*c1_b(d,k) &
                                            +c1_b(a,k)*c1_b(b,l)*c1_b(c,i)*c1_b(d,j) &
                                            +c1_b(a,l)*c1_b(b,k)*c1_b(c,j)*c1_b(d,i) &
                                            -c1_b(a,j)*c1_b(b,k)*c1_b(c,l)*c1_b(d,i) &
                                            -c1_b(a,j)*c1_b(b,l)*c1_b(c,i)*c1_b(d,k) &
                                            -c1_b(a,k)*c1_b(b,l)*c1_b(c,j)*c1_b(d,i) &
                                            -c1_b(a,k)*c1_b(b,i)*c1_b(c,l)*c1_b(d,j) &
                                            -c1_b(a,l)*c1_b(b,i)*c1_b(c,j)*c1_b(d,k) &
                                            -c1_b(a,l)*c1_b(b,k)*c1_b(c,i)*c1_b(d,j)))
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine analyze_t4_bbbb

    subroutine write_t3(cc, print_doubles, t1_a, t1_b, &
            t2_aa, t2_ab, t2_bb, &
            t3_aaa, t3_aab, t3_abb, t3_bbb)

        ! Write the CC files required for externally corrected CC calculations.
        !
        ! In:
        !   print_doubles: if true writes down single and double amplitudes as well
        !   t1_x: singles amplitude array (x can be a for alpha, or b for beta)
        !   t2_xx: doubles amplitude array
        !   t3_xxx: triples amplitude array
        !   t4_xxxx: quadruples amplitude array

        use system, only: cc_t
        use sys_data

        type(cc_t), intent(inout), target :: cc
        logical, intent(in) :: print_doubles

        real(kind=8), allocatable, intent(in) :: t1_a(:,:)
        real(kind=8), allocatable, intent(in) :: t1_b(:,:)

        real(kind=8), allocatable, intent(in) :: t2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: t2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(in) :: t2_bb(:,:,:,:)

        real(kind=8), allocatable, intent(in) :: t3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: t3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: t3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(in) :: t3_bbb(:,:,:,:,:,:)

        integer :: i, j, k, l
        integer :: a, b, c, d
        real(kind=8), pointer :: t3(:) => null()

        integer :: indx

        t3 => cc%t_vec

        if (print_doubles) then
            indx = 1

            ! T1A
            do i=froz+1,occ_a
                do a=occ_a+1,total
                    t3(indx) = t1_a(a,i)
                    indx = indx + 1
                enddo
            enddo

            ! T1B
            do i=froz+1,occ_b
                do a=occ_b+1,total
                    t3(indx) = t1_b(a,i)
                    indx = indx + 1
                enddo
            enddo

            ! T2A
            do i=froz+1,occ_a
                do j=froz+1,occ_a
                    do a=occ_a+1,total
                        do b=occ_a+1,total
                            t3(indx) = t2_aa(a,b,i,j)
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
                            t3(indx) = t2_ab(a,b,i,j)
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
                            t3(indx) = t2_bb(a,b,i,j)
                            indx = indx + 1
                        enddo
                    enddo
                enddo
            enddo

        else
            indx = i_t3a
        endif

        ! T3A
        do i=froz+1,occ_a
            do j=froz+1,occ_a
                do k=froz+1,occ_a
                    do a=occ_a+1,total
                        do b=occ_a+1,total
                            do c=occ_a+1,total
                                t3(indx) = t3_aaa(a,b,c,i,j,k)
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
                                t3(indx) = t3_aab(a,b,c,i,j,k)
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
                                t3(indx) = t3_abb(a,b,c,i,j,k)
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
                                t3(indx) = t3_bbb(a,b,c,i,j,k)
                                indx = indx + 1
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo

    end subroutine write_t3

    subroutine write_t4_aaaa(t4_aaaa)
        use const, only: ta
        use sys_data
        real(kind=8), allocatable, intent(in) :: t4_aaaa(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_aaaa_reorder(:,:,:,:,:,:,:,:)
        integer :: i, j, k, l, a, b, c, d
        allocate(t4_aaaa_reorder(occ_a+1:total,occ_a+1:total,occ_a+1:total,occ_a+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
        t4_aaaa_reorder = 0.0d0
        do i=froz+1,occ_a
            do j=froz+1,occ_a
                do k=froz+1,occ_a
                    do l=froz+1,occ_a
                        do a=occ_a+1,total
                            do b=occ_a+1,total
                                do c=occ_a+1,total
                                    do d=occ_a+1,total
                                        t4_aaaa_reorder(d,c,b,a,l,k,j,i) = t4_aaaa(a,b,c,d,i,j,k,l)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        open(unit=ta,file='t4a.bin',status='unknown',form='unformatted')
        write(ta) t4_aaaa_reorder
        deallocate(t4_aaaa_reorder)

    end subroutine write_t4_aaaa

    subroutine write_t4_aaab(t4_aaab)
        use const, only: tb
        use sys_data
        real(kind=8), allocatable, intent(in) :: t4_aaab(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_aaab_reorder(:,:,:,:,:,:,:,:)
        integer :: i, j, k, l, a, b, c, d
        allocate(t4_aaab_reorder(occ_a+1:total,occ_a+1:total,occ_a+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_a,froz+1:occ_b))
        t4_aaab_reorder = 0.0d0
        do i=froz+1,occ_a
            do j=froz+1,occ_a
                do k=froz+1,occ_a
                    do l=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_a+1,total
                                do c=occ_a+1,total
                                    do d=occ_b+1,total
                                        t4_aaab_reorder(d,c,b,a,l,k,j,i) = t4_aaab(a,b,c,d,i,j,k,l)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        open(unit=tb,file='t4b.bin',status='unknown',form='unformatted')
        write(tb) t4_aaab_reorder
        deallocate(t4_aaab_reorder)
    end subroutine write_t4_aaab

    subroutine write_t4_aabb(t4_aabb)
        use const, only: tc
        use sys_data
        real(kind=8), allocatable, intent(in) :: t4_aabb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_aabb_reorder(:,:,:,:,:,:,:,:)
        integer :: i, j, k, l, a, b, c, d
        allocate(t4_aabb_reorder(occ_a+1:total,occ_a+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_a,froz+1:occ_b,froz+1:occ_b))
        t4_aabb_reorder = 0.0d0
        do i=froz+1,occ_a
            do j=froz+1,occ_a
                do k=froz+1,occ_b
                    do l=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_a+1,total
                                do c=occ_b+1,total
                                    do d=occ_b+1,total
                                        t4_aabb_reorder(d,c,b,a,l,k,j,i) = t4_aabb(a,b,c,d,i,j,k,l)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        open(unit=tc,file='t4c.bin',status='unknown',form='unformatted')
        write(tc) t4_aabb_reorder
        deallocate(t4_aabb_reorder)
    end subroutine write_t4_aabb

    subroutine write_t4_abbb(t4_abbb)
        use const, only: td
        use sys_data
        real(kind=8), allocatable, intent(in) :: t4_abbb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_abbb_reorder(:,:,:,:,:,:,:,:)
        integer :: i, j, k, l, a, b, c, d
        allocate(t4_abbb_reorder(occ_a+1:total,occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_a,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
        t4_abbb_reorder = 0.0d0
        do i=froz+1,occ_a
            do j=froz+1,occ_b
                do k=froz+1,occ_b
                    do l=froz+1,occ_b
                        do a=occ_a+1,total
                            do b=occ_b+1,total
                                do c=occ_b+1,total
                                    do d=occ_b+1,total
                                        t4_abbb_reorder(d,c,b,a,l,k,j,i) = t4_abbb(a,b,c,d,i,j,k,l)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        open(unit=td,file='t4d.bin',status='unknown',form='unformatted')
        write(td) t4_abbb_reorder
        deallocate(t4_abbb_reorder)
    end subroutine write_t4_abbb

    subroutine write_t4_bbbb(t4_bbbb)

        use const, only: te
        use sys_data
        real(kind=8), allocatable, intent(in) :: t4_bbbb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable :: t4_bbbb_reorder(:,:,:,:,:,:,:,:)
        integer :: i, j, k, l, a, b, c, d
        allocate(t4_bbbb_reorder(occ_b+1:total,occ_b+1:total,occ_b+1:total,occ_b+1:total, &
            froz+1:occ_b,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
        t4_bbbb_reorder = 0.0d0

        do i=froz+1,occ_b
            do j=froz+1,occ_b
                do k=froz+1,occ_b
                    do l=froz+1,occ_b
                        do a=occ_b+1,total
                            do b=occ_b+1,total
                                do c=occ_b+1,total
                                    do d=occ_b+1,total
                                        t4_bbbb_reorder(d,c,b,a,l,k,j,i) = t4_bbbb(a,b,c,d,i,j,k,l)
                                    enddo
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
        open(unit=te,file='t4e.bin',status='unknown',form='unformatted')
        write(te) t4_bbbb_reorder

        deallocate(t4_bbbb_reorder)
    end subroutine write_t4_bbbb


end module cluster_analysis
