! [TODO] This module is basically a compatibility layer has to be improved
module update_cc

    ! Module used for wrapping the automatically generated CC
    ! routines. This is the module where one would want to add routines
    ! in order to extend ccq's functionality (i.e. add new methods)

    implicit none

contains

    subroutine update_clusters_t4(sys, run, cc)

        ! Update all cluster operators up to T_4 during the Jacobi iteration

        ! In:
        !   sys: molecular system data
        !   run: runtime configuration data

        ! In/Out:
        !   cc: CC vector and data

        use const, only: sp, p, ta, tb, tc, td, te
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use stoch_cc, only: update_stoch_t3
        use cc_utils, only: antisym_t

        use contract_t3, only: hbar_on_t2a, hbar_on_t2b, hbar_on_t2c

        implicit none

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout), target :: cc

        real(p), pointer :: t(:) => null()
        real(p), pointer :: t2_mc(:) => null()

        real(p), allocatable :: v1(:,:)
        real(p), allocatable :: v2(:,:,:,:)
        real(p), allocatable :: v3(:,:,:,:,:,:)


        ! Adapt for Ilias old scheme
        real(sp), allocatable :: t4ae(:)

        integer :: n0, n1, n2 ,n3
        integer :: k1, k2, k3, k4
        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d

        real(sp) :: diag1, diag2, diag3, diag4, diag5
        real(sp) :: dt3diag3, dt3diag4
        real(sp) :: t1diag1, t1diag2, t1diag3, t1diag4
        real(sp) :: t2diag3, t2diag4, t2diag5
        real(sp) :: t3diag1, t3diag2, t3diag3, t3diag4, t3diag5

        integer :: iactocca, iactoccb, iactunoa, iactunob
        integer :: iactindt
        integer :: iactindq

        integer :: i

        ! Compatibility layer
        call diag_compat(cc%acc, &
            diag1, diag2, diag3, diag4, diag5, &
            dt3diag3, dt3diag4, &
            t1diag1, t1diag2, t1diag3, t1diag4, &
            t2diag3, t2diag4, t2diag5, &
            t3diag1, t3diag2, t3diag3, t3diag4, t3diag5)

        iactocca = sys%act_occ_a
        iactoccb = sys%act_occ_b
        iactunoa = sys%act_unocc_a
        iactunob = sys%act_unocc_b
        iactindt = run%act_ind_t
        iactindq = run%act_ind_q

        n0 = sys%froz
        n1 = sys%occ_a
        n2 = sys%occ_b
        n3 = sys%orbs
        k1 = n1 - n0
        k2 = n2 - n0
        k3 = n3 - n1
        k4 = n3 - n2

        t => cc%t_vec
        t2_mc => cc%acc%t2_mc

        k1a = cc%pos(1)
        k1b = cc%pos(2)
        k2a = cc%pos(3)
        k2b = cc%pos(4)
        k2c = cc%pos(5)
        k3a = cc%pos(6)
        k3b = cc%pos(7)
        k3c = cc%pos(8)
        k3d = cc%pos(9)

        associate(fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
                intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab)

            allocate(v2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            v2=0.0_p

            ! Externally corrected pre-load
            if (run%ext_cor) then
                v2(:,:,:,:) = cc%ext_cor%t2a(:,:,:,:)
                call hbar_on_t2a(sys, cc, v2)
            endif

            call sumx2143(n0,n3,n1,n3,n1,n3,n0,n1,n0,n1,v2,intr,1.000)

            call t2a_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,run%shift,v2, &
                run%ext_cor,fockr,fockb,intr,intb,intm,diag1,diag2,diag3,diag4,diag5, &
                t1diag1,t1diag2,t1diag3,t1diag4, &
                dt3diag3,dt3diag4, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c),t(k3a),t(k3b),t(k3c),t(k3d), &
                t2_mc(1),t2_mc(k2b-k2a+1),t2_mc(k2c-k2a+1))
            deallocate(v2)

            if(run%rhf)then
                do i=k2a,k2b-1
                    t(i+k2c-k2a)=t(i)
                enddo
            else
                allocate(v2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
                v2=0.0_p

                if (run%ext_cor) then
                    v2(:,:,:,:) = cc%ext_cor%t2c(:,:,:,:)
                    call hbar_on_t2c(sys, cc, v2)
                endif

                call sumx2143(n0,n3,n2,n3,n2,n3,n0,n2,n0,n2,v2,intb,1.000)

                call t2c_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,run%shift,v2, &
                    run%ext_cor,fockr,fockb,intr,intb,intm,diag1,diag2,diag3,diag4,diag5, &
                    t1diag1,t1diag2,t1diag3,t1diag4, &
                    dt3diag3,dt3diag4, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c),t(k3a),t(k3b),t(k3c),t(k3d), &
                    t2_mc(1),t2_mc(k2b-k2a+1),t2_mc(k2c-k2a+1))
                deallocate(v2)
            endif

            allocate(v2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            v2=0.0_p

            if (run%ext_cor) then
                v2(:,:,:,:) = cc%ext_cor%t2b(:,:,:,:)
                call hbar_on_t2b(sys, cc, v2)
            endif

            call sumx2143(n0,n3,n2,n3,n1,n3,n0,n2,n0,n1,v2,intm,1.000)

            call t2b_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,run%shift,v2, &
                run%ext_cor,fockr,fockb,intr,intb,intm,diag1,diag2,diag3,diag4,diag5, &
                t1diag1,t1diag2,t1diag3,t1diag4, &
                dt3diag3,dt3diag4, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c),t(k3a),t(k3b),t(k3c),t(k3d), &
                t2_mc(1),t2_mc(k2b-k2a+1),t2_mc(k2c-k2a+1))
            deallocate(v2)

            allocate(v1(n1+1:n3,n0+1:n1))
            v1=0.0_p

            call sumx12(0,n3,n1,n3,n0,n1,v1,fockr, 1.000)   !!!!!not sure if the 0 should be n0

            if (run%ext_cor) then
                v1(:,:) = cc%ext_cor%t1a(:,:)
            endif

            call t1a_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%shift,v1, &
                fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                t(k3a),t(k3b),t(k3c),t(k3d))
            deallocate(v1)


            if(run%rhf)then
                do i=k1a,k1b-1
                    t(i+k1b-k1a)=t(i)
                enddo
            else
                allocate(v1(n2+1:n3,n0+1:n2))
                v1=0.0_p

                call sumx12(0,n3,n2,n3,n0,n2,v1,fockb, 1.000)

                if (run%ext_cor) then
                    v1(:,:) = cc%ext_cor%t1b(:,:)
                endif

                call t1b_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%shift,v1, &
                    fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                    t(k3a),t(k3b),t(k3c),t(k3d))
                deallocate(v1)
            endif

            !do i=k3a, k3b-1
            !    if (dabs(t(i)) > 1.0d-5) print *, t(i)
            !enddo
            !if (run%stoch) call update_stoch_t3(sys, run, cc)
            !if (run%stoch) then
            !if (.false.) then

            !allocate(V3(N1+1:N3,N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1,N0+1:N1))
            !V3=0.0_p

            !call t3A_update_stoch(N0,N1,N2,N3,K1,K2,K3,K4,run%shift,V3,&
            !    FockR,FockB,IntR,IntB,IntM,&
            !    t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),&
            !    t(k3a),t(k3b),t(k3c),t(k3d),cc%stoch%o3)
            !deallocate(V3)

            !allocate(V3(N2+1:N3,N1+1:N3,N1+1:N3,N0+1:N2,N0+1:N1,N0+1:N1))
            !V3=0.0_p

            !call t3B_update_stoch(N0,N1,N2,N3,K1,K2,K3,K4,run%shift,V3,&
            !    FockR,FockB,IntR,IntB,IntM,&
            !    t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),&
            !    t(k3a),t(k3b),t(k3c),t(k3d),cc%stoch%o3)
            !deallocate(V3)

            !allocate(V3(N2+1:N3,N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N2,N0+1:N1))
            !V3=0.0_p

            !call t3C_update_stoch(N0,N1,N2,N3,K1,K2,K3,K4,run%shift,V3,&
            !    FockR,FockB,IntR,IntB,IntM,&
            !    t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),&
            !    t(k3a),t(k3b),t(k3c),t(k3d),cc%stoch%o3)
            !deallocate(V3)

            !allocate(V3(N2+1:N3,N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2,N0+1:N2))
            !V3=0.0_p

            !call t3D_update_stoch(N0,N1,N2,N3,K1,K2,K3,K4,run%shift,V3,&
            !    FockR,FockB,IntR,IntB,IntM,&
            !    t(K1A),t(K1B),t(K2A),t(K2B),t(K2C),&
            !    t(k3a),t(k3b),t(k3c),t(k3d),cc%stoch%o3)
            !deallocate(V3)

            !endif

            if (.not. run%ext_cor .and. .not. run%stoch) then
                if(run%lvl_t)then

                    allocate(v3(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
                    v3=0.0d0
                    call t3a_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_q,run%shift,v3, &
                        fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b),t(k3c),t(k3d),iactocca,iactunoa,iactindt, &
                        t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
                    deallocate(v3)

                    if(run%rhf)then
                        do i=k3a,k3b-1
                            t(i+k3d-k3a)=t(i)
                        enddo
                    else
                        allocate(v3(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
                        v3=0.0d0
                        call t3d_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_q,run%shift,v3, &
                            fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                            t(k3a),t(k3b),t(k3c),t(k3d),iactoccb,iactunob,iactindt, &
                            t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
                        deallocate(v3)
                    endif

                    allocate(v3(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
                    v3=0.0d0
                    call t3b_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_q,run%shift,v3, &
                        fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b),t(k3c),t(k3d), &
                        iactocca,iactoccb,iactunoa,iactunob,iactindt, &
                        t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
                    deallocate(v3)

                    if(run%rhf)then
                        call tran3bto3c(n0,n1,n2,n3,t(k3b),t(k3c))
                    else
                        allocate(v3(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
                        v3=0.0d0
                        call t3c_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_q,run%shift,v3, &
                            fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                            t(k3a),t(k3b),t(k3c),t(k3d), &
                            iactocca,iactoccb,iactunoa,iactunob,iactindt, &
                            t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
                        deallocate(v3)
                    endif
                endif

                if(run%lvl_q)then
                    call t4a_update(n0,n1,n2,n3,k1,k2,k3,k4,run%shift, &
                        fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b),t(k3c),t(k3d), &
                        iactocca,iactunoa,iactindq)

                    call t4b_update(n0,n1,n2,n3,k1,k2,k3,k4,run%shift, &
                        fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b),t(k3c),t(k3d), &
                        iactocca,iactoccb,iactunoa,iactunob,iactindq)

                    call t4c_update(n0,n1,n2,n3,k1,k2,k3,k4,run%shift, &
                        fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b),t(k3c),t(k3d), &
                        iactocca,iactoccb,iactunoa,iactunob,iactindq)

                    if(run%rhf)then
                        call tran4bto4d(n0,n1,n2,n3)
                    else
                        call t4d_update(n0,n1,n2,n3,k1,k2,k3,k4,run%shift, &
                            fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                            t(k3a),t(k3b),t(k3c),t(k3d), &
                            iactocca,iactoccb,iactunoa,iactunob,iactindq)
                    endif

                    if(run%rhf)then
                        allocate(t4ae(k1*k1*k1*k1*k3*k3*k3*k3))
                        rewind(ta)
                        read(ta)t4ae
                        write(te)t4ae
                        deallocate(t4ae)
                    else
                        call t4e_update(n0,n1,n2,n3,k1,k2,k3,k4,run%shift, &
                            fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                            t(k3a),t(k3b),t(k3c),t(k3d), &
                            iactoccb,iactunob,iactindq)
                    endif
                endif
            endif

        end associate

    end subroutine update_clusters_t4

    subroutine update_clusters_t3_opt(sys, run, cc)

        ! Update clusters up to T_3, including active-space variants
        ! using the optimized sorted integral updates.

        ! In:
        !   sys: molecular system data
        !   run: runtime configuration data

        ! In/Out:
        !   cc: CC vector and data

        use const, only: sp, p, ta, tb, tc, td, te, &
            part_ints_a_unit, part_ints_b_unit, part_ints_c_unit
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use errors, only: stop_all

        implicit none

        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout), target :: cc

        real(p), pointer :: t(:) => null()

        ! Adapt for Jun's old scheme
        real(p), allocatable :: v2b(:,:,:,:)
        real(p), allocatable :: v2a(:,:,:,:)
        real(p), allocatable :: v2c(:,:,:,:)
        real(p), allocatable :: v1a(:,:)
        real(p), allocatable :: v1b(:,:)
        real(p), allocatable :: v3a(:,:,:,:,:,:)
        real(p), allocatable :: v3b(:,:,:,:,:,:)
        real(p), allocatable :: v3c(:,:,:,:,:,:)
        real(p), allocatable :: v3d(:,:,:,:,:,:)

        real(p), allocatable :: ht1a(:,:)
        real(p), allocatable :: ht1b(:,:)

        real(p), allocatable :: ht2a(:,:,:,:)
        real(p), allocatable :: ht2b(:,:,:,:)
        real(p), allocatable :: ht2c(:,:,:,:)

        real(p), allocatable :: ht3a(:,:,:,:,:,:)

        real(p), allocatable :: ht3b1(:,:,:,:,:,:)
        real(p), allocatable :: ht3b2(:,:,:,:,:,:)
        real(p), allocatable :: ht3b3(:,:,:,:,:,:)
        real(p), allocatable :: ht3b4(:,:,:,:,:,:)

        real(p), allocatable :: ht3c1(:,:,:,:,:,:)
        real(p), allocatable :: ht3c2(:,:,:,:,:,:)
        real(p), allocatable :: ht3c3(:,:,:,:,:,:)
        real(p), allocatable :: ht3c4(:,:,:,:,:,:)

        real(p), allocatable :: ht3d(:,:,:,:,:,:)

#ifdef DISABLE_OPT_T3
        call stop_all('update_clusters_t3_opt', 'ERROR: Feature not compiled.')
#else

        ! Compatibility vars
        ! [TODO] all this has to be removed
        integer :: n0, n1, n2 ,n3, m1, m2
        integer :: k1, k2, k3, k4
        integer :: k5, k6 ,k7, k8, k9, k0

        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d
        integer :: k3b1, k3b2, k3b3, k3b4
        integer :: k3c1, k3c2, k3c3, k3c4

        integer :: i, i0, i1
        integer :: ipa, ipb, ipc

        ! Compatibility layer
        n0 = sys%froz
        n1 = sys%occ_a
        n2 = sys%occ_b
        n3 = sys%orbs
        m1 = sys%act_occ_b
        m2 = sys%act_unocc_a

        ipa = part_ints_a_unit
        ipb = part_ints_b_unit
        ipc = part_ints_c_unit

        ! K1 = # of occ alpha
        k1 = sys%occ_a - sys%froz
        ! K3 = # of unocc alpha
        k3 = sys%orbs - sys%occ_a
        ! K2 = # of occ beta
        k2 = sys%occ_b - sys%froz
        ! K4 = # of unocc beta
        k4 = sys%orbs - sys%occ_b
        ! K5 = # of non-active occ
        k5 = m1 - sys%froz
        ! K6 = # of non-active unocc
        k6 = sys%orbs - m2
        ! K7 = # of active occ alpha
        k7 = sys%occ_a - m1
        ! K8 = # of active occ beta
        k8 = sys%occ_b - m1
        ! K9 = # of active unocc alpha
        k9 = m2 - sys%occ_a
        ! K0 = # of active unocc beta
        k0 = m2 - sys%occ_b

        t => cc%t_vec

        k1a = cc%pos(1)
        k1b = cc%pos(2)
        k2a = cc%pos(3)
        k2b = cc%pos(4)
        k2c = cc%pos(5)
        k3a = cc%pos(6)
        k3b1 = cc%pos(7)
        k3b2 = cc%pos(8)
        k3b3 = cc%pos(9)
        k3b4 = cc%pos(10)
        k3c1 = cc%pos(11)
        k3c2 = cc%pos(12)
        k3c3 = cc%pos(13)
        k3c4 = cc%pos(14)
        k3d = cc%pos(15)

        associate(fahh=>sys%ints%fahh, fahp=>sys%ints%fahp, fapp=>sys%ints%fapp, &
                fbhh=>sys%ints%fbhh, fbhp=>sys%ints%fbhp, fbpp=>sys%ints%fbpp, &
                vahhhh=>sys%ints%vahhhh, vahhhp=>sys%ints%vahhhp, vahhpp=>sys%ints%vahhpp, &
                vahphp=>sys%ints%vahphp, vahppp=>sys%ints%vahppp, &
                vbhhhh=>sys%ints%vbhhhh, vbhhhp=>sys%ints%vbhhhp, vbhhph=>sys%ints%vbhhph, &
                vbhhpp=>sys%ints%vbhhpp, vbhphp=>sys%ints%vbhphp, vbhpph=>sys%ints%vbhpph, &
                vbphph=>sys%ints%vbphph, vbhppp=>sys%ints%vbhppp, vbphpp=>sys%ints%vbphpp, &
                vchhhh=>sys%ints%vchhhh, vchhhp=>sys%ints%vchhhp, vchhpp=>sys%ints%vchhpp, &
                vchphp=>sys%ints%vchphp, vchppp=>sys%ints%vchppp, &
                vaappp=>sys%ints%vaappp, vbappp=>sys%ints%vbappp, vbpapp=>sys%ints%vbpapp, &
                vcappp=>sys%ints%vcappp)


            i0=0
            if (i0.eq.0) then
                allocate(ht2b(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
                ht2b=0.0d0

                if(k5.ge.1.and.k6.ge.1)then
                    call t2b0000_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k0.ge.1)then
                    call t2b0001_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k9.ge.1)then
                    call t2b0010_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k0.ge.1.and.k9.ge.1)then
                    call t2b0011_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k8.ge.1)then
                    call t2b0100_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k8.ge.1.and.k0.ge.1)then
                    call t2b0101_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k8.ge.1.and.k9.ge.1)then
                    call t2b0110_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.1.and.k9.ge.1.and.k0.ge.1)then
                    call t2b0111_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1)then
                    call t2b1000_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k0.ge.1)then
                    call t2b1001_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k9.ge.1)then
                    call t2b1010_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k0.ge.1.and.k7.ge.1.and.k9.ge.1)then
                    call t2b1011_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.1.and.k7.ge.1.and.k8.ge.1)then
                    call t2b1100_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.1.and.k7.ge.1.and.k8.ge.1.and.k0.ge.1)then
                    call t2b1101_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.1.and.k7.ge.1.and.k8.ge.1.and.k9.ge.1)then
                    call t2b1110_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k9.ge.1.and.k7.ge.1.and.k8.ge.1.and.k0.ge.1)then
                    call t2b1111_update(n0,n1,n2,n3,ht2b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                ht2b=ht2b+vbhhpp
                call t2b_update_opt(n0,n1,n2,n3,ht2b,run%shift, &
                    k1,k2,k3,k4, &
                    fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                    vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                    vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                    vbphph,vbhppp,vbphpp, &
                    vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))

                call t2b_update1(n0,n1,n2,n3,ht2b,run%shift, &
                    ipb,k1,k2,k3,k4, &
                    fahh,fapp,fbhh,fbpp,t(k1a),t(k1b),t(k2b))
                deallocate(ht2b)

            endif

            i0=0
            if(i0.eq.0)then
                allocate(ht2a(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
                ht2a=0.0d0

                if(k5.ge.2.and.k6.ge.2)then
                    call t2a0000_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k6.ge.1.and.k9.ge.1)then
                    call t2a0010_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k9.ge.2)then
                    call t2a0011_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.2.and.k7.ge.1)then
                    call t2a1000_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k9.ge.1)then
                    call t2a1010_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k7.ge.1.and.k9.ge.2)then
                    call t2a1011_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k6.ge.2)then
                    call t2a1100_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k6.ge.1.and.k9.ge.1)then
                    call t2a1110_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k9.ge.2)then
                    call t2a1111_update(n0,n1,n2,n3,ht2a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                ht2a=ht2a+vahhpp
                call t2a_update_opt(n0,n1,n2,n3,ht2a,run%shift, &
                    k1,k2,k3,k4, &
                    fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                    vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                    vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                    vbphph,vbhppp,vbphpp, &
                    vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))

                call t2a_update1(n0,n1,n3,ht2a,run%shift, &
                    ipa,k1,k3,fahh,fapp,t(k1a),t(k2a))
                deallocate(ht2a)

                allocate(ht2c(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
                ht2c=0.0d0

                if(k5.ge.2.and.k6.ge.2)then
                    call t2c0000_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k6.ge.1.and.k0.ge.1)then
                    call t2c0010_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k0.ge.2)then
                    call t2c0011_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k6.ge.1.and.k8.ge.1)then
                    call t2c1000_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k8.ge.1.and.k0.ge.1)then
                    call t2c1010_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.1.and.k0.ge.2)then
                    call t2c1011_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.2.and.k8.ge.2)then
                    call t2c1100_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.1.and.k8.ge.2.and.k0.ge.1)then
                    call t2c1110_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k8.ge.2.and.k0.ge.2)then
                    call t2c1111_update(n0,n1,n2,n3,ht2c,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                ht2c=ht2c+vchhpp
                call t2c_update_opt(n0,n1,n2,n3,ht2c,run%shift, &
                    k1,k2,k3,k4, &
                    fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                    vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                    vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                    vbphph,vbhppp,vbphpp, &
                    vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))

                call t2c_update1(n0,n2,n3,ht2c,run%shift, &
                    ipc,k2,k4,fbhh,fbpp,t(k1b),t(k2c))
                deallocate(ht2c)

            endif

            i0=0
            if(i0.eq.0)then
                allocate(ht1a(n1+1:n3,n0+1:n1))
                ht1a=0.0d0

                if(k5.ge.1.and.k6.ge.1)then
                    call t1a00_update(n0,n1,n2,n3,ht1a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k9.ge.1)then
                    call t1a01_update(n0,n1,n2,n3,ht1a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.1.and.k7.ge.1)then
                    call t1a10_update(n0,n1,n2,n3,ht1a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k9.ge.1.and.k7.ge.1)then

                    call t1a11_update(n0,n1,n2,n3,ht1a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                ht1a=ht1a+fahp
                call t1a_update_opt(n0,n1,n2,n3,ht1a,run%shift, &
                    k1,k2,k3,k4, &
                    fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                    vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                    vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                    vbphph,vbhppp,vbphpp, &
                    vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))
                deallocate(ht1a)

            endif

            i0=0
            if(i0.eq.0)then

                allocate(ht1b(n2+1:n3,n0+1:n2))
                ht1b=0.0d0

                if(k5.ge.1.and.k6.ge.1)then
                    call t1b00_update(n0,n1,n2,n3,ht1b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k0.ge.1)then
                    call t1b01_update(n0,n1,n2,n3,ht1b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.1.and.k8.ge.1)then
                    call t1b10_update(n0,n1,n2,n3,ht1b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k0.ge.1.and.k8.ge.1)then
                    call t1b11_update(n0,n1,n2,n3,ht1b,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                ht1b=ht1b+fbhp
                call t1b_update_opt(n0,n1,n2,n3,ht1b,run%shift, &
                    k1,k2,k3,k4, &
                    fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                    vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                    vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                    vbphph,vbhppp,vbphpp, &
                    vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))
                deallocate(ht1b)

            endif

            allocate(ht3b1(n2+1:n3,n1+1:n3,n1+1:m2,n0+1:n2,n0+1:n1,m1+1:n1))
            ht3b1=0.0d0

            i1=0
            if(i1.eq.0)then
                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k9.ge.1)then
                    call t3b100100_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k9.ge.1.and.k0.ge.1)then
                    call t3b100101_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k9.ge.2)then
                    call t3b100110_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k7.ge.1.and.k9.ge.2.and.k0.ge.1)then
                    call t3b100111_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k9.ge.1.and.k8.ge.1)then
                    call t3b101100_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k9.ge.1.and.k8.ge.1 &
                    .and.k0.ge.1)then
                    call t3b101101_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.1.and.k9.ge.2.and.k8.ge.1)then
                    call t3b101110_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k0.ge.1.and.k7.ge.1.and.k9.ge.2.and.k8.ge.1)then
                    call t3b101111_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.2.and.k9.ge.1)then
                    call t3b110100_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.2.and.k9.ge.1.and.k0.ge.1)then
                    call t3b110101_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k7.ge.2.and.k9.ge.2)then
                    call t3b110110_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k7.ge.2.and.k9.ge.2.and.k0.ge.1)then
                    call t3b110111_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k8.ge.1.and.k9.ge.1.and.k6.ge.1)then
                    call t3b111100_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k8.ge.1.and.k9.ge.1.and.k6.ge.1.and.k0.ge.1)then
                    call t3b111101_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k8.ge.1.and.k9.ge.2.and.k6.ge.1)then
                    call t3b111110_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k8.ge.1.and.k9.ge.2.and.k0.ge.1)then
                    call t3b111111_update(n0,n1,n2,n3,ht3b1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif
            endif

            allocate(ht3b4(n2+1:n3,n1+1:n3,n1+1:m2,m1+1:n2,n0+1:m1,n0+1:m1))
            ht3b4=0.0d0

            if(i1.eq.0)then
                if(k5.ge.2.and.k8.ge.1.and.k9.ge.1.and.k6.ge.1)then
                    call t3b001100_update(n0,n1,n2,n3,ht3b4,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k8.ge.1.and.k9.ge.1.and.k6.ge.1.and.k0.ge.1)then
                    call t3b001101_update(n0,n1,n2,n3,ht3b4,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k8.ge.1.and.k9.ge.2.and.k6.ge.1)then
                    call t3b001110_update(n0,n1,n2,n3,ht3b4,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k8.ge.1.and.k9.ge.2.and.k0.ge.1)then
                    call t3b001111_update(n0,n1,n2,n3,ht3b4,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

            endif
            allocate(ht3c1(n2+1:n3,n2+1:m2,n1+1:n3,n0+1:n2,m1+1:n2,n0+1:n1))
            ht3c1=0.0d0

            if(k5.ge.1.and.k8.ge.1.and.k6.ge.1.and.k0.ge.1)then
                call t3c010010_update(n0,n1,n2,n3,ht3c1,run%shift, &
                    m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                    fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                    vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                    vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                    vbphph,vbhppp,vbphpp, &
                    vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                    vaappp,vbappp,vbpapp,vcappp, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                    t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                    t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
            endif

            i1=0
            if(i1.eq.0)then
                if(k5.ge.1.and.k8.ge.1.and.k6.ge.1.and.k0.ge.2)then
                    call t3c010011_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.1.and.k6.ge.1.and.k9.ge.1.and.k0.ge.1)then
                    call t3c010110_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.1.and.k9.ge.1.and.k0.ge.2)then
                    call t3c010111_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.2.and.k6.ge.1.and.k0.ge.1)then
                    call t3c011010_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.2.and.k6.ge.1.and.k0.ge.1)then
                    call t3c011011_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.2.and.k6.ge.1.and.k9.ge.1.and.k0.ge.1)then
                    call t3c011110_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.2.and.k9.ge.1.and.k0.ge.2)then
                    call t3c011111_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k7.ge.1.and.k8.ge.1.and.k6.ge.1.and.k0.ge.1)then
                    call t3c110010_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k7.ge.1.and.k8.ge.1.and.k6.ge.1.and.k0.ge.2)then
                    call t3c110011_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k7.ge.1.and.k8.ge.1.and.k6.ge.1.and.k0.ge.1 &
                    .and.k9.ge.1)then
                    call t3c110110_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k7.ge.1.and.k8.ge.1.and.k9.ge.1.and.k0.ge.2)then
                    call t3c110111_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k8.ge.2.and.k6.ge.1.and.k0.ge.1)then
                    call t3c111010_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k8.ge.2.and.k6.ge.1.and.k0.ge.2)then
                    call t3c111011_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k8.ge.2.and.k6.ge.1.and.k0.ge.1.and.k9.ge.1)then
                    call t3c111110_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k8.ge.2.and.k0.ge.2.and.k9.ge.1)then
                    call t3c111111_update(n0,n1,n2,n3,ht3c1,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif
            endif

            allocate(ht3c4(n2+1:n3,n2+1:m2,n1+1:n3,n0+1:m1,n0+1:m1,m1+1:n1))
            ht3c4=0.0d0

            if(i1.eq.0)then
                if(k7.ge.1.and.k6.ge.2.and.k0.ge.1.and.k6.ge.1)then
                    call t3c100010_update(n0,n1,n2,n3,ht3c4,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k6.ge.2.and.k0.ge.2.and.k6.ge.1)then
                    call t3c100011_update(n0,n1,n2,n3,ht3c4,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k6.ge.2.and.k0.ge.1.and.k6.ge.1.and.k9.ge.1)then
                    call t3c100110_update(n0,n1,n2,n3,ht3c4,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k6.ge.2.and.k0.ge.2.and.k9.ge.1)then
                    call t3c100111_update(n0,n1,n2,n3,ht3c4,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif
            endif

            call t3bc_update(n0,n1,n2,n3,run%shift, &
                m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0,ipb, &
                fahh,fapp,fbhh,fbpp, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                t(k3b1),t(k3b4),t(k3c1),t(k3c4), &
                ht3b1,ht3b4,ht3c1,ht3c4)
            deallocate(ht3b1,ht3b4,ht3c1,ht3c4)

            i1=0
            if(i1.eq.0)then	
                allocate(ht3a(n1+1:n3,n1+1:n3,n1+1:m2,n0+1:n1,n0+1:n1,m1+1:n1))
                ht3a=0.0d0

                if(k7.ge.1.and.k5.ge.2.and.k6.ge.2.and.k9.ge.1)then
                    call t3a100100_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k5.ge.2.and.k6.ge.1.and.k9.ge.2)then
                    call t3a100110_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.1.and.k5.ge.2.and.k9.ge.3)then
                    call t3a100111_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k5.ge.1.and.k6.ge.2.and.k9.ge.1)then
                    call t3a110100_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k5.ge.1.and.k6.ge.1.and.k9.ge.2)then
                    call t3a110110_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.2.and.k5.ge.1.and.k9.ge.3)then
                    call t3a110111_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.3.and.k6.ge.2.and.k9.ge.1)then
                    call t3a111100_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.3.and.k6.ge.1.and.k9.ge.2)then
                    call t3a111110_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k7.ge.3.and.k9.ge.3)then
                    call t3a111111_update(n0,n1,n2,n3,ht3a,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                allocate(ht3b2(n2+1:m2,m2+1:n3,m2+1:n3,m1+1:n2,n0+1:m1,n0+1:m1))
                ht3b2=0.0d0

                if(k5.ge.2.and.k6.ge.2.and.k8.ge.1.and.k0.ge.1)then
                    call t3b001001_update(n0,n1,n2,n3,ht3b2,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                allocate(ht3b3(n2+1:m2,m2+1:n3,m2+1:n3,n0+1:n2,n0+1:n1,m1+1:n1))
                ht3b3=0.0d0

                if(k5.ge.1.and.k6.ge.2.and.k7.ge.1.and.k0.ge.1)then
                    call t3b100001_update(n0,n1,n2,n3,ht3b3,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.2.and.k7.ge.1.and.k8.ge.1.and.k0.ge.1)then
                    call t3b101001_update(n0,n1,n2,n3,ht3b3,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.2.and.k7.ge.2.and.k0.ge.1)then
                    call t3b110001_update(n0,n1,n2,n3,ht3b3,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.2.and.k7.ge.2.and.k8.ge.1.and.k0.ge.1)then
                    call t3b111001_update(n0,n1,n2,n3,ht3b3,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                call t3ab_update(n0,n1,n2,n3,run%shift, &
                    m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0,ipa, &
                    fahh,fapp,fbhh,fbpp, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                    t(k3a),t(k3b2),t(k3b3), &
                    ht3a,ht3b2,ht3b3)
                deallocate(ht3a,ht3b2,ht3b3)

            endif

            allocate(ht3c2(m2+1:n3,m2+1:n3,n1+1:m2,n0+1:m1,n0+1:m1,m1+1:n1))
            ht3c2=0.0d0

            i1=0
            if(i1.eq.0)then
                if(k5.ge.2.and.k6.ge.2.and.k7.ge.1.and.k9.ge.1)then
                    call t3c100100_update(n0,n1,n2,n3,ht3c2,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif
            endif

            allocate(ht3c3(m2+1:n3,m2+1:n3,n1+1:m2,n0+1:n2,m1+1:n2,n0+1:n1))
            ht3c3=0.0d0

            if(i1.eq.0)then
                if(k5.ge.1.and.k6.ge.2.and.k8.ge.1.and.k9.ge.1)then
                    call t3c010100_update(n0,n1,n2,n3,ht3c3,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.2.and.k8.ge.2.and.k9.ge.1)then
                    call t3c011100_update(n0,n1,n2,n3,ht3c3,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.2.and.k7.ge.1.and.k8.ge.1.and.k9.ge.1)then
                    call t3c110100_update(n0,n1,n2,n3,ht3c3,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.2.and.k7.ge.1.and.k8.ge.2.and.k9.ge.1)then
                    call t3c111100_update(n0,n1,n2,n3,ht3c3,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif
            endif

            allocate(ht3d(n2+1:n3,n2+1:n3,n2+1:m2,n0+1:n2,n0+1:n2,m1+1:n2))
            ht3d=0.0d0

            if(i1.eq.0)then
                if(k5.ge.2.and.k6.ge.2.and.k8.ge.1.and.k0.ge.1)then
                    call t3d100100_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k6.ge.1.and.k8.ge.1.and.k0.ge.2)then
                    call t3d100110_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.2.and.k8.ge.1.and.k0.ge.3)then
                    call t3d100111_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.2.and.k8.ge.2.and.k0.ge.1)then
                    call t3d110100_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k6.ge.1.and.k8.ge.2.and.k0.ge.2)then
                    call t3d110110_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k5.ge.1.and.k8.ge.2.and.k0.ge.3)then
                    call t3d110111_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.2.and.k8.ge.3.and.k0.ge.1)then
                    call t3d111100_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k6.ge.1.and.k8.ge.3.and.k0.ge.2)then
                    call t3d111110_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif

                if(k8.ge.3.and.k0.ge.3)then
                    call t3d111111_update(n0,n1,n2,n3,ht3d,run%shift, &
                        m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0, &
                        fahh,fahp,fapp,fbhh,fbhp,fbpp, &
                        vahhhh,vahhhp,vahhpp,vahphp,vahppp, &
                        vbhhhh,vbhhhp,vbhhph,vbhhpp,vbhphp,vbhpph, &
                        vbphph,vbhppp,vbphpp, &
                        vchhhh,vchhhp,vchhpp,vchphp,vchppp, &
                        vaappp,vbappp,vbpapp,vcappp, &
                        t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b1),t(k3b2),t(k3b3),t(k3b4), &
                        t(k3c1),t(k3c2),t(k3c3),t(k3c4),t(k3d))
                endif
            endif

            call t3cd_update(n0,n1,n2,n3,run%shift, &
                m1,m2,k1,k2,k3,k4,k5,k6,k7,k8,k9,k0,ipc, &
                fahh,fapp,fbhh,fbpp, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                t(k3c2),t(k3c3),t(k3d), &
                ht3c2,ht3c3,ht3d)
            deallocate(ht3d,ht3c2,ht3c3)

        end associate

#endif

    end subroutine update_clusters_t3_opt

    subroutine diag_compat(acc, &
            diag1, diag2, diag3, diag4, diag5, &
            dt3diag3, dt3diag4, &
            t1diag1, t1diag2, t1diag3, t1diag4, &
            t2diag3, t2diag4, t2diag5, &
            t3diag1, t3diag2, t3diag3, t3diag4, t3diag5)

        ! ACC compatibility routine. Used to help transition between
        ! the new dervied type, acc, and the way Ilias wrote the
        ! original code.
        
        ! [TODO] This subroutine should be deprecated at one point

        use const, only: sp
        use cc_types, only: acc_t

        type(acc_t), intent(in) :: acc
        real(sp), intent(inout) :: diag1, diag2, diag3, diag4, diag5
        real(sp), intent(inout) :: dt3diag3, dt3diag4
        real(sp), intent(inout) :: t1diag1, t1diag2, t1diag3, t1diag4
        real(sp), intent(inout) :: t2diag3, t2diag4, t2diag5
        real(sp), intent(inout) :: t3diag1, t3diag2, t3diag3, t3diag4, t3diag5

        ! T2^2 on doubles
        diag1 = acc%t2t2_t2(1)
        diag2 = acc%t2t2_t2(2)
        diag3 = acc%t2t2_t2(3)
        diag4 = acc%t2t2_t2(4)
        diag5 = acc%t2t2_t2(5)

        ! T3 on doubles
        dt3diag3 = acc%t3_t2(1)
        dt3diag4 = acc%t3_t2(2)

        ! T1*T3 on doubles
        t1diag1 = acc%t1t3_t2(1)
        t1diag2 = acc%t1t3_t2(2)
        t1diag3 = acc%t1t3_t2(3)
        t1diag4 = acc%t1t3_t2(4)

        ! T2^2 on triples
        t2diag3 = acc%t2t2_t3(1)
        t2diag4 = acc%t2t2_t3(2)
        t2diag5 = acc%t2t2_t3(3)

        ! T2*T3 on triples
        t3diag1 = acc%t2t3_t3(1)
        t3diag2 = acc%t2t3_t3(2)
        t3diag3 = acc%t2t3_t3(3)
        t3diag4 = acc%t2t3_t3(4)
        t3diag5 = acc%t2t3_t3(5)

    end subroutine diag_compat

end module update_cc
