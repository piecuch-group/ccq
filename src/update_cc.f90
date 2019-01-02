module update_cc

    implicit none

contains

    subroutine update_clusters_t4(sys, run, cc)

        use const, only: sp, dp, ta, tb, tc, td, te
        use system, only: sys_t, run_t, cc_t

        implicit none

        type(sys_t), intent(in), target :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout), target :: cc

        real(dp), pointer :: t(:) => null()
        real(dp), pointer :: t2_mc(:) => null()
        real(dp), pointer :: fockr(:,:) => null()
        real(dp), pointer :: fockb(:,:) => null()
        real(dp), pointer :: intr(:,:,:,:) => null()
        real(dp), pointer :: intb(:,:,:,:) => null()
        real(dp), pointer :: intm(:,:,:,:) => null()

        real(dp), allocatable :: v1(:,:)
        real(dp), allocatable :: v2(:,:,:,:)
        real(dp), allocatable :: v3(:,:,:,:,:,:)


        ! Adapt for Ilias old scheme

        real(sp), allocatable :: t4ae(:)

        integer :: n0, n1, n2 ,n3
        real(dp) :: shift
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

        shift = run%shift

        t => cc%t_vec
        t2_mc => cc%acc%t2_mc
        fockr => sys%ints%f_a
        fockb => sys%ints%f_b
        intr => sys%ints%v_aa
        intb => sys%ints%v_bb
        intm => sys%ints%v_ab

        k1a = cc%pos(1)
        k1b = cc%pos(2)
        k2a = cc%pos(3)
        k2b = cc%pos(4)
        k2c = cc%pos(5)
        k3a = cc%pos(6)
        k3b = cc%pos(7)
        k3c = cc%pos(8)
        k3d = cc%pos(9)


        allocate(v2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
        v2=0.0d0
        if (run%ext_cor) then
            v2(:,:,:,:) = cc%ext_cor%t2a(:,:,:,:)
        endif
        call sumx2143(n0,n3,n1,n3,n1,n3,n0,n1,n0,n1,v2,intr,1.000)

        call t2a_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,shift,v2, &
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
            v2=0.0d0
            if (run%ext_cor) then
                v2(:,:,:,:) = cc%ext_cor%t2c(:,:,:,:)
            endif
            call sumx2143(n0,n3,n2,n3,n2,n3,n0,n2,n0,n2,v2,intb,1.000)

            call t2c_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,shift,v2, &
                run%ext_cor,fockr,fockb,intr,intb,intm,diag1,diag2,diag3,diag4,diag5, &
                t1diag1,t1diag2,t1diag3,t1diag4, &
                dt3diag3,dt3diag4, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c),t(k3a),t(k3b),t(k3c),t(k3d), &
                t2_mc(1),t2_mc(k2b-k2a+1),t2_mc(k2c-k2a+1))
            deallocate(v2)
        endif

        allocate(v2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
        v2=0.0d0
        if (run%ext_cor) then
            v2(:,:,:,:) = cc%ext_cor%t2b(:,:,:,:)
        endif
        call sumx2143(n0,n3,n2,n3,n1,n3,n0,n2,n0,n1,v2,intm,1.000)

        call t2b_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,shift,v2, &
            run%ext_cor,fockr,fockb,intr,intb,intm,diag1,diag2,diag3,diag4,diag5, &
            t1diag1,t1diag2,t1diag3,t1diag4, &
            dt3diag3,dt3diag4, &
            t(k1a),t(k1b),t(k2a),t(k2b),t(k2c),t(k3a),t(k3b),t(k3c),t(k3d), &
            t2_mc(1),t2_mc(k2b-k2a+1),t2_mc(k2c-k2a+1))
        deallocate(v2)

        allocate(v1(n1+1:n3,n0+1:n1))
        v1=0.0d0
        call sumx12(0,n3,n1,n3,n0,n1,v1,fockr, 1.000)   !!!!!not sure if the 0 should be n0

        call t1a_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,shift,v1, &
            fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
            t(k3a),t(k3b),t(k3c),t(k3d))
        deallocate(v1)


        if(run%rhf)then
            do i=k1a,k1b-1
                t(i+k1b-k1a)=t(i)
            enddo
        else
            allocate(v1(n2+1:n3,n0+1:n2))
            v1=0.0d0
            call sumx12(0,n3,n2,n3,n0,n2,v1,fockb, 1.000)

            call t1b_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,shift,v1, &
                fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                t(k3a),t(k3b),t(k3c),t(k3d))
            deallocate(v1)
        endif

        if (.not. run%ext_cor) then
            if(run%lvl_t)then

                allocate(v3(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
                v3=0.0d0
                call t3a_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,shift,v3, &
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
                    call t3d_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,shift,v3, &
                        fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b),t(k3c),t(k3d),iactoccb,iactunob,iactindt, &
                        t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
                    deallocate(v3)
                endif

                allocate(v3(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
                v3=0.0d0
                call t3b_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,shift,v3, &
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
                    call t3c_update(n0,n1,n2,n3,k1,k2,k3,k4,run%lvl_t,run%lvl_q,shift,v3, &
                        fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b),t(k3c),t(k3d), &
                        iactocca,iactoccb,iactunoa,iactunob,iactindt, &
                        t2diag3,t2diag4,t2diag5,t3diag1,t3diag2,t3diag3,t3diag4,t3diag5)
                    deallocate(v3)
                endif
            endif

            if(run%lvl_q)then
                call t4a_update(n0,n1,n2,n3,k1,k2,k3,k4,shift, &
                    fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                    t(k3a),t(k3b),t(k3c),t(k3d), &
                    iactocca,iactunoa,iactindq)

                call t4b_update(n0,n1,n2,n3,k1,k2,k3,k4,shift, &
                    fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                    t(k3a),t(k3b),t(k3c),t(k3d), &
                    iactocca,iactoccb,iactunoa,iactunob,iactindq)

                call t4c_update(n0,n1,n2,n3,k1,k2,k3,k4,shift, &
                    fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                    t(k3a),t(k3b),t(k3c),t(k3d), &
                    iactocca,iactoccb,iactunoa,iactunob,iactindq)

                if(run%rhf)then
                    call tran4bto4d(n0,n1,n2,n3)
                else
                    call t4d_update(n0,n1,n2,n3,k1,k2,k3,k4,shift, &
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
                    call t4e_update(n0,n1,n2,n3,k1,k2,k3,k4,shift, &
                        fockr,fockb,intr,intb,intm,t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                        t(k3a),t(k3b),t(k3c),t(k3d), &
                        iactoccb,iactunob,iactindq)
                endif
            endif
        endif

    end subroutine update_clusters_t4

    subroutine diag_compat(acc, &
            diag1, diag2, diag3, diag4, diag5, &
            dt3diag3, dt3diag4, &
            t1diag1, t1diag2, t1diag3, t1diag4, &
            t2diag3, t2diag4, t2diag5, &
            t3diag1, t3diag2, t3diag3, t3diag4, t3diag5)

        use const, only: sp
        use system, only: acc_t

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
