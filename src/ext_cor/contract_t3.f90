module contract_t3

    ! Module for contracting T3 and projecting it onto
    ! singles and doubles.

    implicit none

contains

    subroutine init_xs(sys, ext_cor)

        ! Initialize X intermediates that hold the HT_3 similarity
        ! transformed Hamiltonian used for the H T_1 T_3 updates on
        ! doubles

        ! In:
        !   sys: molecular system information

        ! In/Out:
        !   ext_cor: external correction variables, including
        !            the static T1 and T2 and the Xs

        use system, only: sys_t
        use ext_cor_types, only: ext_cor_t

        type(sys_t), intent(in) :: sys
        type(ext_cor_t), intent(in out) :: ext_cor

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs)

            allocate(ext_cor%x1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
            allocate(ext_cor%x2(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
            allocate(ext_cor%x3(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            allocate(ext_cor%x4(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            allocate(ext_cor%x5(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            allocate(ext_cor%x6(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
            allocate(ext_cor%x7(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
            allocate(ext_cor%x8(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            allocate(ext_cor%x9(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
            allocate(ext_cor%x10(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
            allocate(ext_cor%x11(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            allocate(ext_cor%x12(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
            allocate(ext_cor%x13(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
            allocate(ext_cor%x14(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))

        end associate

    end subroutine init_xs

    subroutine drive_t3_contraction(sys, cc)

        ! Process the externally corrected T3 contributions. This
        ! routine takes T3 and projects it onto singles and doubles, in
        ! order to handle the linear terms, and creates an HBar to deal
        ! with the contraction with T1 projected on doubles.

        ! In:
        !   sys: molecular system information

        ! In/Out:
        !   cc: coupled cluster data, specifically the T vector

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        use errors, only: stop_all

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(in out) :: cc

        real(p), pointer :: t3a(:,:,:,:,:,:) => null()
        real(p), pointer :: t3b(:,:,:,:,:,:) => null()
        real(p), pointer :: t3c(:,:,:,:,:,:) => null()
        real(p), pointer :: t3d(:,:,:,:,:,:) => null()

        if (.not. allocated(cc%ext_cor%t1a)) &
             call stop_all('drive_t3_contraction', 'RUNTIME ERROR: ext_cor%t1a not allocated')
        if (.not. allocated(cc%ext_cor%t1b)) &
             call stop_all('drive_t3_contraction', 'RUNTIME ERROR: ext_cor%t1b not allocated')

        ! Pointers to T3
        t3a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
             sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
             => cc%t_vec(cc%pos(6):cc%pos(7)-1)

        t3b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
             sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
             => cc%t_vec(cc%pos(7):cc%pos(8)-1)

        t3c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
             sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
             => cc%t_vec(cc%pos(8):cc%pos(9)-1)

        t3d(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
             sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
             => cc%t_vec(cc%pos(9):cc%pos(10)-1)


        ! Static updates. These will not be changed during the
        ! iterative procedure (these are the linear terms).
        call t3_on_t1a(sys, t3a, t3b, t3c, cc%ext_cor%t1a)
        call t3_on_t1b(sys, t3b, t3c, t3d, cc%ext_cor%t1b)

        call t3_on_t2a(sys, t3a, t3b, cc%ext_cor%t2a)
        call t3_on_t2b(sys, t3b, t3c, cc%ext_cor%t2b)
        call t3_on_t2c(sys, t3c, t3d, cc%ext_cor%t2c)


        ! Build the similarity transformed Hamiltonian required for
        ! <ijab| (HT1T3)C |phi> terms.

        ! Initialize xs containing the intermediates necessary for updating
        ! T1T3 terms
        call init_xs(sys, cc%ext_cor)
        call v_t3_hbar(sys, cc)


    end subroutine drive_t3_contraction

    subroutine v_t3_hbar(sys, cc)

        ! Build the similarity transformed Hamiltonian required for
        ! <ijab| (HT1T3)C |phi> terms. The result is a series of
        ! intermediates (Xs) that are used to contract with T1.

        ! In:
        !   sys: molecular system information

        ! In/Out:
        !   cc: coupled-cluster data. In this case only t_vec will be used

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(in out) :: cc

        real(p), pointer :: t3a(:,:,:,:,:,:) => null()
        real(p), pointer :: t3b(:,:,:,:,:,:) => null()
        real(p), pointer :: t3c(:,:,:,:,:,:) => null()
        real(p), pointer :: t3d(:,:,:,:,:,:) => null()

        real(p), allocatable::d1(:,:,:,:)
        real(p), allocatable::f2(:,:,:,:,:,:)

        real(p), allocatable::s1(:,:,:,:)
        real(p), allocatable::s2(:,:,:,:)
        real(p), allocatable::s3(:,:,:,:)
        real(p), allocatable::s4(:,:,:,:)
        real(p), allocatable::s5(:,:,:,:)
        real(p), allocatable::s6(:,:,:,:)
        real(p), allocatable::s7(:,:,:,:)
        real(p), allocatable::s8(:,:,:,:)
        real(p), allocatable::s9(:,:,:,:)
        real(p), allocatable::s10(:,:,:,:)
        real(p), allocatable::s11(:,:,:,:)
        real(p), allocatable::s12(:,:,:,:)
        real(p), allocatable::s13(:,:,:,:)
        real(p), allocatable::s14(:,:,:,:)
        real(p), allocatable::s15(:,:,:,:)
        real(p), allocatable::s16(:,:,:,:)
        real(p), allocatable::u1(:,:,:,:,:,:)
        real(p), allocatable::u2(:,:,:,:,:,:)
        real(p), allocatable::u3(:,:,:,:,:,:)
        real(p), allocatable::u4(:,:,:,:,:,:)
        real(p), allocatable::u5(:,:,:,:,:,:)
        real(p), allocatable::u6(:,:,:,:,:,:)
        real(p), allocatable::u7(:,:,:,:,:,:)
        real(p), allocatable::u8(:,:,:,:,:,:)
        real(p), allocatable::u9(:,:,:,:,:,:)
        real(p), allocatable::u10(:,:,:,:,:,:)
        real(p), allocatable::u11(:,:,:,:,:,:)
        real(p), allocatable::u12(:,:,:,:,:,:)

        integer :: k1, k2, k3, k4
        integer :: i1, i2, i3

        ! Pointers to T3
        t3a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(6):cc%pos(7)-1)

        t3b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(7):cc%pos(8)-1)

        t3c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) &
            => cc%t_vec(cc%pos(8):cc%pos(9)-1)

        t3d(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
            sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) &
            => cc%t_vec(cc%pos(9):cc%pos(10)-1)


        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fahh=>sys%ints%fahh, fahp=>sys%ints%fahp, fapp=>sys%ints%fapp, &
             fbhh=>sys%ints%fbhh, fbhp=>sys%ints%fbhp, fbpp=>sys%ints%fbpp, &
             vahhhh=>sys%ints%vahhhh, vahhhp=>sys%ints%vahhhp, vahhpp=>sys%ints%vahhpp, &
             vahphp=>sys%ints%vahphp, vahppp=>sys%ints%vahppp, &
             vbhhhh=>sys%ints%vbhhhh, vbhhhp=>sys%ints%vbhhhp, vbhhph=>sys%ints%vbhhph, &
             vbhhpp=>sys%ints%vbhhpp, vbhphp=>sys%ints%vbhphp, vbhpph=>sys%ints%vbhpph, &
             vbphph=>sys%ints%vbphph, vbhppp=>sys%ints%vbhppp, vbphpp=>sys%ints%vbphpp, &
             vchhhh=>sys%ints%vchhhh, vchhhp=>sys%ints%vchhhp, vchhpp=>sys%ints%vchhpp, &
             vchphp=>sys%ints%vchphp, vchppp=>sys%ints%vchppp, &
             vaappp=>sys%ints%vaappp, vbappp=>sys%ints%vbappp, vbpapp=>sys%ints%vbpapp, &
             vcappp=>sys%ints%vcappp, &
             x1=>cc%ext_cor%x1, &
             x2=>cc%ext_cor%x2, &
             x3=>cc%ext_cor%x3, &
             x4=>cc%ext_cor%x4, &
             x5=>cc%ext_cor%x5, &
             x6=>cc%ext_cor%x6, &
             x7=>cc%ext_cor%x7, &
             x8=>cc%ext_cor%x8, &
             x9=>cc%ext_cor%x9, &
             x10=>cc%ext_cor%x10, &
             x11=>cc%ext_cor%x11, &
             x12=>cc%ext_cor%x12, &
             x13=>cc%ext_cor%x13, &
             x14=>cc%ext_cor%x14)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2

            allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
            call reorder3421(n1,n3,n1,n3,n0,n1,n0,n1, &
                 n0,n1,n0,n1,n1,n3,n1,n3,vahhpp,d1)
            allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1, &
                 n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
            allocate(s1(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
            i1=k3
            i2=k1*k3*k3
            i3=k3*k1*k1
            call egemm(i1,i2,i3,d1,f2,s1)
            deallocate(d1)
            deallocate(f2)

            x1=0.0_p
            call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x1,s1, 1.000)
            deallocate(s1)

            allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
            call reorder4123(n1,n3,n1,n3,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n0,n1,vahhpp,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
            allocate(s2(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
            i1=k1
            i2=k1*k1*k3
            i3=k3*k3*k1
            call egemm(i1,i2,i3,d1,f2,s2)
            deallocate(d1)
            deallocate(f2)

            x2=0.0_p
            call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x2,s2, 1.000)
            deallocate(s2)

            allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
            call reorder4231(n1,n3,n1,n3,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n0,n1,n1,n3,vahhpp,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
            allocate(u1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
            i1=k3*k1
            i2=k1*k1*k3*k3
            i3=k3*k1
            call egemm(i1,i2,i3,d1,f2,u1)
            deallocate(d1)
            deallocate(f2)

            x3=0.0_p
            call sum345612(n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x3,u1, 1.000)
            deallocate(u1)

            allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
            call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n2,n3,n1,n3,vbhhpp,d1)
            allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)
            allocate(s3(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
            i1=k3
            i2=k1*k3*k3
            i3=k4*k1*k2
            call egemm(i1,i2,i3,d1,f2,s3)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x1,s3,-2.000)
            deallocate(s3)

            allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
            call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n0,n1,vbhhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
            allocate(s4(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
            i1=k1
            i2=k1*k1*k3
            i3=k3*k4*k2
            call egemm(i1,i2,i3,d1,f2,s4)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x2,s4,-2.000)
            deallocate(s4)

            allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))
            call reorder3142(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n0,n1,n1,n3,vbhhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
            allocate(u2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3))
            i1=k3*k1
            i2=k1*k1*k3*k3
            i3=k4*k2
            call egemm(i1,i2,i3,d1,f2,u2)
            deallocate(d1)
            deallocate(f2)

            call sum345612(n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x3,u2, 1.000)
            deallocate(u2)

            allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
            call reorder4231(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n1,n1,n3,n0,n2,n2,n3,vbhhpp,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
            allocate(u3(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n2+1:n3))
            i1=k4*k2
            i2=k1*k1*k3*k3
            i3=k3*k1
            call egemm(i1,i2,i3,d1,f2,u3)
            deallocate(d1)
            deallocate(f2)

            x4=0.0_p
            call sum345612(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x4,u3, 1.000)
            deallocate(u3)

            allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
            call reorder4231(n2,n3,n2,n3,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n0,n2,n2,n3,vchhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
            allocate(u4(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n2+1:n3))
            i1=k4*k2
            i2=k1*k1*k3*k3
            i3=k4*k2
            call egemm(i1,i2,i3,d1,f2,u4)
            deallocate(d1)
            deallocate(f2)

            call sum345612(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x4,u4, 1.000)
            deallocate(u4)

            !

            allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
            call reorder3142(n1,n3,n1,n3,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n0,n1,n1,n3,vahhpp,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
            allocate(u5(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3))
            i1=k3*k1
            i2=k2*k2*k4*k4
            i3=k3*k1
            call egemm(i1,i2,i3,d1,f2,u5)
            deallocate(d1)
            deallocate(f2)

            x5=0.0_p
            call sum345612(n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,x5,u5, 1.000)
            deallocate(u5)

            allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))
            call reorder3142(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n0,n1,n1,n3,vbhhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
            allocate(u6(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n1,n1+1:n3))
            i1=k3*k1
            i2=k2*k2*k4*k4
            i3=k4*k2
            call egemm(i1,i2,i3,d1,f2,u6)
            deallocate(d1)
            deallocate(f2)

            call sum345612(n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,x5,u6, 1.000)
            deallocate(u6)

            allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
            call reorder3421(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n1,n3,n2,n3,vbhhpp,d1)
            allocate(f2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
            call reorder463125(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,t3c,f2)
            allocate(s5(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
            i1=k4
            i2=k2*k4*k4
            i3=k3*k1*k2
            call egemm(i1,i2,i3,d1,f2,s5)
            deallocate(d1)
            deallocate(f2)

            x6=0.0_p
            call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x6,s5, 1.000)
            deallocate(s5)

            allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
            call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n1,n2,n3,n1,n3,n0,n2,vbhhpp,d1)
            allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder613245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n1,n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
            allocate(s6(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
            i1=k2
            i2=k2*k2*k4
            i3=k3*k4*k1
            call egemm(i1,i2,i3,d1,f2,s6)
            deallocate(d1)
            deallocate(f2)

            x7=0.0_p
            call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x7,s6, 1.000)
            deallocate(s6)

            allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
            call reorder4231(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n1,n1,n3,n0,n2,n2,n3,vbhhpp,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
            allocate(u7(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
            i1=k4*k2
            i2=k2*k2*k4*k4
            i3=k3*k1
            call egemm(i1,i2,i3,d1,f2,u7)
            deallocate(d1)
            deallocate(f2)

            x8=0.0_p
            call sum345612(n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,x8,u7, 1.000)
            deallocate(u7)

            allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
            call reorder3421(n2,n3,n2,n3,n0,n2,n0,n2, &
                 n0,n2,n0,n2,n2,n3,n2,n3,vchhpp,d1)
            allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
            call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2, &
                 n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
            allocate(s7(n2+1:n3,n2+1:n3,n0+1:n2,n2+1:n3))
            i1=k4
            i2=k2*k4*k4
            i3=k4*k2*k2
            call egemm(i1,i2,i3,d1,f2,s7)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n2,n3,n2,n3,n2,n3,n0,n2,x6,s7,-0.500)
            deallocate(s7)

            allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
            call reorder4123(n2,n3,n2,n3,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n2,n3,n0,n2,vchhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
            allocate(s8(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
            i1=k2
            i2=k2*k2*k4
            i3=k4*k4*k2
            call egemm(i1,i2,i3,d1,f2,s8)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n0,n2,n2,n3,n0,n2,n0,n2,x7,s8,-0.500)
            deallocate(s8)

            allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
            call reorder4231(n2,n3,n2,n3,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n0,n2,n2,n3,vchhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
            allocate(u8(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2,n2+1:n3))
            i1=k4*k2
            i2=k2*k2*k4*k4
            i3=k4*k2
            call egemm(i1,i2,i3,d1,f2,u8)
            deallocate(d1)
            deallocate(f2)

            call sum345612(n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,x8,u8, 1.000)
            deallocate(u8)

            !

            allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
            call reorder3421(n1,n3,n1,n3,n0,n1,n0,n1, &
                 n0,n1,n0,n1,n1,n3,n1,n3,vahhpp,d1)
            allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
            call reorder562134(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,t3b,f2)
            allocate(s9(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
            i1=k3
            i2=k2*k3*k4
            i3=k3*k1*k1
            call egemm(i1,i2,i3,d1,f2,s9)
            deallocate(d1)
            deallocate(f2)

            x9=0.0_p
            call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x9,s9, 1.000)
            deallocate(s9)

            allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
            call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n0,n1,vahhpp,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
            call reorder523146(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3b,f2)
            allocate(s10(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
            i1=k1
            i2=k1*k2*k4
            i3=k3*k3*k1
            call egemm(i1,i2,i3,d1,f2,s10)
            deallocate(d1)
            deallocate(f2)

            x10=0.0_p
            call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x10,s10, 1.000)
            deallocate(s10)

            allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3))
            call reorder4231(n1,n3,n1,n3,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n0,n1,n1,n3,vahhpp,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
            allocate(u9(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3))
            i1=k3*k1
            i2=k1*k2*k3*k4
            i3=k3*k1
            call egemm(i1,i2,i3,d1,f2,u9)
            deallocate(d1)
            deallocate(f2)

            x11=0.0_p
            call sum345612(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x11,u9, 1.000)
            deallocate(u9)

            allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
            call reorder3412(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n2,n3,n1,n3,vbhhpp,d1)
            allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
            call reorder461235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,t3c,f2)
            allocate(s11(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
            i1=k3
            i2=k2*k3*k4
            i3=k4*k1*k2
            call egemm(i1,i2,i3,d1,f2,s11)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x9,s11,-2.000)
            deallocate(s11)

            allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
            call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n0,n1,vbhhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
            call reorder413256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
            allocate(s12(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
            i1=k1
            i2=k1*k2*k4
            i3=k3*k4*k2
            call egemm(i1,i2,i3,d1,f2,s12)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x10,s12, 2.000)
            deallocate(s12)

            allocate(d1(n0+1:n2,n2+1:n3,n0+1:n1,n1+1:n3))
            call reorder3142(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n0,n1,n1,n3,vbhhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
            allocate(u10(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3))
            i1=k3*k1
            i2=k1*k2*k3*k4
            i3=k4*k2
            call egemm(i1,i2,i3,d1,f2,u10)
            deallocate(d1)
            deallocate(f2)

            call sum345612(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x11,u10, 1.000)
            deallocate(u10)

            allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3))
            call reorder3421(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n1,n3,n2,n3,vbhhpp,d1)
            allocate(f2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
            call reorder452136(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n0,n1,n1,n3,n2,n3,n1,n3,n0,n1,t3b,f2)
            allocate(s13(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
            i1=k4
            i2=k1*k3*k4
            i3=k3*k1*k2
            call egemm(i1,i2,i3,d1,f2,s13)
            deallocate(d1)
            deallocate(f2)

            x12=0.0_p
            call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x12,s13, 1.000)
            deallocate(s13)

            allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
            call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n1,n2,n3,n1,n3,n0,n2,vbhhpp,d1)
            allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
            allocate(s14(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
            i1=k2
            i2=k1*k2*k3
            i3=k3*k4*k1
            call egemm(i1,i2,i3,d1,f2,s14)
            deallocate(d1)
            deallocate(f2)

            x13=0.0_p
            call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x13,s14, 1.000)
            deallocate(s14)

            allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3))
            call reorder4231(n2,n3,n1,n3,n0,n2,n0,n1, &
                 n0,n1,n1,n3,n0,n2,n2,n3,vbhhpp,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
            allocate(u11(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n2+1:n3))
            i1=k4*k2
            i2=k1*k2*k3*k4
            i3=k3*k1
            call egemm(i1,i2,i3,d1,f2,u11)
            deallocate(d1)
            deallocate(f2)

            x14=0.0_p
            call sum345612(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x14,u11, 1.000)
            deallocate(u11)

            allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
            call reorder3421(n2,n3,n2,n3,n0,n2,n0,n2, &
                 n0,n2,n0,n2,n2,n3,n2,n3,vchhpp,d1)
            allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
            allocate(s15(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
            i1=k4
            i2=k1*k3*k4
            i3=k4*k2*k2
            call egemm(i1,i2,i3,d1,f2,s15)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x12,s15,-0.500)
            deallocate(s15)

            allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
            call reorder4123(n2,n3,n2,n3,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n2,n3,n0,n2,vchhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
            allocate(s16(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
            i1=k2
            i2=k1*k2*k3
            i3=k4*k4*k2
            call egemm(i1,i2,i3,d1,f2,s16)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x13,s16,-0.500)
            deallocate(s16)

            allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3))
            call reorder4231(n2,n3,n2,n3,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n0,n2,n2,n3,vchhpp,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
            allocate(u12(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n2+1:n3))
            i1=k4*k2
            i2=k1*k2*k3*k4
            i3=k4*k2
            call egemm(i1,i2,i3,d1,f2,u12)
            deallocate(d1)
            deallocate(f2)

            call sum345612(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x14,u12, 1.000)
            deallocate(u12)

        end associate

    end subroutine v_t3_hbar

    subroutine t3_on_t1a(sys, t3a, t3b, t3c, v1a)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys

        real(p), intent(in) :: t3a(:,:,:,:,:,:)
        real(p), intent(in) :: t3b(:,:,:,:,:,:)
        real(p), intent(in) :: t3c(:,:,:,:,:,:)
        real(p), intent(in out) :: v1a(:,:)

        real(p), allocatable :: d1(:,:,:,:)
        real(p), allocatable :: f2(:,:,:,:,:,:)
        real(p), allocatable :: z11(:,:)
        real(p), allocatable :: z12(:,:)
        real(p), allocatable :: z13(:,:)

        integer :: k1, k2, k3, k4
        integer :: i2, i3

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
             intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2

            allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
            call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)

            allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1, &
                 n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)

            allocate(z11(n1+1:n3,n0+1:n1))
            i2=k1*k3
            i3=k3*k3*k1*k1
            call egemm2(i2,i3,d1,f2,z11)
            deallocate(d1)
            deallocate(f2)

            v1a=v1a+0.250*z11(:,:)
            deallocate(z11)

            allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
            call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)

            allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)

            allocate(z12(n1+1:n3,n0+1:n1))
            i2=k1*k3
            i3=k3*k4*k1*k2
            call egemm2(i2,i3,d1,f2,z12)
            deallocate(d1)
            deallocate(f2)

            v1a=v1a+z12
            deallocate(z12)

            allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
            call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
            allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
            allocate(z13(n1+1:n3,n0+1:n1))
            i2=k1*k3
            i3=k4*k4*k2*k2
            call egemm2(i2,i3,d1,f2,z13)
            deallocate(d1)
            deallocate(f2)

            v1a=v1a+0.250*z13
            deallocate(z13)

        end associate

    end subroutine t3_on_t1a

    subroutine t3_on_t1b(sys, t3b, t3c, t3d, v1b)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys

        real(p), intent(in) :: t3b(:,:,:,:,:,:)
        real(p), intent(in) :: t3c(:,:,:,:,:,:)
        real(p), intent(in) :: t3d(:,:,:,:,:,:)
        real(p), intent(in out) :: v1b(:,:)

        real(p), allocatable :: d1(:,:,:,:)
        real(p), allocatable :: f2(:,:,:,:,:,:)
        real(p), allocatable :: z11(:,:)
        real(p), allocatable :: z12(:,:)
        real(p), allocatable :: z13(:,:)

        integer :: k1, k2, k3, k4
        integer :: i2, i3

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
             intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2
            allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
            call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
            allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
            call reorder562314(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n0,n1,n1,n3,n1,n3,n2,n3,n0,n2,t3b,f2)
            allocate(z11(n2+1:n3,n0+1:n2))
            i2=k2*k4
            i3=k3*k3*k1*k1
            call egemm2(i2,i3,d1,f2,z11)
            deallocate(d1)
            deallocate(f2)

            v1b=v1b+0.250*z11
            deallocate(z11)

            allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3))
            call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n2,n0,n1,n2,n3,n1,n3,intm,d1)
            allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
            call reorder461325(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n2,n3,n1,n3,n2,n3,n0,n2,t3c,f2)
            allocate(z12(n2+1:n3,n0+1:n2))
            i2=k2*k4
            i3=k3*k4*k1*k2
            call egemm2(i2,i3,d1,f2,z12)
            deallocate(d1)
            deallocate(f2)

            v1b=v1b+z12
            deallocate(z12)

            allocate(d1(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3))
            call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n2,n0,n2,n2,n3,n2,n3,intb,d1)
            allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
            call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2, &
                 n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
            allocate(z13(n2+1:n3,n0+1:n2))
            i2=k2*k4
            i3=k4*k4*k2*k2
            call egemm2(i2,i3,d1,f2,z13)
            deallocate(d1)
            deallocate(f2)

            v1b=v1b+0.250*z13
            deallocate(z13)

        end associate

    end subroutine t3_on_t1b

    subroutine t3_on_t2a(sys, t3a, t3b, v2a)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys

        real(p), intent(in) :: t3a(:,:,:,:,:,:)
        real(p), intent(in) :: t3b(:,:,:,:,:,:)
        real(p), intent(in out) :: v2a(:,:,:,:)

        real(p), allocatable :: x7(:,:)
        real(p), allocatable :: x8(:,:,:,:)
        real(p), allocatable :: x9(:,:)
        real(p), allocatable :: x10(:,:,:,:)

        real(p), allocatable :: d1(:,:,:,:)
        real(p), allocatable :: f2(:,:,:,:,:,:)

        real(p), allocatable :: z9(:,:,:,:)
        real(p), allocatable :: z10(:,:,:,:)
        real(p), allocatable :: z11(:,:,:,:)
        real(p), allocatable :: z12(:,:,:,:)
        real(p), allocatable :: z13(:,:,:,:)
        real(p), allocatable :: z14(:,:,:,:)

        integer :: k1, k2, k3, k4
        integer :: i1, i2, i3

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
             intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2


            ! From Z9
            allocate(x7(n0+1:n1,n1+1:n3))
            x7 = 0.0_p
            call sumx12(0,n3,n0,n1,n1,n3,x7,fockr, 1.000)

            allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
            allocate(z9(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            i2=k1*k1*k3*k3
            i3=k3*k1
            call egemm2(i2,i3,x7,f2,z9)
            deallocate(f2)

            v2a=v2a+z9
            deallocate(z9)
            deallocate(x7)



            ! From Z10
            allocate(x8(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
            x8 = 0.0_p
            call sumx2143(n0,n3,n0,n1,n0,n1,n1,n3,n0,n1,x8,intr,1.000)

            allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1, &
                 n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)

            allocate(z10(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            i1=k1
            i2=k1*k3*k3
            i3=k3*k1*k1
            call egemm(i1,i2,i3,x8,f2,z10)

            deallocate(f2)

            v2a=v2a+0.500*z10
            call sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z10,-0.500)
            deallocate(z10)
            deallocate(x8)



            ! From Z11
            allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
            call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)

            allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)

            allocate(z11(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
            i1=k3
            i2=k1*k1*k3
            i3=k3*k3*k1
            call egemm(i1,i2,i3,d1,f2,z11)

            deallocate(d1)
            deallocate(f2)

            call sum2341(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z11,0.500)
            call sum1342(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z11,-0.500)
            deallocate(z11)


            ! From Z12
            allocate(x9(n0+1:n2,n2+1:n3))
            x9 = 0.0_p

            call sumx12(0,n3,n0,n2,n2,n3,x9,fockb, 1.000)

            allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)

            allocate(z12(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            i2=k1*k1*k3*k3
            i3=k4*k2
            call egemm2(i2,i3,x9,f2,z12)

            deallocate(f2)

            v2a=v2a+z12
            deallocate(z12)
            deallocate(x9)


            ! From Z13
            allocate(x10(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
            x10 = 0.0_p
            call sumx2143(n0,n3,n0,n2,n0,n1,n2,n3,n0,n1,x10,intm,1.000)

            allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)
            allocate(z13(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            i1=k1
            i2=k1*k3*k3
            i3=k4*k1*k2
            call egemm(i1,i2,i3,x10,f2,z13)
            deallocate(f2)

            v2a=v2a+z13
            call sum1243(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z13,-1.000)
            deallocate(z13)
            deallocate(x10)



            ! From Z14
            allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
            call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)

            allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
            call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)

            allocate(z14(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
            i1=k3
            i2=k1*k1*k3
            i3=k3*k4*k2
            call egemm(i1,i2,i3,d1,f2,z14)

            deallocate(d1)
            deallocate(f2)

            call sum2341(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z14,1.000)
            call sum1342(n1,n3,n1,n3,n0,n1,n0,n1,v2a,z14,-1.000)
            deallocate(z14)

        end associate


    end subroutine t3_on_t2a

    subroutine t3_on_t2b(sys, t3b, t3c, v2b)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(in) :: sys

        real(p), intent(in) :: t3b(:,:,:,:,:,:)
        real(p), intent(in) :: t3c(:,:,:,:,:,:)
        real(p), intent(in out) :: v2b(:,:,:,:)

        real(p), allocatable :: x15(:,:)
        real(p), allocatable :: x16(:,:,:,:)
        real(p), allocatable :: x17(:,:,:,:)
        real(p), allocatable :: x18(:,:)
        real(p), allocatable :: x19(:,:,:,:)
        real(p), allocatable :: x20(:,:,:,:)

        real(p), allocatable :: d1(:,:,:,:)
        real(p), allocatable :: f2(:,:,:,:,:,:)

        real(p), allocatable :: z17(:,:,:,:)
        real(p), allocatable :: z18(:,:,:,:)
        real(p), allocatable :: z19(:,:,:,:)
        real(p), allocatable :: z20(:,:,:,:)
        real(p), allocatable :: z21(:,:,:,:)
        real(p), allocatable :: z22(:,:,:,:)
        real(p), allocatable :: z23(:,:,:,:)
        real(p), allocatable :: z24(:,:,:,:)
        real(p), allocatable :: z25(:,:,:,:)
        real(p), allocatable :: z26(:,:,:,:)

        integer :: k1, k2, k3, k4
        integer :: i1, i2, i3

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
             intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2


            ! From Z17
            allocate(x15(n0+1:n1,n1+1:n3))
            x15 = 0.0_p
            call sumx12(0,n3,n0,n1,n1,n3,x15,fockr, 1.000)

            allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
            allocate(z17(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            i2=k1*k2*k3*k4
            i3=k3*k1
            call egemm2(i2,i3,x15,f2,z17)

            deallocate(f2)
            v2b=v2b+z17
            deallocate(z17)
            deallocate(x15)


            ! From Z18
            allocate(x16(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
            x16 = 0.0_p

            call sumx2143(n0,n3,n0,n1,n0,n1,n1,n3,n0,n1,x16,intr,1.000)

            allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
            call reorder562134(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,t3b,f2)
            allocate(z18(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            i1=k1
            i2=k2*k3*k4
            i3=k3*k1*k1
            call egemm(i1,i2,i3,x16,f2,z18)
            deallocate(f2)

            v2b=v2b-0.500*z18
            deallocate(z18)
            deallocate(x16)



            ! From Z19
            allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
            call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
            allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
            call reorder523146(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3b,f2)
            allocate(z19(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
            i1=k3
            i2=k1*k2*k4
            i3=k3*k3*k1
            call egemm(i1,i2,i3,d1,f2,z19)
            deallocate(d1)
            deallocate(f2)

            call sum1342(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z19,0.500)
            deallocate(z19)


            ! From Z20
            allocate(x17(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
            x17 = 0.0_p
            call sumx2134(n0,n3,n0,n2,n0,n1,n1,n3,n0,n2,x17,intm,1.000)

            allocate(f2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
            call reorder452136(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n2,n0,n1,n1,n3,n2,n3,n1,n3,n0,n1,t3b,f2)
            allocate(z20(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
            i1=k2
            i2=k1*k3*k4
            i3=k3*k1*k2
            call egemm(i1,i2,i3,x17,f2,z20)
            deallocate(f2)

            call sum1243(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z20,-1.000)
            deallocate(z20)
            deallocate(x17)


            ! From Z21
            allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
            call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n1,n2,n3,n1,n3,n2,n3,intm,d1)
            allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1, &
                 n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)

            allocate(z21(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
            i1=k4
            i2=k1*k2*k3
            i3=k3*k4*k1
            call egemm(i1,i2,i3,d1,f2,z21)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z21,1.000)
            deallocate(z21)


            ! From Z22
            allocate(x18(n0+1:n2,n2+1:n3))
            x18 = 0.0_p
            call sumx12(0,n3,n0,n2,n2,n3,x18,fockb, 1.000)

            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
            allocate(z22(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            i2=k1*k2*k3*k4
            i3=k4*k2
            call egemm2(i2,i3,x18,f2,z22)
            deallocate(f2)

            v2b=v2b+z22
            deallocate(z22)
            deallocate(x18)


            ! From Z23
            allocate(x19(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
            x19 = 0.0_p
            call sumx2143(n0,n3,n0,n2,n0,n1,n2,n3,n0,n1,x19,intm,1.000)

            allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
            call reorder461235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n2,n3,n2,n3,n1,n3,n0,n2,t3c,f2)
            allocate(z23(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            i1=k1
            i2=k2*k3*k4
            i3=k4*k1*k2
            call egemm(i1,i2,i3,x19,f2,z23)
            deallocate(f2)

            v2b=v2b-z23
            deallocate(z23)
            deallocate(x19)


            ! From Z24
            allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
            call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
            call reorder413256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n1,n3,n2,n3,n0,n2,n0,n1,t3c,f2)
            allocate(z24(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3))
            i1=k3
            i2=k1*k2*k4
            i3=k3*k4*k2
            call egemm(i1,i2,i3,d1,f2,z24)
            deallocate(d1)
            deallocate(f2)

            call sum1342(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z24,1.000)
            deallocate(z24)


            ! From Z25
            allocate(x20(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
            x20 = 0.0_p
            call sumx2143(n0,n3,n0,n2,n0,n2,n2,n3,n0,n2,x20,intb,1.000)

            allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
            call reorder451236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n0,n2,n2,n3,n2,n3,n1,n3,n0,n1,t3c,f2)
            allocate(z25(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
            i1=k2
            i2=k1*k3*k4
            i3=k4*k2*k2
            call egemm(i1,i2,i3,x20,f2,z25)
            deallocate(f2)

            call sum1243(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z25,-0.500)
            deallocate(z25)
            deallocate(x20)

            ! From Z26
            allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
            call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
            call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
            allocate(z26(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3))
            i1=k4
            i2=k1*k2*k3
            i3=k4*k4*k2
            call egemm(i1,i2,i3,d1,f2,z26)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n2,n3,n1,n3,n0,n2,n0,n1,v2b,z26,0.500)
            deallocate(z26)

        end associate

    end subroutine t3_on_t2b

    subroutine t3_on_t2c(sys, t3c, t3d, v2c)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys

        real(p), intent(in) :: t3c(:,:,:,:,:,:)
        real(p), intent(in) :: t3d(:,:,:,:,:,:)
        real(p), intent(in out) :: v2c(:,:, :, :)

        real(p), allocatable :: x7(:,:)
        real(p), allocatable :: x8(:,:,:,:)
        real(p), allocatable :: x9(:,:)
        real(p), allocatable :: x10(:,:,:,:)

        real(p), allocatable :: d1(:,:,:,:)
        real(p), allocatable :: f2(:,:,:,:,:,:)

        real(p), allocatable :: z9(:,:,:,:)
        real(p), allocatable :: z10(:,:,:,:)
        real(p), allocatable :: z11(:,:,:,:)
        real(p), allocatable :: z12(:,:,:,:)
        real(p), allocatable :: z13(:,:,:,:)
        real(p), allocatable :: z14(:,:,:,:)

        integer :: k1, k2, k3, k4
        integer :: i1, i2, i3

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
             intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2


            ! From Z9
            allocate(x7(n0+1:n1,n1+1:n3))
            x7 = 0.0_p
            call sumx12(0,n3,n0,n1,n1,n3,x7,fockr, 1.000)

            allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder631245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
            allocate(z9(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            i2=k2*k2*k4*k4
            i3=k3*k1
            call egemm2(i2,i3,x7,f2,z9)
            deallocate(f2)

            v2c=v2c+z9
            deallocate(z9)
            deallocate(x7)


            ! From Z10
            allocate(x8(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
            x8 = 0.0_p
            call sumx2134(n0,n3,n0,n2,n0,n1,n1,n3,n0,n2,x8,intm,1.000)

            allocate(f2(n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
            call reorder463125(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n2,n0,n1,n1,n3,n2,n3,n2,n3,n0,n2,t3c,f2)
            allocate(z10(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            i1=k2
            i2=k2*k4*k4
            i3=k3*k1*k2
            call egemm(i1,i2,i3,x8,f2,z10)
            deallocate(f2)

            v2c=v2c+z10
            call sum1243(n2,n3,n2,n3,n0,n2,n0,n2,v2c,z10,-1.000)
            deallocate(z10)
            deallocate(x8)


            ! From Z11
            allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
            call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n1,n2,n3,n1,n3,n2,n3,intm,d1)
            allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder613245(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1, &
                 n0,n1,n2,n3,n1,n3,n2,n3,n0,n2,n0,n2,t3c,f2)
            allocate(z11(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
            i1=k4
            i2=k2*k2*k4
            i3=k3*k4*k1
            call egemm(i1,i2,i3,d1,f2,z11)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n2,n3,n2,n3,n0,n2,n0,n2,v2c,z11,1.000)
            call sum1342(n2,n3,n2,n3,n0,n2,n0,n2,v2c,z11,-1.000)
            deallocate(z11)


            ! From Z12
            allocate(x9(n0+1:n2,n2+1:n3))
            x9 = 0.0_p
            call sumx12(0,n3,n0,n2,n2,n3,x9,fockb, 1.000)

            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
            allocate(z12(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            i2=k2*k2*k4*k4
            i3=k4*k2
            call egemm2(i2,i3,x9,f2,z12)
            deallocate(f2)

            v2c=v2c+z12
            deallocate(z12)
            deallocate(x9)


            ! From Z13
            allocate(x10(n0+1:n2,n0+1:n2,n2+1:n3,n0+1:n2))
            x10 = 0.0_p
            call sumx2143(n0,n3,n0,n2,n0,n2,n2,n3,n0,n2,x10,intb,1.000)

            allocate(f2(n0+1:n2,n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
            call reorder451236(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2, &
                 n0,n2,n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,t3d,f2)
            allocate(z13(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            i1=k2
            i2=k2*k4*k4
            i3=k4*k2*k2
            call egemm(i1,i2,i3,x10,f2,z13)
            deallocate(f2)

            v2c=v2c+0.500*z13
            call sum1243(n2,n3,n2,n3,n0,n2,n0,n2,v2c,z13,-0.500)
            deallocate(z13)
            deallocate(x10)


            ! From Z14
            allocate(d1(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3))
            call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3, &
                 n0,n2,n2,n3,n2,n3,n2,n3,intb,d1)
            allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
            call reorder412356(n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,n0,n2, &
                 n0,n2,n2,n3,n2,n3,n2,n3,n0,n2,n0,n2,t3d,f2)
            allocate(z14(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
            i1=k4
            i2=k2*k2*k4
            i3=k4*k4*k2
            call egemm(i1,i2,i3,d1,f2,z14)
            deallocate(d1)
            deallocate(f2)

            call sum2341(n2,n3,n2,n3,n0,n2,n0,n2,v2c,z14,0.500)
            call sum1342(n2,n3,n2,n3,n0,n2,n0,n2,v2c,z14,-0.500)
            deallocate(z14)

        end associate



    end subroutine t3_on_t2c

    subroutine hbar_on_t2a(sys, cc, v2A)


        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(in out) :: cc


        real(p), pointer :: t1A(:,:) => null()
        real(p), pointer :: t1B(:,:) => null()

        ! [TMPDEBUG]
        !real(p), pointer :: v2A(:,:,:,:) => null()
        real(p), intent(in out) :: v2A(:,:,:,:)

        real(p),allocatable::B2(:,:)

        real(p),allocatable::Z2(:,:,:,:)
        real(p),allocatable::Z4(:,:,:,:)
        real(p),allocatable::Z5(:,:,:,:)
        real(p),allocatable::Z11(:,:,:,:)

        integer :: k1, k2, k3, k4
        integer :: i1, i2, i3

        t1a(sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a) => &
             cc%t_vec(cc%pos(1):cc%pos(2)-1)
        t1b(sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b) => &
             cc%t_vec(cc%pos(2):cc%pos(3)-1)

        ! [TMPDEBUG]
        !v2a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, &
        !     sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) => &
        !     cc%t_vec(cc%pos(3):cc%pos(4)-1)

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fahh=>sys%ints%fahh, fahp=>sys%ints%fahp, fapp=>sys%ints%fapp, &
             fbhh=>sys%ints%fbhh, fbhp=>sys%ints%fbhp, fbpp=>sys%ints%fbpp, &
             vahhhh=>sys%ints%vahhhh, vahhhp=>sys%ints%vahhhp, vahhpp=>sys%ints%vahhpp, &
             vahphp=>sys%ints%vahphp, vahppp=>sys%ints%vahppp, &
             vbhhhh=>sys%ints%vbhhhh, vbhhhp=>sys%ints%vbhhhp, vbhhph=>sys%ints%vbhhph, &
             vbhhpp=>sys%ints%vbhhpp, vbhphp=>sys%ints%vbhphp, vbhpph=>sys%ints%vbhpph, &
             vbphph=>sys%ints%vbphph, vbhppp=>sys%ints%vbhppp, vbphpp=>sys%ints%vbphpp, &
             vchhhh=>sys%ints%vchhhh, vchhhp=>sys%ints%vchhhp, vchhpp=>sys%ints%vchhpp, &
             vchphp=>sys%ints%vchphp, vchppp=>sys%ints%vchppp, &
             vaappp=>sys%ints%vaappp, vbappp=>sys%ints%vbappp, vbpapp=>sys%ints%vbpapp, &
             vcappp=>sys%ints%vcappp, &
             x1=>cc%ext_cor%x1, &
             x2=>cc%ext_cor%x2, &
             x3=>cc%ext_cor%x3, &
             x4=>cc%ext_cor%x4)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2

            allocate(Z2(N0+1:N1,N1+1:N3,N1+1:N3,N0+1:N1))
            I1=K1*K3*K3
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,X1,t1A,Z2)

            call sum4123(N1,N3,N1,N3,N0,N1,N0,N1,V2A,Z2,-0.500)
            call sum3124(N1,N3,N1,N3,N0,N1,N0,N1,V2A,Z2, 0.500)
            deallocate(Z2)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                 N0,N1,N1,N3,t1A,B2)
            allocate(Z4(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1*K3
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,X2,B2,Z4)
            deallocate(B2)

            V2A=V2A+0.500*Z4
            call sum2134(N1,N3,N1,N3,N0,N1,N0,N1,V2A,Z4,-0.500)
            deallocate(Z4)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                 N0,N1,N1,N3,t1A,B2)
            allocate(Z5(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1*K3*K3
            I3=K3*K1
            call EGEMM1(I1,I3,X3,B2,Z5)
            deallocate(B2)

            V2A=V2A+Z5
            deallocate(Z5)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                 N0,N2,N2,N3,t1B,B2)
            allocate(Z11(N1+1:N3,N1+1:N3,N0+1:N1,N0+1:N1))
            I1=K1*K1*K3*K3
            I3=K4*K2
            call EGEMM1(I1,I3,X4,B2,Z11)
            deallocate(B2)

            V2A=V2A+Z11
            deallocate(Z11)

        end associate

    end subroutine hbar_on_t2a

    subroutine hbar_on_t2c(sys, cc, v2c)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(in out) :: cc

        real(p), pointer :: t1A(:,:) => null()
        real(p), pointer :: t1B(:,:) => null()

        ! [TMPDEBUG]
        !real(p), pointer :: v2C(:,:,:,:) => null()
        real(p), intent(in out) :: v2C(:,:,:,:)

        real(p), allocatable::B2(:,:)

        real(p), allocatable::Z1(:,:,:,:)
        real(p), allocatable::Z4(:,:,:,:)
        real(p), allocatable::Z6(:,:,:,:)
        real(p), allocatable::Z7(:,:,:,:)

        integer :: k1, k2, k3, k4
        integer :: i1, i2, i3

        t1a(sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a) => &
             cc%t_vec(cc%pos(1):cc%pos(2)-1)
        t1b(sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b) => &
             cc%t_vec(cc%pos(2):cc%pos(3)-1)

        ! [TMPDEBUG]
        !v2c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, &
        !     sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) => &
        !     cc%t_vec(cc%pos(5):cc%pos(6)-1)

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fahh=>sys%ints%fahh, fahp=>sys%ints%fahp, fapp=>sys%ints%fapp, &
             fbhh=>sys%ints%fbhh, fbhp=>sys%ints%fbhp, fbpp=>sys%ints%fbpp, &
             vahhhh=>sys%ints%vahhhh, vahhhp=>sys%ints%vahhhp, vahhpp=>sys%ints%vahhpp, &
             vahphp=>sys%ints%vahphp, vahppp=>sys%ints%vahppp, &
             vbhhhh=>sys%ints%vbhhhh, vbhhhp=>sys%ints%vbhhhp, vbhhph=>sys%ints%vbhhph, &
             vbhhpp=>sys%ints%vbhhpp, vbhphp=>sys%ints%vbhphp, vbhpph=>sys%ints%vbhpph, &
             vbphph=>sys%ints%vbphph, vbhppp=>sys%ints%vbhppp, vbphpp=>sys%ints%vbphpp, &
             vchhhh=>sys%ints%vchhhh, vchhhp=>sys%ints%vchhhp, vchhpp=>sys%ints%vchhpp, &
             vchphp=>sys%ints%vchphp, vchppp=>sys%ints%vchppp, &
             vaappp=>sys%ints%vaappp, vbappp=>sys%ints%vbappp, vbpapp=>sys%ints%vbpapp, &
             vcappp=>sys%ints%vcappp, &
             x5=>cc%ext_cor%x5, &
             x6=>cc%ext_cor%x6, &
             x7=>cc%ext_cor%x7, &
             x8=>cc%ext_cor%x8)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                 N0,N1,N1,N3,t1A,B2)
            allocate(Z1(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2*K4*K4
            I3=K3*K1
            call EGEMM1(I1,I3,X5,B2,Z1)
            deallocate(B2)

            V2C=V2C+Z1
            deallocate(Z1)

            allocate(Z4(N0+1:N2,N2+1:N3,N2+1:N3,N0+1:N2))
            I1=K2*K4*K4
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,X6,t1B,Z4)

            call &
                 sum4123(N2,N3,N2,N3,N0,N2,N0,N2,V2C,Z4, 1.000)
            call &
                 sum3124(N2,N3,N2,N3,N0,N2,N0,N2,V2C,Z4,-1.000)
            deallocate(Z4)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                 N0,N2,N2,N3,t1B,B2)
            allocate(Z6(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2*K4
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,X7,B2,Z6)
            deallocate(B2)

            V2C=V2C-Z6
            call &
                 sum2134(N2,N3,N2,N3,N0,N2,N0,N2,V2C,Z6, 1.000)
            deallocate(Z6)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                 N0,N2,N2,N3,t1B,B2)
            allocate(Z7(N2+1:N3,N2+1:N3,N0+1:N2,N0+1:N2))
            I1=K2*K2*K4*K4
            I3=K4*K2
            call EGEMM1(I1,I3,X8,B2,Z7)
            deallocate(B2)

            V2C=V2C+Z7
            deallocate(Z7)

        end associate

    end subroutine hbar_on_t2c

    subroutine hbar_on_t2b(sys, cc, v2b)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        type(sys_t), intent(in) :: sys
        type(cc_t), target, intent(in out) :: cc

        real(p), pointer :: t1A(:,:) => null()
        real(p), pointer :: t1B(:,:) => null()

        ! [TMPDEBUG]
        !real(p), pointer :: v2B(:,:,:,:) => null()
        real(p), intent(in out) :: v2B(:,:,:,:)

        real(p), allocatable :: B2(:,:)

        real(p), allocatable :: Z2(:,:,:,:)
        real(p), allocatable :: Z4(:,:,:,:)
        real(p), allocatable :: Z5(:,:,:,:)
        real(p), allocatable :: Z12(:,:,:,:)
        real(p), allocatable :: Z14(:,:,:,:)
        real(p), allocatable :: Z15(:,:,:,:)

        integer :: k1, k2, k3, k4
        integer :: i1, i2, i3

        t1a(sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a) => &
             cc%t_vec(cc%pos(1):cc%pos(2)-1)
        t1b(sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b) => &
             cc%t_vec(cc%pos(2):cc%pos(3)-1)

        ! [TMPDEBUG]
        !v2b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, &
        !     sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) => &
        !     cc%t_vec(cc%pos(4):cc%pos(5)-1)

        associate(n0=>sys%froz, n1=>sys%occ_a, n2=>sys%occ_b, n3=>sys%orbs, &
             fahh=>sys%ints%fahh, fahp=>sys%ints%fahp, fapp=>sys%ints%fapp, &
             fbhh=>sys%ints%fbhh, fbhp=>sys%ints%fbhp, fbpp=>sys%ints%fbpp, &
             vahhhh=>sys%ints%vahhhh, vahhhp=>sys%ints%vahhhp, vahhpp=>sys%ints%vahhpp, &
             vahphp=>sys%ints%vahphp, vahppp=>sys%ints%vahppp, &
             vbhhhh=>sys%ints%vbhhhh, vbhhhp=>sys%ints%vbhhhp, vbhhph=>sys%ints%vbhhph, &
             vbhhpp=>sys%ints%vbhhpp, vbhphp=>sys%ints%vbhphp, vbhpph=>sys%ints%vbhpph, &
             vbphph=>sys%ints%vbphph, vbhppp=>sys%ints%vbhppp, vbphpp=>sys%ints%vbphpp, &
             vchhhh=>sys%ints%vchhhh, vchhhp=>sys%ints%vchhhp, vchhpp=>sys%ints%vchhpp, &
             vchphp=>sys%ints%vchphp, vchppp=>sys%ints%vchppp, &
             vaappp=>sys%ints%vaappp, vbappp=>sys%ints%vbappp, vbpapp=>sys%ints%vbpapp, &
             vcappp=>sys%ints%vcappp, &
             x9=>cc%ext_cor%x9, &
             x10=>cc%ext_cor%x10, &
             x11=>cc%ext_cor%x11, &
             x12=>cc%ext_cor%x12, &
             x13=>cc%ext_cor%x13, &
             x14=>cc%ext_cor%x14)

            k1 = n1 - n0
            k2 = n2 - n0
            k3 = n3 - n1
            k4 = n3 - n2

            allocate(Z2(N0+1:N1,N2+1:N3,N1+1:N3,N0+1:N2))
            I1=K2*K3*K4
            I2=K1
            I3=K3
            call EGEMM(I1,I2,I3,X9,t1A,Z2)

            call sum4123(N2,N3,N1,N3,N0,N2,N0,N1,V2B,Z2, 0.500)
            deallocate(Z2)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                 N0,N1,N1,N3,t1A,B2)
            allocate(Z4(N1+1:N3,N2+1:N3,N0+1:N2,N0+1:N1))
            I1=K1*K2*K4
            I2=K3
            I3=K1
            call EGEMM(I1,I2,I3,X10,B2,Z4)
            deallocate(B2)

            call sum2134(N2,N3,N1,N3,N0,N2,N0,N1,V2B,Z4,-0.500)
            deallocate(Z4)

            allocate(B2(N0+1:N1,N1+1:N3))
            call reorder21(N1,N3,N0,N1, &
                 N0,N1,N1,N3,t1A,B2)
            allocate(Z5(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I1=K1*K2*K3*K4
            I3=K3*K1
            call EGEMM1(I1,I3,X11,B2,Z5)
            deallocate(B2)

            V2B=V2B+Z5
            deallocate(Z5)

            allocate(Z12(N0+1:N2,N2+1:N3,N1+1:N3,N0+1:N1))
            I1=K1*K3*K4
            I2=K2
            I3=K4
            call EGEMM(I1,I2,I3,X12,t1B,Z12)

            call sum3124(N2,N3,N1,N3,N0,N2,N0,N1,V2B,Z12,-1.000)
            deallocate(Z12)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                 N0,N2,N2,N3,t1B,B2)
            allocate(Z14(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I1=K1*K2*K3
            I2=K4
            I3=K2
            call EGEMM(I1,I2,I3,X13,B2,Z14)
            deallocate(B2)

            V2B=V2B-Z14
            deallocate(Z14)

            allocate(B2(N0+1:N2,N2+1:N3))
            call reorder21(N2,N3,N0,N2, &
                 N0,N2,N2,N3,t1B,B2)
            allocate(Z15(N2+1:N3,N1+1:N3,N0+1:N2,N0+1:N1))
            I1=K1*K2*K3*K4
            I3=K4*K2
            call EGEMM1(I1,I3,X14,B2,Z15)
            deallocate(B2)

            V2B=V2B+Z15
            deallocate(Z15)

        end associate

    end subroutine hbar_on_t2b

end module contract_t3
