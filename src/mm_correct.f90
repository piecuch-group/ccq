module mm_correct

    implicit none

contains

    subroutine crcc23(sys, run, cc)

        use const, only: p, l_unit
        use system, only: sys_t, run_t
        use cc_types, only: cc_t
        use printing, only: print_date, print_cct3


        type(sys_t), intent(in) :: sys
        type(run_t), intent(in) :: run
        type(cc_t), intent(inout), target :: cc

        real(p), pointer :: t(:) => null()
        real(p), pointer :: l(:) => null()
        real(p), pointer :: lh(:) => null()



        real(p) :: res,pp,shift,lhm,r0,ecc
        real(p) :: da,db,dc,dd

        real(p) :: e23a
        real(p) :: e23b
        real(p) :: e23c
        real(p) :: e23d
        real(p), allocatable :: r(:)
        real(p), allocatable :: lh3(:,:,:,:,:,:)
        real(p), allocatable :: mm3(:,:,:,:,:,:)
        real(p), allocatable :: d3a1(:,:,:)
        real(p), allocatable :: d3a2(:,:,:)
        real(p), allocatable :: d3b1(:,:,:)
        real(p), allocatable :: d3b2(:,:,:)
        real(p), allocatable :: d3c1(:,:,:)
        real(p), allocatable :: d3c2(:,:,:)
        real(p), allocatable :: d3d1(:,:,:)
        real(p), allocatable :: d3d2(:,:,:)

        ! Compatibility vars
        integer :: a,b,c,d,e,f
        integer :: i, j, k
        integer :: nroot, iroot
        integer :: k1a, k1b, k2a, k2b, k2c, k3a, k3b, k3c, k3d, kkk
        integer :: l2b, l2c
        integer :: m1, m2
        integer :: k1, k2, k3, k4

        call print_date('  MM 23 program started on:')

        ! Compatibility layer
        iroot = 0
        pp = 0.0_p
        r0 = 1.0_p

        m1 = sys%act_occ_b
        m2 = sys%act_unocc_a
        k1 = sys%occ_a - sys%froz
        k2 = sys%occ_b - sys%froz
        k3 = sys%orbs - sys%occ_a
        k4 = sys%orbs - sys%occ_b

        k1a = cc%pos(1)
        k1b = cc%pos(2)
        k2a = cc%pos(3)
        k2b = cc%pos(4)
        k2c = cc%pos(5)

        t => cc%t_vec
        l => cc%l_vec
        lh => cc%lh_vec


        ! Variable declaration
        e23a = 0.0_p
        e23b = 0.0_p
        e23c = 0.0_p
        e23d = 0.0_p
        allocate(d3a1(sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_a,sys%froz+1:sys%occ_a))
        allocate(d3a2(sys%occ_a+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_a))
        allocate(d3b1(sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_a))
        allocate(d3b2(sys%occ_b+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_a))
        allocate(d3c1(sys%occ_b+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_a))
        allocate(d3c2(sys%occ_b+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_b))
        allocate(d3d1(sys%occ_b+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_b))
        allocate(d3d2(sys%occ_b+1:sys%orbs,sys%occ_b+1:sys%orbs,sys%froz+1:sys%occ_b))
        d3a1=0.0_p
        d3a2=0.0_p
        d3b1=0.0_p
        d3b2=0.0_p
        d3c1=0.0_p
        d3c2=0.0_p
        d3d1=0.0_p
        d3d2=0.0_p

        associate(fockr=>sys%ints%f_a, fockb=>sys%ints%f_b, &
                intr=>sys%ints%v_aa, intb=>sys%ints%v_bb, intm=>sys%ints%v_ab, &
                h1a=>cc%hbar%a, h1b=>cc%hbar%b, h2a=>cc%hbar%aa, h2b=>cc%hbar%ab, &
                h2c=>cc%hbar%bb)

            call cal_d3(sys, cc, &
                d3a1,d3a2,d3b1,d3b2,d3c1,d3c2,d3d1,d3d2)

            allocate(lh3(sys%occ_a+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_a,sys%froz+1:sys%occ_a,sys%froz+1:sys%occ_a))
            lh3=0.0_p
            call l3a_update(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,lh3, &
                k1,k2,k3,k4, &
                fockr,fockb,intr,intb,intm, &
                h1a,h1b,h2a,h2b,h2c, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                l(k1a),l(k1b),l(k2a),l(k2b),l(k2c))

            allocate(mm3(sys%occ_a+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_a,sys%froz+1:sys%occ_a,sys%froz+1:sys%occ_a))
            mm3=0.0_p
            if(dabs(r0).gt.1.0d-12)then
                call mm_t3a(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,mm3, &
                    k1,k2,k3,k4, &
                    fockr,fockb,intr,intb,intm, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))
                mm3=r0*mm3
            endif

            !call print_ra(1111,sys%froz,sys%occ_a,sys%occ_b,sys%orbs,k3b-k3a,mm3,1.0d-3)

            ! loop that has to be changed to fit the active space
            do i=sys%froz+1,sys%occ_a-2
                do j=i+1,sys%occ_a-1
                    do k=j+1,sys%occ_a
                        do a=sys%occ_a+1,sys%orbs-2
                            do b=a+1,sys%orbs-1
                                do c=b+1,sys%orbs
                                    if (k > m1 .and. a <= m2) cycle
                                    da=pp+fockr(i,i)+fockr(j,j)+fockr(k,k) &
                                        -fockr(a,a)-fockr(b,b)-fockr(c,c)
                                    db=pp+h1a(i,i)+h1a(j,j)+h1a(k,k) &
                                        -h1a(a,a)-h1a(b,b)-h1a(c,c)
                                    dc=db+h2a(a,i,a,i)+h2a(b,i,b,i)+h2a(c,i,c,i) &
                                        +h2a(a,j,a,j)+h2a(b,j,b,j)+h2a(c,j,c,j) &
                                        +h2a(a,k,a,k)+h2a(b,k,b,k)+h2a(c,k,c,k) &
                                        -h2a(j,i,j,i)-h2a(k,i,k,i)-h2a(k,j,k,j) &
                                        -h2a(b,a,b,a)-h2a(c,a,c,a)-h2a(c,b,c,b)
                                    dd=dc+d3a1(a,j,i)+d3a1(a,k,i)+d3a1(a,k,j) &
                                        +d3a1(b,j,i)+d3a1(b,k,i)+d3a1(b,k,j) &
                                        +d3a1(c,j,i)+d3a1(c,k,i)+d3a1(c,k,j) &
                                        -d3a2(b,a,i)-d3a2(b,a,j)-d3a2(b,a,k) &
                                        -d3a2(c,a,i)-d3a2(c,a,j)-d3a2(c,a,k) &
                                        -d3a2(c,b,i)-d3a2(c,b,j)-d3a2(c,b,k)
                                    lhm=lh3(c,b,a,k,j,i)*mm3(c,b,a,k,j,i)
                                    e23a = e23a +lhm/da!/36.0_p
                                    e23b = e23b +lhm/db!/36.0_p
                                    e23c = e23c +lhm/dc!/36.0_p
                                    e23d = e23d +lhm/dd!/36.0_p
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
            deallocate(lh3,mm3)

            allocate(lh3(sys%occ_b+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_a,sys%froz+1:sys%occ_a))
            lh3=0.0_p
            call l3b_update(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,lh3, &
                k1,k2,k3,k4, &
                fockr,fockb,intr,intb,intm, &
                h1a,h1b,h2a,h2b,h2c, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                l(k1a),l(k1b),l(k2a),l(k2b),l(k2c))

            allocate(mm3(sys%occ_b+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_a,sys%froz+1:sys%occ_a))
            mm3=0.0_p
            if(dabs(r0).gt.1.0d-12)then
                call mm_t3b(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,mm3, &
                    k1,k2,k3,k4, &
                    fockr,fockb,intr,intb,intm, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))
                mm3=r0*mm3
            endif
            !call print_rb(2222,sys%froz,sys%occ_a,sys%occ_b,sys%orbs,k3c-k3b,mm3,1.0d-3)
            do i=sys%froz+1,sys%occ_a-1
                do j=i+1,sys%occ_a
                    do k=sys%froz+1,sys%occ_b
                        do a=sys%occ_a+1,sys%orbs-1
                            do b=a+1,sys%orbs
                                do c=sys%occ_b+1,sys%orbs
                                    if ((j > m1 .or. k > m1) .and. (a <= m2 .or. c <= m2)) cycle
                                    da=pp+fockr(i,i)+fockr(j,j)+fockb(k,k) &
                                        -fockr(a,a)-fockr(b,b)-fockb(c,c)
                                    db=pp+h1a(i,i)+h1a(j,j)+h1b(k,k) &
                                        -h1a(a,a)-h1a(b,b)-h1b(c,c)
                                    dc=db+h2a(a,i,a,i)+h2a(b,i,b,i)+h2b(c,i,c,i) &
                                        +h2a(a,j,a,j)+h2a(b,j,b,j)+h2b(c,j,c,j) &
                                        +h2b(k,a,k,a)+h2b(k,b,k,b)+h2c(c,k,c,k) &
                                        -h2a(j,i,j,i)-h2b(k,i,k,i)-h2b(k,j,k,j) &
                                        -h2a(b,a,b,a)-h2b(c,a,c,a)-h2b(c,b,c,b)
                                    dd=dc+d3a1(a,j,i)+d3b1(a,k,i)+d3b1(a,k,j) &
                                        +d3a1(b,j,i)+d3b1(b,k,i)+d3b1(b,k,j) &
                                        +d3c1(c,k,i)+d3c1(c,k,j) &
                                        -d3a2(b,a,i)-d3a2(b,a,j) &
                                        -d3b2(c,a,i)-d3b2(c,a,j)-d3c2(c,a,k) &
                                        -d3b2(c,b,i)-d3b2(c,b,j)-d3c2(c,b,k)
                                    lhm=lh3(c,b,a,k,j,i)*mm3(c,b,a,k,j,i)
                                    e23a = e23a+lhm/da!/4.0_p
                                    e23b = e23b+lhm/db!/4.0_p
                                    e23c = e23c+lhm/dc!/4.0_p
                                    e23d = e23d+lhm/dd!/4.0_p
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
            deallocate(lh3,mm3)

            allocate(lh3(sys%occ_b+1:sys%orbs,sys%occ_b+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_a))
            lh3=0.0_p
            call l3c_update(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,lh3, &
                k1,k2,k3,k4, &
                fockr,fockb,intr,intb,intm, &
                h1a,h1b,h2a,h2b,h2c, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                l(k1a),l(k1b),l(k2a),l(k2b),l(k2c))

            allocate(mm3(sys%occ_b+1:sys%orbs,sys%occ_b+1:sys%orbs,sys%occ_a+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_a))
            mm3=0.0_p
            if(dabs(r0).gt.1.0d-12)then
                call mm_t3c(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,mm3, &
                    k1,k2,k3,k4, &
                    fockr,fockb,intr,intb,intm, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))
                mm3=r0*mm3
            endif

            !call print_rc(3333,sys%froz,sys%occ_a,sys%occ_b,sys%orbs,k3d-k3c,mm3,1.0d-3)

            do i=sys%froz+1,sys%occ_a
                do j=sys%froz+1,sys%occ_b-1
                    do k=j+1,sys%occ_b
                        do a=sys%occ_a+1,sys%orbs
                            do b=sys%occ_b+1,sys%orbs-1
                                do c=b+1,sys%orbs
                                    if ((i > m1 .or. k > m1) .and. (a <= m2 .or. b <= m2)) cycle
                                    da=pp+fockr(i,i)+fockb(j,j)+fockb(k,k) &
                                        -fockr(a,a)-fockb(b,b)-fockb(c,c)
                                    db=pp+h1a(i,i)+h1b(j,j)+h1b(k,k) &
                                        -h1a(a,a)-h1b(b,b)-h1b(c,c)
                                    dc=db+h2a(a,i,a,i)+h2b(b,i,b,i)+h2b(c,i,c,i) &
                                        +h2b(j,a,j,a)+h2c(b,j,b,j)+h2c(c,j,c,j) &
                                        +h2b(k,a,k,a)+h2c(b,k,b,k)+h2c(c,k,c,k) &
                                        -h2b(j,i,j,i)-h2b(k,i,k,i)-h2c(k,j,k,j) &
                                        -h2b(b,a,b,a)-h2b(c,a,c,a)-h2c(c,b,c,b)
                                    dd=dc+d3b1(a,j,i)+d3b1(a,k,i) &
                                        +d3c1(b,j,i)+d3c1(b,k,i)+d3d1(b,k,j) &
                                        +d3c1(c,j,i)+d3c1(c,k,i)+d3d1(c,k,j) &
                                        -d3b2(b,a,i)-d3c2(b,a,j)-d3c2(b,a,k) &
                                        -d3b2(c,a,i)-d3c2(c,a,j)-d3c2(c,a,k) &
                                        -d3d2(c,b,j)-d3d2(c,b,k)
                                    lhm=lh3(c,b,a,k,j,i)*mm3(c,b,a,k,j,i)
                                    e23a = e23a+lhm/da!/4.0_p
                                    e23b = e23b+lhm/db!/4.0_p
                                    e23c = e23c+lhm/dc!/4.0_p
                                    e23d = e23d+lhm/dd!/4.0_p
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
            deallocate(lh3,mm3)

            allocate(lh3(sys%occ_b+1:sys%orbs,sys%occ_b+1:sys%orbs,sys%occ_b+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_b))
            lh3=0.0_p
            call l3d_update(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,lh3, &
                k1,k2,k3,k4, &
                fockr,fockb,intr,intb,intm, &
                h1a,h1b,h2a,h2b,h2c, &
                t(k1a),t(k1b),t(k2a),t(k2b),t(k2c), &
                l(k1a),l(k1b),l(k2a),l(k2b),l(k2c))

            allocate(mm3(sys%occ_b+1:sys%orbs,sys%occ_b+1:sys%orbs,sys%occ_b+1:sys%orbs,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_b,sys%froz+1:sys%occ_b))
            mm3=0.0_p
            if(dabs(r0).gt.1.0d-12)then
                call mm_t3d(sys%froz,sys%occ_a,sys%occ_b,sys%orbs,mm3, &
                    k1,k2,k3,k4, &
                    fockr,fockb,intr,intb,intm, &
                    t(k1a),t(k1b),t(k2a),t(k2b),t(k2c))
                mm3=r0*mm3
            endif

            !call print_rd(4444,sys%froz,sys%occ_a,sys%occ_b,sys%orbs,kkk-k3d+1,mm3,1.0d-3)

            do i=sys%froz+1,sys%occ_b-2
                do j=i+1,sys%occ_b-1
                    do k=j+1,sys%occ_b
                        do a=sys%occ_b+1,sys%orbs-2
                            do b=a+1,sys%orbs-1
                                do c=b+1,sys%orbs
                                    if (k > m1 .and. a <= m2) cycle
                                    da=pp+fockb(i,i)+fockb(j,j)+fockb(k,k) &
                                        -fockb(a,a)-fockb(b,b)-fockb(c,c)
                                    db=pp+h1b(i,i)+h1b(j,j)+h1b(k,k) &
                                        -h1b(a,a)-h1b(b,b)-h1b(c,c)
                                    dc=db+h2c(a,i,a,i)+h2c(b,i,b,i)+h2c(c,i,c,i) &
                                        +h2c(a,j,a,j)+h2c(b,j,b,j)+h2c(c,j,c,j) &
                                        +h2c(a,k,a,k)+h2c(b,k,b,k)+h2c(c,k,c,k) &
                                        -h2c(j,i,j,i)-h2c(k,i,k,i)-h2c(k,j,k,j) &
                                        -h2c(b,a,b,a)-h2c(c,a,c,a)-h2c(c,b,c,b)
                                    dd=dc+d3d1(a,j,i)+d3d1(a,k,i)+d3d1(a,k,j) &
                                        +d3d1(b,j,i)+d3d1(b,k,i)+d3d1(b,k,j) &
                                        +d3d1(c,j,i)+d3d1(c,k,i)+d3d1(c,k,j) &
                                        -d3d2(b,a,i)-d3d2(b,a,j)-d3d2(b,a,k) &
                                        -d3d2(c,a,i)-d3d2(c,a,j)-d3d2(c,a,k) &
                                        -d3d2(c,b,i)-d3d2(c,b,j)-d3d2(c,b,k)
                                    lhm=lh3(c,b,a,k,j,i)*mm3(c,b,a,k,j,i)
                                    e23a =e23a+lhm/da!/36.0_p
                                    e23b =e23b+lhm/db!/36.0_p
                                    e23c =e23c+lhm/dc!/36.0_p
                                    e23d =e23d+lhm/dd!/36.0_p
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
            enddo
            deallocate(lh3,mm3)

        end associate


        cc%mm_en_cor_a = e23a
        cc%mm_en_cor_b = e23b
        cc%mm_en_cor_c = e23c
        cc%mm_en_cor_d = e23d

        call print_cct3(sys,cc)
        call print_date('  MM 23 program ended on:')
        cc%en_cor = cc%en_cor + e23d

    end subroutine crcc23

    subroutine cal_d3(sys, cc, &
            d3a1,d3a2,d3b1,d3b2,d3c1,d3c2,d3d1,d3d2)

        use const, only: p
        use system, only: sys_t
        use cc_types, only: cc_t

        implicit none

        type(sys_t), intent(in) :: sys
        type(cc_t), intent(in), target :: cc

        real(p), allocatable, intent(inout) :: d3a1(:,:,:)
        real(p), allocatable, intent(inout) :: d3a2(:,:,:)
        real(p), allocatable, intent(inout) :: d3b1(:,:,:)
        real(p), allocatable, intent(inout) :: d3b2(:,:,:)
        real(p), allocatable, intent(inout) :: d3c1(:,:,:)
        real(p), allocatable, intent(inout) :: d3c2(:,:,:)
        real(p), allocatable, intent(inout) :: d3d1(:,:,:)
        real(p), allocatable, intent(inout) :: d3d2(:,:,:)

        integer :: a,b,e
        integer :: i,j,m
        real(p) :: pp

        real(p), pointer :: t2a(:,:,:,:) => null()
        real(p), pointer :: t2b(:,:,:,:) => null()
        real(p), pointer :: t2c(:,:,:,:) => null()

        t2a(sys%occ_a+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_a, sys%froz+1:sys%occ_a) => &
            cc%t_vec(cc%pos(3):cc%pos(4) - 1)
        t2b(sys%occ_b+1:sys%orbs, sys%occ_a+1:sys%orbs, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_a) => &
            cc%t_vec(cc%pos(4):cc%pos(5) - 1)
        t2c(sys%occ_b+1:sys%orbs, sys%occ_b+1:sys%orbs, sys%froz+1:sys%occ_b, sys%froz+1:sys%occ_b) => &
            cc%t_vec(cc%pos(5):cc%pos(6) - 1)

        associate(h1a=>cc%hbar%a, h1b=>cc%hbar%b, &
                h2a=>cc%hbar%aa, h2b=>cc%hbar%ab, h2c=>cc%hbar%bb)

            do i=sys%froz+1,sys%occ_a
                do j=sys%froz+1,sys%occ_a
                    do a=sys%occ_a+1,sys%orbs
                        pp=0.0_p
                        do e=sys%occ_a+1,sys%orbs
                            pp=pp+h2a(e,a,j,i)*t2a(e,a,j,i)
                        enddo
                        d3a1(a,j,i)=pp
                    enddo
                enddo
            enddo

            do i=sys%froz+1,sys%occ_a
                do a=sys%occ_a+1,sys%orbs
                    do b=sys%occ_a+1,sys%orbs
                        pp=0.0_p
                        do m=sys%froz+1,sys%occ_a
                            pp=pp+h2a(b,a,m,i)*t2a(b,a,m,i)
                        enddo
                        d3a2(b,a,i)=-pp
                    enddo
                enddo
            enddo

            do i=sys%froz+1,sys%occ_a
                do j=sys%froz+1,sys%occ_b
                    do a=sys%occ_a+1,sys%orbs
                        pp=0.0_p
                        do e=sys%occ_b+1,sys%orbs
                            pp=pp+h2b(e,a,j,i)*t2b(e,a,j,i)
                        enddo
                        d3b1(a,j,i)=pp
                    enddo
                enddo
            enddo
            do i=sys%froz+1,sys%occ_a
                do a=sys%occ_a+1,sys%orbs
                    do b=sys%occ_b+1,sys%orbs
                        pp=0.0_p
                        do m=sys%froz+1,sys%occ_b
                            pp=pp+h2b(b,a,m,i)*t2b(b,a,m,i)
                        enddo
                        d3b2(b,a,i)=-pp
                    enddo
                enddo
            enddo

            do i=sys%froz+1,sys%occ_a
                do j=sys%froz+1,sys%occ_b
                    do b=sys%occ_b+1,sys%orbs
                        pp=0.0_p
                        do e=sys%occ_a+1,sys%orbs
                            pp=pp+h2b(b,e,j,i)*t2b(b,e,j,i)
                        enddo
                        d3c1(b,j,i)=pp
                    enddo
                enddo
            enddo
            do j=sys%froz+1,sys%occ_b
                do a=sys%occ_a+1,sys%orbs
                    do b=sys%occ_b+1,sys%orbs
                        pp=0.0_p
                        do m=sys%froz+1,sys%occ_a
                            pp=pp+h2b(b,a,j,m)*t2b(b,a,j,m)
                        enddo
                        d3c2(b,a,j)=-pp
                    enddo
                enddo
            enddo

            do i=sys%froz+1,sys%occ_b
                do j=sys%froz+1,sys%occ_b
                    do a=sys%occ_b+1,sys%orbs
                        pp=0.0_p
                        do e=sys%occ_a+2,sys%orbs
                            pp=pp+h2c(e,a,j,i)*t2c(e,a,j,i)
                        enddo
                        d3d1(a,j,i)=pp
                    enddo
                enddo
            enddo
            do i=sys%froz+1,sys%occ_b
                do a=sys%occ_b+1,sys%orbs
                    do b=sys%occ_b+1,sys%orbs
                        pp=0.0_p
                        do m=sys%froz+1,sys%occ_b
                            pp=pp+h2c(b,a,m,i)*t2c(b,a,m,i)
                        enddo
                        d3d2(b,a,i)=-pp
                    enddo
                enddo
            enddo

        end associate


    end subroutine cal_d3
end module mm_correct
