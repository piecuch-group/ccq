module integrals

    implicit none


contains

    subroutine load_ints(sys, run)

        use const, only: p, tmp_unit
        use system, only: sys_t, run_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run

        real(p), allocatable :: e1int(:,:)
        real(p), allocatable :: e2int(:,:,:,:)
        real(p) :: hh

        integer :: ios
        integer :: indx
        integer :: i, j, a, b


        allocate(e1int(sys%orbs, sys%orbs))

        open(tmp_unit, file=trim(run%onebody_file), status="old")
        e1int = 0.0_p

        do i=1,sys%orbs
            do j=1, i
                read(tmp_unit, *) hh, indx
                !if (dabs(hh) < 1.0e-20_dp) hh = 0.0_dp
                e1int(i, j) = hh
                e1int(j, i) = hh
            enddo
        enddo

        close(tmp_unit)

        ! <ij|1/r_12|ab>
        ! NOTE: integral file is in Chemist notation, hence a and j are swapped
        !if (.not. allocated(sys%ints%v_ab)) allocate(sys%ints%v_ab(

        allocate(e2int(sys%orbs, sys%orbs, sys%orbs, sys%orbs))
        e2int = 0.0_p
        open(tmp_unit, file=trim(run%twobody_file), status="old")
        do
            read(tmp_unit, *, iostat=ios) i, a, j, b, hh
            !if (dabs(hh) < 1.0e-20_dp) hh = 0.0_dp
            if (i+a+j+b == 0) then
                sys%en_repul = hh
                exit
            endif

            if (ios /= 0) exit

            e2int(i, j, a, b) = hh
        enddo
        close(tmp_unit)

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, orbs=>sys%orbs)

            if (.not. allocated(sys%ints%f_a)) &
                allocate(sys%ints%f_a(orbs, orbs))

            if (.not. allocated(sys%ints%f_b)) &
                allocate(sys%ints%f_b(orbs, orbs))

            if (.not. allocated(sys%ints%v_aa)) &
                allocate(sys%ints%v_aa(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs))

            if (.not. allocated(sys%ints%v_ab)) &
                allocate(sys%ints%v_ab(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs))

            if (.not. allocated(sys%ints%v_bb)) &
                allocate(sys%ints%v_bb(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs))

            ! Antisymmetrize two-body matrix
            do i=froz+1,orbs
                do j=froz+1,orbs
                    do a=froz+1,orbs
                        do b=froz+1,orbs
                            sys%ints%v_ab(i,j,a,b) = e2int(i,j,a,b)
                            sys%ints%v_aa(i,j,a,b) = e2int(i,j,a,b) - e2int(i,j,b,a)
                            sys%ints%v_bb(i,j,a,b) = e2int(i,j,a,b) - e2int(i,j,b,a)
                        enddo
                    enddo
                enddo
            enddo

            sys%ints%f_a = e1int
            sys%ints%f_b = e1int

            ! Generate fock matrix
            call calculate_fock(sys, e1int, e2int)

            deallocate(e1int)
            deallocate(e2int)

        end associate

        sys%en_ref = sys%en_ref + sys%en_repul


    end subroutine load_ints

    subroutine calculate_fock(sys, e1int, e2int)

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(inout) :: sys
        real(p), allocatable, intent(in) :: e1int(:,:)
        real(p), allocatable, intent(in) :: e2int(:,:,:,:)
        real(p) :: eref

        integer :: i, j, k, l

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, orbs=>sys%orbs)
            ! Calculate reference energy
            eref=0.0_p
            do i=1,occ_a
                eref = eref + e1int(i,i)
            enddo
            do i=1,occ_b
                eref = eref + e1int(i,i)
            enddo

            do i=1,occ_a
                do j=1,occ_b
                    eref = eref + e2int(i,j,i,j)
                enddo
            enddo

            do i=1,occ_a
                do j=1,occ_a
                    eref = eref + 0.5_p * (e2int(i,j,i,j)-e2int(i,j,j,i))
                enddo
            enddo

            do i=1,occ_b
                do j=1,occ_b
                    eref = eref + 0.5_p * (e2int(i,j,i,j)-e2int(i,j,j,i))
                enddo
            enddo
            sys%en_ref = eref

            ! Calculate Fock Operators
            do i=1,orbs
                do j=1,i
                    do k=1,occ_b
                        sys%ints%f_a(i,j) = sys%ints%f_a(i,j) + e2int(i,k,j,k)
                    enddo
                    do k=1,occ_a
                        sys%ints%f_a(i,j) = sys%ints%f_a(i,j) + (e2int(i,k,j,k) - e2int(i,k,k,j))
                    enddo
                    sys%ints%f_a(j,i) = sys%ints%f_a(i,j)

                    do k=1,occ_a
                        sys%ints%f_b(i,j) = sys%ints%f_b(i,j) + e2int(i,k,j,k)
                    enddo
                    do k=1,occ_b
                        sys%ints%f_b(i,j) = sys%ints%f_b(i,j) + (e2int(i,k,j,k) - e2int(i,k,k,j))
                    enddo
                    sys%ints%f_b(j,i) = sys%ints%f_b(i,j)
                enddo
            enddo

        end associate

    end subroutine calculate_fock

    subroutine unload_ints(sys)

        use system, only: sys_t

        type(sys_t), intent(inout) :: sys

        if (allocated(sys%ints%f_a)) deallocate(sys%ints%f_a)
        if (allocated(sys%ints%f_b)) deallocate(sys%ints%f_b)

        if (allocated(sys%ints%v_aa)) deallocate(sys%ints%v_aa)
        if (allocated(sys%ints%v_ab)) deallocate(sys%ints%v_ab)
        if (allocated(sys%ints%v_bb)) deallocate(sys%ints%v_bb)

    end subroutine unload_ints

    ! Sorted integral section

    subroutine load_sorted_ints(sys, run)

        ! This routine sorts integrals into unique cases.
        ! In:
        !    sys: system information
        !    run: runtime configurations
        ! Out:
        !    sorted integrals in sys%ints

        use const, only: p, dp, tmp_unit, part_ints_a_unit, part_ints_b_unit, part_ints_c_unit
        use system, only: sys_t, run_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run

        real(dp), allocatable :: e1int(:,:)
        real(dp), allocatable :: e2int(:,:,:,:)
        real(dp) :: hh

        ! Aux matrices used to generate all particle arrays
        real(p), allocatable :: vappp(:,:,:)
        real(p), allocatable :: vbppp(:,:,:)
        real(p), allocatable :: vcppp(:,:,:)

        integer :: ios
        integer :: indx
        integer :: i, j, a, b, c, d, l

        integer :: unocc_a, unocc_b

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, orbs=>sys%orbs, &
                f_a=>sys%ints%f_a, f_b=>sys%ints%f_b, v_aa=>sys%ints%v_aa, v_ab=>sys%ints%v_ab, &
                v_bb=>sys%ints%v_bb)

            ! Fock alpha
            allocate(sys%ints%fahh(froz+1:occ_a,froz+1:occ_a))
            allocate(sys%ints%fahp(occ_a+1:orbs,froz+1:occ_a))
            allocate(sys%ints%fapp(occ_a+1:orbs,occ_a+1:orbs))

            ! Fock beta
            allocate(sys%ints%fbhh(froz+1:occ_b,froz+1:occ_b))
            allocate(sys%ints%fbhp(occ_b+1:orbs,froz+1:occ_b))
            allocate(sys%ints%fbpp(occ_b+1:orbs,occ_b+1:orbs))

            allocate(sys%ints%vahhhh(froz+1:occ_a,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
            allocate(sys%ints%vahhhp(occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a,froz+1:occ_a))
            allocate(sys%ints%vahhpp(occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a,froz+1:occ_a))
            allocate(sys%ints%vahphp(occ_a+1:orbs,froz+1:occ_a,occ_a+1:orbs,froz+1:occ_a))
            allocate(sys%ints%vahppp(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,froz+1:occ_a))
            allocate(sys%ints%vbhhhh(froz+1:occ_b,froz+1:occ_a,froz+1:occ_b,froz+1:occ_a))
            allocate(sys%ints%vbhhhp(occ_b+1:orbs,froz+1:occ_a,froz+1:occ_b,froz+1:occ_a))
            allocate(sys%ints%vbhhph(froz+1:occ_b,occ_a+1:orbs,froz+1:occ_b,froz+1:occ_a))
            allocate(sys%ints%vbhhpp(occ_b+1:orbs,occ_a+1:orbs,froz+1:occ_b,froz+1:occ_a))
            allocate(sys%ints%vbhphp(occ_b+1:orbs,froz+1:occ_a,occ_b+1:orbs,froz+1:occ_a))
            allocate(sys%ints%vbhpph(froz+1:occ_b,occ_a+1:orbs,occ_b+1:orbs,froz+1:occ_a))
            allocate(sys%ints%vbphph(froz+1:occ_b,occ_a+1:orbs,froz+1:occ_b,occ_a+1:orbs))
            allocate(sys%ints%vbhppp(occ_b+1:orbs,occ_a+1:orbs,occ_b+1:orbs,froz+1:occ_a))
            allocate(sys%ints%vbphpp(occ_b+1:orbs,occ_a+1:orbs,froz+1:occ_b,occ_a+1:orbs))
            allocate(sys%ints%vchhhh(froz+1:occ_b,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
            allocate(sys%ints%vchhhp(occ_b+1:orbs,froz+1:occ_b,froz+1:occ_b,froz+1:occ_b))
            allocate(sys%ints%vchhpp(occ_b+1:orbs,occ_b+1:orbs,froz+1:occ_b,froz+1:occ_b))
            allocate(sys%ints%vchphp(occ_b+1:orbs,froz+1:occ_b,occ_b+1:orbs,froz+1:occ_b))
            allocate(sys%ints%vchppp(occ_b+1:orbs,occ_b+1:orbs,occ_b+1:orbs,froz+1:occ_b))

            ! active-space
            allocate(sys%ints%vaappp(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs,occ_a+1:sys%act_unocc_a))
            allocate(vappp(occ_a+1:orbs,occ_a+1:orbs,occ_a+1:orbs))

            allocate(sys%ints%vbappp(occ_b+1:orbs,occ_a+1:orbs,occ_b+1:orbs,occ_a+1:sys%act_unocc_a))
            allocate(sys%ints%vbpapp(occ_b+1:orbs,occ_a+1:orbs,occ_b+1:sys%act_unocc_a,occ_a+1:orbs))
            allocate(vbppp(occ_b+1:orbs,occ_a+1:orbs,occ_b+1:orbs))

            allocate(sys%ints%vcappp(occ_b+1:orbs,occ_b+1:orbs,occ_b+1:orbs,occ_b+1:sys%act_unocc_a))
            allocate(vcppp(occ_b+1:orbs,occ_b+1:orbs,occ_b+1:orbs))

            ! One body integrals
            sys%ints%fahh(froz+1:occ_a, froz+1:occ_a) = f_a(froz+1:occ_a, froz+1:occ_a)
            sys%ints%fahp(occ_a+1:orbs, froz+1:occ_a) = f_a(occ_a+1:orbs, froz+1:occ_a)
            sys%ints%fapp(occ_a+1:orbs, occ_a+1:orbs) = f_a(occ_a+1:orbs, occ_a+1:orbs)

            sys%ints%fbhh(froz+1:occ_b, froz+1:occ_b) = f_b(froz+1:occ_b, froz+1:occ_b)
            sys%ints%fbhp(occ_b+1:orbs, froz+1:occ_b) = f_b(occ_b+1:orbs, froz+1:occ_b)
            sys%ints%fbpp(occ_b+1:orbs, occ_b+1:orbs) = f_b(occ_b+1:orbs, occ_b+1:orbs)


            ! Alpha-alpha cases
            sys%ints%vahhhh(froz+1:occ_a, froz+1:occ_a, froz+1:occ_a, froz+1:occ_a) = &
                v_aa(froz+1:occ_a, froz+1:occ_a, froz+1:occ_a, froz+1:occ_a)

            sys%ints%vahhhp(occ_a+1:orbs, froz+1:occ_a, froz+1:occ_a, froz+1:occ_a) = &
                v_aa(occ_a+1:orbs, froz+1:occ_a, froz+1:occ_a, froz+1:occ_a)

            sys%ints%vahhpp(occ_a+1:orbs, occ_a+1:orbs, froz+1:occ_a, froz+1:occ_a) = &
                v_aa(occ_a+1:orbs, occ_a+1:orbs, froz+1:occ_a, froz+1:occ_a)

            sys%ints%vahphp(occ_a+1:orbs, froz+1:occ_a, occ_a+1:orbs, froz+1:occ_a) = &
                v_aa(occ_a+1:orbs, froz+1:occ_a, occ_a+1:orbs, froz+1:occ_a)

            sys%ints%vahppp(occ_a+1:orbs, occ_a+1:orbs, occ_a+1:orbs, froz+1:occ_a) = &
                v_aa(occ_a+1:orbs, occ_a+1:orbs, occ_a+1:orbs, froz+1:occ_a)

            ! Alpha-beta cases
            sys%ints%vbhhhh(froz+1:occ_b, froz+1:occ_a, froz+1:occ_b, froz+1:occ_a) = &
                v_ab(froz+1:occ_b, froz+1:occ_a, froz+1:occ_b, froz+1:occ_a)

            sys%ints%vbhhhp(occ_b+1:orbs, froz+1:occ_a, froz+1:occ_b, froz+1:occ_a) = &
                v_ab(occ_b+1:orbs, froz+1:occ_a, froz+1:occ_b, froz+1:occ_a)

            sys%ints%vbhhph(froz+1:occ_b, occ_a+1:orbs, froz+1:occ_b, froz+1:occ_a) = &
                v_ab(froz+1:occ_b, occ_a+1:orbs,froz+1:occ_b, froz+1:occ_a)

            sys%ints%vbhhpp(occ_b+1:orbs, occ_a+1:orbs, froz+1:occ_b, froz+1:occ_a) = &
                v_ab(occ_b+1:orbs, occ_a+1:orbs, froz+1:occ_b, froz+1:occ_a)

            sys%ints%vbhphp(occ_b+1:orbs, froz+1:occ_a, occ_b+1:orbs, froz+1:occ_a) = &
                v_ab(occ_b+1:orbs, froz+1:occ_a, occ_b+1:orbs, froz+1:occ_a)

            sys%ints%vbhpph(froz+1:occ_b, occ_a+1:orbs, occ_b+1:orbs, froz+1:occ_a) = &
                v_ab(froz+1:occ_b, occ_a+1:orbs, occ_b+1:orbs, froz+1:occ_a)

            sys%ints%vbphph(froz+1:occ_b, occ_a+1:orbs, froz+1:occ_b, occ_a+1:orbs) = &
                v_ab(froz+1:occ_b, occ_a+1:orbs, froz+1:occ_b, occ_a+1:orbs)

            sys%ints%vbhppp(occ_b+1:orbs, occ_a+1:orbs, occ_b+1:orbs, froz+1:occ_a) = &
                v_ab(occ_b+1:orbs, occ_a+1:orbs, occ_b+1:orbs, froz+1:occ_a)

            sys%ints%vbphpp(occ_b+1:orbs, occ_a+1:orbs, froz+1:occ_b, occ_a+1:orbs) = &
                v_ab(occ_b+1:orbs, occ_a+1:orbs, froz+1:occ_b, occ_a+1:orbs)

            ! Beta-beta cases
            sys%ints%vchhhh(froz+1:occ_b, froz+1:occ_b, froz+1:occ_b, froz+1:occ_b) = &
                v_bb(froz+1:occ_b, froz+1:occ_b, froz+1:occ_b, froz+1:occ_b)

            sys%ints%vchhhp(occ_b+1:orbs, froz+1:occ_b, froz+1:occ_b, froz+1:occ_b) = &
                v_bb(occ_b+1:orbs, froz+1:occ_b, froz+1:occ_b, froz+1:occ_b)

            sys%ints%vchhpp(occ_b+1:orbs, occ_b+1:orbs, froz+1:occ_b, froz+1:occ_b) = &
                v_bb(occ_b+1:orbs, occ_b+1:orbs, froz+1:occ_b, froz+1:occ_b)

            sys%ints%vchphp(occ_b+1:orbs, froz+1:occ_b, occ_b+1:orbs, froz+1:occ_b) = &
                v_bb(occ_b+1:orbs, froz+1:occ_b, occ_b+1:orbs, froz+1:occ_b)

            sys%ints%vchppp(occ_b+1:orbs, occ_b+1:orbs, occ_b+1:orbs, froz+1:occ_b) = &
                v_bb(occ_b+1:orbs, occ_b+1:orbs, occ_b+1:orbs, froz+1:occ_b)

            unocc_a = orbs - occ_a
            unocc_b = orbs - occ_b

            open(part_ints_a_unit,file='part_ints_a.bin',form='unformatted', &
                recl=(unocc_a**3)*8, access='direct')
            open(part_ints_b_unit,file='part_ints_b.bin',form='unformatted', &
                recl=(unocc_b**2)*unocc_a*8, access='direct')
            open(part_ints_c_unit,file='part_ints_c.bin',form='unformatted', &
                recl=(unocc_b**3)*8, access='direct')

            !  VAPPPP
            do l=occ_a+1,orbs
                do b=occ_a+1,orbs
                    do c=occ_a+1,orbs
                        do d=occ_a+1,orbs
                            vappp(d,c,b) = v_aa(d,c,b,l)
                        enddo
                    enddo
                enddo

                if (l <= sys%act_unocc_a) then
                    sys%ints%vaappp(:,:,:,l) = vappp
                endif

                ! Write whole aa matrix to disk
                write(part_ints_a_unit,rec=l) vappp
            enddo
            deallocate(vappp)

            !  VBPPPP
            do l=occ_a+1,orbs
                do b=occ_b+1,orbs
                    do c=occ_a+1,orbs
                        do d=occ_b+1,orbs
                            vbppp(d,c,b)= v_ab(d,c,b,l)
                        enddo
                    enddo

                    !  VBPAPP
                    if (b <= sys%act_unocc_a) then
                        sys%ints%vbpapp(:,:,b,l) = vbppp(:,:,b)
                    endif
                enddo

                !  VBAPPP
                if (l <= sys%act_unocc_a) then
                    sys%ints%vbappp(:,:,:,l) = vbppp
                endif

                ! Write whole aa matrix to disk
                write(part_ints_b_unit,rec=l) vbppp
            enddo

            deallocate(vbppp)

            !  VCPPPP
            do l=occ_b+1,orbs
                do b=occ_b+1,orbs
                    do c=occ_b+1,orbs
                        do d=occ_b+1,orbs
                            vcppp(d,c,b) = v_bb(d,c,b,l)
                        enddo
                    enddo
                enddo

                if (l <= sys%act_unocc_a) then
                    sys%ints%vcappp(:,:,:,l) = vcppp
                endif

                ! Write whole bb matrix to disk
                write(part_ints_c_unit,rec=l) vcppp
            enddo
            deallocate(vcppp)
        end associate

    end subroutine load_sorted_ints

    subroutine unload_sorted_ints(sys)

        use const, only: dp, part_ints_a_unit, part_ints_b_unit, part_ints_c_unit
        use system, only: sys_t

        type(sys_t), intent(inout) :: sys

        if (allocated(sys%ints%fahh)) deallocate(sys%ints%fahh)
        if (allocated(sys%ints%fahp)) deallocate(sys%ints%fahp)
        if (allocated(sys%ints%fapp)) deallocate(sys%ints%fapp)
        if (allocated(sys%ints%fbhh)) deallocate(sys%ints%fbhh)
        if (allocated(sys%ints%fbhp)) deallocate(sys%ints%fbhp)
        if (allocated(sys%ints%fbpp)) deallocate(sys%ints%fbpp)
        if (allocated(sys%ints%vahhhh)) deallocate(sys%ints%vahhhh)
        if (allocated(sys%ints%vahhhp)) deallocate(sys%ints%vahhhp)
        if (allocated(sys%ints%vahhpp)) deallocate(sys%ints%vahhpp)
        if (allocated(sys%ints%vahphp)) deallocate(sys%ints%vahphp)
        if (allocated(sys%ints%vahppp)) deallocate(sys%ints%vahppp)
        if (allocated(sys%ints%vbhhhh)) deallocate(sys%ints%vbhhhh)
        if (allocated(sys%ints%vbhhhp)) deallocate(sys%ints%vbhhhp)
        if (allocated(sys%ints%vbhhph)) deallocate(sys%ints%vbhhph)
        if (allocated(sys%ints%vbhhpp)) deallocate(sys%ints%vbhhpp)
        if (allocated(sys%ints%vbhphp)) deallocate(sys%ints%vbhphp)
        if (allocated(sys%ints%vbhpph)) deallocate(sys%ints%vbhpph)
        if (allocated(sys%ints%vbphph)) deallocate(sys%ints%vbphph)
        if (allocated(sys%ints%vbhppp)) deallocate(sys%ints%vbhppp)
        if (allocated(sys%ints%vbphpp)) deallocate(sys%ints%vbphpp)
        if (allocated(sys%ints%vchhhh)) deallocate(sys%ints%vchhhh)
        if (allocated(sys%ints%vchhhp)) deallocate(sys%ints%vchhhp)
        if (allocated(sys%ints%vchhpp)) deallocate(sys%ints%vchhpp)
        if (allocated(sys%ints%vchphp)) deallocate(sys%ints%vchphp)
        if (allocated(sys%ints%vchppp)) deallocate(sys%ints%vchppp)

        close(part_ints_a_unit, status="delete")
        close(part_ints_b_unit, status="delete")
        close(part_ints_c_unit, status="delete")

    end subroutine unload_sorted_ints

end module integrals
