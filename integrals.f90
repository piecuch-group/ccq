module integrals

    implicit none


contains

    subroutine load_ints(sys, run)

        use const, only: dp, tmp_unit
        use system, only: sys_t, run_t

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run

        real(dp), allocatable :: e1int(:,:)
        real(dp), allocatable :: e2int(:,:,:,:)
        real(dp) :: hh

        integer :: ios
        integer :: indx
        integer :: i, j, a, b


        allocate(e1int(sys%orbs, sys%orbs))

        open(tmp_unit, file=trim(run%onebody_file), status="old")
        e1int = 0.0_dp

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
        e2int = 0.0_dp
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
            !sys%ints%v_ab = e2int(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs)

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
            !sys%ints%v_aa(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs) = &
            !    sys%ints%v_ab(froz+1:orbs, froz+1:orbs, froz+1:orbs, froz+1:orbs) - &
            !    reshape(sys%ints%v_ab,  (/orbs-froz, orbs-froz, orbs-froz, orbs-froz/), order=(/1,2,4,3/))
            !sys%ints%v_bb = sys%ints%v_aa

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

        use const, only: dp
        use system, only: sys_t

        type(sys_t), intent(inout) :: sys
        real(dp), allocatable, intent(in) :: e1int(:,:)
        real(dp), allocatable, intent(in) :: e2int(:,:,:,:)
        real(dp) :: eref

        integer :: i, j, k, l

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, orbs=>sys%orbs)
            ! Calculate reference energy
            eref=0.0_dp
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
                    eref = eref + 0.5_dp * (e2int(i,j,i,j)-e2int(i,j,j,i))
                enddo
            enddo

            do i=1,occ_b
                do j=1,occ_b
                    eref = eref + 0.5_dp * (e2int(i,j,i,j)-e2int(i,j,j,i))
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

        use const, only: dp
        use system, only: sys_t

        type(sys_t), intent(inout) :: sys

        if (allocated(sys%ints%f_a)) deallocate(sys%ints%f_a)
        if (allocated(sys%ints%f_b)) deallocate(sys%ints%f_b)

        if (allocated(sys%ints%v_aa)) deallocate(sys%ints%v_aa)
        if (allocated(sys%ints%v_ab)) deallocate(sys%ints%v_ab)
        if (allocated(sys%ints%v_bb)) deallocate(sys%ints%v_bb)

    end subroutine unload_ints

end module integrals
