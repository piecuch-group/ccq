module integrals

    ! This module is responsible for reading files, constructing arrays,
    ! and managing erverything related to molecular integrals coming from
    ! HF calculations.

    implicit none

contains

    subroutine load_e1int(orbs, filename, e1int)

        ! Load onebody molecular integrals

        ! In:
        !    orbs: number of spatial orbitals
        !    filename: path to the file containing the integrals

        ! In/Out:
        !    e1int: dim(2) array holding the integrals

        use const, only: p, tmp_unit

        integer, intent(in) :: orbs
        character(len=*), intent(in) :: filename
        real(p), allocatable, intent(inout) :: e1int(:,:)

        integer :: i, j, indx
        real(p) :: hh

        open(tmp_unit, file=trim(filename), status="old")

        do i=1,orbs
            do j=1, i
                read(tmp_unit, *) hh, indx
                e1int(i, j) = hh
                e1int(j, i) = hh
            enddo
        enddo

        close(tmp_unit)

    end subroutine load_e1int

    subroutine load_e2int(filename, e2int, en_repul)

        ! Load twobody molecular integrals

        ! In:
        !    filename: path to the file containing the integrals

        ! In/Out:
        !    e2int: dim(4) array holding the integrals
        !    en_repul: repulsing energy which is written in the
        !              last line of the twobody file


        use const, only: p, tmp_unit

        character(len=*), intent(in) :: filename
        real(p), allocatable, intent(inout) :: e2int(:,:,:,:)
        real(p), intent(out) :: en_repul

        integer :: i, j, a, b
        real(p) :: hh

        integer :: ios

        ! <ij|1/r_12|ab>
        ! NOTE: integral file is in Chemist notation, hence a and j are swapped.
        ! The Chemist notation is never used in coupled-cluster codes. Be ware!

        open(tmp_unit, file=trim(filename), status="old")
        do

            read(tmp_unit, *, iostat=ios) i, a, j, b, hh

            ! If the entry has no indices that means that it is the repulsion energy
            if (i+a+j+b == 0) then
                en_repul = hh
                exit
            endif

            if (ios /= 0) exit

            e2int(i, j, a, b) = hh

        enddo
        close(tmp_unit)

    end subroutine load_e2int

    subroutine load_fcidump(norbs, fcidump, e1int, e2int, hh)

        ! Load integrals from a FCIDUMP file as opposed to the
        ! onebody.inp and twobody.inp files from CC_PACKAGE

        ! In:
        !   fcidump: path to the FCIDUMP file

        ! In/Out:
        !   e1int: onebody integral array
        !   e2int: twobody integral array
        !   hh: internuclear repulsion energy

        use const, only: tmp_unit, p, line_len
        use ranking, only: insertion_rank

        integer, intent(in) :: norbs
        character(len=*), intent(in) :: fcidump
        real(p), allocatable, intent(in out) :: e1int(:,:)
        real(p), allocatable, intent(in out) :: e2int(:,:,:,:)
        real(p), intent(in out) :: hh

        real(p) :: eigenvalues(norbs)
        integer :: rank(norbs), inv_rank(norbs)

        character(len=line_len) :: cur_line
        integer :: idx
        integer :: ios
        logical :: read_flag = .false.

        integer :: i, j, a, b
        real(p) :: val


        open(tmp_unit, file=trim(fcidump), status="old")
        do

            ! Skip FCIDUMP header. The information contained in it has
            ! been already parsed in parser.f90
            if (.not. read_flag) then

                read(tmp_unit, *) cur_line
                idx = index(cur_line, "END")
                if (idx /= 0) read_flag = .true.

            else

                ! Read integrals
                read(tmp_unit, *, iostat=ios) val, i, a, j, b
                if (ios /= 0) exit

                if (i /= 0 .and. a + j + b == 0) eigenvalues(i) = val

            endif

        enddo

        rewind(tmp_unit)
        read_flag = .false.

        ! Get ranking and inverse ranking
        call insertion_rank(eigenvalues, rank)
        do idx=1, norbs
            inv_rank(rank(idx)) = idx
        enddo

        do

            ! Skip FCIDUMP header. The information contained in it has
            ! been already parsed in parser.f90
            if (.not. read_flag) then

                read(tmp_unit, *) cur_line
                idx = index(cur_line, "END")
                if (idx /= 0) read_flag = .true.

            else

                ! Read integrals
                read(tmp_unit, *, iostat=ios) val, i, a, j, b
                if (ios /= 0) exit


                if (i /= 0 .and. a /= 0 .and. j /= 0 .and. b /= 0) then
                    ! Twobody integrals
                    i = inv_rank(i)
                    a = inv_rank(a)
                    j = inv_rank(j)
                    b = inv_rank(b)
                    e2int(i, j, a, b) = val
                    e2int(j, i, b, a) = val
                    e2int(a, b, i, j) = val
                    e2int(b, a, j, i) = val
                    e2int(a, j, i, b) = val
                    e2int(b, i, j, a) = val
                    e2int(i, b, a, j) = val
                    e2int(j, a, b, i) = val

                else if (i /= 0 .and. a /= 0 .and. j + b == 0) then
                    ! Onebody integrals
                    i = inv_rank(i)
                    a = inv_rank(a)
                    e1int(i, a) = val
                    e1int(a, i) = val

                else if (i + a + j + b == 0) then
                    ! Repulsion energy
                    hh = val
                endif

            endif


        enddo

        close(tmp_unit)

    end subroutine load_fcidump

    subroutine load_ints(sys, run)

        ! Load one- and two-body integrals from a file/files.

        ! In:
        !    run: runtime configuration and data

        ! In/Out:
        !    sys: molecular system data. On return, the
        !         integrals will be loaded on sys%ints

        use const, only: p
        use system, only: sys_t, run_t

        use checking, only: check_allocate
        use errors, only: stop_all
        use utils, only: count_file_lines

        type(sys_t), intent(inout) :: sys
        type(run_t), intent(in) :: run

        integer :: i, j, a, b
        integer :: onebody_lines, orbs

        integer :: ierr


        ! Initialize onebody electronic integrals array
        allocate(sys%ints%e1int(sys%orbs, sys%orbs), stat=ierr)
        call check_allocate('sys%ints%e1int', sys%orbs ** 2, ierr)
        sys%ints%e1int = 0.0_p


        ! Initialize twobody electronic integrals array
        allocate(sys%ints%e2int(sys%orbs, sys%orbs, sys%orbs, sys%orbs), stat=ierr)
        call check_allocate('sys%ints%e2int', sys%orbs ** 4, ierr)
        sys%ints%e2int = 0.0_p


        if (trim(run%fcidump) /= '') then
            call load_fcidump(sys%orbs, run%fcidump, sys%ints%e1int, sys%ints%e2int, sys%en_repul)
        else
            onebody_lines = count_file_lines(run%onebody_file)
            orbs = int(-(1 - sqrt(real(1 + 8*onebody_lines))) / 2)
            if (int(orbs) /= sys%orbs) call stop_all('load_ints', "Number of orbitals doesn't match the number of integrals")
            call load_e1int(sys%orbs, run%onebody_file, sys%ints%e1int)

            call load_e2int(run%twobody_file, sys%ints%e2int, sys%en_repul)
        endif

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
                            sys%ints%v_ab(i,j,a,b) = sys%ints%e2int(i,j,a,b)
                            sys%ints%v_aa(i,j,a,b) = sys%ints%e2int(i,j,a,b) - sys%ints%e2int(i,j,b,a)
                            sys%ints%v_bb(i,j,a,b) = sys%ints%e2int(i,j,a,b) - sys%ints%e2int(i,j,b,a)
                        enddo
                    enddo
                enddo
            enddo

            ! Generate fock matrix
            call gen_fock_operator(sys, sys%ints%e1int, sys%ints%e2int)

        end associate

    end subroutine load_ints


    subroutine gen_fock_operator(sys, e1int, e2int)

        ! Generate the Fock operator out of onebody (Z) and
        ! twobody (V) integrals

        ! In:
        !   e1int: onebody integrals (Z)
        !   e2int: twobody integrals (V)

        ! In/Out:
        !   sys: molecular system data. On return, this parameter
        !        will be loaded with the Fock operator (F), such
        !        that F = Z + G

        use const, only: p
        use system, only: sys_t

        type(sys_t), intent(inout) :: sys
        real(p), allocatable, intent(in) :: e1int(:,:)
        real(p), allocatable, intent(in) :: e2int(:,:,:,:)

        integer :: i, j, k

        associate(froz=>sys%froz, occ_a=>sys%occ_a, occ_b=>sys%occ_b, orbs=>sys%orbs)

            ! Calculate Fock Operators
            ! F_N = <i|f|j> = <i|z|j> + <ik|v|jk>
            sys%ints%f_a = e1int
            sys%ints%f_b = e1int

            ! At this point f cointains only z
            do i=1,orbs
                do j=1,i

                    ! Split into spin cases

                    ! F_N(alpha) += <ik~|v|jk~>
                    do k=1,occ_b
                        sys%ints%f_a(i,j) = sys%ints%f_a(i,j) + e2int(i,k,j,k)
                    enddo

                    ! F_N(alpha) += <ik|v|jk>
                    do k=1,occ_a
                        sys%ints%f_a(i,j) = sys%ints%f_a(i,j) + (e2int(i,k,j,k) - e2int(i,k,k,j))
                    enddo

                    ! F_N(beta) += <i~k|v|j~k>
                    do k=1,occ_a
                        sys%ints%f_b(i,j) = sys%ints%f_b(i,j) + e2int(i,k,j,k)
                    enddo

                    ! F_N(beta) += <i~k~|v|j~k~>
                    do k=1,occ_b
                        sys%ints%f_b(i,j) = sys%ints%f_b(i,j) + (e2int(i,k,j,k) - e2int(i,k,k,j))
                    enddo

                    ! Copy the triangular matrices to the other side
                    sys%ints%f_a(j,i) = sys%ints%f_a(i,j)
                    sys%ints%f_b(j,i) = sys%ints%f_b(i,j)
                enddo
            enddo

        end associate

    end subroutine gen_fock_operator


    subroutine unload_ints(sys)

        ! Unload and deallocate all unsorted integrals

        ! In:
        !   sys: molecular system data


        use system, only: sys_t

        type(sys_t), intent(inout) :: sys

        if (allocated(sys%ints%f_a)) deallocate(sys%ints%f_a)
        if (allocated(sys%ints%f_b)) deallocate(sys%ints%f_b)

        if (allocated(sys%ints%v_aa)) deallocate(sys%ints%v_aa)
        if (allocated(sys%ints%v_ab)) deallocate(sys%ints%v_ab)
        if (allocated(sys%ints%v_bb)) deallocate(sys%ints%v_bb)

    end subroutine unload_ints

    !---- Sorted integral section

    subroutine load_sorted_ints(sys)

        ! This routine sorts integrals into unique cases.

        ! In:
        !    sys: system information

        ! Out:
        !    sorted integrals in sys%ints

        use const, only: p, tmp_unit, part_ints_a_unit, part_ints_b_unit, part_ints_c_unit
        use system, only: sys_t

        type(sys_t), intent(in out) :: sys

        ! Aux matrices used to generate all particle arrays
        real(p), allocatable :: vappp(:,:,:)
        real(p), allocatable :: vbppp(:,:,:)
        real(p), allocatable :: vcppp(:,:,:)

        integer :: b, c, d, l

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

            ! Integral sorting according to Jun Shen
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

            ! Active-space dependent integrals
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


            ! [TODO] maybe move this to a more adequate place (e.g. system data?)
            unocc_a = orbs - occ_a
            unocc_b = orbs - occ_b

            ! [TODO] URGENT: reformat these files. Record reading is not
            ! supported any more!
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

        ! Unload and deallocate all sorted integrals

        ! In:
        !   sys: molecular system data

        use const, only: part_ints_a_unit, part_ints_b_unit, part_ints_c_unit
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
