module hmatrix

    ! Produce the matrix representation of an operator with one- and two-body
    ! term in a given space.
    implicit none


contains

    subroutine update_proj_t2(nsocc, nsorb, ints, &
            nconf, twobody_occ, &
            right_num_conf, coef, &
            twobody_proj)

        use const, only: dp
        use determinant, only: decode_determinant
        use ext_cor_types, only: excit_t
        use system, only: ints_t

        integer, intent(in) :: nsocc, nsorb
        type(ints_t), intent(in) :: ints
        integer, intent(in) :: nconf
        integer, intent(in) :: twobody_occ(nsorb,nconf)
        integer, intent(in) :: right_num_conf(nsocc)
        real(dp), intent(in) :: coef
        real(dp), allocatable, intent(inout) :: twobody_proj(:)
        type(excit_t) :: excitation

        !  Local variables
        integer :: i,j,k
        integer :: idiff

        integer :: ierr

        integer, allocatable :: left(:), left_conf(:),isub(:),right(:)
        real(dp), allocatable :: v2eint(:)

        !integer :: nconf
        !type(dic_t), allocatable :: hmat_dic(:)
        !real(dp), allocatable :: hmat(:)
        integer :: cnt

        real(dp) :: x

        real(dp) :: element

        ! Read configurations
        allocate(right(nsorb))
        call num_conf_to_occ(right_num_conf, nsorb, nsocc, right)
        !call read_ints(nsorb, v2eint)
        !call get_confs_from_file(filename, conf, num_conf, nconf, nsorb, nsocc)

        allocate(left(nsorb),isub(nsorb))

        cnt = 1
        do i=1, nconf

            left = twobody_occ(:,i)
            !left_conf = twobody_conf(:,i)

            isub = left - right

            idiff=0
            do k=1, nsorb
                idiff = idiff + abs(isub(k))
            enddo

            idiff=idiff/2

            if (idiff == 2) then
                !call decode_determinant(1, left_conf, excitation)
                element = calc_2_diff(nsorb, left, right, ints)
                !element = calc_2_diff(nsorb, left, right, v2eint)
                !print '(i4,6i3,3f10.5,6i3)', i, left_conf, excitation%coef, element, coef, right_num_conf
                !print '(i4,2f18.10)', i, element, coef
                twobody_proj(i) = twobody_proj(i) + element * coef !* excitation%coef
                cnt = cnt + 1
            endif
        enddo

        cnt = cnt - 1


        deallocate(left,right,isub)

    end subroutine update_proj_t2

    subroutine num_confs_to_occs(num_conf, nconf, nsorb, nsocc, conf)
        integer, intent(in) :: num_conf(nsocc,nconf)
        integer, intent(in) :: nconf, nsorb, nsocc
        integer, allocatable, intent(inout) :: conf(:,:)

        integer :: i, j, indx


        allocate(conf(nsorb, nconf))

        do i=1, nconf
            call num_conf_to_occ(num_conf(:,i), nsorb, nsocc, conf(:,i))
        enddo

    end subroutine num_confs_to_occs

    subroutine num_conf_to_occ(num_conf, nsorb, nsocc, occ_conf)
        integer, intent(in) :: num_conf(nsocc)
        integer, intent(in) :: nsorb, nsocc
        integer, intent(inout) :: occ_conf(nsorb)
        integer :: i, indx

        occ_conf = 0
        do i=1, nsocc
            indx = num_conf(i)
            occ_conf(indx) = 1
        enddo

    end subroutine num_conf_to_occ

    subroutine get_confs_from_file(filename, conf, num_conf, nconf, nsorb, nsocc)
        character(len=*), intent(in) :: filename
        integer, allocatable, intent(inout) :: conf(:,:)
        integer, allocatable, intent(inout) :: num_conf(:,:)
        integer, intent(inout) :: nconf, nsorb, nsocc

        integer, allocatable :: tmp_det_sorb(:)
        integer, allocatable :: tmp_det_occ(:)
        integer, parameter :: cnf_unit = 147
        integer :: i, j, indx

        open(cnf_unit, file=trim(filename), status='old')

        read(cnf_unit, *) nconf, nsorb, nsocc

        allocate(tmp_det_occ(nsorb))
        allocate(tmp_det_sorb(nsocc))
        allocate(conf(nsorb, nconf))
        allocate(num_conf(nsocc, nconf))

        do i=1, nconf
            read(cnf_unit, *) tmp_det_sorb
            num_conf(:,i) = tmp_det_sorb

            tmp_det_occ = 0
            do j=1, nsocc
                indx = tmp_det_sorb(j)
                tmp_det_occ(indx) = 1
            enddo

            conf(:,i) = tmp_det_occ
            !print '(184i1)', tmp_det_occ

        enddo

        close(cnf_unit)

    end subroutine get_confs_from_file

    function calc_2_diff(nsorb, left, right, ints) result(element)
    !function calc_2_diff(nsorb, left, right, v) result(element)

        use const, only: dp
        use system, only: ints_t

        implicit none

        type(ints_t), intent(in) :: ints
        !real(dp), intent(in) :: v(:)
        integer, intent(in) :: nsorb
        integer, intent(in) :: left(nsorb), right(nsorb)

        integer :: k
        integer :: l1, l2
        integer :: ipl(2), ipr(2)
        integer :: isub(nsorb), itmp(nsorb)
        integer :: isgn, istep, isum

        real(dp) :: element

        element = 0.0_dp

        l1=1
        l2=1

        ! Find excitations
        itmp = right
        isub = left - right
        do k=1,nsorb

            ! From left
            if(isub(k).eq.1) then
                ipl(l1)=k
                l1=l1+1
            endif

            ! To right
            if(isub(k).eq.-1) then
                ipr(l2)=k
                l2=l2+1
            endif
        enddo

        ! Calculate determinant phase
        istep = (ipr(1)-ipl(1)) / (abs(ipr(1)-ipl(1)))
        isum=0

        do k=ipl(1),ipr(1),istep
            isum=isum+right(k)
        enddo

        isum=isum-1
        isgn=(-1)**isum

        itmp(ipr(1))=0
        itmp(ipl(1))=1

        istep = (ipr(2)-ipl(2)) / (abs(ipr(2)-ipl(2)))
        isum=0

        do k=ipl(2),ipr(2),istep
            isum=isum+itmp(k)
        enddo

        isum=isum-1
        isgn=isgn*(-1)**isum
        !isgn = 1

        ! Get matrix element
        !print '(4i4)', ipl, ipr
        element = spin(ipl(1),ipl(2),ipr(1),ipr(2),ints) * isgn
        !element = spin(ipl(1),ipl(2),ipr(1),ipr(2),v) * isgn

    end function calc_2_diff

    function spin(aa,bb,cc,dd,ints) result(ga)
    !function spin(aa,bb,cc,dd,v) result(ga)

        use const, only: dp
        use system, only: ints_t

        implicit none
        integer, intent(in) :: aa,bb,cc,dd
        type(ints_t), intent(in) :: ints
        integer :: dod
        integer :: a,b,c,d
        !real(dp), intent(in) :: v(:)
        real(dp) :: ga

        a=int((aa+1) / 2)
        b=int((bb+1) / 2)
        c=int((cc+1) / 2)
        d=int((dd+1) / 2)

        ! Total spin
        !print '(4i4)', aa,bb,cc,dd
        !print '(4i4)', a,b,c,d
        dod=mod(aa,2)+mod(bb,2)+mod(cc,2)+mod(dd,2)

        ! All spins are the same
        if (dod.eq.0.or.dod.eq.4) then
            ga=ints%v_aa(a,b,c,d)
            !ga=gammy(a,b,c,d,v)-gammy(a,b,d,c,v)

            ! All but one spin are the same
        else if (dod.eq.1.or.dod.eq.3) then
            ga=0.0_dp

            ! Bra and ket indices have wrong spins
        else if (mod(aa,2).eq.mod(bb,2)) then
            ga=0.0_dp

            ! Bra and ket indices coincide
        else if (mod(aa,2).eq.mod(cc,2)) then
            !ga=gammy(a,b,c,d,v)
            ga=ints%v_ab(a,b,c,d)

            ! Bra and ket indices are flipped
        else if (mod(aa,2).eq.mod(dd,2)) then
            !ga=-gammy(a,b,d,c,v)
            ga=-ints%v_ab(a,b,d,c)
            !ga=0.0_dp
        end if

    end function spin

    subroutine ext_cor_update_t2_cluster(sys, ext_cor)

        use const, only: dp
        use determinant, only: decode_determinant
        use ext_cor_types, only: excit_t
        use system, only: sys_t, ext_cor_t

        type(sys_t), intent(in) :: sys
        type(ext_cor_t), intent(inout) :: ext_cor
        type(excit_t) :: excitation
        integer :: nconf
        integer :: nocc_a, nocc_b, nunocc_a, nunocc_b

        integer :: i

        nconf = size(ext_cor%twobody_proj, 1)

        nocc_a = sys%occ_a - sys%froz
        nocc_b = sys%occ_b - sys%froz
        nunocc_a = sys%orbs - sys%occ_a
        nunocc_b = sys%orbs - sys%occ_b

        if (.not. allocated(ext_cor%t2a)) &
            allocate(ext_cor%t2a(nunocc_a, nunocc_a, nocc_a, nocc_a))
        ext_cor%t2a = 0.0_dp
        if (.not. allocated(ext_cor%t2b)) &
            allocate(ext_cor%t2b(nunocc_b, nunocc_a, nocc_b, nocc_a))
        ext_cor%t2b = 0.0_dp
        if (.not. allocated(ext_cor%t2c)) &
            allocate(ext_cor%t2c(nunocc_b, nunocc_b, nocc_b, nocc_b))
        ext_cor%t2c = 0.0_dp

        associate(excit_rank_a=>excitation%rank_a, e_sign=>excitation%coef, &
                to_a=>excitation%to_a, to_b=>excitation%to_b, &
                from_a=>excitation%from_a, from_b=>excitation%from_b)


            do i=1, nconf
                !print '(6i4)', ext_cor%twobody_confs(:,i)
                call decode_determinant(1, ext_cor%twobody_confs(:,i), excitation)
                to_a = to_a - sys%occ_a
                to_b = to_b - sys%occ_b
                !e_sign = 1.0_dp

                if (excit_rank_a == 2) then
                    !print '(i4,a,f14.8)', i, ' aa ', ext_cor%twobody_proj(i) * coef
                    !print '(i6,6i6)', i, ext_cor%twobody_confs(:,i)
                    ext_cor%t2a(to_a(2), to_a(1), from_a(2), from_a(1)) = &
                        ext_cor%twobody_proj(i) * e_sign
                else if (excit_rank_a == 1) then
                    ext_cor%t2b(to_b(1), to_a(1), from_b(1), from_a(1)) = &
                        ext_cor%twobody_proj(i) * e_sign
                else if (excit_rank_a == 0) then
                    ext_cor%t2c(to_b(2), to_b(1), from_b(2), from_b(1)) = &
                        ext_cor%twobody_proj(i) * e_sign
                endif

            enddo

        end associate

    end subroutine ext_cor_update_t2_cluster

    function ind(indx, indy) result(res)
        integer, intent(in) :: indx, indy
        integer :: res

        res = (indx * (indx - 1)) / 2 + indy
    end function ind

    function gammy(i,j,a,b,v) result(res)
        use const, only: dp
        implicit none
        integer :: nd
        integer :: i,j,a,b
        integer :: aa,bb,ii,jj,ia,jb
        integer :: iii, jjj
        integer :: indx
        real(dp), intent(in) :: v(:)
        real(dp) :: res

        ! <ij|1/r_12|ab>

        if (i < a) then
            ii = a
            aa = i
        else
            ii = i
            aa = a
        endif

        if (j < b) then
            jj = b
            bb = j
        else
            jj = j
            bb = b
        endif

        ia = ind(ii,aa)
        jb = ind(jj,bb)

        if (ia < jb) then
            indx = ind(jb,ia)
        else
            indx = ind(ia,jb)
        endif

        res = v(indx)
    end function gammy

    subroutine read_ints(nsorb, v2eint)

        use, intrinsic :: iso_fortran_env, only: error_unit
        use const, only: dp
        integer, intent(in) :: nsorb
        real(dp), allocatable, intent(inout) :: v2eint(:)
        integer :: n2eint


        integer :: norb
        integer :: ierr
        integer :: i, a, j, b
        integer :: ia, jb
        integer :: ii, jj, aa, bb
        integer :: iii, jjj
        integer :: indx

        logical :: exist_t


        real(dp) :: enuc, hh

        norb = nsorb / 2

        ! Allocate integral arrays. Note: this is only for the Hamiltonian matrix elements
        if (allocated(v2eint)) then
            write(error_unit, '(a)') 'Two-body array already allocated.'
        endif

        n2eint = (norb * (norb+1) * (norb * (norb+1) + 2)) / 8
        allocate(v2eint(n2eint))

        inquire(file='twobody.inp', exist=exist_t)

        if (.not. exist_t) then
            write(error_unit, '(a)') 'Ints file not found!'
            call exit(1)
        endif

        v2eint(:) = 0.0d0

        open(183, file='twobody.inp', iostat=ierr)

        do
            read(183,*,iostat=ierr) i,a,j,b, hh
            if (ierr /= 0) exit

            ! Repulsion energy
            if (i == 0 .and. a == 0) then
                enuc = hh

                ! <ij|1/r_12|ab>
                ! NOTE: integral file is in Chemist notation, hence
                ! the swapping of a and j
            elseif (j /= 0 .and. b /= 0) then
                if (i < a) then
                    ii = a
                    aa = i
                else
                    ii = i
                    aa = a
                endif

                if (j < b) then
                    jj = b
                    bb = j
                else
                    jj = j
                    bb = b
                endif

                ia = ind(ii,aa)
                jb = ind(jj,bb)

                if (ia < jb) then
                    indx = ind(jb,ia)
                else
                    indx = ind(ia,jb)
                endif

                v2eint(indx)=hh
            endif

        enddo

        close(183)

    end subroutine read_ints

end module hmatrix

