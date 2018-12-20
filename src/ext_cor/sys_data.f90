module sys_data

    ! Module for reading system data

    implicit none
    ! Occupation numbers
    integer :: froz, occ_a, occ_b, unocc, total
    integer :: nele
    ! Number of occupied/unoccupied orbitals
    integer :: nocc_a, nocc_b, nunocc_a, nunocc_b
    ! Size of the T cluster components
    integer :: i_t1a, i_t1b
    integer :: i_t2a, i_t2b, i_t2c
    integer :: i_t3a, i_t3b, i_t3c, i_t3d
    integer :: i_t4a, i_t4b, i_t4c, i_t4d, i_t4e
    integer :: ntotal
    ! TO BE FIXED!!!!
    integer :: nocc, nunocc, occ
    integer(kind=8) :: ist, ntotal_moe

    real(kind=8) :: corr_energy, ft1a_energy, ft1b_energy
    real(kind=8) :: vt2a_energy, vt2b_energy, vt2c_energy
    real(kind=8) :: vt1a2_energy, vt1b2_energy, vt1at1b_energy

    integer(kind=8) :: ci_size

contains

    subroutine read_singles(c1_a, c1_b)

        ! Read singly excited CI coefficients from ASCII files
        !
        ! In/Out:
        !   c1*: singles array

        real(kind=8), allocatable, intent(inout) :: c1_a(:,:)
        real(kind=8), allocatable, intent(inout) :: c1_b(:,:)

        ! Dummy indices
        integer :: a
        integer :: i

        real(kind=8) :: amp

        ! Aux vars
        integer :: ios

        open(101,file='c1_a.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,i
            if (ios /= 0) exit
            c1_a(a,i) = amp
        enddo

        open(101,file='c1_b.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,i
            if (ios /= 0) exit
            c1_b(a,i) = amp
        enddo

    end subroutine read_singles

    subroutine read_doubles(c2_aa, c2_ab, c2_bb)

        ! Read doubly excited CI coefficients from ASCII files
        !
        ! In/Out:
        !   c2*: doubles array

        real(kind=8), allocatable, intent(inout) :: c2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c2_bb(:,:,:,:)

        ! Dummy indices
        integer :: a, b
        integer :: i, j

        real(kind=8) :: amp

        ! Aux vars
        integer :: ios

        open(101,file='c2_aa.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,i,j
            if (ios /= 0) exit
            c2_aa(a,b,i,j) = amp
        enddo

        open(101,file='c2_ab.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,i,j
            if (ios /= 0) exit
            c2_ab(a,b,i,j) = amp
        enddo

        open(101,file='c2_bb.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,i,j
            if (ios /= 0) exit
            c2_bb(a,b,i,j) = amp
        enddo

    end subroutine read_doubles

    subroutine read_triples(c3_aaa, c3_aab, c3_abb, c3_bbb)

        ! Read triply excited CI coefficients from ASCII files
        !
        ! In/Out:
        !   c3*: triples arrays

        real(kind=8), allocatable, intent(inout) :: c3_aaa(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c3_aab(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c3_abb(:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c3_bbb(:,:,:,:,:,:)

        ! Dummy indices
        integer :: a, b, c
        integer :: i, j, k

        real(kind=8) :: amp

        ! Aux vars
        integer :: ios


        ! Load full triples A
        open(101,file='c3_aaa.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,i,j,k
            if (ios /= 0) exit
            c3_aaa(a,b,c,i,j,k) = amp
        enddo
        close(101)

        open(101,file='c3_aab.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,i,j,k
            if (ios /= 0) exit
            c3_aab(a,b,c,i,j,k) = amp
        enddo
        close(101)

        open(101,file='c3_abb.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,i,j,k
            if (ios /= 0) exit
            c3_abb(a,b,c,i,j,k) = amp
        enddo
        close(101)

        open(101,file='c3_bbb.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,i,j,k
            if (ios /= 0) exit
            c3_bbb(a,b,c,i,j,k) = amp
        enddo
        close(101)

    end subroutine read_triples

    subroutine read_quadruples(c4_aaaa, c4_aaab, c4_aabb, c4_abbb, c4_bbbb)

        ! Read quadruply excited CI coefficients from ASCII files
        !
        ! In/Out:
        !   c4*: quadruples arrays

        real(kind=8), allocatable, intent(inout) :: c4_aaaa(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c4_aaab(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c4_aabb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c4_abbb(:,:,:,:,:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: c4_bbbb(:,:,:,:,:,:,:,:)

        ! Dummy indices
        integer :: a, b, c, d
        integer :: i, j, k, l

        real(kind=8) :: amp

        ! Aux vars
        integer :: ios
        integer :: i_vec

        ! Load full triples A
        open(101,file='c4_aaaa.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,d,i,j,k,l
            if (ios /= 0) exit
            c4_aaaa(a,b,c,d,i,j,k,l) = amp
        enddo
        close(101)

        open(101,file='c4_aaab.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,d,i,j,k,l
            if (ios /= 0) exit
            c4_aaab(a,b,c,d,i,j,k,l) = amp
        enddo
        close(101)

        open(101,file='c4_aabb.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,d,i,j,k,l
            if (ios /= 0) exit
            c4_aabb(a,b,c,d,i,j,k,l) = amp
        enddo
        close(101)

        open(101,file='c4_abbb.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,d,i,j,k,l
            if (ios /= 0) exit
            c4_abbb(a,b,c,d,i,j,k,l) = amp
        enddo
        close(101)

        open(101,file='c4_bbbb.txt',status='old')
        do
            read(101,*,iostat=ios) amp, a,b,c,d,i,j,k,l
            if (ios /= 0) exit
            c4_bbbb(a,b,c,d,i,j,k,l) = amp
        enddo
        close(101)

    end subroutine read_quadruples

    subroutine read_singles_doubles_moe(filename, t1_a, t1_b, &
            t2_aa, t2_ab, t2_bb)

        ! Read CC singles and doubles from a previous calculation
        !
        ! In:
        !   filename: binary file containing the T vector
        ! In/Out:
        !   t1*: singles amplitude array
        !   t2*: doubles amplitude array

        character(len=250), intent(in) :: filename

        ! Singles and doubles
        real(kind=8), allocatable, intent(inout) :: t1_a(:,:)
        real(kind=8), allocatable, intent(inout) :: t1_b(:,:)

        real(kind=8), allocatable, intent(inout) :: t2_aa(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: t2_ab(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: t2_bb(:,:,:,:)

        real(kind=8), allocatable :: t3(:)

        integer :: i, j, k, l
        integer :: a, b, c, d

        integer :: indx

        allocate(t3(ntotal))
        t3 = 0.0d0

        open(unit=105,file=trim(filename), status='unknown',form='unformatted')
        read(105) ist, corr_energy, ft1a_energy, ft1b_energy, vt2a_energy, vt2b_energy, &
            vt2c_energy, vt1a2_energy, vt1b2_energy, vt1at1b_energy, ntotal_moe

        read(105) t3

        close(105)

        indx = 1
        ! T1A
        do i=froz+1,occ_a
            do a=occ_a+1,total
                t1_a(a,i) = t3(indx)
                indx = indx + 1
            enddo
        enddo

        ! T1A
        do i=froz+1,occ_b
            do a=occ_b+1,total
                t1_b(a,i) = t3(indx)
                indx = indx + 1
            enddo
        enddo

        ! T2A
        do i=froz+1,occ_a
            do j=froz+1,occ_a
                do a=occ_a+1,total
                    do b=occ_a+1,total
                        t2_aa(a,b,i,j) = t3(indx)
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
                        t2_ab(a,b,i,j) = t3(indx)
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
                        t2_bb(a,b,i,j) = t3(indx)
                        indx = indx + 1
                    enddo
                enddo
            enddo
        enddo

        deallocate(t3)

    end subroutine read_singles_doubles_moe


    subroutine read_singles_sign(t1_a, t1_b)

        ! This routine change the sign of the T1 component
        ! to match the ones coming from other integral files
        !
        ! In/Out:
        !   t1*: singles amplitude array

        integer, allocatable, intent(inout) :: t1_a(:,:)
        integer, allocatable, intent(inout) :: t1_b(:,:)

        ! Dummy indices
        integer :: a
        integer :: i
        integer :: s

        ! Aux vars
        integer :: ios

        open(101,file='t1_a.txt',status='old')
        do
            read(101,*,iostat=ios) a, i, s
            if (ios /= 0) exit
            print *, s
            t1_a(a,i) = s
        enddo

        open(101,file='t1_b.txt',status='old')
        do
            read(101,*,iostat=ios) a, i, s
            if (ios /= 0) exit
            t1_b(a,i) = s
        enddo

    end subroutine read_singles_sign

    subroutine read_doubles_sign(t2_aa, t2_ab, t2_bb)

        ! This routine change the sign of the T2 component
        ! to match the ones coming from other integral files
        !
        ! In/Out:
        !   t2*: doubles amplitude array

        integer, allocatable, intent(inout) :: t2_aa(:,:,:,:)
        integer, allocatable, intent(inout) :: t2_ab(:,:,:,:)
        integer, allocatable, intent(inout) :: t2_bb(:,:,:,:)

        ! Dummy indices
        integer :: a, b
        integer :: i, j

        integer :: s

        ! Aux vars
        integer :: ios

        open(101,file='t2_aa.txt',status='old')
        do
            read(101,*,iostat=ios) a,b,i,j,s
            if (ios /= 0) exit
            t2_aa(a,b,i,j) = s
            t2_aa(b,a,i,j) = -s
            t2_aa(a,b,j,i) = -s
            t2_aa(b,a,j,i) = s
        enddo

        open(101,file='t2_ab.txt',status='old')
        do
            read(101,*,iostat=ios) a,b,i,j,s
            if (ios /= 0) exit
            t2_ab(a,b,i,j) = s
            t2_ab(b,a,i,j) = -s
            t2_ab(a,b,j,i) = -s
            t2_ab(b,a,j,i) = s
        enddo

        open(101,file='t2_bb.txt',status='old')
        do
            read(101,*,iostat=ios) a,b,i,j,s
            if (ios /= 0) exit
            t2_bb(a,b,i,j) = s
            t2_bb(b,a,i,j) = -s
            t2_bb(a,b,j,i) = -s
            t2_bb(b,a,j,i) = s
        enddo

    end subroutine read_doubles_sign

    subroutine read_aldet_ci_vec(filename, ci_vec, vec_num)

        ! Read GAMESS CI binary vector file
        !
        ! In:
        !   filename: CI binary file generated by GAMESS
        ! Out:
        !   ci_vec: array containing CI coefficients

        implicit none

        character(len=*), intent(in) :: filename
        integer, intent(in) :: vec_num

        integer :: i
        integer(kind=8) :: k
        integer(kind=8) :: ci_size

        real(kind=8), allocatable, intent(out) :: ci_vec(:)

        open(unit=101, file=trim(filename), access="sequential", form="unformatted")
        read(101) k, ci_size

        if (vec_num > k) then
            print '(a)', 'Error! Vector number not available'
            call exit(1)
        endif

        if (allocated(ci_vec)) then
            print '(a)', 'Error! CI vector array already allocated'
            call exit(1)
        endif

        allocate(ci_vec(ci_size))

        do i=1, vec_num
            read(101) ci_vec
        enddo
        close(101)

    end subroutine read_aldet_ci_vec

    subroutine read_integrals(fn_a, fn_b, vn_aa, vn_ab, vn_bb, e_repul)

        ! Read one- and two-body integrals from ASCII files
        !
        ! In/Out:
        !   fn_a: alpha fock matrix
        !   fn_b: beta fock matrix
        !   vn_aa: alpha-alpha two-body matrix
        !   vn_ab: alpha-beta two-body matrix
        !   vn_bb: beta-beta two-body matrix
        !   e_repul: inter-nuclear repulsion energy

        real(kind=8), allocatable, intent(inout) :: fn_a(:,:), fn_b(:,:)
        real(kind=8), allocatable, intent(inout) :: vn_aa(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: vn_ab(:,:,:,:)
        real(kind=8), allocatable, intent(inout) :: vn_bb(:,:,:,:)
        real(kind=8), intent(inout) :: e_repul

        real(kind=8), allocatable :: z(:,:)
        real(kind=8), allocatable :: v(:,:,:,:)
        integer :: i, j, a, b, x
        integer :: ios
        real(kind=8) :: val

        allocate(z(total,total))
        allocate(v(total,total,total,total))

        z = 0.0d0
        v = 0.0d0
        fn_a = 0.0d0
        fn_b = 0.0d0
        vn_aa = 0.0d0
        vn_ab = 0.0d0
        vn_bb = 0.0d0

        open(unit=106, file='onebody.inp', status='old')
        do i=1, total
            do j=1, i
                read(106,*) val, x
                z(i,j) = val
                z(j,i) = val
            enddo
        enddo

        close(106)

        open(unit=106, file='twobody.inp', status='old')
        do
            read(106,*,iostat=ios) i, a, j, b, val
            if (ios /= 0) exit

            if (i+j+a+b == 0) then
                e_repul = val
            else
                v(i,j,a,b) = val
            endif
        enddo

        close(106)

        do i=froz+1,total
            do j=froz+1,total
                fn_a(i,j) = z(i,j)
                fn_b(i,j) = z(i,j)

                do a=1,occ_a
                    fn_a(i,j) = fn_a(i,j) + v(i,a,j,a) - v(i,a,a,j)
                    fn_b(i,j) = fn_b(i,j) + v(i,a,j,a)
                enddo

                do a=1,occ_b
                    fn_b(i,j) = fn_b(i,j) + v(i,a,j,a) - v(i,a,a,j)
                    fn_a(i,j) = fn_a(i,j) + v(i,a,j,a)
                enddo

                fn_a(j,i) =fn_a(i,j)
                fn_b(j,i) =fn_b(i,j)
            enddo
        enddo



        vn_ab = v(froz+1:total,froz+1:total,froz+1:total,froz+1:total)

        do b=froz+1, total
            do a=froz+1, total
                do j=froz+1, total
                    do i=froz+1, total
                        vn_aa(i,j,a,b) = vn_ab(i,j,a,b) - vn_ab(i,j,b,a)
                        vn_bb(i,j,a,b) = vn_ab(i,j,a,b) - vn_ab(i,j,b,a)
                    enddo
                enddo
            enddo
        enddo

        deallocate(z, v)


    end subroutine read_integrals

end module sys_data
