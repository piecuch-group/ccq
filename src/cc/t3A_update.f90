subroutine t3a_update(n0, n1, n2, n3, k1, k2, k3, k4, lvl_q, shift, v3a &
                      , fockr, fockb, intr, intb, intm, t1a, t1b, t2a, t2b, t2c, t3a, t3b, t3c, t3d, &
                      iactocca, iactunoa, iactindt, &
                      t2diag3, t2diag4, t2diag5, t3diag1, t3diag2, t3diag3, t3diag4, t3diag5)

    integer :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p
    integer :: iactocca, iactunoa, iactindt
    integer :: iocca, iunoa
    real :: t2diag3, t2diag4, t2diag5
    real :: t3diag1, t3diag2, t3diag3, t3diag4, t3diag5
    real :: factor
    real(kind=8) :: shift, pp, coeleft, time1, time2
    integer, allocatable::indocc(:, :, :)
    integer, allocatable::indunocc(:, :, :)
    logical :: lvl_q
    real(kind=8) :: fockr(n3, n3)
    real(kind=8) :: fockb(n3, n3)
    real(kind=8) :: sum
    real(kind=8) :: intr(n0 + 1:n3, n0 + 1:n3, n0 + 1:n3, n0 + 1:n3)
    real(kind=8) :: intb(n0 + 1:n3, n0 + 1:n3, n0 + 1:n3, n0 + 1:n3)
    real(kind=8) :: intm(n0 + 1:n3, n0 + 1:n3, n0 + 1:n3, n0 + 1:n3)
    real(kind=8) :: t1a(n1 + 1:n3, n0 + 1:n1)
    real(kind=8) :: t1b(n2 + 1:n3, n0 + 1:n2)
    real(kind=8) :: t2a(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1)
    real(kind=8) :: t2b(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1)
    real(kind=8) :: t2c(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2)
    real(kind=8) :: t3a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1)
    real(kind=8) :: t3b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1)
    real(kind=8) :: t3c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1)
    real(kind=8) :: t3d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2)
    real(kind=8) :: v3a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1)

    real(kind=8), allocatable::t4a(:, :, :, :, :, :, :, :)                     !ilias: if no quadruples comment out the following 2 lines
    real(kind=8), allocatable::t4b(:, :, :, :, :, :, :, :)

    integer :: ta, tb, tc, td, te                                       !ilias: if no quadruples comment out the following 2 lines
    parameter(ta=29, tb=30, tc=31, td=32, te=33)

    integer :: i1, i2
    integer, allocatable::ind1(:, :, :, :)
    integer, allocatable::ind2(:, :, :, :)

    real(kind=8), allocatable::b1(:, :)
    real(kind=8), allocatable::b2(:, :)
    real(kind=8), allocatable::d1(:, :, :, :)
    real(kind=8), allocatable::d2(:, :, :, :)
    real(kind=8), allocatable::f2(:, :, :, :, :, :)
    real(kind=8), allocatable::h2(:, :, :, :, :, :, :, :)

    real(kind=8), allocatable::s1(:, :, :, :)
    real(kind=8), allocatable::s2(:, :, :, :)
    real(kind=8), allocatable::s3(:, :, :, :)
    real(kind=8), allocatable::s4(:, :, :, :)
    real(kind=8), allocatable::q1(:, :)
    real(kind=8), allocatable::q2(:, :)
    real(kind=8), allocatable::s5(:, :, :, :)
    real(kind=8), allocatable::s32(:, :, :, :)
    real(kind=8), allocatable::s6(:, :, :, :)
    real(kind=8), allocatable::s33(:, :, :, :)
    real(kind=8), allocatable::q3(:, :)
    real(kind=8), allocatable::s7(:, :, :, :)
    real(kind=8), allocatable::s35(:, :, :, :)
    real(kind=8), allocatable::s34(:, :, :, :)
    real(kind=8), allocatable::s8(:, :, :, :)
    real(kind=8), allocatable::q4(:, :)
    real(kind=8), allocatable::s9(:, :, :, :)
    real(kind=8), allocatable::s10(:, :, :, :)
    real(kind=8), allocatable::s11(:, :, :, :)
    real(kind=8), allocatable::s42(:, :, :, :)
    real(kind=8), allocatable::s41(:, :, :, :)
    real(kind=8), allocatable::s40(:, :, :, :)
    real(kind=8), allocatable::q15(:, :)
    real(kind=8), allocatable::s37(:, :, :, :)
    real(kind=8), allocatable::s51(:, :, :, :)
    real(kind=8), allocatable::s36(:, :, :, :)
    real(kind=8), allocatable::s50(:, :, :, :)
    real(kind=8), allocatable::s12(:, :, :, :)
    real(kind=8), allocatable::s45(:, :, :, :)
    real(kind=8), allocatable::s44(:, :, :, :)
    real(kind=8), allocatable::s43(:, :, :, :)
    real(kind=8), allocatable::q16(:, :)
    real(kind=8), allocatable::s38(:, :, :, :)
    real(kind=8), allocatable::q5(:, :)
    real(kind=8), allocatable::s46(:, :, :, :)
    real(kind=8), allocatable::s13(:, :, :, :)
    real(kind=8), allocatable::s47(:, :, :, :)
    real(kind=8), allocatable::q17(:, :)
    real(kind=8), allocatable::s39(:, :, :, :)
    real(kind=8), allocatable::s14(:, :, :, :)
    real(kind=8), allocatable::s48(:, :, :, :)
    real(kind=8), allocatable::q18(:, :)
    real(kind=8), allocatable::q6(:, :)
    real(kind=8), allocatable::q7(:, :)
    real(kind=8), allocatable::q8(:, :)
    real(kind=8), allocatable::q9(:, :)
    real(kind=8), allocatable::s49(:, :, :, :)
    real(kind=8), allocatable::q10(:, :)
    real(kind=8), allocatable::s15(:, :, :, :)
    real(kind=8), allocatable::s16(:, :, :, :)
    real(kind=8), allocatable::s17(:, :, :, :)
    real(kind=8), allocatable::s18(:, :, :, :)
    real(kind=8), allocatable::s19(:, :, :, :)
    real(kind=8), allocatable::s20(:, :, :, :)
    real(kind=8), allocatable::s21(:, :, :, :)
    real(kind=8), allocatable::s22(:, :, :, :)
    real(kind=8), allocatable::s23(:, :, :, :)
    real(kind=8), allocatable::s24(:, :, :, :)
    real(kind=8), allocatable::s25(:, :, :, :)
    real(kind=8), allocatable::q11(:, :)
    real(kind=8), allocatable::s26(:, :, :, :)
    real(kind=8), allocatable::q12(:, :)
    real(kind=8), allocatable::s27(:, :, :, :)
    real(kind=8), allocatable::s28(:, :, :, :)
    real(kind=8), allocatable::s29(:, :, :, :)
    real(kind=8), allocatable::s30(:, :, :, :)
    real(kind=8), allocatable::q13(:, :)
    real(kind=8), allocatable::q14(:, :)
    real(kind=8), allocatable::s31(:, :, :, :)
    real(kind=8), allocatable::x1(:, :, :, :)
    real(kind=8), allocatable::z1(:, :, :, :, :, :)
    real(kind=8), allocatable::x2(:, :, :, :)
    real(kind=8), allocatable::z2(:, :, :, :, :, :)
    real(kind=8), allocatable::x3(:, :)
    real(kind=8), allocatable::z3(:, :, :, :, :, :)
    real(kind=8), allocatable::x4(:, :)
    real(kind=8), allocatable::z4(:, :, :, :, :, :)
    real(kind=8), allocatable::x5(:, :, :, :)
    real(kind=8), allocatable::z5(:, :, :, :, :, :)
    real(kind=8), allocatable::x6(:, :, :, :)
    real(kind=8), allocatable::z6(:, :, :, :, :, :)
    real(kind=8), allocatable::x7(:, :, :, :)
    real(kind=8), allocatable::z7(:, :, :, :, :, :)
    real(kind=8), allocatable::x8(:, :, :, :)
    real(kind=8), allocatable::z8(:, :, :, :, :, :)
    real(kind=8), allocatable::x9(:, :)
    real(kind=8), allocatable::z9(:, :, :, :, :, :)
    real(kind=8), allocatable::x10(:, :, :, :)
    real(kind=8), allocatable::z10(:, :, :, :, :, :)
    real(kind=8), allocatable::x11(:, :, :, :)
    real(kind=8), allocatable::z11(:, :, :, :, :, :)
    real(kind=8), allocatable::x12(:, :)
    real(kind=8), allocatable::z12(:, :, :, :, :, :)
    real(kind=8), allocatable::x13(:, :, :, :)
    real(kind=8), allocatable::z13(:, :, :, :, :, :)
    real(kind=8), allocatable::x14(:, :, :, :)
    real(kind=8), allocatable::z14(:, :, :, :, :, :)
    real(kind=8), allocatable::x15(:, :, :, :)
    real(kind=8), allocatable::z16(:, :, :, :, :, :)
    real(kind=8), allocatable::x16(:, :, :, :)
    real(kind=8), allocatable::z17(:, :, :, :, :, :)
    real(kind=8), allocatable::z21(:, :, :, :, :, :)
    real(kind=8), allocatable::z25(:, :, :, :, :, :)
    real(kind=8), allocatable::z74(:, :, :, :, :, :)
    real(kind=8), allocatable::z73(:, :, :, :, :, :)
    real(kind=8), allocatable::z72(:, :, :, :, :, :)
    real(kind=8), allocatable::z76(:, :, :, :, :, :)
    real(kind=8), allocatable::z75(:, :, :, :, :, :)
    real(kind=8), allocatable::x17(:, :, :, :)
    real(kind=8), allocatable::z79(:, :, :, :, :, :)
    real(kind=8), allocatable::x18(:, :, :, :)
    real(kind=8), allocatable::z80(:, :, :, :, :, :)
    real(kind=8), allocatable::z40(:, :, :, :, :, :)
    real(kind=8), allocatable::z41(:, :, :, :, :, :)
    real(kind=8), allocatable::z42(:, :, :, :, :, :)
    real(kind=8), allocatable::x19(:, :, :, :)
    real(kind=8), allocatable::z46(:, :, :, :, :, :)
    real(kind=8), allocatable::x20(:, :, :, :)
    real(kind=8), allocatable::z48(:, :, :, :, :, :)

    allocate (indocc(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    allocate (indunocc(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    indocc = 0
    indunocc = 0
    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        iocca = 0
        if (i .gt. (n1 - iactocca)) iocca = iocca + 1
        if (j .gt. (n1 - iactocca)) iocca = iocca + 1
        if (k .gt. (n1 - iactocca)) iocca = iocca + 1
        if (iocca .lt. iactindt) indocc(k, j, i) = 1
    end do; end do; end do
    do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
        iunoa = 0
        if (a .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (b .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (c .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (iunoa .lt. iactindt) indunocc(c, b, a) = 1
    end do; end do; end do

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s1)
    deallocate (d1)
    deallocate (b2)

    allocate (x1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x1 = 0.0d0
    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s1)
    deallocate (s1)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s2)
    deallocate (d1)
    deallocate (b2)

    allocate (x15(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x15 = 0.0d0
    call sum_stripe(4, shape(x15), size(x15), '3124', 1.000, &
                    x15, s2)
    deallocate (s2)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2413', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s3(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s3)
    deallocate (d1)
    deallocate (b2)

    allocate (x16(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x16 = 0.0d0
    call sum_stripe(4, shape(x16), size(x16), '3124', 1.000, &
                    x16, s3)
    deallocate (s3)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '3421', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s4(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s4)
    deallocate (d1)
    deallocate (b2)

    allocate (x2(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x2 = 0.0d0
    call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                    s4)
    deallocate (s4)

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q1(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, b1, b2, q1)
    deallocate (b1)
    deallocate (b2)

    allocate (x3(n0 + 1:n1, n0 + 1:n1))
    x3 = 0.0d0
    call sum_stripe(2, shape(x3), size(x3), '21', 1.000, x3, &
                    q1)
    deallocate (q1)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n0 - n0, n1 - n0/), '21', fockr, b1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q2(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, b1, b2, q2)
    deallocate (b1)
    deallocate (b2)

    allocate (x4(n1 + 1:n3, n1 + 1:n3))
    x4 = 0.0d0
    call sum_stripe(2, shape(x4), size(x4), '21', -1.000, x4, &
                    q2)
    deallocate (q2)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s5(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s5)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,s5,t3a) &
        !$omp private(a,b,c,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (s5(j, n, m, i)*t3a(c, b, a, n, m, k) & !kji
                         - s5(k, n, m, i)*t3a(c, b, a, n, m, j) & !jki(-0.500)
                         - s5(i, n, m, j)*t3a(c, b, a, n, m, k) & !kij(-0.500)
                         + s5(i, n, m, k)*t3a(c, b, a, n, m, j) & !jik( 0.500)
                         + s5(k, n, m, j)*t3a(c, b, a, n, m, i) & !ikj( 0.500)
                         - s5(j, n, m, k)*t3a(c, b, a, n, m, i))/2.0d0!ijk(-0.500)
            end do; end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s5), size(s5), '3214', s5, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s32(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s32)
    deallocate (d1)
    deallocate (b2)
    deallocate (s5)

    call sum_stripe(4, shape(x15), size(x15), '2134', -1.000, &
                    x15, s32)
    deallocate (s32)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,x15) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x15(m, c, j, i)*t2a(b, a, m, k) & !bakcji( 1.000)
                      - x15(m, b, j, i)*t2a(c, a, m, k) & !cakbji(-1.000)
                      + x15(m, a, j, i)*t2a(c, b, m, k) & !cbkaji( 1.000)
                      - x15(m, c, k, i)*t2a(b, a, m, j) & !bajcki(-1.000)
                      + x15(m, b, k, i)*t2a(c, a, m, j) & !cajbki( 1.000)
                      - x15(m, a, k, i)*t2a(c, b, m, j) & !cbjaki(-1.000)
                      - x15(m, c, i, j)*t2a(b, a, m, k) & !bakcij(-1.000)
                      + x15(m, b, i, j)*t2a(c, a, m, k) & !cakbij( 1.000)
                      - x15(m, a, i, j)*t2a(c, b, m, k) & !cbkaij(-1.000)
                      + x15(m, c, i, k)*t2a(b, a, m, j) & !bajcik( 1.000)
                      - x15(m, b, i, k)*t2a(c, a, m, j) & !cajbik(-1.000)
                      + x15(m, a, i, k)*t2a(c, b, m, j) & !cbjaik( 1.000)
                      + x15(m, c, k, j)*t2a(b, a, m, i) & !baickj( 1.000)
                      - x15(m, b, k, j)*t2a(c, a, m, i) & !caibkj(-1.000)
                      + x15(m, a, k, j)*t2a(c, b, m, i) & !cbiakj( 1.000)
                      - x15(m, c, j, k)*t2a(b, a, m, i) & !baicjk(-1.000)
                      + x15(m, b, j, k)*t2a(c, a, m, i) & !caibjk( 1.000)
                      - x15(m, a, j, k)*t2a(c, b, m, i) !cbiajk(-1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1243', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s6(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s6)
    deallocate (d1)
    deallocate (b2)

    allocate (x6(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x6 = 0.0d0
    call sum_stripe(4, shape(x6), size(x6), '3124', -1.000, &
                    x6, s6)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s6), size(s6), '2314', s6, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s33(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s33)
    deallocate (d1)
    deallocate (b2)
    deallocate (s6)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s33)
    deallocate (s33)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1423', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q3(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q3)
    deallocate (d1)
    deallocate (b2)

    x3 = x3 - q3
    deallocate (q3)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s7(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s7)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '4123', 1.000, x6, &
                    s7)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s7), size(s7), '2341', s7, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s35(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s35)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x16), size(x16), '3124', 1.000, &
                    x16, s35)
    deallocate (s35)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,x16) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      - x16(e, c, b, i)*t2a(e, a, k, j) & !ecbieakj (-1.000)
                      + x16(e, c, a, i)*t2a(e, b, k, j) & !ecaiebkj (+1.000)
                      + x16(e, b, c, i)*t2a(e, a, k, j) & !ebcieakj (+1.000)
                      - x16(e, a, c, i)*t2a(e, b, k, j) & !eaciebkj (-1.000)
                      - x16(e, b, a, i)*t2a(e, c, k, j) & !ebaieckj (-1.000)
                      + x16(e, a, b, i)*t2a(e, c, k, j) & !eabieckj (+1.000)
                      + x16(e, c, b, j)*t2a(e, a, k, i) & !ecbjeaki (+1.000)
                      - x16(e, c, a, j)*t2a(e, b, k, i) & !ecajebki (-1.000)
                      - x16(e, b, c, j)*t2a(e, a, k, i) & !ebcjeaki (-1.000)
                      + x16(e, a, c, j)*t2a(e, b, k, i) & !eacjebki (+1.000)
                      + x16(e, b, a, j)*t2a(e, c, k, i) & !ebajecki (+1.000)
                      - x16(e, a, b, j)*t2a(e, c, k, i) & !eabjecki (-1.000)
                      - x16(e, c, b, k)*t2a(e, a, j, i) & !ecbkeaji (-1.000)
                      + x16(e, c, a, k)*t2a(e, b, j, i) & !ecakebji (+1.000)
                      + x16(e, b, c, k)*t2a(e, a, j, i) & !ebckeaji (+1.000)
                      - x16(e, a, c, k)*t2a(e, b, j, i) & !eackebji (-1.000)
                      - x16(e, b, a, k)*t2a(e, c, j, i) & !ebakecji (-1.000)
                      + x16(e, a, b, k)*t2a(e, c, j, i)     !eabkecji (+1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s7), size(s7), '3241', s7, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s34(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s34)
    deallocate (d1)
    deallocate (b2)
    deallocate (s7)

    call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                    s34)
    deallocate (s34)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2431', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s8(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s8)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,s8,t3a) &
        !$omp private(a,b,c,e,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do f = n1 + 1, n3
                sum = sum &
                      + (s8(b, f, e, c)*t3a(f, e, a, k, j, i) & !bfecfeakji (+0.500)
                         - s8(a, f, e, c)*t3a(f, e, b, k, j, i) & !afecfebkji (-0.500)
                         - s8(c, f, e, b)*t3a(f, e, a, k, j, i) & !cfebfeakji (-0.500)
                         + s8(c, f, e, a)*t3a(f, e, b, k, j, i) & !cfeafebkji (+0.500)
                         + s8(a, f, e, b)*t3a(f, e, c, k, j, i) & !afebfeckji (+0.500)
                         - s8(b, f, e, a)*t3a(f, e, c, k, j, i))/2.0d0!bfeafeckji (-0.500)
            end do; end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2341', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q4(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q4)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 - q4
    deallocate (q4)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s9(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s9)
    deallocate (d1)
    deallocate (b2)

    allocate (x8(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x8 = 0.0d0
    call sum_stripe(4, shape(x8), size(x8), '3124', -1.000, &
                    x8, s9)
    deallocate (s9)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n1 - n0/), '3241', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s10(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s10)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '4123', 1.000, x8, &
                    s10)
    deallocate (s10)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3214', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s11(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s11)
    deallocate (d1)
    deallocate (b2)

    allocate (x10(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    x10 = 0.0d0
    call sum_stripe(4, shape(x10), size(x10), '4123', 1.000, &
                    x10, s11)

    call sumx2143(n0, n3, n0, n1, n0, n1, n1, n3, n0, n1, x10, intr, 1.000)

    if (lvl_q) then
        allocate (t4a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        rewind (ta)
        read (ta) t4a

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1                     !ilias: if no quadruples comment out the following 14 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x10,t4a) &
        !$omp private(a,b,c,n,m,e,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n1
                    sum = sum & !top 2 switched
                          + (x10(n, m, e, j)*t4a(e, c, b, a, n, m, k, i) & !jcbaki(+0.500)
                             - x10(n, m, e, i)*t4a(e, c, b, a, n, m, k, j) & !icbakj(-0.500)
                             - x10(n, m, e, k)*t4a(e, c, b, a, n, m, j, i))/2.0d0!kcbaji(-0.500)
                end do; end do; end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (x10)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (t4a)
    end if

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s11), size(s11), '2431', s11, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s42(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s42)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,s42) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - s42(b, k, m, i)*t2a(c, a, m, j) & !bkicaj (-1.000)
                      - s42(c, j, m, i)*t2a(b, a, m, k) & !cjibak (-1.000)
                      + s42(c, k, m, i)*t2a(b, a, m, j) & !ckibaj (+1.000)
                      + s42(b, j, m, i)*t2a(c, a, m, k) & !bjicak (+1.000)
                      + s42(b, k, m, j)*t2a(c, a, m, i) & !bkjcai (+1.000)
                      + s42(c, i, m, j)*t2a(b, a, m, k) & !cijbak (+1.000)
                      - s42(c, k, m, j)*t2a(b, a, m, i) & !ckjbai (-1.000)
                      - s42(b, i, m, j)*t2a(c, a, m, k) & !bijcak (-1.000)
                      - s42(b, j, m, k)*t2a(c, a, m, i) & !bjkcai (-1.000)
                      - s42(c, i, m, k)*t2a(b, a, m, j) & !cikbaj (-1.000)
                      + s42(c, j, m, k)*t2a(b, a, m, i) & !cjkbai (+1.000)
                      + s42(b, i, m, k)*t2a(c, a, m, j)     !bikcaj (+1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s42)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s11), size(s11), '3421', s11, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s41(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s41)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,s41) &
        !$omp private(a,b,c,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      - s41(a, k, n, i)*t2a(c, b, n, j) & !akicbj (-1.000)
                      + s41(a, j, n, i)*t2a(c, b, n, k) & !ajicbk (+1.000)
                      + s41(a, k, n, j)*t2a(c, b, n, i) & !akjcbi (+1.000)
                      - s41(a, i, n, j)*t2a(c, b, n, k) & !aijcbk (-1.000)
                      - s41(a, j, n, k)*t2a(c, b, n, i) & !ajkcbi (-1.000)
                      + s41(a, i, n, k)*t2a(c, b, n, j)     !aikcbj (+1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s41)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s11), size(s11), '2341', s11, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
    allocate (s40(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k3*k3
    i3 = k1*k1
    call egemm(i1, i2, i3, d1, d2, s40)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,s40) &
        !$omp private(a,b,c,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum & !top 2 switched
                      + (s40(c, a, f, i)*t2a(f, b, k, j) & !caibkj (+0.500)
                         - s40(b, a, f, i)*t2a(f, c, k, j) & !baickj (-0.500)
                         - s40(c, b, f, i)*t2a(f, a, k, j) & !cbiakj (-0.500)
                         + s40(b, a, f, j)*t2a(f, c, k, i) & !bajcki (+0.500)
                         - s40(c, a, f, j)*t2a(f, b, k, i) & !cajbki (-0.500)
                         + s40(c, b, f, j)*t2a(f, a, k, i) & !cbjaki (+0.500)
                         - s40(b, a, f, k)*t2a(f, c, j, i) & !bakcji (-0.500)
                         + s40(c, a, f, k)*t2a(f, b, j, i) & !cakbji (+0.500)
                         - s40(c, b, f, k)*t2a(f, a, j, i))/2.0d0!cbkaji (-0.500)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s40)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s11), size(s11), '3421', s11, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q15(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q15)
    deallocate (d1)
    deallocate (b2)

    x3 = x3 - q15
    deallocate (q15)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s11), size(s11), '3241', s11, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s37(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s37)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '3124', -1.000, &
                    x6, s37)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s37), size(s37), '2314', s37, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s51(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s51)
    deallocate (d1)
    deallocate (b2)
    deallocate (s37)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s51)
    deallocate (s51)

    call sumx3241(n0, n3, n1, n3, n1, n3, n1, n3, n0, n1, x2, intr, 1.000)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x2,t2a) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      - x2(e, c, b, i)*t2a(e, a, k, j) & !cbiakj (-1.000)
                      + x2(e, c, a, i)*t2a(e, b, k, j) & !caibkj (+1.000)
                      - x2(e, b, a, i)*t2a(e, c, k, j) & !baickj (-1.000)
                      + x2(e, c, b, j)*t2a(e, a, k, i) & !cbjaki (+1.000)
                      - x2(e, c, a, j)*t2a(e, b, k, i) & !cajbki (-1.000)
                      + x2(e, b, a, j)*t2a(e, c, k, i) & !bajcki (+1.000)
                      - x2(e, c, b, k)*t2a(e, a, j, i) & !cbkaji (-1.000)
                      + x2(e, c, a, k)*t2a(e, b, j, i) & !cakbji (+1.000)
                      - x2(e, b, a, k)*t2a(e, c, j, i)     !bakcji (-1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x2)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s11), size(s11), '4231', s11, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s36(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s36)
    deallocate (d1)
    deallocate (b2)
    deallocate (s11)

    allocate (x5(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    x5 = 0.0d0
    call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                    s36)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s36), size(s36), '3214', s36, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s50(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s50)
    deallocate (d1)
    deallocate (b2)
    deallocate (s36)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s50)
    deallocate (s50)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '1243', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s12(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s12)
    deallocate (d1)
    deallocate (b2)

    allocate (x11(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    x11 = 0.0d0
    call sum_stripe(4, shape(x11), size(x11), '4123', -1.000, &
                    x11, s12)

    call sumx4132(n0, n3, n0, n1, n1, n3, n1, n3, n1, n3, x11, intr, 1.000)

    if (lvl_q) then
        allocate (t4a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        rewind (ta)
        read (ta) t4a

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1                     !ilias: if no quadruples comment out the following 14 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x11,t4a) &
        !$omp private(a,b,c,m,f,e,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n1 + 1, n3; do f = n1 + 1, n3; do m = n0 + 1, n1
                    sum = sum &
                          + (x11(m, f, e, c)*t4a(f, e, b, a, m, k, j, i) & !cbakji(+0.500)
                             - x11(m, f, e, b)*t4a(f, e, c, a, m, k, j, i) & !bcakji(-0.500)
                             + x11(m, f, e, a)*t4a(f, e, c, b, m, k, j, i))/2.0d0!acbkji(+0.500)
                end do; end do; end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (x11)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (t4a)
    end if

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(4, shape(s12), size(s12), '3421', s12, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s45(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i2 = k1*k1
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, d2, s45)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '3412', -0.500, &
                    x1, s45)
    deallocate (s45)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s12), size(s12), '2341', s12, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s44(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s44)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,s44) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + s44(b, k, e, c)*t2a(e, a, j, i) & !bkcaji (+1.000)
                      - s44(c, k, e, b)*t2a(e, a, j, i) & !ckbaji (-1.000)
                      + s44(c, k, e, a)*t2a(e, b, j, i) & !ckabji (+1.000)
                      - s44(b, j, e, c)*t2a(e, a, k, i) & !bjcaki (-1.000)
                      + s44(c, j, e, b)*t2a(e, a, k, i) & !cjbaki (+1.000)
                      - s44(c, j, e, a)*t2a(e, b, k, i) & !cjabki (-1.000)
                      + s44(b, i, e, c)*t2a(e, a, k, j) & !bicakj (+1.000)
                      - s44(c, i, e, b)*t2a(e, a, k, j) & !cibakj (-1.000)
                      + s44(c, i, e, a)*t2a(e, b, k, j)     !ciabkj (+1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s44)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s12), size(s12), '2431', s12, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s43(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s43)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,s43) &
        !$omp private(a,b,c,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum &
                      + s43(a, k, f, c)*t2a(f, b, j, i) & !akcbji    (+1.000)
                      - s43(a, k, f, b)*t2a(f, c, j, i) & !akbcji    (-1.000)
                      + s43(b, k, f, a)*t2a(f, c, j, i) & !bkacji    (+1.000)
                      - s43(a, j, f, c)*t2a(f, b, k, i) & !ajcbki    (-1.000)
                      + s43(a, j, f, b)*t2a(f, c, k, i) & !ajbcki    (+1.000)
                      - s43(b, j, f, a)*t2a(f, c, k, i) & !bjacki    (-1.000)
                      + s43(a, i, f, c)*t2a(f, b, k, j) & !aicbkj    (+1.000)
                      - s43(a, i, f, b)*t2a(f, c, k, j) & !aibckj    (-1.000)
                      + s43(b, i, f, a)*t2a(f, c, k, j)     !biackj    (+1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s43)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s12), size(s12), '2431', s12, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q16(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q16)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 + q16
    deallocate (q16)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s12), size(s12), '2341', s12, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s38(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s38)
    deallocate (d1)
    deallocate (b2)
    deallocate (s12)

    allocate (x7(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    x7 = 0.0d0
    call sum_stripe(4, shape(x7), size(x7), '3124', 1.000, x7, &
                    s38)
    deallocate (s38)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q5(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q5)
    deallocate (d1)
    deallocate (b2)

    allocate (x9(n0 + 1:n1, n1 + 1:n3))
    x9 = 0.0d0
    x9 = x9 + q5

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(q5), size(q5), '21', q5, b1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s46(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, b1, d2, s46)
    deallocate (b1)
    deallocate (d2)
    deallocate (q5)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s46)
    deallocate (s46)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3214', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s13(n0 + 1:n1, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
    i1 = k4*k1*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s13)
    deallocate (d1)
    deallocate (b2)

    allocate (x13(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    x13 = 0.0d0
    call sum_stripe(4, shape(x13), size(x13), '4123', 1.000, &
                    x13, s13)

    call sumx2143(n0, n3, n0, n2, n0, n1, n2, n3, n0, n1, x13, intm, 1.000)

    if (lvl_q) then
        allocate (t4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        rewind (tb)
        read (tb) t4b

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1                     !ilias: if no quadruples comment out the following 14 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x13,t4b) &
        !$omp private(a,b,c,n,m,e,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n2 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                    sum = sum &
                          - x13(n, m, e, i)*t4b(e, c, b, a, n, m, k, j) & !icbakj(-1.000)
                          + x13(n, m, e, j)*t4b(e, c, b, a, n, m, k, i) & !jcbaki(+1.000)
                          - x13(n, m, e, k)*t4b(e, c, b, a, n, m, j, i) !kcbaji(-1.000)
                end do; end do; end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4b)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x13)
    end if

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '2431', s13, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s47(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k3
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s47)
    deallocate (d1)
    deallocate (d2)

    allocate (x17(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x17 = 0.0d0
    call sum_stripe(4, shape(x17), size(x17), '2314', 1.000, &
                    x17, s47)
    deallocate (s47)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '2431', s13, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q17(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q17)
    deallocate (d1)
    deallocate (b2)

    x3 = x3 + q17
    deallocate (q17)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '3241', s13, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s39(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s39)
    deallocate (d1)
    deallocate (b2)
    deallocate (s13)

    call sum_stripe(4, shape(x8), size(x8), '3124', -1.000, &
                    x8, s39)
    deallocate (s39)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '1243', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s14(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s14)
    deallocate (d1)
    deallocate (b2)

    allocate (x14(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    x14 = 0.0d0
    call sum_stripe(4, shape(x14), size(x14), '4123', -1.000, &
                    x14, s14)

    call sumx4132(n0, n3, n0, n2, n2, n3, n1, n3, n1, n3, x14, intm, 1.000)

    if (lvl_q) then
        allocate (t4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        rewind (tb)
        read (tb) t4b

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1                     !ilias: if no quadruples comment out the following 14 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x14,t4b) &
        !$omp private(a,b,c,m,f,e,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n1 + 1, n3; do f = n2 + 1, n3; do m = n0 + 1, n2
                    sum = sum &
                          + x14(m, f, e, c)*t4b(f, e, b, a, m, k, j, i) & !cbakji(+1.000)
                          - x14(m, f, e, b)*t4b(f, e, c, a, m, k, j, i) & !bcakji(-1.000)
                          + x14(m, f, e, a)*t4b(f, e, c, b, m, k, j, i) !acbkji(+1.000)
                end do; end do; end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4b)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x14)
    end if

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s14), size(s14), '2341', s14, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s48(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k1*k3
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s48)
    deallocate (d1)
    deallocate (d2)

    allocate (x18(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x18 = 0.0d0
    call sum_stripe(4, shape(x18), size(x18), '3412', 1.000, &
                    x18, s48)
    deallocate (s48)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s14), size(s14), '2341', s14, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q18(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q18)
    deallocate (d1)
    deallocate (b2)
    deallocate (s14)

    x4 = x4 - q18
    deallocate (q18)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q6(n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q6)
    deallocate (d1)
    deallocate (b2)

    allocate (x12(n0 + 1:n2, n2 + 1:n3))
    x12 = 0.0d0
    x12 = x12 + q6
    deallocate (q6)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q7(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q7)
    deallocate (d1)
    deallocate (b2)

    x3 = x3 + q7
    deallocate (q7)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q8(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q8)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 + q8
    deallocate (q8)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q9(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q9)
    deallocate (d1)
    deallocate (b2)

    x9 = x9 + q9

    call sumx12(0, n3, n0, n1, n1, n3, x9, fockr, 1.000)

    if (lvl_q) then
        allocate (t4a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        rewind (ta)
        read (ta) t4a

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1                     !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t4a,x9) &
        !$omp private(a,b,c,m,e,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n1 + 1, n3; do m = n0 + 1, n1
                    sum = sum &
                          + x9(m, e)*t4a(e, c, b, a, m, k, j, i) !cbakji(+1.000)
                end do; end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4a)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x9)
    end if

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(q9), size(q9), '21', q9, b1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s49(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, b1, d2, s49)
    deallocate (b1)
    deallocate (d2)
    deallocate (q9)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s49)
    deallocate (s49)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q10(n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q10)
    deallocate (d1)
    deallocate (b2)

    x12 = x12 + q10
    deallocate (q10)

    call sumx12(0, n3, n0, n2, n2, n3, x12, fockb, 1.000)

    if (lvl_q) then
        allocate (t4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        rewind (tb)
        read (tb) t4b

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1                     !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x12,t4b) &
        !$omp private(a,b,c,m,e,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n2 + 1, n3; do m = n0 + 1, n2
                    sum = sum &
                          + x12(m, e)*t4b(e, c, b, a, m, k, j, i) !cbakji(+1.000)
                end do; end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (x12)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (t4b)
    end if

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s15(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, b1, d2, s15)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s15)
    deallocate (s15)

    if (t2diag5 .ne. 0) then
        allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2143', intr, d1)
        allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
        allocate (s16(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k3*k3
        i3 = k1*k1
        call egemm(i1, i2, i3, d1, d2, s16)
        deallocate (d1)
        deallocate (d2)

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,s16,t2diag5) &
        !$omp private(a,b,c,e,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n1 + 1, n3
                    sum = sum & !top 2 switched
                          + (s16(c, a, e, i)*t2a(e, b, k, j) & !caeiebkj (+0.500)
                             - s16(b, a, e, i)*t2a(e, c, k, j) & !baeieckj (-0.500)
                             - s16(c, b, e, i)*t2a(e, a, k, j) & !cbeieakj (-0.500)
                             + s16(b, a, e, j)*t2a(e, c, k, i) & !baejecki (+0.500)
                             - s16(c, a, e, j)*t2a(e, b, k, i) & !caejebki (-0.500)
                             + s16(c, b, e, j)*t2a(e, a, k, i) & !cbejeaki (+0.500)
                             - s16(b, a, e, k)*t2a(e, c, j, i) & !baekecji (-0.500)
                             + s16(c, a, e, k)*t2a(e, b, j, i) & !caekebji (+0.500)
                             - s16(c, b, e, k)*t2a(e, a, j, i))/2.0d0!cbekeaji (-0.500)
                end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + t2diag5*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s16)

    end if
    if (t2diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1423', intr, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
        allocate (s17(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        i1 = k1*k1
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s17)
        deallocate (d1)
        deallocate (d2)

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,s17,t2a,t2diag4) &
        !$omp private(a,b,c,n,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do n = n0 + 1, n1
                    sum = sum &
                          - s17(a, k, n, i)*t2a(c, b, n, j) & !akicbj (-1.000)
                          + s17(b, k, n, i)*t2a(c, a, n, j) & !bkicaj (+1.000)
                          + s17(c, j, n, i)*t2a(b, a, n, k) & !cjibak (+1.000)
                          - s17(c, k, n, i)*t2a(b, a, n, j) & !ckibaj (-1.000)
                          - s17(b, j, n, i)*t2a(c, a, n, k) & !bjicak (-1.000)
                          + s17(a, j, n, i)*t2a(c, b, n, k) & !ajicbk (+1.000)
                          + s17(a, k, n, j)*t2a(c, b, n, i) & !akjcbi (+1.000)
                          - s17(b, k, n, j)*t2a(c, a, n, i) & !bkjcai (-1.000)
                          - s17(c, i, n, j)*t2a(b, a, n, k) & !cijbak (-1.000)
                          + s17(c, k, n, j)*t2a(b, a, n, i) & !ckjbai (+1.000)
                          + s17(b, i, n, j)*t2a(c, a, n, k) & !bijcak (+1.000)
                          - s17(a, i, n, j)*t2a(c, b, n, k) & !aijcbk (-1.000)
                          - s17(a, j, n, k)*t2a(c, b, n, i) & !ajkcbi (-1.000)
                          + s17(b, j, n, k)*t2a(c, a, n, i) & !bjkcai (+1.000)
                          + s17(c, i, n, k)*t2a(b, a, n, j) & !cikbaj (+1.000)
                          - s17(c, j, n, k)*t2a(b, a, n, i) & !cjkbai (-1.000)
                          - s17(b, i, n, k)*t2a(c, a, n, j) & !bikcaj (-1.000)
                          + s17(a, i, n, k)*t2a(c, b, n, j)     !aikcbj (+1.000)
                end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + t2diag4*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s17)

    end if
    if (t2diag3 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))                 !ilias: commented out 13 lines for 3cc
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2341', intr, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
        allocate (s18(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        i1 = k3*k3
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s18)
        deallocate (d1)
        deallocate (d2)

        do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1                      !ilias: commented out 29 lines for 3cc
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,s18,t2diag3) &
        !$omp private(a,b,c,f,sum)
        !$omp do
            do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do f = n1 + 1, n3
                    sum = sum &
                          - s18(a, k, f, c)*t2a(f, b, j, i) & !akcbji (-1.000)
                          + s18(b, k, f, c)*t2a(f, a, j, i) & !bkcaji (+1.000)
                          + s18(a, k, f, b)*t2a(f, c, j, i) & !akbcji (+1.000)
                          - s18(b, k, f, a)*t2a(f, c, j, i) & !bkacji (-1.000)
                          - s18(c, k, f, b)*t2a(f, a, j, i) & !ckbaji (-1.000)
                          + s18(c, k, f, a)*t2a(f, b, j, i) & !ckabji (+1.000)
                          + s18(a, j, f, c)*t2a(f, b, k, i) & !ajcbki (+1.000)
                          - s18(b, j, f, c)*t2a(f, a, k, i) & !bjcaki (-1.000)
                          - s18(a, j, f, b)*t2a(f, c, k, i) & !ajbcki (-1.000)
                          + s18(b, j, f, a)*t2a(f, c, k, i) & !bjacki (+1.000)
                          + s18(c, j, f, b)*t2a(f, a, k, i) & !cjbaki (+1.000)
                          - s18(c, j, f, a)*t2a(f, b, k, i) & !cjabki (-1.000)
                          + s18(b, i, f, c)*t2a(f, a, k, j) & !bicakj (+1.000)
                          - s18(a, i, f, c)*t2a(f, b, k, j) & !aicbkj (-1.000)
                          - s18(c, i, f, b)*t2a(f, a, k, j) & !cibakj (-1.000)
                          + s18(c, i, f, a)*t2a(f, b, k, j) & !ciabkj (+1.000)
                          + s18(a, i, f, b)*t2a(f, c, k, j) & !aibckj (+1.000)
                          - s18(b, i, f, a)*t2a(f, c, k, j)     !biackj (-1.000)
                end do
                v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + t2diag3*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s18)                                              !ilias: commented out this line for 3cc

    end if
    if (t2diag5 .ne. 0) then
        allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n1 - n0, n1 - n0, n0 - n0, n1 - n0/), '4321', intr, d1)
        allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
        allocate (s19(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k1
        i3 = k3*k3
        call egemm(i1, i2, i3, d1, d2, s19)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*t2diag5
        call sum_stripe(4, shape(x1), size(x1), '3412', factor, &
                        x1, s19)
        deallocate (s19)

    end if
    call sumx2143(n0, n3, n0, n1, n1, n3, n0, n1, n0, n1, x1, intr, 1.000)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x1,t2a,t2diag3) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x1(m, c, j, i)*t2a(b, a, m, k) & !cjibak (+1.000)
                      - x1(m, b, j, i)*t2a(c, a, m, k) & !bjicak (-1.000)
                      + x1(m, a, j, i)*t2a(c, b, m, k) & !ajicbk (+1.000)
                      - x1(m, c, k, i)*t2a(b, a, m, j) & !ckibaj (-1.000)
                      + x1(m, b, k, i)*t2a(c, a, m, j) & !bkicaj (+1.000)
                      - x1(m, a, k, i)*t2a(c, b, m, j) & !akicbj (-1.000)
                      + x1(m, c, k, j)*t2a(b, a, m, i) & !ckjbai (+1.000)
                      - x1(m, b, k, j)*t2a(c, a, m, i) & !bkjcai (-1.000)
                      + x1(m, a, k, j)*t2a(c, b, m, i)     !akjcbi (+1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x1)

    if (t2diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
        allocate (s20(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        i1 = k1*k1
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s20)
        deallocate (d1)
        deallocate (d2)

        factor = t2diag4
        call sum_stripe(4, shape(x17), size(x17), '2314', factor, &
                        x17, s20)
        deallocate (s20)

    end if
    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x17,t2a,t2diag3) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x17(m, a, k, i)*t2a(c, b, m, j) & !akicbj (+1.000)
                      - x17(m, b, k, i)*t2a(c, a, m, j) & !bkicaj (-1.000)
                      + x17(m, c, k, i)*t2a(b, a, m, j) & !ckibaj (+1.000)
                      - x17(m, a, j, i)*t2a(c, b, m, k) & !ajicbk (-1.000)
                      + x17(m, b, j, i)*t2a(c, a, m, k) & !bjicak (+1.000)
                      - x17(m, c, j, i)*t2a(b, a, m, k) & !cjibak (-1.000)
                      - x17(m, a, k, j)*t2a(c, b, m, i) & !akjcbi (-1.000)
                      + x17(m, b, k, j)*t2a(c, a, m, i) & !bkjcai (+1.000)
                      - x17(m, c, k, j)*t2a(b, a, m, i) & !ckjbai (-1.000)
                      + x17(m, a, j, k)*t2a(c, b, m, i) & !ajkcbi (+1.000)
                      - x17(m, b, j, k)*t2a(c, a, m, i) & !bjkcai (-1.000)
                      + x17(m, c, j, k)*t2a(b, a, m, i) & !cjkbai (+1.000)
                      + x17(m, a, i, j)*t2a(c, b, m, k) & !aijcbk (+1.000)
                      - x17(m, b, i, j)*t2a(c, a, m, k) & !bijcak (-1.000)
                      + x17(m, c, i, j)*t2a(b, a, m, k) & !cijbak (+1.000)
                      - x17(m, a, i, k)*t2a(c, b, m, j) & !aikcbj (-1.000)
                      + x17(m, b, i, k)*t2a(c, a, m, j) & !bikcaj (+1.000)
                      - x17(m, c, i, k)*t2a(b, a, m, j)     !cikbaj (-1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x17)

    if (t2diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
        allocate (s21(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        i1 = k3*k3
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s21)
        deallocate (d1)
        deallocate (d2)

        factor = -t2diag3
        call sum_stripe(4, shape(x18), size(x18), '3412', factor, &
                        x18, s21)
        deallocate (s21)

    end if
    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,x18,t2diag3) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      - x18(e, c, a, k)*t2a(e, b, j, i) & !cakbji (-1.000)
                      + x18(e, c, b, k)*t2a(e, a, j, i) & !cbkaji (+1.000)
                      + x18(e, b, a, k)*t2a(e, c, j, i) & !bakcji (+1.000)
                      - x18(e, a, b, k)*t2a(e, c, j, i) & !abkcji (-1.000)
                      - x18(e, b, c, k)*t2a(e, a, j, i) & !bckaji (-1.000)
                      + x18(e, a, c, k)*t2a(e, b, j, i) & !ackbji (+1.000)
                      + x18(e, c, a, j)*t2a(e, b, k, i) & !cajbki (+1.000)
                      - x18(e, c, b, j)*t2a(e, a, k, i) & !cbjaki (-1.000)
                      - x18(e, b, a, j)*t2a(e, c, k, i) & !bajcki (-1.000)
                      + x18(e, a, b, j)*t2a(e, c, k, i) & !abjcki (+1.000)
                      + x18(e, b, c, j)*t2a(e, a, k, i) & !bcjaki (+1.000)
                      - x18(e, a, c, j)*t2a(e, b, k, i) & !acjbki (-1.000)
                      - x18(e, c, a, i)*t2a(e, b, k, j) & !caibkj (-1.000)
                      + x18(e, c, b, i)*t2a(e, a, k, j) & !cbiakj (+1.000)
                      + x18(e, b, a, i)*t2a(e, c, k, j) & !baickj (+1.000)
                      - x18(e, a, b, i)*t2a(e, c, k, j) & !abickj (-1.000)
                      - x18(e, b, c, i)*t2a(e, a, k, j) & !bciakj (-1.000)
                      + x18(e, a, c, i)*t2a(e, b, k, j)     !acibkj (+1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x18)

    allocate (x19(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x19 = 0.0d0

    if (t3diag3 .ne. 0) then
        allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
        allocate (f2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(6, shape(t3a), size(t3a), '451236', t3a, f2)
        allocate (s22(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
        i1 = k3
        i2 = k1*k3*k3
        i3 = k3*k1*k1
        call egemm(i1, i2, i3, d1, f2, s22)
        deallocate (d1)
        deallocate (f2)

        factor = t3diag3
        call sum_stripe(4, shape(x19), size(x19), '2341', factor, &
                        x19, s22)
        deallocate (s22)

    end if
    if (t3diag5 .ne. 0) then
        allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
        allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
        allocate (s23(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        i1 = k1*k1
        i2 = k1*k1
        i3 = k3*k3
        call egemm(i1, i2, i3, d1, d2, s23)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*t3diag5
        call sum_stripe(4, shape(x5), size(x5), '3412', factor, &
                        x5, s23)
        deallocate (s23)

    end if
    call sumx2143(n0, n3, n0, n1, n0, n1, n0, n1, n0, n1, x5, intr, 1.000)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t3a,x5,t2diag3) &
        !$omp private(a,b,c,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (x5(n, m, j, i)*t3a(c, b, a, n, m, k) & !jicbak (+0.500)
                         - x5(n, m, k, i)*t3a(c, b, a, n, m, j) & !kicbaj (-0.500)
                         + x5(n, m, k, j)*t3a(c, b, a, n, m, i))/2.0d0!kjcbai (+0.500)
            end do; end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x5)

    allocate (x20(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x20 = 0.0d0

    if (t3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2431', intr, d1)
        allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
        call reorder_stripe(6, shape(t3a), size(t3a), '412356', t3a, f2)
        allocate (s24(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1*k1*k3
        i3 = k3*k3*k1
        call egemm(i1, i2, i3, d1, f2, s24)
        deallocate (d1)
        deallocate (f2)

        factor = t3diag4
        call sum_stripe(4, shape(x20), size(x20), '2341', factor, &
                        x20, s24)
        deallocate (s24)

    end if
    if (t3diag1 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        if (t3diag2 .eq. 0) then
            call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                               (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intm, d1)
        else
            call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                               (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
        end if
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
        allocate (s25(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s25)
        deallocate (d1)
        deallocate (d2)

        factor = t3diag1
        call sum_stripe(4, shape(x6), size(x6), '3412', factor, &
                        x6, s25)
        deallocate (s25)

    end if
    if (t3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '1432', intr, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
        allocate (q11(n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1
        i3 = k3*k3*k1
        call egemm(i1, i2, i3, d1, d2, q11)
        deallocate (d1)
        deallocate (d2)

        factor = -0.500*t3diag4
        call sum_stripe(2, shape(x3), size(x3), '21', factor, x3, &
                        q11)
        deallocate (q11)

    end if
    if (t3diag5 .ne. 0) then
        allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
        allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
        allocate (s26(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
        i1 = k3*k3
        i2 = k3*k3
        i3 = k1*k1
        call egemm(i1, i2, i3, d1, d2, s26)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*t3diag5
        call sum_stripe(4, shape(x7), size(x7), '3412', factor, &
                        x7, s26)
        deallocate (s26)

    end if
    call sumx4321(n0, n3, n1, n3, n1, n3, n1, n3, n1, n3, x7, intr, 1.000)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x7,t3a,t2diag3) &
        !$omp private(a,b,c,e,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do f = n1 + 1, n3
                sum = sum &
                      + (x7(f, e, c, b)*t3a(f, e, a, k, j, i) & !cbakji (+0.500)
                         - x7(f, e, c, a)*t3a(f, e, b, k, j, i) & !cabkji (-0.500)
                         + x7(f, e, b, a)*t3a(f, e, c, k, j, i))/2.0d0!backji (+0.500)
            end do; end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x7)

    if (t3diag3 .ne. 0) then
        allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2134', intr, d1)
        allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
        allocate (q12(n1 + 1:n3, n1 + 1:n3))
        i1 = k3
        i2 = k3
        i3 = k3*k1*k1
        call egemm(i1, i2, i3, d1, d2, q12)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*t3diag3
        call sum_stripe(2, shape(x4), size(x4), '21', factor, x4, &
                        q12)
        deallocate (q12)

        allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
        allocate (f2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(6, shape(t3b), size(t3b), '451236', t3b, f2)
        allocate (s27(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
        i1 = k3
        i2 = k1*k3*k3
        i3 = k4*k1*k2
        call egemm(i1, i2, i3, d1, f2, s27)
        deallocate (d1)
        deallocate (f2)

        factor = 2.000*t3diag3
        call sum_stripe(4, shape(x19), size(x19), '2341', factor, &
                        x19, s27)
        deallocate (s27)

    end if
    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,x19,t2diag3) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + (x19(e, b, a, k)*t2a(e, c, j, i) & !bakcji (+0.500)
                         - x19(e, c, a, k)*t2a(e, b, j, i) & !cakbji (-0.500)
                         + x19(e, c, b, k)*t2a(e, a, j, i) & !cbkaji (+0.500)
                         - x19(e, b, a, j)*t2a(e, c, k, i) & !bajcki (-0.500)
                         + x19(e, c, a, j)*t2a(e, b, k, i) & !cajbki (+0.500)
                         - x19(e, c, b, j)*t2a(e, a, k, i) & !cbjaki (-0.500)
                         + x19(e, b, a, i)*t2a(e, c, k, j) & !baickj (+0.500)
                         - x19(e, c, a, i)*t2a(e, b, k, j) & !caibkj (-0.500)
                         + x19(e, c, b, i)*t2a(e, a, k, j))/2.0d0!cbiakj (+0.500)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x19)

    if (t3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
        allocate (f2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
        call reorder_stripe(6, shape(t3b), size(t3b), '412356', t3b, f2)
        allocate (s28(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1*k1*k3
        i3 = k3*k4*k2
        call egemm(i1, i2, i3, d1, f2, s28)
        deallocate (d1)
        deallocate (f2)

        factor = 2.000*t3diag4
        call sum_stripe(4, shape(x20), size(x20), '2341', 2.000, &
                        x20, s28)
        deallocate (s28)

    end if
    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t2a,x20,t2diag3) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + (x20(m, a, k, j)*t2a(c, b, m, i) & !akjcbi (+0.500)
                         - x20(m, b, k, j)*t2a(c, a, m, i) & !bkjcai (-0.500)
                         + x20(m, c, k, j)*t2a(b, a, m, i) & !ckjbai (+0.500)
                         - x20(m, a, k, i)*t2a(c, b, m, j) & !akicbj (-0.500)
                         + x20(m, b, k, i)*t2a(c, a, m, j) & !bkicaj (+0.500)
                         - x20(m, c, k, i)*t2a(b, a, m, j) & !ckibaj (-0.500)
                         + x20(m, a, j, i)*t2a(c, b, m, k) & !ajicbk (+0.500)
                         - x20(m, b, j, i)*t2a(c, a, m, k) & !bjicak (-0.500)
                         + x20(m, c, j, i)*t2a(b, a, m, k))/2.0d0!cjibak (+0.500)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x20)

    if (t3diag1 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
        allocate (s29(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s29)
        deallocate (d1)
        deallocate (d2)

        factor = t3diag1
        call sum_stripe(4, shape(x8), size(x8), '3412', factor, &
                        x8, s29)
        deallocate (s29)

        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
        allocate (s30(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s30)
        deallocate (d1)
        deallocate (d2)

        call sum_stripe(4, shape(x6), size(x6), '3412', factor, &
                        x6, s30)
        deallocate (s30)

    end if
    call sumx3142(n0, n3, n0, n1, n1, n3, n1, n3, n0, n1, x6, intr, 1.000)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x6,t3a,t2diag3) &
        !$omp private(a,b,c,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x6(m, e, c, i)*t3a(e, b, a, m, k, j) & !cibakj (+1.000)
                      - x6(m, e, b, i)*t3a(e, c, a, m, k, j) & !bicakj (-1.000)
                      + x6(m, e, a, i)*t3a(e, c, b, m, k, j) & !aicbkj (+1.000)
                      - x6(m, e, c, j)*t3a(e, b, a, m, k, i) & !cjbaki (-1.000)
                      + x6(m, e, b, j)*t3a(e, c, a, m, k, i) & !bjcaki (+1.000)
                      - x6(m, e, a, j)*t3a(e, c, b, m, k, i) & !ajcbki (-1.000)
                      + x6(m, e, c, k)*t3a(e, b, a, m, j, i) & !ckbaji (+1.000)
                      - x6(m, e, b, k)*t3a(e, c, a, m, j, i) & !bkcaji (-1.000)
                      + x6(m, e, a, k)*t3a(e, c, b, m, j, i)   !akcbji (+1.000)
            end do; end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x6)

    if (t3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
        allocate (q13(n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1
        i3 = k3*k4*k2
        call egemm(i1, i2, i3, d1, d2, q13)
        deallocate (d1)
        deallocate (d2)

        factor = t3diag4
        call sum_stripe(2, shape(x3), size(x3), '21', factor, x3, &
                        q13)
        deallocate (q13)

    end if
    call sumx12(0, n3, n0, n1, n0, n1, x3, fockr, 1.000)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x3,t3a,t2diag3) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x3(m, i)*t3a(c, b, a, m, k, j) & !icbakj (-1.000)
                      + x3(m, j)*t3a(c, b, a, m, k, i) & !jcbaki (+1.000)
                      - x3(m, k)*t3a(c, b, a, m, j, i)     !kcbaji (-1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x3)

    if (t3diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
        allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
        allocate (q14(n1 + 1:n3, n1 + 1:n3))
        i1 = k3
        i2 = k3
        i3 = k4*k1*k2
        call egemm(i1, i2, i3, d1, d2, q14)
        deallocate (d1)
        deallocate (d2)

        factor = -t3diag3
        call sum_stripe(2, shape(x4), size(x4), '21', factor, x4, &
                        q14)
        deallocate (q14)

    end if
    call sumx21(0, n3, n1, n3, n1, n3, x4, fockr, 1.000)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,x4,t3a,t2diag3) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x4(e, c)*t3a(e, b, a, k, j, i) & !cbakji (+1.000)
                      - x4(e, b)*t3a(e, c, a, k, j, i) & !bcakji (-1.000)
                      + x4(e, a)*t3a(e, c, b, k, j, i)     !acbkji (+1.000)
            end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x4)

    if (t3diag1 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        if (t3diag2 .eq. 0) then
            call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                               (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
        else
            call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                               (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
        end if
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
        allocate (s31(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s31)
        deallocate (d1)
        deallocate (d2)

        factor = t3diag1
        call sum_stripe(4, shape(x8), size(x8), '3412', factor, &
                        x8, s31)
        deallocate (s31)

    end if
    call sumx3142(n0, n3, n0, n2, n2, n3, n1, n3, n0, n1, x8, intm, 1.000)

    do i = n0 + 1, n1 - 2; do j = i + 1, n1 - 1; do k = j + 1, n1
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3a,t3b,x8,t2diag3) &
        !$omp private(a,b,c,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x8(m, e, c, i)*t3b(e, b, a, m, k, j) & !cibakj (+1.000)
                      - x8(m, e, b, i)*t3b(e, c, a, m, k, j) & !bicakj (-1.000)
                      + x8(m, e, a, i)*t3b(e, c, b, m, k, j) & !aicbkj (+1.000)
                      - x8(m, e, c, j)*t3b(e, b, a, m, k, i) & !cjbaki (-1.000)
                      + x8(m, e, b, j)*t3b(e, c, a, m, k, i) & !bjcaki (+1.000)
                      - x8(m, e, a, j)*t3b(e, c, b, m, k, i) & !ajcbki (-1.000)
                      + x8(m, e, c, k)*t3b(e, b, a, m, j, i) & !ckbaji (+1.000)
                      - x8(m, e, b, k)*t3b(e, c, a, m, j, i) & !bkcaji (-1.000)
                      + x8(m, e, a, k)*t3b(e, c, b, m, j, i)   !akcbji (+1.000)
            end do; end do
            v3a(c, b, a, k, j, i) = v3a(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x8)

    do i = n0 + 1, n1 - 2
    do j = i + 1, n1 - 1
    do k = j + 1, n1
    do a = n1 + 1, n3 - 2
    do b = a + 1, n3 - 1
    do c = b + 1, n3
        coeleft = fockr(c, c) &
                  + fockr(b, b) &
                  + fockr(a, a) &
                  - fockr(k, k) &
                  - fockr(j, j) &
                  - fockr(i, i) &
                  + shift
        t3a(c, b, a, k, j, i) = t3a(c, b, a, k, j, i) - v3a(c, b, a, k, j, i)/coeleft
        t3a(c, b, a, k, i, j) = -t3a(c, b, a, k, j, i)
        t3a(c, b, a, i, j, k) = -t3a(c, b, a, k, j, i)
        t3a(c, b, a, i, k, j) = t3a(c, b, a, k, j, i)
        t3a(c, b, a, j, k, i) = -t3a(c, b, a, k, j, i)
        t3a(c, b, a, j, i, k) = t3a(c, b, a, k, j, i)
        t3a(c, a, b, k, j, i) = -t3a(c, b, a, k, j, i)
        t3a(c, a, b, k, i, j) = t3a(c, b, a, k, j, i)
        t3a(c, a, b, i, j, k) = t3a(c, b, a, k, j, i)
        t3a(c, a, b, i, k, j) = -t3a(c, b, a, k, j, i)
        t3a(c, a, b, j, k, i) = t3a(c, b, a, k, j, i)
        t3a(c, a, b, j, i, k) = -t3a(c, b, a, k, j, i)
        t3a(a, b, c, k, j, i) = -t3a(c, b, a, k, j, i)
        t3a(a, b, c, k, i, j) = t3a(c, b, a, k, j, i)
        t3a(a, b, c, i, j, k) = t3a(c, b, a, k, j, i)
        t3a(a, b, c, i, k, j) = -t3a(c, b, a, k, j, i)
        t3a(a, b, c, j, k, i) = t3a(c, b, a, k, j, i)
        t3a(a, b, c, j, i, k) = -t3a(c, b, a, k, j, i)
        t3a(a, c, b, k, j, i) = t3a(c, b, a, k, j, i)
        t3a(a, c, b, k, i, j) = -t3a(c, b, a, k, j, i)
        t3a(a, c, b, i, j, k) = -t3a(c, b, a, k, j, i)
        t3a(a, c, b, i, k, j) = t3a(c, b, a, k, j, i)
        t3a(a, c, b, j, k, i) = -t3a(c, b, a, k, j, i)
        t3a(a, c, b, j, i, k) = t3a(c, b, a, k, j, i)
        t3a(b, c, a, k, j, i) = -t3a(c, b, a, k, j, i)
        t3a(b, c, a, k, i, j) = t3a(c, b, a, k, j, i)
        t3a(b, c, a, i, j, k) = t3a(c, b, a, k, j, i)
        t3a(b, c, a, i, k, j) = -t3a(c, b, a, k, j, i)
        t3a(b, c, a, j, k, i) = t3a(c, b, a, k, j, i)
        t3a(b, c, a, j, i, k) = -t3a(c, b, a, k, j, i)
        t3a(b, a, c, k, j, i) = t3a(c, b, a, k, j, i)
        t3a(b, a, c, k, i, j) = -t3a(c, b, a, k, j, i)
        t3a(b, a, c, i, j, k) = -t3a(c, b, a, k, j, i)
        t3a(b, a, c, i, k, j) = t3a(c, b, a, k, j, i)
        t3a(b, a, c, j, k, i) = -t3a(c, b, a, k, j, i)
        t3a(b, a, c, j, i, k) = t3a(c, b, a, k, j, i)
    end do
    end do
    end do
    end do
    end do
    end do

end subroutine t3a_update



