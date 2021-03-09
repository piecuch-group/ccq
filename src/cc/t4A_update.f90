subroutine t4a_update(n0, n1, n2, n3, k1, k2, k3, k4, shift, &
                      fockr, fockb, intr, intb, intm, t1a, t1b, t2a, t2b, t2c, t3a, t3b, t3c, t3d, &
                      iactocca, iactunoa, iactindq)

    integer :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p
    integer :: iactocca, iactunoa, iactindq
    integer :: iocca, iunoa
    integer, allocatable:: indocc(:, :, :, :)
    integer, allocatable:: indunocc(:, :, :, :)
    real(kind=8) :: shift, pp, coeleft, timt1, timt2, time1, time2
    real(kind=8) :: sum
    real(kind=8) :: fockr(n3, n3)
    real(kind=8) :: fockb(n3, n3)
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

    real(kind=8), allocatable::t4a(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::t4b(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::v4a(:, :, :, :, :, :, :, :)

    integer :: ta, tb, tc, td, te
    parameter(ta=29, tb=30, tc=31, td=32, te=33)

    real(kind=8), allocatable::b1(:, :)
    real(kind=8), allocatable::b2(:, :)
    real(kind=8), allocatable::d1(:, :, :, :)
    real(kind=8), allocatable::d2(:, :, :, :)
    real(kind=8), allocatable::f1(:, :, :, :, :, :)
    real(kind=8), allocatable::f2(:, :, :, :, :, :)
    real(kind=8), allocatable::h2(:, :, :, :, :, :, :, :)

    real(kind=8), allocatable::s1(:, :, :, :)
    real(kind=8), allocatable::s2(:, :, :, :)
    real(kind=8), allocatable::s3(:, :, :, :)
    real(kind=8), allocatable::s4(:, :, :, :)
    real(kind=8), allocatable::q1(:, :)
    real(kind=8), allocatable::q2(:, :)
    real(kind=8), allocatable::s5(:, :, :, :)
    real(kind=8), allocatable::u18(:, :, :, :, :, :)
    real(kind=8), allocatable::s29(:, :, :, :)
    real(kind=8), allocatable::s6(:, :, :, :)
    real(kind=8), allocatable::u19(:, :, :, :, :, :)
    real(kind=8), allocatable::s30(:, :, :, :)
    real(kind=8), allocatable::q3(:, :)
    real(kind=8), allocatable::s7(:, :, :, :)
    real(kind=8), allocatable::u20(:, :, :, :, :, :)
    real(kind=8), allocatable::s32(:, :, :, :)
    real(kind=8), allocatable::s31(:, :, :, :)
    real(kind=8), allocatable::s8(:, :, :, :)
    real(kind=8), allocatable::u21(:, :, :, :, :, :)
    real(kind=8), allocatable::q4(:, :)
    real(kind=8), allocatable::s9(:, :, :, :)
    real(kind=8), allocatable::s10(:, :, :, :)
    real(kind=8), allocatable::q5(:, :)
    real(kind=8), allocatable::q6(:, :)
    real(kind=8), allocatable::u1(:, :, :, :, :, :)
    real(kind=8), allocatable::u2(:, :, :, :, :, :)
    real(kind=8), allocatable::u3(:, :, :, :, :, :)
    real(kind=8), allocatable::s11(:, :, :, :)
    real(kind=8), allocatable::s12(:, :, :, :)
    real(kind=8), allocatable::u4(:, :, :, :, :, :)
    real(kind=8), allocatable::u5(:, :, :, :, :, :)
    real(kind=8), allocatable::s13(:, :, :, :)
    real(kind=8), allocatable::s14(:, :, :, :)
    real(kind=8), allocatable::u6(:, :, :, :, :, :)
    real(kind=8), allocatable::s15(:, :, :, :)
    real(kind=8), allocatable::u7(:, :, :, :, :, :)
    real(kind=8), allocatable::s16(:, :, :, :)
    real(kind=8), allocatable::u8(:, :, :, :, :, :)
    real(kind=8), allocatable::u9(:, :, :, :, :, :)
    real(kind=8), allocatable::u10(:, :, :, :, :, :)
    real(kind=8), allocatable::u30(:, :, :, :, :, :)
    real(kind=8), allocatable::u29(:, :, :, :, :, :)
    real(kind=8), allocatable::u28(:, :, :, :, :, :)
    real(kind=8), allocatable::s43(:, :, :, :)
    real(kind=8), allocatable::u24(:, :, :, :, :, :)
    real(kind=8), allocatable::u35(:, :, :, :, :, :)
    real(kind=8), allocatable::u22(:, :, :, :, :, :)
    real(kind=8), allocatable::u34(:, :, :, :, :, :)
    real(kind=8), allocatable::s17(:, :, :, :)
    real(kind=8), allocatable::u31(:, :, :, :, :, :)
    real(kind=8), allocatable::s42(:, :, :, :)
    real(kind=8), allocatable::u11(:, :, :, :, :, :)
    real(kind=8), allocatable::s18(:, :, :, :)
    real(kind=8), allocatable::s44(:, :, :, :)
    real(kind=8), allocatable::s40(:, :, :, :)
    real(kind=8), allocatable::q7(:, :)
    real(kind=8), allocatable::s19(:, :, :, :)
    real(kind=8), allocatable::s41(:, :, :, :)
    real(kind=8), allocatable::q8(:, :)
    real(kind=8), allocatable::u12(:, :, :, :, :, :)
    real(kind=8), allocatable::u32(:, :, :, :, :, :)
    real(kind=8), allocatable::s48(:, :, :, :)
    real(kind=8), allocatable::u27(:, :, :, :, :, :)
    real(kind=8), allocatable::u13(:, :, :, :, :, :)
    real(kind=8), allocatable::s20(:, :, :, :)
    real(kind=8), allocatable::s21(:, :, :, :)
    real(kind=8), allocatable::s22(:, :, :, :)
    real(kind=8), allocatable::s23(:, :, :, :)
    real(kind=8), allocatable::s47(:, :, :, :)
    real(kind=8), allocatable::s46(:, :, :, :)
    real(kind=8), allocatable::q9(:, :)
    real(kind=8), allocatable::q10(:, :)
    real(kind=8), allocatable::s24(:, :, :, :)
    real(kind=8), allocatable::s25(:, :, :, :)
    real(kind=8), allocatable::u14(:, :, :, :, :, :)
    real(kind=8), allocatable::u25(:, :, :, :, :, :)
    real(kind=8), allocatable::s26(:, :, :, :)
    real(kind=8), allocatable::u15(:, :, :, :, :, :)
    real(kind=8), allocatable::u23(:, :, :, :, :, :)
    real(kind=8), allocatable::s27(:, :, :, :)
    real(kind=8), allocatable::s28(:, :, :, :)
    real(kind=8), allocatable::u16(:, :, :, :, :, :)
    real(kind=8), allocatable::u17(:, :, :, :, :, :)
    real(kind=8), allocatable::s33(:, :, :, :)
    real(kind=8), allocatable::q11(:, :)
    real(kind=8), allocatable::s35(:, :, :, :)
    real(kind=8), allocatable::s51(:, :, :, :)
    real(kind=8), allocatable::s34(:, :, :, :)
    real(kind=8), allocatable::u33(:, :, :, :, :, :)
    real(kind=8), allocatable::s50(:, :, :, :)
    real(kind=8), allocatable::s36(:, :, :, :)
    real(kind=8), allocatable::q12(:, :)
    real(kind=8), allocatable::s37(:, :, :, :)
    real(kind=8), allocatable::s38(:, :, :, :)
    real(kind=8), allocatable::u26(:, :, :, :, :, :)
    real(kind=8), allocatable::q13(:, :)
    real(kind=8), allocatable::s39(:, :, :, :)
    real(kind=8), allocatable::q14(:, :)
    real(kind=8), allocatable::s49(:, :, :, :)
    real(kind=8), allocatable::q15(:, :)
    real(kind=8), allocatable::q16(:, :)
    real(kind=8), allocatable::s45(:, :, :, :)
    real(kind=8), allocatable::x1(:, :, :, :)
    real(kind=8), allocatable::z1(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x2(:, :, :, :)
    real(kind=8), allocatable::z2(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x3(:, :)
    real(kind=8), allocatable::z3(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x4(:, :)
    real(kind=8), allocatable::z4(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x5(:, :, :, :)
    real(kind=8), allocatable::z5(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x6(:, :, :, :)
    real(kind=8), allocatable::z6(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x7(:, :, :, :)
    real(kind=8), allocatable::z7(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x8(:, :, :, :)
    real(kind=8), allocatable::z8(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x9(:, :, :, :)
    real(kind=8), allocatable::z10(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x10(:, :, :, :)
    real(kind=8), allocatable::z11(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z76(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z15(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z77(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z78(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z79(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z19(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z25(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z26(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z27(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z30(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z31(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z34(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z36(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z38(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z39(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z98(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z97(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z96(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z105(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z84(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z104(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z80(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z40(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z99(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z42(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z100(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z91(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z47(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z48(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z56(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z87(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z57(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z58(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z81(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z59(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z60(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z61(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z62(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z63(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z103(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z90(:, :, :, :, :, :, :, :)

    allocate (t4a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    allocate (t4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    allocate (v4a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))

    rewind (ta)
    rewind (tb)
    read (ta) t4a
    read (tb) t4b

    allocate (indocc(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    allocate (indunocc(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    indocc = 0
    indunocc = 0
    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        iocca = 0
        if (i .gt. (n1 - iactocca)) iocca = iocca + 1
        if (j .gt. (n1 - iactocca)) iocca = iocca + 1
        if (k .gt. (n1 - iactocca)) iocca = iocca + 1
        if (l .gt. (n1 - iactocca)) iocca = iocca + 1
        if (iocca .lt. iactindq) indocc(l, k, j, i) = 1
    end do; end do; end do; end do
    do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
        iunoa = 0
        if (a .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (b .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (c .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (d .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (iunoa .lt. iactindq) indunocc(d, c, b, a) = 1
    end do; end do; end do; end do

    v4a = 0.0d0

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

    allocate (x9(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x9 = 0.0d0
    call sum_stripe(4, shape(x9), size(x9), '3124', 1.000, x9, &
                    s2)
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

    allocate (x10(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x10 = 0.0d0
    call sum_stripe(4, shape(x10), size(x10), '3124', 1.000, &
                    x10, s3)
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
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s5(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s5)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t4a,s5) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (s5(j, m, i, n)*t4a(d, c, b, a, n, m, l, k) & ! lkji
                         - s5(k, m, i, n)*t4a(d, c, b, a, n, m, l, j) & ! ljki (-0.500)
                         + s5(l, m, i, n)*t4a(d, c, b, a, n, m, k, j) & ! kjli (+0.500)
                         - s5(i, m, j, n)*t4a(d, c, b, a, n, m, l, k) & ! lkij (-0.500)
                         + s5(i, m, k, n)*t4a(d, c, b, a, n, m, l, j) & ! ljik (+0.500)
                         - s5(i, m, l, n)*t4a(d, c, b, a, n, m, k, j) & ! kjil (-0.500)
                         + s5(k, m, j, n)*t4a(d, c, b, a, n, m, l, i) & ! likj (+0.500)
                         - s5(l, m, j, n)*t4a(d, c, b, a, n, m, k, i) & ! kilj (-0.500)
                         - s5(j, m, k, n)*t4a(d, c, b, a, n, m, l, i) & ! lijk (-0.500)
                         + s5(j, m, l, n)*t4a(d, c, b, a, n, m, k, i) & ! kijl (+0.500)
                         + s5(l, m, k, n)*t4a(d, c, b, a, n, m, j, i) & ! jilk (+0.500)
                         - s5(k, m, l, n)*t4a(d, c, b, a, n, m, j, i))/2.0d0! jikl (-0.500)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s5), size(s5), '2413', s5, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u18(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u18)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u18) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + u18(b, a, l, n, j, i)*t2a(d, c, n, k) & !balnjidcnk      (+1.000)
                      - u18(c, a, l, n, j, i)*t2a(d, b, n, k) & !calnjidbnk      (-1.000)
                      - u18(d, a, k, n, j, i)*t2a(c, b, n, l) & !daknjicbnl      (-1.000)
                      + u18(d, a, l, n, j, i)*t2a(c, b, n, k) & !dalnjicbnk      (+1.000)
                      + u18(c, a, k, n, j, i)*t2a(d, b, n, l) & !caknjidbnl      (+1.000)
                      - u18(b, a, k, n, j, i)*t2a(d, c, n, l) & !baknjidcnl      (-1.000)
                      - u18(b, a, l, n, k, i)*t2a(d, c, n, j) & !balnkidcnj      (-1.000)
                      + u18(c, a, l, n, k, i)*t2a(d, b, n, j) & !calnkidbnj      (+1.000)
                      + u18(d, a, j, n, k, i)*t2a(c, b, n, l) & !dajnkicbnl      (+1.000)
                      - u18(d, a, l, n, k, i)*t2a(c, b, n, j) & !dalnkicbnj      (-1.000)
                      - u18(c, a, j, n, k, i)*t2a(d, b, n, l) & !cajnkidbnl      (-1.000)
                      + u18(b, a, j, n, k, i)*t2a(d, c, n, l) & !bajnkidcnl      (+1.000)
                      + u18(b, a, k, n, l, i)*t2a(d, c, n, j) & !baknlidcnj      (+1.000)
                      - u18(c, a, k, n, l, i)*t2a(d, b, n, j) & !caknlidbnj      (-1.000)
                      - u18(d, a, j, n, l, i)*t2a(c, b, n, k) & !dajnlicbnk      (-1.000)
                      + u18(d, a, k, n, l, i)*t2a(c, b, n, j) & !daknlicbnj      (+1.000)
                      + u18(c, a, j, n, l, i)*t2a(d, b, n, k) & !cajnlidbnk      (+1.000)
                      - u18(b, a, j, n, l, i)*t2a(d, c, n, k) & !bajnlidcnk      (-1.000)
                      - u18(b, a, l, n, i, j)*t2a(d, c, n, k) & !balnijdcnk      (-1.000)
                      + u18(c, a, l, n, i, j)*t2a(d, b, n, k) & !calnijdbnk      (+1.000)
                      + u18(d, a, k, n, i, j)*t2a(c, b, n, l) & !daknijcbnl      (+1.000)
                      - u18(d, a, l, n, i, j)*t2a(c, b, n, k) & !dalnijcbnk      (-1.000)
                      - u18(c, a, k, n, i, j)*t2a(d, b, n, l) & !caknijdbnl      (-1.000)
                      + u18(b, a, k, n, i, j)*t2a(d, c, n, l) & !baknijdcnl      (+1.000)
                      + u18(b, a, l, n, i, k)*t2a(d, c, n, j) & !balnikdcnj      (+1.000)
                      - u18(c, a, l, n, i, k)*t2a(d, b, n, j) & !calnikdbnj      (-1.000)
                      - u18(d, a, j, n, i, k)*t2a(c, b, n, l) & !dajnikcbnl      (-1.000)
                      + u18(d, a, l, n, i, k)*t2a(c, b, n, j) & !dalnikcbnj      (+1.000)
                      + u18(c, a, j, n, i, k)*t2a(d, b, n, l) & !cajnikdbnl      (+1.000)
                      - u18(b, a, j, n, i, k)*t2a(d, c, n, l) & !bajnikdcnl      (-1.000)
                      - u18(b, a, k, n, i, l)*t2a(d, c, n, j) & !baknildcnj      (-1.000)
                      + u18(c, a, k, n, i, l)*t2a(d, b, n, j) & !caknildbnj      (+1.000)
                      + u18(d, a, j, n, i, l)*t2a(c, b, n, k) & !dajnilcbnk      (+1.000)
                      - u18(d, a, k, n, i, l)*t2a(c, b, n, j) & !daknilcbnj      (-1.000)
                      - u18(c, a, j, n, i, l)*t2a(d, b, n, k) & !cajnildbnk      (-1.000)
                      + u18(b, a, j, n, i, l)*t2a(d, c, n, k) & !bajnildcnk      (+1.000)
                      + u18(b, a, l, n, k, j)*t2a(d, c, n, i) & !balnkjdcni      (+1.000)
                      - u18(c, a, l, n, k, j)*t2a(d, b, n, i) & !calnkjdbni      (-1.000)
                      - u18(d, a, i, n, k, j)*t2a(c, b, n, l) & !dainkjcbnl      (-1.000)
                      + u18(d, a, l, n, k, j)*t2a(c, b, n, i) & !dalnkjcbni      (+1.000)
                      + u18(c, a, i, n, k, j)*t2a(d, b, n, l) & !cainkjdbnl      (+1.000)
                      - u18(b, a, i, n, k, j)*t2a(d, c, n, l) & !bainkjdcnl      (-1.000)
                      - u18(b, a, k, n, l, j)*t2a(d, c, n, i) & !baknljdcni      (-1.000)
                      + u18(c, a, k, n, l, j)*t2a(d, b, n, i) & !caknljdbni      (+1.000)
                      + u18(d, a, i, n, l, j)*t2a(c, b, n, k) & !dainljcbnk      (+1.000)
                      - u18(d, a, k, n, l, j)*t2a(c, b, n, i) & !daknljcbni      (-1.000)
                      - u18(c, a, i, n, l, j)*t2a(d, b, n, k) & !cainljdbnk      (-1.000)
                      + u18(b, a, i, n, l, j)*t2a(d, c, n, k) & !bainljdcnk      (+1.000)
                      - u18(b, a, l, n, j, k)*t2a(d, c, n, i) & !balnjkdcni      (-1.000)
                      + u18(c, a, l, n, j, k)*t2a(d, b, n, i) & !calnjkdbni      (+1.000)
                      + u18(d, a, i, n, j, k)*t2a(c, b, n, l) & !dainjkcbnl      (+1.000)
                      - u18(d, a, l, n, j, k)*t2a(c, b, n, i) & !dalnjkcbni      (-1.000)
                      - u18(c, a, i, n, j, k)*t2a(d, b, n, l) & !cainjkdbnl      (-1.000)
                      + u18(b, a, i, n, j, k)*t2a(d, c, n, l) & !bainjkdcnl      (+1.000)
                      + u18(b, a, k, n, j, l)*t2a(d, c, n, i) & !baknjldcni      (+1.000)
                      - u18(c, a, k, n, j, l)*t2a(d, b, n, i) & !caknjldbni      (-1.000)
                      - u18(d, a, i, n, j, l)*t2a(c, b, n, k) & !dainjlcbnk      (-1.000)
                      + u18(d, a, k, n, j, l)*t2a(c, b, n, i) & !daknjlcbni      (+1.000)
                      + u18(c, a, i, n, j, l)*t2a(d, b, n, k) & !cainjldbnk      (+1.000)
                      - u18(b, a, i, n, j, l)*t2a(d, c, n, k) & !bainjldcnk      (-1.000)
                      + u18(b, a, j, n, l, k)*t2a(d, c, n, i) & !bajnlkdcni      (+1.000)
                      - u18(c, a, j, n, l, k)*t2a(d, b, n, i) & !cajnlkdbni      (-1.000)
                      - u18(d, a, i, n, l, k)*t2a(c, b, n, j) & !dainlkcbnj      (-1.000)
                      + u18(d, a, j, n, l, k)*t2a(c, b, n, i) & !dajnlkcbni      (+1.000)
                      + u18(c, a, i, n, l, k)*t2a(d, b, n, j) & !cainlkdbnj      (+1.000)
                      - u18(b, a, i, n, l, k)*t2a(d, c, n, j) & !bainlkdcnj      (-1.000)
                      - u18(b, a, j, n, k, l)*t2a(d, c, n, i) & !bajnkldcni      (-1.000)
                      + u18(c, a, j, n, k, l)*t2a(d, b, n, i) & !cajnkldbni      (+1.000)
                      + u18(d, a, i, n, k, l)*t2a(c, b, n, j) & !dainklcbnj      (+1.000)
                      - u18(d, a, j, n, k, l)*t2a(c, b, n, i) & !dajnklcbni      (-1.000)
                      - u18(c, a, i, n, k, l)*t2a(d, b, n, j) & !cainkldbnj      (-1.000)
                      + u18(b, a, i, n, k, l)*t2a(d, c, n, j)       !bainkldcnj      (+1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u18)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s5), size(s5), '2413', s5, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s29(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s29)
    deallocate (d1)
    deallocate (b2)
    deallocate (s5)

    call sum_stripe(4, shape(x9), size(x9), '2134', -1.000, &
                    x9, s29)
    deallocate (s29)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s6(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s6)
    deallocate (d1)
    deallocate (b2)

    allocate (x6(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x6 = 0.0d0
    call sum_stripe(4, shape(x6), size(x6), '3241', -1.000, &
                    x6, s6)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s6), size(s6), '2413', s6, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u19(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u19)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u19) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + u19(c, k, j, n, d, i)*t2a(b, a, n, l) & !ckjndibanl      (+1.000)
                      - u19(b, k, j, n, d, i)*t2a(c, a, n, l) & !bkjndicanl      (-1.000)
                      + u19(a, k, j, n, d, i)*t2a(c, b, n, l) & !akjndicbnl      (+1.000)
                      - u19(d, k, j, n, c, i)*t2a(b, a, n, l) & !dkjncibanl      (-1.000)
                      + u19(d, k, j, n, b, i)*t2a(c, a, n, l) & !dkjnbicanl      (+1.000)
                      - u19(d, k, j, n, a, i)*t2a(c, b, n, l) & !dkjnaicbnl      (-1.000)
                      + u19(b, k, j, n, c, i)*t2a(d, a, n, l) & !bkjncidanl      (+1.000)
                      - u19(a, k, j, n, c, i)*t2a(d, b, n, l) & !akjncidbnl      (-1.000)
                      - u19(c, k, j, n, b, i)*t2a(d, a, n, l) & !ckjnbidanl      (-1.000)
                      + u19(c, k, j, n, a, i)*t2a(d, b, n, l) & !ckjnaidbnl      (+1.000)
                      + u19(a, k, j, n, b, i)*t2a(d, c, n, l) & !akjnbidcnl      (+1.000)
                      - u19(b, k, j, n, a, i)*t2a(d, c, n, l) & !bkjnaidcnl      (-1.000)
                      - u19(c, l, j, n, d, i)*t2a(b, a, n, k) & !cljndibank      (-1.000)
                      + u19(b, l, j, n, d, i)*t2a(c, a, n, k) & !bljndicank      (+1.000)
                      - u19(a, l, j, n, d, i)*t2a(c, b, n, k) & !aljndicbnk      (-1.000)
                      + u19(d, l, j, n, c, i)*t2a(b, a, n, k) & !dljncibank      (+1.000)
                      - u19(d, l, j, n, b, i)*t2a(c, a, n, k) & !dljnbicank      (-1.000)
                      + u19(d, l, j, n, a, i)*t2a(c, b, n, k) & !dljnaicbnk      (+1.000)
                      - u19(b, l, j, n, c, i)*t2a(d, a, n, k) & !bljncidank      (-1.000)
                      + u19(a, l, j, n, c, i)*t2a(d, b, n, k) & !aljncidbnk      (+1.000)
                      + u19(c, l, j, n, b, i)*t2a(d, a, n, k) & !cljnbidank      (+1.000)
                      - u19(c, l, j, n, a, i)*t2a(d, b, n, k) & !cljnaidbnk      (-1.000)
                      - u19(a, l, j, n, b, i)*t2a(d, c, n, k) & !aljnbidcnk      (-1.000)
                      + u19(b, l, j, n, a, i)*t2a(d, c, n, k) & !bljnaidcnk      (+1.000)
                      + u19(a, l, k, n, d, i)*t2a(c, b, n, j) & !alkndicbnj      (+1.000)
                      - u19(b, l, k, n, d, i)*t2a(c, a, n, j) & !blkndicanj      (-1.000)
                      + u19(c, l, k, n, d, i)*t2a(b, a, n, j) & !clkndibanj      (+1.000)
                      - u19(a, l, k, n, c, i)*t2a(d, b, n, j) & !alkncidbnj      (-1.000)
                      + u19(b, l, k, n, c, i)*t2a(d, a, n, j) & !blkncidanj      (+1.000)
                      + u19(a, l, k, n, b, i)*t2a(d, c, n, j) & !alknbidcnj      (+1.000)
                      - u19(b, l, k, n, a, i)*t2a(d, c, n, j) & !blknaidcnj      (-1.000)
                      - u19(c, l, k, n, b, i)*t2a(d, a, n, j) & !clknbidanj      (-1.000)
                      + u19(c, l, k, n, a, i)*t2a(d, b, n, j) & !clknaidbnj      (+1.000)
                      - u19(d, l, k, n, c, i)*t2a(b, a, n, j) & !dlkncibanj      (-1.000)
                      + u19(d, l, k, n, b, i)*t2a(c, a, n, j) & !dlknbicanj      (+1.000)
                      - u19(d, l, k, n, a, i)*t2a(c, b, n, j) & !dlknaicbnj      (-1.000)
                      - u19(c, k, i, n, d, j)*t2a(b, a, n, l) & !ckindjbanl      (-1.000)
                      + u19(b, k, i, n, d, j)*t2a(c, a, n, l) & !bkindjcanl      (+1.000)
                      - u19(a, k, i, n, d, j)*t2a(c, b, n, l) & !akindjcbnl      (-1.000)
                      + u19(d, k, i, n, c, j)*t2a(b, a, n, l) & !dkincjbanl      (+1.000)
                      - u19(d, k, i, n, b, j)*t2a(c, a, n, l) & !dkinbjcanl      (-1.000)
                      + u19(d, k, i, n, a, j)*t2a(c, b, n, l) & !dkinajcbnl      (+1.000)
                      - u19(b, k, i, n, c, j)*t2a(d, a, n, l) & !bkincjdanl      (-1.000)
                      + u19(a, k, i, n, c, j)*t2a(d, b, n, l) & !akincjdbnl      (+1.000)
                      + u19(c, k, i, n, b, j)*t2a(d, a, n, l) & !ckinbjdanl      (+1.000)
                      - u19(c, k, i, n, a, j)*t2a(d, b, n, l) & !ckinajdbnl      (-1.000)
                      - u19(a, k, i, n, b, j)*t2a(d, c, n, l) & !akinbjdcnl      (-1.000)
                      + u19(b, k, i, n, a, j)*t2a(d, c, n, l) & !bkinajdcnl      (+1.000)
                      + u19(c, l, i, n, d, j)*t2a(b, a, n, k) & !clindjbank      (+1.000)
                      - u19(b, l, i, n, d, j)*t2a(c, a, n, k) & !blindjcank      (-1.000)
                      + u19(a, l, i, n, d, j)*t2a(c, b, n, k) & !alindjcbnk      (+1.000)
                      - u19(d, l, i, n, c, j)*t2a(b, a, n, k) & !dlincjbank      (-1.000)
                      + u19(d, l, i, n, b, j)*t2a(c, a, n, k) & !dlinbjcank      (+1.000)
                      - u19(d, l, i, n, a, j)*t2a(c, b, n, k) & !dlinajcbnk      (-1.000)
                      + u19(b, l, i, n, c, j)*t2a(d, a, n, k) & !blincjdank      (+1.000)
                      - u19(a, l, i, n, c, j)*t2a(d, b, n, k) & !alincjdbnk      (-1.000)
                      - u19(c, l, i, n, b, j)*t2a(d, a, n, k) & !clinbjdank      (-1.000)
                      + u19(c, l, i, n, a, j)*t2a(d, b, n, k) & !clinajdbnk      (+1.000)
                      + u19(a, l, i, n, b, j)*t2a(d, c, n, k) & !alinbjdcnk      (+1.000)
                      - u19(b, l, i, n, a, j)*t2a(d, c, n, k) & !blinajdcnk      (-1.000)
                      - u19(a, l, k, n, d, j)*t2a(c, b, n, i) & !alkndjcbni      (-1.000)
                      + u19(b, l, k, n, d, j)*t2a(c, a, n, i) & !blkndjcani      (+1.000)
                      - u19(c, l, k, n, d, j)*t2a(b, a, n, i) & !clkndjbani      (-1.000)
                      + u19(a, l, k, n, c, j)*t2a(d, b, n, i) & !alkncjdbni      (+1.000)
                      - u19(b, l, k, n, c, j)*t2a(d, a, n, i) & !blkncjdani      (-1.000)
                      - u19(a, l, k, n, b, j)*t2a(d, c, n, i) & !alknbjdcni      (-1.000)
                      + u19(b, l, k, n, a, j)*t2a(d, c, n, i) & !blknajdcni      (+1.000)
                      + u19(c, l, k, n, b, j)*t2a(d, a, n, i) & !clknbjdani      (+1.000)
                      - u19(c, l, k, n, a, j)*t2a(d, b, n, i) & !clknajdbni      (-1.000)
                      + u19(d, l, k, n, c, j)*t2a(b, a, n, i) & !dlkncjbani      (+1.000)
                      - u19(d, l, k, n, b, j)*t2a(c, a, n, i) & !dlknbjcani      (-1.000)
                      + u19(d, l, k, n, a, j)*t2a(c, b, n, i) & !dlknajcbni      (+1.000)
                      + u19(c, j, i, n, d, k)*t2a(b, a, n, l) & !cjindkbanl      (+1.000)
                      - u19(b, j, i, n, d, k)*t2a(c, a, n, l) & !bjindkcanl      (-1.000)
                      + u19(a, j, i, n, d, k)*t2a(c, b, n, l) & !ajindkcbnl      (+1.000)
                      - u19(d, j, i, n, c, k)*t2a(b, a, n, l) & !djinckbanl      (-1.000)
                      + u19(d, j, i, n, b, k)*t2a(c, a, n, l) & !djinbkcanl      (+1.000)
                      - u19(d, j, i, n, a, k)*t2a(c, b, n, l) & !djinakcbnl      (-1.000)
                      + u19(b, j, i, n, c, k)*t2a(d, a, n, l) & !bjinckdanl      (+1.000)
                      - u19(a, j, i, n, c, k)*t2a(d, b, n, l) & !ajinckdbnl      (-1.000)
                      - u19(c, j, i, n, b, k)*t2a(d, a, n, l) & !cjinbkdanl      (-1.000)
                      + u19(c, j, i, n, a, k)*t2a(d, b, n, l) & !cjinakdbnl      (+1.000)
                      + u19(a, j, i, n, b, k)*t2a(d, c, n, l) & !ajinbkdcnl      (+1.000)
                      - u19(b, j, i, n, a, k)*t2a(d, c, n, l) & !bjinakdcnl      (-1.000)
                      - u19(c, j, i, n, d, l)*t2a(b, a, n, k) & !cjindlbank      (-1.000)
                      + u19(b, j, i, n, d, l)*t2a(c, a, n, k) & !bjindlcank      (+1.000)
                      - u19(a, j, i, n, d, l)*t2a(c, b, n, k) & !ajindlcbnk      (-1.000)
                      + u19(d, j, i, n, c, l)*t2a(b, a, n, k) & !djinclbank      (+1.000)
                      - u19(d, j, i, n, b, l)*t2a(c, a, n, k) & !djinblcank      (-1.000)
                      + u19(d, j, i, n, a, l)*t2a(c, b, n, k) & !djinalcbnk      (+1.000)
                      - u19(b, j, i, n, c, l)*t2a(d, a, n, k) & !bjincldank      (-1.000)
                      + u19(a, j, i, n, c, l)*t2a(d, b, n, k) & !ajincldbnk      (+1.000)
                      + u19(c, j, i, n, b, l)*t2a(d, a, n, k) & !cjinbldank      (+1.000)
                      - u19(c, j, i, n, a, l)*t2a(d, b, n, k) & !cjinaldbnk      (-1.000)
                      - u19(a, j, i, n, b, l)*t2a(d, c, n, k) & !ajinbldcnk      (-1.000)
                      + u19(b, j, i, n, a, l)*t2a(d, c, n, k) & !bjinaldcnk      (+1.000)
                      - u19(c, l, i, n, d, k)*t2a(b, a, n, j) & !clindkbanj      (-1.000)
                      + u19(b, l, i, n, d, k)*t2a(c, a, n, j) & !blindkcanj      (+1.000)
                      - u19(a, l, i, n, d, k)*t2a(c, b, n, j) & !alindkcbnj      (-1.000)
                      + u19(d, l, i, n, c, k)*t2a(b, a, n, j) & !dlinckbanj      (+1.000)
                      - u19(d, l, i, n, b, k)*t2a(c, a, n, j) & !dlinbkcanj      (-1.000)
                      + u19(d, l, i, n, a, k)*t2a(c, b, n, j) & !dlinakcbnj      (+1.000)
                      - u19(b, l, i, n, c, k)*t2a(d, a, n, j) & !blinckdanj      (-1.000)
                      + u19(a, l, i, n, c, k)*t2a(d, b, n, j) & !alinckdbnj      (+1.000)
                      + u19(c, l, i, n, b, k)*t2a(d, a, n, j) & !clinbkdanj      (+1.000)
                      - u19(c, l, i, n, a, k)*t2a(d, b, n, j) & !clinakdbnj      (-1.000)
                      - u19(a, l, i, n, b, k)*t2a(d, c, n, j) & !alinbkdcnj      (-1.000)
                      + u19(b, l, i, n, a, k)*t2a(d, c, n, j) & !blinakdcnj      (+1.000)
                      + u19(a, l, j, n, d, k)*t2a(c, b, n, i) & !aljndkcbni      (+1.000)
                      - u19(b, l, j, n, d, k)*t2a(c, a, n, i) & !bljndkcani      (-1.000)
                      + u19(c, l, j, n, d, k)*t2a(b, a, n, i) & !cljndkbani      (+1.000)
                      - u19(a, l, j, n, c, k)*t2a(d, b, n, i) & !aljnckdbni      (-1.000)
                      + u19(b, l, j, n, c, k)*t2a(d, a, n, i) & !bljnckdani      (+1.000)
                      + u19(a, l, j, n, b, k)*t2a(d, c, n, i) & !aljnbkdcni      (+1.000)
                      - u19(b, l, j, n, a, k)*t2a(d, c, n, i) & !bljnakdcni      (-1.000)
                      - u19(c, l, j, n, b, k)*t2a(d, a, n, i) & !cljnbkdani      (-1.000)
                      + u19(c, l, j, n, a, k)*t2a(d, b, n, i) & !cljnakdbni      (+1.000)
                      - u19(d, l, j, n, c, k)*t2a(b, a, n, i) & !dljnckbani      (-1.000)
                      + u19(d, l, j, n, b, k)*t2a(c, a, n, i) & !dljnbkcani      (+1.000)
                      - u19(d, l, j, n, a, k)*t2a(c, b, n, i) & !dljnakcbni      (-1.000)
                      + u19(c, k, i, n, d, l)*t2a(b, a, n, j) & !ckindlbanj      (+1.000)
                      - u19(b, k, i, n, d, l)*t2a(c, a, n, j) & !bkindlcanj      (-1.000)
                      + u19(a, k, i, n, d, l)*t2a(c, b, n, j) & !akindlcbnj      (+1.000)
                      - u19(d, k, i, n, c, l)*t2a(b, a, n, j) & !dkinclbanj      (-1.000)
                      + u19(d, k, i, n, b, l)*t2a(c, a, n, j) & !dkinblcanj      (+1.000)
                      - u19(d, k, i, n, a, l)*t2a(c, b, n, j) & !dkinalcbnj      (-1.000)
                      + u19(b, k, i, n, c, l)*t2a(d, a, n, j) & !bkincldanj      (+1.000)
                      - u19(a, k, i, n, c, l)*t2a(d, b, n, j) & !akincldbnj      (-1.000)
                      - u19(c, k, i, n, b, l)*t2a(d, a, n, j) & !ckinbldanj      (-1.000)
                      + u19(c, k, i, n, a, l)*t2a(d, b, n, j) & !ckinaldbnj      (+1.000)
                      + u19(a, k, i, n, b, l)*t2a(d, c, n, j) & !akinbldcnj      (+1.000)
                      - u19(b, k, i, n, a, l)*t2a(d, c, n, j) & !bkinaldcnj      (-1.000)
                      - u19(a, k, j, n, d, l)*t2a(c, b, n, i) & !akjndlcbni      (-1.000)
                      + u19(b, k, j, n, d, l)*t2a(c, a, n, i) & !bkjndlcani      (+1.000)
                      - u19(c, k, j, n, d, l)*t2a(b, a, n, i) & !ckjndlbani      (-1.000)
                      + u19(a, k, j, n, c, l)*t2a(d, b, n, i) & !akjncldbni      (+1.000)
                      - u19(b, k, j, n, c, l)*t2a(d, a, n, i) & !bkjncldani      (-1.000)
                      - u19(a, k, j, n, b, l)*t2a(d, c, n, i) & !akjnbldcni      (-1.000)
                      + u19(b, k, j, n, a, l)*t2a(d, c, n, i) & !bkjnaldcni      (+1.000)
                      + u19(c, k, j, n, b, l)*t2a(d, a, n, i) & !ckjnbldani      (+1.000)
                      - u19(c, k, j, n, a, l)*t2a(d, b, n, i) & !ckjnaldbni      (-1.000)
                      + u19(d, k, j, n, c, l)*t2a(b, a, n, i) & !dkjnclbani      (+1.000)
                      - u19(d, k, j, n, b, l)*t2a(c, a, n, i) & !dkjnblcani      (-1.000)
                      + u19(d, k, j, n, a, l)*t2a(c, b, n, i)       !dkjnalcbni      (+1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u19)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s6), size(s6), '4213', s6, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s30(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s30)
    deallocate (d1)
    deallocate (b2)
    deallocate (s6)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s30)
    deallocate (s30)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q3(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q3)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(2, shape(x3), size(x3), '21', -1.000, x3, &
                    q3)
    deallocate (q3)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s7(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s7)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '4231', 1.000, x6, &
                    s7)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s7), size(s7), '2431', s7, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u20(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u20)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u20) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - u20(c, k, j, m, d, i)*t2a(b, a, m, l) & !ckjmdibaml      (-1.000)
                      + u20(b, k, j, m, d, i)*t2a(c, a, m, l) & !bkjmdicaml      (+1.000)
                      - u20(a, k, j, m, d, i)*t2a(c, b, m, l) & !akjmdicbml      (-1.000)
                      + u20(d, k, j, m, c, i)*t2a(b, a, m, l) & !dkjmcibaml      (+1.000)
                      - u20(d, k, j, m, b, i)*t2a(c, a, m, l) & !dkjmbicaml      (-1.000)
                      + u20(d, k, j, m, a, i)*t2a(c, b, m, l) & !dkjmaicbml      (+1.000)
                      - u20(b, k, j, m, c, i)*t2a(d, a, m, l) & !bkjmcidaml      (-1.000)
                      + u20(a, k, j, m, c, i)*t2a(d, b, m, l) & !akjmcidbml      (+1.000)
                      + u20(c, k, j, m, b, i)*t2a(d, a, m, l) & !ckjmbidaml      (+1.000)
                      - u20(c, k, j, m, a, i)*t2a(d, b, m, l) & !ckjmaidbml      (-1.000)
                      - u20(a, k, j, m, b, i)*t2a(d, c, m, l) & !akjmbidcml      (-1.000)
                      + u20(b, k, j, m, a, i)*t2a(d, c, m, l) & !bkjmaidcml      (+1.000)
                      + u20(c, l, j, m, d, i)*t2a(b, a, m, k) & !cljmdibamk      (+1.000)
                      - u20(b, l, j, m, d, i)*t2a(c, a, m, k) & !bljmdicamk      (-1.000)
                      + u20(a, l, j, m, d, i)*t2a(c, b, m, k) & !aljmdicbmk      (+1.000)
                      - u20(d, l, j, m, c, i)*t2a(b, a, m, k) & !dljmcibamk      (-1.000)
                      + u20(d, l, j, m, b, i)*t2a(c, a, m, k) & !dljmbicamk      (+1.000)
                      - u20(d, l, j, m, a, i)*t2a(c, b, m, k) & !dljmaicbmk      (-1.000)
                      + u20(b, l, j, m, c, i)*t2a(d, a, m, k) & !bljmcidamk      (+1.000)
                      - u20(a, l, j, m, c, i)*t2a(d, b, m, k) & !aljmcidbmk      (-1.000)
                      - u20(c, l, j, m, b, i)*t2a(d, a, m, k) & !cljmbidamk      (-1.000)
                      + u20(c, l, j, m, a, i)*t2a(d, b, m, k) & !cljmaidbmk      (+1.000)
                      + u20(a, l, j, m, b, i)*t2a(d, c, m, k) & !aljmbidcmk      (+1.000)
                      - u20(b, l, j, m, a, i)*t2a(d, c, m, k) & !bljmaidcmk      (-1.000)
                      - u20(a, l, k, m, d, i)*t2a(c, b, m, j) & !alkmdicbmj      (-1.000)
                      + u20(b, l, k, m, d, i)*t2a(c, a, m, j) & !blkmdicamj      (+1.000)
                      - u20(c, l, k, m, d, i)*t2a(b, a, m, j) & !clkmdibamj      (-1.000)
                      + u20(a, l, k, m, c, i)*t2a(d, b, m, j) & !alkmcidbmj      (+1.000)
                      - u20(b, l, k, m, c, i)*t2a(d, a, m, j) & !blkmcidamj      (-1.000)
                      - u20(a, l, k, m, b, i)*t2a(d, c, m, j) & !alkmbidcmj      (-1.000)
                      + u20(b, l, k, m, a, i)*t2a(d, c, m, j) & !blkmaidcmj      (+1.000)
                      + u20(c, l, k, m, b, i)*t2a(d, a, m, j) & !clkmbidamj      (+1.000)
                      - u20(c, l, k, m, a, i)*t2a(d, b, m, j) & !clkmaidbmj      (-1.000)
                      + u20(d, l, k, m, c, i)*t2a(b, a, m, j) & !dlkmcibamj      (+1.000)
                      - u20(d, l, k, m, b, i)*t2a(c, a, m, j) & !dlkmbicamj      (-1.000)
                      + u20(d, l, k, m, a, i)*t2a(c, b, m, j) & !dlkmaicbmj      (+1.000)
                      + u20(c, k, i, m, d, j)*t2a(b, a, m, l) & !ckimdjbaml      (+1.000)
                      - u20(b, k, i, m, d, j)*t2a(c, a, m, l) & !bkimdjcaml      (-1.000)
                      + u20(a, k, i, m, d, j)*t2a(c, b, m, l) & !akimdjcbml      (+1.000)
                      - u20(d, k, i, m, c, j)*t2a(b, a, m, l) & !dkimcjbaml      (-1.000)
                      + u20(d, k, i, m, b, j)*t2a(c, a, m, l) & !dkimbjcaml      (+1.000)
                      - u20(d, k, i, m, a, j)*t2a(c, b, m, l) & !dkimajcbml      (-1.000)
                      + u20(b, k, i, m, c, j)*t2a(d, a, m, l) & !bkimcjdaml      (+1.000)
                      - u20(a, k, i, m, c, j)*t2a(d, b, m, l) & !akimcjdbml      (-1.000)
                      - u20(c, k, i, m, b, j)*t2a(d, a, m, l) & !ckimbjdaml      (-1.000)
                      + u20(c, k, i, m, a, j)*t2a(d, b, m, l) & !ckimajdbml      (+1.000)
                      + u20(a, k, i, m, b, j)*t2a(d, c, m, l) & !akimbjdcml      (+1.000)
                      - u20(b, k, i, m, a, j)*t2a(d, c, m, l) & !bkimajdcml      (-1.000)
                      - u20(c, l, i, m, d, j)*t2a(b, a, m, k) & !climdjbamk      (-1.000)
                      + u20(b, l, i, m, d, j)*t2a(c, a, m, k) & !blimdjcamk      (+1.000)
                      - u20(a, l, i, m, d, j)*t2a(c, b, m, k) & !alimdjcbmk      (-1.000)
                      + u20(d, l, i, m, c, j)*t2a(b, a, m, k) & !dlimcjbamk      (+1.000)
                      - u20(d, l, i, m, b, j)*t2a(c, a, m, k) & !dlimbjcamk      (-1.000)
                      + u20(d, l, i, m, a, j)*t2a(c, b, m, k) & !dlimajcbmk      (+1.000)
                      - u20(b, l, i, m, c, j)*t2a(d, a, m, k) & !blimcjdamk      (-1.000)
                      + u20(a, l, i, m, c, j)*t2a(d, b, m, k) & !alimcjdbmk      (+1.000)
                      + u20(c, l, i, m, b, j)*t2a(d, a, m, k) & !climbjdamk      (+1.000)
                      - u20(c, l, i, m, a, j)*t2a(d, b, m, k) & !climajdbmk      (-1.000)
                      - u20(a, l, i, m, b, j)*t2a(d, c, m, k) & !alimbjdcmk      (-1.000)
                      + u20(b, l, i, m, a, j)*t2a(d, c, m, k) & !blimajdcmk      (+1.000)
                      + u20(a, l, k, m, d, j)*t2a(c, b, m, i) & !alkmdjcbmi      (+1.000)
                      - u20(b, l, k, m, d, j)*t2a(c, a, m, i) & !blkmdjcami      (-1.000)
                      + u20(c, l, k, m, d, j)*t2a(b, a, m, i) & !clkmdjbami      (+1.000)
                      - u20(a, l, k, m, c, j)*t2a(d, b, m, i) & !alkmcjdbmi      (-1.000)
                      + u20(b, l, k, m, c, j)*t2a(d, a, m, i) & !blkmcjdami      (+1.000)
                      + u20(a, l, k, m, b, j)*t2a(d, c, m, i) & !alkmbjdcmi      (+1.000)
                      - u20(b, l, k, m, a, j)*t2a(d, c, m, i) & !blkmajdcmi      (-1.000)
                      - u20(c, l, k, m, b, j)*t2a(d, a, m, i) & !clkmbjdami      (-1.000)
                      + u20(c, l, k, m, a, j)*t2a(d, b, m, i) & !clkmajdbmi      (+1.000)
                      - u20(d, l, k, m, c, j)*t2a(b, a, m, i) & !dlkmcjbami      (-1.000)
                      + u20(d, l, k, m, b, j)*t2a(c, a, m, i) & !dlkmbjcami      (+1.000)
                      - u20(d, l, k, m, a, j)*t2a(c, b, m, i) & !dlkmajcbmi      (-1.000)
                      - u20(c, j, i, m, d, k)*t2a(b, a, m, l) & !cjimdkbaml      (-1.000)
                      + u20(b, j, i, m, d, k)*t2a(c, a, m, l) & !bjimdkcaml      (+1.000)
                      - u20(a, j, i, m, d, k)*t2a(c, b, m, l) & !ajimdkcbml      (-1.000)
                      + u20(d, j, i, m, c, k)*t2a(b, a, m, l) & !djimckbaml      (+1.000)
                      - u20(d, j, i, m, b, k)*t2a(c, a, m, l) & !djimbkcaml      (-1.000)
                      + u20(d, j, i, m, a, k)*t2a(c, b, m, l) & !djimakcbml      (+1.000)
                      - u20(b, j, i, m, c, k)*t2a(d, a, m, l) & !bjimckdaml      (-1.000)
                      + u20(a, j, i, m, c, k)*t2a(d, b, m, l) & !ajimckdbml      (+1.000)
                      + u20(c, j, i, m, b, k)*t2a(d, a, m, l) & !cjimbkdaml      (+1.000)
                      - u20(c, j, i, m, a, k)*t2a(d, b, m, l) & !cjimakdbml      (-1.000)
                      - u20(a, j, i, m, b, k)*t2a(d, c, m, l) & !ajimbkdcml      (-1.000)
                      + u20(b, j, i, m, a, k)*t2a(d, c, m, l) & !bjimakdcml      (+1.000)
                      + u20(c, j, i, m, d, l)*t2a(b, a, m, k) & !cjimdlbamk      (+1.000)
                      - u20(b, j, i, m, d, l)*t2a(c, a, m, k) & !bjimdlcamk      (-1.000)
                      + u20(a, j, i, m, d, l)*t2a(c, b, m, k) & !ajimdlcbmk      (+1.000)
                      - u20(d, j, i, m, c, l)*t2a(b, a, m, k) & !djimclbamk      (-1.000)
                      + u20(d, j, i, m, b, l)*t2a(c, a, m, k) & !djimblcamk      (+1.000)
                      - u20(d, j, i, m, a, l)*t2a(c, b, m, k) & !djimalcbmk      (-1.000)
                      + u20(b, j, i, m, c, l)*t2a(d, a, m, k) & !bjimcldamk      (+1.000)
                      - u20(a, j, i, m, c, l)*t2a(d, b, m, k) & !ajimcldbmk      (-1.000)
                      - u20(c, j, i, m, b, l)*t2a(d, a, m, k) & !cjimbldamk      (-1.000)
                      + u20(c, j, i, m, a, l)*t2a(d, b, m, k) & !cjimaldbmk      (+1.000)
                      + u20(a, j, i, m, b, l)*t2a(d, c, m, k) & !ajimbldcmk      (+1.000)
                      - u20(b, j, i, m, a, l)*t2a(d, c, m, k) & !bjimaldcmk      (-1.000)
                      + u20(c, l, i, m, d, k)*t2a(b, a, m, j) & !climdkbamj      (+1.000)
                      - u20(b, l, i, m, d, k)*t2a(c, a, m, j) & !blimdkcamj      (-1.000)
                      + u20(a, l, i, m, d, k)*t2a(c, b, m, j) & !alimdkcbmj      (+1.000)
                      - u20(d, l, i, m, c, k)*t2a(b, a, m, j) & !dlimckbamj      (-1.000)
                      + u20(d, l, i, m, b, k)*t2a(c, a, m, j) & !dlimbkcamj      (+1.000)
                      - u20(d, l, i, m, a, k)*t2a(c, b, m, j) & !dlimakcbmj      (-1.000)
                      + u20(b, l, i, m, c, k)*t2a(d, a, m, j) & !blimckdamj      (+1.000)
                      - u20(a, l, i, m, c, k)*t2a(d, b, m, j) & !alimckdbmj      (-1.000)
                      - u20(c, l, i, m, b, k)*t2a(d, a, m, j) & !climbkdamj      (-1.000)
                      + u20(c, l, i, m, a, k)*t2a(d, b, m, j) & !climakdbmj      (+1.000)
                      + u20(a, l, i, m, b, k)*t2a(d, c, m, j) & !alimbkdcmj      (+1.000)
                      - u20(b, l, i, m, a, k)*t2a(d, c, m, j) & !blimakdcmj      (-1.000)
                      - u20(a, l, j, m, d, k)*t2a(c, b, m, i) & !aljmdkcbmi      (-1.000)
                      + u20(b, l, j, m, d, k)*t2a(c, a, m, i) & !bljmdkcami      (+1.000)
                      - u20(c, l, j, m, d, k)*t2a(b, a, m, i) & !cljmdkbami      (-1.000)
                      + u20(a, l, j, m, c, k)*t2a(d, b, m, i) & !aljmckdbmi      (+1.000)
                      - u20(b, l, j, m, c, k)*t2a(d, a, m, i) & !bljmckdami      (-1.000)
                      - u20(a, l, j, m, b, k)*t2a(d, c, m, i) & !aljmbkdcmi      (-1.000)
                      + u20(b, l, j, m, a, k)*t2a(d, c, m, i) & !bljmakdcmi      (+1.000)
                      + u20(c, l, j, m, b, k)*t2a(d, a, m, i) & !cljmbkdami      (+1.000)
                      - u20(c, l, j, m, a, k)*t2a(d, b, m, i) & !cljmakdbmi      (-1.000)
                      + u20(d, l, j, m, c, k)*t2a(b, a, m, i) & !dljmckbami      (+1.000)
                      - u20(d, l, j, m, b, k)*t2a(c, a, m, i) & !dljmbkcami      (-1.000)
                      + u20(d, l, j, m, a, k)*t2a(c, b, m, i) & !dljmakcbmi      (+1.000)
                      - u20(c, k, i, m, d, l)*t2a(b, a, m, j) & !ckimdlbamj      (-1.000)
                      + u20(b, k, i, m, d, l)*t2a(c, a, m, j) & !bkimdlcamj      (+1.000)
                      - u20(a, k, i, m, d, l)*t2a(c, b, m, j) & !akimdlcbmj      (-1.000)
                      + u20(d, k, i, m, c, l)*t2a(b, a, m, j) & !dkimclbamj      (+1.000)
                      - u20(d, k, i, m, b, l)*t2a(c, a, m, j) & !dkimblcamj      (-1.000)
                      + u20(d, k, i, m, a, l)*t2a(c, b, m, j) & !dkimalcbmj      (+1.000)
                      - u20(b, k, i, m, c, l)*t2a(d, a, m, j) & !bkimcldamj      (-1.000)
                      + u20(a, k, i, m, c, l)*t2a(d, b, m, j) & !akimcldbmj      (+1.000)
                      + u20(c, k, i, m, b, l)*t2a(d, a, m, j) & !ckimbldamj      (+1.000)
                      - u20(c, k, i, m, a, l)*t2a(d, b, m, j) & !ckimaldbmj      (-1.000)
                      - u20(a, k, i, m, b, l)*t2a(d, c, m, j) & !akimbldcmj      (-1.000)
                      + u20(b, k, i, m, a, l)*t2a(d, c, m, j) & !bkimaldcmj      (+1.000)
                      + u20(a, k, j, m, d, l)*t2a(c, b, m, i) & !akjmdlcbmi      (+1.000)
                      - u20(b, k, j, m, d, l)*t2a(c, a, m, i) & !bkjmdlcami      (-1.000)
                      + u20(c, k, j, m, d, l)*t2a(b, a, m, i) & !ckjmdlbami      (+1.000)
                      - u20(a, k, j, m, c, l)*t2a(d, b, m, i) & !akjmcldbmi      (-1.000)
                      + u20(b, k, j, m, c, l)*t2a(d, a, m, i) & !bkjmcldami      (+1.000)
                      + u20(a, k, j, m, b, l)*t2a(d, c, m, i) & !akjmbldcmi      (+1.000)
                      - u20(b, k, j, m, a, l)*t2a(d, c, m, i) & !bkjmaldcmi      (-1.000)
                      - u20(c, k, j, m, b, l)*t2a(d, a, m, i) & !ckjmbldami      (-1.000)
                      + u20(c, k, j, m, a, l)*t2a(d, b, m, i) & !ckjmaldbmi      (+1.000)
                      - u20(d, k, j, m, c, l)*t2a(b, a, m, i) & !dkjmclbami      (-1.000)
                      + u20(d, k, j, m, b, l)*t2a(c, a, m, i) & !dkjmblcami      (+1.000)
                      - u20(d, k, j, m, a, l)*t2a(c, b, m, i)       !dkjmalcbmi      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u20)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s7), size(s7), '4231', s7, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s32(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s32)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x10), size(x10), '3124', 1.000, &
                    x10, s32)
    deallocate (s32)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s7), size(s7), '2431', s7, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s31(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s31)
    deallocate (d1)
    deallocate (b2)
    deallocate (s7)

    call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                    s31)
    deallocate (s31)

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

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,s8,t4a) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do f = n1 + 1, n3
                sum = sum &
                      + (s8(c, f, e, d)*t4a(f, e, b, a, l, k, j, i) & !cfedfebalkji    (+0.500)
                         - s8(b, f, e, d)*t4a(f, e, c, a, l, k, j, i) & !bfedfecalkji    (-0.500)
                         + s8(a, f, e, d)*t4a(f, e, c, b, l, k, j, i) & !afedfecblkji    (+0.500)
                         - s8(d, f, e, c)*t4a(f, e, b, a, l, k, j, i) & !dfecfebalkji    (-0.500)
                         + s8(d, f, e, b)*t4a(f, e, c, a, l, k, j, i) & !dfebfecalkji    (+0.500)
                         - s8(d, f, e, a)*t4a(f, e, c, b, l, k, j, i) & !dfeafecblkji    (-0.500)
                         + s8(b, f, e, c)*t4a(f, e, d, a, l, k, j, i) & !bfecfedalkji    (+0.500)
                         - s8(a, f, e, c)*t4a(f, e, d, b, l, k, j, i) & !afecfedblkji    (-0.500)
                         - s8(c, f, e, b)*t4a(f, e, d, a, l, k, j, i) & !cfebfedalkji    (-0.500)
                         + s8(c, f, e, a)*t4a(f, e, d, b, l, k, j, i) & !cfeafedblkji    (+0.500)
                         + s8(a, f, e, b)*t4a(f, e, d, c, l, k, j, i) & !afebfedclkji    (+0.500)
                         - s8(b, f, e, a)*t4a(f, e, d, c, l, k, j, i))/2.0d0!bfeafedclkji    (-0.500)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s8), size(s8), '3241', s8, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u21(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u21)
    deallocate (d1)
    deallocate (d2)
    deallocate (s8)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u21) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum &
                      - u21(a, l, k, f, d, c)*t2a(f, b, j, i) & !alkfdcfbji      (-1.000)
                      - u21(a, j, i, f, d, c)*t2a(f, b, l, k) & !ajifdcfblk      (-1.000)
                      + u21(a, l, k, f, d, b)*t2a(f, c, j, i) & !alkfdbfcji      (+1.000)
                      - u21(b, l, k, f, d, a)*t2a(f, c, j, i) & !blkfdafcji      (-1.000)
                      + u21(a, j, i, f, d, b)*t2a(f, c, l, k) & !ajifdbfclk      (+1.000)
                      - u21(b, j, i, f, d, a)*t2a(f, c, l, k) & !bjifdafclk      (-1.000)
                      + u21(a, l, k, f, c, d)*t2a(f, b, j, i) & !alkfcdfbji      (+1.000)
                      + u21(a, j, i, f, c, d)*t2a(f, b, l, k) & !ajifcdfblk      (+1.000)
                      - u21(a, l, k, f, b, d)*t2a(f, c, j, i) & !alkfbdfcji      (-1.000)
                      + u21(b, l, k, f, a, d)*t2a(f, c, j, i) & !blkfadfcji      (+1.000)
                      - u21(a, j, i, f, b, d)*t2a(f, c, l, k) & !ajifbdfclk      (-1.000)
                      + u21(b, j, i, f, a, d)*t2a(f, c, l, k) & !bjifadfclk      (+1.000)
                      - u21(a, l, k, f, c, b)*t2a(f, d, j, i) & !alkfcbfdji      (-1.000)
                      + u21(b, l, k, f, c, a)*t2a(f, d, j, i) & !blkfcafdji      (+1.000)
                      + u21(a, l, k, f, b, c)*t2a(f, d, j, i) & !alkfbcfdji      (+1.000)
                      - u21(b, l, k, f, a, c)*t2a(f, d, j, i) & !blkfacfdji      (-1.000)
                      - u21(c, l, k, f, b, a)*t2a(f, d, j, i) & !clkfbafdji      (-1.000)
                      + u21(c, l, k, f, a, b)*t2a(f, d, j, i) & !clkfabfdji      (+1.000)
                      - u21(a, j, i, f, c, b)*t2a(f, d, l, k) & !ajifcbfdlk      (-1.000)
                      + u21(b, j, i, f, c, a)*t2a(f, d, l, k) & !bjifcafdlk      (+1.000)
                      + u21(a, j, i, f, b, c)*t2a(f, d, l, k) & !ajifbcfdlk      (+1.000)
                      - u21(b, j, i, f, a, c)*t2a(f, d, l, k) & !bjifacfdlk      (-1.000)
                      - u21(c, j, i, f, b, a)*t2a(f, d, l, k) & !cjifbafdlk      (-1.000)
                      + u21(c, j, i, f, a, b)*t2a(f, d, l, k) & !cjifabfdlk      (+1.000)
                      + u21(a, l, j, f, d, c)*t2a(f, b, k, i) & !aljfdcfbki      (+1.000)
                      + u21(a, k, i, f, d, c)*t2a(f, b, l, j) & !akifdcfblj      (+1.000)
                      - u21(a, l, j, f, d, b)*t2a(f, c, k, i) & !aljfdbfcki      (-1.000)
                      + u21(b, l, j, f, d, a)*t2a(f, c, k, i) & !bljfdafcki      (+1.000)
                      - u21(a, k, i, f, d, b)*t2a(f, c, l, j) & !akifdbfclj      (-1.000)
                      + u21(b, k, i, f, d, a)*t2a(f, c, l, j) & !bkifdafclj      (+1.000)
                      - u21(a, l, j, f, c, d)*t2a(f, b, k, i) & !aljfcdfbki      (-1.000)
                      - u21(a, k, i, f, c, d)*t2a(f, b, l, j) & !akifcdfblj      (-1.000)
                      + u21(a, l, j, f, b, d)*t2a(f, c, k, i) & !aljfbdfcki      (+1.000)
                      - u21(b, l, j, f, a, d)*t2a(f, c, k, i) & !bljfadfcki      (-1.000)
                      + u21(a, k, i, f, b, d)*t2a(f, c, l, j) & !akifbdfclj      (+1.000)
                      - u21(b, k, i, f, a, d)*t2a(f, c, l, j) & !bkifadfclj      (-1.000)
                      + u21(a, l, j, f, c, b)*t2a(f, d, k, i) & !aljfcbfdki      (+1.000)
                      - u21(b, l, j, f, c, a)*t2a(f, d, k, i) & !bljfcafdki      (-1.000)
                      - u21(a, l, j, f, b, c)*t2a(f, d, k, i) & !aljfbcfdki      (-1.000)
                      + u21(b, l, j, f, a, c)*t2a(f, d, k, i) & !bljfacfdki      (+1.000)
                      + u21(c, l, j, f, b, a)*t2a(f, d, k, i) & !cljfbafdki      (+1.000)
                      - u21(c, l, j, f, a, b)*t2a(f, d, k, i) & !cljfabfdki      (-1.000)
                      + u21(a, k, i, f, c, b)*t2a(f, d, l, j) & !akifcbfdlj      (+1.000)
                      - u21(b, k, i, f, c, a)*t2a(f, d, l, j) & !bkifcafdlj      (-1.000)
                      - u21(a, k, i, f, b, c)*t2a(f, d, l, j) & !akifbcfdlj      (-1.000)
                      + u21(b, k, i, f, a, c)*t2a(f, d, l, j) & !bkifacfdlj      (+1.000)
                      + u21(c, k, i, f, b, a)*t2a(f, d, l, j) & !ckifbafdlj      (+1.000)
                      - u21(c, k, i, f, a, b)*t2a(f, d, l, j) & !ckifabfdlj      (-1.000)
                      - u21(a, k, j, f, d, c)*t2a(f, b, l, i) & !akjfdcfbli      (-1.000)
                      - u21(a, l, i, f, d, c)*t2a(f, b, k, j) & !alifdcfbkj      (-1.000)
                      + u21(a, k, j, f, d, b)*t2a(f, c, l, i) & !akjfdbfcli      (+1.000)
                      - u21(b, k, j, f, d, a)*t2a(f, c, l, i) & !bkjfdafcli      (-1.000)
                      + u21(a, l, i, f, d, b)*t2a(f, c, k, j) & !alifdbfckj      (+1.000)
                      - u21(b, l, i, f, d, a)*t2a(f, c, k, j) & !blifdafckj      (-1.000)
                      + u21(a, k, j, f, c, d)*t2a(f, b, l, i) & !akjfcdfbli      (+1.000)
                      + u21(a, l, i, f, c, d)*t2a(f, b, k, j) & !alifcdfbkj      (+1.000)
                      - u21(a, k, j, f, b, d)*t2a(f, c, l, i) & !akjfbdfcli      (-1.000)
                      + u21(b, k, j, f, a, d)*t2a(f, c, l, i) & !bkjfadfcli      (+1.000)
                      - u21(a, l, i, f, b, d)*t2a(f, c, k, j) & !alifbdfckj      (-1.000)
                      + u21(b, l, i, f, a, d)*t2a(f, c, k, j) & !blifadfckj      (+1.000)
                      - u21(a, k, j, f, c, b)*t2a(f, d, l, i) & !akjfcbfdli      (-1.000)
                      + u21(b, k, j, f, c, a)*t2a(f, d, l, i) & !bkjfcafdli      (+1.000)
                      + u21(a, k, j, f, b, c)*t2a(f, d, l, i) & !akjfbcfdli      (+1.000)
                      - u21(b, k, j, f, a, c)*t2a(f, d, l, i) & !bkjfacfdli      (-1.000)
                      - u21(c, k, j, f, b, a)*t2a(f, d, l, i) & !ckjfbafdli      (-1.000)
                      + u21(c, k, j, f, a, b)*t2a(f, d, l, i) & !ckjfabfdli      (+1.000)
                      - u21(a, l, i, f, c, b)*t2a(f, d, k, j) & !alifcbfdkj      (-1.000)
                      + u21(b, l, i, f, c, a)*t2a(f, d, k, j) & !blifcafdkj      (+1.000)
                      + u21(a, l, i, f, b, c)*t2a(f, d, k, j) & !alifbcfdkj      (+1.000)
                      - u21(b, l, i, f, a, c)*t2a(f, d, k, j) & !blifacfdkj      (-1.000)
                      - u21(c, l, i, f, b, a)*t2a(f, d, k, j) & !clifbafdkj      (-1.000)
                      + u21(c, l, i, f, a, b)*t2a(f, d, k, j)       !clifabfdkj      (+1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u21)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q4(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k1*k3
    call egemm1(i1, i3, d1, b2, q4)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 - q4
    deallocate (q4)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s9(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s9)
    deallocate (d1)
    deallocate (b2)

    allocate (x8(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x8 = 0.0d0
    call sum_stripe(4, shape(x8), size(x8), '3241', -1.000, &
                    x8, s9)
    deallocate (s9)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s10(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s10)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '4231', 1.000, x8, &
                    s10)
    deallocate (s10)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q5(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q5)
    deallocate (d1)
    deallocate (b2)

    x3 = x3 + q5
    deallocate (q5)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q6(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q6)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 + q6
    deallocate (q6)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intr, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u1)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u1) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + u1(b, a, l, n, j, i)*t2a(d, c, n, k) & !balnjidcnk      (+1.000)
                      - u1(c, a, l, n, j, i)*t2a(d, b, n, k) & !calnjidbnk      (-1.000)
                      - u1(d, a, k, n, j, i)*t2a(c, b, n, l) & !daknjicbnl      (-1.000)
                      + u1(d, a, l, n, j, i)*t2a(c, b, n, k) & !dalnjicbnk      (+1.000)
                      + u1(c, a, k, n, j, i)*t2a(d, b, n, l) & !caknjidbnl      (+1.000)
                      - u1(b, a, k, n, j, i)*t2a(d, c, n, l) & !baknjidcnl      (-1.000)
                      - u1(b, a, l, n, k, i)*t2a(d, c, n, j) & !balnkidcnj      (-1.000)
                      + u1(c, a, l, n, k, i)*t2a(d, b, n, j) & !calnkidbnj      (+1.000)
                      + u1(d, a, j, n, k, i)*t2a(c, b, n, l) & !dajnkicbnl      (+1.000)
                      - u1(d, a, l, n, k, i)*t2a(c, b, n, j) & !dalnkicbnj      (-1.000)
                      - u1(c, a, j, n, k, i)*t2a(d, b, n, l) & !cajnkidbnl      (-1.000)
                      + u1(b, a, j, n, k, i)*t2a(d, c, n, l) & !bajnkidcnl      (+1.000)
                      + u1(b, a, k, n, l, i)*t2a(d, c, n, j) & !baknlidcnj      (+1.000)
                      - u1(c, a, k, n, l, i)*t2a(d, b, n, j) & !caknlidbnj      (-1.000)
                      - u1(d, a, j, n, l, i)*t2a(c, b, n, k) & !dajnlicbnk      (-1.000)
                      + u1(d, a, k, n, l, i)*t2a(c, b, n, j) & !daknlicbnj      (+1.000)
                      + u1(c, a, j, n, l, i)*t2a(d, b, n, k) & !cajnlidbnk      (+1.000)
                      - u1(b, a, j, n, l, i)*t2a(d, c, n, k) & !bajnlidcnk      (-1.000)
                      + u1(b, a, l, n, k, j)*t2a(d, c, n, i) & !balnkjdcni      (+1.000)
                      - u1(c, a, l, n, k, j)*t2a(d, b, n, i) & !calnkjdbni      (-1.000)
                      - u1(d, a, i, n, k, j)*t2a(c, b, n, l) & !dainkjcbnl      (-1.000)
                      + u1(d, a, l, n, k, j)*t2a(c, b, n, i) & !dalnkjcbni      (+1.000)
                      + u1(c, a, i, n, k, j)*t2a(d, b, n, l) & !cainkjdbnl      (+1.000)
                      - u1(b, a, i, n, k, j)*t2a(d, c, n, l) & !bainkjdcnl      (-1.000)
                      - u1(b, a, k, n, l, j)*t2a(d, c, n, i) & !baknljdcni      (-1.000)
                      + u1(c, a, k, n, l, j)*t2a(d, b, n, i) & !caknljdbni      (+1.000)
                      + u1(d, a, i, n, l, j)*t2a(c, b, n, k) & !dainljcbnk      (+1.000)
                      - u1(d, a, k, n, l, j)*t2a(c, b, n, i) & !daknljcbni      (-1.000)
                      - u1(c, a, i, n, l, j)*t2a(d, b, n, k) & !cainljdbnk      (-1.000)
                      + u1(b, a, i, n, l, j)*t2a(d, c, n, k) & !bainljdcnk      (+1.000)
                      + u1(b, a, j, n, l, k)*t2a(d, c, n, i) & !bajnlkdcni      (+1.000)
                      - u1(c, a, j, n, l, k)*t2a(d, b, n, i) & !cajnlkdbni      (-1.000)
                      - u1(d, a, i, n, l, k)*t2a(c, b, n, j) & !dainlkcbnj      (-1.000)
                      + u1(d, a, j, n, l, k)*t2a(c, b, n, i) & !dajnlkcbni      (+1.000)
                      + u1(c, a, i, n, l, k)*t2a(d, b, n, j) & !cainlkdbnj      (+1.000)
                      - u1(b, a, i, n, l, k)*t2a(d, c, n, j)       !bainlkdcnj      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u1)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u2)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u2) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - u2(c, k, j, m, d, i)*t2a(b, a, m, l) & !ckjmdibaml      (-1.000)
                      + u2(b, k, j, m, d, i)*t2a(c, a, m, l) & !bkjmdicaml      (+1.000)
                      - u2(a, k, j, m, d, i)*t2a(c, b, m, l) & !akjmdicbml      (-1.000)
                      + u2(d, k, j, m, c, i)*t2a(b, a, m, l) & !dkjmcibaml      (+1.000)
                      - u2(d, k, j, m, b, i)*t2a(c, a, m, l) & !dkjmbicaml      (-1.000)
                      + u2(d, k, j, m, a, i)*t2a(c, b, m, l) & !dkjmaicbml      (+1.000)
                      - u2(b, k, j, m, c, i)*t2a(d, a, m, l) & !bkjmcidaml      (-1.000)
                      + u2(a, k, j, m, c, i)*t2a(d, b, m, l) & !akjmcidbml      (+1.000)
                      + u2(c, k, j, m, b, i)*t2a(d, a, m, l) & !ckjmbidaml      (+1.000)
                      - u2(c, k, j, m, a, i)*t2a(d, b, m, l) & !ckjmaidbml      (-1.000)
                      - u2(a, k, j, m, b, i)*t2a(d, c, m, l) & !akjmbidcml      (-1.000)
                      + u2(b, k, j, m, a, i)*t2a(d, c, m, l) & !bkjmaidcml      (+1.000)
                      + u2(c, l, j, m, d, i)*t2a(b, a, m, k) & !cljmdibamk      (+1.000)
                      - u2(b, l, j, m, d, i)*t2a(c, a, m, k) & !bljmdicamk      (-1.000)
                      + u2(a, l, j, m, d, i)*t2a(c, b, m, k) & !aljmdicbmk      (+1.000)
                      - u2(d, l, j, m, c, i)*t2a(b, a, m, k) & !dljmcibamk      (-1.000)
                      + u2(d, l, j, m, b, i)*t2a(c, a, m, k) & !dljmbicamk      (+1.000)
                      - u2(d, l, j, m, a, i)*t2a(c, b, m, k) & !dljmaicbmk      (-1.000)
                      + u2(b, l, j, m, c, i)*t2a(d, a, m, k) & !bljmcidamk      (+1.000)
                      - u2(a, l, j, m, c, i)*t2a(d, b, m, k) & !aljmcidbmk      (-1.000)
                      - u2(c, l, j, m, b, i)*t2a(d, a, m, k) & !cljmbidamk      (-1.000)
                      + u2(c, l, j, m, a, i)*t2a(d, b, m, k) & !cljmaidbmk      (+1.000)
                      + u2(a, l, j, m, b, i)*t2a(d, c, m, k) & !aljmbidcmk      (+1.000)
                      - u2(b, l, j, m, a, i)*t2a(d, c, m, k) & !bljmaidcmk      (-1.000)
                      - u2(a, l, k, m, d, i)*t2a(c, b, m, j) & !alkmdicbmj      (-1.000)
                      + u2(b, l, k, m, d, i)*t2a(c, a, m, j) & !blkmdicamj      (+1.000)
                      - u2(c, l, k, m, d, i)*t2a(b, a, m, j) & !clkmdibamj      (-1.000)
                      + u2(a, l, k, m, c, i)*t2a(d, b, m, j) & !alkmcidbmj      (+1.000)
                      - u2(b, l, k, m, c, i)*t2a(d, a, m, j) & !blkmcidamj      (-1.000)
                      - u2(a, l, k, m, b, i)*t2a(d, c, m, j) & !alkmbidcmj      (-1.000)
                      + u2(b, l, k, m, a, i)*t2a(d, c, m, j) & !blkmaidcmj      (+1.000)
                      + u2(c, l, k, m, b, i)*t2a(d, a, m, j) & !clkmbidamj      (+1.000)
                      - u2(c, l, k, m, a, i)*t2a(d, b, m, j) & !clkmaidbmj      (-1.000)
                      + u2(d, l, k, m, c, i)*t2a(b, a, m, j) & !dlkmcibamj      (+1.000)
                      - u2(d, l, k, m, b, i)*t2a(c, a, m, j) & !dlkmbicamj      (-1.000)
                      + u2(d, l, k, m, a, i)*t2a(c, b, m, j) & !dlkmaicbmj      (+1.000)
                      + u2(c, k, i, m, d, j)*t2a(b, a, m, l) & !ckimdjbaml      (+1.000)
                      - u2(b, k, i, m, d, j)*t2a(c, a, m, l) & !bkimdjcaml      (-1.000)
                      + u2(a, k, i, m, d, j)*t2a(c, b, m, l) & !akimdjcbml      (+1.000)
                      - u2(d, k, i, m, c, j)*t2a(b, a, m, l) & !dkimcjbaml      (-1.000)
                      + u2(d, k, i, m, b, j)*t2a(c, a, m, l) & !dkimbjcaml      (+1.000)
                      - u2(d, k, i, m, a, j)*t2a(c, b, m, l) & !dkimajcbml      (-1.000)
                      + u2(b, k, i, m, c, j)*t2a(d, a, m, l) & !bkimcjdaml      (+1.000)
                      - u2(a, k, i, m, c, j)*t2a(d, b, m, l) & !akimcjdbml      (-1.000)
                      - u2(c, k, i, m, b, j)*t2a(d, a, m, l) & !ckimbjdaml      (-1.000)
                      + u2(c, k, i, m, a, j)*t2a(d, b, m, l) & !ckimajdbml      (+1.000)
                      + u2(a, k, i, m, b, j)*t2a(d, c, m, l) & !akimbjdcml      (+1.000)
                      - u2(b, k, i, m, a, j)*t2a(d, c, m, l) & !bkimajdcml      (-1.000)
                      - u2(c, l, i, m, d, j)*t2a(b, a, m, k) & !climdjbamk      (-1.000)
                      + u2(b, l, i, m, d, j)*t2a(c, a, m, k) & !blimdjcamk      (+1.000)
                      - u2(a, l, i, m, d, j)*t2a(c, b, m, k) & !alimdjcbmk      (-1.000)
                      + u2(d, l, i, m, c, j)*t2a(b, a, m, k) & !dlimcjbamk      (+1.000)
                      - u2(d, l, i, m, b, j)*t2a(c, a, m, k) & !dlimbjcamk      (-1.000)
                      + u2(d, l, i, m, a, j)*t2a(c, b, m, k) & !dlimajcbmk      (+1.000)
                      - u2(b, l, i, m, c, j)*t2a(d, a, m, k) & !blimcjdamk      (-1.000)
                      + u2(a, l, i, m, c, j)*t2a(d, b, m, k) & !alimcjdbmk      (+1.000)
                      + u2(c, l, i, m, b, j)*t2a(d, a, m, k) & !climbjdamk      (+1.000)
                      - u2(c, l, i, m, a, j)*t2a(d, b, m, k) & !climajdbmk      (-1.000)
                      - u2(a, l, i, m, b, j)*t2a(d, c, m, k) & !alimbjdcmk      (-1.000)
                      + u2(b, l, i, m, a, j)*t2a(d, c, m, k) & !blimajdcmk      (+1.000)
                      + u2(a, l, k, m, d, j)*t2a(c, b, m, i) & !alkmdjcbmi      (+1.000)
                      - u2(b, l, k, m, d, j)*t2a(c, a, m, i) & !blkmdjcami      (-1.000)
                      + u2(c, l, k, m, d, j)*t2a(b, a, m, i) & !clkmdjbami      (+1.000)
                      - u2(a, l, k, m, c, j)*t2a(d, b, m, i) & !alkmcjdbmi      (-1.000)
                      + u2(b, l, k, m, c, j)*t2a(d, a, m, i) & !blkmcjdami      (+1.000)
                      + u2(a, l, k, m, b, j)*t2a(d, c, m, i) & !alkmbjdcmi      (+1.000)
                      - u2(b, l, k, m, a, j)*t2a(d, c, m, i) & !blkmajdcmi      (-1.000)
                      - u2(c, l, k, m, b, j)*t2a(d, a, m, i) & !clkmbjdami      (-1.000)
                      + u2(c, l, k, m, a, j)*t2a(d, b, m, i) & !clkmajdbmi      (+1.000)
                      - u2(d, l, k, m, c, j)*t2a(b, a, m, i) & !dlkmcjbami      (-1.000)
                      + u2(d, l, k, m, b, j)*t2a(c, a, m, i) & !dlkmbjcami      (+1.000)
                      - u2(d, l, k, m, a, j)*t2a(c, b, m, i) & !dlkmajcbmi      (-1.000)
                      - u2(c, j, i, m, d, k)*t2a(b, a, m, l) & !cjimdkbaml      (-1.000)
                      + u2(b, j, i, m, d, k)*t2a(c, a, m, l) & !bjimdkcaml      (+1.000)
                      - u2(a, j, i, m, d, k)*t2a(c, b, m, l) & !ajimdkcbml      (-1.000)
                      + u2(d, j, i, m, c, k)*t2a(b, a, m, l) & !djimckbaml      (+1.000)
                      - u2(d, j, i, m, b, k)*t2a(c, a, m, l) & !djimbkcaml      (-1.000)
                      + u2(d, j, i, m, a, k)*t2a(c, b, m, l) & !djimakcbml      (+1.000)
                      - u2(b, j, i, m, c, k)*t2a(d, a, m, l) & !bjimckdaml      (-1.000)
                      + u2(a, j, i, m, c, k)*t2a(d, b, m, l) & !ajimckdbml      (+1.000)
                      + u2(c, j, i, m, b, k)*t2a(d, a, m, l) & !cjimbkdaml      (+1.000)
                      - u2(c, j, i, m, a, k)*t2a(d, b, m, l) & !cjimakdbml      (-1.000)
                      - u2(a, j, i, m, b, k)*t2a(d, c, m, l) & !ajimbkdcml      (-1.000)
                      + u2(b, j, i, m, a, k)*t2a(d, c, m, l) & !bjimakdcml      (+1.000)
                      + u2(c, j, i, m, d, l)*t2a(b, a, m, k) & !cjimdlbamk      (+1.000)
                      - u2(b, j, i, m, d, l)*t2a(c, a, m, k) & !bjimdlcamk      (-1.000)
                      + u2(a, j, i, m, d, l)*t2a(c, b, m, k) & !ajimdlcbmk      (+1.000)
                      - u2(d, j, i, m, c, l)*t2a(b, a, m, k) & !djimclbamk      (-1.000)
                      + u2(d, j, i, m, b, l)*t2a(c, a, m, k) & !djimblcamk      (+1.000)
                      - u2(d, j, i, m, a, l)*t2a(c, b, m, k) & !djimalcbmk      (-1.000)
                      + u2(b, j, i, m, c, l)*t2a(d, a, m, k) & !bjimcldamk      (+1.000)
                      - u2(a, j, i, m, c, l)*t2a(d, b, m, k) & !ajimcldbmk      (-1.000)
                      - u2(c, j, i, m, b, l)*t2a(d, a, m, k) & !cjimbldamk      (-1.000)
                      + u2(c, j, i, m, a, l)*t2a(d, b, m, k) & !cjimaldbmk      (+1.000)
                      + u2(a, j, i, m, b, l)*t2a(d, c, m, k) & !ajimbldcmk      (+1.000)
                      - u2(b, j, i, m, a, l)*t2a(d, c, m, k) & !bjimaldcmk      (-1.000)
                      + u2(c, l, i, m, d, k)*t2a(b, a, m, j) & !climdkbamj      (+1.000)
                      - u2(b, l, i, m, d, k)*t2a(c, a, m, j) & !blimdkcamj      (-1.000)
                      + u2(a, l, i, m, d, k)*t2a(c, b, m, j) & !alimdkcbmj      (+1.000)
                      - u2(d, l, i, m, c, k)*t2a(b, a, m, j) & !dlimckbamj      (-1.000)
                      + u2(d, l, i, m, b, k)*t2a(c, a, m, j) & !dlimbkcamj      (+1.000)
                      - u2(d, l, i, m, a, k)*t2a(c, b, m, j) & !dlimakcbmj      (-1.000)
                      + u2(b, l, i, m, c, k)*t2a(d, a, m, j) & !blimckdamj      (+1.000)
                      - u2(a, l, i, m, c, k)*t2a(d, b, m, j) & !alimckdbmj      (-1.000)
                      - u2(c, l, i, m, b, k)*t2a(d, a, m, j) & !climbkdamj      (-1.000)
                      + u2(c, l, i, m, a, k)*t2a(d, b, m, j) & !climakdbmj      (+1.000)
                      + u2(a, l, i, m, b, k)*t2a(d, c, m, j) & !alimbkdcmj      (+1.000)
                      - u2(b, l, i, m, a, k)*t2a(d, c, m, j) & !blimakdcmj      (-1.000)
                      - u2(a, l, j, m, d, k)*t2a(c, b, m, i) & !aljmdkcbmi      (-1.000)
                      + u2(b, l, j, m, d, k)*t2a(c, a, m, i) & !bljmdkcami      (+1.000)
                      - u2(c, l, j, m, d, k)*t2a(b, a, m, i) & !cljmdkbami      (-1.000)
                      + u2(a, l, j, m, c, k)*t2a(d, b, m, i) & !aljmckdbmi      (+1.000)
                      - u2(b, l, j, m, c, k)*t2a(d, a, m, i) & !bljmckdami      (-1.000)
                      - u2(a, l, j, m, b, k)*t2a(d, c, m, i) & !aljmbkdcmi      (-1.000)
                      + u2(b, l, j, m, a, k)*t2a(d, c, m, i) & !bljmakdcmi      (+1.000)
                      + u2(c, l, j, m, b, k)*t2a(d, a, m, i) & !cljmbkdami      (+1.000)
                      - u2(c, l, j, m, a, k)*t2a(d, b, m, i) & !cljmakdbmi      (-1.000)
                      + u2(d, l, j, m, c, k)*t2a(b, a, m, i) & !dljmckbami      (+1.000)
                      - u2(d, l, j, m, b, k)*t2a(c, a, m, i) & !dljmbkcami      (-1.000)
                      + u2(d, l, j, m, a, k)*t2a(c, b, m, i) & !dljmakcbmi      (+1.000)
                      - u2(c, k, i, m, d, l)*t2a(b, a, m, j) & !ckimdlbamj      (-1.000)
                      + u2(b, k, i, m, d, l)*t2a(c, a, m, j) & !bkimdlcamj      (+1.000)
                      - u2(a, k, i, m, d, l)*t2a(c, b, m, j) & !akimdlcbmj      (-1.000)
                      + u2(d, k, i, m, c, l)*t2a(b, a, m, j) & !dkimclbamj      (+1.000)
                      - u2(d, k, i, m, b, l)*t2a(c, a, m, j) & !dkimblcamj      (-1.000)
                      + u2(d, k, i, m, a, l)*t2a(c, b, m, j) & !dkimalcbmj      (+1.000)
                      - u2(b, k, i, m, c, l)*t2a(d, a, m, j) & !bkimcldamj      (-1.000)
                      + u2(a, k, i, m, c, l)*t2a(d, b, m, j) & !akimcldbmj      (+1.000)
                      + u2(c, k, i, m, b, l)*t2a(d, a, m, j) & !ckimbldamj      (+1.000)
                      - u2(c, k, i, m, a, l)*t2a(d, b, m, j) & !ckimaldbmj      (-1.000)
                      - u2(a, k, i, m, b, l)*t2a(d, c, m, j) & !akimbldcmj      (-1.000)
                      + u2(b, k, i, m, a, l)*t2a(d, c, m, j) & !bkimaldcmj      (+1.000)
                      + u2(a, k, j, m, d, l)*t2a(c, b, m, i) & !akjmdlcbmi      (+1.000)
                      - u2(b, k, j, m, d, l)*t2a(c, a, m, i) & !bkjmdlcami      (-1.000)
                      + u2(c, k, j, m, d, l)*t2a(b, a, m, i) & !ckjmdlbami      (+1.000)
                      - u2(a, k, j, m, c, l)*t2a(d, b, m, i) & !akjmcldbmi      (-1.000)
                      + u2(b, k, j, m, c, l)*t2a(d, a, m, i) & !bkjmcldami      (+1.000)
                      + u2(a, k, j, m, b, l)*t2a(d, c, m, i) & !akjmbldcmi      (+1.000)
                      - u2(b, k, j, m, a, l)*t2a(d, c, m, i) & !bkjmaldcmi      (-1.000)
                      - u2(c, k, j, m, b, l)*t2a(d, a, m, i) & !ckjmbldami      (-1.000)
                      + u2(c, k, j, m, a, l)*t2a(d, b, m, i) & !ckjmaldbmi      (+1.000)
                      - u2(d, k, j, m, c, l)*t2a(b, a, m, i) & !dkjmclbami      (-1.000)
                      + u2(d, k, j, m, b, l)*t2a(c, a, m, i) & !dkjmblcami      (+1.000)
                      - u2(d, k, j, m, a, l)*t2a(c, b, m, i)       !dkjmalcbmi      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u2)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '3421', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u3(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u3)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u3) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum &
                      - u3(a, l, k, f, d, c)*t2a(f, b, j, i) & !alkfdcfbji      (-1.000)
                      - u3(a, j, i, f, d, c)*t2a(f, b, l, k) & !ajifdcfblk      (-1.000)
                      + u3(a, l, k, f, d, b)*t2a(f, c, j, i) & !alkfdbfcji      (+1.000)
                      - u3(b, l, k, f, d, a)*t2a(f, c, j, i) & !blkfdafcji      (-1.000)
                      + u3(a, j, i, f, d, b)*t2a(f, c, l, k) & !ajifdbfclk      (+1.000)
                      - u3(b, j, i, f, d, a)*t2a(f, c, l, k) & !bjifdafclk      (-1.000)
                      - u3(a, l, k, f, c, b)*t2a(f, d, j, i) & !alkfcbfdji      (-1.000)
                      + u3(b, l, k, f, c, a)*t2a(f, d, j, i) & !blkfcafdji      (+1.000)
                      - u3(c, l, k, f, b, a)*t2a(f, d, j, i) & !clkfbafdji      (-1.000)
                      - u3(a, j, i, f, c, b)*t2a(f, d, l, k) & !ajifcbfdlk      (-1.000)
                      + u3(b, j, i, f, c, a)*t2a(f, d, l, k) & !bjifcafdlk      (+1.000)
                      - u3(c, j, i, f, b, a)*t2a(f, d, l, k) & !cjifbafdlk      (-1.000)
                      + u3(a, l, j, f, d, c)*t2a(f, b, k, i) & !aljfdcfbki      (+1.000)
                      + u3(a, k, i, f, d, c)*t2a(f, b, l, j) & !akifdcfblj      (+1.000)
                      - u3(a, l, j, f, d, b)*t2a(f, c, k, i) & !aljfdbfcki      (-1.000)
                      + u3(b, l, j, f, d, a)*t2a(f, c, k, i) & !bljfdafcki      (+1.000)
                      - u3(a, k, i, f, d, b)*t2a(f, c, l, j) & !akifdbfclj      (-1.000)
                      + u3(b, k, i, f, d, a)*t2a(f, c, l, j) & !bkifdafclj      (+1.000)
                      + u3(a, l, j, f, c, b)*t2a(f, d, k, i) & !aljfcbfdki      (+1.000)
                      - u3(b, l, j, f, c, a)*t2a(f, d, k, i) & !bljfcafdki      (-1.000)
                      + u3(c, l, j, f, b, a)*t2a(f, d, k, i) & !cljfbafdki      (+1.000)
                      + u3(a, k, i, f, c, b)*t2a(f, d, l, j) & !akifcbfdlj      (+1.000)
                      - u3(b, k, i, f, c, a)*t2a(f, d, l, j) & !bkifcafdlj      (-1.000)
                      + u3(c, k, i, f, b, a)*t2a(f, d, l, j) & !ckifbafdlj      (+1.000)
                      - u3(a, k, j, f, d, c)*t2a(f, b, l, i) & !akjfdcfbli      (-1.000)
                      - u3(a, l, i, f, d, c)*t2a(f, b, k, j) & !alifdcfbkj      (-1.000)
                      + u3(a, k, j, f, d, b)*t2a(f, c, l, i) & !akjfdbfcli      (+1.000)
                      - u3(b, k, j, f, d, a)*t2a(f, c, l, i) & !bkjfdafcli      (-1.000)
                      + u3(a, l, i, f, d, b)*t2a(f, c, k, j) & !alifdbfckj      (+1.000)
                      - u3(b, l, i, f, d, a)*t2a(f, c, k, j) & !blifdafckj      (-1.000)
                      - u3(a, k, j, f, c, b)*t2a(f, d, l, i) & !akjfcbfdli      (-1.000)
                      + u3(b, k, j, f, c, a)*t2a(f, d, l, i) & !bkjfcafdli      (+1.000)
                      - u3(c, k, j, f, b, a)*t2a(f, d, l, i) & !ckjfbafdli      (-1.000)
                      - u3(a, l, i, f, c, b)*t2a(f, d, k, j) & !alifcbfdkj      (-1.000)
                      + u3(b, l, i, f, c, a)*t2a(f, d, k, j) & !blifcafdkj      (+1.000)
                      - u3(c, l, i, f, b, a)*t2a(f, d, k, j)       !clifbafdkj      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u3)

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s11(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, b1, d2, s11)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s11)
    deallocate (s11)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n0 - n0, n1 - n0/), '21', fockr, b1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s12(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s12)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s12)
    deallocate (s12)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u4(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u4)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,u4) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (u4(d, k, j, m, i, n)*t3a(c, b, a, n, m, l) & !dkjmincbanml    (+0.500)
                         - u4(c, k, j, m, i, n)*t3a(d, b, a, n, m, l) & !ckjmindbanml    (-0.500)
                         + u4(b, k, j, m, i, n)*t3a(d, c, a, n, m, l) & !bkjmindcanml    (+0.500)
                         - u4(a, k, j, m, i, n)*t3a(d, c, b, n, m, l) & !akjmindcbnml    (-0.500)
                         - u4(d, l, j, m, i, n)*t3a(c, b, a, n, m, k) & !dljmincbanmk    (-0.500)
                         + u4(c, l, j, m, i, n)*t3a(d, b, a, n, m, k) & !cljmindbanmk    (+0.500)
                         - u4(b, l, j, m, i, n)*t3a(d, c, a, n, m, k) & !bljmindcanmk    (-0.500)
                         + u4(a, l, j, m, i, n)*t3a(d, c, b, n, m, k) & !aljmindcbnmk    (+0.500)
                         + u4(d, l, k, m, i, n)*t3a(c, b, a, n, m, j) & !dlkmincbanmj    (+0.500)
                         - u4(c, l, k, m, i, n)*t3a(d, b, a, n, m, j) & !clkmindbanmj    (-0.500)
                         + u4(b, l, k, m, i, n)*t3a(d, c, a, n, m, j) & !blkmindcanmj    (+0.500)
                         - u4(a, l, k, m, i, n)*t3a(d, c, b, n, m, j) & !alkmindcbnmj    (-0.500)
                         - u4(d, k, i, m, j, n)*t3a(c, b, a, n, m, l) & !dkimjncbanml    (-0.500)
                         + u4(c, k, i, m, j, n)*t3a(d, b, a, n, m, l) & !ckimjndbanml    (+0.500)
                         - u4(b, k, i, m, j, n)*t3a(d, c, a, n, m, l) & !bkimjndcanml    (-0.500)
                         + u4(a, k, i, m, j, n)*t3a(d, c, b, n, m, l) & !akimjndcbnml    (+0.500)
                         + u4(d, l, i, m, j, n)*t3a(c, b, a, n, m, k) & !dlimjncbanmk    (+0.500)
                         - u4(c, l, i, m, j, n)*t3a(d, b, a, n, m, k) & !climjndbanmk    (-0.500)
                         + u4(b, l, i, m, j, n)*t3a(d, c, a, n, m, k) & !blimjndcanmk    (+0.500)
                         - u4(a, l, i, m, j, n)*t3a(d, c, b, n, m, k) & !alimjndcbnmk    (-0.500)
                         + u4(d, j, i, m, k, n)*t3a(c, b, a, n, m, l) & !djimkncbanml    (+0.500)
                         - u4(c, j, i, m, k, n)*t3a(d, b, a, n, m, l) & !cjimkndbanml    (-0.500)
                         + u4(b, j, i, m, k, n)*t3a(d, c, a, n, m, l) & !bjimkndcanml    (+0.500)
                         - u4(a, j, i, m, k, n)*t3a(d, c, b, n, m, l) & !ajimkndcbnml    (-0.500)
                         - u4(d, j, i, m, l, n)*t3a(c, b, a, n, m, k) & !djimlncbanmk    (-0.500)
                         + u4(c, j, i, m, l, n)*t3a(d, b, a, n, m, k) & !cjimlndbanmk    (+0.500)
                         - u4(b, j, i, m, l, n)*t3a(d, c, a, n, m, k) & !bjimlndcanmk    (-0.500)
                         + u4(a, j, i, m, l, n)*t3a(d, c, b, n, m, k) & !ajimlndcbnmk    (+0.500)
                         - u4(d, l, i, m, k, n)*t3a(c, b, a, n, m, j) & !dlimkncbanmj    (-0.500)
                         + u4(c, l, i, m, k, n)*t3a(d, b, a, n, m, j) & !climkndbanmj    (+0.500)
                         - u4(b, l, i, m, k, n)*t3a(d, c, a, n, m, j) & !blimkndcanmj    (-0.500)
                         + u4(a, l, i, m, k, n)*t3a(d, c, b, n, m, j) & !alimkndcbnmj    (+0.500)
                         + u4(d, k, i, m, l, n)*t3a(c, b, a, n, m, j) & !dkimlncbanmj    (+0.500)
                         - u4(c, k, i, m, l, n)*t3a(d, b, a, n, m, j) & !ckimlndbanmj    (-0.500)
                         + u4(b, k, i, m, l, n)*t3a(d, c, a, n, m, j) & !bkimlndcanmj    (+0.500)
                         - u4(a, k, i, m, l, n)*t3a(d, c, b, n, m, j) & !akimlndcbnmj    (-0.500)
                         - u4(d, l, k, m, j, n)*t3a(c, b, a, n, m, i) & !dlkmjncbanmi    (-0.500)
                         + u4(c, l, k, m, j, n)*t3a(d, b, a, n, m, i) & !clkmjndbanmi    (+0.500)
                         - u4(b, l, k, m, j, n)*t3a(d, c, a, n, m, i) & !blkmjndcanmi    (-0.500)
                         + u4(a, l, k, m, j, n)*t3a(d, c, b, n, m, i) & !alkmjndcbnmi    (+0.500)
                         + u4(d, l, j, m, k, n)*t3a(c, b, a, n, m, i) & !dljmkncbanmi    (+0.500)
                         - u4(c, l, j, m, k, n)*t3a(d, b, a, n, m, i) & !cljmkndbanmi    (-0.500)
                         + u4(b, l, j, m, k, n)*t3a(d, c, a, n, m, i) & !bljmkndcanmi    (+0.500)
                         - u4(a, l, j, m, k, n)*t3a(d, c, b, n, m, i) & !aljmkndcbnmi    (-0.500)
                         - u4(d, k, j, m, l, n)*t3a(c, b, a, n, m, i) & !dkjmlncbanmi    (-0.500)
                         + u4(c, k, j, m, l, n)*t3a(d, b, a, n, m, i) & !ckjmlndbanmi    (+0.500)
                         - u4(b, k, j, m, l, n)*t3a(d, c, a, n, m, i) & !bkjmlndcanmi    (-0.500)
                         + u4(a, k, j, m, l, n)*t3a(d, c, b, n, m, i))/2.0d0!akjmlndcbnmi    (+0.500)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u4)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '142356', t3a, f2)
    allocate (u5(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k1*k3*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u5)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u5) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + u5(b, a, l, k, m, i)*t2a(d, c, m, j) & !balkmidcmj      (+1.000)
                      - u5(c, a, l, k, m, i)*t2a(d, b, m, j) & !calkmidbmj      (-1.000)
                      + u5(c, b, l, k, m, i)*t2a(d, a, m, j) & !cblkmidamj      (+1.000)
                      + u5(d, a, l, k, m, i)*t2a(c, b, m, j) & !dalkmicbmj      (+1.000)
                      - u5(d, b, l, k, m, i)*t2a(c, a, m, j) & !dblkmicamj      (-1.000)
                      + u5(d, c, l, k, m, i)*t2a(b, a, m, j) & !dclkmibamj      (+1.000)
                      - u5(b, a, l, j, m, i)*t2a(d, c, m, k) & !baljmidcmk      (-1.000)
                      + u5(c, a, l, j, m, i)*t2a(d, b, m, k) & !caljmidbmk      (+1.000)
                      - u5(c, b, l, j, m, i)*t2a(d, a, m, k) & !cbljmidamk      (-1.000)
                      - u5(d, a, l, j, m, i)*t2a(c, b, m, k) & !daljmicbmk      (-1.000)
                      + u5(d, b, l, j, m, i)*t2a(c, a, m, k) & !dbljmicamk      (+1.000)
                      - u5(d, c, l, j, m, i)*t2a(b, a, m, k) & !dcljmibamk      (-1.000)
                      + u5(b, a, k, j, m, i)*t2a(d, c, m, l) & !bakjmidcml      (+1.000)
                      - u5(c, a, k, j, m, i)*t2a(d, b, m, l) & !cakjmidbml      (-1.000)
                      + u5(c, b, k, j, m, i)*t2a(d, a, m, l) & !cbkjmidaml      (+1.000)
                      + u5(d, a, k, j, m, i)*t2a(c, b, m, l) & !dakjmicbml      (+1.000)
                      - u5(d, b, k, j, m, i)*t2a(c, a, m, l) & !dbkjmicaml      (-1.000)
                      + u5(d, c, k, j, m, i)*t2a(b, a, m, l) & !dckjmibaml      (+1.000)
                      - u5(b, a, l, k, m, j)*t2a(d, c, m, i) & !balkmjdcmi      (-1.000)
                      + u5(c, a, l, k, m, j)*t2a(d, b, m, i) & !calkmjdbmi      (+1.000)
                      - u5(c, b, l, k, m, j)*t2a(d, a, m, i) & !cblkmjdami      (-1.000)
                      - u5(d, a, l, k, m, j)*t2a(c, b, m, i) & !dalkmjcbmi      (-1.000)
                      + u5(d, b, l, k, m, j)*t2a(c, a, m, i) & !dblkmjcami      (+1.000)
                      - u5(d, c, l, k, m, j)*t2a(b, a, m, i) & !dclkmjbami      (-1.000)
                      + u5(b, a, l, j, m, k)*t2a(d, c, m, i) & !baljmkdcmi      (+1.000)
                      - u5(c, a, l, j, m, k)*t2a(d, b, m, i) & !caljmkdbmi      (-1.000)
                      + u5(c, b, l, j, m, k)*t2a(d, a, m, i) & !cbljmkdami      (+1.000)
                      + u5(d, a, l, j, m, k)*t2a(c, b, m, i) & !daljmkcbmi      (+1.000)
                      - u5(d, b, l, j, m, k)*t2a(c, a, m, i) & !dbljmkcami      (-1.000)
                      + u5(d, c, l, j, m, k)*t2a(b, a, m, i) & !dcljmkbami      (+1.000)
                      - u5(b, a, k, j, m, l)*t2a(d, c, m, i) & !bakjmldcmi      (-1.000)
                      + u5(c, a, k, j, m, l)*t2a(d, b, m, i) & !cakjmldbmi      (+1.000)
                      - u5(c, b, k, j, m, l)*t2a(d, a, m, i) & !cbkjmldami      (-1.000)
                      - u5(d, a, k, j, m, l)*t2a(c, b, m, i) & !dakjmlcbmi      (-1.000)
                      + u5(d, b, k, j, m, l)*t2a(c, a, m, i) & !dbkjmlcami      (+1.000)
                      - u5(d, c, k, j, m, l)*t2a(b, a, m, i) & !dckjmlbami      (-1.000)
                      + u5(b, a, l, i, m, j)*t2a(d, c, m, k) & !balimjdcmk      (+1.000)
                      - u5(c, a, l, i, m, j)*t2a(d, b, m, k) & !calimjdbmk      (-1.000)
                      + u5(c, b, l, i, m, j)*t2a(d, a, m, k) & !cblimjdamk      (+1.000)
                      + u5(d, a, l, i, m, j)*t2a(c, b, m, k) & !dalimjcbmk      (+1.000)
                      - u5(d, b, l, i, m, j)*t2a(c, a, m, k) & !dblimjcamk      (-1.000)
                      + u5(d, c, l, i, m, j)*t2a(b, a, m, k) & !dclimjbamk      (+1.000)
                      - u5(b, a, k, i, m, j)*t2a(d, c, m, l) & !bakimjdcml      (-1.000)
                      + u5(c, a, k, i, m, j)*t2a(d, b, m, l) & !cakimjdbml      (+1.000)
                      - u5(c, b, k, i, m, j)*t2a(d, a, m, l) & !cbkimjdaml      (-1.000)
                      - u5(d, a, k, i, m, j)*t2a(c, b, m, l) & !dakimjcbml      (-1.000)
                      + u5(d, b, k, i, m, j)*t2a(c, a, m, l) & !dbkimjcaml      (+1.000)
                      - u5(d, c, k, i, m, j)*t2a(b, a, m, l) & !dckimjbaml      (-1.000)
                      - u5(b, a, l, i, m, k)*t2a(d, c, m, j) & !balimkdcmj      (-1.000)
                      + u5(c, a, l, i, m, k)*t2a(d, b, m, j) & !calimkdbmj      (+1.000)
                      - u5(c, b, l, i, m, k)*t2a(d, a, m, j) & !cblimkdamj      (-1.000)
                      - u5(d, a, l, i, m, k)*t2a(c, b, m, j) & !dalimkcbmj      (-1.000)
                      + u5(d, b, l, i, m, k)*t2a(c, a, m, j) & !dblimkcamj      (+1.000)
                      - u5(d, c, l, i, m, k)*t2a(b, a, m, j) & !dclimkbamj      (-1.000)
                      + u5(b, a, k, i, m, l)*t2a(d, c, m, j) & !bakimldcmj      (+1.000)
                      - u5(c, a, k, i, m, l)*t2a(d, b, m, j) & !cakimldbmj      (-1.000)
                      + u5(c, b, k, i, m, l)*t2a(d, a, m, j) & !cbkimldamj      (+1.000)
                      + u5(d, a, k, i, m, l)*t2a(c, b, m, j) & !dakimlcbmj      (+1.000)
                      - u5(d, b, k, i, m, l)*t2a(c, a, m, j) & !dbkimlcamj      (-1.000)
                      + u5(d, c, k, i, m, l)*t2a(b, a, m, j) & !dckimlbamj      (+1.000)
                      + u5(b, a, j, i, m, k)*t2a(d, c, m, l) & !bajimkdcml      (+1.000)
                      - u5(c, a, j, i, m, k)*t2a(d, b, m, l) & !cajimkdbml      (-1.000)
                      + u5(c, b, j, i, m, k)*t2a(d, a, m, l) & !cbjimkdaml      (+1.000)
                      + u5(d, a, j, i, m, k)*t2a(c, b, m, l) & !dajimkcbml      (+1.000)
                      - u5(d, b, j, i, m, k)*t2a(c, a, m, l) & !dbjimkcaml      (-1.000)
                      + u5(d, c, j, i, m, k)*t2a(b, a, m, l) & !dcjimkbaml      (+1.000)
                      - u5(b, a, j, i, m, l)*t2a(d, c, m, k) & !bajimldcmk      (-1.000)
                      + u5(c, a, j, i, m, l)*t2a(d, b, m, k) & !cajimldbmk      (+1.000)
                      - u5(c, b, j, i, m, l)*t2a(d, a, m, k) & !cbjimldamk      (-1.000)
                      - u5(d, a, j, i, m, l)*t2a(c, b, m, k) & !dajimlcbmk      (-1.000)
                      + u5(d, b, j, i, m, l)*t2a(c, a, m, k) & !dbjimlcamk      (+1.000)
                      - u5(d, c, j, i, m, l)*t2a(b, a, m, k)       !dcjimlbamk      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u5)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s13(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s13)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x9), size(x9), '2341', 1.000, x9, &
                    s13)
    deallocate (s13)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1243', intr, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '4312', t2a, d2)
    allocate (s14(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k3*k3
    i3 = k1*k1
    call egemm(i1, i2, i3, d1, d2, s14)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2314', 0.500, x2, &
                    s14)
    deallocate (s14)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u6(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u6)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,u6) &
        !$omp private(a,b,c,d,f,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + u6(c, j, i, f, d, m)*t3a(f, b, a, m, l, k) & !cjifdmfbamlk    (+1.000)
                      - u6(b, j, i, f, d, m)*t3a(f, c, a, m, l, k) & !bjifdmfcamlk    (-1.000)
                      + u6(a, j, i, f, d, m)*t3a(f, c, b, m, l, k) & !ajifdmfcbmlk    (+1.000)
                      - u6(d, j, i, f, c, m)*t3a(f, b, a, m, l, k) & !djifcmfbamlk    (-1.000)
                      + u6(d, j, i, f, b, m)*t3a(f, c, a, m, l, k) & !djifbmfcamlk    (+1.000)
                      - u6(d, j, i, f, a, m)*t3a(f, c, b, m, l, k) & !djifamfcbmlk    (-1.000)
                      + u6(b, j, i, f, c, m)*t3a(f, d, a, m, l, k) & !bjifcmfdamlk    (+1.000)
                      - u6(a, j, i, f, c, m)*t3a(f, d, b, m, l, k) & !ajifcmfdbmlk    (-1.000)
                      - u6(c, j, i, f, b, m)*t3a(f, d, a, m, l, k) & !cjifbmfdamlk    (-1.000)
                      + u6(c, j, i, f, a, m)*t3a(f, d, b, m, l, k) & !cjifamfdbmlk    (+1.000)
                      + u6(a, j, i, f, b, m)*t3a(f, d, c, m, l, k) & !ajifbmfdcmlk    (+1.000)
                      - u6(b, j, i, f, a, m)*t3a(f, d, c, m, l, k) & !bjifamfdcmlk    (-1.000)
                      - u6(c, k, i, f, d, m)*t3a(f, b, a, m, l, j) & !ckifdmfbamlj    (-1.000)
                      + u6(b, k, i, f, d, m)*t3a(f, c, a, m, l, j) & !bkifdmfcamlj    (+1.000)
                      - u6(a, k, i, f, d, m)*t3a(f, c, b, m, l, j) & !akifdmfcbmlj    (-1.000)
                      + u6(d, k, i, f, c, m)*t3a(f, b, a, m, l, j) & !dkifcmfbamlj    (+1.000)
                      - u6(d, k, i, f, b, m)*t3a(f, c, a, m, l, j) & !dkifbmfcamlj    (-1.000)
                      + u6(d, k, i, f, a, m)*t3a(f, c, b, m, l, j) & !dkifamfcbmlj    (+1.000)
                      - u6(b, k, i, f, c, m)*t3a(f, d, a, m, l, j) & !bkifcmfdamlj    (-1.000)
                      + u6(a, k, i, f, c, m)*t3a(f, d, b, m, l, j) & !akifcmfdbmlj    (+1.000)
                      + u6(c, k, i, f, b, m)*t3a(f, d, a, m, l, j) & !ckifbmfdamlj    (+1.000)
                      - u6(c, k, i, f, a, m)*t3a(f, d, b, m, l, j) & !ckifamfdbmlj    (-1.000)
                      - u6(a, k, i, f, b, m)*t3a(f, d, c, m, l, j) & !akifbmfdcmlj    (-1.000)
                      + u6(b, k, i, f, a, m)*t3a(f, d, c, m, l, j) & !bkifamfdcmlj    (+1.000)
                      + u6(c, l, i, f, d, m)*t3a(f, b, a, m, k, j) & !clifdmfbamkj    (+1.000)
                      - u6(b, l, i, f, d, m)*t3a(f, c, a, m, k, j) & !blifdmfcamkj    (-1.000)
                      + u6(a, l, i, f, d, m)*t3a(f, c, b, m, k, j) & !alifdmfcbmkj    (+1.000)
                      - u6(d, l, i, f, c, m)*t3a(f, b, a, m, k, j) & !dlifcmfbamkj    (-1.000)
                      + u6(d, l, i, f, b, m)*t3a(f, c, a, m, k, j) & !dlifbmfcamkj    (+1.000)
                      - u6(d, l, i, f, a, m)*t3a(f, c, b, m, k, j) & !dlifamfcbmkj    (-1.000)
                      + u6(b, l, i, f, c, m)*t3a(f, d, a, m, k, j) & !blifcmfdamkj    (+1.000)
                      - u6(a, l, i, f, c, m)*t3a(f, d, b, m, k, j) & !alifcmfdbmkj    (-1.000)
                      - u6(c, l, i, f, b, m)*t3a(f, d, a, m, k, j) & !clifbmfdamkj    (-1.000)
                      + u6(c, l, i, f, a, m)*t3a(f, d, b, m, k, j) & !clifamfdbmkj    (+1.000)
                      + u6(a, l, i, f, b, m)*t3a(f, d, c, m, k, j) & !alifbmfdcmkj    (+1.000)
                      - u6(b, l, i, f, a, m)*t3a(f, d, c, m, k, j) & !blifamfdcmkj    (-1.000)
                      + u6(c, k, j, f, d, m)*t3a(f, b, a, m, l, i) & !ckjfdmfbamli    (+1.000)
                      - u6(b, k, j, f, d, m)*t3a(f, c, a, m, l, i) & !bkjfdmfcamli    (-1.000)
                      + u6(a, k, j, f, d, m)*t3a(f, c, b, m, l, i) & !akjfdmfcbmli    (+1.000)
                      - u6(d, k, j, f, c, m)*t3a(f, b, a, m, l, i) & !dkjfcmfbamli    (-1.000)
                      + u6(d, k, j, f, b, m)*t3a(f, c, a, m, l, i) & !dkjfbmfcamli    (+1.000)
                      - u6(d, k, j, f, a, m)*t3a(f, c, b, m, l, i) & !dkjfamfcbmli    (-1.000)
                      + u6(b, k, j, f, c, m)*t3a(f, d, a, m, l, i) & !bkjfcmfdamli    (+1.000)
                      - u6(a, k, j, f, c, m)*t3a(f, d, b, m, l, i) & !akjfcmfdbmli    (-1.000)
                      - u6(c, k, j, f, b, m)*t3a(f, d, a, m, l, i) & !ckjfbmfdamli    (-1.000)
                      + u6(c, k, j, f, a, m)*t3a(f, d, b, m, l, i) & !ckjfamfdbmli    (+1.000)
                      + u6(a, k, j, f, b, m)*t3a(f, d, c, m, l, i) & !akjfbmfdcmli    (+1.000)
                      - u6(b, k, j, f, a, m)*t3a(f, d, c, m, l, i) & !bkjfamfdcmli    (-1.000)
                      - u6(c, l, j, f, d, m)*t3a(f, b, a, m, k, i) & !cljfdmfbamki    (-1.000)
                      + u6(b, l, j, f, d, m)*t3a(f, c, a, m, k, i) & !bljfdmfcamki    (+1.000)
                      - u6(a, l, j, f, d, m)*t3a(f, c, b, m, k, i) & !aljfdmfcbmki    (-1.000)
                      + u6(d, l, j, f, c, m)*t3a(f, b, a, m, k, i) & !dljfcmfbamki    (+1.000)
                      - u6(d, l, j, f, b, m)*t3a(f, c, a, m, k, i) & !dljfbmfcamki    (-1.000)
                      + u6(d, l, j, f, a, m)*t3a(f, c, b, m, k, i) & !dljfamfcbmki    (+1.000)
                      - u6(b, l, j, f, c, m)*t3a(f, d, a, m, k, i) & !bljfcmfdamki    (-1.000)
                      + u6(a, l, j, f, c, m)*t3a(f, d, b, m, k, i) & !aljfcmfdbmki    (+1.000)
                      + u6(c, l, j, f, b, m)*t3a(f, d, a, m, k, i) & !cljfbmfdamki    (+1.000)
                      - u6(c, l, j, f, a, m)*t3a(f, d, b, m, k, i) & !cljfamfdbmki    (-1.000)
                      - u6(a, l, j, f, b, m)*t3a(f, d, c, m, k, i) & !aljfbmfdcmki    (-1.000)
                      + u6(b, l, j, f, a, m)*t3a(f, d, c, m, k, i) & !bljfamfdcmki    (+1.000)
                      + u6(c, l, k, f, d, m)*t3a(f, b, a, m, j, i) & !clkfdmfbamji    (+1.000)
                      - u6(b, l, k, f, d, m)*t3a(f, c, a, m, j, i) & !blkfdmfcamji    (-1.000)
                      + u6(a, l, k, f, d, m)*t3a(f, c, b, m, j, i) & !alkfdmfcbmji    (+1.000)
                      - u6(d, l, k, f, c, m)*t3a(f, b, a, m, j, i) & !dlkfcmfbamji    (-1.000)
                      + u6(d, l, k, f, b, m)*t3a(f, c, a, m, j, i) & !dlkfbmfcamji    (+1.000)
                      - u6(d, l, k, f, a, m)*t3a(f, c, b, m, j, i) & !dlkfamfcbmji    (-1.000)
                      + u6(b, l, k, f, c, m)*t3a(f, d, a, m, j, i) & !blkfcmfdamji    (+1.000)
                      - u6(a, l, k, f, c, m)*t3a(f, d, b, m, j, i) & !alkfcmfdbmji    (-1.000)
                      - u6(c, l, k, f, b, m)*t3a(f, d, a, m, j, i) & !clkfbmfdamji    (-1.000)
                      + u6(c, l, k, f, a, m)*t3a(f, d, b, m, j, i) & !clkfamfdbmji    (+1.000)
                      + u6(a, l, k, f, b, m)*t3a(f, d, c, m, j, i) & !alkfbmfdcmji    (+1.000)
                      - u6(b, l, k, f, a, m)*t3a(f, d, c, m, j, i)     !blkfamfdcmji    (-1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u6)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s15(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k1
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, d2, s15)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '3421', 0.500, x1, &
                    s15)
    deallocate (s15)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intr, d1)
    allocate (f2(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '123456', t3a, f2)
    allocate (u7(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k1*k1*k3
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, f2, u7)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u7) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum & !top 2 switched
                      + (u7(b, l, k, j, d, m)*t2a(c, a, m, i) & !blkjdmcami      (+0.500)
                         - u7(a, l, k, j, d, m)*t2a(c, b, m, i) & !alkjdmcbmi      (-0.500)
                         - u7(c, l, k, j, d, m)*t2a(b, a, m, i) & !clkjdmbami      (-0.500)
                         + u7(a, l, k, j, c, m)*t2a(d, b, m, i) & !alkjcmdbmi      (+0.500)
                         - u7(b, l, k, j, c, m)*t2a(d, a, m, i) & !blkjcmdami      (-0.500)
                         - u7(a, l, k, j, b, m)*t2a(d, c, m, i) & !alkjbmdcmi      (-0.500)
                         + u7(b, l, k, j, a, m)*t2a(d, c, m, i) & !blkjamdcmi      (+0.500)
                         + u7(c, l, k, j, b, m)*t2a(d, a, m, i) & !clkjbmdami      (+0.500)
                         - u7(c, l, k, j, a, m)*t2a(d, b, m, i) & !clkjamdbmi      (-0.500)
                         + u7(d, l, k, j, c, m)*t2a(b, a, m, i) & !dlkjcmbami      (+0.500)
                         - u7(d, l, k, j, b, m)*t2a(c, a, m, i) & !dlkjbmcami      (-0.500)
                         + u7(d, l, k, j, a, m)*t2a(c, b, m, i) & !dlkjamcbmi      (+0.500)
                         + u7(a, l, k, i, d, m)*t2a(c, b, m, j) & !alkidmcbmj      (+0.500)
                         - u7(b, l, k, i, d, m)*t2a(c, a, m, j) & !blkidmcamj      (-0.500)
                         + u7(c, l, k, i, d, m)*t2a(b, a, m, j) & !clkidmbamj      (+0.500)
                         - u7(a, l, k, i, c, m)*t2a(d, b, m, j) & !alkicmdbmj      (-0.500)
                         + u7(b, l, k, i, c, m)*t2a(d, a, m, j) & !blkicmdamj      (+0.500)
                         + u7(a, l, k, i, b, m)*t2a(d, c, m, j) & !alkibmdcmj      (+0.500)
                         - u7(b, l, k, i, a, m)*t2a(d, c, m, j) & !blkiamdcmj      (-0.500)
                         - u7(c, l, k, i, b, m)*t2a(d, a, m, j) & !clkibmdamj      (-0.500)
                         + u7(c, l, k, i, a, m)*t2a(d, b, m, j) & !clkiamdbmj      (+0.500)
                         - u7(d, l, k, i, c, m)*t2a(b, a, m, j) & !dlkicmbamj      (-0.500)
                         + u7(d, l, k, i, b, m)*t2a(c, a, m, j) & !dlkibmcamj      (+0.500)
                         - u7(d, l, k, i, a, m)*t2a(c, b, m, j) & !dlkiamcbmj      (-0.500)
                         - u7(a, l, j, i, d, m)*t2a(c, b, m, k) & !aljidmcbmk      (-0.500)
                         + u7(b, l, j, i, d, m)*t2a(c, a, m, k) & !bljidmcamk      (+0.500)
                         - u7(c, l, j, i, d, m)*t2a(b, a, m, k) & !cljidmbamk      (-0.500)
                         + u7(a, l, j, i, c, m)*t2a(d, b, m, k) & !aljicmdbmk      (+0.500)
                         - u7(b, l, j, i, c, m)*t2a(d, a, m, k) & !bljicmdamk      (-0.500)
                         - u7(a, l, j, i, b, m)*t2a(d, c, m, k) & !aljibmdcmk      (-0.500)
                         + u7(b, l, j, i, a, m)*t2a(d, c, m, k) & !bljiamdcmk      (+0.500)
                         + u7(c, l, j, i, b, m)*t2a(d, a, m, k) & !cljibmdamk      (+0.500)
                         - u7(c, l, j, i, a, m)*t2a(d, b, m, k) & !cljiamdbmk      (-0.500)
                         + u7(d, l, j, i, c, m)*t2a(b, a, m, k) & !dljicmbamk      (+0.500)
                         - u7(d, l, j, i, b, m)*t2a(c, a, m, k) & !dljibmcamk      (-0.500)
                         + u7(d, l, j, i, a, m)*t2a(c, b, m, k) & !dljiamcbmk      (+0.500)
                         + u7(a, k, j, i, d, m)*t2a(c, b, m, l) & !akjidmcbml      (+0.500)
                         - u7(b, k, j, i, d, m)*t2a(c, a, m, l) & !bkjidmcaml      (-0.500)
                         + u7(c, k, j, i, d, m)*t2a(b, a, m, l) & !ckjidmbaml      (+0.500)
                         - u7(a, k, j, i, c, m)*t2a(d, b, m, l) & !akjicmdbml      (-0.500)
                         + u7(b, k, j, i, c, m)*t2a(d, a, m, l) & !bkjicmdaml      (+0.500)
                         + u7(a, k, j, i, b, m)*t2a(d, c, m, l) & !akjibmdcml      (+0.500)
                         - u7(b, k, j, i, a, m)*t2a(d, c, m, l) & !bkjiamdcml      (-0.500)
                         - u7(c, k, j, i, b, m)*t2a(d, a, m, l) & !ckjibmdaml      (-0.500)
                         + u7(c, k, j, i, a, m)*t2a(d, b, m, l) & !ckjiamdbml      (+0.500)
                         - u7(d, k, j, i, c, m)*t2a(b, a, m, l) & !dkjicmbaml      (-0.500)
                         + u7(d, k, j, i, b, m)*t2a(c, a, m, l) & !dkjibmcaml      (+0.500)
                         - u7(d, k, j, i, a, m)*t2a(c, b, m, l))/2.0d0!dkjiamcbml      (-0.500)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u7)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s16(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s16)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x10), size(x10), '3412', -1.000, &
                    x10, s16)
    deallocate (s16)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
    allocate (u8(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k1*k3*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u8)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u8) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + u8(b, a, l, k, m, i)*t2a(d, c, m, j) & !balkmidcmj      (+1.000)
                      - u8(c, a, l, k, m, i)*t2a(d, b, m, j) & !calkmidbmj      (-1.000)
                      + u8(c, b, l, k, m, i)*t2a(d, a, m, j) & !cblkmidamj      (+1.000)
                      + u8(d, a, l, k, m, i)*t2a(c, b, m, j) & !dalkmicbmj      (+1.000)
                      - u8(d, b, l, k, m, i)*t2a(c, a, m, j) & !dblkmicamj      (-1.000)
                      + u8(d, c, l, k, m, i)*t2a(b, a, m, j) & !dclkmibamj      (+1.000)
                      - u8(b, a, l, j, m, i)*t2a(d, c, m, k) & !baljmidcmk      (-1.000)
                      + u8(c, a, l, j, m, i)*t2a(d, b, m, k) & !caljmidbmk      (+1.000)
                      - u8(c, b, l, j, m, i)*t2a(d, a, m, k) & !cbljmidamk      (-1.000)
                      - u8(d, a, l, j, m, i)*t2a(c, b, m, k) & !daljmicbmk      (-1.000)
                      + u8(d, b, l, j, m, i)*t2a(c, a, m, k) & !dbljmicamk      (+1.000)
                      - u8(d, c, l, j, m, i)*t2a(b, a, m, k) & !dcljmibamk      (-1.000)
                      + u8(b, a, k, j, m, i)*t2a(d, c, m, l) & !bakjmidcml      (+1.000)
                      - u8(c, a, k, j, m, i)*t2a(d, b, m, l) & !cakjmidbml      (-1.000)
                      + u8(c, b, k, j, m, i)*t2a(d, a, m, l) & !cbkjmidaml      (+1.000)
                      + u8(d, a, k, j, m, i)*t2a(c, b, m, l) & !dakjmicbml      (+1.000)
                      - u8(d, b, k, j, m, i)*t2a(c, a, m, l) & !dbkjmicaml      (-1.000)
                      + u8(d, c, k, j, m, i)*t2a(b, a, m, l) & !dckjmibaml      (+1.000)
                      - u8(b, a, l, k, m, j)*t2a(d, c, m, i) & !balkmjdcmi      (-1.000)
                      + u8(c, a, l, k, m, j)*t2a(d, b, m, i) & !calkmjdbmi      (+1.000)
                      - u8(c, b, l, k, m, j)*t2a(d, a, m, i) & !cblkmjdami      (-1.000)
                      - u8(d, a, l, k, m, j)*t2a(c, b, m, i) & !dalkmjcbmi      (-1.000)
                      + u8(d, b, l, k, m, j)*t2a(c, a, m, i) & !dblkmjcami      (+1.000)
                      - u8(d, c, l, k, m, j)*t2a(b, a, m, i) & !dclkmjbami      (-1.000)
                      + u8(b, a, l, j, m, k)*t2a(d, c, m, i) & !baljmkdcmi      (+1.000)
                      - u8(c, a, l, j, m, k)*t2a(d, b, m, i) & !caljmkdbmi      (-1.000)
                      + u8(c, b, l, j, m, k)*t2a(d, a, m, i) & !cbljmkdami      (+1.000)
                      + u8(d, a, l, j, m, k)*t2a(c, b, m, i) & !daljmkcbmi      (+1.000)
                      - u8(d, b, l, j, m, k)*t2a(c, a, m, i) & !dbljmkcami      (-1.000)
                      + u8(d, c, l, j, m, k)*t2a(b, a, m, i) & !dcljmkbami      (+1.000)
                      - u8(b, a, k, j, m, l)*t2a(d, c, m, i) & !bakjmldcmi      (-1.000)
                      + u8(c, a, k, j, m, l)*t2a(d, b, m, i) & !cakjmldbmi      (+1.000)
                      - u8(c, b, k, j, m, l)*t2a(d, a, m, i) & !cbkjmldami      (-1.000)
                      - u8(d, a, k, j, m, l)*t2a(c, b, m, i) & !dakjmlcbmi      (-1.000)
                      + u8(d, b, k, j, m, l)*t2a(c, a, m, i) & !dbkjmlcami      (+1.000)
                      - u8(d, c, k, j, m, l)*t2a(b, a, m, i) & !dckjmlbami      (-1.000)
                      + u8(b, a, l, i, m, j)*t2a(d, c, m, k) & !balimjdcmk      (+1.000)
                      - u8(c, a, l, i, m, j)*t2a(d, b, m, k) & !calimjdbmk      (-1.000)
                      + u8(c, b, l, i, m, j)*t2a(d, a, m, k) & !cblimjdamk      (+1.000)
                      + u8(d, a, l, i, m, j)*t2a(c, b, m, k) & !dalimjcbmk      (+1.000)
                      - u8(d, b, l, i, m, j)*t2a(c, a, m, k) & !dblimjcamk      (-1.000)
                      + u8(d, c, l, i, m, j)*t2a(b, a, m, k) & !dclimjbamk      (+1.000)
                      - u8(b, a, k, i, m, j)*t2a(d, c, m, l) & !bakimjdcml      (-1.000)
                      + u8(c, a, k, i, m, j)*t2a(d, b, m, l) & !cakimjdbml      (+1.000)
                      - u8(c, b, k, i, m, j)*t2a(d, a, m, l) & !cbkimjdaml      (-1.000)
                      - u8(d, a, k, i, m, j)*t2a(c, b, m, l) & !dakimjcbml      (-1.000)
                      + u8(d, b, k, i, m, j)*t2a(c, a, m, l) & !dbkimjcaml      (+1.000)
                      - u8(d, c, k, i, m, j)*t2a(b, a, m, l) & !dckimjbaml      (-1.000)
                      - u8(b, a, l, i, m, k)*t2a(d, c, m, j) & !balimkdcmj      (-1.000)
                      + u8(c, a, l, i, m, k)*t2a(d, b, m, j) & !calimkdbmj      (+1.000)
                      - u8(c, b, l, i, m, k)*t2a(d, a, m, j) & !cblimkdamj      (-1.000)
                      - u8(d, a, l, i, m, k)*t2a(c, b, m, j) & !dalimkcbmj      (-1.000)
                      + u8(d, b, l, i, m, k)*t2a(c, a, m, j) & !dblimkcamj      (+1.000)
                      - u8(d, c, l, i, m, k)*t2a(b, a, m, j) & !dclimkbamj      (-1.000)
                      + u8(b, a, k, i, m, l)*t2a(d, c, m, j) & !bakimldcmj      (+1.000)
                      - u8(c, a, k, i, m, l)*t2a(d, b, m, j) & !cakimldbmj      (-1.000)
                      + u8(c, b, k, i, m, l)*t2a(d, a, m, j) & !cbkimldamj      (+1.000)
                      + u8(d, a, k, i, m, l)*t2a(c, b, m, j) & !dakimlcbmj      (+1.000)
                      - u8(d, b, k, i, m, l)*t2a(c, a, m, j) & !dbkimlcamj      (-1.000)
                      + u8(d, c, k, i, m, l)*t2a(b, a, m, j) & !dckimlbamj      (+1.000)
                      + u8(b, a, j, i, m, k)*t2a(d, c, m, l) & !bajimkdcml      (+1.000)
                      - u8(c, a, j, i, m, k)*t2a(d, b, m, l) & !cajimkdbml      (-1.000)
                      + u8(c, b, j, i, m, k)*t2a(d, a, m, l) & !cbjimkdaml      (+1.000)
                      + u8(d, a, j, i, m, k)*t2a(c, b, m, l) & !dajimkcbml      (+1.000)
                      - u8(d, b, j, i, m, k)*t2a(c, a, m, l) & !dbjimkcaml      (-1.000)
                      + u8(d, c, j, i, m, k)*t2a(b, a, m, l) & !dcjimkbaml      (+1.000)
                      - u8(b, a, j, i, m, l)*t2a(d, c, m, k) & !bajimldcmk      (-1.000)
                      + u8(c, a, j, i, m, l)*t2a(d, b, m, k) & !cajimldbmk      (+1.000)
                      - u8(c, b, j, i, m, l)*t2a(d, a, m, k) & !cbjimldamk      (-1.000)
                      - u8(d, a, j, i, m, l)*t2a(c, b, m, k) & !dajimlcbmk      (-1.000)
                      + u8(d, b, j, i, m, l)*t2a(c, a, m, k) & !dbjimlcamk      (+1.000)
                      - u8(d, c, j, i, m, l)*t2a(b, a, m, k)       !dcjimlbamk      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u8)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u9(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u9)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3b,u9) &
        !$omp private(a,b,c,d,f,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + u9(c, j, i, f, d, m)*t3b(f, b, a, m, l, k) & !cjifdmfbamlk    (+1.000)
                      - u9(b, j, i, f, d, m)*t3b(f, c, a, m, l, k) & !bjifdmfcamlk    (-1.000)
                      + u9(a, j, i, f, d, m)*t3b(f, c, b, m, l, k) & !ajifdmfcbmlk    (+1.000)
                      - u9(d, j, i, f, c, m)*t3b(f, b, a, m, l, k) & !djifcmfbamlk    (-1.000)
                      + u9(d, j, i, f, b, m)*t3b(f, c, a, m, l, k) & !djifbmfcamlk    (+1.000)
                      - u9(d, j, i, f, a, m)*t3b(f, c, b, m, l, k) & !djifamfcbmlk    (-1.000)
                      + u9(b, j, i, f, c, m)*t3b(f, d, a, m, l, k) & !bjifcmfdamlk    (+1.000)
                      - u9(a, j, i, f, c, m)*t3b(f, d, b, m, l, k) & !ajifcmfdbmlk    (-1.000)
                      - u9(c, j, i, f, b, m)*t3b(f, d, a, m, l, k) & !cjifbmfdamlk    (-1.000)
                      + u9(c, j, i, f, a, m)*t3b(f, d, b, m, l, k) & !cjifamfdbmlk    (+1.000)
                      + u9(a, j, i, f, b, m)*t3b(f, d, c, m, l, k) & !ajifbmfdcmlk    (+1.000)
                      - u9(b, j, i, f, a, m)*t3b(f, d, c, m, l, k) & !bjifamfdcmlk    (-1.000)
                      - u9(c, k, i, f, d, m)*t3b(f, b, a, m, l, j) & !ckifdmfbamlj    (-1.000)
                      + u9(b, k, i, f, d, m)*t3b(f, c, a, m, l, j) & !bkifdmfcamlj    (+1.000)
                      - u9(a, k, i, f, d, m)*t3b(f, c, b, m, l, j) & !akifdmfcbmlj    (-1.000)
                      + u9(d, k, i, f, c, m)*t3b(f, b, a, m, l, j) & !dkifcmfbamlj    (+1.000)
                      - u9(d, k, i, f, b, m)*t3b(f, c, a, m, l, j) & !dkifbmfcamlj    (-1.000)
                      + u9(d, k, i, f, a, m)*t3b(f, c, b, m, l, j) & !dkifamfcbmlj    (+1.000)
                      - u9(b, k, i, f, c, m)*t3b(f, d, a, m, l, j) & !bkifcmfdamlj    (-1.000)
                      + u9(a, k, i, f, c, m)*t3b(f, d, b, m, l, j) & !akifcmfdbmlj    (+1.000)
                      + u9(c, k, i, f, b, m)*t3b(f, d, a, m, l, j) & !ckifbmfdamlj    (+1.000)
                      - u9(c, k, i, f, a, m)*t3b(f, d, b, m, l, j) & !ckifamfdbmlj    (-1.000)
                      - u9(a, k, i, f, b, m)*t3b(f, d, c, m, l, j) & !akifbmfdcmlj    (-1.000)
                      + u9(b, k, i, f, a, m)*t3b(f, d, c, m, l, j) & !bkifamfdcmlj    (+1.000)
                      + u9(c, l, i, f, d, m)*t3b(f, b, a, m, k, j) & !clifdmfbamkj    (+1.000)
                      - u9(b, l, i, f, d, m)*t3b(f, c, a, m, k, j) & !blifdmfcamkj    (-1.000)
                      + u9(a, l, i, f, d, m)*t3b(f, c, b, m, k, j) & !alifdmfcbmkj    (+1.000)
                      - u9(d, l, i, f, c, m)*t3b(f, b, a, m, k, j) & !dlifcmfbamkj    (-1.000)
                      + u9(d, l, i, f, b, m)*t3b(f, c, a, m, k, j) & !dlifbmfcamkj    (+1.000)
                      - u9(d, l, i, f, a, m)*t3b(f, c, b, m, k, j) & !dlifamfcbmkj    (-1.000)
                      + u9(b, l, i, f, c, m)*t3b(f, d, a, m, k, j) & !blifcmfdamkj    (+1.000)
                      - u9(a, l, i, f, c, m)*t3b(f, d, b, m, k, j) & !alifcmfdbmkj    (-1.000)
                      - u9(c, l, i, f, b, m)*t3b(f, d, a, m, k, j) & !clifbmfdamkj    (-1.000)
                      + u9(c, l, i, f, a, m)*t3b(f, d, b, m, k, j) & !clifamfdbmkj    (+1.000)
                      + u9(a, l, i, f, b, m)*t3b(f, d, c, m, k, j) & !alifbmfdcmkj    (+1.000)
                      - u9(b, l, i, f, a, m)*t3b(f, d, c, m, k, j) & !blifamfdcmkj    (-1.000)
                      + u9(c, k, j, f, d, m)*t3b(f, b, a, m, l, i) & !ckjfdmfbamli    (+1.000)
                      - u9(b, k, j, f, d, m)*t3b(f, c, a, m, l, i) & !bkjfdmfcamli    (-1.000)
                      + u9(a, k, j, f, d, m)*t3b(f, c, b, m, l, i) & !akjfdmfcbmli    (+1.000)
                      - u9(d, k, j, f, c, m)*t3b(f, b, a, m, l, i) & !dkjfcmfbamli    (-1.000)
                      + u9(d, k, j, f, b, m)*t3b(f, c, a, m, l, i) & !dkjfbmfcamli    (+1.000)
                      - u9(d, k, j, f, a, m)*t3b(f, c, b, m, l, i) & !dkjfamfcbmli    (-1.000)
                      + u9(b, k, j, f, c, m)*t3b(f, d, a, m, l, i) & !bkjfcmfdamli    (+1.000)
                      - u9(a, k, j, f, c, m)*t3b(f, d, b, m, l, i) & !akjfcmfdbmli    (-1.000)
                      - u9(c, k, j, f, b, m)*t3b(f, d, a, m, l, i) & !ckjfbmfdamli    (-1.000)
                      + u9(c, k, j, f, a, m)*t3b(f, d, b, m, l, i) & !ckjfamfdbmli    (+1.000)
                      + u9(a, k, j, f, b, m)*t3b(f, d, c, m, l, i) & !akjfbmfdcmli    (+1.000)
                      - u9(b, k, j, f, a, m)*t3b(f, d, c, m, l, i) & !bkjfamfdcmli    (-1.000)
                      - u9(c, l, j, f, d, m)*t3b(f, b, a, m, k, i) & !cljfdmfbamki    (-1.000)
                      + u9(b, l, j, f, d, m)*t3b(f, c, a, m, k, i) & !bljfdmfcamki    (+1.000)
                      - u9(a, l, j, f, d, m)*t3b(f, c, b, m, k, i) & !aljfdmfcbmki    (-1.000)
                      + u9(d, l, j, f, c, m)*t3b(f, b, a, m, k, i) & !dljfcmfbamki    (+1.000)
                      - u9(d, l, j, f, b, m)*t3b(f, c, a, m, k, i) & !dljfbmfcamki    (-1.000)
                      + u9(d, l, j, f, a, m)*t3b(f, c, b, m, k, i) & !dljfamfcbmki    (+1.000)
                      - u9(b, l, j, f, c, m)*t3b(f, d, a, m, k, i) & !bljfcmfdamki    (-1.000)
                      + u9(a, l, j, f, c, m)*t3b(f, d, b, m, k, i) & !aljfcmfdbmki    (+1.000)
                      + u9(c, l, j, f, b, m)*t3b(f, d, a, m, k, i) & !cljfbmfdamki    (+1.000)
                      - u9(c, l, j, f, a, m)*t3b(f, d, b, m, k, i) & !cljfamfdbmki    (-1.000)
                      - u9(a, l, j, f, b, m)*t3b(f, d, c, m, k, i) & !aljfbmfdcmki    (-1.000)
                      + u9(b, l, j, f, a, m)*t3b(f, d, c, m, k, i) & !bljfamfdcmki    (+1.000)
                      + u9(c, l, k, f, d, m)*t3b(f, b, a, m, j, i) & !clkfdmfbamji    (+1.000)
                      - u9(b, l, k, f, d, m)*t3b(f, c, a, m, j, i) & !blkfdmfcamji    (-1.000)
                      + u9(a, l, k, f, d, m)*t3b(f, c, b, m, j, i) & !alkfdmfcbmji    (+1.000)
                      - u9(d, l, k, f, c, m)*t3b(f, b, a, m, j, i) & !dlkfcmfbamji    (-1.000)
                      + u9(d, l, k, f, b, m)*t3b(f, c, a, m, j, i) & !dlkfbmfcamji    (+1.000)
                      - u9(d, l, k, f, a, m)*t3b(f, c, b, m, j, i) & !dlkfamfcbmji    (-1.000)
                      + u9(b, l, k, f, c, m)*t3b(f, d, a, m, j, i) & !blkfcmfdamji    (+1.000)
                      - u9(a, l, k, f, c, m)*t3b(f, d, b, m, j, i) & !alkfcmfdbmji    (-1.000)
                      - u9(c, l, k, f, b, m)*t3b(f, d, a, m, j, i) & !clkfbmfdamji    (-1.000)
                      + u9(c, l, k, f, a, m)*t3b(f, d, b, m, j, i) & !clkfamfdbmji    (+1.000)
                      + u9(a, l, k, f, b, m)*t3b(f, d, c, m, j, i) & !alkfbmfdcmji    (+1.000)
                      - u9(b, l, k, f, a, m)*t3b(f, d, c, m, j, i)     !blkfamfdcmji    (-1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u9)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u10(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u10)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,u10,t4a) &
        !$omp private(a,b,c,d,n,f,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (u10(d, j, i, f, m, n)*t4a(f, c, b, a, n, m, l, k) & !djifmnfcbanmlk  (+0.500)
                         - u10(c, j, i, f, m, n)*t4a(f, d, b, a, n, m, l, k) & !cjifmnfdbanmlk  (-0.500)
                         + u10(b, j, i, f, m, n)*t4a(f, d, c, a, n, m, l, k) & !bjifmnfdcanmlk  (+0.500)
                         - u10(a, j, i, f, m, n)*t4a(f, d, c, b, n, m, l, k) & !ajifmnfdcbnmlk  (-0.500)
                         - u10(d, k, i, f, m, n)*t4a(f, c, b, a, n, m, l, j) & !dkifmnfcbanmlj  (-0.500)
                         + u10(c, k, i, f, m, n)*t4a(f, d, b, a, n, m, l, j) & !ckifmnfdbanmlj  (+0.500)
                         - u10(b, k, i, f, m, n)*t4a(f, d, c, a, n, m, l, j) & !bkifmnfdcanmlj  (-0.500)
                         + u10(a, k, i, f, m, n)*t4a(f, d, c, b, n, m, l, j) & !akifmnfdcbnmlj  (+0.500)
                         + u10(d, l, i, f, m, n)*t4a(f, c, b, a, n, m, k, j) & !dlifmnfcbanmkj  (+0.500)
                         - u10(c, l, i, f, m, n)*t4a(f, d, b, a, n, m, k, j) & !clifmnfdbanmkj  (-0.500)
                         + u10(b, l, i, f, m, n)*t4a(f, d, c, a, n, m, k, j) & !blifmnfdcanmkj  (+0.500)
                         - u10(a, l, i, f, m, n)*t4a(f, d, c, b, n, m, k, j) & !alifmnfdcbnmkj  (-0.500)
                         + u10(d, k, j, f, m, n)*t4a(f, c, b, a, n, m, l, i) & !dkjfmnfcbanmli  (+0.500)
                         - u10(c, k, j, f, m, n)*t4a(f, d, b, a, n, m, l, i) & !ckjfmnfdbanmli  (-0.500)
                         + u10(b, k, j, f, m, n)*t4a(f, d, c, a, n, m, l, i) & !bkjfmnfdcanmli  (+0.500)
                         - u10(a, k, j, f, m, n)*t4a(f, d, c, b, n, m, l, i) & !akjfmnfdcbnmli  (-0.500)
                         - u10(d, l, j, f, m, n)*t4a(f, c, b, a, n, m, k, i) & !dljfmnfcbanmki  (-0.500)
                         + u10(c, l, j, f, m, n)*t4a(f, d, b, a, n, m, k, i) & !cljfmnfdbanmki  (+0.500)
                         - u10(b, l, j, f, m, n)*t4a(f, d, c, a, n, m, k, i) & !bljfmnfdcanmki  (-0.500)
                         + u10(a, l, j, f, m, n)*t4a(f, d, c, b, n, m, k, i) & !aljfmnfdcbnmki  (+0.500)
                         + u10(d, l, k, f, m, n)*t4a(f, c, b, a, n, m, j, i) & !dlkfmnfcbanmji  (+0.500)
                         - u10(c, l, k, f, m, n)*t4a(f, d, b, a, n, m, j, i) & !clkfmnfdbanmji  (-0.500)
                         + u10(b, l, k, f, m, n)*t4a(f, d, c, a, n, m, j, i) & !blkfmnfdcanmji  (+0.500)
                         - u10(a, l, k, f, m, n)*t4a(f, d, c, b, n, m, j, i))/2.0d0!alkfmnfdcbnmji  (-0.500)
            end do; end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u10), size(u10), '465123', u10, f1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (u30(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k1
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, f1, d2, u30)
    deallocate (f1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u30) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + u30(b, l, m, d, j, i)*t2a(c, a, m, k) & !blmdjicamk      (+1.000)
                      + u30(c, k, m, d, j, i)*t2a(b, a, m, l) & !ckmdjibaml      (+1.000)
                      - u30(c, l, m, d, j, i)*t2a(b, a, m, k) & !clmdjibamk      (-1.000)
                      - u30(b, k, m, d, j, i)*t2a(c, a, m, l) & !bkmdjicaml      (-1.000)
                      - u30(b, l, m, c, j, i)*t2a(d, a, m, k) & !blmcjidamk      (-1.000)
                      - u30(d, k, m, c, j, i)*t2a(b, a, m, l) & !dkmcjibaml      (-1.000)
                      + u30(c, l, m, b, j, i)*t2a(d, a, m, k) & !clmbjidamk      (+1.000)
                      + u30(d, k, m, b, j, i)*t2a(c, a, m, l) & !dkmbjicaml      (+1.000)
                      - u30(c, l, m, a, j, i)*t2a(d, b, m, k) & !clmajidbmk      (-1.000)
                      - u30(d, k, m, a, j, i)*t2a(c, b, m, l) & !dkmajicbml      (-1.000)
                      + u30(d, l, m, c, j, i)*t2a(b, a, m, k) & !dlmcjibamk      (+1.000)
                      + u30(b, k, m, c, j, i)*t2a(d, a, m, l) & !bkmcjidaml      (+1.000)
                      - u30(d, l, m, b, j, i)*t2a(c, a, m, k) & !dlmbjicamk      (-1.000)
                      - u30(c, k, m, b, j, i)*t2a(d, a, m, l) & !ckmbjidaml      (-1.000)
                      + u30(d, l, m, a, j, i)*t2a(c, b, m, k) & !dlmajicbmk      (+1.000)
                      + u30(c, k, m, a, j, i)*t2a(d, b, m, l) & !ckmajidbml      (+1.000)
                      - u30(b, l, m, d, k, i)*t2a(c, a, m, j) & !blmdkicamj      (-1.000)
                      - u30(c, j, m, d, k, i)*t2a(b, a, m, l) & !cjmdkibaml      (-1.000)
                      + u30(c, l, m, d, k, i)*t2a(b, a, m, j) & !clmdkibamj      (+1.000)
                      + u30(b, j, m, d, k, i)*t2a(c, a, m, l) & !bjmdkicaml      (+1.000)
                      + u30(b, l, m, c, k, i)*t2a(d, a, m, j) & !blmckidamj      (+1.000)
                      + u30(d, j, m, c, k, i)*t2a(b, a, m, l) & !djmckibaml      (+1.000)
                      - u30(c, l, m, b, k, i)*t2a(d, a, m, j) & !clmbkidamj      (-1.000)
                      - u30(d, j, m, b, k, i)*t2a(c, a, m, l) & !djmbkicaml      (-1.000)
                      + u30(c, l, m, a, k, i)*t2a(d, b, m, j) & !clmakidbmj      (+1.000)
                      + u30(d, j, m, a, k, i)*t2a(c, b, m, l) & !djmakicbml      (+1.000)
                      - u30(d, l, m, c, k, i)*t2a(b, a, m, j) & !dlmckibamj      (-1.000)
                      - u30(b, j, m, c, k, i)*t2a(d, a, m, l) & !bjmckidaml      (-1.000)
                      + u30(d, l, m, b, k, i)*t2a(c, a, m, j) & !dlmbkicamj      (+1.000)
                      + u30(c, j, m, b, k, i)*t2a(d, a, m, l) & !cjmbkidaml      (+1.000)
                      - u30(d, l, m, a, k, i)*t2a(c, b, m, j) & !dlmakicbmj      (-1.000)
                      - u30(c, j, m, a, k, i)*t2a(d, b, m, l) & !cjmakidbml      (-1.000)
                      - u30(b, l, m, c, k, j)*t2a(d, a, m, i) & !blmckjdami      (-1.000)
                      - u30(d, i, m, c, k, j)*t2a(b, a, m, l) & !dimckjbaml      (-1.000)
                      - u30(c, l, m, a, k, j)*t2a(d, b, m, i) & !clmakjdbmi      (-1.000)
                      + u30(c, l, m, b, k, j)*t2a(d, a, m, i) & !clmbkjdami      (+1.000)
                      + u30(d, i, m, b, k, j)*t2a(c, a, m, l) & !dimbkjcaml      (+1.000)
                      - u30(d, i, m, a, k, j)*t2a(c, b, m, l) & !dimakjcbml      (-1.000)
                      + u30(b, l, m, d, k, j)*t2a(c, a, m, i) & !blmdkjcami      (+1.000)
                      + u30(c, i, m, d, k, j)*t2a(b, a, m, l) & !cimdkjbaml      (+1.000)
                      - u30(c, l, m, d, k, j)*t2a(b, a, m, i) & !clmdkjbami      (-1.000)
                      - u30(b, i, m, d, k, j)*t2a(c, a, m, l) & !bimdkjcaml      (-1.000)
                      + u30(d, l, m, a, k, j)*t2a(c, b, m, i) & !dlmakjcbmi      (+1.000)
                      - u30(d, l, m, b, k, j)*t2a(c, a, m, i) & !dlmbkjcami      (-1.000)
                      - u30(c, i, m, b, k, j)*t2a(d, a, m, l) & !cimbkjdaml      (-1.000)
                      + u30(c, i, m, a, k, j)*t2a(d, b, m, l) & !cimakjdbml      (+1.000)
                      + u30(d, l, m, c, k, j)*t2a(b, a, m, i) & !dlmckjbami      (+1.000)
                      + u30(b, i, m, c, k, j)*t2a(d, a, m, l) & !bimckjdaml      (+1.000)
                      + u30(b, k, m, d, l, i)*t2a(c, a, m, j) & !bkmdlicamj      (+1.000)
                      + u30(c, j, m, d, l, i)*t2a(b, a, m, k) & !cjmdlibamk      (+1.000)
                      - u30(c, k, m, d, l, i)*t2a(b, a, m, j) & !ckmdlibamj      (-1.000)
                      - u30(b, j, m, d, l, i)*t2a(c, a, m, k) & !bjmdlicamk      (-1.000)
                      - u30(b, k, m, c, l, i)*t2a(d, a, m, j) & !bkmclidamj      (-1.000)
                      - u30(d, j, m, c, l, i)*t2a(b, a, m, k) & !djmclibamk      (-1.000)
                      + u30(c, k, m, b, l, i)*t2a(d, a, m, j) & !ckmblidamj      (+1.000)
                      + u30(d, j, m, b, l, i)*t2a(c, a, m, k) & !djmblicamk      (+1.000)
                      - u30(c, k, m, a, l, i)*t2a(d, b, m, j) & !ckmalidbmj      (-1.000)
                      - u30(d, j, m, a, l, i)*t2a(c, b, m, k) & !djmalicbmk      (-1.000)
                      + u30(d, k, m, c, l, i)*t2a(b, a, m, j) & !dkmclibamj      (+1.000)
                      + u30(b, j, m, c, l, i)*t2a(d, a, m, k) & !bjmclidamk      (+1.000)
                      - u30(d, k, m, b, l, i)*t2a(c, a, m, j) & !dkmblicamj      (-1.000)
                      - u30(c, j, m, b, l, i)*t2a(d, a, m, k) & !cjmblidamk      (-1.000)
                      + u30(d, k, m, a, l, i)*t2a(c, b, m, j) & !dkmalicbmj      (+1.000)
                      + u30(c, j, m, a, l, i)*t2a(d, b, m, k) & !cjmalidbmk      (+1.000)
                      + u30(b, k, m, c, l, j)*t2a(d, a, m, i) & !bkmcljdami      (+1.000)
                      + u30(d, i, m, c, l, j)*t2a(b, a, m, k) & !dimcljbamk      (+1.000)
                      + u30(c, k, m, a, l, j)*t2a(d, b, m, i) & !ckmaljdbmi      (+1.000)
                      - u30(c, k, m, b, l, j)*t2a(d, a, m, i) & !ckmbljdami      (-1.000)
                      - u30(d, i, m, b, l, j)*t2a(c, a, m, k) & !dimbljcamk      (-1.000)
                      + u30(d, i, m, a, l, j)*t2a(c, b, m, k) & !dimaljcbmk      (+1.000)
                      - u30(b, k, m, d, l, j)*t2a(c, a, m, i) & !bkmdljcami      (-1.000)
                      - u30(c, i, m, d, l, j)*t2a(b, a, m, k) & !cimdljbamk      (-1.000)
                      + u30(c, k, m, d, l, j)*t2a(b, a, m, i) & !ckmdljbami      (+1.000)
                      + u30(b, i, m, d, l, j)*t2a(c, a, m, k) & !bimdljcamk      (+1.000)
                      - u30(d, k, m, a, l, j)*t2a(c, b, m, i) & !dkmaljcbmi      (-1.000)
                      + u30(d, k, m, b, l, j)*t2a(c, a, m, i) & !dkmbljcami      (+1.000)
                      + u30(c, i, m, b, l, j)*t2a(d, a, m, k) & !cimbljdamk      (+1.000)
                      - u30(c, i, m, a, l, j)*t2a(d, b, m, k) & !cimaljdbmk      (-1.000)
                      - u30(d, k, m, c, l, j)*t2a(b, a, m, i) & !dkmcljbami      (-1.000)
                      - u30(b, i, m, c, l, j)*t2a(d, a, m, k) & !bimcljdamk      (-1.000)
                      - u30(c, j, m, a, l, k)*t2a(d, b, m, i) & !cjmalkdbmi      (-1.000)
                      - u30(d, i, m, a, l, k)*t2a(c, b, m, j) & !dimalkcbmj      (-1.000)
                      + u30(c, j, m, b, l, k)*t2a(d, a, m, i) & !cjmblkdami      (+1.000)
                      + u30(d, i, m, b, l, k)*t2a(c, a, m, j) & !dimblkcamj      (+1.000)
                      - u30(b, j, m, c, l, k)*t2a(d, a, m, i) & !bjmclkdami      (-1.000)
                      - u30(d, i, m, c, l, k)*t2a(b, a, m, j) & !dimclkbamj      (-1.000)
                      + u30(d, j, m, a, l, k)*t2a(c, b, m, i) & !djmalkcbmi      (+1.000)
                      + u30(c, i, m, a, l, k)*t2a(d, b, m, j) & !cimalkdbmj      (+1.000)
                      - u30(d, j, m, b, l, k)*t2a(c, a, m, i) & !djmblkcami      (-1.000)
                      - u30(c, i, m, b, l, k)*t2a(d, a, m, j) & !cimblkdamj      (-1.000)
                      + u30(d, j, m, c, l, k)*t2a(b, a, m, i) & !djmclkbami      (+1.000)
                      + u30(b, i, m, c, l, k)*t2a(d, a, m, j) & !bimclkdamj      (+1.000)
                      + u30(b, j, m, d, l, k)*t2a(c, a, m, i) & !bjmdlkcami      (+1.000)
                      + u30(c, i, m, d, l, k)*t2a(b, a, m, j) & !cimdlkbamj      (+1.000)
                      - u30(c, j, m, d, l, k)*t2a(b, a, m, i) & !cjmdlkbami      (-1.000)
                      - u30(b, i, m, d, l, k)*t2a(c, a, m, j)       !bimdlkcamj      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u30)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u10), size(u10), '541236', u10, f1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u29(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1*k3
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, f1, d2, u29)
    deallocate (f1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u29) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + u29(a, l, d, j, i, n)*t2a(c, b, n, k) & !aldjincbnk      (+1.000)
                      - u29(a, k, d, j, i, n)*t2a(c, b, n, l) & !akdjincbnl      (-1.000)
                      - u29(a, l, c, j, i, n)*t2a(d, b, n, k) & !alcjindbnk      (-1.000)
                      + u29(a, l, b, j, i, n)*t2a(d, c, n, k) & !albjindcnk      (+1.000)
                      - u29(b, l, a, j, i, n)*t2a(d, c, n, k) & !blajindcnk      (-1.000)
                      + u29(a, k, c, j, i, n)*t2a(d, b, n, l) & !akcjindbnl      (+1.000)
                      - u29(a, k, b, j, i, n)*t2a(d, c, n, l) & !akbjindcnl      (-1.000)
                      + u29(b, k, a, j, i, n)*t2a(d, c, n, l) & !bkajindcnl      (+1.000)
                      - u29(a, l, d, k, i, n)*t2a(c, b, n, j) & !aldkincbnj      (-1.000)
                      + u29(a, j, d, k, i, n)*t2a(c, b, n, l) & !ajdkincbnl      (+1.000)
                      + u29(a, l, c, k, i, n)*t2a(d, b, n, j) & !alckindbnj      (+1.000)
                      - u29(a, l, b, k, i, n)*t2a(d, c, n, j) & !albkindcnj      (-1.000)
                      + u29(b, l, a, k, i, n)*t2a(d, c, n, j) & !blakindcnj      (+1.000)
                      - u29(a, j, c, k, i, n)*t2a(d, b, n, l) & !ajckindbnl      (-1.000)
                      + u29(a, j, b, k, i, n)*t2a(d, c, n, l) & !ajbkindcnl      (+1.000)
                      - u29(b, j, a, k, i, n)*t2a(d, c, n, l) & !bjakindcnl      (-1.000)
                      + u29(a, l, b, k, j, n)*t2a(d, c, n, i) & !albkjndcni      (+1.000)
                      - u29(b, l, a, k, j, n)*t2a(d, c, n, i) & !blakjndcni      (-1.000)
                      - u29(a, l, c, k, j, n)*t2a(d, b, n, i) & !alckjndbni      (-1.000)
                      + u29(a, l, d, k, j, n)*t2a(c, b, n, i) & !aldkjncbni      (+1.000)
                      - u29(a, i, d, k, j, n)*t2a(c, b, n, l) & !aidkjncbnl      (-1.000)
                      + u29(a, i, c, k, j, n)*t2a(d, b, n, l) & !aickjndbnl      (+1.000)
                      + u29(b, i, a, k, j, n)*t2a(d, c, n, l) & !biakjndcnl      (+1.000)
                      - u29(a, i, b, k, j, n)*t2a(d, c, n, l) & !aibkjndcnl      (-1.000)
                      + u29(a, k, d, l, i, n)*t2a(c, b, n, j) & !akdlincbnj      (+1.000)
                      - u29(a, j, d, l, i, n)*t2a(c, b, n, k) & !ajdlincbnk      (-1.000)
                      - u29(a, k, c, l, i, n)*t2a(d, b, n, j) & !akclindbnj      (-1.000)
                      + u29(a, k, b, l, i, n)*t2a(d, c, n, j) & !akblindcnj      (+1.000)
                      - u29(b, k, a, l, i, n)*t2a(d, c, n, j) & !bkalindcnj      (-1.000)
                      + u29(a, j, c, l, i, n)*t2a(d, b, n, k) & !ajclindbnk      (+1.000)
                      - u29(a, j, b, l, i, n)*t2a(d, c, n, k) & !ajblindcnk      (-1.000)
                      + u29(b, j, a, l, i, n)*t2a(d, c, n, k) & !bjalindcnk      (+1.000)
                      - u29(a, k, b, l, j, n)*t2a(d, c, n, i) & !akbljndcni      (-1.000)
                      + u29(b, k, a, l, j, n)*t2a(d, c, n, i) & !bkaljndcni      (+1.000)
                      + u29(a, k, c, l, j, n)*t2a(d, b, n, i) & !akcljndbni      (+1.000)
                      - u29(a, k, d, l, j, n)*t2a(c, b, n, i) & !akdljncbni      (-1.000)
                      + u29(a, i, d, l, j, n)*t2a(c, b, n, k) & !aidljncbnk      (+1.000)
                      - u29(a, i, c, l, j, n)*t2a(d, b, n, k) & !aicljndbnk      (-1.000)
                      - u29(b, i, a, l, j, n)*t2a(d, c, n, k) & !bialjndcnk      (-1.000)
                      + u29(a, i, b, l, j, n)*t2a(d, c, n, k) & !aibljndcnk      (+1.000)
                      - u29(b, j, a, l, k, n)*t2a(d, c, n, i) & !bjalkndcni      (-1.000)
                      + u29(a, j, b, l, k, n)*t2a(d, c, n, i) & !ajblkndcni      (+1.000)
                      - u29(a, j, c, l, k, n)*t2a(d, b, n, i) & !ajclkndbni      (-1.000)
                      + u29(b, i, a, l, k, n)*t2a(d, c, n, j) & !bialkndcnj      (+1.000)
                      - u29(a, i, b, l, k, n)*t2a(d, c, n, j) & !aiblkndcnj      (-1.000)
                      + u29(a, i, c, l, k, n)*t2a(d, b, n, j) & !aiclkndbnj      (+1.000)
                      + u29(a, j, d, l, k, n)*t2a(c, b, n, i) & !ajdlkncbni      (+1.000)
                      - u29(a, i, d, l, k, n)*t2a(c, b, n, j)       !aidlkncbnj      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u29)

    allocate (f1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u10), size(u10), '564123', u10, f1)
    allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '4312', t2a, d2)
    allocate (u28(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k3
    i2 = k3*k3
    i3 = k1*k1
    call egemm(i1, i2, i3, f1, d2, u28)
    deallocate (f1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u28) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum & !top 2 switched
                      + (u28(c, a, f, b, l, k)*t2a(f, d, j, i) & !cafblkfdji      (+0.500)
                         - u28(b, a, f, c, l, k)*t2a(f, d, j, i) & !bafclkfdji      (-0.500)
                         - u28(c, b, f, a, l, k)*t2a(f, d, j, i) & !cbfalkfdji      (-0.500)
                         - u28(b, a, f, c, j, i)*t2a(f, d, l, k) & !bafcjifdlk      (-0.500)
                         + u28(c, a, f, b, j, i)*t2a(f, d, l, k) & !cafbjifdlk      (+0.500)
                         - u28(c, b, f, a, j, i)*t2a(f, d, l, k) & !cbfajifdlk      (-0.500)
                         - u28(d, a, f, b, l, k)*t2a(f, c, j, i) & !dafblkfcji      (-0.500)
                         + u28(d, b, f, a, l, k)*t2a(f, c, j, i) & !dbfalkfcji      (+0.500)
                         - u28(d, a, f, b, j, i)*t2a(f, c, l, k) & !dafbjifclk      (-0.500)
                         + u28(d, b, f, a, j, i)*t2a(f, c, l, k) & !dbfajifclk      (+0.500)
                         - u28(d, c, f, a, l, k)*t2a(f, b, j, i) & !dcfalkfbji      (-0.500)
                         - u28(d, c, f, a, j, i)*t2a(f, b, l, k) & !dcfajifblk      (-0.500)
                         + u28(b, a, f, c, l, j)*t2a(f, d, k, i) & !bafcljfdki      (+0.500)
                         - u28(c, a, f, b, l, j)*t2a(f, d, k, i) & !cafbljfdki      (-0.500)
                         + u28(c, b, f, a, l, j)*t2a(f, d, k, i) & !cbfaljfdki      (+0.500)
                         + u28(b, a, f, c, k, i)*t2a(f, d, l, j) & !bafckifdlj      (+0.500)
                         - u28(c, a, f, b, k, i)*t2a(f, d, l, j) & !cafbkifdlj      (-0.500)
                         + u28(c, b, f, a, k, i)*t2a(f, d, l, j) & !cbfakifdlj      (+0.500)
                         + u28(d, a, f, b, l, j)*t2a(f, c, k, i) & !dafbljfcki      (+0.500)
                         - u28(d, b, f, a, l, j)*t2a(f, c, k, i) & !dbfaljfcki      (-0.500)
                         + u28(d, a, f, b, k, i)*t2a(f, c, l, j) & !dafbkifclj      (+0.500)
                         - u28(d, b, f, a, k, i)*t2a(f, c, l, j) & !dbfakifclj      (-0.500)
                         + u28(d, c, f, a, l, j)*t2a(f, b, k, i) & !dcfaljfbki      (+0.500)
                         + u28(d, c, f, a, k, i)*t2a(f, b, l, j) & !dcfakifblj      (+0.500)
                         - u28(b, a, f, c, k, j)*t2a(f, d, l, i) & !bafckjfdli      (-0.500)
                         + u28(c, a, f, b, k, j)*t2a(f, d, l, i) & !cafbkjfdli      (+0.500)
                         - u28(c, b, f, a, k, j)*t2a(f, d, l, i) & !cbfakjfdli      (-0.500)
                         - u28(b, a, f, c, l, i)*t2a(f, d, k, j) & !bafclifdkj      (-0.500)
                         + u28(c, a, f, b, l, i)*t2a(f, d, k, j) & !cafblifdkj      (+0.500)
                         - u28(c, b, f, a, l, i)*t2a(f, d, k, j) & !cbfalifdkj      (-0.500)
                         - u28(d, a, f, b, k, j)*t2a(f, c, l, i) & !dafbkjfcli      (-0.500)
                         + u28(d, b, f, a, k, j)*t2a(f, c, l, i) & !dbfakjfcli      (+0.500)
                         - u28(d, a, f, b, l, i)*t2a(f, c, k, j) & !dafblifckj      (-0.500)
                         + u28(d, b, f, a, l, i)*t2a(f, c, k, j) & !dbfalifckj      (+0.500)
                         - u28(d, c, f, a, k, j)*t2a(f, b, l, i) & !dcfakjfbli      (-0.500)
                         - u28(d, c, f, a, l, i)*t2a(f, b, k, j))/2.0d0!dcfalifbkj      (-0.500)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u28)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u10), size(u10), '541236', u10, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s43(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1*k3
    i3 = k3*k1
    call egemm1(i1, i3, f1, b2, s43)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2341', -1.000, &
                    x1, s43)
    deallocate (s43)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u10), size(u10), '541236', u10, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u24(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u24)
    deallocate (f1)
    deallocate (b2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,u24) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do n = n0 + 1, n1
                sum = sum &
                      - u24(d, f, c, j, i, n)*t3a(f, b, a, n, l, k) & !dfcjinfbanlk    (-1.000)
                      + u24(d, f, b, j, i, n)*t3a(f, c, a, n, l, k) & !dfbjinfcanlk    (+1.000)
                      - u24(d, f, a, j, i, n)*t3a(f, c, b, n, l, k) & !dfajinfcbnlk    (-1.000)
                      + u24(c, f, d, j, i, n)*t3a(f, b, a, n, l, k) & !cfdjinfbanlk    (+1.000)
                      - u24(b, f, d, j, i, n)*t3a(f, c, a, n, l, k) & !bfdjinfcanlk    (-1.000)
                      + u24(a, f, d, j, i, n)*t3a(f, c, b, n, l, k) & !afdjinfcbnlk    (+1.000)
                      - u24(c, f, b, j, i, n)*t3a(f, d, a, n, l, k) & !cfbjinfdanlk    (-1.000)
                      + u24(c, f, a, j, i, n)*t3a(f, d, b, n, l, k) & !cfajinfdbnlk    (+1.000)
                      + u24(b, f, c, j, i, n)*t3a(f, d, a, n, l, k) & !bfcjinfdanlk    (+1.000)
                      - u24(a, f, c, j, i, n)*t3a(f, d, b, n, l, k) & !afcjinfdbnlk    (-1.000)
                      - u24(b, f, a, j, i, n)*t3a(f, d, c, n, l, k) & !bfajinfdcnlk    (-1.000)
                      + u24(a, f, b, j, i, n)*t3a(f, d, c, n, l, k) & !afbjinfdcnlk    (+1.000)
                      + u24(d, f, c, k, i, n)*t3a(f, b, a, n, l, j) & !dfckinfbanlj    (+1.000)
                      - u24(d, f, b, k, i, n)*t3a(f, c, a, n, l, j) & !dfbkinfcanlj    (-1.000)
                      + u24(d, f, a, k, i, n)*t3a(f, c, b, n, l, j) & !dfakinfcbnlj    (+1.000)
                      - u24(c, f, d, k, i, n)*t3a(f, b, a, n, l, j) & !cfdkinfbanlj    (-1.000)
                      + u24(b, f, d, k, i, n)*t3a(f, c, a, n, l, j) & !bfdkinfcanlj    (+1.000)
                      - u24(a, f, d, k, i, n)*t3a(f, c, b, n, l, j) & !afdkinfcbnlj    (-1.000)
                      + u24(c, f, b, k, i, n)*t3a(f, d, a, n, l, j) & !cfbkinfdanlj    (+1.000)
                      - u24(c, f, a, k, i, n)*t3a(f, d, b, n, l, j) & !cfakinfdbnlj    (-1.000)
                      - u24(b, f, c, k, i, n)*t3a(f, d, a, n, l, j) & !bfckinfdanlj    (-1.000)
                      + u24(a, f, c, k, i, n)*t3a(f, d, b, n, l, j) & !afckinfdbnlj    (+1.000)
                      + u24(b, f, a, k, i, n)*t3a(f, d, c, n, l, j) & !bfakinfdcnlj    (+1.000)
                      - u24(a, f, b, k, i, n)*t3a(f, d, c, n, l, j) & !afbkinfdcnlj    (-1.000)
                      - u24(d, f, c, l, i, n)*t3a(f, b, a, n, k, j) & !dfclinfbankj    (-1.000)
                      + u24(d, f, b, l, i, n)*t3a(f, c, a, n, k, j) & !dfblinfcankj    (+1.000)
                      - u24(d, f, a, l, i, n)*t3a(f, c, b, n, k, j) & !dfalinfcbnkj    (-1.000)
                      + u24(c, f, d, l, i, n)*t3a(f, b, a, n, k, j) & !cfdlinfbankj    (+1.000)
                      - u24(b, f, d, l, i, n)*t3a(f, c, a, n, k, j) & !bfdlinfcankj    (-1.000)
                      + u24(a, f, d, l, i, n)*t3a(f, c, b, n, k, j) & !afdlinfcbnkj    (+1.000)
                      - u24(c, f, b, l, i, n)*t3a(f, d, a, n, k, j) & !cfblinfdankj    (-1.000)
                      + u24(c, f, a, l, i, n)*t3a(f, d, b, n, k, j) & !cfalinfdbnkj    (+1.000)
                      + u24(b, f, c, l, i, n)*t3a(f, d, a, n, k, j) & !bfclinfdankj    (+1.000)
                      - u24(a, f, c, l, i, n)*t3a(f, d, b, n, k, j) & !afclinfdbnkj    (-1.000)
                      - u24(b, f, a, l, i, n)*t3a(f, d, c, n, k, j) & !bfalinfdcnkj    (-1.000)
                      + u24(a, f, b, l, i, n)*t3a(f, d, c, n, k, j) & !afblinfdcnkj    (+1.000)
                      - u24(d, f, c, k, j, n)*t3a(f, b, a, n, l, i) & !dfckjnfbanli    (-1.000)
                      + u24(d, f, b, k, j, n)*t3a(f, c, a, n, l, i) & !dfbkjnfcanli    (+1.000)
                      - u24(d, f, a, k, j, n)*t3a(f, c, b, n, l, i) & !dfakjnfcbnli    (-1.000)
                      + u24(c, f, d, k, j, n)*t3a(f, b, a, n, l, i) & !cfdkjnfbanli    (+1.000)
                      - u24(b, f, d, k, j, n)*t3a(f, c, a, n, l, i) & !bfdkjnfcanli    (-1.000)
                      + u24(a, f, d, k, j, n)*t3a(f, c, b, n, l, i) & !afdkjnfcbnli    (+1.000)
                      - u24(c, f, b, k, j, n)*t3a(f, d, a, n, l, i) & !cfbkjnfdanli    (-1.000)
                      + u24(c, f, a, k, j, n)*t3a(f, d, b, n, l, i) & !cfakjnfdbnli    (+1.000)
                      + u24(b, f, c, k, j, n)*t3a(f, d, a, n, l, i) & !bfckjnfdanli    (+1.000)
                      - u24(a, f, c, k, j, n)*t3a(f, d, b, n, l, i) & !afckjnfdbnli    (-1.000)
                      - u24(b, f, a, k, j, n)*t3a(f, d, c, n, l, i) & !bfakjnfdcnli    (-1.000)
                      + u24(a, f, b, k, j, n)*t3a(f, d, c, n, l, i) & !afbkjnfdcnli    (+1.000)
                      + u24(d, f, c, l, j, n)*t3a(f, b, a, n, k, i) & !dfcljnfbanki    (+1.000)
                      - u24(d, f, b, l, j, n)*t3a(f, c, a, n, k, i) & !dfbljnfcanki    (-1.000)
                      + u24(d, f, a, l, j, n)*t3a(f, c, b, n, k, i) & !dfaljnfcbnki    (+1.000)
                      - u24(c, f, d, l, j, n)*t3a(f, b, a, n, k, i) & !cfdljnfbanki    (-1.000)
                      + u24(b, f, d, l, j, n)*t3a(f, c, a, n, k, i) & !bfdljnfcanki    (+1.000)
                      - u24(a, f, d, l, j, n)*t3a(f, c, b, n, k, i) & !afdljnfcbnki    (-1.000)
                      + u24(c, f, b, l, j, n)*t3a(f, d, a, n, k, i) & !cfbljnfdanki    (+1.000)
                      - u24(c, f, a, l, j, n)*t3a(f, d, b, n, k, i) & !cfaljnfdbnki    (-1.000)
                      - u24(b, f, c, l, j, n)*t3a(f, d, a, n, k, i) & !bfcljnfdanki    (-1.000)
                      + u24(a, f, c, l, j, n)*t3a(f, d, b, n, k, i) & !afcljnfdbnki    (+1.000)
                      + u24(b, f, a, l, j, n)*t3a(f, d, c, n, k, i) & !bfaljnfdcnki    (+1.000)
                      - u24(a, f, b, l, j, n)*t3a(f, d, c, n, k, i) & !afbljnfdcnki    (-1.000)
                      - u24(d, f, c, l, k, n)*t3a(f, b, a, n, j, i) & !dfclknfbanji    (-1.000)
                      + u24(d, f, b, l, k, n)*t3a(f, c, a, n, j, i) & !dfblknfcanji    (+1.000)
                      - u24(d, f, a, l, k, n)*t3a(f, c, b, n, j, i) & !dfalknfcbnji    (-1.000)
                      + u24(c, f, d, l, k, n)*t3a(f, b, a, n, j, i) & !cfdlknfbanji    (+1.000)
                      - u24(b, f, d, l, k, n)*t3a(f, c, a, n, j, i) & !bfdlknfcanji    (-1.000)
                      + u24(a, f, d, l, k, n)*t3a(f, c, b, n, j, i) & !afdlknfcbnji    (+1.000)
                      - u24(c, f, b, l, k, n)*t3a(f, d, a, n, j, i) & !cfblknfdanji    (-1.000)
                      + u24(c, f, a, l, k, n)*t3a(f, d, b, n, j, i) & !cfalknfdbnji    (+1.000)
                      + u24(b, f, c, l, k, n)*t3a(f, d, a, n, j, i) & !bfclknfdanji    (+1.000)
                      - u24(a, f, c, l, k, n)*t3a(f, d, b, n, j, i) & !afclknfdbnji    (-1.000)
                      - u24(b, f, a, l, k, n)*t3a(f, d, c, n, j, i) & !bfalknfdcnji    (-1.000)
                      + u24(a, f, b, l, k, n)*t3a(f, d, c, n, j, i)     !afblknfdcnji    (+1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u24), size(u24), '621345', u24, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u35(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u35)
    deallocate (f1)
    deallocate (b2)
    deallocate (u24)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u35) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum &
                      - u35(d, f, c, a, l, k)*t2a(f, b, j, i) & !dfcalkfbji      (-1.000)
                      - u35(d, f, c, a, j, i)*t2a(f, b, l, k) & !dfcajifblk      (-1.000)
                      + u35(d, f, b, a, l, k)*t2a(f, c, j, i) & !dfbalkfcji      (+1.000)
                      - u35(d, f, a, b, l, k)*t2a(f, c, j, i) & !dfablkfcji      (-1.000)
                      + u35(d, f, b, a, j, i)*t2a(f, c, l, k) & !dfbajifclk      (+1.000)
                      - u35(d, f, a, b, j, i)*t2a(f, c, l, k) & !dfabjifclk      (-1.000)
                      - u35(c, f, b, a, l, k)*t2a(f, d, j, i) & !cfbalkfdji      (-1.000)
                      + u35(c, f, a, b, l, k)*t2a(f, d, j, i) & !cfablkfdji      (+1.000)
                      - u35(b, f, a, c, l, k)*t2a(f, d, j, i) & !bfaclkfdji      (-1.000)
                      - u35(c, f, b, a, j, i)*t2a(f, d, l, k) & !cfbajifdlk      (-1.000)
                      + u35(c, f, a, b, j, i)*t2a(f, d, l, k) & !cfabjifdlk      (+1.000)
                      - u35(b, f, a, c, j, i)*t2a(f, d, l, k) & !bfacjifdlk      (-1.000)
                      + u35(d, f, c, a, l, j)*t2a(f, b, k, i) & !dfcaljfbki      (+1.000)
                      + u35(d, f, c, a, k, i)*t2a(f, b, l, j) & !dfcakifblj      (+1.000)
                      - u35(d, f, b, a, l, j)*t2a(f, c, k, i) & !dfbaljfcki      (-1.000)
                      + u35(d, f, a, b, l, j)*t2a(f, c, k, i) & !dfabljfcki      (+1.000)
                      - u35(d, f, b, a, k, i)*t2a(f, c, l, j) & !dfbakifclj      (-1.000)
                      + u35(d, f, a, b, k, i)*t2a(f, c, l, j) & !dfabkifclj      (+1.000)
                      + u35(c, f, b, a, l, j)*t2a(f, d, k, i) & !cfbaljfdki      (+1.000)
                      - u35(c, f, a, b, l, j)*t2a(f, d, k, i) & !cfabljfdki      (-1.000)
                      + u35(b, f, a, c, l, j)*t2a(f, d, k, i) & !bfacljfdki      (+1.000)
                      + u35(c, f, b, a, k, i)*t2a(f, d, l, j) & !cfbakifdlj      (+1.000)
                      - u35(c, f, a, b, k, i)*t2a(f, d, l, j) & !cfabkifdlj      (-1.000)
                      + u35(b, f, a, c, k, i)*t2a(f, d, l, j) & !bfackifdlj      (+1.000)
                      - u35(d, f, c, a, k, j)*t2a(f, b, l, i) & !dfcakjfbli      (-1.000)
                      - u35(d, f, c, a, l, i)*t2a(f, b, k, j) & !dfcalifbkj      (-1.000)
                      + u35(d, f, b, a, k, j)*t2a(f, c, l, i) & !dfbakjfcli      (+1.000)
                      - u35(d, f, a, b, k, j)*t2a(f, c, l, i) & !dfabkjfcli      (-1.000)
                      + u35(d, f, b, a, l, i)*t2a(f, c, k, j) & !dfbalifckj      (+1.000)
                      - u35(d, f, a, b, l, i)*t2a(f, c, k, j) & !dfablifckj      (-1.000)
                      - u35(c, f, b, a, k, j)*t2a(f, d, l, i) & !cfbakjfdli      (-1.000)
                      + u35(c, f, a, b, k, j)*t2a(f, d, l, i) & !cfabkjfdli      (+1.000)
                      - u35(b, f, a, c, k, j)*t2a(f, d, l, i) & !bfackjfdli      (-1.000)
                      - u35(c, f, b, a, l, i)*t2a(f, d, k, j) & !cfbalifdkj      (-1.000)
                      + u35(c, f, a, b, l, i)*t2a(f, d, k, j) & !cfablifdkj      (+1.000)
                      - u35(b, f, a, c, l, i)*t2a(f, d, k, j)       !bfaclifdkj      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u35)

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u10), size(u10), '451236', u10, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u22(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u22)
    deallocate (f1)
    deallocate (b2)
    deallocate (u10)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,u22) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum & !top 2 switched
                      + (u22(i, m, c, k, j, n)*t3a(d, b, a, n, m, l) & !imckjndbanml    (+0.500)
                         - u22(i, m, d, k, j, n)*t3a(c, b, a, n, m, l) & !imdkjncbanml    (-0.500)
                         - u22(i, m, b, k, j, n)*t3a(d, c, a, n, m, l) & !imbkjndcanml    (-0.500)
                         + u22(i, m, a, k, j, n)*t3a(d, c, b, n, m, l) & !imakjndcbnml    (+0.500)
                         + u22(i, m, d, l, j, n)*t3a(c, b, a, n, m, k) & !imdljncbanmk    (+0.500)
                         - u22(i, m, c, l, j, n)*t3a(d, b, a, n, m, k) & !imcljndbanmk    (-0.500)
                         + u22(i, m, b, l, j, n)*t3a(d, c, a, n, m, k) & !imbljndcanmk    (+0.500)
                         - u22(i, m, a, l, j, n)*t3a(d, c, b, n, m, k) & !imaljndcbnmk    (-0.500)
                         - u22(i, m, d, l, k, n)*t3a(c, b, a, n, m, j) & !imdlkncbanmj    (-0.500)
                         + u22(i, m, c, l, k, n)*t3a(d, b, a, n, m, j) & !imclkndbanmj    (+0.500)
                         - u22(i, m, b, l, k, n)*t3a(d, c, a, n, m, j) & !imblkndcanmj    (-0.500)
                         + u22(i, m, a, l, k, n)*t3a(d, c, b, n, m, j) & !imalkndcbnmj    (+0.500)
                         + u22(j, m, d, k, i, n)*t3a(c, b, a, n, m, l) & !jmdkincbanml    (+0.500)
                         - u22(j, m, c, k, i, n)*t3a(d, b, a, n, m, l) & !jmckindbanml    (-0.500)
                         + u22(j, m, b, k, i, n)*t3a(d, c, a, n, m, l) & !jmbkindcanml    (+0.500)
                         - u22(j, m, a, k, i, n)*t3a(d, c, b, n, m, l) & !jmakindcbnml    (-0.500)
                         - u22(j, m, d, l, i, n)*t3a(c, b, a, n, m, k) & !jmdlincbanmk    (-0.500)
                         + u22(j, m, c, l, i, n)*t3a(d, b, a, n, m, k) & !jmclindbanmk    (+0.500)
                         - u22(j, m, b, l, i, n)*t3a(d, c, a, n, m, k) & !jmblindcanmk    (-0.500)
                         + u22(j, m, a, l, i, n)*t3a(d, c, b, n, m, k) & !jmalindcbnmk    (+0.500)
                         - u22(k, m, d, j, i, n)*t3a(c, b, a, n, m, l) & !kmdjincbanml    (-0.500)
                         + u22(k, m, c, j, i, n)*t3a(d, b, a, n, m, l) & !kmcjindbanml    (+0.500)
                         - u22(k, m, b, j, i, n)*t3a(d, c, a, n, m, l) & !kmbjindcanml    (-0.500)
                         + u22(k, m, a, j, i, n)*t3a(d, c, b, n, m, l) & !kmajindcbnml    (+0.500)
                         + u22(l, m, d, j, i, n)*t3a(c, b, a, n, m, k) & !lmdjincbanmk    (+0.500)
                         - u22(l, m, c, j, i, n)*t3a(d, b, a, n, m, k) & !lmcjindbanmk    (-0.500)
                         + u22(l, m, b, j, i, n)*t3a(d, c, a, n, m, k) & !lmbjindcanmk    (+0.500)
                         - u22(l, m, a, j, i, n)*t3a(d, c, b, n, m, k) & !lmajindcbnmk    (-0.500)
                         + u22(k, m, d, l, i, n)*t3a(c, b, a, n, m, j) & !kmdlincbanmj    (+0.500)
                         - u22(k, m, c, l, i, n)*t3a(d, b, a, n, m, j) & !kmclindbanmj    (-0.500)
                         + u22(k, m, b, l, i, n)*t3a(d, c, a, n, m, j) & !kmblindcanmj    (+0.500)
                         - u22(k, m, a, l, i, n)*t3a(d, c, b, n, m, j) & !kmalindcbnmj    (-0.500)
                         - u22(l, m, d, k, i, n)*t3a(c, b, a, n, m, j) & !lmdkincbanmj    (-0.500)
                         + u22(l, m, c, k, i, n)*t3a(d, b, a, n, m, j) & !lmckindbanmj    (+0.500)
                         - u22(l, m, b, k, i, n)*t3a(d, c, a, n, m, j) & !lmbkindcanmj    (-0.500)
                         + u22(l, m, a, k, i, n)*t3a(d, c, b, n, m, j) & !lmakindcbnmj    (+0.500)
                         + u22(j, m, d, l, k, n)*t3a(c, b, a, n, m, i) & !jmdlkncbanmi    (+0.500)
                         - u22(j, m, c, l, k, n)*t3a(d, b, a, n, m, i) & !jmclkndbanmi    (-0.500)
                         + u22(j, m, b, l, k, n)*t3a(d, c, a, n, m, i) & !jmblkndcanmi    (+0.500)
                         - u22(j, m, a, l, k, n)*t3a(d, c, b, n, m, i) & !jmalkndcbnmi    (-0.500)
                         - u22(k, m, d, l, j, n)*t3a(c, b, a, n, m, i) & !kmdljncbanmi    (-0.500)
                         + u22(k, m, c, l, j, n)*t3a(d, b, a, n, m, i) & !kmcljndbanmi    (+0.500)
                         - u22(k, m, b, l, j, n)*t3a(d, c, a, n, m, i) & !kmbljndcanmi    (-0.500)
                         + u22(k, m, a, l, j, n)*t3a(d, c, b, n, m, i) & !kmaljndcbnmi    (+0.500)
                         + u22(l, m, d, k, j, n)*t3a(c, b, a, n, m, i) & !lmdkjncbanmi    (+0.500)
                         - u22(l, m, c, k, j, n)*t3a(d, b, a, n, m, i) & !lmckjndbanmi    (-0.500)
                         + u22(l, m, b, k, j, n)*t3a(d, c, a, n, m, i) & !lmbkjndcanmi    (+0.500)
                         - u22(l, m, a, k, j, n)*t3a(d, c, b, n, m, i))/2.0d0!lmakjndcbnmi    (-0.500)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u22), size(u22), '263451', u22, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u34(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1*k3*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u34)
    deallocate (f1)
    deallocate (b2)
    deallocate (u22)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u34) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      - u34(d, n, c, k, j, i)*t2a(b, a, n, l) & !dnckjibanl      (-1.000)
                      + u34(d, n, b, k, j, i)*t2a(c, a, n, l) & !dnbkjicanl      (+1.000)
                      - u34(d, n, a, k, j, i)*t2a(c, b, n, l) & !dnakjicbnl      (-1.000)
                      + u34(c, n, d, k, j, i)*t2a(b, a, n, l) & !cndkjibanl      (+1.000)
                      - u34(b, n, d, k, j, i)*t2a(c, a, n, l) & !bndkjicanl      (-1.000)
                      + u34(a, n, d, k, j, i)*t2a(c, b, n, l) & !andkjicbnl      (+1.000)
                      - u34(c, n, b, k, j, i)*t2a(d, a, n, l) & !cnbkjidanl      (-1.000)
                      + u34(c, n, a, k, j, i)*t2a(d, b, n, l) & !cnakjidbnl      (+1.000)
                      + u34(b, n, c, k, j, i)*t2a(d, a, n, l) & !bnckjidanl      (+1.000)
                      - u34(a, n, c, k, j, i)*t2a(d, b, n, l) & !anckjidbnl      (-1.000)
                      - u34(b, n, a, k, j, i)*t2a(d, c, n, l) & !bnakjidcnl      (-1.000)
                      + u34(a, n, b, k, j, i)*t2a(d, c, n, l) & !anbkjidcnl      (+1.000)
                      + u34(d, n, c, l, j, i)*t2a(b, a, n, k) & !dncljibank      (+1.000)
                      - u34(d, n, b, l, j, i)*t2a(c, a, n, k) & !dnbljicank      (-1.000)
                      + u34(d, n, a, l, j, i)*t2a(c, b, n, k) & !dnaljicbnk      (+1.000)
                      - u34(c, n, d, l, j, i)*t2a(b, a, n, k) & !cndljibank      (-1.000)
                      + u34(b, n, d, l, j, i)*t2a(c, a, n, k) & !bndljicank      (+1.000)
                      - u34(a, n, d, l, j, i)*t2a(c, b, n, k) & !andljicbnk      (-1.000)
                      + u34(c, n, b, l, j, i)*t2a(d, a, n, k) & !cnbljidank      (+1.000)
                      - u34(c, n, a, l, j, i)*t2a(d, b, n, k) & !cnaljidbnk      (-1.000)
                      - u34(b, n, c, l, j, i)*t2a(d, a, n, k) & !bncljidank      (-1.000)
                      + u34(a, n, c, l, j, i)*t2a(d, b, n, k) & !ancljidbnk      (+1.000)
                      + u34(b, n, a, l, j, i)*t2a(d, c, n, k) & !bnaljidcnk      (+1.000)
                      - u34(a, n, b, l, j, i)*t2a(d, c, n, k) & !anbljidcnk      (-1.000)
                      - u34(d, n, a, l, k, i)*t2a(c, b, n, j) & !dnalkicbnj      (-1.000)
                      + u34(d, n, b, l, k, i)*t2a(c, a, n, j) & !dnblkicanj      (+1.000)
                      - u34(d, n, c, l, k, i)*t2a(b, a, n, j) & !dnclkibanj      (-1.000)
                      + u34(c, n, a, l, k, i)*t2a(d, b, n, j) & !cnalkidbnj      (+1.000)
                      - u34(c, n, b, l, k, i)*t2a(d, a, n, j) & !cnblkidanj      (-1.000)
                      - u34(b, n, a, l, k, i)*t2a(d, c, n, j) & !bnalkidcnj      (-1.000)
                      + u34(a, n, b, l, k, i)*t2a(d, c, n, j) & !anblkidcnj      (+1.000)
                      + u34(b, n, c, l, k, i)*t2a(d, a, n, j) & !bnclkidanj      (+1.000)
                      - u34(a, n, c, l, k, i)*t2a(d, b, n, j) & !anclkidbnj      (-1.000)
                      + u34(c, n, d, l, k, i)*t2a(b, a, n, j) & !cndlkibanj      (+1.000)
                      - u34(b, n, d, l, k, i)*t2a(c, a, n, j) & !bndlkicanj      (-1.000)
                      + u34(a, n, d, l, k, i)*t2a(c, b, n, j) & !andlkicbnj      (+1.000)
                      + u34(d, n, c, k, i, j)*t2a(b, a, n, l) & !dnckijbanl      (+1.000)
                      - u34(d, n, b, k, i, j)*t2a(c, a, n, l) & !dnbkijcanl      (-1.000)
                      + u34(d, n, a, k, i, j)*t2a(c, b, n, l) & !dnakijcbnl      (+1.000)
                      - u34(c, n, d, k, i, j)*t2a(b, a, n, l) & !cndkijbanl      (-1.000)
                      + u34(b, n, d, k, i, j)*t2a(c, a, n, l) & !bndkijcanl      (+1.000)
                      - u34(a, n, d, k, i, j)*t2a(c, b, n, l) & !andkijcbnl      (-1.000)
                      + u34(c, n, b, k, i, j)*t2a(d, a, n, l) & !cnbkijdanl      (+1.000)
                      - u34(c, n, a, k, i, j)*t2a(d, b, n, l) & !cnakijdbnl      (-1.000)
                      - u34(b, n, c, k, i, j)*t2a(d, a, n, l) & !bnckijdanl      (-1.000)
                      + u34(a, n, c, k, i, j)*t2a(d, b, n, l) & !anckijdbnl      (+1.000)
                      + u34(b, n, a, k, i, j)*t2a(d, c, n, l) & !bnakijdcnl      (+1.000)
                      - u34(a, n, b, k, i, j)*t2a(d, c, n, l) & !anbkijdcnl      (-1.000)
                      - u34(d, n, c, l, i, j)*t2a(b, a, n, k) & !dnclijbank      (-1.000)
                      + u34(d, n, b, l, i, j)*t2a(c, a, n, k) & !dnblijcank      (+1.000)
                      - u34(d, n, a, l, i, j)*t2a(c, b, n, k) & !dnalijcbnk      (-1.000)
                      + u34(c, n, d, l, i, j)*t2a(b, a, n, k) & !cndlijbank      (+1.000)
                      - u34(b, n, d, l, i, j)*t2a(c, a, n, k) & !bndlijcank      (-1.000)
                      + u34(a, n, d, l, i, j)*t2a(c, b, n, k) & !andlijcbnk      (+1.000)
                      - u34(c, n, b, l, i, j)*t2a(d, a, n, k) & !cnblijdank      (-1.000)
                      + u34(c, n, a, l, i, j)*t2a(d, b, n, k) & !cnalijdbnk      (+1.000)
                      + u34(b, n, c, l, i, j)*t2a(d, a, n, k) & !bnclijdank      (+1.000)
                      - u34(a, n, c, l, i, j)*t2a(d, b, n, k) & !anclijdbnk      (-1.000)
                      - u34(b, n, a, l, i, j)*t2a(d, c, n, k) & !bnalijdcnk      (-1.000)
                      + u34(a, n, b, l, i, j)*t2a(d, c, n, k) & !anblijdcnk      (+1.000)
                      + u34(d, n, a, l, k, j)*t2a(c, b, n, i) & !dnalkjcbni      (+1.000)
                      - u34(d, n, b, l, k, j)*t2a(c, a, n, i) & !dnblkjcani      (-1.000)
                      + u34(d, n, c, l, k, j)*t2a(b, a, n, i) & !dnclkjbani      (+1.000)
                      - u34(c, n, a, l, k, j)*t2a(d, b, n, i) & !cnalkjdbni      (-1.000)
                      + u34(c, n, b, l, k, j)*t2a(d, a, n, i) & !cnblkjdani      (+1.000)
                      + u34(b, n, a, l, k, j)*t2a(d, c, n, i) & !bnalkjdcni      (+1.000)
                      - u34(a, n, b, l, k, j)*t2a(d, c, n, i) & !anblkjdcni      (-1.000)
                      - u34(b, n, c, l, k, j)*t2a(d, a, n, i) & !bnclkjdani      (-1.000)
                      + u34(a, n, c, l, k, j)*t2a(d, b, n, i) & !anclkjdbni      (+1.000)
                      - u34(c, n, d, l, k, j)*t2a(b, a, n, i) & !cndlkjbani      (-1.000)
                      + u34(b, n, d, l, k, j)*t2a(c, a, n, i) & !bndlkjcani      (+1.000)
                      - u34(a, n, d, l, k, j)*t2a(c, b, n, i) & !andlkjcbni      (-1.000)
                      - u34(d, n, c, j, i, k)*t2a(b, a, n, l) & !dncjikbanl      (-1.000)
                      + u34(d, n, b, j, i, k)*t2a(c, a, n, l) & !dnbjikcanl      (+1.000)
                      - u34(d, n, a, j, i, k)*t2a(c, b, n, l) & !dnajikcbnl      (-1.000)
                      + u34(c, n, d, j, i, k)*t2a(b, a, n, l) & !cndjikbanl      (+1.000)
                      - u34(b, n, d, j, i, k)*t2a(c, a, n, l) & !bndjikcanl      (-1.000)
                      + u34(a, n, d, j, i, k)*t2a(c, b, n, l) & !andjikcbnl      (+1.000)
                      - u34(c, n, b, j, i, k)*t2a(d, a, n, l) & !cnbjikdanl      (-1.000)
                      + u34(c, n, a, j, i, k)*t2a(d, b, n, l) & !cnajikdbnl      (+1.000)
                      + u34(b, n, c, j, i, k)*t2a(d, a, n, l) & !bncjikdanl      (+1.000)
                      - u34(a, n, c, j, i, k)*t2a(d, b, n, l) & !ancjikdbnl      (-1.000)
                      - u34(b, n, a, j, i, k)*t2a(d, c, n, l) & !bnajikdcnl      (-1.000)
                      + u34(a, n, b, j, i, k)*t2a(d, c, n, l) & !anbjikdcnl      (+1.000)
                      + u34(d, n, c, j, i, l)*t2a(b, a, n, k) & !dncjilbank      (+1.000)
                      - u34(d, n, b, j, i, l)*t2a(c, a, n, k) & !dnbjilcank      (-1.000)
                      + u34(d, n, a, j, i, l)*t2a(c, b, n, k) & !dnajilcbnk      (+1.000)
                      - u34(c, n, d, j, i, l)*t2a(b, a, n, k) & !cndjilbank      (-1.000)
                      + u34(b, n, d, j, i, l)*t2a(c, a, n, k) & !bndjilcank      (+1.000)
                      - u34(a, n, d, j, i, l)*t2a(c, b, n, k) & !andjilcbnk      (-1.000)
                      + u34(c, n, b, j, i, l)*t2a(d, a, n, k) & !cnbjildank      (+1.000)
                      - u34(c, n, a, j, i, l)*t2a(d, b, n, k) & !cnajildbnk      (-1.000)
                      - u34(b, n, c, j, i, l)*t2a(d, a, n, k) & !bncjildank      (-1.000)
                      + u34(a, n, c, j, i, l)*t2a(d, b, n, k) & !ancjildbnk      (+1.000)
                      + u34(b, n, a, j, i, l)*t2a(d, c, n, k) & !bnajildcnk      (+1.000)
                      - u34(a, n, b, j, i, l)*t2a(d, c, n, k) & !anbjildcnk      (-1.000)
                      + u34(d, n, c, l, i, k)*t2a(b, a, n, j) & !dnclikbanj      (+1.000)
                      - u34(d, n, b, l, i, k)*t2a(c, a, n, j) & !dnblikcanj      (-1.000)
                      + u34(d, n, a, l, i, k)*t2a(c, b, n, j) & !dnalikcbnj      (+1.000)
                      - u34(c, n, d, l, i, k)*t2a(b, a, n, j) & !cndlikbanj      (-1.000)
                      + u34(b, n, d, l, i, k)*t2a(c, a, n, j) & !bndlikcanj      (+1.000)
                      - u34(a, n, d, l, i, k)*t2a(c, b, n, j) & !andlikcbnj      (-1.000)
                      + u34(c, n, b, l, i, k)*t2a(d, a, n, j) & !cnblikdanj      (+1.000)
                      - u34(c, n, a, l, i, k)*t2a(d, b, n, j) & !cnalikdbnj      (-1.000)
                      - u34(b, n, c, l, i, k)*t2a(d, a, n, j) & !bnclikdanj      (-1.000)
                      + u34(a, n, c, l, i, k)*t2a(d, b, n, j) & !anclikdbnj      (+1.000)
                      + u34(b, n, a, l, i, k)*t2a(d, c, n, j) & !bnalikdcnj      (+1.000)
                      - u34(a, n, b, l, i, k)*t2a(d, c, n, j) & !anblikdcnj      (-1.000)
                      - u34(d, n, a, l, j, k)*t2a(c, b, n, i) & !dnaljkcbni      (-1.000)
                      + u34(d, n, b, l, j, k)*t2a(c, a, n, i) & !dnbljkcani      (+1.000)
                      - u34(d, n, c, l, j, k)*t2a(b, a, n, i) & !dncljkbani      (-1.000)
                      + u34(c, n, a, l, j, k)*t2a(d, b, n, i) & !cnaljkdbni      (+1.000)
                      - u34(c, n, b, l, j, k)*t2a(d, a, n, i) & !cnbljkdani      (-1.000)
                      - u34(b, n, a, l, j, k)*t2a(d, c, n, i) & !bnaljkdcni      (-1.000)
                      + u34(a, n, b, l, j, k)*t2a(d, c, n, i) & !anbljkdcni      (+1.000)
                      + u34(b, n, c, l, j, k)*t2a(d, a, n, i) & !bncljkdani      (+1.000)
                      - u34(a, n, c, l, j, k)*t2a(d, b, n, i) & !ancljkdbni      (-1.000)
                      + u34(c, n, d, l, j, k)*t2a(b, a, n, i) & !cndljkbani      (+1.000)
                      - u34(b, n, d, l, j, k)*t2a(c, a, n, i) & !bndljkcani      (-1.000)
                      + u34(a, n, d, l, j, k)*t2a(c, b, n, i) & !andljkcbni      (+1.000)
                      - u34(d, n, c, k, i, l)*t2a(b, a, n, j) & !dnckilbanj      (-1.000)
                      + u34(d, n, b, k, i, l)*t2a(c, a, n, j) & !dnbkilcanj      (+1.000)
                      - u34(d, n, a, k, i, l)*t2a(c, b, n, j) & !dnakilcbnj      (-1.000)
                      + u34(c, n, d, k, i, l)*t2a(b, a, n, j) & !cndkilbanj      (+1.000)
                      - u34(b, n, d, k, i, l)*t2a(c, a, n, j) & !bndkilcanj      (-1.000)
                      + u34(a, n, d, k, i, l)*t2a(c, b, n, j) & !andkilcbnj      (+1.000)
                      - u34(c, n, b, k, i, l)*t2a(d, a, n, j) & !cnbkildanj      (-1.000)
                      + u34(c, n, a, k, i, l)*t2a(d, b, n, j) & !cnakildbnj      (+1.000)
                      + u34(b, n, c, k, i, l)*t2a(d, a, n, j) & !bnckildanj      (+1.000)
                      - u34(a, n, c, k, i, l)*t2a(d, b, n, j) & !anckildbnj      (-1.000)
                      - u34(b, n, a, k, i, l)*t2a(d, c, n, j) & !bnakildcnj      (-1.000)
                      + u34(a, n, b, k, i, l)*t2a(d, c, n, j) & !anbkildcnj      (+1.000)
                      + u34(d, n, a, k, j, l)*t2a(c, b, n, i) & !dnakjlcbni      (+1.000)
                      - u34(d, n, b, k, j, l)*t2a(c, a, n, i) & !dnbkjlcani      (-1.000)
                      + u34(d, n, c, k, j, l)*t2a(b, a, n, i) & !dnckjlbani      (+1.000)
                      - u34(c, n, a, k, j, l)*t2a(d, b, n, i) & !cnakjldbni      (-1.000)
                      + u34(c, n, b, k, j, l)*t2a(d, a, n, i) & !cnbkjldani      (+1.000)
                      + u34(b, n, a, k, j, l)*t2a(d, c, n, i) & !bnakjldcni      (+1.000)
                      - u34(a, n, b, k, j, l)*t2a(d, c, n, i) & !anbkjldcni      (-1.000)
                      - u34(b, n, c, k, j, l)*t2a(d, a, n, i) & !bnckjldani      (-1.000)
                      + u34(a, n, c, k, j, l)*t2a(d, b, n, i) & !anckjldbni      (+1.000)
                      - u34(c, n, d, k, j, l)*t2a(b, a, n, i) & !cndkjlbani      (-1.000)
                      + u34(b, n, d, k, j, l)*t2a(c, a, n, i) & !bndkjlcani      (+1.000)
                      - u34(a, n, d, k, j, l)*t2a(c, b, n, i)          !andkjlcbni      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u34)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s17(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k1
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, d2, s17)
    deallocate (d1)
    deallocate (d2)

    allocate (x5(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    x5 = 0.0d0
    call sum_stripe(4, shape(x5), size(x5), '3421', 0.500, x5, &
                    s17)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s17), size(s17), '3412', s17, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u31(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u31)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u31) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + (u31(b, a, l, n, j, i)*t2a(d, c, n, k) & !balnjidcnk      (+0.500)
                         - u31(c, a, l, n, j, i)*t2a(d, b, n, k) & !calnjidbnk      (-0.500)
                         - u31(d, a, k, n, j, i)*t2a(c, b, n, l) & !daknjicbnl      (-0.500)
                         + u31(d, a, l, n, j, i)*t2a(c, b, n, k) & !dalnjicbnk      (+0.500)
                         + u31(c, a, k, n, j, i)*t2a(d, b, n, l) & !caknjidbnl      (+0.500)
                         - u31(b, a, k, n, j, i)*t2a(d, c, n, l) & !baknjidcnl      (-0.500)
                         - u31(b, a, l, n, k, i)*t2a(d, c, n, j) & !balnkidcnj      (-0.500)
                         + u31(c, a, l, n, k, i)*t2a(d, b, n, j) & !calnkidbnj      (+0.500)
                         + u31(d, a, j, n, k, i)*t2a(c, b, n, l) & !dajnkicbnl      (+0.500)
                         - u31(d, a, l, n, k, i)*t2a(c, b, n, j) & !dalnkicbnj      (-0.500)
                         - u31(c, a, j, n, k, i)*t2a(d, b, n, l) & !cajnkidbnl      (-0.500)
                         + u31(b, a, j, n, k, i)*t2a(d, c, n, l) & !bajnkidcnl      (+0.500)
                         + u31(b, a, l, n, k, j)*t2a(d, c, n, i) & !balnkjdcni      (+0.500)
                         - u31(c, a, l, n, k, j)*t2a(d, b, n, i) & !calnkjdbni      (-0.500)
                         - u31(d, a, i, n, k, j)*t2a(c, b, n, l) & !dainkjcbnl      (-0.500)
                         + u31(d, a, l, n, k, j)*t2a(c, b, n, i) & !dalnkjcbni      (+0.500)
                         + u31(c, a, i, n, k, j)*t2a(d, b, n, l) & !cainkjdbnl      (+0.500)
                         - u31(b, a, i, n, k, j)*t2a(d, c, n, l) & !bainkjdcnl      (-0.500)
                         + u31(b, a, k, n, l, i)*t2a(d, c, n, j) & !baknlidcnj      (+0.500)
                         - u31(c, a, k, n, l, i)*t2a(d, b, n, j) & !caknlidbnj      (-0.500)
                         - u31(d, a, j, n, l, i)*t2a(c, b, n, k) & !dajnlicbnk      (-0.500)
                         + u31(d, a, k, n, l, i)*t2a(c, b, n, j) & !daknlicbnj      (+0.500)
                         + u31(c, a, j, n, l, i)*t2a(d, b, n, k) & !cajnlidbnk      (+0.500)
                         - u31(b, a, j, n, l, i)*t2a(d, c, n, k) & !bajnlidcnk      (-0.500)
                         - u31(b, a, k, n, l, j)*t2a(d, c, n, i) & !baknljdcni      (-0.500)
                         + u31(c, a, k, n, l, j)*t2a(d, b, n, i) & !caknljdbni      (+0.500)
                         + u31(d, a, i, n, l, j)*t2a(c, b, n, k) & !dainljcbnk      (+0.500)
                         - u31(d, a, k, n, l, j)*t2a(c, b, n, i) & !daknljcbni      (-0.500)
                         - u31(c, a, i, n, l, j)*t2a(d, b, n, k) & !cainljdbnk      (-0.500)
                         + u31(b, a, i, n, l, j)*t2a(d, c, n, k) & !bainljdcnk      (+0.500)
                         + u31(b, a, j, n, l, k)*t2a(d, c, n, i) & !bajnlkdcni      (+0.500)
                         - u31(c, a, j, n, l, k)*t2a(d, b, n, i) & !cajnlkdbni      (-0.500)
                         - u31(d, a, i, n, l, k)*t2a(c, b, n, j) & !dainlkcbnj      (-0.500)
                         + u31(d, a, j, n, l, k)*t2a(c, b, n, i) & !dajnlkcbni      (+0.500)
                         + u31(c, a, i, n, l, k)*t2a(d, b, n, j) & !cainlkdbnj      (+0.500)
                         - u31(b, a, i, n, l, k)*t2a(d, c, n, j))/2.0d0   !bainlkdcnj      (-0.500)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u31)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s17), size(s17), '3412', s17, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s42(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s42)
    deallocate (d1)
    deallocate (b2)
    deallocate (s17)

    call sum_stripe(4, shape(x1), size(x1), '2134', -0.500, &
                    x1, s42)
    deallocate (s42)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
    allocate (h2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, &
                 n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(8, shape(t4a), size(t4a), '12534678', t4a, h2)
    allocate (u11(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k1*k3*k3
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, h2, u11)
    deallocate (d1)
    deallocate (h2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u11) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1                           !top 2 switched
                sum = sum &
                      + (u11(c, a, l, k, j, m)*t2a(d, b, m, i) & !calkjmdbmi      (+0.500)
                         - u11(b, a, l, k, j, m)*t2a(d, c, m, i) & !balkjmdcmi      (-0.500)
                         - u11(c, b, l, k, j, m)*t2a(d, a, m, i) & !cblkjmdami      (-0.500)
                         - u11(d, a, l, k, j, m)*t2a(c, b, m, i) & !dalkjmcbmi      (-0.500)
                         + u11(d, b, l, k, j, m)*t2a(c, a, m, i) & !dblkjmcami      (+0.500)
                         - u11(d, c, l, k, j, m)*t2a(b, a, m, i) & !dclkjmbami      (-0.500)
                         + u11(b, a, l, k, i, m)*t2a(d, c, m, j) & !balkimdcmj      (+0.500)
                         - u11(c, a, l, k, i, m)*t2a(d, b, m, j) & !calkimdbmj      (-0.500)
                         + u11(c, b, l, k, i, m)*t2a(d, a, m, j) & !cblkimdamj      (+0.500)
                         + u11(d, a, l, k, i, m)*t2a(c, b, m, j) & !dalkimcbmj      (+0.500)
                         - u11(d, b, l, k, i, m)*t2a(c, a, m, j) & !dblkimcamj      (-0.500)
                         + u11(d, c, l, k, i, m)*t2a(b, a, m, j) & !dclkimbamj      (+0.500)
                         - u11(b, a, l, j, i, m)*t2a(d, c, m, k) & !baljimdcmk      (-0.500)
                         + u11(c, a, l, j, i, m)*t2a(d, b, m, k) & !caljimdbmk      (+0.500)
                         - u11(c, b, l, j, i, m)*t2a(d, a, m, k) & !cbljimdamk      (-0.500)
                         - u11(d, a, l, j, i, m)*t2a(c, b, m, k) & !daljimcbmk      (-0.500)
                         + u11(d, b, l, j, i, m)*t2a(c, a, m, k) & !dbljimcamk      (+0.500)
                         - u11(d, c, l, j, i, m)*t2a(b, a, m, k) & !dcljimbamk      (-0.500)
                         + u11(b, a, k, j, i, m)*t2a(d, c, m, l) & !bakjimdcml      (+0.500)
                         - u11(c, a, k, j, i, m)*t2a(d, b, m, l) & !cakjimdbml      (-0.500)
                         + u11(c, b, k, j, i, m)*t2a(d, a, m, l) & !cbkjimdaml      (+0.500)
                         + u11(d, a, k, j, i, m)*t2a(c, b, m, l) & !dakjimcbml      (+0.500)
                         - u11(d, b, k, j, i, m)*t2a(c, a, m, l) & !dbkjimcaml      (-0.500)
                         + u11(d, c, k, j, i, m)*t2a(b, a, m, l))/2.0d0   !dckjimbaml      (+0.500)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u11)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s18(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s18)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '3421', 1.000, x6, &
                    s18)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s18), size(s18), '4312', s18, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s44(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s44)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x10), size(x10), '2134', -1.000, &
                    x10, s44)
    deallocate (s44)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s18), size(s18), '3412', s18, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s40(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s40)
    deallocate (d1)
    deallocate (b2)
    deallocate (s18)

    call sum_stripe(4, shape(x9), size(x9), '4123', -1.000, &
                    x9, s40)
    deallocate (s40)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (q7(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, d2, q7)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x3), size(x3), '21', -0.500, x3, &
                    q7)
    deallocate (q7)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '1243', intr, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '4312', t2a, d2)
    allocate (s19(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k3*k3
    i3 = k1*k1
    call egemm(i1, i2, i3, d1, d2, s19)
    deallocate (d1)
    deallocate (d2)

    allocate (x7(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    x7 = 0.0d0
    call sum_stripe(4, shape(x7), size(x7), '3412', 0.500, x7, &
                    s19)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s19), size(s19), '4312', s19, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s41(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s41)
    deallocate (d1)
    deallocate (b2)
    deallocate (s19)

    call sum_stripe(4, shape(x2), size(x2), '4123', 0.500, x2, &
                    s41)
    deallocate (s41)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3124', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '1432', t2a, d2)
    allocate (q8(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k1*k1*k3
    call egemm(i1, i2, i3, d1, d2, q8)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x4), size(x4), '21', 0.500, x4, &
                    q8)
    deallocate (q8)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u12(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u12)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,u12,t4b) &
        !$omp private(a,b,c,d,n,f,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + u12(d, j, i, f, m, n)*t4b(f, c, b, a, n, m, l, k) & !djifmnfcbanmlk  (+1.000)
                      - u12(c, j, i, f, m, n)*t4b(f, d, b, a, n, m, l, k) & !cjifmnfdbanmlk  (-1.000)
                      + u12(b, j, i, f, m, n)*t4b(f, d, c, a, n, m, l, k) & !bjifmnfdcanmlk  (+1.000)
                      - u12(a, j, i, f, m, n)*t4b(f, d, c, b, n, m, l, k) & !ajifmnfdcbnmlk  (-1.000)
                      - u12(d, k, i, f, m, n)*t4b(f, c, b, a, n, m, l, j) & !dkifmnfcbanmlj  (-1.000)
                      + u12(c, k, i, f, m, n)*t4b(f, d, b, a, n, m, l, j) & !ckifmnfdbanmlj  (+1.000)
                      - u12(b, k, i, f, m, n)*t4b(f, d, c, a, n, m, l, j) & !bkifmnfdcanmlj  (-1.000)
                      + u12(a, k, i, f, m, n)*t4b(f, d, c, b, n, m, l, j) & !akifmnfdcbnmlj  (+1.000)
                      + u12(d, l, i, f, m, n)*t4b(f, c, b, a, n, m, k, j) & !dlifmnfcbanmkj  (+1.000)
                      - u12(c, l, i, f, m, n)*t4b(f, d, b, a, n, m, k, j) & !clifmnfdbanmkj  (-1.000)
                      + u12(b, l, i, f, m, n)*t4b(f, d, c, a, n, m, k, j) & !blifmnfdcanmkj  (+1.000)
                      - u12(a, l, i, f, m, n)*t4b(f, d, c, b, n, m, k, j) & !alifmnfdcbnmkj  (-1.000)
                      + u12(d, k, j, f, m, n)*t4b(f, c, b, a, n, m, l, i) & !dkjfmnfcbanmli  (+1.000)
                      - u12(c, k, j, f, m, n)*t4b(f, d, b, a, n, m, l, i) & !ckjfmnfdbanmli  (-1.000)
                      + u12(b, k, j, f, m, n)*t4b(f, d, c, a, n, m, l, i) & !bkjfmnfdcanmli  (+1.000)
                      - u12(a, k, j, f, m, n)*t4b(f, d, c, b, n, m, l, i) & !akjfmnfdcbnmli  (-1.000)
                      - u12(d, l, j, f, m, n)*t4b(f, c, b, a, n, m, k, i) & !dljfmnfcbanmki  (-1.000)
                      + u12(c, l, j, f, m, n)*t4b(f, d, b, a, n, m, k, i) & !cljfmnfdbanmki  (+1.000)
                      - u12(b, l, j, f, m, n)*t4b(f, d, c, a, n, m, k, i) & !bljfmnfdcanmki  (-1.000)
                      + u12(a, l, j, f, m, n)*t4b(f, d, c, b, n, m, k, i) & !aljfmnfdcbnmki  (+1.000)
                      + u12(d, l, k, f, m, n)*t4b(f, c, b, a, n, m, j, i) & !dlkfmnfcbanmji  (+1.000)
                      - u12(c, l, k, f, m, n)*t4b(f, d, b, a, n, m, j, i) & !clkfmnfdbanmji  (-1.000)
                      + u12(b, l, k, f, m, n)*t4b(f, d, c, a, n, m, j, i) & !blkfmnfdcanmji  (+1.000)
                      - u12(a, l, k, f, m, n)*t4b(f, d, c, b, n, m, j, i)  !alkfmnfdcbnmji  (-1.000)
            end do; end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u12), size(u12), '465123', u12, f1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (u32(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k1
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, f1, d2, u32)
    deallocate (f1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u32) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - u32(a, l, m, d, j, i)*t2a(c, b, m, k) & !almdjicbmk      (-1.000)
                      + u32(b, l, m, d, j, i)*t2a(c, a, m, k) & !blmdjicamk      (+1.000)
                      - u32(c, l, m, d, j, i)*t2a(b, a, m, k) & !clmdjibamk      (-1.000)
                      + u32(a, l, m, c, j, i)*t2a(d, b, m, k) & !almcjidbmk      (+1.000)
                      - u32(b, l, m, c, j, i)*t2a(d, a, m, k) & !blmcjidamk      (-1.000)
                      - u32(a, l, m, b, j, i)*t2a(d, c, m, k) & !almbjidcmk      (-1.000)
                      + u32(b, l, m, a, j, i)*t2a(d, c, m, k) & !blmajidcmk      (+1.000)
                      + u32(c, l, m, b, j, i)*t2a(d, a, m, k) & !clmbjidamk      (+1.000)
                      - u32(c, l, m, a, j, i)*t2a(d, b, m, k) & !clmajidbmk      (-1.000)
                      + u32(d, l, m, c, j, i)*t2a(b, a, m, k) & !dlmcjibamk      (+1.000)
                      - u32(d, l, m, b, j, i)*t2a(c, a, m, k) & !dlmbjicamk      (-1.000)
                      + u32(d, l, m, a, j, i)*t2a(c, b, m, k) & !dlmajicbmk      (+1.000)
                      + u32(a, k, m, d, j, i)*t2a(c, b, m, l) & !akmdjicbml      (+1.000)
                      - u32(b, k, m, d, j, i)*t2a(c, a, m, l) & !bkmdjicaml      (-1.000)
                      + u32(c, k, m, d, j, i)*t2a(b, a, m, l) & !ckmdjibaml      (+1.000)
                      - u32(a, k, m, c, j, i)*t2a(d, b, m, l) & !akmcjidbml      (-1.000)
                      + u32(b, k, m, c, j, i)*t2a(d, a, m, l) & !bkmcjidaml      (+1.000)
                      + u32(a, k, m, b, j, i)*t2a(d, c, m, l) & !akmbjidcml      (+1.000)
                      - u32(b, k, m, a, j, i)*t2a(d, c, m, l) & !bkmajidcml      (-1.000)
                      - u32(c, k, m, b, j, i)*t2a(d, a, m, l) & !ckmbjidaml      (-1.000)
                      + u32(c, k, m, a, j, i)*t2a(d, b, m, l) & !ckmajidbml      (+1.000)
                      - u32(d, k, m, c, j, i)*t2a(b, a, m, l) & !dkmcjibaml      (-1.000)
                      + u32(d, k, m, b, j, i)*t2a(c, a, m, l) & !dkmbjicaml      (+1.000)
                      - u32(d, k, m, a, j, i)*t2a(c, b, m, l) & !dkmajicbml      (-1.000)
                      + u32(a, l, m, d, k, i)*t2a(c, b, m, j) & !almdkicbmj      (+1.000)
                      - u32(b, l, m, d, k, i)*t2a(c, a, m, j) & !blmdkicamj      (-1.000)
                      + u32(c, l, m, d, k, i)*t2a(b, a, m, j) & !clmdkibamj      (+1.000)
                      - u32(a, l, m, c, k, i)*t2a(d, b, m, j) & !almckidbmj      (-1.000)
                      + u32(b, l, m, c, k, i)*t2a(d, a, m, j) & !blmckidamj      (+1.000)
                      + u32(a, l, m, b, k, i)*t2a(d, c, m, j) & !almbkidcmj      (+1.000)
                      - u32(b, l, m, a, k, i)*t2a(d, c, m, j) & !blmakidcmj      (-1.000)
                      - u32(c, l, m, b, k, i)*t2a(d, a, m, j) & !clmbkidamj      (-1.000)
                      + u32(c, l, m, a, k, i)*t2a(d, b, m, j) & !clmakidbmj      (+1.000)
                      - u32(d, l, m, c, k, i)*t2a(b, a, m, j) & !dlmckibamj      (-1.000)
                      + u32(d, l, m, b, k, i)*t2a(c, a, m, j) & !dlmbkicamj      (+1.000)
                      - u32(d, l, m, a, k, i)*t2a(c, b, m, j) & !dlmakicbmj      (-1.000)
                      - u32(a, l, m, b, k, j)*t2a(d, c, m, i) & !almbkjdcmi      (-1.000)
                      + u32(b, l, m, a, k, j)*t2a(d, c, m, i) & !blmakjdcmi      (+1.000)
                      + u32(a, l, m, c, k, j)*t2a(d, b, m, i) & !almckjdbmi      (+1.000)
                      - u32(b, l, m, c, k, j)*t2a(d, a, m, i) & !blmckjdami      (-1.000)
                      - u32(c, l, m, a, k, j)*t2a(d, b, m, i) & !clmakjdbmi      (-1.000)
                      + u32(c, l, m, b, k, j)*t2a(d, a, m, i) & !clmbkjdami      (+1.000)
                      - u32(a, l, m, d, k, j)*t2a(c, b, m, i) & !almdkjcbmi      (-1.000)
                      + u32(b, l, m, d, k, j)*t2a(c, a, m, i) & !blmdkjcami      (+1.000)
                      - u32(c, l, m, d, k, j)*t2a(b, a, m, i) & !clmdkjbami      (-1.000)
                      + u32(d, l, m, a, k, j)*t2a(c, b, m, i) & !dlmakjcbmi      (+1.000)
                      - u32(d, l, m, b, k, j)*t2a(c, a, m, i) & !dlmbkjcami      (-1.000)
                      + u32(d, l, m, c, k, j)*t2a(b, a, m, i) & !dlmckjbami      (+1.000)
                      - u32(a, k, m, d, l, i)*t2a(c, b, m, j) & !akmdlicbmj      (-1.000)
                      + u32(b, k, m, d, l, i)*t2a(c, a, m, j) & !bkmdlicamj      (+1.000)
                      - u32(c, k, m, d, l, i)*t2a(b, a, m, j) & !ckmdlibamj      (-1.000)
                      + u32(a, k, m, c, l, i)*t2a(d, b, m, j) & !akmclidbmj      (+1.000)
                      - u32(b, k, m, c, l, i)*t2a(d, a, m, j) & !bkmclidamj      (-1.000)
                      - u32(a, k, m, b, l, i)*t2a(d, c, m, j) & !akmblidcmj      (-1.000)
                      + u32(b, k, m, a, l, i)*t2a(d, c, m, j) & !bkmalidcmj      (+1.000)
                      + u32(c, k, m, b, l, i)*t2a(d, a, m, j) & !ckmblidamj      (+1.000)
                      - u32(c, k, m, a, l, i)*t2a(d, b, m, j) & !ckmalidbmj      (-1.000)
                      + u32(d, k, m, c, l, i)*t2a(b, a, m, j) & !dkmclibamj      (+1.000)
                      - u32(d, k, m, b, l, i)*t2a(c, a, m, j) & !dkmblicamj      (-1.000)
                      + u32(d, k, m, a, l, i)*t2a(c, b, m, j) & !dkmalicbmj      (+1.000)
                      + u32(a, k, m, b, l, j)*t2a(d, c, m, i) & !akmbljdcmi      (+1.000)
                      - u32(b, k, m, a, l, j)*t2a(d, c, m, i) & !bkmaljdcmi      (-1.000)
                      - u32(a, k, m, c, l, j)*t2a(d, b, m, i) & !akmcljdbmi      (-1.000)
                      + u32(b, k, m, c, l, j)*t2a(d, a, m, i) & !bkmcljdami      (+1.000)
                      + u32(c, k, m, a, l, j)*t2a(d, b, m, i) & !ckmaljdbmi      (+1.000)
                      - u32(c, k, m, b, l, j)*t2a(d, a, m, i) & !ckmbljdami      (-1.000)
                      + u32(a, k, m, d, l, j)*t2a(c, b, m, i) & !akmdljcbmi      (+1.000)
                      - u32(b, k, m, d, l, j)*t2a(c, a, m, i) & !bkmdljcami      (-1.000)
                      + u32(c, k, m, d, l, j)*t2a(b, a, m, i) & !ckmdljbami      (+1.000)
                      - u32(d, k, m, a, l, j)*t2a(c, b, m, i) & !dkmaljcbmi      (-1.000)
                      + u32(d, k, m, b, l, j)*t2a(c, a, m, i) & !dkmbljcami      (+1.000)
                      - u32(d, k, m, c, l, j)*t2a(b, a, m, i) & !dkmcljbami      (-1.000)
                      - u32(a, j, m, d, k, i)*t2a(c, b, m, l) & !ajmdkicbml      (-1.000)
                      + u32(b, j, m, d, k, i)*t2a(c, a, m, l) & !bjmdkicaml      (+1.000)
                      - u32(c, j, m, d, k, i)*t2a(b, a, m, l) & !cjmdkibaml      (-1.000)
                      + u32(a, j, m, c, k, i)*t2a(d, b, m, l) & !ajmckidbml      (+1.000)
                      - u32(b, j, m, c, k, i)*t2a(d, a, m, l) & !bjmckidaml      (-1.000)
                      - u32(a, j, m, b, k, i)*t2a(d, c, m, l) & !ajmbkidcml      (-1.000)
                      + u32(b, j, m, a, k, i)*t2a(d, c, m, l) & !bjmakidcml      (+1.000)
                      + u32(c, j, m, b, k, i)*t2a(d, a, m, l) & !cjmbkidaml      (+1.000)
                      - u32(c, j, m, a, k, i)*t2a(d, b, m, l) & !cjmakidbml      (-1.000)
                      + u32(d, j, m, c, k, i)*t2a(b, a, m, l) & !djmckibaml      (+1.000)
                      - u32(d, j, m, b, k, i)*t2a(c, a, m, l) & !djmbkicaml      (-1.000)
                      + u32(d, j, m, a, k, i)*t2a(c, b, m, l) & !djmakicbml      (+1.000)
                      + u32(a, j, m, d, l, i)*t2a(c, b, m, k) & !ajmdlicbmk      (+1.000)
                      - u32(b, j, m, d, l, i)*t2a(c, a, m, k) & !bjmdlicamk      (-1.000)
                      + u32(c, j, m, d, l, i)*t2a(b, a, m, k) & !cjmdlibamk      (+1.000)
                      - u32(a, j, m, c, l, i)*t2a(d, b, m, k) & !ajmclidbmk      (-1.000)
                      + u32(b, j, m, c, l, i)*t2a(d, a, m, k) & !bjmclidamk      (+1.000)
                      + u32(a, j, m, b, l, i)*t2a(d, c, m, k) & !ajmblidcmk      (+1.000)
                      - u32(b, j, m, a, l, i)*t2a(d, c, m, k) & !bjmalidcmk      (-1.000)
                      - u32(c, j, m, b, l, i)*t2a(d, a, m, k) & !cjmblidamk      (-1.000)
                      + u32(c, j, m, a, l, i)*t2a(d, b, m, k) & !cjmalidbmk      (+1.000)
                      - u32(d, j, m, c, l, i)*t2a(b, a, m, k) & !djmclibamk      (-1.000)
                      + u32(d, j, m, b, l, i)*t2a(c, a, m, k) & !djmblicamk      (+1.000)
                      - u32(d, j, m, a, l, i)*t2a(c, b, m, k) & !djmalicbmk      (-1.000)
                      - u32(a, j, m, b, l, k)*t2a(d, c, m, i) & !ajmblkdcmi      (-1.000)
                      + u32(b, j, m, a, l, k)*t2a(d, c, m, i) & !bjmalkdcmi      (+1.000)
                      + u32(a, j, m, c, l, k)*t2a(d, b, m, i) & !ajmclkdbmi      (+1.000)
                      - u32(b, j, m, c, l, k)*t2a(d, a, m, i) & !bjmclkdami      (-1.000)
                      - u32(c, j, m, a, l, k)*t2a(d, b, m, i) & !cjmalkdbmi      (-1.000)
                      + u32(c, j, m, b, l, k)*t2a(d, a, m, i) & !cjmblkdami      (+1.000)
                      - u32(a, j, m, d, l, k)*t2a(c, b, m, i) & !ajmdlkcbmi      (-1.000)
                      + u32(b, j, m, d, l, k)*t2a(c, a, m, i) & !bjmdlkcami      (+1.000)
                      - u32(c, j, m, d, l, k)*t2a(b, a, m, i) & !cjmdlkbami      (-1.000)
                      + u32(d, j, m, a, l, k)*t2a(c, b, m, i) & !djmalkcbmi      (+1.000)
                      - u32(d, j, m, b, l, k)*t2a(c, a, m, i) & !djmblkcami      (-1.000)
                      + u32(d, j, m, c, l, k)*t2a(b, a, m, i) & !djmclkbami      (+1.000)
                      + u32(a, i, m, d, k, j)*t2a(c, b, m, l) & !aimdkjcbml      (+1.000)
                      - u32(b, i, m, d, k, j)*t2a(c, a, m, l) & !bimdkjcaml      (-1.000)
                      + u32(c, i, m, d, k, j)*t2a(b, a, m, l) & !cimdkjbaml      (+1.000)
                      - u32(a, i, m, c, k, j)*t2a(d, b, m, l) & !aimckjdbml      (-1.000)
                      + u32(b, i, m, c, k, j)*t2a(d, a, m, l) & !bimckjdaml      (+1.000)
                      + u32(a, i, m, b, k, j)*t2a(d, c, m, l) & !aimbkjdcml      (+1.000)
                      - u32(b, i, m, a, k, j)*t2a(d, c, m, l) & !bimakjdcml      (-1.000)
                      - u32(c, i, m, b, k, j)*t2a(d, a, m, l) & !cimbkjdaml      (-1.000)
                      + u32(c, i, m, a, k, j)*t2a(d, b, m, l) & !cimakjdbml      (+1.000)
                      - u32(d, i, m, c, k, j)*t2a(b, a, m, l) & !dimckjbaml      (-1.000)
                      + u32(d, i, m, b, k, j)*t2a(c, a, m, l) & !dimbkjcaml      (+1.000)
                      - u32(d, i, m, a, k, j)*t2a(c, b, m, l) & !dimakjcbml      (-1.000)
                      - u32(a, i, m, d, l, j)*t2a(c, b, m, k) & !aimdljcbmk      (-1.000)
                      + u32(b, i, m, d, l, j)*t2a(c, a, m, k) & !bimdljcamk      (+1.000)
                      - u32(c, i, m, d, l, j)*t2a(b, a, m, k) & !cimdljbamk      (-1.000)
                      + u32(a, i, m, c, l, j)*t2a(d, b, m, k) & !aimcljdbmk      (+1.000)
                      - u32(b, i, m, c, l, j)*t2a(d, a, m, k) & !bimcljdamk      (-1.000)
                      - u32(a, i, m, b, l, j)*t2a(d, c, m, k) & !aimbljdcmk      (-1.000)
                      + u32(b, i, m, a, l, j)*t2a(d, c, m, k) & !bimaljdcmk      (+1.000)
                      + u32(c, i, m, b, l, j)*t2a(d, a, m, k) & !cimbljdamk      (+1.000)
                      - u32(c, i, m, a, l, j)*t2a(d, b, m, k) & !cimaljdbmk      (-1.000)
                      + u32(d, i, m, c, l, j)*t2a(b, a, m, k) & !dimcljbamk      (+1.000)
                      - u32(d, i, m, b, l, j)*t2a(c, a, m, k) & !dimbljcamk      (-1.000)
                      + u32(d, i, m, a, l, j)*t2a(c, b, m, k) & !dimaljcbmk      (+1.000)
                      + u32(a, i, m, b, l, k)*t2a(d, c, m, j) & !aimblkdcmj      (+1.000)
                      - u32(b, i, m, a, l, k)*t2a(d, c, m, j) & !bimalkdcmj      (-1.000)
                      - u32(a, i, m, c, l, k)*t2a(d, b, m, j) & !aimclkdbmj      (-1.000)
                      + u32(b, i, m, c, l, k)*t2a(d, a, m, j) & !bimclkdamj      (+1.000)
                      + u32(c, i, m, a, l, k)*t2a(d, b, m, j) & !cimalkdbmj      (+1.000)
                      - u32(c, i, m, b, l, k)*t2a(d, a, m, j) & !cimblkdamj      (-1.000)
                      + u32(a, i, m, d, l, k)*t2a(c, b, m, j) & !aimdlkcbmj      (+1.000)
                      - u32(b, i, m, d, l, k)*t2a(c, a, m, j) & !bimdlkcamj      (-1.000)
                      + u32(c, i, m, d, l, k)*t2a(b, a, m, j) & !cimdlkbamj      (+1.000)
                      - u32(d, i, m, a, l, k)*t2a(c, b, m, j) & !dimalkcbmj      (-1.000)
                      + u32(d, i, m, b, l, k)*t2a(c, a, m, j) & !dimblkcamj      (+1.000)
                      - u32(d, i, m, c, l, k)*t2a(b, a, m, j)          !dimclkbamj      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u32)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u12), size(u12), '465123', u12, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s48(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k1
    i3 = k2*k4
    call egemm1(i1, i3, f1, b2, s48)
    deallocate (f1)
    deallocate (b2)

    x1 = x1 + s48
    deallocate (s48)

    allocate (f1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u12), size(u12), '541236', u12, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u27(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1*k3*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u27)
    deallocate (f1)
    deallocate (b2)
    deallocate (u12)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,u27,t3b) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do n = n0 + 1, n2
                sum = sum &
                      - u27(d, f, c, j, i, n)*t3b(f, b, a, n, l, k) & !dfcjinfbanlk    (-1.000)
                      + u27(d, f, b, j, i, n)*t3b(f, c, a, n, l, k) & !dfbjinfcanlk    (+1.000)
                      - u27(d, f, a, j, i, n)*t3b(f, c, b, n, l, k) & !dfajinfcbnlk    (-1.000)
                      + u27(c, f, d, j, i, n)*t3b(f, b, a, n, l, k) & !cfdjinfbanlk    (+1.000)
                      - u27(b, f, d, j, i, n)*t3b(f, c, a, n, l, k) & !bfdjinfcanlk    (-1.000)
                      + u27(a, f, d, j, i, n)*t3b(f, c, b, n, l, k) & !afdjinfcbnlk    (+1.000)
                      - u27(c, f, b, j, i, n)*t3b(f, d, a, n, l, k) & !cfbjinfdanlk    (-1.000)
                      + u27(c, f, a, j, i, n)*t3b(f, d, b, n, l, k) & !cfajinfdbnlk    (+1.000)
                      + u27(b, f, c, j, i, n)*t3b(f, d, a, n, l, k) & !bfcjinfdanlk    (+1.000)
                      - u27(a, f, c, j, i, n)*t3b(f, d, b, n, l, k) & !afcjinfdbnlk    (-1.000)
                      - u27(b, f, a, j, i, n)*t3b(f, d, c, n, l, k) & !bfajinfdcnlk    (-1.000)
                      + u27(a, f, b, j, i, n)*t3b(f, d, c, n, l, k) & !afbjinfdcnlk    (+1.000)
                      + u27(d, f, c, k, i, n)*t3b(f, b, a, n, l, j) & !dfckinfbanlj    (+1.000)
                      - u27(d, f, b, k, i, n)*t3b(f, c, a, n, l, j) & !dfbkinfcanlj    (-1.000)
                      + u27(d, f, a, k, i, n)*t3b(f, c, b, n, l, j) & !dfakinfcbnlj    (+1.000)
                      - u27(c, f, d, k, i, n)*t3b(f, b, a, n, l, j) & !cfdkinfbanlj    (-1.000)
                      + u27(b, f, d, k, i, n)*t3b(f, c, a, n, l, j) & !bfdkinfcanlj    (+1.000)
                      - u27(a, f, d, k, i, n)*t3b(f, c, b, n, l, j) & !afdkinfcbnlj    (-1.000)
                      + u27(c, f, b, k, i, n)*t3b(f, d, a, n, l, j) & !cfbkinfdanlj    (+1.000)
                      - u27(c, f, a, k, i, n)*t3b(f, d, b, n, l, j) & !cfakinfdbnlj    (-1.000)
                      - u27(b, f, c, k, i, n)*t3b(f, d, a, n, l, j) & !bfckinfdanlj    (-1.000)
                      + u27(a, f, c, k, i, n)*t3b(f, d, b, n, l, j) & !afckinfdbnlj    (+1.000)
                      + u27(b, f, a, k, i, n)*t3b(f, d, c, n, l, j) & !bfakinfdcnlj    (+1.000)
                      - u27(a, f, b, k, i, n)*t3b(f, d, c, n, l, j) & !afbkinfdcnlj    (-1.000)
                      - u27(d, f, c, l, i, n)*t3b(f, b, a, n, k, j) & !dfclinfbankj    (-1.000)
                      + u27(d, f, b, l, i, n)*t3b(f, c, a, n, k, j) & !dfblinfcankj    (+1.000)
                      - u27(d, f, a, l, i, n)*t3b(f, c, b, n, k, j) & !dfalinfcbnkj    (-1.000)
                      + u27(c, f, d, l, i, n)*t3b(f, b, a, n, k, j) & !cfdlinfbankj    (+1.000)
                      - u27(b, f, d, l, i, n)*t3b(f, c, a, n, k, j) & !bfdlinfcankj    (-1.000)
                      + u27(a, f, d, l, i, n)*t3b(f, c, b, n, k, j) & !afdlinfcbnkj    (+1.000)
                      - u27(c, f, b, l, i, n)*t3b(f, d, a, n, k, j) & !cfblinfdankj    (-1.000)
                      + u27(c, f, a, l, i, n)*t3b(f, d, b, n, k, j) & !cfalinfdbnkj    (+1.000)
                      + u27(b, f, c, l, i, n)*t3b(f, d, a, n, k, j) & !bfclinfdankj    (+1.000)
                      - u27(a, f, c, l, i, n)*t3b(f, d, b, n, k, j) & !afclinfdbnkj    (-1.000)
                      - u27(b, f, a, l, i, n)*t3b(f, d, c, n, k, j) & !bfalinfdcnkj    (-1.000)
                      + u27(a, f, b, l, i, n)*t3b(f, d, c, n, k, j) & !afblinfdcnkj    (+1.000)
                      - u27(d, f, c, k, j, n)*t3b(f, b, a, n, l, i) & !dfckjnfbanli    (-1.000)
                      + u27(d, f, b, k, j, n)*t3b(f, c, a, n, l, i) & !dfbkjnfcanli    (+1.000)
                      - u27(d, f, a, k, j, n)*t3b(f, c, b, n, l, i) & !dfakjnfcbnli    (-1.000)
                      + u27(c, f, d, k, j, n)*t3b(f, b, a, n, l, i) & !cfdkjnfbanli    (+1.000)
                      - u27(b, f, d, k, j, n)*t3b(f, c, a, n, l, i) & !bfdkjnfcanli    (-1.000)
                      + u27(a, f, d, k, j, n)*t3b(f, c, b, n, l, i) & !afdkjnfcbnli    (+1.000)
                      - u27(c, f, b, k, j, n)*t3b(f, d, a, n, l, i) & !cfbkjnfdanli    (-1.000)
                      + u27(c, f, a, k, j, n)*t3b(f, d, b, n, l, i) & !cfakjnfdbnli    (+1.000)
                      + u27(b, f, c, k, j, n)*t3b(f, d, a, n, l, i) & !bfckjnfdanli    (+1.000)
                      - u27(a, f, c, k, j, n)*t3b(f, d, b, n, l, i) & !afckjnfdbnli    (-1.000)
                      - u27(b, f, a, k, j, n)*t3b(f, d, c, n, l, i) & !bfakjnfdcnli    (-1.000)
                      + u27(a, f, b, k, j, n)*t3b(f, d, c, n, l, i) & !afbkjnfdcnli    (+1.000)
                      + u27(d, f, c, l, j, n)*t3b(f, b, a, n, k, i) & !dfcljnfbanki    (+1.000)
                      - u27(d, f, b, l, j, n)*t3b(f, c, a, n, k, i) & !dfbljnfcanki    (-1.000)
                      + u27(d, f, a, l, j, n)*t3b(f, c, b, n, k, i) & !dfaljnfcbnki    (+1.000)
                      - u27(c, f, d, l, j, n)*t3b(f, b, a, n, k, i) & !cfdljnfbanki    (-1.000)
                      + u27(b, f, d, l, j, n)*t3b(f, c, a, n, k, i) & !bfdljnfcanki    (+1.000)
                      - u27(a, f, d, l, j, n)*t3b(f, c, b, n, k, i) & !afdljnfcbnki    (-1.000)
                      + u27(c, f, b, l, j, n)*t3b(f, d, a, n, k, i) & !cfbljnfdanki    (+1.000)
                      - u27(c, f, a, l, j, n)*t3b(f, d, b, n, k, i) & !cfaljnfdbnki    (-1.000)
                      - u27(b, f, c, l, j, n)*t3b(f, d, a, n, k, i) & !bfcljnfdanki    (-1.000)
                      + u27(a, f, c, l, j, n)*t3b(f, d, b, n, k, i) & !afcljnfdbnki    (+1.000)
                      + u27(b, f, a, l, j, n)*t3b(f, d, c, n, k, i) & !bfaljnfdcnki    (+1.000)
                      - u27(a, f, b, l, j, n)*t3b(f, d, c, n, k, i) & !afbljnfdcnki    (-1.000)
                      - u27(d, f, c, l, k, n)*t3b(f, b, a, n, j, i) & !dfclknfbanji    (-1.000)
                      + u27(d, f, b, l, k, n)*t3b(f, c, a, n, j, i) & !dfblknfcanji    (+1.000)
                      - u27(d, f, a, l, k, n)*t3b(f, c, b, n, j, i) & !dfalknfcbnji    (-1.000)
                      + u27(c, f, d, l, k, n)*t3b(f, b, a, n, j, i) & !cfdlknfbanji    (+1.000)
                      - u27(b, f, d, l, k, n)*t3b(f, c, a, n, j, i) & !bfdlknfcanji    (-1.000)
                      + u27(a, f, d, l, k, n)*t3b(f, c, b, n, j, i) & !afdlknfcbnji    (+1.000)
                      - u27(c, f, b, l, k, n)*t3b(f, d, a, n, j, i) & !cfblknfdanji    (-1.000)
                      + u27(c, f, a, l, k, n)*t3b(f, d, b, n, j, i) & !cfalknfdbnji    (+1.000)
                      + u27(b, f, c, l, k, n)*t3b(f, d, a, n, j, i) & !bfclknfdanji    (+1.000)
                      - u27(a, f, c, l, k, n)*t3b(f, d, b, n, j, i) & !afclknfdbnji    (-1.000)
                      - u27(b, f, a, l, k, n)*t3b(f, d, c, n, j, i) & !bfalknfdcnji    (-1.000)
                      + u27(a, f, b, l, k, n)*t3b(f, d, c, n, j, i)      !afblknfdcnji    (+1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u27)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, &
                 n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(8, shape(t4b), size(t4b), '12534678', t4b, h2)
    allocate (u13(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k1*k3*k3
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, h2, u13)
    deallocate (d1)
    deallocate (h2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u13) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - u13(b, a, l, k, j, m)*t2a(d, c, m, i) & !balkjmdcmi      (-1.000)
                      + u13(c, a, l, k, j, m)*t2a(d, b, m, i) & !calkjmdbmi      (+1.000)
                      - u13(c, b, l, k, j, m)*t2a(d, a, m, i) & !cblkjmdami      (-1.000)
                      - u13(d, a, l, k, j, m)*t2a(c, b, m, i) & !dalkjmcbmi      (-1.000)
                      + u13(d, b, l, k, j, m)*t2a(c, a, m, i) & !dblkjmcami      (+1.000)
                      - u13(d, c, l, k, j, m)*t2a(b, a, m, i) & !dclkjmbami      (-1.000)
                      + u13(b, a, l, k, i, m)*t2a(d, c, m, j) & !balkimdcmj      (+1.000)
                      - u13(c, a, l, k, i, m)*t2a(d, b, m, j) & !calkimdbmj      (-1.000)
                      + u13(c, b, l, k, i, m)*t2a(d, a, m, j) & !cblkimdamj      (+1.000)
                      + u13(d, a, l, k, i, m)*t2a(c, b, m, j) & !dalkimcbmj      (+1.000)
                      - u13(d, b, l, k, i, m)*t2a(c, a, m, j) & !dblkimcamj      (-1.000)
                      + u13(d, c, l, k, i, m)*t2a(b, a, m, j) & !dclkimbamj      (+1.000)
                      - u13(b, a, l, j, i, m)*t2a(d, c, m, k) & !baljimdcmk      (-1.000)
                      + u13(c, a, l, j, i, m)*t2a(d, b, m, k) & !caljimdbmk      (+1.000)
                      - u13(c, b, l, j, i, m)*t2a(d, a, m, k) & !cbljimdamk      (-1.000)
                      - u13(d, a, l, j, i, m)*t2a(c, b, m, k) & !daljimcbmk      (-1.000)
                      + u13(d, b, l, j, i, m)*t2a(c, a, m, k) & !dbljimcamk      (+1.000)
                      - u13(d, c, l, j, i, m)*t2a(b, a, m, k) & !dcljimbamk      (-1.000)
                      + u13(b, a, k, j, i, m)*t2a(d, c, m, l) & !bakjimdcml      (+1.000)
                      - u13(c, a, k, j, i, m)*t2a(d, b, m, l) & !cakjimdbml      (-1.000)
                      + u13(c, b, k, j, i, m)*t2a(d, a, m, l) & !cbkjimdaml      (+1.000)
                      + u13(d, a, k, j, i, m)*t2a(c, b, m, l) & !dakjimcbml      (+1.000)
                      - u13(d, b, k, j, i, m)*t2a(c, a, m, l) & !dbkjimcaml      (-1.000)
                      + u13(d, c, k, j, i, m)*t2a(b, a, m, l)          !dckjimbaml      (+1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u13)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s20(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s20)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x8), size(x8), '3421', 1.000, x8, &
                    s20)
    deallocate (s20)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s21(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s21)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x9), size(x9), '2314', -1.000, &
                    x9, s21)
    deallocate (s21)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s22(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s22)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x10), size(x10), '3412', 1.000, &
                    x10, s22)
    deallocate (s22)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s23(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s23)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '3421', 1.000, x6, &
                    s23)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s23), size(s23), '4312', s23, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s47(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s47)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x10), size(x10), '2134', -1.000, &
                    x10, s47)
    deallocate (s47)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,x10) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x10(e, d, c, i)*t3a(e, b, a, l, k, j) & !edciebalkj      (+1.000)
                      - x10(e, d, b, i)*t3a(e, c, a, l, k, j) & !edbiecalkj      (-1.000)
                      + x10(e, d, a, i)*t3a(e, c, b, l, k, j) & !edaiecblkj      (+1.000)
                      - x10(e, c, d, i)*t3a(e, b, a, l, k, j) & !ecdiebalkj      (-1.000)
                      + x10(e, b, d, i)*t3a(e, c, a, l, k, j) & !ebdiecalkj      (+1.000)
                      - x10(e, a, d, i)*t3a(e, c, b, l, k, j) & !eadiecblkj      (-1.000)
                      + x10(e, c, b, i)*t3a(e, d, a, l, k, j) & !ecbiedalkj      (+1.000)
                      - x10(e, c, a, i)*t3a(e, d, b, l, k, j) & !ecaiedblkj      (-1.000)
                      - x10(e, b, c, i)*t3a(e, d, a, l, k, j) & !ebciedalkj      (-1.000)
                      + x10(e, a, c, i)*t3a(e, d, b, l, k, j) & !eaciedblkj      (+1.000)
                      + x10(e, b, a, i)*t3a(e, d, c, l, k, j) & !ebaiedclkj      (+1.000)
                      - x10(e, a, b, i)*t3a(e, d, c, l, k, j) & !eabiedclkj      (-1.000)
                      - x10(e, d, c, j)*t3a(e, b, a, l, k, i) & !edcjebalki      (-1.000)
                      + x10(e, d, b, j)*t3a(e, c, a, l, k, i) & !edbjecalki      (+1.000)
                      - x10(e, d, a, j)*t3a(e, c, b, l, k, i) & !edajecblki      (-1.000)
                      + x10(e, c, d, j)*t3a(e, b, a, l, k, i) & !ecdjebalki      (+1.000)
                      - x10(e, b, d, j)*t3a(e, c, a, l, k, i) & !ebdjecalki      (-1.000)
                      + x10(e, a, d, j)*t3a(e, c, b, l, k, i) & !eadjecblki      (+1.000)
                      - x10(e, c, b, j)*t3a(e, d, a, l, k, i) & !ecbjedalki      (-1.000)
                      + x10(e, c, a, j)*t3a(e, d, b, l, k, i) & !ecajedblki      (+1.000)
                      + x10(e, b, c, j)*t3a(e, d, a, l, k, i) & !ebcjedalki      (+1.000)
                      - x10(e, a, c, j)*t3a(e, d, b, l, k, i) & !eacjedblki      (-1.000)
                      - x10(e, b, a, j)*t3a(e, d, c, l, k, i) & !ebajedclki      (-1.000)
                      + x10(e, a, b, j)*t3a(e, d, c, l, k, i) & !eabjedclki      (+1.000)
                      + x10(e, d, c, k)*t3a(e, b, a, l, j, i) & !edckebalji      (+1.000)
                      - x10(e, d, b, k)*t3a(e, c, a, l, j, i) & !edbkecalji      (-1.000)
                      + x10(e, d, a, k)*t3a(e, c, b, l, j, i) & !edakecblji      (+1.000)
                      - x10(e, c, d, k)*t3a(e, b, a, l, j, i) & !ecdkebalji      (-1.000)
                      + x10(e, b, d, k)*t3a(e, c, a, l, j, i) & !ebdkecalji      (+1.000)
                      - x10(e, a, d, k)*t3a(e, c, b, l, j, i) & !eadkecblji      (-1.000)
                      + x10(e, c, b, k)*t3a(e, d, a, l, j, i) & !ecbkedalji      (+1.000)
                      - x10(e, c, a, k)*t3a(e, d, b, l, j, i) & !ecakedblji      (-1.000)
                      - x10(e, b, c, k)*t3a(e, d, a, l, j, i) & !ebckedalji      (-1.000)
                      + x10(e, a, c, k)*t3a(e, d, b, l, j, i) & !eackedblji      (+1.000)
                      + x10(e, b, a, k)*t3a(e, d, c, l, j, i) & !ebakedclji      (+1.000)
                      - x10(e, a, b, k)*t3a(e, d, c, l, j, i) & !eabkedclji      (-1.000)
                      - x10(e, d, c, l)*t3a(e, b, a, k, j, i) & !edclebakji      (-1.000)
                      + x10(e, d, b, l)*t3a(e, c, a, k, j, i) & !edblecakji      (+1.000)
                      - x10(e, d, a, l)*t3a(e, c, b, k, j, i) & !edalecbkji      (-1.000)
                      + x10(e, c, d, l)*t3a(e, b, a, k, j, i) & !ecdlebakji      (+1.000)
                      - x10(e, b, d, l)*t3a(e, c, a, k, j, i) & !ebdlecakji      (-1.000)
                      + x10(e, a, d, l)*t3a(e, c, b, k, j, i) & !eadlecbkji      (+1.000)
                      - x10(e, c, b, l)*t3a(e, d, a, k, j, i) & !ecbledakji      (-1.000)
                      + x10(e, c, a, l)*t3a(e, d, b, k, j, i) & !ecaledbkji      (+1.000)
                      + x10(e, b, c, l)*t3a(e, d, a, k, j, i) & !ebcledakji      (+1.000)
                      - x10(e, a, c, l)*t3a(e, d, b, k, j, i) & !eacledbkji      (-1.000)
                      - x10(e, b, a, l)*t3a(e, d, c, k, j, i) & !ebaledckji      (-1.000)
                      + x10(e, a, b, l)*t3a(e, d, c, k, j, i)          !eabledckji      (+1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x10)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s23), size(s23), '3412', s23, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s46(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s46)
    deallocate (d1)
    deallocate (b2)
    deallocate (s23)

    call sum_stripe(4, shape(x9), size(x9), '4123', -1.000, &
                    x9, s46)
    deallocate (s46)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,x9) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x9(m, d, j, i)*t3a(c, b, a, m, l, k) & !mdjicbamlk      (+1.000)
                      - x9(m, c, j, i)*t3a(d, b, a, m, l, k) & !mcjidbamlk      (-1.000)
                      + x9(m, b, j, i)*t3a(d, c, a, m, l, k) & !mbjidcamlk      (+1.000)
                      - x9(m, a, j, i)*t3a(d, c, b, m, l, k) & !majidcbmlk      (-1.000)
                      - x9(m, d, k, i)*t3a(c, b, a, m, l, j) & !mdkicbamlj      (-1.000)
                      + x9(m, c, k, i)*t3a(d, b, a, m, l, j) & !mckidbamlj      (+1.000)
                      - x9(m, b, k, i)*t3a(d, c, a, m, l, j) & !mbkidcamlj      (-1.000)
                      + x9(m, a, k, i)*t3a(d, c, b, m, l, j) & !makidcbmlj      (+1.000)
                      + x9(m, d, l, i)*t3a(c, b, a, m, k, j) & !mdlicbamkj      (+1.000)
                      - x9(m, c, l, i)*t3a(d, b, a, m, k, j) & !mclidbamkj      (-1.000)
                      + x9(m, b, l, i)*t3a(d, c, a, m, k, j) & !mblidcamkj      (+1.000)
                      - x9(m, a, l, i)*t3a(d, c, b, m, k, j) & !malidcbmkj      (-1.000)
                      - x9(m, d, i, j)*t3a(c, b, a, m, l, k) & !mdijcbamlk      (-1.000)
                      + x9(m, c, i, j)*t3a(d, b, a, m, l, k) & !mcijdbamlk      (+1.000)
                      - x9(m, b, i, j)*t3a(d, c, a, m, l, k) & !mbijdcamlk      (-1.000)
                      + x9(m, a, i, j)*t3a(d, c, b, m, l, k) & !maijdcbmlk      (+1.000)
                      + x9(m, d, i, k)*t3a(c, b, a, m, l, j) & !mdikcbamlj      (+1.000)
                      - x9(m, c, i, k)*t3a(d, b, a, m, l, j) & !mcikdbamlj      (-1.000)
                      + x9(m, b, i, k)*t3a(d, c, a, m, l, j) & !mbikdcamlj      (+1.000)
                      - x9(m, a, i, k)*t3a(d, c, b, m, l, j) & !maikdcbmlj      (-1.000)
                      - x9(m, d, i, l)*t3a(c, b, a, m, k, j) & !mdilcbamkj      (-1.000)
                      + x9(m, c, i, l)*t3a(d, b, a, m, k, j) & !mcildbamkj      (+1.000)
                      - x9(m, b, i, l)*t3a(d, c, a, m, k, j) & !mbildcamkj      (-1.000)
                      + x9(m, a, i, l)*t3a(d, c, b, m, k, j) & !maildcbmkj      (+1.000)
                      + x9(m, d, k, j)*t3a(c, b, a, m, l, i) & !mdkjcbamli      (+1.000)
                      - x9(m, c, k, j)*t3a(d, b, a, m, l, i) & !mckjdbamli      (-1.000)
                      + x9(m, b, k, j)*t3a(d, c, a, m, l, i) & !mbkjdcamli      (+1.000)
                      - x9(m, a, k, j)*t3a(d, c, b, m, l, i) & !makjdcbmli      (-1.000)
                      - x9(m, d, l, j)*t3a(c, b, a, m, k, i) & !mdljcbamki      (-1.000)
                      + x9(m, c, l, j)*t3a(d, b, a, m, k, i) & !mcljdbamki      (+1.000)
                      - x9(m, b, l, j)*t3a(d, c, a, m, k, i) & !mbljdcamki      (-1.000)
                      + x9(m, a, l, j)*t3a(d, c, b, m, k, i) & !maljdcbmki      (+1.000)
                      - x9(m, d, j, k)*t3a(c, b, a, m, l, i) & !mdjkcbamli      (-1.000)
                      + x9(m, c, j, k)*t3a(d, b, a, m, l, i) & !mcjkdbamli      (+1.000)
                      - x9(m, b, j, k)*t3a(d, c, a, m, l, i) & !mbjkdcamli      (-1.000)
                      + x9(m, a, j, k)*t3a(d, c, b, m, l, i) & !majkdcbmli      (+1.000)
                      + x9(m, d, j, l)*t3a(c, b, a, m, k, i) & !mdjlcbamki      (+1.000)
                      - x9(m, c, j, l)*t3a(d, b, a, m, k, i) & !mcjldbamki      (-1.000)
                      + x9(m, b, j, l)*t3a(d, c, a, m, k, i) & !mbjldcamki      (+1.000)
                      - x9(m, a, j, l)*t3a(d, c, b, m, k, i) & !majldcbmki      (-1.000)
                      + x9(m, d, l, k)*t3a(c, b, a, m, j, i) & !mdlkcbamji      (+1.000)
                      - x9(m, c, l, k)*t3a(d, b, a, m, j, i) & !mclkdbamji      (-1.000)
                      + x9(m, b, l, k)*t3a(d, c, a, m, j, i) & !mblkdcamji      (+1.000)
                      - x9(m, a, l, k)*t3a(d, c, b, m, j, i) & !malkdcbmji      (-1.000)
                      - x9(m, d, k, l)*t3a(c, b, a, m, j, i) & !mdklcbamji      (-1.000)
                      + x9(m, c, k, l)*t3a(d, b, a, m, j, i) & !mckldbamji      (+1.000)
                      - x9(m, b, k, l)*t3a(d, c, a, m, j, i) & !mbkldcamji      (-1.000)
                      + x9(m, a, k, l)*t3a(d, c, b, m, j, i)           !makldcbmji      (+1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x9)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (q9(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, d2, q9)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x3), size(x3), '21', 1.000, x3, &
                    q9)
    deallocate (q9)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '1432', t2b, d2)
    allocate (q10(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k2*k1*k4
    call egemm(i1, i2, i3, d1, d2, q10)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x4), size(x4), '21', -1.000, x4, &
                    q10)
    deallocate (q10)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s24(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s24)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x8), size(x8), '3421', 1.000, x8, &
                    s24)
    deallocate (s24)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3124', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '154236', t3a, f2)
    allocate (s25(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k1*k1*k3
    call egemm(i1, i2, i3, d1, f2, s25)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,s25) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum & !top 2 switched
                      + (s25(c, a, l, f)*t3a(f, d, b, k, j, i) & !calffdbkji      (+0.500)
                         - s25(b, a, l, f)*t3a(f, d, c, k, j, i) & !balffdckji      (-0.500)
                         - s25(c, b, l, f)*t3a(f, d, a, k, j, i) & !cblffdakji      (-0.500)
                         - s25(d, a, l, f)*t3a(f, c, b, k, j, i) & !dalffcbkji      (-0.500)
                         + s25(d, b, l, f)*t3a(f, c, a, k, j, i) & !dblffcakji      (+0.500)
                         - s25(d, c, l, f)*t3a(f, b, a, k, j, i) & !dclffbakji      (-0.500)
                         + s25(b, a, k, f)*t3a(f, d, c, l, j, i) & !bakffdclji      (+0.500)
                         - s25(c, a, k, f)*t3a(f, d, b, l, j, i) & !cakffdblji      (-0.500)
                         + s25(c, b, k, f)*t3a(f, d, a, l, j, i) & !cbkffdalji      (+0.500)
                         + s25(d, a, k, f)*t3a(f, c, b, l, j, i) & !dakffcblji      (+0.500)
                         - s25(d, b, k, f)*t3a(f, c, a, l, j, i) & !dbkffcalji      (-0.500)
                         + s25(d, c, k, f)*t3a(f, b, a, l, j, i) & !dckffbalji      (+0.500)
                         - s25(b, a, j, f)*t3a(f, d, c, l, k, i) & !bajffdclki      (-0.500)
                         + s25(c, a, j, f)*t3a(f, d, b, l, k, i) & !cajffdblki      (+0.500)
                         - s25(c, b, j, f)*t3a(f, d, a, l, k, i) & !cbjffdalki      (-0.500)
                         - s25(d, a, j, f)*t3a(f, c, b, l, k, i) & !dajffcblki      (-0.500)
                         + s25(d, b, j, f)*t3a(f, c, a, l, k, i) & !dbjffcalki      (+0.500)
                         - s25(d, c, j, f)*t3a(f, b, a, l, k, i) & !dcjffbalki      (-0.500)
                         + s25(d, c, i, f)*t3a(f, b, a, l, k, j) & !dciffbalkj      (+0.500)
                         - s25(d, b, i, f)*t3a(f, c, a, l, k, j) & !dbiffcalkj      (-0.500)
                         + s25(d, a, i, f)*t3a(f, c, b, l, k, j) & !daiffcblkj      (+0.500)
                         + s25(c, b, i, f)*t3a(f, d, a, l, k, j) & !cbiffdalkj      (+0.500)
                         - s25(c, a, i, f)*t3a(f, d, b, l, k, j) & !caiffdblkj      (-0.500)
                         + s25(b, a, i, f)*t3a(f, d, c, l, k, j))/2.0d0   !baiffdclkj      (+0.500)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s25)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (f2(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '123456', t3a, f2)
    allocate (u14(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k1*k1*k3
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, f2, u14)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,u14,t3a) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum & !top 2 switched
                      + (u14(c, k, j, i, m, n)*t3a(d, b, a, n, m, l) & !ckjimndbanml    (+0.250)
                         - u14(d, k, j, i, m, n)*t3a(c, b, a, n, m, l) & !dkjimncbanml    (-0.250)
                         - u14(b, k, j, i, m, n)*t3a(d, c, a, n, m, l) & !bkjimndcanml    (-0.250)
                         + u14(a, k, j, i, m, n)*t3a(d, c, b, n, m, l) & !akjimndcbnml    (+0.250)
                         + u14(d, l, j, i, m, n)*t3a(c, b, a, n, m, k) & !dljimncbanmk    (+0.250)
                         - u14(c, l, j, i, m, n)*t3a(d, b, a, n, m, k) & !cljimndbanmk    (-0.250)
                         + u14(b, l, j, i, m, n)*t3a(d, c, a, n, m, k) & !bljimndcanmk    (+0.250)
                         - u14(a, l, j, i, m, n)*t3a(d, c, b, n, m, k) & !aljimndcbnmk    (-0.250)
                         - u14(d, l, k, i, m, n)*t3a(c, b, a, n, m, j) & !dlkimncbanmj    (-0.250)
                         + u14(c, l, k, i, m, n)*t3a(d, b, a, n, m, j) & !clkimndbanmj    (+0.250)
                         - u14(b, l, k, i, m, n)*t3a(d, c, a, n, m, j) & !blkimndcanmj    (-0.250)
                         + u14(a, l, k, i, m, n)*t3a(d, c, b, n, m, j) & !alkimndcbnmj    (+0.250)
                         - u14(a, l, k, j, m, n)*t3a(d, c, b, n, m, i) & !alkjmndcbnmi    (-0.250)
                         + u14(b, l, k, j, m, n)*t3a(d, c, a, n, m, i) & !blkjmndcanmi    (+0.250)
                         - u14(c, l, k, j, m, n)*t3a(d, b, a, n, m, i) & !clkjmndbanmi    (-0.250)
                         + u14(d, l, k, j, m, n)*t3a(c, b, a, n, m, i))/4.0d0  !dlkjmncbanmi    (+0.250)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u14), size(u14), '561234', u14, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u25(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1*k3*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u25)
    deallocate (f1)
    deallocate (b2)
    deallocate (u14)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u25) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + (u25(d, n, a, l, k, j)*t2a(c, b, n, i) & !dnalkjcbni      (+0.500)
                         - u25(d, n, b, l, k, j)*t2a(c, a, n, i) & !dnblkjcani      (-0.500)
                         + u25(d, n, c, l, k, j)*t2a(b, a, n, i) & !dnclkjbani      (+0.500)
                         - u25(c, n, a, l, k, j)*t2a(d, b, n, i) & !cnalkjdbni      (-0.500)
                         + u25(c, n, b, l, k, j)*t2a(d, a, n, i) & !cnblkjdani      (+0.500)
                         + u25(b, n, a, l, k, j)*t2a(d, c, n, i) & !bnalkjdcni      (+0.500)
                         - u25(a, n, b, l, k, j)*t2a(d, c, n, i) & !anblkjdcni      (-0.500)
                         - u25(b, n, c, l, k, j)*t2a(d, a, n, i) & !bnclkjdani      (-0.500)
                         + u25(a, n, c, l, k, j)*t2a(d, b, n, i) & !anclkjdbni      (+0.500)
                         - u25(c, n, d, l, k, j)*t2a(b, a, n, i) & !cndlkjbani      (-0.500)
                         + u25(b, n, d, l, k, j)*t2a(c, a, n, i) & !bndlkjcani      (+0.500)
                         - u25(a, n, d, l, k, j)*t2a(c, b, n, i) & !andlkjcbni      (-0.500)
                         - u25(d, n, a, l, k, i)*t2a(c, b, n, j) & !dnalkicbnj      (-0.500)
                         + u25(d, n, b, l, k, i)*t2a(c, a, n, j) & !dnblkicanj      (+0.500)
                         - u25(d, n, c, l, k, i)*t2a(b, a, n, j) & !dnclkibanj      (-0.500)
                         + u25(c, n, a, l, k, i)*t2a(d, b, n, j) & !cnalkidbnj      (+0.500)
                         - u25(c, n, b, l, k, i)*t2a(d, a, n, j) & !cnblkidanj      (-0.500)
                         - u25(b, n, a, l, k, i)*t2a(d, c, n, j) & !bnalkidcnj      (-0.500)
                         + u25(a, n, b, l, k, i)*t2a(d, c, n, j) & !anblkidcnj      (+0.500)
                         + u25(b, n, c, l, k, i)*t2a(d, a, n, j) & !bnclkidanj      (+0.500)
                         - u25(a, n, c, l, k, i)*t2a(d, b, n, j) & !anclkidbnj      (-0.500)
                         + u25(c, n, d, l, k, i)*t2a(b, a, n, j) & !cndlkibanj      (+0.500)
                         - u25(b, n, d, l, k, i)*t2a(c, a, n, j) & !bndlkicanj      (-0.500)
                         + u25(a, n, d, l, k, i)*t2a(c, b, n, j) & !andlkicbnj      (+0.500)
                         + u25(d, n, a, l, j, i)*t2a(c, b, n, k) & !dnaljicbnk      (+0.500)
                         - u25(d, n, b, l, j, i)*t2a(c, a, n, k) & !dnbljicank      (-0.500)
                         + u25(d, n, c, l, j, i)*t2a(b, a, n, k) & !dncljibank      (+0.500)
                         - u25(c, n, a, l, j, i)*t2a(d, b, n, k) & !cnaljidbnk      (-0.500)
                         + u25(c, n, b, l, j, i)*t2a(d, a, n, k) & !cnbljidank      (+0.500)
                         + u25(b, n, a, l, j, i)*t2a(d, c, n, k) & !bnaljidcnk      (+0.500)
                         - u25(a, n, b, l, j, i)*t2a(d, c, n, k) & !anbljidcnk      (-0.500)
                         - u25(b, n, c, l, j, i)*t2a(d, a, n, k) & !bncljidank      (-0.500)
                         + u25(a, n, c, l, j, i)*t2a(d, b, n, k) & !ancljidbnk      (+0.500)
                         - u25(c, n, d, l, j, i)*t2a(b, a, n, k) & !cndljibank      (-0.500)
                         + u25(b, n, d, l, j, i)*t2a(c, a, n, k) & !bndljicank      (+0.500)
                         - u25(a, n, d, l, j, i)*t2a(c, b, n, k) & !andljicbnk      (-0.500)
                         - u25(d, n, a, k, j, i)*t2a(c, b, n, l) & !dnakjicbnl      (-0.500)
                         + u25(d, n, b, k, j, i)*t2a(c, a, n, l) & !dnbkjicanl      (+0.500)
                         - u25(d, n, c, k, j, i)*t2a(b, a, n, l) & !dnckjibanl      (-0.500)
                         + u25(c, n, a, k, j, i)*t2a(d, b, n, l) & !cnakjidbnl      (+0.500)
                         - u25(c, n, b, k, j, i)*t2a(d, a, n, l) & !cnbkjidanl      (-0.500)
                         - u25(b, n, a, k, j, i)*t2a(d, c, n, l) & !bnakjidcnl      (-0.500)
                         + u25(a, n, b, k, j, i)*t2a(d, c, n, l) & !anbkjidcnl      (+0.500)
                         + u25(b, n, c, k, j, i)*t2a(d, a, n, l) & !bnckjidanl      (+0.500)
                         - u25(a, n, c, k, j, i)*t2a(d, b, n, l) & !anckjidbnl      (-0.500)
                         + u25(c, n, d, k, j, i)*t2a(b, a, n, l) & !cndkjibanl      (+0.500)
                         - u25(b, n, d, k, j, i)*t2a(c, a, n, l) & !bndkjicanl      (-0.500)
                         + u25(a, n, d, k, j, i)*t2a(c, b, n, l))/2.0d0   !andkjicbnl      (+0.500)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u25)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (f2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '124356', t3a, f2)
    allocate (s26(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, f2, s26)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,s26) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + (s26(a, l, k, n)*t3a(d, c, b, n, j, i) & !alkndcbnji      (+0.500)
                         - s26(b, l, k, n)*t3a(d, c, a, n, j, i) & !blkndcanji      (-0.500)
                         + s26(c, l, k, n)*t3a(d, b, a, n, j, i) & !clkndbanji      (+0.500)
                         - s26(d, j, i, n)*t3a(c, b, a, n, l, k) & !djincbanlk      (-0.500)
                         - s26(d, l, k, n)*t3a(c, b, a, n, j, i) & !dlkncbanji      (-0.500)
                         + s26(c, j, i, n)*t3a(d, b, a, n, l, k) & !cjindbanlk      (+0.500)
                         - s26(b, j, i, n)*t3a(d, c, a, n, l, k) & !bjindcanlk      (-0.500)
                         + s26(a, j, i, n)*t3a(d, c, b, n, l, k) & !ajindcbnlk      (+0.500)
                         - s26(a, l, j, n)*t3a(d, c, b, n, k, i) & !aljndcbnki      (-0.500)
                         + s26(b, l, j, n)*t3a(d, c, a, n, k, i) & !bljndcanki      (+0.500)
                         - s26(c, l, j, n)*t3a(d, b, a, n, k, i) & !cljndbanki      (-0.500)
                         + s26(d, k, i, n)*t3a(c, b, a, n, l, j) & !dkincbanlj      (+0.500)
                         + s26(d, l, j, n)*t3a(c, b, a, n, k, i) & !dljncbanki      (+0.500)
                         - s26(c, k, i, n)*t3a(d, b, a, n, l, j) & !ckindbanlj      (-0.500)
                         + s26(b, k, i, n)*t3a(d, c, a, n, l, j) & !bkindcanlj      (+0.500)
                         - s26(a, k, i, n)*t3a(d, c, b, n, l, j) & !akindcbnlj      (-0.500)
                         + s26(a, k, j, n)*t3a(d, c, b, n, l, i) & !akjndcbnli      (+0.500)
                         - s26(b, k, j, n)*t3a(d, c, a, n, l, i) & !bkjndcanli      (-0.500)
                         + s26(c, k, j, n)*t3a(d, b, a, n, l, i) & !ckjndbanli      (+0.500)
                         - s26(d, l, i, n)*t3a(c, b, a, n, k, j) & !dlincbankj      (-0.500)
                         - s26(d, k, j, n)*t3a(c, b, a, n, l, i) & !dkjncbanli      (-0.500)
                         + s26(c, l, i, n)*t3a(d, b, a, n, k, j) & !clindbankj      (+0.500)
                         - s26(b, l, i, n)*t3a(d, c, a, n, k, j) & !blindcankj      (-0.500)
                         + s26(a, l, i, n)*t3a(d, c, b, n, k, j))/2.0d0   !alindcbnkj      (+0.500)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s26)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '142356', t3a, f2)
    allocate (u15(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k1*k3*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u15)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,u15) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do n = n0 + 1, n1
                sum = sum &
                      + u15(b, a, l, k, f, n)*t3a(f, d, c, n, j, i) & !balkfnfdcnji    (+1.000)
                      - u15(c, a, l, k, f, n)*t3a(f, d, b, n, j, i) & !calkfnfdbnji    (-1.000)
                      + u15(d, a, j, i, f, n)*t3a(f, c, b, n, l, k) & !dajifnfcbnlk    (+1.000)
                      + u15(d, a, l, k, f, n)*t3a(f, c, b, n, j, i) & !dalkfnfcbnji    (+1.000)
                      - u15(c, a, j, i, f, n)*t3a(f, d, b, n, l, k) & !cajifnfdbnlk    (-1.000)
                      + u15(b, a, j, i, f, n)*t3a(f, d, c, n, l, k) & !bajifnfdcnlk    (+1.000)
                      - u15(b, a, l, j, f, n)*t3a(f, d, c, n, k, i) & !baljfnfdcnki    (-1.000)
                      + u15(c, a, l, j, f, n)*t3a(f, d, b, n, k, i) & !caljfnfdbnki    (+1.000)
                      - u15(d, a, k, i, f, n)*t3a(f, c, b, n, l, j) & !dakifnfcbnlj    (-1.000)
                      - u15(d, a, l, j, f, n)*t3a(f, c, b, n, k, i) & !daljfnfcbnki    (-1.000)
                      + u15(c, a, k, i, f, n)*t3a(f, d, b, n, l, j) & !cakifnfdbnlj    (+1.000)
                      - u15(b, a, k, i, f, n)*t3a(f, d, c, n, l, j) & !bakifnfdcnlj    (-1.000)
                      + u15(b, a, k, j, f, n)*t3a(f, d, c, n, l, i) & !bakjfnfdcnli    (+1.000)
                      - u15(c, a, k, j, f, n)*t3a(f, d, b, n, l, i) & !cakjfnfdbnli    (-1.000)
                      + u15(d, a, l, i, f, n)*t3a(f, c, b, n, k, j) & !dalifnfcbnkj    (+1.000)
                      + u15(d, a, k, j, f, n)*t3a(f, c, b, n, l, i) & !dakjfnfcbnli    (+1.000)
                      - u15(c, a, l, i, f, n)*t3a(f, d, b, n, k, j) & !califnfdbnkj    (-1.000)
                      + u15(b, a, l, i, f, n)*t3a(f, d, c, n, k, j)      !balifnfdcnkj    (+1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u15), size(u15), '561234', u15, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u23(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u23)
    deallocate (f1)
    deallocate (b2)
    deallocate (u15)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u23) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + u23(i, m, b, a, l, k)*t2a(d, c, m, j) & !imbalkdcmj      (+1.000)
                      - u23(i, m, c, a, l, k)*t2a(d, b, m, j) & !imcalkdbmj      (-1.000)
                      + u23(i, m, c, b, l, k)*t2a(d, a, m, j) & !imcblkdamj      (+1.000)
                      + u23(i, m, d, a, l, k)*t2a(c, b, m, j) & !imdalkcbmj      (+1.000)
                      - u23(i, m, d, b, l, k)*t2a(c, a, m, j) & !imdblkcamj      (-1.000)
                      + u23(i, m, d, c, l, k)*t2a(b, a, m, j) & !imdclkbamj      (+1.000)
                      - u23(i, m, b, a, l, j)*t2a(d, c, m, k) & !imbaljdcmk      (-1.000)
                      + u23(i, m, c, a, l, j)*t2a(d, b, m, k) & !imcaljdbmk      (+1.000)
                      - u23(i, m, c, b, l, j)*t2a(d, a, m, k) & !imcbljdamk      (-1.000)
                      - u23(i, m, d, a, l, j)*t2a(c, b, m, k) & !imdaljcbmk      (-1.000)
                      + u23(i, m, d, b, l, j)*t2a(c, a, m, k) & !imdbljcamk      (+1.000)
                      - u23(i, m, d, c, l, j)*t2a(b, a, m, k) & !imdcljbamk      (-1.000)
                      + u23(i, m, b, a, k, j)*t2a(d, c, m, l) & !imbakjdcml      (+1.000)
                      - u23(i, m, c, a, k, j)*t2a(d, b, m, l) & !imcakjdbml      (-1.000)
                      + u23(i, m, c, b, k, j)*t2a(d, a, m, l) & !imcbkjdaml      (+1.000)
                      + u23(i, m, d, a, k, j)*t2a(c, b, m, l) & !imdakjcbml      (+1.000)
                      - u23(i, m, d, b, k, j)*t2a(c, a, m, l) & !imdbkjcaml      (-1.000)
                      + u23(i, m, d, c, k, j)*t2a(b, a, m, l) & !imdckjbaml      (+1.000)
                      - u23(j, m, b, a, l, k)*t2a(d, c, m, i) & !jmbalkdcmi      (-1.000)
                      + u23(j, m, c, a, l, k)*t2a(d, b, m, i) & !jmcalkdbmi      (+1.000)
                      - u23(j, m, c, b, l, k)*t2a(d, a, m, i) & !jmcblkdami      (-1.000)
                      - u23(j, m, d, a, l, k)*t2a(c, b, m, i) & !jmdalkcbmi      (-1.000)
                      + u23(j, m, d, b, l, k)*t2a(c, a, m, i) & !jmdblkcami      (+1.000)
                      - u23(j, m, d, c, l, k)*t2a(b, a, m, i) & !jmdclkbami      (-1.000)
                      + u23(k, m, b, a, l, j)*t2a(d, c, m, i) & !kmbaljdcmi      (+1.000)
                      - u23(k, m, c, a, l, j)*t2a(d, b, m, i) & !kmcaljdbmi      (-1.000)
                      + u23(k, m, c, b, l, j)*t2a(d, a, m, i) & !kmcbljdami      (+1.000)
                      + u23(k, m, d, a, l, j)*t2a(c, b, m, i) & !kmdaljcbmi      (+1.000)
                      - u23(k, m, d, b, l, j)*t2a(c, a, m, i) & !kmdbljcami      (-1.000)
                      + u23(k, m, d, c, l, j)*t2a(b, a, m, i) & !kmdcljbami      (+1.000)
                      - u23(l, m, b, a, k, j)*t2a(d, c, m, i) & !lmbakjdcmi      (-1.000)
                      + u23(l, m, c, a, k, j)*t2a(d, b, m, i) & !lmcakjdbmi      (+1.000)
                      - u23(l, m, c, b, k, j)*t2a(d, a, m, i) & !lmcbkjdami      (-1.000)
                      - u23(l, m, d, a, k, j)*t2a(c, b, m, i) & !lmdakjcbmi      (-1.000)
                      + u23(l, m, d, b, k, j)*t2a(c, a, m, i) & !lmdbkjcami      (+1.000)
                      - u23(l, m, d, c, k, j)*t2a(b, a, m, i) & !lmdckjbami      (-1.000)
                      + u23(j, m, b, a, l, i)*t2a(d, c, m, k) & !jmbalidcmk      (+1.000)
                      - u23(j, m, c, a, l, i)*t2a(d, b, m, k) & !jmcalidbmk      (-1.000)
                      + u23(j, m, c, b, l, i)*t2a(d, a, m, k) & !jmcblidamk      (+1.000)
                      + u23(j, m, d, a, l, i)*t2a(c, b, m, k) & !jmdalicbmk      (+1.000)
                      - u23(j, m, d, b, l, i)*t2a(c, a, m, k) & !jmdblicamk      (-1.000)
                      + u23(j, m, d, c, l, i)*t2a(b, a, m, k) & !jmdclibamk      (+1.000)
                      - u23(j, m, b, a, k, i)*t2a(d, c, m, l) & !jmbakidcml      (-1.000)
                      + u23(j, m, c, a, k, i)*t2a(d, b, m, l) & !jmcakidbml      (+1.000)
                      - u23(j, m, c, b, k, i)*t2a(d, a, m, l) & !jmcbkidaml      (-1.000)
                      - u23(j, m, d, a, k, i)*t2a(c, b, m, l) & !jmdakicbml      (-1.000)
                      + u23(j, m, d, b, k, i)*t2a(c, a, m, l) & !jmdbkicaml      (+1.000)
                      - u23(j, m, d, c, k, i)*t2a(b, a, m, l) & !jmdckibaml      (-1.000)
                      - u23(k, m, b, a, l, i)*t2a(d, c, m, j) & !kmbalidcmj      (-1.000)
                      + u23(k, m, c, a, l, i)*t2a(d, b, m, j) & !kmcalidbmj      (+1.000)
                      - u23(k, m, c, b, l, i)*t2a(d, a, m, j) & !kmcblidamj      (-1.000)
                      - u23(k, m, d, a, l, i)*t2a(c, b, m, j) & !kmdalicbmj      (-1.000)
                      + u23(k, m, d, b, l, i)*t2a(c, a, m, j) & !kmdblicamj      (+1.000)
                      - u23(k, m, d, c, l, i)*t2a(b, a, m, j) & !kmdclibamj      (-1.000)
                      + u23(l, m, b, a, k, i)*t2a(d, c, m, j) & !lmbakidcmj      (+1.000)
                      - u23(l, m, c, a, k, i)*t2a(d, b, m, j) & !lmcakidbmj      (-1.000)
                      + u23(l, m, c, b, k, i)*t2a(d, a, m, j) & !lmcbkidamj      (+1.000)
                      + u23(l, m, d, a, k, i)*t2a(c, b, m, j) & !lmdakicbmj      (+1.000)
                      - u23(l, m, d, b, k, i)*t2a(c, a, m, j) & !lmdbkicamj      (-1.000)
                      + u23(l, m, d, c, k, i)*t2a(b, a, m, j) & !lmdckibamj      (+1.000)
                      + u23(k, m, b, a, j, i)*t2a(d, c, m, l) & !kmbajidcml      (+1.000)
                      - u23(k, m, c, a, j, i)*t2a(d, b, m, l) & !kmcajidbml      (-1.000)
                      + u23(k, m, c, b, j, i)*t2a(d, a, m, l) & !kmcbjidaml      (+1.000)
                      + u23(k, m, d, a, j, i)*t2a(c, b, m, l) & !kmdajicbml      (+1.000)
                      - u23(k, m, d, b, j, i)*t2a(c, a, m, l) & !kmdbjicaml      (-1.000)
                      + u23(k, m, d, c, j, i)*t2a(b, a, m, l) & !kmdcjibaml      (+1.000)
                      - u23(l, m, b, a, j, i)*t2a(d, c, m, k) & !lmbajidcmk      (-1.000)
                      + u23(l, m, c, a, j, i)*t2a(d, b, m, k) & !lmcajidbmk      (+1.000)
                      - u23(l, m, c, b, j, i)*t2a(d, a, m, k) & !lmcbjidamk      (-1.000)
                      - u23(l, m, d, a, j, i)*t2a(c, b, m, k) & !lmdajicbmk      (-1.000)
                      + u23(l, m, d, b, j, i)*t2a(c, a, m, k) & !lmdbjicamk      (+1.000)
                      - u23(l, m, d, c, j, i)*t2a(b, a, m, k)          !lmdcjibamk      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u23)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '154236', t3b, f2)
    allocate (s27(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k2*k1*k4
    call egemm(i1, i2, i3, d1, f2, s27)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,s27) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + s27(b, a, l, e)*t3a(e, d, c, k, j, i) & !baleedckji      (+1.000)
                      - s27(c, a, l, e)*t3a(e, d, b, k, j, i) & !caleedbkji      (-1.000)
                      + s27(c, b, l, e)*t3a(e, d, a, k, j, i) & !cbleedakji      (+1.000)
                      + s27(d, a, l, e)*t3a(e, c, b, k, j, i) & !daleecbkji      (+1.000)
                      - s27(d, b, l, e)*t3a(e, c, a, k, j, i) & !dbleecakji      (-1.000)
                      + s27(d, c, l, e)*t3a(e, b, a, k, j, i) & !dcleebakji      (+1.000)
                      - s27(b, a, k, e)*t3a(e, d, c, l, j, i) & !bakeedclji      (-1.000)
                      + s27(c, a, k, e)*t3a(e, d, b, l, j, i) & !cakeedblji      (+1.000)
                      - s27(c, b, k, e)*t3a(e, d, a, l, j, i) & !cbkeedalji      (-1.000)
                      - s27(d, a, k, e)*t3a(e, c, b, l, j, i) & !dakeecblji      (-1.000)
                      + s27(d, b, k, e)*t3a(e, c, a, l, j, i) & !dbkeecalji      (+1.000)
                      - s27(d, c, k, e)*t3a(e, b, a, l, j, i) & !dckeebalji      (-1.000)
                      + s27(b, a, j, e)*t3a(e, d, c, l, k, i) & !bajeedclki      (+1.000)
                      - s27(c, a, j, e)*t3a(e, d, b, l, k, i) & !cajeedblki      (-1.000)
                      + s27(c, b, j, e)*t3a(e, d, a, l, k, i) & !cbjeedalki      (+1.000)
                      + s27(d, a, j, e)*t3a(e, c, b, l, k, i) & !dajeecblki      (+1.000)
                      - s27(d, b, j, e)*t3a(e, c, a, l, k, i) & !dbjeecalki      (-1.000)
                      + s27(d, c, j, e)*t3a(e, b, a, l, k, i) & !dcjeebalki      (+1.000)
                      - s27(b, a, i, e)*t3a(e, d, c, l, k, j) & !baieedclkj      (-1.000)
                      + s27(c, a, i, e)*t3a(e, d, b, l, k, j) & !caieedblkj      (+1.000)
                      - s27(c, b, i, e)*t3a(e, d, a, l, k, j) & !cbieedalkj      (-1.000)
                      - s27(d, a, i, e)*t3a(e, c, b, l, k, j) & !daieecblkj      (-1.000)
                      + s27(d, b, i, e)*t3a(e, c, a, l, k, j) & !dbieecalkj      (+1.000)
                      - s27(d, c, i, e)*t3a(e, b, a, l, k, j)          !dcieebalkj      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s27)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '124356', t3b, f2)
    allocate (s28(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, f2, s28)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,s28) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - s28(a, l, k, m)*t3a(d, c, b, m, j, i) & !alkmdcbmji      (-1.000)
                      + s28(b, l, k, m)*t3a(d, c, a, m, j, i) & !blkmdcamji      (+1.000)
                      - s28(c, l, k, m)*t3a(d, b, a, m, j, i) & !clkmdbamji      (-1.000)
                      + s28(d, l, k, m)*t3a(c, b, a, m, j, i) & !dlkmcbamji      (+1.000)
                      + s28(a, l, j, m)*t3a(d, c, b, m, k, i) & !aljmdcbmki      (+1.000)
                      - s28(b, l, j, m)*t3a(d, c, a, m, k, i) & !bljmdcamki      (-1.000)
                      + s28(c, l, j, m)*t3a(d, b, a, m, k, i) & !cljmdbamki      (+1.000)
                      - s28(d, l, j, m)*t3a(c, b, a, m, k, i) & !dljmcbamki      (-1.000)
                      - s28(a, k, j, m)*t3a(d, c, b, m, l, i) & !akjmdcbmli      (-1.000)
                      + s28(b, k, j, m)*t3a(d, c, a, m, l, i) & !bkjmdcamli      (+1.000)
                      - s28(c, k, j, m)*t3a(d, b, a, m, l, i) & !ckjmdbamli      (-1.000)
                      + s28(d, k, j, m)*t3a(c, b, a, m, l, i) & !dkjmcbamli      (+1.000)
                      - s28(a, l, i, m)*t3a(d, c, b, m, k, j) & !alimdcbmkj      (-1.000)
                      + s28(b, l, i, m)*t3a(d, c, a, m, k, j) & !blimdcamkj      (+1.000)
                      - s28(c, l, i, m)*t3a(d, b, a, m, k, j) & !climdbamkj      (-1.000)
                      + s28(d, l, i, m)*t3a(c, b, a, m, k, j) & !dlimcbamkj      (+1.000)
                      + s28(a, k, i, m)*t3a(d, c, b, m, l, j) & !akimdcbmlj      (+1.000)
                      - s28(b, k, i, m)*t3a(d, c, a, m, l, j) & !bkimdcamlj      (-1.000)
                      + s28(c, k, i, m)*t3a(d, b, a, m, l, j) & !ckimdbamlj      (+1.000)
                      - s28(d, k, i, m)*t3a(c, b, a, m, l, j) & !dkimcbamlj      (-1.000)
                      - s28(a, j, i, m)*t3a(d, c, b, m, l, k) & !ajimdcbmlk      (-1.000)
                      + s28(b, j, i, m)*t3a(d, c, a, m, l, k) & !bjimdcamlk      (+1.000)
                      - s28(c, j, i, m)*t3a(d, b, a, m, l, k) & !cjimdbamlk      (-1.000)
                      + s28(d, j, i, m)*t3a(c, b, a, m, l, k)          !djimcbamlk      (+1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s28)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '142356', t3a, f2)
    allocate (u16(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k1*k3*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u16)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,u16,t3b) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do n = n0 + 1, n2
                sum = sum &
                      + u16(d, c, j, i, f, n)*t3b(f, b, a, n, l, k) & !dcjifnfbanlk    (+1.000)
                      - u16(d, b, j, i, f, n)*t3b(f, c, a, n, l, k) & !dbjifnfcanlk    (-1.000)
                      + u16(d, a, j, i, f, n)*t3b(f, c, b, n, l, k) & !dajifnfcbnlk    (+1.000)
                      + u16(c, b, j, i, f, n)*t3b(f, d, a, n, l, k) & !cbjifnfdanlk    (+1.000)
                      - u16(c, a, j, i, f, n)*t3b(f, d, b, n, l, k) & !cajifnfdbnlk    (-1.000)
                      + u16(b, a, j, i, f, n)*t3b(f, d, c, n, l, k) & !bajifnfdcnlk    (+1.000)
                      - u16(d, c, k, i, f, n)*t3b(f, b, a, n, l, j) & !dckifnfbanlj    (-1.000)
                      + u16(d, b, k, i, f, n)*t3b(f, c, a, n, l, j) & !dbkifnfcanlj    (+1.000)
                      - u16(d, a, k, i, f, n)*t3b(f, c, b, n, l, j) & !dakifnfcbnlj    (-1.000)
                      - u16(c, b, k, i, f, n)*t3b(f, d, a, n, l, j) & !cbkifnfdanlj    (-1.000)
                      + u16(c, a, k, i, f, n)*t3b(f, d, b, n, l, j) & !cakifnfdbnlj    (+1.000)
                      - u16(b, a, k, i, f, n)*t3b(f, d, c, n, l, j) & !bakifnfdcnlj    (-1.000)
                      + u16(d, c, l, i, f, n)*t3b(f, b, a, n, k, j) & !dclifnfbankj    (+1.000)
                      - u16(d, b, l, i, f, n)*t3b(f, c, a, n, k, j) & !dblifnfcankj    (-1.000)
                      + u16(d, a, l, i, f, n)*t3b(f, c, b, n, k, j) & !dalifnfcbnkj    (+1.000)
                      + u16(c, b, l, i, f, n)*t3b(f, d, a, n, k, j) & !cblifnfdankj    (+1.000)
                      - u16(c, a, l, i, f, n)*t3b(f, d, b, n, k, j) & !califnfdbnkj    (-1.000)
                      + u16(b, a, l, i, f, n)*t3b(f, d, c, n, k, j) & !balifnfdcnkj    (+1.000)
                      + u16(d, c, k, j, f, n)*t3b(f, b, a, n, l, i) & !dckjfnfbanli    (+1.000)
                      - u16(d, b, k, j, f, n)*t3b(f, c, a, n, l, i) & !dbkjfnfcanli    (-1.000)
                      + u16(d, a, k, j, f, n)*t3b(f, c, b, n, l, i) & !dakjfnfcbnli    (+1.000)
                      + u16(c, b, k, j, f, n)*t3b(f, d, a, n, l, i) & !cbkjfnfdanli    (+1.000)
                      - u16(c, a, k, j, f, n)*t3b(f, d, b, n, l, i) & !cakjfnfdbnli    (-1.000)
                      + u16(b, a, k, j, f, n)*t3b(f, d, c, n, l, i) & !bakjfnfdcnli    (+1.000)
                      - u16(d, c, l, j, f, n)*t3b(f, b, a, n, k, i) & !dcljfnfbanki    (-1.000)
                      + u16(d, b, l, j, f, n)*t3b(f, c, a, n, k, i) & !dbljfnfcanki    (+1.000)
                      - u16(d, a, l, j, f, n)*t3b(f, c, b, n, k, i) & !daljfnfcbnki    (-1.000)
                      - u16(c, b, l, j, f, n)*t3b(f, d, a, n, k, i) & !cbljfnfdanki    (-1.000)
                      + u16(c, a, l, j, f, n)*t3b(f, d, b, n, k, i) & !caljfnfdbnki    (+1.000)
                      - u16(b, a, l, j, f, n)*t3b(f, d, c, n, k, i) & !baljfnfdcnki    (-1.000)
                      + u16(d, c, l, k, f, n)*t3b(f, b, a, n, j, i) & !dclkfnfbanji    (+1.000)
                      - u16(d, b, l, k, f, n)*t3b(f, c, a, n, j, i) & !dblkfnfcanji    (-1.000)
                      + u16(d, a, l, k, f, n)*t3b(f, c, b, n, j, i) & !dalkfnfcbnji    (+1.000)
                      + u16(c, b, l, k, f, n)*t3b(f, d, a, n, j, i) & !cblkfnfdanji    (+1.000)
                      - u16(c, a, l, k, f, n)*t3b(f, d, b, n, j, i) & !calkfnfdbnji    (-1.000)
                      + u16(b, a, l, k, f, n)*t3b(f, d, c, n, j, i)      !balkfnfdcnji    (+1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u16)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
    allocate (u17(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k1*k3*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u17)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,u17,t3b) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do n = n0 + 1, n2
                sum = sum &
                      + u17(b, a, l, k, f, n)*t3b(f, d, c, n, j, i) & !balkfnfdcnji    (+1.000)
                      - u17(c, a, l, k, f, n)*t3b(f, d, b, n, j, i) & !calkfnfdbnji    (-1.000)
                      + u17(d, a, j, i, f, n)*t3b(f, c, b, n, l, k) & !dajifnfcbnlk    (+1.000)
                      + u17(d, a, l, k, f, n)*t3b(f, c, b, n, j, i) & !dalkfnfcbnji    (+1.000)
                      - u17(c, a, j, i, f, n)*t3b(f, d, b, n, l, k) & !cajifnfdbnlk    (-1.000)
                      + u17(b, a, j, i, f, n)*t3b(f, d, c, n, l, k) & !bajifnfdcnlk    (+1.000)
                      - u17(b, a, l, j, f, n)*t3b(f, d, c, n, k, i) & !baljfnfdcnki    (-1.000)
                      + u17(c, a, l, j, f, n)*t3b(f, d, b, n, k, i) & !caljfnfdbnki    (+1.000)
                      - u17(d, a, k, i, f, n)*t3b(f, c, b, n, l, j) & !dakifnfcbnlj    (-1.000)
                      - u17(d, a, l, j, f, n)*t3b(f, c, b, n, k, i) & !daljfnfcbnki    (-1.000)
                      + u17(c, a, k, i, f, n)*t3b(f, d, b, n, l, j) & !cakifnfdbnlj    (+1.000)
                      - u17(b, a, k, i, f, n)*t3b(f, d, c, n, l, j) & !bakifnfdcnlj    (-1.000)
                      + u17(b, a, k, j, f, n)*t3b(f, d, c, n, l, i) & !bakjfnfdcnli    (+1.000)
                      - u17(c, a, k, j, f, n)*t3b(f, d, b, n, l, i) & !cakjfnfdbnli    (-1.000)
                      + u17(d, a, l, i, f, n)*t3b(f, c, b, n, k, j) & !dalifnfcbnkj    (+1.000)
                      + u17(d, a, k, j, f, n)*t3b(f, c, b, n, l, i) & !dakjfnfcbnli    (+1.000)
                      - u17(c, a, l, i, f, n)*t3b(f, d, b, n, k, j) & !califnfdbnkj    (-1.000)
                      + u17(b, a, l, i, f, n)*t3b(f, d, c, n, k, j)      !balifnfdcnkj    (+1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u17)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s33(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s33)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s33), size(s33), '3214', s33, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q11(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q11)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(2, shape(x3), size(x3), '21', -1.000, x3, &
                    q11)
    deallocate (q11)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s33), size(s33), '3214', s33, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s35(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s35)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '3241', -1.000, &
                    x6, s35)

  call sum_shift(4,shape(intr),size(intr),shape(x6), &
   size(x6),(/n0-n0,n1-n0,n1-n0,n0-n0/),'3142',1.000,intr,x6)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t4a,x6) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      - x6(m, e, d, i)*t4a(e, c, b, a, m, l, k, j) & !mediecbamlkj    (-1.000)
                      + x6(m, e, c, i)*t4a(e, d, b, a, m, l, k, j) & !meciedbamlkj    (+1.000)
                      - x6(m, e, b, i)*t4a(e, d, c, a, m, l, k, j) & !mebiedcamlkj    (-1.000)
                      + x6(m, e, a, i)*t4a(e, d, c, b, m, l, k, j) & !meaiedcbmlkj    (+1.000)
                      + x6(m, e, d, j)*t4a(e, c, b, a, m, l, k, i) & !medjecbamlki    (+1.000)
                      - x6(m, e, c, j)*t4a(e, d, b, a, m, l, k, i) & !mecjedbamlki    (-1.000)
                      + x6(m, e, b, j)*t4a(e, d, c, a, m, l, k, i) & !mebjedcamlki    (+1.000)
                      - x6(m, e, a, j)*t4a(e, d, c, b, m, l, k, i) & !meajedcbmlki    (-1.000)
                      - x6(m, e, d, k)*t4a(e, c, b, a, m, l, j, i) & !medkecbamlji    (-1.000)
                      + x6(m, e, c, k)*t4a(e, d, b, a, m, l, j, i) & !meckedbamlji    (+1.000)
                      - x6(m, e, b, k)*t4a(e, d, c, a, m, l, j, i) & !mebkedcamlji    (-1.000)
                      + x6(m, e, a, k)*t4a(e, d, c, b, m, l, j, i) & !meakedcbmlji    (+1.000)
                      + x6(m, e, d, l)*t4a(e, c, b, a, m, k, j, i) & !medlecbamkji    (+1.000)
                      - x6(m, e, c, l)*t4a(e, d, b, a, m, k, j, i) & !mecledbamkji    (-1.000)
                      + x6(m, e, b, l)*t4a(e, d, c, a, m, k, j, i) & !mebledcamkji    (+1.000)
                      - x6(m, e, a, l)*t4a(e, d, c, b, m, k, j, i)       !mealedcbmkji    (-1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x6)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s35), size(s35), '4213', s35, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s51(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s51)
    deallocate (d1)
    deallocate (b2)
    deallocate (s35)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s51)
    deallocate (s51)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s33), size(s33), '2314', s33, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s34(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s34)
    deallocate (d1)
    deallocate (b2)
    deallocate (s33)

    call sum_stripe(4, shape(x5), size(x5), '3241', 1.000, x5, &
                    s34)

  call sum_shift(4,shape(intr),size(intr),shape(x5), &
   size(x5),(/n0-n0,n0-n0,n0-n0,n0-n0/),'2143',1.000,intr,x5)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,x5,t4a) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (x5(n, m, j, i)*t4a(d, c, b, a, n, m, l, k) & !nmjidcbanmlk    (+0.500)
                         - x5(n, m, k, i)*t4a(d, c, b, a, n, m, l, j) & !nmkidcbanmlj    (-0.500)
                         + x5(n, m, l, i)*t4a(d, c, b, a, n, m, k, j) & !nmlidcbanmkj    (+0.500)
                         + x5(n, m, k, j)*t4a(d, c, b, a, n, m, l, i) & !nmkjdcbanmli    (+0.500)
                         - x5(n, m, l, j)*t4a(d, c, b, a, n, m, k, i) & !nmljdcbanmki    (-0.500)
                         + x5(n, m, l, k)*t4a(d, c, b, a, n, m, j, i))/2.0d0!nmlkdcbanmji    (+0.500)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x5)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s34), size(s34), '2413', s34, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u33(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u33)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u33) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + u33(b, a, l, n, j, i)*t2a(d, c, n, k) & !balnjidcnk      (+1.000)
                      - u33(c, a, l, n, j, i)*t2a(d, b, n, k) & !calnjidbnk      (-1.000)
                      - u33(d, a, k, n, j, i)*t2a(c, b, n, l) & !daknjicbnl      (-1.000)
                      + u33(d, a, l, n, j, i)*t2a(c, b, n, k) & !dalnjicbnk      (+1.000)
                      + u33(c, a, k, n, j, i)*t2a(d, b, n, l) & !caknjidbnl      (+1.000)
                      - u33(b, a, k, n, j, i)*t2a(d, c, n, l) & !baknjidcnl      (-1.000)
                      - u33(b, a, l, n, k, i)*t2a(d, c, n, j) & !balnkidcnj      (-1.000)
                      + u33(c, a, l, n, k, i)*t2a(d, b, n, j) & !calnkidbnj      (+1.000)
                      + u33(d, a, j, n, k, i)*t2a(c, b, n, l) & !dajnkicbnl      (+1.000)
                      - u33(d, a, l, n, k, i)*t2a(c, b, n, j) & !dalnkicbnj      (-1.000)
                      - u33(c, a, j, n, k, i)*t2a(d, b, n, l) & !cajnkidbnl      (-1.000)
                      + u33(b, a, j, n, k, i)*t2a(d, c, n, l) & !bajnkidcnl      (+1.000)
                      + u33(b, a, k, n, l, i)*t2a(d, c, n, j) & !baknlidcnj      (+1.000)
                      - u33(c, a, k, n, l, i)*t2a(d, b, n, j) & !caknlidbnj      (-1.000)
                      - u33(d, a, j, n, l, i)*t2a(c, b, n, k) & !dajnlicbnk      (-1.000)
                      + u33(d, a, k, n, l, i)*t2a(c, b, n, j) & !daknlicbnj      (+1.000)
                      + u33(c, a, j, n, l, i)*t2a(d, b, n, k) & !cajnlidbnk      (+1.000)
                      - u33(b, a, j, n, l, i)*t2a(d, c, n, k) & !bajnlidcnk      (-1.000)
                      + u33(b, a, l, n, k, j)*t2a(d, c, n, i) & !balnkjdcni      (+1.000)
                      - u33(c, a, l, n, k, j)*t2a(d, b, n, i) & !calnkjdbni      (-1.000)
                      - u33(d, a, i, n, k, j)*t2a(c, b, n, l) & !dainkjcbnl      (-1.000)
                      + u33(d, a, l, n, k, j)*t2a(c, b, n, i) & !dalnkjcbni      (+1.000)
                      + u33(c, a, i, n, k, j)*t2a(d, b, n, l) & !cainkjdbnl      (+1.000)
                      - u33(b, a, i, n, k, j)*t2a(d, c, n, l) & !bainkjdcnl      (-1.000)
                      - u33(b, a, k, n, l, j)*t2a(d, c, n, i) & !baknljdcni      (-1.000)
                      + u33(c, a, k, n, l, j)*t2a(d, b, n, i) & !caknljdbni      (+1.000)
                      + u33(d, a, i, n, l, j)*t2a(c, b, n, k) & !dainljcbnk      (+1.000)
                      - u33(d, a, k, n, l, j)*t2a(c, b, n, i) & !daknljcbni      (-1.000)
                      - u33(c, a, i, n, l, j)*t2a(d, b, n, k) & !cainljdbnk      (-1.000)
                      + u33(b, a, i, n, l, j)*t2a(d, c, n, k) & !bainljdcnk      (+1.000)
                      + u33(b, a, j, n, l, k)*t2a(d, c, n, i) & !bajnlkdcni      (+1.000)
                      - u33(c, a, j, n, l, k)*t2a(d, b, n, i) & !cajnlkdbni      (-1.000)
                      - u33(d, a, i, n, l, k)*t2a(c, b, n, j) & !dainlkcbnj      (-1.000)
                      + u33(d, a, j, n, l, k)*t2a(c, b, n, i) & !dajnlkcbni      (+1.000)
                      + u33(c, a, i, n, l, k)*t2a(d, b, n, j) & !cainlkdbnj      (+1.000)
                      - u33(b, a, i, n, l, k)*t2a(d, c, n, j)          !bainlkdcnj      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u33)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s34), size(s34), '2413', s34, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s50(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s50)
    deallocate (d1)
    deallocate (b2)
    deallocate (s34)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s50)
    deallocate (s50)

  call sum_shift(4,shape(intr),size(intr),shape(x1), &
   size(x1),(/n0-n0,n1-n0,n0-n0,n0-n0/),'2143',1.000,intr,x1)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t3a,x1) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x1(m, d, j, i)*t3a(c, b, a, m, l, k) & !mdjicbamlk      (+1.000)
                      - x1(m, c, j, i)*t3a(d, b, a, m, l, k) & !mcjidbamlk      (-1.000)
                      + x1(m, b, j, i)*t3a(d, c, a, m, l, k) & !mbjidcamlk      (+1.000)
                      - x1(m, a, j, i)*t3a(d, c, b, m, l, k) & !majidcbmlk      (-1.000)
                      - x1(m, d, k, i)*t3a(c, b, a, m, l, j) & !mdkicbamlj      (-1.000)
                      + x1(m, c, k, i)*t3a(d, b, a, m, l, j) & !mckidbamlj      (+1.000)
                      - x1(m, b, k, i)*t3a(d, c, a, m, l, j) & !mbkidcamlj      (-1.000)
                      + x1(m, a, k, i)*t3a(d, c, b, m, l, j) & !makidcbmlj      (+1.000)
                      + x1(m, d, l, i)*t3a(c, b, a, m, k, j) & !mdlicbamkj      (+1.000)
                      - x1(m, c, l, i)*t3a(d, b, a, m, k, j) & !mclidbamkj      (-1.000)
                      + x1(m, b, l, i)*t3a(d, c, a, m, k, j) & !mblidcamkj      (+1.000)
                      - x1(m, a, l, i)*t3a(d, c, b, m, k, j) & !malidcbmkj      (-1.000)
                      + x1(m, d, k, j)*t3a(c, b, a, m, l, i) & !mdkjcbamli      (+1.000)
                      - x1(m, c, k, j)*t3a(d, b, a, m, l, i) & !mckjdbamli      (-1.000)
                      + x1(m, b, k, j)*t3a(d, c, a, m, l, i) & !mbkjdcamli      (+1.000)
                      - x1(m, a, k, j)*t3a(d, c, b, m, l, i) & !makjdcbmli      (-1.000)
                      - x1(m, d, l, j)*t3a(c, b, a, m, k, i) & !mdljcbamki      (-1.000)
                      + x1(m, c, l, j)*t3a(d, b, a, m, k, i) & !mcljdbamki      (+1.000)
                      - x1(m, b, l, j)*t3a(d, c, a, m, k, i) & !mbljdcamki      (-1.000)
                      + x1(m, a, l, j)*t3a(d, c, b, m, k, i) & !maljdcbmki      (+1.000)
                      + x1(m, d, l, k)*t3a(c, b, a, m, j, i) & !mdlkcbamji      (+1.000)
                      - x1(m, c, l, k)*t3a(d, b, a, m, j, i) & !mclkdbamji      (-1.000)
                      + x1(m, b, l, k)*t3a(d, c, a, m, j, i) & !mblkdcamji      (+1.000)
                      - x1(m, a, l, k)*t3a(d, c, b, m, j, i)           !malkdcbmji      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x1)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '1432', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s36(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s36)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s36), size(s36), '3421', s36, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q12(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k1*k3
    call egemm1(i1, i3, d1, b2, q12)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 + q12
    deallocate (q12)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s36), size(s36), '4231', s36, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s37(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s37)
    deallocate (d1)
    deallocate (b2)
    deallocate (s36)

    call sum_stripe(4, shape(x7), size(x7), '3124', 1.000, x7, &
                    s37)
    deallocate (s37)

  call sum_shift(4,shape(intr),size(intr),shape(x7), &
   size(x7),(/n1-n0,n1-n0,n1-n0,n1-n0/),'4321',1.000,intr,x7)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,x7,t4a) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do f = n1 + 1, n3
                sum = sum &
                      + (x7(f, e, d, c)*t4a(f, e, b, a, l, k, j, i) & !fedcfebalkji    (+0.500)
                         - x7(f, e, d, b)*t4a(f, e, c, a, l, k, j, i) & !fedbfecalkji    (-0.500)
                         + x7(f, e, d, a)*t4a(f, e, c, b, l, k, j, i) & !fedafecblkji    (+0.500)
                         + x7(f, e, c, b)*t4a(f, e, d, a, l, k, j, i) & !fecbfedalkji    (+0.500)
                         - x7(f, e, c, a)*t4a(f, e, d, b, l, k, j, i) & !fecafedblkji    (-0.500)
                         + x7(f, e, b, a)*t4a(f, e, d, c, l, k, j, i))/2.0d0!febafedclkji    (+0.500)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x7)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s38(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s38)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s38), size(s38), '2431', s38, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
    allocate (u26(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k1*k3*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u26)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t2a,u26) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + u26(b, a, l, k, m, i)*t2a(d, c, m, j) & !balkmidcmj      (+1.000)
                      - u26(c, a, l, k, m, i)*t2a(d, b, m, j) & !calkmidbmj      (-1.000)
                      + u26(c, b, l, k, m, i)*t2a(d, a, m, j) & !cblkmidamj      (+1.000)
                      + u26(d, a, l, k, m, i)*t2a(c, b, m, j) & !dalkmicbmj      (+1.000)
                      - u26(d, b, l, k, m, i)*t2a(c, a, m, j) & !dblkmicamj      (-1.000)
                      + u26(d, c, l, k, m, i)*t2a(b, a, m, j) & !dclkmibamj      (+1.000)
                      - u26(b, a, l, j, m, i)*t2a(d, c, m, k) & !baljmidcmk      (-1.000)
                      + u26(c, a, l, j, m, i)*t2a(d, b, m, k) & !caljmidbmk      (+1.000)
                      - u26(c, b, l, j, m, i)*t2a(d, a, m, k) & !cbljmidamk      (-1.000)
                      - u26(d, a, l, j, m, i)*t2a(c, b, m, k) & !daljmicbmk      (-1.000)
                      + u26(d, b, l, j, m, i)*t2a(c, a, m, k) & !dbljmicamk      (+1.000)
                      - u26(d, c, l, j, m, i)*t2a(b, a, m, k) & !dcljmibamk      (-1.000)
                      + u26(b, a, k, j, m, i)*t2a(d, c, m, l) & !bakjmidcml      (+1.000)
                      - u26(c, a, k, j, m, i)*t2a(d, b, m, l) & !cakjmidbml      (-1.000)
                      + u26(c, b, k, j, m, i)*t2a(d, a, m, l) & !cbkjmidaml      (+1.000)
                      + u26(d, a, k, j, m, i)*t2a(c, b, m, l) & !dakjmicbml      (+1.000)
                      - u26(d, b, k, j, m, i)*t2a(c, a, m, l) & !dbkjmicaml      (-1.000)
                      + u26(d, c, k, j, m, i)*t2a(b, a, m, l) & !dckjmibaml      (+1.000)
                      - u26(b, a, l, k, m, j)*t2a(d, c, m, i) & !balkmjdcmi      (-1.000)
                      + u26(c, a, l, k, m, j)*t2a(d, b, m, i) & !calkmjdbmi      (+1.000)
                      - u26(c, b, l, k, m, j)*t2a(d, a, m, i) & !cblkmjdami      (-1.000)
                      - u26(d, a, l, k, m, j)*t2a(c, b, m, i) & !dalkmjcbmi      (-1.000)
                      + u26(d, b, l, k, m, j)*t2a(c, a, m, i) & !dblkmjcami      (+1.000)
                      - u26(d, c, l, k, m, j)*t2a(b, a, m, i) & !dclkmjbami      (-1.000)
                      + u26(b, a, l, j, m, k)*t2a(d, c, m, i) & !baljmkdcmi      (+1.000)
                      - u26(c, a, l, j, m, k)*t2a(d, b, m, i) & !caljmkdbmi      (-1.000)
                      + u26(c, b, l, j, m, k)*t2a(d, a, m, i) & !cbljmkdami      (+1.000)
                      + u26(d, a, l, j, m, k)*t2a(c, b, m, i) & !daljmkcbmi      (+1.000)
                      - u26(d, b, l, j, m, k)*t2a(c, a, m, i) & !dbljmkcami      (-1.000)
                      + u26(d, c, l, j, m, k)*t2a(b, a, m, i) & !dcljmkbami      (+1.000)
                      - u26(b, a, k, j, m, l)*t2a(d, c, m, i) & !bakjmldcmi      (-1.000)
                      + u26(c, a, k, j, m, l)*t2a(d, b, m, i) & !cakjmldbmi      (+1.000)
                      - u26(c, b, k, j, m, l)*t2a(d, a, m, i) & !cbkjmldami      (-1.000)
                      - u26(d, a, k, j, m, l)*t2a(c, b, m, i) & !dakjmlcbmi      (-1.000)
                      + u26(d, b, k, j, m, l)*t2a(c, a, m, i) & !dbkjmlcami      (+1.000)
                      - u26(d, c, k, j, m, l)*t2a(b, a, m, i) & !dckjmlbami      (-1.000)
                      + u26(b, a, l, i, m, j)*t2a(d, c, m, k) & !balimjdcmk      (+1.000)
                      - u26(c, a, l, i, m, j)*t2a(d, b, m, k) & !calimjdbmk      (-1.000)
                      + u26(c, b, l, i, m, j)*t2a(d, a, m, k) & !cblimjdamk      (+1.000)
                      + u26(d, a, l, i, m, j)*t2a(c, b, m, k) & !dalimjcbmk      (+1.000)
                      - u26(d, b, l, i, m, j)*t2a(c, a, m, k) & !dblimjcamk      (-1.000)
                      + u26(d, c, l, i, m, j)*t2a(b, a, m, k) & !dclimjbamk      (+1.000)
                      - u26(b, a, k, i, m, j)*t2a(d, c, m, l) & !bakimjdcml      (-1.000)
                      + u26(c, a, k, i, m, j)*t2a(d, b, m, l) & !cakimjdbml      (+1.000)
                      - u26(c, b, k, i, m, j)*t2a(d, a, m, l) & !cbkimjdaml      (-1.000)
                      - u26(d, a, k, i, m, j)*t2a(c, b, m, l) & !dakimjcbml      (-1.000)
                      + u26(d, b, k, i, m, j)*t2a(c, a, m, l) & !dbkimjcaml      (+1.000)
                      - u26(d, c, k, i, m, j)*t2a(b, a, m, l) & !dckimjbaml      (-1.000)
                      - u26(b, a, l, i, m, k)*t2a(d, c, m, j) & !balimkdcmj      (-1.000)
                      + u26(c, a, l, i, m, k)*t2a(d, b, m, j) & !calimkdbmj      (+1.000)
                      - u26(c, b, l, i, m, k)*t2a(d, a, m, j) & !cblimkdamj      (-1.000)
                      - u26(d, a, l, i, m, k)*t2a(c, b, m, j) & !dalimkcbmj      (-1.000)
                      + u26(d, b, l, i, m, k)*t2a(c, a, m, j) & !dblimkcamj      (+1.000)
                      - u26(d, c, l, i, m, k)*t2a(b, a, m, j) & !dclimkbamj      (-1.000)
                      + u26(b, a, k, i, m, l)*t2a(d, c, m, j) & !bakimldcmj      (+1.000)
                      - u26(c, a, k, i, m, l)*t2a(d, b, m, j) & !cakimldbmj      (-1.000)
                      + u26(c, b, k, i, m, l)*t2a(d, a, m, j) & !cbkimldamj      (+1.000)
                      + u26(d, a, k, i, m, l)*t2a(c, b, m, j) & !dakimlcbmj      (+1.000)
                      - u26(d, b, k, i, m, l)*t2a(c, a, m, j) & !dbkimlcamj      (-1.000)
                      + u26(d, c, k, i, m, l)*t2a(b, a, m, j) & !dckimlbamj      (+1.000)
                      + u26(b, a, j, i, m, k)*t2a(d, c, m, l) & !bajimkdcml      (+1.000)
                      - u26(c, a, j, i, m, k)*t2a(d, b, m, l) & !cajimkdbml      (-1.000)
                      + u26(c, b, j, i, m, k)*t2a(d, a, m, l) & !cbjimkdaml      (+1.000)
                      + u26(d, a, j, i, m, k)*t2a(c, b, m, l) & !dajimkcbml      (+1.000)
                      - u26(d, b, j, i, m, k)*t2a(c, a, m, l) & !dbjimkcaml      (-1.000)
                      + u26(d, c, j, i, m, k)*t2a(b, a, m, l) & !dcjimkbaml      (+1.000)
                      - u26(b, a, j, i, m, l)*t2a(d, c, m, k) & !bajimldcmk      (-1.000)
                      + u26(c, a, j, i, m, l)*t2a(d, b, m, k) & !cajimldbmk      (+1.000)
                      - u26(c, b, j, i, m, l)*t2a(d, a, m, k) & !cbjimldamk      (-1.000)
                      - u26(d, a, j, i, m, l)*t2a(c, b, m, k) & !dajimlcbmk      (-1.000)
                      + u26(d, b, j, i, m, l)*t2a(c, a, m, k) & !dbjimlcamk      (+1.000)
                      - u26(d, c, j, i, m, l)*t2a(b, a, m, k)          !dcjimlbamk      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u26)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s38), size(s38), '2431', s38, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q13(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q13)
    deallocate (d1)
    deallocate (b2)

    x3 = x3 + q13
    deallocate (q13)

  call sum_shift(2,shape(fockr),size(fockr),shape(x3), &
   size(x3),(/n0,n0/),'12',1.000,fockr,x3)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,x3,t4a) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x3(m, i)*t4a(d, c, b, a, m, l, k, j) & !midcbamlkj      (+1.000)
                      - x3(m, j)*t4a(d, c, b, a, m, l, k, i) & !mjdcbamlki      (-1.000)
                      + x3(m, k)*t4a(d, c, b, a, m, l, j, i) & !mkdcbamlji      (+1.000)
                      - x3(m, l)*t4a(d, c, b, a, m, k, j, i)           !mldcbamkji      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x3)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s38), size(s38), '3214', s38, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s39(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s39)
    deallocate (d1)
    deallocate (b2)
    deallocate (s38)

    call sum_stripe(4, shape(x8), size(x8), '3241', -1.000, &
                    x8, s39)
    deallocate (s39)

  call sum_shift(4,shape(intm),size(intm),shape(x8), &
   size(x8),(/n0-n0,n2-n0,n1-n0,n0-n0/),'3142',1.000,intm,x8)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t4b,x8) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      - x8(m, e, d, i)*t4b(e, c, b, a, m, l, k, j) & !mediecbamlkj    (-1.000)
                      + x8(m, e, c, i)*t4b(e, d, b, a, m, l, k, j) & !meciedbamlkj    (+1.000)
                      - x8(m, e, b, i)*t4b(e, d, c, a, m, l, k, j) & !mebiedcamlkj    (-1.000)
                      + x8(m, e, a, i)*t4b(e, d, c, b, m, l, k, j) & !meaiedcbmlkj    (+1.000)
                      + x8(m, e, d, j)*t4b(e, c, b, a, m, l, k, i) & !medjecbamlki    (+1.000)
                      - x8(m, e, c, j)*t4b(e, d, b, a, m, l, k, i) & !mecjedbamlki    (-1.000)
                      + x8(m, e, b, j)*t4b(e, d, c, a, m, l, k, i) & !mebjedcamlki    (+1.000)
                      - x8(m, e, a, j)*t4b(e, d, c, b, m, l, k, i) & !meajedcbmlki    (-1.000)
                      - x8(m, e, d, k)*t4b(e, c, b, a, m, l, j, i) & !medkecbamlji    (-1.000)
                      + x8(m, e, c, k)*t4b(e, d, b, a, m, l, j, i) & !meckedbamlji    (+1.000)
                      - x8(m, e, b, k)*t4b(e, d, c, a, m, l, j, i) & !mebkedcamlji    (-1.000)
                      + x8(m, e, a, k)*t4b(e, d, c, b, m, l, j, i) & !meakedcbmlji    (+1.000)
                      + x8(m, e, d, l)*t4b(e, c, b, a, m, k, j, i) & !medlecbamkji    (+1.000)
                      - x8(m, e, c, l)*t4b(e, d, b, a, m, k, j, i) & !mecledbamkji    (-1.000)
                      + x8(m, e, b, l)*t4b(e, d, c, a, m, k, j, i) & !mebledcamkji    (+1.000)
                      - x8(m, e, a, l)*t4b(e, d, c, b, m, k, j, i)       !mealedcbmkji    (-1.000)
            end do; end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x8)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q14(n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q14)
    deallocate (d1)
    deallocate (b2)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q14), size(q14), '21', q14, b1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s49(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s49)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s49)
    deallocate (s49)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q14), size(q14), '21', q14, b1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q15(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, b1, b2, q15)
    deallocate (b1)
    deallocate (b2)
    deallocate (q14)

    call sum_stripe(2, shape(x4), size(x4), '21', -1.000, x4, &
                    q15)
    deallocate (q15)

  call sum_shift(2,shape(fockr),size(fockr),shape(x4), &
   size(x4),(/n1,n1/),'21',1.000,fockr,x4)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,t4a,x4) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x4(e, d)*t4a(e, c, b, a, l, k, j, i) & !edecbalkji      (+1.000)
                      - x4(e, c)*t4a(e, d, b, a, l, k, j, i) & !ecedbalkji      (-1.000)
                      + x4(e, b)*t4a(e, d, c, a, l, k, j, i) & !ebedcalkji      (+1.000)
                      - x4(e, a)*t4a(e, d, c, b, l, k, j, i)           !eaedcblkji      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x4)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q16(n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i3 = k1*k3
    call egemm1(i1, i3, d1, b2, q16)
    deallocate (d1)
    deallocate (b2)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q16), size(q16), '21', q16, b1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s45(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s45)
    deallocate (b1)
    deallocate (d2)
    deallocate (q16)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s45)
    deallocate (s45)

  call sum_shift(4,shape(intr),size(intr),shape(x2), &
   size(x2),(/n1-n0,n1-n0,n1-n0,n0-n0/),'3241',1.000,intr,x2)

    do i = n0 + 1, n1 - 3; do j = i + 1, n1 - 2; do k = j + 1, n1 - 1; do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4a,x2,t3a) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 3; do b = a + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x2(e, d, c, i)*t3a(e, b, a, l, k, j) & !edciebalkj      (+1.000)
                      - x2(e, d, b, i)*t3a(e, c, a, l, k, j) & !edbiecalkj      (-1.000)
                      + x2(e, d, a, i)*t3a(e, c, b, l, k, j) & !edaiecblkj      (+1.000)
                      + x2(e, c, b, i)*t3a(e, d, a, l, k, j) & !ecbiedalkj      (+1.000)
                      - x2(e, c, a, i)*t3a(e, d, b, l, k, j) & !ecaiedblkj      (-1.000)
                      + x2(e, b, a, i)*t3a(e, d, c, l, k, j) & !ebaiedclkj      (+1.000)
                      - x2(e, d, c, j)*t3a(e, b, a, l, k, i) & !edcjebalki      (-1.000)
                      + x2(e, d, b, j)*t3a(e, c, a, l, k, i) & !edbjecalki      (+1.000)
                      - x2(e, d, a, j)*t3a(e, c, b, l, k, i) & !edajecblki      (-1.000)
                      - x2(e, c, b, j)*t3a(e, d, a, l, k, i) & !ecbjedalki      (-1.000)
                      + x2(e, c, a, j)*t3a(e, d, b, l, k, i) & !ecajedblki      (+1.000)
                      - x2(e, b, a, j)*t3a(e, d, c, l, k, i) & !ebajedclki      (-1.000)
                      + x2(e, d, c, k)*t3a(e, b, a, l, j, i) & !edckebalji      (+1.000)
                      - x2(e, d, b, k)*t3a(e, c, a, l, j, i) & !edbkecalji      (-1.000)
                      + x2(e, d, a, k)*t3a(e, c, b, l, j, i) & !edakecblji      (+1.000)
                      + x2(e, c, b, k)*t3a(e, d, a, l, j, i) & !ecbkedalji      (+1.000)
                      - x2(e, c, a, k)*t3a(e, d, b, l, j, i) & !ecakedblji      (-1.000)
                      + x2(e, b, a, k)*t3a(e, d, c, l, j, i) & !ebakedclji      (+1.000)
                      - x2(e, d, c, l)*t3a(e, b, a, k, j, i) & !edclebakji      (-1.000)
                      + x2(e, d, b, l)*t3a(e, c, a, k, j, i) & !edblecakji      (+1.000)
                      - x2(e, d, a, l)*t3a(e, c, b, k, j, i) & !edalecbkji      (-1.000)
                      - x2(e, c, b, l)*t3a(e, d, a, k, j, i) & !ecbledakji      (-1.000)
                      + x2(e, c, a, l)*t3a(e, d, b, k, j, i) & !ecaledbkji      (+1.000)
                      - x2(e, b, a, l)*t3a(e, d, c, k, j, i)           !ebaledckji      (-1.000)
            end do
            v4a(d, c, b, a, l, k, j, i) = v4a(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x2)

    do i = n0 + 1, n1 - 3
    do j = i + 1, n1 - 2
    do k = j + 1, n1 - 1
    do l = k + 1, n1
        if (indocc(l, k, j, i) .eq. 1) cycle
        do a = n1 + 1, n3 - 3
        do b = a + 1, n3 - 2
        do c = b + 1, n3 - 1
        do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            coeleft = fockr(d, d) &
                      + fockr(c, c) &
                      + fockr(b, b) &
                      + fockr(a, a) &
                      - fockr(l, l) &
                      - fockr(k, k) &
                      - fockr(j, j) &
                      - fockr(i, i) &
                      + shift
            t4a(d, c, b, a, l, k, j, i) = t4a(d, c, b, a, l, k, j, i) - v4a(d, c, b, a, l, k, j, &
                                                                            i)/coeleft
            t4a(d, c, b, a, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, b, a, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, c, a, b, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, b, c, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, a, c, b, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, b, d, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, c, d, b, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, b, c, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, d, c, b, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, c, a, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(d, b, a, c, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, c, d, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(a, b, d, c, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, d, a, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, c, a, d, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, d, c, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, a, c, d, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, c, a, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(b, d, a, c, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, b, a, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, d, a, b, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, b, d, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, a, d, b, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, l, k, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, l, k, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, l, i, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, l, i, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, i, k, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, i, k, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, i, l, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, i, l, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, l, j, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, l, j, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, i, j, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, i, j, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, j, k, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, j, k, i, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, j, i, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, j, i, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, j, l, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, j, l, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, k, l, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, k, l, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, k, i, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, k, i, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, k, j, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, d, a, k, j, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, l, k, j, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, l, k, i, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, l, i, j, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, l, i, k, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, i, k, j, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, i, k, l, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, i, l, j, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, i, l, k, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, l, j, k, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, l, j, i, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, i, j, k, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, i, j, l, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, j, k, l, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, j, k, i, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, j, i, l, k) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, j, i, k, l) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, j, l, k, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, j, l, i, k) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, k, l, j, i) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, k, l, i, j) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, k, i, j, l) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, k, i, l, j) = t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, k, j, l, i) = -t4a(d, c, b, a, l, k, j, i)
            t4a(c, b, a, d, k, j, i, l) = t4a(d, c, b, a, l, k, j, i)
        end do
        end do
        end do
        end do
    end do
    end do
    end do
    end do
    rewind (ta)
    write (ta) t4a

end subroutine t4a_update

