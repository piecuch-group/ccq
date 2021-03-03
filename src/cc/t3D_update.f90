subroutine t3d_update(n0, n1, n2, n3, k1, k2, k3, k4, lvl_q, shift, v3d &
                      , fockr, fockb, intr, intb, intm, t1a, t1b, t2a, t2b, t2c, t3a, t3b, t3c, t3d, &
                      iactoccb, iactunob, iactindt, &
                      t2diag3, t2diag4, t2diag5, t3diag1, t3diag2, t3diag3, t3diag4, t3diag5)

    integer :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p
    integer :: iactoccb, iactunob, iactindt
    integer :: ioccb, iunob
    integer, allocatable::indocc(:, :, :)
    integer, allocatable::indunocc(:, :, :)
    logical :: lvl_q
    real :: t2diag3, t2diag4, t2diag5
    real :: t3diag1, t3diag2, t3diag3, t3diag4, t3diag5
    real :: factor
    real(kind=8) :: shift, pp, coeleft, time1, time2, timt1, timt2
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
    real(kind=8) :: v3d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2)

    real(kind=8), allocatable::t4d(:, :, :, :, :, :, :, :)                     !ilias: if no quadruples comment out the following 5 lines
    real(kind=8), allocatable::t4e(:, :, :, :, :, :, :, :)

    integer :: ta, tb, tc, td, te
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

    real(kind=8), allocatable::q1(:, :)
    real(kind=8), allocatable::q2(:, :)
    real(kind=8), allocatable::q3(:, :)
    real(kind=8), allocatable::q4(:, :)
    real(kind=8), allocatable::s32(:, :, :, :)
    real(kind=8), allocatable::q16(:, :)
    real(kind=8), allocatable::q15(:, :)
    real(kind=8), allocatable::s1(:, :, :, :)
    real(kind=8), allocatable::s2(:, :, :, :)
    real(kind=8), allocatable::s3(:, :, :, :)
    real(kind=8), allocatable::s4(:, :, :, :)
    real(kind=8), allocatable::s5(:, :, :, :)
    real(kind=8), allocatable::s6(:, :, :, :)
    real(kind=8), allocatable::q5(:, :)
    real(kind=8), allocatable::q6(:, :)
    real(kind=8), allocatable::s7(:, :, :, :)
    real(kind=8), allocatable::s33(:, :, :, :)
    real(kind=8), allocatable::s8(:, :, :, :)
    real(kind=8), allocatable::s34(:, :, :, :)
    real(kind=8), allocatable::q7(:, :)
    real(kind=8), allocatable::s9(:, :, :, :)
    real(kind=8), allocatable::s36(:, :, :, :)
    real(kind=8), allocatable::s35(:, :, :, :)
    real(kind=8), allocatable::s10(:, :, :, :)
    real(kind=8), allocatable::q8(:, :)
    real(kind=8), allocatable::s11(:, :, :, :)
    real(kind=8), allocatable::s41(:, :, :, :)
    real(kind=8), allocatable::s37(:, :, :, :)
    real(kind=8), allocatable::s12(:, :, :, :)
    real(kind=8), allocatable::s42(:, :, :, :)
    real(kind=8), allocatable::q9(:, :)
    real(kind=8), allocatable::s13(:, :, :, :)
    real(kind=8), allocatable::s45(:, :, :, :)
    real(kind=8), allocatable::s44(:, :, :, :)
    real(kind=8), allocatable::s43(:, :, :, :)
    real(kind=8), allocatable::q17(:, :)
    real(kind=8), allocatable::s39(:, :, :, :)
    real(kind=8), allocatable::s51(:, :, :, :)
    real(kind=8), allocatable::s38(:, :, :, :)
    real(kind=8), allocatable::s50(:, :, :, :)
    real(kind=8), allocatable::s14(:, :, :, :)
    real(kind=8), allocatable::s48(:, :, :, :)
    real(kind=8), allocatable::s47(:, :, :, :)
    real(kind=8), allocatable::s46(:, :, :, :)
    real(kind=8), allocatable::q18(:, :)
    real(kind=8), allocatable::s40(:, :, :, :)
    real(kind=8), allocatable::q10(:, :)
    real(kind=8), allocatable::s49(:, :, :, :)
    real(kind=8), allocatable::s15(:, :, :, :)
    real(kind=8), allocatable::s16(:, :, :, :)
    real(kind=8), allocatable::s17(:, :, :, :)
    real(kind=8), allocatable::s18(:, :, :, :)
    real(kind=8), allocatable::q11(:, :)
    real(kind=8), allocatable::q12(:, :)
    real(kind=8), allocatable::s19(:, :, :, :)
    real(kind=8), allocatable::s20(:, :, :, :)
    real(kind=8), allocatable::s21(:, :, :, :)
    real(kind=8), allocatable::s22(:, :, :, :)
    real(kind=8), allocatable::s23(:, :, :, :)
    real(kind=8), allocatable::s24(:, :, :, :)
    real(kind=8), allocatable::s25(:, :, :, :)
    real(kind=8), allocatable::s26(:, :, :, :)
    real(kind=8), allocatable::s27(:, :, :, :)
    real(kind=8), allocatable::s28(:, :, :, :)
    real(kind=8), allocatable::s29(:, :, :, :)
    real(kind=8), allocatable::s30(:, :, :, :)
    real(kind=8), allocatable::q13(:, :)
    real(kind=8), allocatable::s31(:, :, :, :)
    real(kind=8), allocatable::q14(:, :)
    real(kind=8), allocatable::x1(:, :, :, :)
    real(kind=8), allocatable::z1(:, :, :, :, :, :)
    real(kind=8), allocatable::x2(:, :, :, :)
    real(kind=8), allocatable::z2(:, :, :, :, :, :)
    real(kind=8), allocatable::x3(:, :, :, :)
    real(kind=8), allocatable::z3(:, :, :, :, :, :)
    real(kind=8), allocatable::x4(:, :)
    real(kind=8), allocatable::z4(:, :, :, :, :, :)
    real(kind=8), allocatable::x5(:, :)
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
    real(kind=8), allocatable::z20(:, :, :, :, :, :)
    real(kind=8), allocatable::x16(:, :, :, :)
    real(kind=8), allocatable::z21(:, :, :, :, :, :)
    real(kind=8), allocatable::z27(:, :, :, :, :, :)
    real(kind=8), allocatable::z31(:, :, :, :, :, :)
    real(kind=8), allocatable::z77(:, :, :, :, :, :)
    real(kind=8), allocatable::z76(:, :, :, :, :, :)
    real(kind=8), allocatable::z75(:, :, :, :, :, :)
    real(kind=8), allocatable::z79(:, :, :, :, :, :)
    real(kind=8), allocatable::x17(:, :, :, :)
    real(kind=8), allocatable::z78(:, :, :, :, :, :)
    real(kind=8), allocatable::z46(:, :, :, :, :, :)
    real(kind=8), allocatable::z47(:, :, :, :, :, :)
    real(kind=8), allocatable::z48(:, :, :, :, :, :)
    real(kind=8), allocatable::z51(:, :, :, :, :, :)
    real(kind=8), allocatable::z53(:, :, :, :, :, :)
    real(kind=8), allocatable::z55(:, :, :, :, :, :)
    real(kind=8), allocatable::x999(:, :, :, :)
    real(kind=8), allocatable::z999(:, :, :, :, :, :)

    allocate (indocc(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    allocate (indunocc(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    indocc = 0
    indunocc = 0
    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        ioccb = 0
        if (i .gt. (n2 - iactoccb)) ioccb = ioccb + 1
        if (j .gt. (n2 - iactoccb)) ioccb = ioccb + 1
        if (k .gt. (n2 - iactoccb)) ioccb = ioccb + 1
        if (ioccb .lt. iactindt) indocc(k, j, i) = 1
    end do; end do; end do
    do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
        iunob = 0
        if (a .lt. (n2 + iactunob + 1)) iunob = iunob + 1
        if (b .lt. (n2 + iactunob + 1)) iunob = iunob + 1
        if (c .lt. (n2 + iactunob + 1)) iunob = iunob + 1
        if (iunob .lt. iactindt) indunocc(c, b, a) = 1
    end do; end do; end do

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q1(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q1)
    deallocate (d1)
    deallocate (b2)

    allocate (x4(n0 + 1:n2, n0 + 1:n2))
    x4 = 0.0d0
    x4 = x4 + q1
    deallocate (q1)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n2 - n0, n2 - n0/), '1342', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q2(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q2)
    deallocate (d1)
    deallocate (b2)

    allocate (x5(n2 + 1:n3, n2 + 1:n3))
    x5 = 0.0d0
    x5 = x5 + q2
    deallocate (q2)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q3(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q3)
    deallocate (d1)
    deallocate (b2)

    allocate (x9(n0 + 1:n1, n1 + 1:n3))
    x9 = 0.0d0
    x9 = x9 + q3
    deallocate (q3)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q4(n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q4)
    deallocate (d1)
    deallocate (b2)

    allocate (x12(n0 + 1:n2, n2 + 1:n3))
    x12 = 0.0d0
    x12 = x12 + q4

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(q4), size(q4), '21', q4, b1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s32(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, b1, d2, s32)
    deallocate (b1)
    deallocate (d2)

    allocate (x1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x1 = 0.0d0
    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s32)
    deallocate (s32)

    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q16(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, q4, b2, q16)
    deallocate (b2)

    call sum_stripe(2, shape(x5), size(x5), '21', -1.000, x5, &
                    q16)
    deallocate (q16)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(q4), size(q4), '21', q4, b1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q15(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, b1, b2, q15)
    deallocate (b1)
    deallocate (b2)
    deallocate (q4)

    call sum_stripe(2, shape(x4), size(x4), '21', 1.000, x4, &
                    q15)
    deallocate (q15)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s1)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s1)
    deallocate (s1)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4213', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s2)
    deallocate (d1)
    deallocate (b2)

    allocate (x15(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x15 = 0.0d0
    call sum_stripe(4, shape(x15), size(x15), '3124', 1.000, &
                    x15, s2)
    deallocate (s2)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2413', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s3(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s3)
    deallocate (d1)
    deallocate (b2)

    allocate (x16(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x16 = 0.0d0
    call sum_stripe(4, shape(x16), size(x16), '3124', 1.000, &
                    x16, s3)
    deallocate (s3)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n2 - n0/), '3421', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s4(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s4)
    deallocate (d1)
    deallocate (b2)

    allocate (x2(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x2 = 0.0d0
    call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                    s4)
    deallocate (s4)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s5(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s5)
    deallocate (d1)
    deallocate (b2)

    allocate (x3(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x3 = 0.0d0
    call sum_stripe(4, shape(x3), size(x3), '3124', -1.000, &
                    x3, s5)
    deallocate (s5)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n2 - n0/), '4132', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s6(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    i1 = k4*k3*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s6)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x3), size(x3), '4123', 1.000, x3, &
                    s6)
    deallocate (s6)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q5(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, b1, b2, q5)
    deallocate (b1)
    deallocate (b2)

    call sum_stripe(2, shape(x4), size(x4), '21', 1.000, x4, &
                    q5)
    deallocate (q5)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n0 - n0, n2 - n0/), '21', fockb, b1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q6(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, b1, b2, q6)
    deallocate (b1)
    deallocate (b2)

    call sum_stripe(2, shape(x5), size(x5), '21', -1.000, x5, &
                    q6)
    deallocate (q6)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s7(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s7)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t3d,s7) &
        !$omp private(a,b,c,n,m,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (s7(j, n, m, i)*t3d(c, b, a, n, m, k) & !jicbak (+0.500)
                         - s7(k, n, m, i)*t3d(c, b, a, n, m, j) & !kicbaj (-0.500)
                         - s7(i, n, m, j)*t3d(c, b, a, n, m, k) & !ijcbak (-0.500)
                         + s7(i, n, m, k)*t3d(c, b, a, n, m, j) & !ikcbaj (+0.500)
                         + s7(k, n, m, j)*t3d(c, b, a, n, m, i) & !kjcbai (+0.500)
                         - s7(j, n, m, k)*t3d(c, b, a, n, m, i))/2.0d0!jkcbai (-0.500)
            end do; end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s7), size(s7), '3214', s7, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s33(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s33)
    deallocate (d1)
    deallocate (b2)
    deallocate (s7)

    call sum_stripe(4, shape(x15), size(x15), '2134', -1.000, &
                    x15, s33)
    deallocate (s33)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s8(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s8)
    deallocate (d1)
    deallocate (b2)

    allocate (x7(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x7 = 0.0d0
    call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                    x7, s8)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s8), size(s8), '2314', s8, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s34(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s34)
    deallocate (d1)
    deallocate (b2)
    deallocate (s8)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s34)
    deallocate (s34)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1423', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q7(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q7)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 - q7
    deallocate (q7)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s9(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s9)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x7), size(x7), '4123', 1.000, x7, &
                    s9)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s9), size(s9), '2341', s9, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s36(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s36)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x16), size(x16), '3124', 1.000, &
                    x16, s36)
    deallocate (s36)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s9), size(s9), '3241', s9, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s35(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s35)
    deallocate (d1)
    deallocate (b2)
    deallocate (s9)

    call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                    s35)
    deallocate (s35)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s10(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s10)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t3d,s10) &
        !$omp private(a,b,c,e,f,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do f = n2 + 1, n3
                sum = sum &
                      + (s10(b, f, e, c)*t3d(f, e, a, k, j, i) & !bcakji (+0.500)
                         - s10(a, f, e, c)*t3d(f, e, b, k, j, i) & !acbkji (-0.500)
                         - s10(c, f, e, b)*t3d(f, e, a, k, j, i) & !cbakji (-0.500)
                         + s10(c, f, e, a)*t3d(f, e, b, k, j, i) & !cabkji (+0.500)
                         + s10(a, f, e, b)*t3d(f, e, c, k, j, i) & !abckji (+0.500)
                         - s10(b, f, e, a)*t3d(f, e, c, k, j, i))/2.0d0!backji (-0.500)
            end do; end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s10)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2341', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q8(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q8)
    deallocate (d1)
    deallocate (b2)

    x5 = x5 - q8
    deallocate (q8)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4213', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s11(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s11)
    deallocate (d1)
    deallocate (b2)

    allocate (x10(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    x10 = 0.0d0
    call sum_stripe(4, shape(x10), size(x10), '4123', 1.000, &
                    x10, s11)

    call sumx2134(n0, n3, n0, n2, n0, n1, n1, n3, n0, n2, x10, intm, 1.000)

    if (lvl_q) then
        allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
        rewind (td)
        read (td) t4d

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2                     !ilias: if no quadruples comment out the following 14 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x10,t4d) &
        !$omp private(a,b,c,e,n,m,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                    sum = sum &
                          - x10(n, m, e, i)*t4d(c, b, a, e, n, k, j, m) & !icbakj(-1.000)
                          + x10(n, m, e, j)*t4d(c, b, a, e, n, k, i, m) & !jcbaki(+1.000)
                          - x10(n, m, e, k)*t4d(c, b, a, e, n, j, i, m) !kcbaji(-1.000)
                end do; end do; end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4d)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x10)
    end if

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s11), size(s11), '3421', s11, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (s41(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s41)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x15), size(x15), '2314', -1.000, &
                    x15, s41)
    deallocate (s41)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s11), size(s11), '2341', s11, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s37(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s37)
    deallocate (d1)
    deallocate (b2)
    deallocate (s11)

    call sum_stripe(4, shape(x3), size(x3), '3124', -1.000, &
                    x3, s37)
    deallocate (s37)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s12(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s12)
    deallocate (d1)
    deallocate (b2)

    allocate (x11(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
    x11 = 0.0d0
    call sum_stripe(4, shape(x11), size(x11), '4123', -1.000, &
                    x11, s12)

    call sumx1432(n0, n3, n0, n1, n2, n3, n1, n3, n2, n3, x11, intm, 1.000)

    if (lvl_q) then
        allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
        rewind (td)
        read (td) t4d

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2                     !ilias: if no quadruples comment out the following 14 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x11,t4d) &
        !$omp private(a,b,c,f,e,m,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n1 + 1, n3; do f = n2 + 1, n3; do m = n0 + 1, n1
                    sum = sum &
                          + x11(m, f, e, c)*t4d(f, b, a, e, k, j, i, m) & !cbakji(+1.000)
                          - x11(m, f, e, b)*t4d(f, c, a, e, k, j, i, m) & !bcakji(-1.000)
                          + x11(m, f, e, a)*t4d(f, c, b, e, k, j, i, m) !acbkji(+1.000)
                end do; end do; end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4d)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x11)
    end if

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s12), size(s12), '2431', s12, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (s42(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s42)
    deallocate (d1)
    deallocate (d2)
    deallocate (s12)

    call sum_stripe(4, shape(x16), size(x16), '3412', -1.000, &
                    x16, s42)
    deallocate (s42)

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
    deallocate (q9)

    call sumx12(0, n3, n0, n1, n1, n3, x9, fockr, 1.000)

    if (lvl_q) then
        allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
        rewind (td)
        read (td) t4d

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2                     !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x9,t4d) &
        !$omp private(a,b,c,e,m,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n1 + 1, n3; do m = n0 + 1, n1
                    sum = sum &
                          + x9(m, e)*t4d(c, b, a, e, k, j, i, m)   !cbakji (+1.000)
                end do; end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4d)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x9)
    end if

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '3214', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s13(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s13)
    deallocate (d1)
    deallocate (b2)

    allocate (x13(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    x13 = 0.0d0
    call sum_stripe(4, shape(x13), size(x13), '4123', 1.000, &
                    x13, s13)

    call sumx2143(n0, n3, n0, n2, n0, n2, n2, n3, n0, n2, x13, intb, 1.000)

    if (lvl_q) then
        allocate (t4e(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
        rewind (te)
        read (te) t4e

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2                     !ilias: if no quadruples comment out the following 14 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t4e,x13) &
        !$omp private(a,b,c,e,n,m,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n2 + 1, n3; do m = n0 + 1, n2; do n = n0 + 1, n2
                    sum = sum & !top 2 switched
                          + (x13(n, m, e, j)*t4e(e, c, b, a, n, m, k, i) & !jcbaki(+0.500)
                             - x13(n, m, e, i)*t4e(e, c, b, a, n, m, k, j) & !icbakj(-0.500)
                             - x13(n, m, e, k)*t4e(e, c, b, a, n, m, j, i))/2.0d0!kcbaji(-0.500)
                end do; end do; end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4e)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x13)
    end if

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s13), size(s13), '2431', s13, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s45(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s45)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,s45,t2c) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - s45(b, k, m, i)*t2c(c, a, m, j) & !bkicaj (-1.000)
                      - s45(c, j, m, i)*t2c(b, a, m, k) & !cjibak (-1.000)
                      + s45(c, k, m, i)*t2c(b, a, m, j) & !ckibaj (+1.000)
                      + s45(b, j, m, i)*t2c(c, a, m, k) & !bjicak (+1.000)
                      + s45(b, k, m, j)*t2c(c, a, m, i) & !bkjcai (+1.000)
                      + s45(c, i, m, j)*t2c(b, a, m, k) & !cijbak (+1.000)
                      - s45(c, k, m, j)*t2c(b, a, m, i) & !ckjbai (-1.000)
                      - s45(b, i, m, j)*t2c(c, a, m, k) & !bijcak (-1.000)
                      - s45(b, j, m, k)*t2c(c, a, m, i) & !bjkcai (-1.000)
                      - s45(c, i, m, k)*t2c(b, a, m, j) & !cikbaj (-1.000)
                      + s45(c, j, m, k)*t2c(b, a, m, i) & !cjkbai (+1.000)
                      + s45(b, i, m, k)*t2c(c, a, m, j)     !bikcaj (+1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s45)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s13), size(s13), '3421', s13, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s44(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s44)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,s44,t2c) &
        !$omp private(a,b,c,n,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - s44(a, k, n, i)*t2c(c, b, n, j) & !akicbj (-1.000)
                      + s44(a, j, n, i)*t2c(c, b, n, k) & !ajicbk (+1.000)
                      + s44(a, k, n, j)*t2c(c, b, n, i) & !akjcbi (+1.000)
                      - s44(a, i, n, j)*t2c(c, b, n, k) & !aijcbk (-1.000)
                      - s44(a, j, n, k)*t2c(c, b, n, i) & !ajkcbi (-1.000)
                      + s44(a, i, n, k)*t2c(c, b, n, j)     !aikcbj (+1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s44)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s13), size(s13), '2341', s13, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
    allocate (s43(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k4*k4
    i3 = k2*k2
    call egemm(i1, i2, i3, d1, d2, s43)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t2c,s43) &
        !$omp private(a,b,c,f,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum & !top 2 switched
                      + (s43(c, a, f, i)*t2c(f, b, k, j) & !caibkj (+0.500)
                         - s43(b, a, f, i)*t2c(f, c, k, j) & !baickj (-0.500)
                         - s43(c, b, f, i)*t2c(f, a, k, j) & !cbiakj (-0.500)
                         + s43(b, a, f, j)*t2c(f, c, k, i) & !bajcki (+0.500)
                         - s43(c, a, f, j)*t2c(f, b, k, i) & !cajbki (-0.500)
                         + s43(c, b, f, j)*t2c(f, a, k, i) & !cbjaki (+0.500)
                         - s43(b, a, f, k)*t2c(f, c, j, i) & !bakcji (-0.500)
                         + s43(c, a, f, k)*t2c(f, b, j, i) & !cakbji (+0.500)
                         - s43(c, b, f, k)*t2c(f, a, j, i))/2.0d0!cbkaji (-0.500)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s43)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s13), size(s13), '3421', s13, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q17(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q17)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 - q17
    deallocate (q17)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s13), size(s13), '3241', s13, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s39(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s39)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                    x7, s39)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s39), size(s39), '2314', s39, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s51(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s51)
    deallocate (d1)
    deallocate (b2)
    deallocate (s39)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s51)
    deallocate (s51)

    call sumx3241(n0, n3, n2, n3, n2, n3, n2, n3, n0, n2, x2, intb, 1.000)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t2c,x2) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      - x2(e, c, b, i)*t2c(e, a, k, j) & !cbiakj (-1.000)
                      + x2(e, c, a, i)*t2c(e, b, k, j) & !caibkj (+1.000)
                      - x2(e, b, a, i)*t2c(e, c, k, j) & !baickj (-1.000)
                      + x2(e, c, b, j)*t2c(e, a, k, i) & !cbjaki (+1.000)
                      - x2(e, c, a, j)*t2c(e, b, k, i) & !cajbki (-1.000)
                      + x2(e, b, a, j)*t2c(e, c, k, i) & !bajcki (+1.000)
                      - x2(e, c, b, k)*t2c(e, a, j, i) & !cbkaji (-1.000)
                      + x2(e, c, a, k)*t2c(e, b, j, i) & !cakbji (+1.000)
                      - x2(e, b, a, k)*t2c(e, c, j, i)     !bakcji (-1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s13), size(s13), '4231', s13, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s38(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s38)
    deallocate (d1)
    deallocate (b2)
    deallocate (s13)

    allocate (x6(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    x6 = 0.0d0
    call sum_stripe(4, shape(x6), size(x6), '3124', 1.000, x6, &
                    s38)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s38), size(s38), '3214', s38, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s50(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s50)
    deallocate (d1)
    deallocate (b2)
    deallocate (s38)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s50)
    deallocate (s50)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '1243', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s14(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s14)
    deallocate (d1)
    deallocate (b2)

    allocate (x14(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    x14 = 0.0d0
    call sum_stripe(4, shape(x14), size(x14), '4123', -1.000, &
                    x14, s14)

    call sumx4132(n0, n3, n0, n2, n2, n3, n2, n3, n2, n3, x14, intb, 1.000)

    if (lvl_q) then
        allocate (t4e(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
        rewind (te)
        read (te) t4e

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2                     !ilias: if no quadruples comment out the following 14 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t4e,x14) &
        !$omp private(a,b,c,f,e,m,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n2 + 1, n3; do f = n2 + 1, n3; do m = n0 + 1, n2
                    sum = sum &
                          + (x14(m, f, e, c)*t4e(f, e, b, a, m, k, j, i) & !cbakji(+0.500)
                             - x14(m, f, e, b)*t4e(f, e, c, a, m, k, j, i) & !bcakji(-0.500)
                             + x14(m, f, e, a)*t4e(f, e, c, b, m, k, j, i))/2.0d0!acbkji(+0.500)
                end do; end do; end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4e)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x14)
    end if

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(4, shape(s14), size(s14), '3421', s14, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s48(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i2 = k2*k2
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, d2, s48)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '3412', -0.500, &
                    x1, s48)
    deallocate (s48)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s14), size(s14), '2341', s14, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s47(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s47)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t2c,s47) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      + s47(b, k, e, c)*t2c(e, a, j, i) & !bkcaji    (+1.000)
                      - s47(c, k, e, b)*t2c(e, a, j, i) & !ckbaji    (-1.000)
                      + s47(c, k, e, a)*t2c(e, b, j, i) & !ckabji    (+1.000)
                      - s47(b, j, e, c)*t2c(e, a, k, i) & !bjcaki    (-1.000)
                      + s47(c, j, e, b)*t2c(e, a, k, i) & !cjbaki    (+1.000)
                      - s47(c, j, e, a)*t2c(e, b, k, i) & !cjabki    (-1.000)
                      + s47(b, i, e, c)*t2c(e, a, k, j) & !bicakj    (+1.000)
                      - s47(c, i, e, b)*t2c(e, a, k, j) & !cibakj    (-1.000)
                      + s47(c, i, e, a)*t2c(e, b, k, j)     !ciabkj    (+1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (s47)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s14), size(s14), '2431', s14, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s46(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s46)
    deallocate (d1)
    deallocate (d2)

    allocate (x17(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x17 = 0.0d0
    call sum_stripe(4, shape(x17), size(x17), '3412', 1.000, &
                    x17, s46)
    deallocate (s46)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s14), size(s14), '2431', s14, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q18(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q18)
    deallocate (d1)
    deallocate (b2)

    x5 = x5 + q18
    deallocate (q18)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s14), size(s14), '2341', s14, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s40(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s40)
    deallocate (d1)
    deallocate (b2)
    deallocate (s14)

    allocate (x8(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    x8 = 0.0d0
    call sum_stripe(4, shape(x8), size(x8), '3124', 1.000, x8, &
                    s40)
    deallocate (s40)

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

    call sumx12(0, n3, n0, n2, n2, n3, x12, fockb, 1.000)

    if (lvl_q) then
        allocate (t4e(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                      n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
        rewind (te)
        read (te) t4e

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2                     !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x12,t4e) &
        !$omp private(a,b,c,e,m,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n2 + 1, n3; do m = n0 + 1, n2
                    sum = sum &
                          + x12(m, e)*t4e(e, c, b, a, m, k, j, i)   !cbakji  (+1.000)
                end do; end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (t4e)                                              !ilias: if no quadruples comment out the following 2 lines
        deallocate (x12)
    end if

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(q10), size(q10), '21', q10, b1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s49(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, b1, d2, s49)
    deallocate (b1)
    deallocate (d2)
    deallocate (q10)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s49)
    deallocate (s49)

    if (t2diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
        allocate (s15(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
        i1 = k2*k2
        i2 = k2*k4
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s15)
        deallocate (d1)
        deallocate (d2)

        factor = -t2diag4
        call sum_stripe(4, shape(x15), size(x15), '2314', factor, &
                        x15, s15)
        deallocate (s15)

    end if
    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x15,t2c) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x15(m, c, j, i)*t2c(b, a, m, k) & !cjibak    (+1.000)
                      - x15(m, b, j, i)*t2c(c, a, m, k) & !bjicak    (-1.000)
                      + x15(m, a, j, i)*t2c(c, b, m, k) & !ajicbk    (+1.000)
                      - x15(m, c, k, i)*t2c(b, a, m, j) & !ckibaj    (-1.000)
                      + x15(m, b, k, i)*t2c(c, a, m, j) & !bkicaj    (+1.000)
                      - x15(m, a, k, i)*t2c(c, b, m, j) & !akicbj    (-1.000)
                      - x15(m, c, i, j)*t2c(b, a, m, k) & !cijbak    (-1.000)
                      + x15(m, b, i, j)*t2c(c, a, m, k) & !bijcak    (+1.000)
                      - x15(m, a, i, j)*t2c(c, b, m, k) & !aijcbk    (-1.000)
                      + x15(m, c, i, k)*t2c(b, a, m, j) & !cikbaj    (+1.000)
                      - x15(m, b, i, k)*t2c(c, a, m, j) & !bikcaj    (-1.000)
                      + x15(m, a, i, k)*t2c(c, b, m, j) & !aikcbj    (+1.000)
                      + x15(m, c, k, j)*t2c(b, a, m, i) & !ckjbai    (+1.000)
                      - x15(m, b, k, j)*t2c(c, a, m, i) & !bkjcai    (-1.000)
                      + x15(m, a, k, j)*t2c(c, b, m, i) & !akjcbi    (+1.000)
                      - x15(m, c, j, k)*t2c(b, a, m, i) & !cjkbai    (-1.000)
                      + x15(m, b, j, k)*t2c(c, a, m, i) & !bjkcai    (+1.000)
                      - x15(m, a, j, k)*t2c(c, b, m, i)     !ajkcbi    (-1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x15)

    if (t2diag3 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n2 - n0, n2 - n0/), '1342', intm, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
        allocate (s16(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        i1 = k4*k4
        i2 = k2*k4
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s16)
        deallocate (d1)
        deallocate (d2)

        factor = t2diag3
        call sum_stripe(4, shape(x16), size(x16), '3412', &
                        t2diag3, x16, s16)
        deallocate (s16)

    end if
    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x16,t2c) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      - x16(e, c, b, i)*t2c(e, a, k, j) & !cbiakj    (-1.000)
                      + x16(e, c, a, i)*t2c(e, b, k, j) & !caibkj    (+1.000)
                      + x16(e, b, c, i)*t2c(e, a, k, j) & !bciakj    (+1.000)
                      - x16(e, a, c, i)*t2c(e, b, k, j) & !acibkj    (-1.000)
                      - x16(e, b, a, i)*t2c(e, c, k, j) & !baickj    (-1.000)
                      + x16(e, a, b, i)*t2c(e, c, k, j) & !abickj    (+1.000)
                      + x16(e, c, b, j)*t2c(e, a, k, i) & !cbjaki    (+1.000)
                      - x16(e, c, a, j)*t2c(e, b, k, i) & !cajbki    (-1.000)
                      - x16(e, b, c, j)*t2c(e, a, k, i) & !bcjaki    (-1.000)
                      + x16(e, a, c, j)*t2c(e, b, k, i) & !acjbki    (+1.000)
                      + x16(e, b, a, j)*t2c(e, c, k, i) & !bajcki    (+1.000)
                      - x16(e, a, b, j)*t2c(e, c, k, i) & !abjcki    (-1.000)
                      - x16(e, c, b, k)*t2c(e, a, j, i) & !cbkaji    (-1.000)
                      + x16(e, c, a, k)*t2c(e, b, j, i) & !cakbji    (+1.000)
                      + x16(e, b, c, k)*t2c(e, a, j, i) & !bckaji    (+1.000)
                      - x16(e, a, c, k)*t2c(e, b, j, i) & !ackbji    (-1.000)
                      - x16(e, b, a, k)*t2c(e, c, j, i) & !bakcji    (-1.000)
                      + x16(e, a, b, k)*t2c(e, c, j, i)     !abkcji    (+1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x16)

    if (t3diag1 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        if (t3diag2 .eq. 0) then
            call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                               (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intm, d1)
        else
            call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                               (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
        end if
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
        allocate (s17(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k2*k4
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s17)
        deallocate (d1)
        deallocate (d2)

        factor = t3diag1
        call sum_stripe(4, shape(x3), size(x3), '3412', factor, &
                        x3, s17)
        deallocate (s17)

        allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
        allocate (s18(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k2*k4
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s18)
        deallocate (d1)
        deallocate (d2)

        call sum_stripe(4, shape(x7), size(x7), '3412', factor, &
                        x7, s18)
        deallocate (s18)

    end if
    if (t3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
        allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
        allocate (q11(n0 + 1:n2, n0 + 1:n2))
        i1 = k2
        i2 = k2
        i3 = k3*k4*k1
        call egemm(i1, i2, i3, d1, d2, q11)
        deallocate (d1)
        deallocate (d2)

        factor = t3diag4
        call sum_stripe(2, shape(x4), size(x4), '21', factor, x4, &
                        q11)
        deallocate (q11)

    end if
    if (t3diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1)
        allocate (d2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
        call reorder_stripe(4, shape(t2b), size(t2b), '3421', t2b, d2)
        allocate (q12(n2 + 1:n3, n2 + 1:n3))
        i1 = k4
        i2 = k4
        i3 = k3*k1*k2
        call egemm(i1, i2, i3, d1, d2, q12)
        deallocate (d1)
        deallocate (d2)

        factor = -t3diag3
        call sum_stripe(2, shape(x5), size(x5), '21', factor, x5, &
                        q12)
        deallocate (q12)

    end if
    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s19(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, b1, d2, s19)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s19)
    deallocate (s19)

    if (t2diag5 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intb, d1)
        allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
        allocate (s20(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        i1 = k2*k4
        i2 = k4*k4
        i3 = k2*k2
        call egemm(i1, i2, i3, d1, d2, s20)
        deallocate (d1)
        deallocate (d2)

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t2c,s20,t2diag5) &
        !$omp private(a,b,c,e,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n2 + 1, n3
                    sum = sum & !top 2 swtiched
                          + (s20(c, a, e, i)*t2c(e, b, k, j) & !caibkj (+0.500)
                             - s20(b, a, e, i)*t2c(e, c, k, j) & !baickj (-0.500)
                             - s20(c, b, e, i)*t2c(e, a, k, j) & !cbiakj (-0.500)
                             + s20(b, a, e, j)*t2c(e, c, k, i) & !bajcki (+0.500)
                             - s20(c, a, e, j)*t2c(e, b, k, i) & !cajbki (-0.500)
                             + s20(c, b, e, j)*t2c(e, a, k, i) & !cbjaki (+0.500)
                             - s20(b, a, e, k)*t2c(e, c, j, i) & !bakcji (-0.500)
                             + s20(c, a, e, k)*t2c(e, b, j, i) & !cakbji (+0.500)
                             - s20(c, b, e, k)*t2c(e, a, j, i))/2.0d0!cbkaji (-0.500)
                end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + t2diag5*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s20)

    end if
    if (t2diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1423', intb, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
        allocate (s21(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
        i1 = k2*k2
        i2 = k2*k4
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s21)
        deallocate (d1)
        deallocate (d2)

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,s21,t2c,t2diag4) &
        !$omp private(a,b,c,n,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do n = n0 + 1, n2
                    sum = sum &
                          - s21(a, k, n, i)*t2c(c, b, n, j) & !akicbj (-1.000)
                          + s21(b, k, n, i)*t2c(c, a, n, j) & !bkicaj (+1.000)
                          + s21(c, j, n, i)*t2c(b, a, n, k) & !cjibak (+1.000)
                          - s21(c, k, n, i)*t2c(b, a, n, j) & !ckibaj (-1.000)
                          - s21(b, j, n, i)*t2c(c, a, n, k) & !bjicak (-1.000)
                          + s21(a, j, n, i)*t2c(c, b, n, k) & !ajicbk (+1.000)
                          + s21(a, k, n, j)*t2c(c, b, n, i) & !akjcbi (+1.000)
                          - s21(b, k, n, j)*t2c(c, a, n, i) & !bkjcai (-1.000)
                          - s21(c, i, n, j)*t2c(b, a, n, k) & !cijbak (-1.000)
                          + s21(c, k, n, j)*t2c(b, a, n, i) & !ckjbai (+1.000)
                          + s21(b, i, n, j)*t2c(c, a, n, k) & !bijcak (+1.000)
                          - s21(a, i, n, j)*t2c(c, b, n, k) & !aijcbk (-1.000)
                          - s21(a, j, n, k)*t2c(c, b, n, i) & !ajkcbi (-1.000)
                          + s21(b, j, n, k)*t2c(c, a, n, i) & !bjkcai (+1.000)
                          + s21(c, i, n, k)*t2c(b, a, n, j) & !cikbaj (+1.000)
                          - s21(c, j, n, k)*t2c(b, a, n, i) & !cjkbai (-1.000)
                          - s21(b, i, n, k)*t2c(c, a, n, j) & !bikcaj (-1.000)
                          + s21(a, i, n, k)*t2c(c, b, n, j)     !aikcbj (+1.000)
                end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + t2diag4*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s21)

    end if
    if (t2diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 13 lines for 3cc
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2341', intb, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
        allocate (s22(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        i1 = k4*k4
        i2 = k2*k4
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s22)
        deallocate (d1)
        deallocate (d2)

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2                      !ilias: commented out 29 lines for 3cc
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,s22,t2c,t2diag3) &
        !$omp private(a,b,c,f,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do f = n2 + 1, n3
                    sum = sum &
                          - s22(a, k, f, c)*t2c(f, b, j, i) & !akcbji (-1.000)
                          + s22(b, k, f, c)*t2c(f, a, j, i) & !bkcaji (+1.000)
                          + s22(a, k, f, b)*t2c(f, c, j, i) & !akbcji (+1.000)
                          - s22(b, k, f, a)*t2c(f, c, j, i) & !bkacji (-1.000)
                          - s22(c, k, f, b)*t2c(f, a, j, i) & !ckbaji (-1.000)
                          + s22(c, k, f, a)*t2c(f, b, j, i) & !ckabji (+1.000)
                          + s22(a, j, f, c)*t2c(f, b, k, i) & !ajcbki (+1.000)
                          - s22(b, j, f, c)*t2c(f, a, k, i) & !bjcaki (-1.000)
                          - s22(a, j, f, b)*t2c(f, c, k, i) & !ajbcki (-1.000)
                          + s22(b, j, f, a)*t2c(f, c, k, i) & !bjacki (+1.000)
                          + s22(c, j, f, b)*t2c(f, a, k, i) & !cjbaki (+1.000)
                          - s22(c, j, f, a)*t2c(f, b, k, i) & !cjabki (-1.000)
                          + s22(b, i, f, c)*t2c(f, a, k, j) & !bicakj (+1.000)
                          - s22(a, i, f, c)*t2c(f, b, k, j) & !aicbkj (-1.000)
                          - s22(c, i, f, b)*t2c(f, a, k, j) & !cibakj (-1.000)
                          + s22(c, i, f, a)*t2c(f, b, k, j) & !ciabkj (+1.000)
                          + s22(a, i, f, b)*t2c(f, c, k, j) & !aibckj (+1.000)
                          - s22(b, i, f, a)*t2c(f, c, k, j)     !biackj (-1.000)
                end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + t2diag3*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s22)                                               !ilias: commented out this line for 3cc

    end if
    if (t2diag5 .ne. 0) then
        allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n2 - n0, n2 - n0, n0 - n0, n2 - n0/), '4321', intb, d1)
        allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
        allocate (s23(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k2*k2
        i3 = k4*k4
        call egemm(i1, i2, i3, d1, d2, s23)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*t2diag5
        call sum_stripe(4, shape(x1), size(x1), '3412', factor, &
                        x1, s23)
        deallocate (s23)

    end if
    call sumx2143(n0, n3, n0, n2, n2, n3, n0, n2, n0, n2, x1, intb, 1.000)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x1,t2c,t2diag3) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x1(m, c, j, i)*t2c(b, a, m, k) & !cjibak (+1.000)
                      - x1(m, b, j, i)*t2c(c, a, m, k) & !bjicak (-1.000)
                      + x1(m, a, j, i)*t2c(c, b, m, k) & !ajicbk (+1.000)
                      - x1(m, c, k, i)*t2c(b, a, m, j) & !ckibaj (-1.000)
                      + x1(m, b, k, i)*t2c(c, a, m, j) & !bkicaj (+1.000)
                      - x1(m, a, k, i)*t2c(c, b, m, j) & !akicbj (-1.000)
                      + x1(m, c, k, j)*t2c(b, a, m, i) & !ckjbai (+1.000)
                      - x1(m, b, k, j)*t2c(c, a, m, i) & !bkjcai (-1.000)
                      + x1(m, a, k, j)*t2c(c, b, m, i)     !akjcbi (+1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x1)

    allocate (x999(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x999 = 0.0d0

    if (t3diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))                 !ilias: commented out 13 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1)
        allocate (f2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(6, shape(t3c), size(t3c), '463125', t3c, f2)
        allocate (s24(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
        i1 = k4
        i2 = k2*k4*k4
        i3 = k3*k1*k2
        call egemm(i1, i2, i3, d1, f2, s24)
        deallocate (d1)
        deallocate (f2)

        factor = t3diag3
        call sum_stripe(4, shape(x999), size(x999), '2341', &
                        factor, x999, s24)         !ilias: commented out 2 lines for 3cc
        deallocate (s24)

    end if
    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x999,t2c,t2diag3) &
        !$omp private(a,b,c,f,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum & !(bc)
                      + x999(f, b, a, k)*t2c(f, c, j, i) & !bakcji    (+1.000)
                      - x999(f, c, a, k)*t2c(f, b, j, i) & !cakbji    (-1.000)
                      + x999(f, c, b, k)*t2c(f, a, j, i) & !cbkaji    (+1.000)
                      - x999(f, b, a, j)*t2c(f, c, k, i) & !bajcki    (-1.000)
                      + x999(f, c, a, j)*t2c(f, b, k, i) & !cajbki    (+1.000)
                      - x999(f, c, b, j)*t2c(f, a, k, i) & !cbjaki    (-1.000)
                      + x999(f, b, a, i)*t2c(f, c, k, j) & !baickj    (+1.000)
                      - x999(f, c, a, i)*t2c(f, b, k, j) & !caibkj    (-1.000)
                      + x999(f, c, b, i)*t2c(f, a, k, j)     !cbiakj    (+1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x999)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x17,t2c,t2diag3) &
        !$omp private(a,b,c,f,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      + x17(f, c, a, k)*t2c(f, b, j, i) & !cakbji (+1.000)
                      - x17(f, b, a, k)*t2c(f, c, j, i) & !bakcji (-1.000)
                      + x17(f, a, b, k)*t2c(f, c, j, i) & !abkcji (+1.000)
                      - x17(f, c, a, j)*t2c(f, b, k, i) & !cajbki (-1.000)
                      + x17(f, b, a, j)*t2c(f, c, k, i) & !bajcki (+1.000)
                      - x17(f, a, b, j)*t2c(f, c, k, i) & !abjcki (-1.000)
                      + x17(f, c, a, i)*t2c(f, b, k, j) & !caibkj (+1.000)
                      - x17(f, b, a, i)*t2c(f, c, k, j) & !baickj (-1.000)
                      + x17(f, a, b, i)*t2c(f, c, k, j)     !abickj (+1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x17)

    if (t3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
        allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
        call reorder_stripe(6, shape(t3c), size(t3c), '613245', t3c, f2)
        allocate (s25(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
        i1 = k2
        i2 = k2*k2*k4
        i3 = k3*k4*k1
        call egemm(i1, i2, i3, d1, f2, s25)
        deallocate (d1)
        deallocate (f2)

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,s25,t2c,t3diag4) &
        !$omp private(a,b,c,n,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do n = n0 + 1, n2
                    sum = sum &
                          + s25(a, k, j, n)*t2c(c, b, n, i) & !akjcbi (+1.000)
                          - s25(b, k, j, n)*t2c(c, a, n, i) & !bkjcai (-1.000)
                          + s25(c, k, j, n)*t2c(b, a, n, i) & !ckjbai (+1.000)
                          - s25(a, k, i, n)*t2c(c, b, n, j) & !akicbj (-1.000)
                          + s25(b, k, i, n)*t2c(c, a, n, j) & !bkicaj (+1.000)
                          - s25(c, k, i, n)*t2c(b, a, n, j) & !ckibaj (-1.000)
                          + s25(a, j, i, n)*t2c(c, b, n, k) & !ajicbk (+1.000)
                          - s25(b, j, i, n)*t2c(c, a, n, k) & !bjicak (-1.000)
                          + s25(c, j, i, n)*t2c(b, a, n, k)     !cjibak (+1.000)
                end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + t3diag4*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s25)

    end if
    if (t3diag1 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
        allocate (s26(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k2*k4
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s26)
        deallocate (d1)
        deallocate (d2)

        factor = t3diag1
        call sum_stripe(4, shape(x3), size(x3), '3412', factor, &
                        x3, s26)
        deallocate (s26)

    end if
    call sumx1324(n0, n3, n0, n1, n1, n3, n2, n3, n0, n2, x3, intm, 1.000)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t3c,x3,t3diag4) &
        !$omp private(a,b,c,e,m,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x3(m, e, c, i)*t3c(b, a, e, k, j, m) & !cibakj  (+1.000)
                      - x3(m, e, b, i)*t3c(c, a, e, k, j, m) & !bicakj  (-1.000)
                      + x3(m, e, a, i)*t3c(c, b, e, k, j, m) & !aicbkj  (+1.000)
                      - x3(m, e, c, j)*t3c(b, a, e, k, i, m) & !cjbaki  (-1.000)
                      + x3(m, e, b, j)*t3c(c, a, e, k, i, m) & !bjcaki  (+1.000)
                      - x3(m, e, a, j)*t3c(c, b, e, k, i, m) & !ajcbki  (-1.000)
                      + x3(m, e, c, k)*t3c(b, a, e, j, i, m) & !ckbaji  (+1.000)
                      - x3(m, e, b, k)*t3c(c, a, e, j, i, m) & !bkcaji  (-1.000)
                      + x3(m, e, a, k)*t3c(c, b, e, j, i, m)   !akcbji  (+1.000)
            end do; end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x3)

    if (t3diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 13 lines for 3cc
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
        allocate (f2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(6, shape(t3d), size(t3d), '451236', t3d, f2)
        allocate (s27(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
        i1 = k4
        i2 = k2*k4*k4
        i3 = k4*k2*k2
        call egemm(i1, i2, i3, d1, f2, s27)
        deallocate (d1)
        deallocate (f2)

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2                      !ilias: commented out 20 lines for 3cc
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t2c,s27,t3diag3) &
        !$omp private(a,b,c,e,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do e = n2 + 1, n3
                    sum = sum &
                          + (s27(b, a, k, e)*t2c(e, c, j, i) & !bakcji (+0.500)
                             - s27(c, a, k, e)*t2c(e, b, j, i) & !cakbji (-0.500)
                             + s27(c, b, k, e)*t2c(e, a, j, i) & !cbkaji (+0.500)
                             - s27(b, a, j, e)*t2c(e, c, k, i) & !bajcki (-0.500)
                             + s27(c, a, j, e)*t2c(e, b, k, i) & !cajbki (+0.500)
                             - s27(c, b, j, e)*t2c(e, a, k, i) & !cbjaki (-0.500)
                             + s27(b, a, i, e)*t2c(e, c, k, j) & !baickj (+0.500)
                             - s27(c, a, i, e)*t2c(e, b, k, j) & !caibkj (-0.500)
                             + s27(c, b, i, e)*t2c(e, a, k, j))/2.0d0!cbiakj (+0.500)
                end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + t3diag3*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s27)                                               !ilias: commented out this line for 3cc

    end if
    if (t3diag5 .ne. 0) then
        allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
        allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
        allocate (s28(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
        i1 = k2*k2
        i2 = k2*k2
        i3 = k4*k4
        call egemm(i1, i2, i3, d1, d2, s28)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*t3diag5
        call sum_stripe(4, shape(x6), size(x6), '3412', factor, &
                        x6, s28)
        deallocate (s28)

    end if
    call sumx2143(n0, n3, n0, n2, n0, n2, n0, n2, n0, n2, x6, intb, 1.000)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x6,t3d,t3diag3) &
        !$omp private(a,b,c,n,m,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (x6(n, m, j, i)*t3d(c, b, a, n, m, k) & !jicbak (+0.500)
                         - x6(n, m, k, i)*t3d(c, b, a, n, m, j) & !kicbaj (-0.500)
                         + x6(n, m, k, j)*t3d(c, b, a, n, m, i))/2.0d0!kjcbai (+0.500)
            end do; end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x6)

    if (t3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1)
        allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
        call reorder_stripe(6, shape(t3d), size(t3d), '412356', t3d, f2)
        allocate (s29(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
        i1 = k2
        i2 = k2*k2*k4
        i3 = k4*k4*k2
        call egemm(i1, i2, i3, d1, f2, s29)
        deallocate (d1)
        deallocate (f2)

        do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
            if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,s29,t2c,t3diag4) &
        !$omp private(a,b,c,m,sum)
        !$omp do
            do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
                if (indunocc(c, b, a) .eq. 1) cycle
                sum = 0.0d0
                do m = n0 + 1, n2
                    sum = sum &
                          + (s29(a, k, j, m)*t2c(c, b, m, i) & !akjcbi (+0.500)
                             - s29(b, k, j, m)*t2c(c, a, m, i) & !bkjcai (-0.500)
                             + s29(c, k, j, m)*t2c(b, a, m, i) & !ckjbai (+0.500)
                             - s29(a, k, i, m)*t2c(c, b, m, j) & !akicbj (-0.500)
                             + s29(b, k, i, m)*t2c(c, a, m, j) & !bkicaj (+0.500)
                             - s29(c, k, i, m)*t2c(b, a, m, j) & !ckibaj (-0.500)
                             + s29(a, j, i, m)*t2c(c, b, m, k) & !ajicbk (+0.500)
                             - s29(b, j, i, m)*t2c(c, a, m, k) & !bjicak (-0.500)
                             + s29(c, j, i, m)*t2c(b, a, m, k))/2.0d0!cjibak (+0.500)
                end do
                v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + t3diag4*sum
            end do; end do; end do
        !$omp end do
        !$omp end parallel
        end do; end do; end do

        deallocate (s29)

    end if
    if (t3diag1 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        if (t3diag2 .eq. 0) then
            call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                               (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
        else
            call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                               (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
        end if
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
        allocate (s30(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k2*k4
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s30)
        deallocate (d1)
        deallocate (d2)

        factor = t3diag1
        call sum_stripe(4, shape(x7), size(x7), '3412', factor, &
                        x7, s30)
        deallocate (s30)

    end if
    call sumx3142(n0, n3, n0, n2, n2, n3, n2, n3, n0, n2, x7, intb, 1.000)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x7,t3d,t3diag4) &
        !$omp private(a,b,c,e,m,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x7(m, e, c, i)*t3d(e, b, a, m, k, j) & !cibakj  (+1.000)
                      - x7(m, e, b, i)*t3d(e, c, a, m, k, j) & !bicakj  (-1.000)
                      + x7(m, e, a, i)*t3d(e, c, b, m, k, j) & !aicbkj  (+1.000)
                      - x7(m, e, c, j)*t3d(e, b, a, m, k, i) & !cjbaki  (-1.000)
                      + x7(m, e, b, j)*t3d(e, c, a, m, k, i) & !bjcaki  (+1.000)
                      - x7(m, e, a, j)*t3d(e, c, b, m, k, i) & !ajcbki  (-1.000)
                      + x7(m, e, c, k)*t3d(e, b, a, m, j, i) & !ckbaji  (+1.000)
                      - x7(m, e, b, k)*t3d(e, c, a, m, j, i) & !bkcaji  (-1.000)
                      + x7(m, e, a, k)*t3d(e, c, b, m, j, i)   !akcbji  (+1.000)
            end do; end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x7)

    if (t3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '1432', intb, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
        allocate (q13(n0 + 1:n2, n0 + 1:n2))
        i1 = k2
        i2 = k2
        i3 = k4*k4*k2
        call egemm(i1, i2, i3, d1, d2, q13)
        deallocate (d1)
        deallocate (d2)

        factor = -0.500*t3diag4
        call sum_stripe(2, shape(x4), size(x4), '21', factor, x4, &
                        q13)
        deallocate (q13)

    end if
    call sumx12(0, n3, n0, n2, n0, n2, x4, fockb, 1.000)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x4,t3d,t3diag4) &
        !$omp private(a,b,c,m,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - x4(m, i)*t3d(c, b, a, m, k, j) & !icbakj (-1.000)
                      + x4(m, j)*t3d(c, b, a, m, k, i) & !jcbaki (+1.000)
                      - x4(m, k)*t3d(c, b, a, m, j, i)     !kcbaji (-1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x4)

    if (t3diag5 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
        allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
        allocate (s31(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
        i1 = k4*k4
        i2 = k4*k4
        i3 = k2*k2
        call egemm(i1, i2, i3, d1, d2, s31)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*t3diag5
        call sum_stripe(4, shape(x8), size(x8), '3412', factor, &
                        x8, s31)
        deallocate (s31)

    end if
    call sumx4321(n0, n3, n2, n3, n2, n3, n2, n3, n2, n3, x8, intb, 1.000)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,t3d,x8,t3diag4) &
        !$omp private(a,b,c,e,f,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do f = n2 + 1, n3
                sum = sum &
                      + (x8(f, e, c, b)*t3d(f, e, a, k, j, i) & !cbakji (+0.500)
                         - x8(f, e, c, a)*t3d(f, e, b, k, j, i) & !cabkji (-0.500)
                         + x8(f, e, b, a)*t3d(f, e, c, k, j, i))/2.0d0!backji (+0.500)
            end do; end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x8)

    if (t3diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2134', intb, d1)
        allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
        allocate (q14(n2 + 1:n3, n2 + 1:n3))
        i1 = k4
        i2 = k4
        i3 = k4*k2*k2
        call egemm(i1, i2, i3, d1, d2, q14)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*t3diag3
        call sum_stripe(2, shape(x5), size(x5), '21', factor, x5, &
                        q14)
        deallocate (q14)

    end if
    call sumx21(0, n3, n2, n3, n2, n3, x5, fockb, 1.000)

    do i = n0 + 1, n2 - 2; do j = i + 1, n2 - 1; do k = j + 1, n2
        if (indocc(k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3d,x5,t3d,t3diag4) &
        !$omp private(a,b,c,e,sum)
        !$omp do
        do a = n2 + 1, n3 - 2; do b = a + 1, n3 - 1; do c = b + 1, n3
            if (indunocc(c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      + x5(e, c)*t3d(e, b, a, k, j, i) & !cbakji (+1.000)
                      - x5(e, b)*t3d(e, c, a, k, j, i) & !bcakji (-1.000)
                      + x5(e, a)*t3d(e, c, b, k, j, i)     !acbkji (+1.000)
            end do
            v3d(c, b, a, k, j, i) = v3d(c, b, a, k, j, i) + sum
        end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do

    deallocate (x5)

    do i = n0 + 1, n2 - 2
    do j = i + 1, n2 - 1
    do k = j + 1, n2
    do a = n2 + 1, n3 - 2
    do b = a + 1, n3 - 1
    do c = b + 1, n3
        coeleft = fockb(c, c) &
                  + fockb(b, b) &
                  + fockb(a, a) &
                  - fockb(k, k) &
                  - fockb(j, j) &
                  - fockb(i, i) &
                  + shift
        t3d(c, b, a, k, j, i) = t3d(c, b, a, k, j, i) - v3d(c, b, a, k, j, i)/coeleft
        t3d(c, b, a, k, i, j) = -t3d(c, b, a, k, j, i)
        t3d(c, b, a, i, j, k) = -t3d(c, b, a, k, j, i)
        t3d(c, b, a, i, k, j) = t3d(c, b, a, k, j, i)
        t3d(c, b, a, j, k, i) = -t3d(c, b, a, k, j, i)
        t3d(c, b, a, j, i, k) = t3d(c, b, a, k, j, i)
        t3d(c, a, b, k, j, i) = -t3d(c, b, a, k, j, i)
        t3d(c, a, b, k, i, j) = t3d(c, b, a, k, j, i)
        t3d(c, a, b, i, j, k) = t3d(c, b, a, k, j, i)
        t3d(c, a, b, i, k, j) = -t3d(c, b, a, k, j, i)
        t3d(c, a, b, j, k, i) = t3d(c, b, a, k, j, i)
        t3d(c, a, b, j, i, k) = -t3d(c, b, a, k, j, i)
        t3d(a, b, c, k, j, i) = -t3d(c, b, a, k, j, i)
        t3d(a, b, c, k, i, j) = t3d(c, b, a, k, j, i)
        t3d(a, b, c, i, j, k) = t3d(c, b, a, k, j, i)
        t3d(a, b, c, i, k, j) = -t3d(c, b, a, k, j, i)
        t3d(a, b, c, j, k, i) = t3d(c, b, a, k, j, i)
        t3d(a, b, c, j, i, k) = -t3d(c, b, a, k, j, i)
        t3d(a, c, b, k, j, i) = t3d(c, b, a, k, j, i)
        t3d(a, c, b, k, i, j) = -t3d(c, b, a, k, j, i)
        t3d(a, c, b, i, j, k) = -t3d(c, b, a, k, j, i)
        t3d(a, c, b, i, k, j) = t3d(c, b, a, k, j, i)
        t3d(a, c, b, j, k, i) = -t3d(c, b, a, k, j, i)
        t3d(a, c, b, j, i, k) = t3d(c, b, a, k, j, i)
        t3d(b, c, a, k, j, i) = -t3d(c, b, a, k, j, i)
        t3d(b, c, a, k, i, j) = t3d(c, b, a, k, j, i)
        t3d(b, c, a, i, j, k) = t3d(c, b, a, k, j, i)
        t3d(b, c, a, i, k, j) = -t3d(c, b, a, k, j, i)
        t3d(b, c, a, j, k, i) = t3d(c, b, a, k, j, i)
        t3d(b, c, a, j, i, k) = -t3d(c, b, a, k, j, i)
        t3d(b, a, c, k, j, i) = t3d(c, b, a, k, j, i)
        t3d(b, a, c, k, i, j) = -t3d(c, b, a, k, j, i)
        t3d(b, a, c, i, j, k) = -t3d(c, b, a, k, j, i)
        t3d(b, a, c, i, k, j) = t3d(c, b, a, k, j, i)
        t3d(b, a, c, j, k, i) = -t3d(c, b, a, k, j, i)
        t3d(b, a, c, j, i, k) = t3d(c, b, a, k, j, i)
    end do
    end do
    end do
    end do
    end do
    end do

end subroutine t3d_update



