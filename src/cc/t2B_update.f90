subroutine t2b_update(n0, n1, n2, n3, k1, k2, k3, k4, lvl_t, lvl_q, shift &
                      , v2b, ext_cor, fockr, fockb, intr, intb, intm, &
                      diag1, diag2, diag3, diag4, diag5, &
                      t1diag1, t1diag2, t1diag3, t1diag4, &
                      dt3diag3, dt3diag4, &
                      t1a, t1b, t2a, t2b, t2c, t3a, t3b, t3c, t3d, &
                      t2a_mc, t2b_mc, t2c_mc)

    integer :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p
    logical :: lvl_t, lvl_q
    logical :: ext_cor
    real :: diag1, diag2, diag3, diag4, diag5, factor
    real :: t1diag1, t1diag2, t1diag3, t1diag4
    real :: dt3diag3, dt3diag4
    real(kind=8) :: shift, pp, coeleft
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
    real(kind=8) :: t2a_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1)
    real(kind=8) :: t2b_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1)
    real(kind=8) :: t2c_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2)
    real(kind=8) :: v2b(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1)

    real(kind=8), allocatable::t4b(:, :, :, :, :, :, :, :)                     !ilias: if no quadruples comment out the following 3 lines
    real(kind=8), allocatable::t4c(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::t4d(:, :, :, :, :, :, :, :)

    integer :: ta, tb, tc, td, te                                       !ilias: if no quadruples comment out the following 2 lines
    parameter(ta=29, tb=30, tc=31, td=32, te=33)

    integer :: i1, i2
    integer, allocatable::ind1(:, :, :, :)
    integer, allocatable::ind2(:, :, :, :)

    real(kind=8), allocatable::b1(:, :)
    real(kind=8), allocatable::b1_mc(:, :)
    real(kind=8), allocatable::b2(:, :)
    real(kind=8), allocatable::d1(:, :, :, :)
    real(kind=8), allocatable::d1_mc(:, :, :, :)
    real(kind=8), allocatable::d2(:, :, :, :)
    real(kind=8), allocatable::d2_mc(:, :, :, :)
    real(kind=8), allocatable::f2(:, :, :, :, :, :)
    real(kind=8), allocatable::h2(:, :, :, :, :, :, :, :)

    real(kind=8), allocatable::s30(:, :, :, :)
    real(kind=8), allocatable::s32(:, :, :, :)
    real(kind=8), allocatable::s34(:, :, :, :)
    real(kind=8), allocatable::s36(:, :, :, :)
    real(kind=8), allocatable::s38(:, :, :, :)
    real(kind=8), allocatable::q1(:, :)
    real(kind=8), allocatable::q2(:, :)
    real(kind=8), allocatable::s42(:, :, :, :)
    real(kind=8), allocatable::q3(:, :)
    real(kind=8), allocatable::s45(:, :, :, :)
    real(kind=8), allocatable::q4(:, :)
    real(kind=8), allocatable::s48(:, :, :, :)
    real(kind=8), allocatable::s126(:, :, :, :)
    real(kind=8), allocatable::s50(:, :, :, :)
    real(kind=8), allocatable::s128(:, :, :, :)
    real(kind=8), allocatable::s52(:, :, :, :)
    real(kind=8), allocatable::q5(:, :)
    real(kind=8), allocatable::s55(:, :, :, :)
    real(kind=8), allocatable::q6(:, :)
    real(kind=8), allocatable::s58(:, :, :, :)
    real(kind=8), allocatable::s60(:, :, :, :)
    real(kind=8), allocatable::s138(:, :, :, :)
    real(kind=8), allocatable::s62(:, :, :, :)
    real(kind=8), allocatable::q25(:, :)
    real(kind=8), allocatable::s130(:, :, :, :)
    real(kind=8), allocatable::s64(:, :, :, :)
    real(kind=8), allocatable::q7(:, :)
    real(kind=8), allocatable::q26(:, :)
    real(kind=8), allocatable::s67(:, :, :, :)
    real(kind=8), allocatable::q27(:, :)
    real(kind=8), allocatable::s142(:, :, :, :)
    real(kind=8), allocatable::s140(:, :, :, :)
    real(kind=8), allocatable::s158(:, :, :, :)
    real(kind=8), allocatable::s134(:, :, :, :)
    real(kind=8), allocatable::s69(:, :, :, :)
    real(kind=8), allocatable::q8(:, :)
    real(kind=8), allocatable::q29(:, :)
    real(kind=8), allocatable::q28(:, :)
    real(kind=8), allocatable::s72(:, :, :, :)
    real(kind=8), allocatable::s74(:, :, :, :)
    real(kind=8), allocatable::s76(:, :, :, :)
    real(kind=8), allocatable::q9(:, :)
    real(kind=8), allocatable::q10(:, :)
    real(kind=8), allocatable::s80(:, :, :, :)
    real(kind=8), allocatable::s136(:, :, :, :)
    real(kind=8), allocatable::s82(:, :, :, :)
    real(kind=8), allocatable::q11(:, :)
    real(kind=8), allocatable::s85(:, :, :, :)
    real(kind=8), allocatable::s87(:, :, :, :)
    real(kind=8), allocatable::q12(:, :)
    real(kind=8), allocatable::s90(:, :, :, :)
    real(kind=8), allocatable::q13(:, :)
    real(kind=8), allocatable::s93(:, :, :, :)
    real(kind=8), allocatable::q14(:, :)
    real(kind=8), allocatable::s96(:, :, :, :)
    real(kind=8), allocatable::s152(:, :, :, :)
    real(kind=8), allocatable::s145(:, :, :, :)
    real(kind=8), allocatable::s98(:, :, :, :)
    real(kind=8), allocatable::q15(:, :)
    real(kind=8), allocatable::q30(:, :)
    real(kind=8), allocatable::s101(:, :, :, :)
    real(kind=8), allocatable::q31(:, :)
    real(kind=8), allocatable::s154(:, :, :, :)
    real(kind=8), allocatable::s103(:, :, :, :)
    real(kind=8), allocatable::q16(:, :)
    real(kind=8), allocatable::q32(:, :)
    real(kind=8), allocatable::s106(:, :, :, :)
    real(kind=8), allocatable::s106_mc(:, :, :, :)
    real(kind=8), allocatable::q17(:, :)
    real(kind=8), allocatable::q17_mc(:, :)
    real(kind=8), allocatable::q18(:, :)
    real(kind=8), allocatable::q18_mc(:, :)
    real(kind=8), allocatable::s110(:, :, :, :)
    real(kind=8), allocatable::s110_mc(:, :, :, :)
    real(kind=8), allocatable::q19(:, :)
    real(kind=8), allocatable::q19_mc(:, :)
    real(kind=8), allocatable::q20(:, :)
    real(kind=8), allocatable::q20_mc(:, :)
    real(kind=8), allocatable::s114(:, :, :, :)
    real(kind=8), allocatable::s114_mc(:, :, :, :)
    real(kind=8), allocatable::s148(:, :, :, :)
    real(kind=8), allocatable::q21(:, :)
    real(kind=8), allocatable::q21_mc(:, :)
    real(kind=8), allocatable::s117(:, :, :, :)
    real(kind=8), allocatable::s117_mc(:, :, :, :)
    real(kind=8), allocatable::s119(:, :, :, :)
    real(kind=8), allocatable::s119_mc(:, :, :, :)
    real(kind=8), allocatable::q22(:, :)
    real(kind=8), allocatable::q22_mc(:, :)
    real(kind=8), allocatable::q23(:, :)
    real(kind=8), allocatable::q23_mc(:, :)
    real(kind=8), allocatable::q24(:, :)
    real(kind=8), allocatable::q24_mc(:, :)
    real(kind=8), allocatable::s124(:, :, :, :)
    real(kind=8), allocatable::s124_mc(:, :, :, :)
    real(kind=8), allocatable::x1(:, :, :, :)
    real(kind=8), allocatable::z1(:, :, :, :)
    real(kind=8), allocatable::z2(:, :, :, :)
    real(kind=8), allocatable::x2(:, :, :, :)
    real(kind=8), allocatable::z3(:, :, :, :)
    real(kind=8), allocatable::x3(:, :, :, :)
    real(kind=8), allocatable::z4(:, :, :, :)
    real(kind=8), allocatable::x4(:, :, :, :)
    real(kind=8), allocatable::z5(:, :, :, :)
    real(kind=8), allocatable::x5(:, :)
    real(kind=8), allocatable::z6(:, :, :, :)
    real(kind=8), allocatable::z6_mc(:, :, :, :)
    real(kind=8), allocatable::x6(:, :)
    real(kind=8), allocatable::z7(:, :, :, :)
    real(kind=8), allocatable::z7_mc(:, :, :, :)
    real(kind=8), allocatable::x7(:, :)
    real(kind=8), allocatable::z8(:, :, :, :)
    real(kind=8), allocatable::z8_mc(:, :, :, :)
    real(kind=8), allocatable::x8(:, :)
    real(kind=8), allocatable::z9(:, :, :, :)
    real(kind=8), allocatable::z9_mc(:, :, :, :)
    real(kind=8), allocatable::x9(:, :, :, :)
    real(kind=8), allocatable::z10(:, :, :, :)
    real(kind=8), allocatable::z10_mc(:, :, :, :)
    real(kind=8), allocatable::x10(:, :, :, :)
    real(kind=8), allocatable::z11(:, :, :, :)
    real(kind=8), allocatable::z11_mc(:, :, :, :)
    real(kind=8), allocatable::x11(:, :, :, :)
    real(kind=8), allocatable::z12(:, :, :, :)
    real(kind=8), allocatable::x12(:, :, :, :)
    real(kind=8), allocatable::z13(:, :, :, :)
    real(kind=8), allocatable::z13_mc(:, :, :, :)
    real(kind=8), allocatable::z14(:, :, :, :)
    real(kind=8), allocatable::x13(:, :, :, :)
    real(kind=8), allocatable::z15(:, :, :, :)
    real(kind=8), allocatable::x14(:, :, :, :)
    real(kind=8), allocatable::z16(:, :, :, :)
    real(kind=8), allocatable::z16_mc(:, :, :, :)
    real(kind=8), allocatable::x15(:, :)
    real(kind=8), allocatable::z17(:, :, :, :)
    real(kind=8), allocatable::x16(:, :, :, :)
    real(kind=8), allocatable::z18(:, :, :, :)
    real(kind=8), allocatable::z19(:, :, :, :)
    real(kind=8), allocatable::x17(:, :, :, :)
    real(kind=8), allocatable::z20(:, :, :, :)
    real(kind=8), allocatable::z21(:, :, :, :)
    real(kind=8), allocatable::x18(:, :)
    real(kind=8), allocatable::z22(:, :, :, :)
    real(kind=8), allocatable::x19(:, :, :, :)
    real(kind=8), allocatable::z23(:, :, :, :)
    real(kind=8), allocatable::z24(:, :, :, :)
    real(kind=8), allocatable::x20(:, :, :, :)
    real(kind=8), allocatable::z25(:, :, :, :)
    real(kind=8), allocatable::z26(:, :, :, :)
    real(kind=8), allocatable::z27(:, :, :, :)
    real(kind=8), allocatable::z28(:, :, :, :)
    real(kind=8), allocatable::z29(:, :, :, :)

    factor = 0

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3124', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s30(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s30)
    deallocate (d1)
    deallocate (b2)

    allocate (x1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x1 = 0.0d0
    call sum_stripe(4, shape(x1), size(x1), '4123', 1.000, x1, &
                    s30)
    deallocate (s30)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s32(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s32)
    deallocate (d1)
    deallocate (b2)

    allocate (x2(n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x2 = 0.0d0
    call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                    x2, s32)
    deallocate (s32)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4123', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s34(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s34)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                    s34)
    deallocate (s34)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s36(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s36)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                    s36)
    deallocate (s36)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n1 - n0/), '3421', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s38(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s38)
    deallocate (d1)
    deallocate (b2)

    allocate (x3(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x3 = 0.0d0
    call sum_stripe(4, shape(x3), size(x3), '4123', 1.000, x3, &
                    s38)
    deallocate (s38)

  call sum_shift(4,shape(intm),size(intm),shape(x3), &
   size(x3),(/n2-n0,n2-n0,n1-n0,n0-n0/),'3241',1.000,intm,x3)

    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (z4(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, x3, b2, z4)
    deallocate (b2)

    call sum_stripe(4, shape(v2b), size(v2b), '3124', 1.000, &
                    v2b, z4)
    deallocate (z4)
    deallocate (x3)

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

    allocate (x5(n0 + 1:n1, n0 + 1:n1))
    x5 = 0.0d0
    call sum_stripe(2, shape(x5), size(x5), '21', 1.000, x5, &
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

    allocate (x6(n1 + 1:n3, n1 + 1:n3))
    x6 = 0.0d0
    call sum_stripe(2, shape(x6), size(x6), '21', -1.000, x6, &
                    q2)
    deallocate (q2)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2413', intr, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (s42(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s42)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                    s42)
    deallocate (s42)

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

    x5 = x5 - q3
    deallocate (q3)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s45(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s45)
    deallocate (d1)
    deallocate (b2)

    allocate (x9(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x9 = 0.0d0
    call sum_stripe(4, shape(x9), size(x9), '4123', 1.000, x9, &
                    s45)
    deallocate (s45)

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

    x6 = x6 - q4
    deallocate (q4)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s48(n0 + 1:n1, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s48)
    deallocate (d1)
    deallocate (b2)

    allocate (x10(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    x10 = 0.0d0
    call sum_stripe(4, shape(x10), size(x10), '4123', 1.000, &
                    x10, s48)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s48), size(s48), '3241', s48, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s126(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s126)
    deallocate (d1)
    deallocate (b2)
    deallocate (s48)

    call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                    x2, s126)
    deallocate (s126)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s50(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s50)
    deallocate (d1)
    deallocate (b2)

    allocate (x11(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    x11 = 0.0d0
    call sum_stripe(4, shape(x11), size(x11), '4123', 1.000, &
                    x11, s50)

  call sum_shift(4,shape(intm),size(intm),shape(x11), &
   size(x11),(/n0-n0,n2-n0,n2-n0,n0-n0/),'1342',1.000,intm,x11)

    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (z12(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k2*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, x11, d2, z12)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '2314', -1.000, &
                    v2b, z12)
    deallocate (z12)
    deallocate (x11)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s50), size(s50), '3241', s50, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s128(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s128)
    deallocate (d1)
    deallocate (b2)
    deallocate (s50)

    call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                    s128)
    deallocate (s128)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2314', intm, d1)
    allocate (d2(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3214', t2b, d2)
    allocate (s52(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k4
    i3 = k3*k2
    call egemm(i1, i2, i3, d1, d2, s52)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2413', -1.000, &
                    x1, s52)
    deallocate (s52)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q5(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q5)
    deallocate (d1)
    deallocate (b2)

    allocate (x7(n0 + 1:n2, n0 + 1:n2))
    x7 = 0.0d0
    x7 = x7 + q5
    deallocate (q5)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n2 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s55(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3))
    i1 = k4*k1
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s55)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '3412', 1.000, x1, &
                    s55)
    deallocate (s55)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n2 - n0, n2 - n0/), '1342', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q6(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q6)
    deallocate (d1)
    deallocate (b2)

    allocate (x8(n2 + 1:n3, n2 + 1:n3))
    x8 = 0.0d0
    x8 = x8 + q6
    deallocate (q6)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s58(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s58)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                    s58)
    deallocate (s58)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n1 - n0/), '3241', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s60(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s60)
    deallocate (d1)
    deallocate (b2)

    allocate (x14(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x14 = 0.0d0
    call sum_stripe(4, shape(x14), size(x14), '4123', 1.000, &
                    x14, s60)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s60), size(s60), '3241', s60, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s138(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s138)
    deallocate (d1)
    deallocate (b2)
    deallocate (s60)

    call sum_stripe(4, shape(x2), size(x2), '3124', 1.000, x2, &
                    s138)
    deallocate (s138)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3214', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s62(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s62)
    deallocate (d1)
    deallocate (b2)

    if (lvl_t) then
    if (dt3diag3 .ne. 0 .or. t1diag3 .ne. 0) then
        allocate (x16(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
        x16 = 0.0d0
        if (t1diag3 .ne. 0) then
            factor = t1diag3
            call sum_stripe(4, shape(x16), size(x16), '4123', factor, &
                            x16, s62)

        end if
        if (dt3diag3 .ne. 0) then
            factor = dt3diag3
  call sum_shift(4,shape(intr),size(intr),shape(x16), &
   size(x16),(/n0-n0,n0-n0,n1-n0,n0-n0/),'2143',factor,intr, &
   x16)

        end if
        allocate (f2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(6, shape(t3b), size(t3b), '562134', t3b, f2)
        allocate (z18(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        i1 = k1
        i2 = k2*k3*k4
        i3 = k3*k1*k1
        call egemm(i1, i2, i3, x16, f2, z18)
        deallocate (f2)

        v2b = v2b - 0.500*z18
        deallocate (z18)
        deallocate (x16)
    end if
    end if

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s62), size(s62), '3421', s62, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q25(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q25)
    deallocate (d1)
    deallocate (b2)

    x5 = x5 - q25
    deallocate (q25)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s62), size(s62), '2431', s62, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (s130(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s130)
    deallocate (d1)
    deallocate (d2)
    deallocate (s62)

    call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                    s130)
    deallocate (s130)

    if (lvl_t) then
    if (t1diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2431', intr, d1)
        allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(6, shape(t3b), size(t3b), '523146', t3b, f2)
        allocate (s64(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1*k2*k4
        i3 = k3*k3*k1
        call egemm(i1, i2, i3, d1, f2, s64)
        deallocate (d1)
        deallocate (f2)

        factor = 0.500*t1diag4
        call sum_stripe(4, shape(x1), size(x1), '2341', factor, &
                        x1, s64)
        deallocate (s64)
    end if
    end if

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q7(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q7)
    deallocate (d1)
    deallocate (b2)

    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q26(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, q7, b2, q26)
    deallocate (b2)
    deallocate (q7)

    if (lvl_t) then
        allocate (x15(n0 + 1:n1, n1 + 1:n3))
        x15 = 0.0d0

        if (t1diag1 .ne. 0) then
            allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
            if (t1diag2 .eq. 0) then
                call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                                   (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intm, d1)
            else
                call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                                   (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
            end if
            allocate (b2(n0 + 1:n1, n1 + 1:n3))
            call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
            allocate (q7(n0 + 1:n1, n1 + 1:n3))
            i1 = k3*k1
            i3 = k3*k1
            call egemm1(i1, i3, d1, b2, q7)
            deallocate (d1)
            deallocate (b2)
            x15 = x15 + t1diag1*q7
            deallocate (q7)
        end if
    end if

    call sum_stripe(2, shape(x6), size(x6), '21', -1.000, x6, &
                    q26)
    deallocate (q26)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3214', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s67(n0 + 1:n1, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
    i1 = k4*k1*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s67)
    deallocate (d1)
    deallocate (b2)

    if (lvl_t) then
    if (dt3diag3 .ne. 0 .or. t1diag3 .ne. 0) then
        allocate (x19(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
        x19 = 0.0d0
        if (t1diag3 .ne. 0) then
            factor = t1diag3
            call sum_stripe(4, shape(x19), size(x19), '4123', factor, &
                            x19, s67)

        end if
        if (dt3diag3 .ne. 0) then
            factor = dt3diag3
  call sum_shift(4,shape(intm),size(intm),shape(x19), &
   size(x19),(/n0-n0,n0-n0,n2-n0,n0-n0/),'2143',factor,intm, &
   x19)
        end if

        allocate (f2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(6, shape(t3c), size(t3c), '461235', t3c, f2)
        allocate (z23(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        i1 = k1
        i2 = k2*k3*k4
        i3 = k4*k1*k2
        call egemm(i1, i2, i3, x19, f2, z23)
        deallocate (f2)

        v2b = v2b - z23
        deallocate (z23)
        deallocate (x19)
    end if
    end if

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s67), size(s67), '2431', s67, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q27(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q27)
    deallocate (d1)
    deallocate (b2)

    x5 = x5 + q27
    deallocate (q27)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s67), size(s67), '3421', s67, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s142(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2
    i2 = k2*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, d1, d2, s142)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2314', -1.000, &
                    x2, s142)
    deallocate (s142)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s67), size(s67), '4231', s67, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s140(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s140)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x10), size(x10), '3124', 1.000, &
                    x10, s140)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s140), size(s140), '3214', s140, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s158(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s158)
    deallocate (d1)
    deallocate (b2)
    deallocate (s140)

    call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                    x2, s158)
    deallocate (s158)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s67), size(s67), '2431', s67, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s134(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s134)
    deallocate (d1)
    deallocate (d2)
    deallocate (s67)

    call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                    s134)
    deallocate (s134)

    if (lvl_t) then
    if (t1diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
        allocate (f2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(6, shape(t3c), size(t3c), '413256', t3c, f2)
        allocate (s69(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1*k2*k4
        i3 = k3*k4*k2
        call egemm(i1, i2, i3, d1, f2, s69)
        deallocate (d1)
        deallocate (f2)

        factor = t1diag4
        call sum_stripe(4, shape(x1), size(x1), '2341', factor, &
                        x1, s69)
        deallocate (s69)
    end if
    end if

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q8(n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q8)
    deallocate (d1)
    deallocate (b2)

    if (lvl_t) then
        allocate (x18(n0 + 1:n2, n2 + 1:n3))
        x18 = 0.0d0
        if (t1diag1 .ne. 0) then
            x18 = x18 + t1diag1*q8
        end if
    end if

    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q29(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, q8, b2, q29)
    deallocate (b2)

    call sum_stripe(2, shape(x8), size(x8), '21', -1.000, x8, &
                    q29)
    deallocate (q29)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(q8), size(q8), '21', q8, b1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q28(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, b1, b2, q28)
    deallocate (b1)
    deallocate (b2)
    deallocate (q8)

    call sum_stripe(2, shape(x7), size(x7), '21', 1.000, x7, &
                    q28)
    deallocate (q28)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s72(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s72)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '3124', 1.000, x2, &
                    s72)
    deallocate (s72)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s74(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s74)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2413', 1.000, x2, &
                    s74)
    deallocate (s74)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n2 - n0/), '4132', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s76(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    i1 = k4*k3*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s76)
    deallocate (d1)
    deallocate (b2)

    allocate (x4(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x4 = 0.0d0
    call sum_stripe(4, shape(x4), size(x4), '4123', 1.000, x4, &
                    s76)
    deallocate (s76)

  call sum_shift(4,shape(intm),size(intm),shape(x4), &
   size(x4),(/n0-n0,n1-n0,n2-n0,n0-n0/),'1324',1.000,intm,x4)

    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (z5(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, x4, d2, z5)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '2413', 1.000, &
                    v2b, z5)
    deallocate (z5)
    deallocate (x4)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q9(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, b1, b2, q9)
    deallocate (b1)
    deallocate (b2)

    call sum_stripe(2, shape(x7), size(x7), '21', 1.000, x7, &
                    q9)
    deallocate (q9)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n0 - n0, n2 - n0/), '21', fockb, b1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q10(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, b1, b2, q10)
    deallocate (b1)
    deallocate (b2)

    call sum_stripe(2, shape(x8), size(x8), '21', -1.000, x8, &
                    q10)
    deallocate (q10)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s80(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s80)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x10), size(x10), '3124', 1.000, &
                    x10, s80)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s80), size(s80), '3214', s80, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s136(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s136)
    deallocate (d1)
    deallocate (b2)
    deallocate (s80)

    call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                    x2, s136)
    deallocate (s136)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1423', intm, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s82(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2
    i2 = k2*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, d1, d2, s82)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2314', -1.000, &
                    x2, s82)
    deallocate (s82)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q11(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q11)
    deallocate (d1)
    deallocate (b2)

    x5 = x5 + q11
    deallocate (q11)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s85(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s85)
    deallocate (d1)
    deallocate (b2)

    allocate (x12(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    x12 = 0.0d0
    call sum_stripe(4, shape(x12), size(x12), '4123', 1.000, &
                    x12, s85)
    deallocate (s85)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n1 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s87(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
    i1 = k3*k2
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s87)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '3412', 1.000, x2, &
                    s87)
    deallocate (s87)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q12(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q12)
    deallocate (d1)
    deallocate (b2)

    x6 = x6 + q12
    deallocate (q12)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s90(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k3
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s90)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2413', 1.000, x2, &
                    s90)
    deallocate (s90)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1423', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q13(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q13)
    deallocate (d1)
    deallocate (b2)

    x7 = x7 - q13
    deallocate (q13)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s93(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s93)
    deallocate (d1)
    deallocate (b2)

    allocate (x13(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x13 = 0.0d0
    call sum_stripe(4, shape(x13), size(x13), '4123', 1.000, &
                    x13, s93)
    deallocate (s93)

  call sum_shift(4,shape(intb),size(intb),shape(x13), &
   size(x13),(/n0-n0,n2-n0,n2-n0,n0-n0/),'3142',1.000,intb,x13)

    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (z15(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k3
    i3 = k4*k2
    call egemm(i1, i2, i3, x13, d2, z15)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '2413', 1.000, &
                    v2b, z15)
    deallocate (z15)
    deallocate (x13)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2341', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q14(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q14)
    deallocate (d1)
    deallocate (b2)

    x8 = x8 - q14
    deallocate (q14)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4213', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s96(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s96)
    deallocate (d1)
    deallocate (b2)

    if (lvl_t) then
    if (dt3diag3 .ne. 0 .or. t1diag3 .ne. 0) then
        allocate (x17(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
        x17 = 0.0d0
        if (t1diag3 .ne. 0) then
            factor = t1diag3
            call sum_stripe(4, shape(x17), size(x17), '4123', factor, &
                            x17, s96)

        end if
        if (dt3diag3 .ne. 0) then
            factor = dt3diag3
  call sum_shift(4,shape(intm),size(intm),shape(x17), &
   size(x17),(/n0-n0,n0-n0,n1-n0,n0-n0/),'2134',factor,intm, &
   x17)

        end if
        allocate (f2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(6, shape(t3b), size(t3b), '452136', t3b, f2)
        allocate (z20(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
        i1 = k2
        i2 = k1*k3*k4
        i3 = k3*k1*k2
        call egemm(i1, i2, i3, x17, f2, z20)
        deallocate (f2)

        call sum_stripe(4, shape(v2b), size(v2b), '1243', -1.000, &
                        v2b, z20)
        deallocate (z20)
        deallocate (x17)
    end if
    end if

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s96), size(s96), '3421', s96, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s152(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s152)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2413', 1.000, x2, &
                    s152)
    deallocate (s152)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s96), size(s96), '2431', s96, d1)
    allocate (d2(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3214', t2b, d2)
    allocate (s145(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k4
    i3 = k3*k2
    call egemm(i1, i2, i3, d1, d2, s145)
    deallocate (d1)
    deallocate (d2)
    deallocate (s96)

    call sum_stripe(4, shape(x1), size(x1), '2413', -1.000, &
                    x1, s145)
    deallocate (s145)

  call sum_shift(4,shape(intm),size(intm),shape(x1), &
   size(x1),(/n0-n0,n2-n0,n0-n0,n0-n0/),'1243',1.000,intm,x1)

    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (z1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, x1, b2, z1)
    deallocate (b2)

    call sum_stripe(4, shape(v2b), size(v2b), '2134', -1.000, &
                    v2b, z1)
    deallocate (z1)
    deallocate (x1)

    if (lvl_t) then
    if (t1diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
        allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(6, shape(t3b), size(t3b), '512346', t3b, f2)
        allocate (s98(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
        i1 = k2
        i2 = k1*k2*k3
        i3 = k3*k4*k1
        call egemm(i1, i2, i3, d1, f2, s98)
        deallocate (d1)
        deallocate (f2)

        factor = t1diag4
        call sum_stripe(4, shape(x2), size(x2), '2341', factor, &
                        x2, s98)
        deallocate (s98)
    end if
    end if

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q15(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q15)
    deallocate (d1)
    deallocate (b2)

    if (lvl_t) then
    if (t1diag1 .ne. 0) then
        x15 = x15 + t1diag1*q15
    end if
  call sum_shift(2,shape(fockr),size(fockr),shape(x15), &
   size(x15),(/n0,n1/),'12',1.000,fockr,x15)

    allocate (f2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '521346', t3b, f2)
    allocate (z17(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i2 = k1*k2*k3*k4
    i3 = k3*k1
    call egemm2(i2, i3, x15, f2, z17)
    deallocate (f2)

    v2b = v2b + z17
    deallocate (z17)
    deallocate (x15)
    end if

    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q30(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, q15, b2, q30)
    deallocate (b2)
    deallocate (q15)

    call sum_stripe(2, shape(x6), size(x6), '21', -1.000, x6, &
                    q30)
    deallocate (q30)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '3214', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s101(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s101)
    deallocate (d1)
    deallocate (b2)

    if (lvl_t) then
    if (dt3diag3 .ne. 0 .or. t1diag3 .ne. 0) then
        allocate (x20(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
        x20 = 0.0d0
        if (t1diag3 .ne. 0) then
            factor = t1diag3
            call sum_stripe(4, shape(x20), size(x20), '4123', factor, &
                            x20, s101)
        end if

        if (dt3diag3 .ne. 0) then
            factor = dt3diag3
  call sum_shift(4,shape(intb),size(intb),shape(x20), &
   size(x20),(/n0-n0,n0-n0,n2-n0,n0-n0/),'2143',factor,intb, &
   x20)
        end if

        allocate (f2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(6, shape(t3c), size(t3c), '451236', t3c, f2)
        allocate (z25(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
        i1 = k2
        i2 = k1*k3*k4
        i3 = k4*k2*k2
        call egemm(i1, i2, i3, x20, f2, z25)
        deallocate (f2)

        call sum_stripe(4, shape(v2b), size(v2b), '1243', -0.500, &
                        v2b, z25)
        deallocate (z25)
        deallocate (x20)
    end if
    end if

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s101), size(s101), '3421', s101, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q31(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q31)
    deallocate (d1)
    deallocate (b2)

    x7 = x7 - q31
    deallocate (q31)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s101), size(s101), '2431', s101, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s154(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k3
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s154)
    deallocate (d1)
    deallocate (d2)
    deallocate (s101)

    call sum_stripe(4, shape(x2), size(x2), '2413', 1.000, x2, &
                    s154)
    deallocate (s154)

    if (lvl_t) then
    if (t1diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1)
        allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(6, shape(t3c), size(t3c), '412356', t3c, f2)
        allocate (s103(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
        i1 = k2
        i2 = k1*k2*k3
        i3 = k4*k4*k2
        call egemm(i1, i2, i3, d1, f2, s103)
        deallocate (d1)
        deallocate (f2)

        factor = 0.500*t1diag4
        call sum_stripe(4, shape(x2), size(x2), '2341', factor, &
                        x2, s103)
        deallocate (s103)
    end if
    end if

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q16(n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q16)
    deallocate (d1)
    deallocate (b2)

    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q32(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, q16, b2, q32)
    deallocate (b2)
    deallocate (q16)

    if (lvl_t) then
    if (t1diag1 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
        if (t1diag2 .eq. 0) then
            call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                               (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
        else
            call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                               (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
        end if
        allocate (b2(n0 + 1:n2, n2 + 1:n3))
        call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
        allocate (q16(n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i3 = k4*k2
        call egemm1(i1, i3, d1, b2, q16)
        deallocate (d1)
        deallocate (b2)

        x18 = x18 + t1diag1*q16
        deallocate (q16)

    end if
  call sum_shift(2,shape(fockb),size(fockb),shape(x18), &
   size(x18),(/n0,n2/),'12',1.000,fockb,x18)

    allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '412356', t3c, f2)
    allocate (z22(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i2 = k1*k2*k3*k4
    i3 = k4*k2
    call egemm2(i2, i3, x18, f2, z22)
    deallocate (f2)

    v2b = v2b + z22
    deallocate (z22)
    deallocate (x18)
    end if

    call sum_stripe(2, shape(x8), size(x8), '21', -1.000, x8, &
                    q32)
    deallocate (q32)

    if (diag1 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: acp d1
        if (diag2 .eq. 0) then
            call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                               (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intm, d1)
        else
            call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                               (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
        end if
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
        allocate (s106(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s106)
        deallocate (d1)
        deallocate (d2)

        factor = diag1
        call sum_stripe(4, shape(x9), size(x9), '3412', factor, &
                        x9, s106)
        deallocate (s106)
        factor = 0
!
    end if
    if (ext_cor .and. diag1 .ne. 1) then
        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                            d2_mc)
        allocate (s106_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, s106_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(s106_mc), size(s106_mc), '3412', &
                            s106_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                            d2_mc)
        allocate (z10_mc(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k2*k4
        i3 = k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, z10_mc)
        deallocate (s106_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = (1.0 - diag1)
        call sum_stripe(4, shape(v2b), size(v2b), '1324', factor, &
                        v2b, z10_mc)
        deallocate (z10_mc)
        factor = 0
    end if
!
    if (ext_cor .and. diag1 .eq. 1 .and. diag2 .eq. 0) then
        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                            d2_mc)
        allocate (s106_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, s106_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(s106_mc), size(s106_mc), '3412', &
                            s106_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                            d2_mc)
        allocate (z10_mc(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k2*k4
        i3 = k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, z10_mc)
        deallocate (s106_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = diag1
        call sum_stripe(4, shape(v2b), size(v2b), '1324', factor, &
                        v2b, z10_mc)
        deallocate (z10_mc)
        factor = 0

        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                            d2_mc)
        allocate (s106_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, s106_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(s106_mc), size(s106_mc), '3412', &
                            s106_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                            d2_mc)
        allocate (z10_mc(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k2*k4
        i3 = k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, z10_mc)
        deallocate (s106_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = -diag1
        call sum_stripe(4, shape(v2b), size(v2b), '1324', factor, &
                        v2b, z10_mc)
        deallocate (z10_mc)
        factor = 0
    end if

    if (diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))                 !ilias: acp d4
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '1432', intr, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
        allocate (q17(n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1
        i3 = k3*k3*k1
        call egemm(i1, i2, i3, d1, d2, q17)
        deallocate (d1)
        deallocate (d2)

        factor = -0.500*diag4
        call sum_stripe(2, shape(x5), size(x5), '21', factor, x5, &
                        q17)
        deallocate (q17)
        factor = 0
!
    end if
    if (ext_cor .and. diag4 .ne. 1) then
        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intr), size(intr), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '1432', intr, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                            d2_mc)
        allocate (q17_mc(n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1
        i3 = k3*k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, q17_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (b1_mc(n0 + 1:n1, n0 + 1:n1))
        call reorder_stripe(2, shape(q17_mc), size(q17_mc), '21', q17_mc, &
                            b1_mc)
        allocate (d2_mc(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4123', t2b_mc, &
                            d2_mc)
        allocate (z6_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        i1 = k1
        i2 = k2*k3*k4
        i3 = k1
        call egemm(i1, i2, i3, b1_mc, d2_mc, z6_mc)
        deallocate (b1_mc)
        deallocate (d2_mc)
        deallocate (q17_mc)

        factor = 0.500*(1.0 - diag4)
        v2b = v2b + factor*z6_mc
        deallocate (z6_mc)
        factor = 0
    end if

    if (diag3 .ne. 0) then
        allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))                !ilias: acp d3
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2134', intr, d1)
        allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
        allocate (q18(n1 + 1:n3, n1 + 1:n3))
        i1 = k3
        i2 = k3
        i3 = k3*k1*k1
        call egemm(i1, i2, i3, d1, d2, q18)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*diag3
        call sum_stripe(2, shape(x6), size(x6), '21', factor, x6, &
                        q18)
        deallocate (q18)
        factor = 0
!
    end if
    if (ext_cor .and. diag3 .ne. 1) then
        allocate (d1_mc(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2134', intr, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3412', t2a_mc, &
                            d2_mc)
        allocate (q18_mc(n1 + 1:n3, n1 + 1:n3))
        i1 = k3
        i2 = k3
        i3 = k3*k1*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, q18_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (b1_mc(n1 + 1:n3, n1 + 1:n3))
        call reorder_stripe(2, shape(q18_mc), size(q18_mc), '21', q18_mc, &
                            b1_mc)
        allocate (d2_mc(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '2134', t2b_mc, &
                            d2_mc)
        allocate (z7_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
        i1 = k3
        i2 = k1*k2*k4
        i3 = k3
        call egemm(i1, i2, i3, b1_mc, d2_mc, z7_mc)
        deallocate (q18_mc)
        deallocate (b1_mc)
        deallocate (d2_mc)

        factor = 0.500*(1.0 - diag3)
        call sum_stripe(4, shape(v2b), size(v2b), '1342', factor, &
                        v2b, z7_mc)
        deallocate (z7_mc)
        factor = 0
    end if

    if (diag1 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: acp d1
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
        allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
        allocate (s110(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1, d2, s110)
        deallocate (d1)
        deallocate (d2)

        factor = diag1
        call sum_stripe(4, shape(x14), size(x14), '3412', factor, &
                        x14, s110)
        deallocate (s110)
        factor = 0
!
    end if
    if (ext_cor .and. diag1 .ne. 1) then
        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                            d2_mc)
        allocate (s110_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k1*k3
        i3 = k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, s110_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(s110_mc), size(s110_mc), '3412', &
                            s110_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                            d2_mc)
        allocate (z16_mc(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k2*k4
        i3 = k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, z16_mc)
        deallocate (s110_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = (1.0 - diag1)
        call sum_stripe(4, shape(v2b), size(v2b), '1324', factor, &
                        v2b, z16_mc)
        deallocate (z16_mc)
        factor = 0
    end if

    if (diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))                 !ilias:acp d3
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
        allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
        allocate (q19(n1 + 1:n3, n1 + 1:n3))
        i1 = k3
        i2 = k3
        i3 = k4*k1*k2
        call egemm(i1, i2, i3, d1, d2, q19)
        deallocate (d1)
        deallocate (d2)

        factor = -diag3
        call sum_stripe(2, shape(x6), size(x6), '21', factor, x6, &
                        q19)
        deallocate (q19)
        factor = 0
!
    end if
    if (ext_cor .and. diag3 .ne. 1) then
        allocate (d1_mc(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3412', t2b_mc, &
                            d2_mc)
        allocate (q19_mc(n1 + 1:n3, n1 + 1:n3))
        i1 = k3
        i2 = k3
        i3 = k4*k1*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, q19_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (b1_mc(n1 + 1:n3, n1 + 1:n3))
        call reorder_stripe(2, shape(q19_mc), size(q19_mc), '21', q19_mc, &
                            b1_mc)
        allocate (d2_mc(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '2134', t2b_mc, &
                            d2_mc)
        allocate (z7_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
        i1 = k3
        i2 = k1*k2*k4
        i3 = k3
        call egemm(i1, i2, i3, b1_mc, d2_mc, z7_mc)
        deallocate (q19_mc)
        deallocate (b1_mc)
        deallocate (d2_mc)

        factor = -(1.0 - diag3)
        call sum_stripe(4, shape(v2b), size(v2b), '1342', factor, &
                        v2b, z7_mc)
        deallocate (z7_mc)
        factor = 0
    end if

  call sum_shift(2,shape(fockr),size(fockr),shape(x6), &
   size(x6),(/n1,n1/),'21',1.000,fockr,x6)

    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (z7(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, x6, d2, z7)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '1342', 1.000, &
                    v2b, z7)
    deallocate (z7)
    deallocate (x6)

    if (diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))                 !ilias: acp d3
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1)
        allocate (d2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
        call reorder_stripe(4, shape(t2b), size(t2b), '3421', t2b, d2)
        allocate (q20(n2 + 1:n3, n2 + 1:n3))
        i1 = k4
        i2 = k4
        i3 = k3*k1*k2
        call egemm(i1, i2, i3, d1, d2, q20)
        deallocate (d1)
        deallocate (d2)

        factor = -diag3
        call sum_stripe(2, shape(x8), size(x8), '21', factor, x8, &
                        q20)
        deallocate (q20)
        factor = 0
!
    end if
    if (ext_cor .and. diag3 .ne. 1) then
        allocate (d1_mc(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3421', t2b_mc, &
                            d2_mc)
        allocate (q20_mc(n2 + 1:n3, n2 + 1:n3))
        i1 = k4
        i2 = k4
        i3 = k3*k1*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, q20_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (b1_mc(n2 + 1:n3, n2 + 1:n3))
        call reorder_stripe(2, shape(q20_mc), size(q20_mc), '21', q20_mc, &
                            b1_mc)
        allocate (d2_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '1234', t2b_mc, &
                            d2_mc)
        allocate (z9_mc(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
        i1 = k4
        i2 = k1*k2*k3
        i3 = k4
        call egemm(i1, i2, i3, b1_mc, d2_mc, z9_mc)
        deallocate (q20_mc)
        deallocate (b1_mc)
        deallocate (d2_mc)

        factor = -(1.0 - diag3)
        call sum_stripe(4, shape(v2b), size(v2b), '2341', factor, &
                        v2b, z9_mc)
        deallocate (z9_mc)
        factor = 0
    end if

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s114(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s114)
    deallocate (d1)
    deallocate (d2)

    if (diag5 .ne. 0) then
        factor = diag5
        call sum_stripe(4, shape(x10), size(x10), '3412', factor, &
                        x10, s114)        !ilias: acp d5
        factor = 0
!
    end if

    if (ext_cor .and. diag5 .ne. 1) then
        allocate (d1_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1_mc)
        allocate (d2_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '1234', t2b_mc, &
                            d2_mc)
        allocate (s114_mc(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
        i1 = k1*k2
        i2 = k1*k2
        i3 = k3*k4
        call egemm(i1, i2, i3, d1_mc, d2_mc, s114_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(4, shape(s114_mc), size(s114_mc), '3412', &
                            s114_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3412', t2b_mc, &
                            d2_mc)
        allocate (z11_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        i1 = k1*k2
        i2 = k3*k4
        i3 = k1*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, z11_mc)
        deallocate (s114_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = (1.0 - diag5)
        v2b = v2b + factor*z11_mc
        deallocate (z11_mc)
        factor = 0
    end if

  call sum_shift(4,shape(intm),size(intm),shape(x10), &
   size(x10),(/n0-n0,n0-n0,n0-n0,n0-n0/),'2143',1.000,intm,x10)

    allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
    allocate (z11(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2
    i2 = k3*k4
    i3 = k1*k2
    call egemm(i1, i2, i3, x10, d2, z11)
    deallocate (d2)

    v2b = v2b + z11
    deallocate (z11)
    deallocate (x10)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s114), size(s114), '4312', s114, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s148(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s148)
    deallocate (d1)
    deallocate (b2)
    deallocate (s114)

    call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                    x2, s148)
    deallocate (s148)

  call sum_shift(4,shape(intm),size(intm),shape(x2), &
   size(x2),(/n0-n0,n1-n0,n0-n0,n0-n0/),'2143',1.000,intm,x2)

    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (z3(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, x2, b2, z3)
    deallocate (b2)

    v2b = v2b - z3
    deallocate (z3)
    deallocate (x2)

    if (diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))                 !ilias: acp d4
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
        allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
        allocate (q21(n0 + 1:n2, n0 + 1:n2))
        i1 = k2
        i2 = k2
        i3 = k3*k4*k1
        call egemm(i1, i2, i3, d1, d2, q21)
        deallocate (d1)
        deallocate (d2)

        factor = diag4
        call sum_stripe(2, shape(x7), size(x7), '21', factor, x7, &
                        q21)
        deallocate (q21)
        factor = 0
!
    end if
    if (ext_cor .and. diag4 .ne. 1) then
        allocate (d1_mc(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4123', t2b_mc, &
                            d2_mc)
        allocate (q21_mc(n0 + 1:n2, n0 + 1:n2))
        i1 = k2
        i2 = k2
        i3 = k3*k4*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, q21_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (b1_mc(n0 + 1:n2, n0 + 1:n2))
        call reorder_stripe(2, shape(q21_mc), size(q21_mc), '21', q21_mc, &
                            b1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                            d2_mc)
        allocate (z8_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
        i1 = k2
        i2 = k1*k3*k4
        i3 = k2
        call egemm(i1, i2, i3, b1_mc, d2_mc, z8_mc)
        deallocate (q21_mc)
        deallocate (b1_mc)
        deallocate (d2_mc)

        factor = -(1.0 - diag4)
        call sum_stripe(4, shape(v2b), size(v2b), '1243', factor, &
                        v2b, z8_mc)
        deallocate (z8_mc)
        factor = 0
    end if

    if (diag2 .ne. 0) then
        allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n1 + 1:n3))                !ilias: acp d2
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '1423', intm, d1)
        allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
        allocate (s117(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3))
        i1 = k3*k2
        i2 = k2*k3
        i3 = k4*k1
        call egemm(i1, i2, i3, d1, d2, s117)
        deallocate (d1)
        deallocate (d2)

        factor = -diag2
        call sum_stripe(4, shape(x12), size(x12), '3412', factor, &
                        x12, s117)
        deallocate (s117)
        factor = 0
!
    end if
    if (ext_cor .and. diag2 .ne. 1) then
        allocate (d1_mc(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n1 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '1423', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4123', t2b_mc, &
                            d2_mc)
        allocate (s117_mc(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3))
        i1 = k3*k2
        i2 = k2*k3
        i3 = k4*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, s117_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(s117_mc), size(s117_mc), '3412', &
                            s117_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3214', t2b_mc, &
                            d2_mc)
        allocate (z13_mc(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
        i1 = k2*k3
        i2 = k1*k4
        i3 = k3*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, z13_mc)
        deallocate (s117_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = (1.0 - diag2)
        call sum_stripe(4, shape(v2b), size(v2b), '1423', factor, &
                        v2b, z13_mc)
        deallocate (z13_mc)
        factor = 0
    end if

  call sum_shift(4,shape(intm),size(intm),shape(x12), &
   size(x12),(/n0-n0,n1-n0,n1-n0,n0-n0/),'3124',1.000,intm,x12)

    allocate (d2(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3214', t2b, d2)
    allocate (z13(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k1*k4
    i3 = k3*k2
    call egemm(i1, i2, i3, x12, d2, z13)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '1423', -1.000, &
                    v2b, z13)
    deallocate (z13)
    deallocate (x12)

    if (diag1 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: acp d1
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
        allocate (s119(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s119)
        deallocate (d1)
        deallocate (d2)

        factor = diag1
        call sum_stripe(4, shape(x9), size(x9), '3412', factor, &
                        x9, s119)
        deallocate (s119)
        factor = 0
    end if
!
    if (ext_cor .and. diag1 .ne. 1) then
        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                            d2_mc)
        allocate (s119_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
        i1 = k3*k1
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, s119_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(s119_mc), size(s119_mc), '3412', &
                            s119_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                            d2_mc)
        allocate (z10_mc(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k2*k4
        i3 = k3*k1
        call egemm(i1, i2, i3, d1_mc, d2_mc, z10_mc)
        deallocate (s119_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = (1.0 - diag1)
        call sum_stripe(4, shape(v2b), size(v2b), '1324', factor, &
                        v2b, z10_mc)
        deallocate (z10_mc)
        factor = 0
    end if

  call sum_shift(4,shape(intr),size(intr),shape(x9), &
   size(x9),(/n0-n0,n1-n0,n1-n0,n0-n0/),'3142',1.000,intr,x9)

    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (z10(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, x9, d2, z10)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '1324', 1.000, &
                    v2b, z10)
    deallocate (z10)
    deallocate (x9)

    if (diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))                 !ilias: acp d4
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
        allocate (q22(n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1
        i3 = k3*k4*k2
        call egemm(i1, i2, i3, d1, d2, q22)
        deallocate (d1)
        deallocate (d2)

        factor = diag4
        call sum_stripe(2, shape(x5), size(x5), '21', factor, x5, &
                        q22)
        deallocate (q22)
        factor = 0
!
    end if
    if (ext_cor .and. diag4 .ne. 1) then
        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                            d2_mc)
        allocate (q22_mc(n0 + 1:n1, n0 + 1:n1))
        i1 = k1
        i2 = k1
        i3 = k3*k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, q22_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (b1_mc(n0 + 1:n1, n0 + 1:n1))
        call reorder_stripe(2, shape(q22_mc), size(q22_mc), '21', q22_mc, &
                            b1_mc)
        allocate (d2_mc(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4123', t2b_mc, &
                            d2_mc)
        allocate (z6_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        i1 = k1
        i2 = k2*k3*k4
        i3 = k1
        call egemm(i1, i2, i3, b1_mc, d2_mc, z6_mc)
        deallocate (q22_mc)
        deallocate (b1_mc)
        deallocate (d2_mc)

        factor = (1.0 - diag4)
        v2b = v2b - factor*z6_mc
        deallocate (z6_mc)
        factor = 0
    end if

  call sum_shift(2,shape(fockr),size(fockr),shape(x5), &
   size(x5),(/n0,n0/),'12',1.000,fockr,x5)

    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (z6(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, x5, d2, z6)
    deallocate (d2)

    v2b = v2b - z6
    deallocate (z6)
    deallocate (x5)

    if (diag3 .ne. 0) then
        allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))                !ilias: acp d3
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
        allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
        allocate (q23(n2 + 1:n3, n2 + 1:n3))
        i1 = k4
        i2 = k4
        i3 = k4*k2*k2
        call egemm(i1, i2, i3, d1, d2, q23)
        deallocate (d1)
        deallocate (d2)

        factor = -0.500*diag3
        call sum_stripe(2, shape(x8), size(x8), '21', factor, x8, &
                        q23)
        deallocate (q23)
        factor = 0
!
    end if
    if (ext_cor .and. diag3 .ne. 1) then
        allocate (d1_mc(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3412', t2c_mc, &
                            d2_mc)
        allocate (q23_mc(n2 + 1:n3, n2 + 1:n3))
        i1 = k4
        i2 = k4
        i3 = k4*k2*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, q23_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (b1_mc(n2 + 1:n3, n2 + 1:n3))
        call reorder_stripe(2, shape(q23_mc), size(q23_mc), '21', q23_mc, &
                            b1_mc)
        allocate (d2_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '1234', t2b_mc, &
                            d2_mc)
        allocate (z9_mc(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
        i1 = k4
        i2 = k1*k2*k3
        i3 = k4
        call egemm(i1, i2, i3, b1_mc, d2_mc, z9_mc)
        deallocate (q23_mc)
        deallocate (b1_mc)
        deallocate (d2_mc)

        factor = -0.500*(1.0 - diag3)
        call sum_stripe(4, shape(v2b), size(v2b), '2341', factor, &
                        v2b, z9_mc)
        deallocate (z9_mc)
        factor = 0
    end if

  call sum_shift(2,shape(fockb),size(fockb),shape(x8), &
   size(x8),(/n2,n2/),'21',1.000,fockb,x8)

    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (z9(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, x8, d2, z9)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '2341', 1.000, &
                    v2b, z9)
    deallocate (z9)
    deallocate (x8)

    if (diag4 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))                 !ilias: acp d4
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1)
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
        allocate (q24(n0 + 1:n2, n0 + 1:n2))
        i1 = k2
        i2 = k2
        i3 = k4*k4*k2
        call egemm(i1, i2, i3, d1, d2, q24)
        deallocate (d1)
        deallocate (d2)

        factor = 0.500*diag4
        call sum_stripe(2, shape(x7), size(x7), '21', factor, x7, &
                        q24)
        deallocate (q24)
        factor = 0
!
    end if
    if (ext_cor .and. diag4 .ne. 1) then
        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_shift(4, shape(intb), size(intb), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                            d2_mc)
        allocate (q24_mc(n0 + 1:n2, n0 + 1:n2))
        i1 = k2
        i2 = k2
        i3 = k4*k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, q24_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (b1_mc(n0 + 1:n2, n0 + 1:n2))
        call reorder_stripe(2, shape(q24_mc), size(q24_mc), '21', q24_mc, &
                            b1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                            d2_mc)
        allocate (z8_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
        i1 = k2
        i2 = k1*k3*k4
        i3 = k2
        call egemm(i1, i2, i3, b1_mc, d2_mc, z8_mc)
        deallocate (q24_mc)
        deallocate (b1_mc)
        deallocate (d2_mc)

        factor = -0.500*(1.0 - diag4)
        call sum_stripe(4, shape(v2b), size(v2b), '1243', factor, &
                        v2b, z8_mc)
        deallocate (z8_mc)
        factor = 0
    end if

  call sum_shift(2,shape(fockb),size(fockb),shape(x7), &
   size(x7),(/n0,n0/),'12',1.000,fockb,x7)

    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (z8(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, x7, d2, z8)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '1243', -1.000, &
                    v2b, z8)
    deallocate (z8)
    deallocate (x7)

    if (diag1 .ne. 0) then
        allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: acp d1
        if (diag2 .eq. 0) then
            call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                               (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
        else
            call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                               (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
        end if
        allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
        allocate (s124(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1, d2, s124)
        deallocate (d1)
        deallocate (d2)

        factor = diag1
        call sum_stripe(4, shape(x14), size(x14), '3412', factor, &
                        x14, s124)
        deallocate (s124)
        factor = 0
!
    end if
    if (ext_cor .and. diag1 .ne. 1) then
        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                            d2_mc)
        allocate (s124_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, s124_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(s124_mc), size(s124_mc), '3412', &
                            s124_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                            d2_mc)
        allocate (z16_mc(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k2*k4
        i3 = k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, z16_mc)
        deallocate (s124_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = (1.0 - diag1)
        call sum_stripe(4, shape(v2b), size(v2b), '1324', factor, &
                        v2b, z16_mc)
        deallocate (z16_mc)
        factor = 0
    end if
!
    if (ext_cor .and. diag1 .eq. 1 .and. diag2 .eq. 0) then
        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                            d2_mc)
        allocate (s124_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, s124_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(s124_mc), size(s124_mc), '3412', &
                            s124_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                            d2_mc)
        allocate (z16_mc(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k2*k4
        i3 = k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, z16_mc)
        deallocate (s124_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = diag1
        call sum_stripe(4, shape(v2b), size(v2b), '1324', factor, &
                        v2b, z16_mc)
        deallocate (z16_mc)
        factor = 0

        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                           size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intm, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                            d2_mc)
        allocate (s124_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
        i1 = k4*k2
        i2 = k1*k3
        i3 = k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, s124_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(4, shape(s124_mc), size(s124_mc), '3412', &
                            s124_mc, d1_mc)
        allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                            d2_mc)
        allocate (z16_mc(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
        i1 = k1*k3
        i2 = k2*k4
        i3 = k4*k2
        call egemm(i1, i2, i3, d1_mc, d2_mc, z16_mc)
        deallocate (s124_mc)
        deallocate (d1_mc)
        deallocate (d2_mc)

        factor = -diag1
        call sum_stripe(4, shape(v2b), size(v2b), '1324', factor, &
                        v2b, z16_mc)
        deallocate (z16_mc)
        factor = 0
    end if

  call sum_shift(4,shape(intm),size(intm),shape(x14), &
   size(x14),(/n0-n0,n2-n0,n1-n0,n0-n0/),'3142',1.000,intm,x14)

    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (z16(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, x14, d2, z16)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '1324', 1.000, &
                    v2b, z16)
    deallocate (z16)
    deallocate (x14)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (z2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, z2)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(v2b), size(v2b), '4123', 1.000, &
                    v2b, z2)
    deallocate (z2)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n1 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (z14(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, z14)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(v2b), size(v2b), '3412', 1.000, &
                    v2b, z14)
    deallocate (z14)

    if (lvl_t) then
    if (dt3diag4 .ne. 0) then
        allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2431', intr, d1)
        allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(6, shape(t3b), size(t3b), '523146', t3b, f2)
        allocate (z19(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
        i1 = k3
        i2 = k1*k2*k4
        i3 = k3*k3*k1
        call egemm(i1, i2, i3, d1, f2, z19)
        deallocate (d1)
        deallocate (f2)

        factor = 0.500*dt3diag4
        call sum_stripe(4, shape(v2b), size(v2b), '1342', factor, &
                        v2b, z19)
        deallocate (z19)

        allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n2 - n0/), '1432', intm, d1)
        allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(6, shape(t3b), size(t3b), '512346', t3b, f2)
        allocate (z21(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
        i1 = k4
        i2 = k1*k2*k3
        i3 = k3*k4*k1
        call egemm(i1, i2, i3, d1, f2, z21)
        deallocate (d1)
        deallocate (f2)

        factor = dt3diag4
        call sum_stripe(4, shape(v2b), size(v2b), '2341', factor, &
                        v2b, z21)
        deallocate (z21)

        allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
        allocate (f2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(6, shape(t3c), size(t3c), '413256', t3c, f2)
        allocate (z24(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
        i1 = k3
        i2 = k1*k2*k4
        i3 = k3*k4*k2
        call egemm(i1, i2, i3, d1, f2, z24)
        deallocate (d1)
        deallocate (f2)

        factor = dt3diag4
        call sum_stripe(4, shape(v2b), size(v2b), '1342', factor, &
                        v2b, z24)
        deallocate (z24)

        allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
        allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(6, shape(t3c), size(t3c), '412356', t3c, f2)
        allocate (z26(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
        i1 = k4
        i2 = k1*k2*k3
        i3 = k4*k4*k2
        call egemm(i1, i2, i3, d1, f2, z26)
        deallocate (d1)
        deallocate (f2)

        factor = 0.500*dt3diag4
        call sum_stripe(4, shape(v2b), size(v2b), '2341', factor, &
                        v2b, z26)
        deallocate (z26)
    end if
    end if

    if (lvl_q) then
        allocate (t4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 21 lines
                      n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
        rewind (tb)
        read (tb) t4b
        allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
        allocate (h2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, &
                     n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(8, shape(t4b), size(t4b), '67231458', t4b, h2)
        allocate (z27(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        i2 = k1*k2*k3*k4
        i3 = k3*k3*k1*k1
        call egemm2(i2, i3, d1, h2, z27)
        deallocate (d1)
        deallocate (h2)
        deallocate (t4b)

        v2b = v2b + 0.250*z27
        deallocate (z27)

        allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 21 lines
                      n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
        rewind (tc)
        read (tc) t4c
        allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
        allocate (h2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, &
                     n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(8, shape(t4c), size(t4c), '57132468', t4c, h2)
        allocate (z28(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        i2 = k1*k2*k3*k4
        i3 = k3*k4*k1*k2
        call egemm2(i2, i3, d1, h2, z28)
        deallocate (d1)
        deallocate (h2)
        deallocate (t4c)

        v2b = v2b + z28
        deallocate (z28)

        allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 21 lines
                      n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
        rewind (td)
        read (td) t4d
        allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
        allocate (h2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, &
                     n0 + 1:n2, n0 + 1:n1))
        call reorder_stripe(8, shape(t4d), size(t4d), '56123478', t4d, h2)
        allocate (z29(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
        i2 = k1*k2*k3*k4
        i3 = k4*k4*k2*k2
        call egemm2(i2, i3, d1, h2, z29)
        deallocate (d1)
        deallocate (h2)
        deallocate (t4d)

        v2b = v2b + 0.250*z29
        deallocate (z29)

    end if
    do i = n0 + 1, n1
    do j = n0 + 1, n2
    do a = n1 + 1, n3
    do b = n2 + 1, n3
        coeleft = fockb(b, b) &
                  + fockr(a, a) &
                  - fockb(j, j) &
                  - fockr(i, i) &
                  + shift
        t2b(b, a, j, i) = t2b(b, a, j, i) - v2b(b, a, j, i)/coeleft
    end do
    end do
    end do
    end do

end subroutine t2b_update
