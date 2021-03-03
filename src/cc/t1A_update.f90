subroutine t1a_update(n0, n1, n2, n3, k1, k2, k3, k4, lvl_t, shift, v1a, &
                      fockr, fockb, intr, intb, intm, t1a, t1b, t2a, t2b, t2c, t3a, t3b, t3c, t3d)

    integer :: a, b, c, e, f, g, h, i, j, k, m, n, o, p
    logical :: lvl_t
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
    real(kind=8) :: v1a(n1 + 1:n3, n0 + 1:n1)

    real(kind=8), allocatable::b1(:, :)
    real(kind=8), allocatable::b2(:, :)
    real(kind=8), allocatable::d1(:, :, :, :)
    real(kind=8), allocatable::d2(:, :, :, :)
    real(kind=8), allocatable::f2(:, :, :, :, :, :)

    real(kind=8), allocatable::q14(:, :)
    real(kind=8), allocatable::q16(:, :)
    real(kind=8), allocatable::q18(:, :)
    real(kind=8), allocatable::q20(:, :)
    real(kind=8), allocatable::q22(:, :)
    real(kind=8), allocatable::q24(:, :)
    real(kind=8), allocatable::q26(:, :)
    real(kind=8), allocatable::q28(:, :)
    real(kind=8), allocatable::q40(:, :)
    real(kind=8), allocatable::q30(:, :)
    real(kind=8), allocatable::q32(:, :)
    real(kind=8), allocatable::q34(:, :)
    real(kind=8), allocatable::q36(:, :)
    real(kind=8), allocatable::q42(:, :)
    real(kind=8), allocatable::q38(:, :)
    real(kind=8), allocatable::x1(:, :)
    real(kind=8), allocatable::z1(:, :)
    real(kind=8), allocatable::x2(:, :)
    real(kind=8), allocatable::z2(:, :)
    real(kind=8), allocatable::z3(:, :)
    real(kind=8), allocatable::z4(:, :)
    real(kind=8), allocatable::x3(:, :)
    real(kind=8), allocatable::z5(:, :)
    real(kind=8), allocatable::z6(:, :)
    real(kind=8), allocatable::z7(:, :)
    real(kind=8), allocatable::x4(:, :)
    real(kind=8), allocatable::z8(:, :)
    real(kind=8), allocatable::z9(:, :)
    real(kind=8), allocatable::z10(:, :)
    real(kind=8), allocatable::z11(:, :)
    real(kind=8), allocatable::z12(:, :)
    real(kind=8), allocatable::z13(:, :)

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q14(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, b1, b2, q14)
    deallocate (b1)
    deallocate (b2)

    allocate (x1(n0 + 1:n1, n0 + 1:n1))
    x1 = 0.0d0
    call sum_stripe(2, shape(x1), size(x1), '21', 1.000, x1, &
                    q14)
    deallocate (q14)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2413', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q16(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q16)
    deallocate (d1)
    deallocate (b2)

    x1 = x1 + q16
    deallocate (q16)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2431', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q18(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q18)
    deallocate (d1)
    deallocate (b2)

    allocate (x2(n1 + 1:n3, n1 + 1:n3))
    x2 = 0.0d0
    x2 = x2 + q18
    deallocate (q18)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q20(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q20)
    deallocate (d1)
    deallocate (b2)

    x1 = x1 + q20
    deallocate (q20)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q22(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q22)
    deallocate (d1)
    deallocate (b2)

    x2 = x2 + q22
    deallocate (q22)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
    allocate (q24(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k3*k1*k1
    call egemm(i1, i2, i3, d1, d2, q24)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x2), size(x2), '21', -0.500, x2, &
                    q24)
    deallocate (q24)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2431', intr, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (q26(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k3*k3*k1
    call egemm(i1, i2, i3, d1, d2, q26)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x1), size(x1), '21', 0.500, x1, &
                    q26)
    deallocate (q26)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q28(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q28)
    deallocate (d1)
    deallocate (b2)

    allocate (x3(n0 + 1:n1, n1 + 1:n3))
    x3 = 0.0d0
    x3 = x3 + q28

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(q28), size(q28), '21', q28, b1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q40(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, b1, b2, q40)
    deallocate (b1)
    deallocate (b2)
    deallocate (q28)

    call sum_stripe(2, shape(x1), size(x1), '21', 1.000, x1, &
                    q40)
    deallocate (q40)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
    allocate (q30(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k4*k1*k2
    call egemm(i1, i2, i3, d1, d2, q30)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x2), size(x2), '21', -1.000, x2, &
                    q30)
    deallocate (q30)

    call sumx21(0, n3, n1, n3, n1, n3, x2, fockr, 1.000)

    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (z2(n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, x2, b2, z2)
    deallocate (b2)

    call sum_stripe(2, shape(v1a), size(v1a), '21', 1.000, &
                    v1a, z2)
    deallocate (z2)
    deallocate (x2)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (q32(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k3*k4*k2
    call egemm(i1, i2, i3, d1, d2, q32)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x1), size(x1), '21', 1.000, x1, &
                    q32)
    deallocate (q32)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q34(n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q34)
    deallocate (d1)
    deallocate (b2)

    allocate (x4(n0 + 1:n2, n2 + 1:n3))
    x4 = 0.0d0
    x4 = x4 + q34
    deallocate (q34)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q36(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q36)
    deallocate (d1)
    deallocate (b2)

    x3 = x3 + q36

    call sumx12(0, n3, n0, n1, n1, n3, x3, fockr, 1.000)

    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (z5(n1 + 1:n3, n0 + 1:n1))
    i2 = k1*k3
    i3 = k3*k1
    call egemm2(i2, i3, x3, d2, z5)
    deallocate (d2)

    v1a = v1a + z5
    deallocate (z5)
    deallocate (x3)

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(q36), size(q36), '21', q36, b1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q42(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, b1, b2, q42)
    deallocate (b1)
    deallocate (b2)
    deallocate (q36)

    call sum_stripe(2, shape(x1), size(x1), '21', 1.000, x1, &
                    q42)
    deallocate (q42)

    call sumx12(0, n3, n0, n1, n0, n1, x1, fockr, 1.000)

    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (z1(n1 + 1:n3, n0 + 1:n1))
    i1 = k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, x1, b2, z1)
    deallocate (b2)

    v1a = v1a - z1
    deallocate (z1)
    deallocate (x1)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q38(n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q38)
    deallocate (d1)
    deallocate (b2)

    x4 = x4 + q38
    deallocate (q38)

    call sumx12(0, n3, n0, n2, n2, n3, x4, fockb, 1.000)

    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (z8(n1 + 1:n3, n0 + 1:n1))
    i2 = k1*k3
    i3 = k4*k2
    call egemm2(i2, i3, x4, d2, z8)
    deallocate (d2)

    v1a = v1a + z8
    deallocate (z8)
    deallocate (x4)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2413', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (z3(n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, z3)
    deallocate (d1)
    deallocate (b2)

    v1a = v1a + z3
    deallocate (z3)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (z4(n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, z4)
    deallocate (d1)
    deallocate (b2)

    v1a = v1a + z4
    deallocate (z4)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2143', intr, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
    allocate (z6(n1 + 1:n3, n0 + 1:n1))
    i1 = k1
    i2 = k3
    i3 = k3*k1*k1
    call egemm(i1, i2, i3, d1, d2, z6)
    deallocate (d1)
    deallocate (d2)

    v1a = v1a - 0.500*z6
    deallocate (z6)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2431', intr, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (z7(n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1
    i3 = k3*k3*k1
    call egemm(i1, i2, i3, d1, d2, z7)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(v1a), size(v1a), '21', 0.500, &
                    v1a, z7)
    deallocate (z7)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intm, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
    allocate (z9(n1 + 1:n3, n0 + 1:n1))
    i1 = k1
    i2 = k3
    i3 = k4*k1*k2
    call egemm(i1, i2, i3, d1, d2, z9)
    deallocate (d1)
    deallocate (d2)

    v1a = v1a - z9
    deallocate (z9)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (z10(n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1
    i3 = k3*k4*k2
    call egemm(i1, i2, i3, d1, d2, z10)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(v1a), size(v1a), '21', 1.000, &
                    v1a, z10)
    deallocate (z10)

    if (lvl_t) then
        allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
        allocate (f2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(6, shape(t3a), size(t3a), '451236', t3a, f2)
        allocate (z11(n1 + 1:n3, n0 + 1:n1))
        i2 = k1*k3
        i3 = k3*k3*k1*k1
        call egemm2(i2, i3, d1, f2, z11)
        deallocate (d1)
        deallocate (f2)

        v1a = v1a + 0.250*z11
        deallocate (z11)

        allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
        allocate (f2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(6, shape(t3b), size(t3b), '451236', t3b, f2)
        allocate (z12(n1 + 1:n3, n0 + 1:n1))
        i2 = k1*k3
        i3 = k3*k4*k1*k2
        call egemm2(i2, i3, d1, f2, z12)
        deallocate (d1)
        deallocate (f2)

        v1a = v1a + z12
        deallocate (z12)

        allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
        allocate (f2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
        call reorder_stripe(6, shape(t3c), size(t3c), '451236', t3c, f2)
        allocate (z13(n1 + 1:n3, n0 + 1:n1))
        i2 = k1*k3
        i3 = k4*k4*k2*k2
        call egemm2(i2, i3, d1, f2, z13)
        deallocate (d1)
        deallocate (f2)

        v1a = v1a + 0.250*z13
        deallocate (z13)
    end if

    do i = n0 + 1, n1
    do a = n1 + 1, n3
        coeleft = fockr(a, a) &
                  - fockr(i, i) &
                  + shift
        t1a(a, i) = t1a(a, i) - v1a(a, i)/coeleft
    end do
    end do

end subroutine t1a_update
