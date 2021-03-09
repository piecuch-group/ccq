subroutine t1b_update(n0, n1, n2, n3, k1, k2, k3, k4, lvl_t, shift, v1b, &
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
    real(kind=8) :: v1b(n2 + 1:n3, n0 + 1:n2)

    real(kind=8), allocatable::b1(:, :)
    real(kind=8), allocatable::b2(:, :)
    real(kind=8), allocatable::d1(:, :, :, :)
    real(kind=8), allocatable::d2(:, :, :, :)
    real(kind=8), allocatable::f2(:, :, :, :, :, :)

    real(kind=8), allocatable::q14(:, :)
    real(kind=8), allocatable::q16(:, :)
    real(kind=8), allocatable::q18(:, :)
    real(kind=8), allocatable::q20(:, :)
    real(kind=8), allocatable::q40(:, :)
    real(kind=8), allocatable::q22(:, :)
    real(kind=8), allocatable::q24(:, :)
    real(kind=8), allocatable::q26(:, :)
    real(kind=8), allocatable::q28(:, :)
    real(kind=8), allocatable::q30(:, :)
    real(kind=8), allocatable::q32(:, :)
    real(kind=8), allocatable::q34(:, :)
    real(kind=8), allocatable::q36(:, :)
    real(kind=8), allocatable::q38(:, :)
    real(kind=8), allocatable::q42(:, :)
    real(kind=8), allocatable::z1(:, :)
    real(kind=8), allocatable::x1(:, :)
    real(kind=8), allocatable::z2(:, :)
    real(kind=8), allocatable::x2(:, :)
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

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q14(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q14)
    deallocate (d1)
    deallocate (b2)

    allocate (x1(n0 + 1:n2, n0 + 1:n2))
    x1 = 0.0d0
    x1 = x1 + q14
    deallocate (q14)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n2 - n0, n2 - n0/), '1342', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q16(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q16)
    deallocate (d1)
    deallocate (b2)

    allocate (x2(n2 + 1:n3, n2 + 1:n3))
    x2 = 0.0d0
    x2 = x2 + q16
    deallocate (q16)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q18(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q18)
    deallocate (d1)
    deallocate (b2)

    allocate (x3(n0 + 1:n1, n1 + 1:n3))
    x3 = 0.0d0
    x3 = x3 + q18
    deallocate (q18)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q20(n0 + 1:n2, n2 + 1:n3))
    i1 = k4*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q20)
    deallocate (d1)
    deallocate (b2)

    allocate (x4(n0 + 1:n2, n2 + 1:n3))
    x4 = 0.0d0
    x4 = x4 + q20

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(q20), size(q20), '21', q20, b1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q40(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, b1, b2, q40)
    deallocate (b1)
    deallocate (b2)
    deallocate (q20)

    call sum_stripe(2, shape(x1), size(x1), '21', 1.000, x1, &
                    q40)
    deallocate (q40)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q22(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, b1, b2, q22)
    deallocate (b1)
    deallocate (b2)

    call sum_stripe(2, shape(x1), size(x1), '21', 1.000, x1, &
                    q22)
    deallocate (q22)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q24(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q24)
    deallocate (d1)
    deallocate (b2)

    x1 = x1 + q24
    deallocate (q24)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q26(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q26)
    deallocate (d1)
    deallocate (b2)

    x2 = x2 + q26
    deallocate (q26)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '3421', t2b, d2)
    allocate (q28(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k3*k1*k2
    call egemm(i1, i2, i3, d1, d2, q28)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x2), size(x2), '21', -1.000, x2, &
                    q28)
    deallocate (q28)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (q30(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k3*k4*k1
    call egemm(i1, i2, i3, d1, d2, q30)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x1), size(x1), '21', 1.000, x1, &
                    q30)
    deallocate (q30)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q32(n0 + 1:n1, n1 + 1:n3))
    i1 = k3*k1
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q32)
    deallocate (d1)
    deallocate (b2)

    x3 = x3 + q32
    deallocate (q32)

  call sum_shift(2,shape(fockr),size(fockr),shape(x3), &
   size(x3),(/n0,n1/),'12',1.000,fockr,x3)

    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (z5(n2 + 1:n3, n0 + 1:n2))
    i2 = k2*k4
    i3 = k3*k1
    call egemm2(i2, i3, x3, d2, z5)
    deallocate (d2)

    v1b = v1b + z5
    deallocate (z5)
    deallocate (x3)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
    allocate (q34(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k4*k2*k2
    call egemm(i1, i2, i3, d1, d2, q34)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x2), size(x2), '21', -0.500, x2, &
                    q34)
    deallocate (q34)

  call sum_shift(2,shape(fockb),size(fockb),shape(x2), &
   size(x2),(/n2,n2/),'21',1.000,fockb,x2)

    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (z3(n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, x2, b2, z3)
    deallocate (b2)

    call sum_stripe(2, shape(v1b), size(v1b), '21', 1.000, &
                    v1b, z3)
    deallocate (z3)
    deallocate (x2)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (q36(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k4*k4*k2
    call egemm(i1, i2, i3, d1, d2, q36)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x1), size(x1), '21', 0.500, x1, &
                    q36)
    deallocate (q36)

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

  call sum_shift(2,shape(fockb),size(fockb),shape(x4), &
   size(x4),(/n0,n2/),'12',1.000,fockb,x4)

    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (z8(n2 + 1:n3, n0 + 1:n2))
    i2 = k2*k4
    i3 = k4*k2
    call egemm2(i2, i3, x4, d2, z8)
    deallocate (d2)

    v1b = v1b + z8
    deallocate (z8)
    deallocate (x4)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(q38), size(q38), '21', q38, b1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q42(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, b1, b2, q42)
    deallocate (b1)
    deallocate (b2)
    deallocate (q38)

    call sum_stripe(2, shape(x1), size(x1), '21', 1.000, x1, &
                    q42)
    deallocate (q42)

  call sum_shift(2,shape(fockb),size(fockb),shape(x1), &
   size(x1),(/n0,n0/),'12',1.000,fockb,x1)

    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (z2(n2 + 1:n3, n0 + 1:n2))
    i1 = k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, x1, b2, z2)
    deallocate (b2)

    v1b = v1b - z2
    deallocate (z2)
    deallocate (x1)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n2 - n0, n0 - n0/), '1324', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (z1(n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, z1)
    deallocate (d1)
    deallocate (b2)

    v1b = v1b + z1
    deallocate (z1)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2413', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (z4(n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, z4)
    deallocate (d1)
    deallocate (b2)

    v1b = v1b + z4
    deallocate (z4)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', intm, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '3421', t2b, d2)
    allocate (z6(n2 + 1:n3, n0 + 1:n2))
    i1 = k2
    i2 = k4
    i3 = k3*k1*k2
    call egemm(i1, i2, i3, d1, d2, z6)
    deallocate (d1)
    deallocate (d2)

    v1b = v1b - z6
    deallocate (z6)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n2 - n0/), '1432', intm, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (z7(n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2
    i3 = k3*k4*k1
    call egemm(i1, i2, i3, d1, d2, z7)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(v1b), size(v1b), '21', 1.000, &
                    v1b, z7)
    deallocate (z7)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intb, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
    allocate (z9(n2 + 1:n3, n0 + 1:n2))
    i1 = k2
    i2 = k4
    i3 = k4*k2*k2
    call egemm(i1, i2, i3, d1, d2, z9)
    deallocate (d1)
    deallocate (d2)

    v1b = v1b - 0.500*z9
    deallocate (z9)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (z10(n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2
    i3 = k4*k4*k2
    call egemm(i1, i2, i3, d1, d2, z10)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(v1b), size(v1b), '21', 0.500, &
                    v1b, z10)
    deallocate (z10)

    if (lvl_t) then
        allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
        allocate (f2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(6, shape(t3b), size(t3b), '562314', t3b, f2)
        allocate (z11(n2 + 1:n3, n0 + 1:n2))
        i2 = k2*k4
        i3 = k3*k3*k1*k1
        call egemm2(i2, i3, d1, f2, z11)
        deallocate (d1)
        deallocate (f2)

        v1b = v1b + 0.250*z11
        deallocate (z11)

        allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
        call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
        allocate (f2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(6, shape(t3c), size(t3c), '461325', t3c, f2)
        allocate (z12(n2 + 1:n3, n0 + 1:n2))
        i2 = k2*k4
        i3 = k3*k4*k1*k2
        call egemm2(i2, i3, d1, f2, z12)
        deallocate (d1)
        deallocate (f2)

        v1b = v1b + z12
        deallocate (z12)

        allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
        call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                           (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
        allocate (f2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
        call reorder_stripe(6, shape(t3d), size(t3d), '451236', t3d, f2)
        allocate (z13(n2 + 1:n3, n0 + 1:n2))
        i2 = k2*k4
        i3 = k4*k4*k2*k2
        call egemm2(i2, i3, d1, f2, z13)
        deallocate (d1)
        deallocate (f2)

        v1b = v1b + 0.250*z13
        deallocate (z13)
    end if

    do i = n0 + 1, n2
    do a = n2 + 1, n3
        coeleft = fockb(a, a) &
                  - fockb(i, i) &
                  + shift
        t1b(a, i) = t1b(a, i) - v1b(a, i)/coeleft
    end do
    end do

end subroutine t1b_update
