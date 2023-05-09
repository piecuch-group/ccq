subroutine t2a_update(n0, n1, n2, n3, k1, k2, k3, k4, lvl_t, lvl_q, shift &
                      , v2a, ext_cor, &
                      fockr, fockb, intr, intb, intm, &
                      diag1, diag2, diag3, diag4, diag5, &
                      t1diag1, t1diag2, t1diag3, t1diag4, &
                      dt3diag3, dt3diag4, &
                      t1a, t1b, &
                      t2a, t2b, &
                      t3a, t3b, &
                      t2a_mc, t2b_mc)

   implicit none

   integer, intent(in) :: n0, n1, n2, n3, k1, k2, k3, k4
   integer :: i1, i2, i3
   integer :: a, b, i, j
   logical :: lvl_t, lvl_q
   logical :: ext_cor
   real :: diag1, diag2, diag3, diag4, diag5, factor
   real :: t1diag1, t1diag2, t1diag3, t1diag4
   real :: dt3diag3, dt3diag4
   real(kind=8) :: shift, coeleft
   real(kind=8) :: fockr(n3, n3)
   real(kind=8) :: fockb(n3, n3)
   real(kind=8) :: intr(n0 + 1:n3, n0 + 1:n3, n0 + 1:n3, n0 + 1:n3)
   real(kind=8) :: intb(n0 + 1:n3, n0 + 1:n3, n0 + 1:n3, n0 + 1:n3)
   real(kind=8) :: intm(n0 + 1:n3, n0 + 1:n3, n0 + 1:n3, n0 + 1:n3)
   real(kind=8) :: t1a(n1 + 1:n3, n0 + 1:n1)
   real(kind=8) :: t1b(n2 + 1:n3, n0 + 1:n2)
   real(kind=8) :: t2a(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1)
   real(kind=8) :: t2b(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1)
   real(kind=8) :: t3a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1)
   real(kind=8) :: t3b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1)
   real(kind=8) :: t2a_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1)
   real(kind=8) :: t2b_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1)
   real(kind=8) :: v2a(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1)

   real(kind=8), allocatable :: t4a(:, :, :, :, :, :, :, :)                       !ilias: if no quadruples comment out the next 6 lines
   real(kind=8), allocatable :: t4b(:, :, :, :, :, :, :, :)
   real(kind=8), allocatable :: t4c(:, :, :, :, :, :, :, :)

   integer :: ta, tb, tc, td, te
   parameter(ta=29, tb=30, tc=31, td=32, te=33)

   real(kind=8), allocatable :: b1(:, :)
   real(kind=8), allocatable :: b1_mc(:, :)
   real(kind=8), allocatable :: b2(:, :)
   real(kind=8), allocatable :: d1(:, :, :, :)
   real(kind=8), allocatable :: d2(:, :, :, :)
   real(kind=8), allocatable :: d1_mc(:, :, :, :)
   real(kind=8), allocatable :: d2_mc(:, :, :, :)
   real(kind=8), allocatable :: f2(:, :, :, :, :, :)
   real(kind=8), allocatable :: h2(:, :, :, :, :, :, :, :)

   real(kind=8), allocatable :: s18(:, :, :, :)
   real(kind=8), allocatable :: s20(:, :, :, :)
   real(kind=8), allocatable :: s22(:, :, :, :)
   real(kind=8), allocatable :: q1(:, :)
   real(kind=8), allocatable :: q2(:, :)
   real(kind=8), allocatable :: s26(:, :, :, :)
   real(kind=8), allocatable :: s66(:, :, :, :)
   real(kind=8), allocatable :: s28(:, :, :, :)
   real(kind=8), allocatable :: q3(:, :)
   real(kind=8), allocatable :: s31(:, :, :, :)
   real(kind=8), allocatable :: s68(:, :, :, :)
   real(kind=8), allocatable :: s33(:, :, :, :)
   real(kind=8), allocatable :: q4(:, :)
   real(kind=8), allocatable :: s36(:, :, :, :)
   real(kind=8), allocatable :: s38(:, :, :, :)
   real(kind=8), allocatable :: s40(:, :, :, :)
   real(kind=8), allocatable :: q15(:, :)
   real(kind=8), allocatable :: s72(:, :, :, :)
   real(kind=8), allocatable :: s70(:, :, :, :)
   real(kind=8), allocatable :: s82(:, :, :, :)
   real(kind=8), allocatable :: s42(:, :, :, :)
   real(kind=8), allocatable :: q5(:, :)
   real(kind=8), allocatable :: q16(:, :)
   real(kind=8), allocatable :: s45(:, :, :, :)
   real(kind=8), allocatable :: q17(:, :)
   real(kind=8), allocatable :: s78(:, :, :, :)
   real(kind=8), allocatable :: s47(:, :, :, :)
   real(kind=8), allocatable :: q6(:, :)
   real(kind=8), allocatable :: q7(:, :)
   real(kind=8), allocatable :: q8(:, :)
   real(kind=8), allocatable :: q9(:, :)
   real(kind=8), allocatable :: q18(:, :)
   real(kind=8), allocatable :: q10(:, :)
   real(kind=8), allocatable :: q11(:, :)
   real(kind=8), allocatable :: q11_mc(:, :)
   real(kind=8), allocatable :: s55(:, :, :, :)
   real(kind=8), allocatable :: s55_mc(:, :, :, :)
   real(kind=8), allocatable :: s75(:, :, :, :)
   real(kind=8), allocatable :: q12(:, :)
   real(kind=8), allocatable :: q12_mc(:, :)
   real(kind=8), allocatable :: s58(:, :, :, :)
   real(kind=8), allocatable :: s58_mc(:, :, :, :)
   real(kind=8), allocatable :: q13(:, :)
   real(kind=8), allocatable :: q13_mc(:, :)
   real(kind=8), allocatable :: q14(:, :)
   real(kind=8), allocatable :: q14_mc(:, :)
   real(kind=8), allocatable :: s62(:, :, :, :)
   real(kind=8), allocatable :: s62_mc(:, :, :, :)
   real(kind=8), allocatable :: s64(:, :, :, :)
   real(kind=8), allocatable :: s64_mc(:, :, :, :)
   real(kind=8), allocatable :: x1(:, :, :, :)
   real(kind=8), allocatable :: z1(:, :, :, :)
   real(kind=8), allocatable :: z2(:, :, :, :)
   real(kind=8), allocatable :: x2(:, :)
   real(kind=8), allocatable :: z3(:, :, :, :)
   real(kind=8), allocatable :: z3_mc(:, :, :, :)
   real(kind=8), allocatable :: x3(:, :)
   real(kind=8), allocatable :: z4(:, :, :, :)
   real(kind=8), allocatable :: z4_mc(:, :, :, :)
   real(kind=8), allocatable :: x4(:, :, :, :)
   real(kind=8), allocatable :: z5(:, :, :, :)
   real(kind=8), allocatable :: z5_mc(:, :, :, :)
   real(kind=8), allocatable :: x5(:, :, :, :)
   real(kind=8), allocatable :: z6(:, :, :, :)
   real(kind=8), allocatable :: z7(:, :, :, :)
   real(kind=8), allocatable :: x6(:, :, :, :)
   real(kind=8), allocatable :: z8(:, :, :, :)
   real(kind=8), allocatable :: z8_mc(:, :, :, :)
   real(kind=8), allocatable :: x7(:, :)
   real(kind=8), allocatable :: z9(:, :, :, :)
   real(kind=8), allocatable :: x8(:, :, :, :)
   real(kind=8), allocatable :: z10(:, :, :, :)
   real(kind=8), allocatable :: z11(:, :, :, :)
   real(kind=8), allocatable :: x9(:, :)
   real(kind=8), allocatable :: z12(:, :, :, :)
   real(kind=8), allocatable :: x10(:, :, :, :)
   real(kind=8), allocatable :: z13(:, :, :, :)
   real(kind=8), allocatable :: z14(:, :, :, :)
   real(kind=8), allocatable :: z15(:, :, :, :)
   real(kind=8), allocatable :: z16(:, :, :, :)
   real(kind=8), allocatable :: z17(:, :, :, :)
   real(kind=8), allocatable :: x11(:, :, :, :)
   real(kind=8), allocatable :: z19(:, :, :, :)
   real(kind=8), allocatable :: x12(:, :, :, :)
   real(kind=8), allocatable :: z21(:, :, :, :)
   real(kind=8), allocatable :: z23(:, :, :, :)
   real(kind=8), allocatable :: z67(:, :, :, :)
   real(kind=8), allocatable :: z27(:, :, :, :)
   real(kind=8), allocatable :: z54(:, :, :, :)
   real(kind=8), allocatable :: z54_mc(:, :, :, :)
   real(kind=8), allocatable :: z59(:, :, :, :)
   real(kind=8), allocatable :: z59_mc(:, :, :, :)
   real(kind=8), allocatable :: z65(:, :, :, :)
   real(kind=8), allocatable :: z65_mc(:, :, :, :)

   factor = 0

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s18(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s18)
   deallocate (d1)
   deallocate (b2)

   allocate (x11(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x11 = 0.0d0
   call sum_stripe(4, shape(x11), size(x11), '2134', 1.000, &
                   x11, s18)
   deallocate (s18)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s20(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s20)
   deallocate (d1)
   deallocate (b2)

   allocate (x12(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x12 = 0.0d0
   call sum_stripe(4, shape(x12), size(x12), '3124', 1.000, &
                   x12, s20)
   deallocate (s20)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '3421', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s22(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k3
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s22)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s22), size(s22), '2341', s22, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (z23(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, z23)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(v2a), size(v2a), '3124', 1.000, &
                   v2a, z23)
   deallocate (z23)
   deallocate (s22)

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

   allocate (x2(n0 + 1:n1, n0 + 1:n1))
   x2 = 0.0d0
   call sum_stripe(2, shape(x2), size(x2), '21', 1.000, x2, &
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

   allocate (x3(n1 + 1:n3, n1 + 1:n3))
   x3 = 0.0d0
   call sum_stripe(2, shape(x3), size(x3), '21', -1.000, x3, &
                   q2)
   deallocate (q2)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s26(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s26)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s26), size(s26), '2314', s26, d1)
   allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
   allocate (z27(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k3 * k3
   i3 = k1 * k1
   call egemm(i1, i2, i3, d1, d2, z27)
   deallocate (d1)
   deallocate (d2)

   v2a = v2a + 0.500 * z27
   call sum_stripe(4, shape(v2a), size(v2a), '1243', -0.500, &
                   v2a, z27)
   deallocate (z27)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s26), size(s26), '3214', s26, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s66(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s66)
   deallocate (d1)
   deallocate (b2)
   deallocate (s26)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s66), size(s66), '2134', s66, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (z67(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, z67)
   deallocate (d1)
   deallocate (b2)

   v2a = v2a + z67
   call sum_stripe(4, shape(v2a), size(v2a), '1243', -1.000, &
                   v2a, z67)
   deallocate (z67)
   deallocate (s66)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2413', intr, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s28(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s28)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x12), size(x12), '2314', -1.000, &
                   x12, s28)
   deallocate (s28)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1423', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q3(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q3)
   deallocate (d1)
   deallocate (b2)

   x2 = x2 - q3
   deallocate (q3)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s31(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s31)
   deallocate (d1)
   deallocate (b2)

   allocate (x5(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x5 = 0.0d0
   call sum_stripe(4, shape(x5), size(x5), '4123', 1.000, x5, &
                   s31)

   call sum_shift(4, shape(intr), size(intr), shape(x5), &
                  size(x5), (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '3142', 1.000, intr, x5)

   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (z6(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k1 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, x5, d2, z6)
   deallocate (d2)

   call sum_stripe(4, shape(v2a), size(v2a), '2314', -1.000, &
                   v2a, z6)
   call sum_stripe(4, shape(v2a), size(v2a), '1324', 1.000, &
                   v2a, z6)
   call sum_stripe(4, shape(v2a), size(v2a), '2413', 1.000, &
                   v2a, z6)
   call sum_stripe(4, shape(v2a), size(v2a), '1423', -1.000, &
                   v2a, z6)
   deallocate (z6)
   deallocate (x5)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s31), size(s31), '3241', s31, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s68(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s68)
   deallocate (d1)
   deallocate (b2)
   deallocate (s31)

   allocate (x1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x1 = 0.0d0
   call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                   s68)
   deallocate (s68)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n1 - n0/), '4321', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (s33(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1
   i2 = k1 * k1
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, d2, s33)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '3412', 0.500, x1, &
                   s33)
   deallocate (s33)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2341', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q4(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q4)
   deallocate (d1)
   deallocate (b2)

   x3 = x3 - q4
   deallocate (q4)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (s36(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k3
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s36)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x12), size(x12), '2314', -1.000, &
                   x12, s36)
   deallocate (s36)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n1 - n0/), '3241', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s38(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s38)
   deallocate (d1)
   deallocate (b2)

   allocate (x6(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x6 = 0.0d0
   call sum_stripe(4, shape(x6), size(x6), '4123', 1.000, x6, &
                   s38)
   deallocate (s38)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3214', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s40(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s40)
   deallocate (d1)
   deallocate (b2)

   if (lvl_t) then
   if (dt3diag3 .ne. 0 .or. t1diag3 .ne. 0) then
      allocate (x8(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      x8 = 0.0d0
      if (t1diag3 .ne. 0) then
         factor = t1diag3
         call sum_stripe(4, shape(x8), size(x8), '4123', factor, &
                         x8, s40)

      end if
      if (dt3diag3 .ne. 0) then
         factor = dt3diag3
         call sum_shift(4, shape(intr), size(intr), shape(x8), &
                        size(x8), (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2143', factor, intr, x8)

      end if
      allocate (f2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(6, shape(t3a), size(t3a), '451236', t3a, f2)
      allocate (z10(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1 * k3 * k3
      i3 = k3 * k1 * k1
      call egemm(i1, i2, i3, x8, f2, z10)
      deallocate (f2)

      v2a = v2a + 0.500 * z10
      call sum_stripe(4, shape(v2a), size(v2a), '1243', -0.500, &
                      v2a, z10)
      deallocate (z10)
      deallocate (x8)
   end if
   end if

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s40), size(s40), '3421', s40, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q15(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q15)
   deallocate (d1)
   deallocate (b2)

   x2 = x2 - q15
   deallocate (q15)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s40), size(s40), '2431', s40, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s72(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s72)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x12), size(x12), '2314', -1.000, &
                   x12, s72)
   deallocate (s72)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s40), size(s40), '4231', s40, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s70(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s70)
   deallocate (d1)
   deallocate (b2)
   deallocate (s40)

   allocate (x4(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x4 = 0.0d0
   call sum_stripe(4, shape(x4), size(x4), '3124', 1.000, x4, &
                   s70)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s70), size(s70), '3214', s70, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s82(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s82)
   deallocate (d1)
   deallocate (b2)
   deallocate (s70)

   call sum_stripe(4, shape(x11), size(x11), '2134', 1.000, &
                   x11, s82)
   deallocate (s82)

   if (lvl_t) then
   if (t1diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2431', intr, d1)
      allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(6, shape(t3a), size(t3a), '412356', t3a, f2)
      allocate (s42(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1 * k1 * k3
      i3 = k3 * k3 * k1
      call egemm(i1, i2, i3, d1, f2, s42)
      deallocate (d1)
      deallocate (f2)

      factor = 0.500 * t1diag4
      call sum_stripe(4, shape(x1), size(x1), '2341', factor, &
                      x1, s42)
      deallocate (s42)
   end if
   end if

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q5(n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q5)
   deallocate (d1)
   deallocate (b2)

   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q16(n1 + 1:n3, n1 + 1:n3))
   i1 = k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, q5, b2, q16)
   deallocate (b2)
   deallocate (q5)

   if (lvl_t) then
      allocate (x7(n0 + 1:n1, n1 + 1:n3))
      x7 = 0.0d0

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
         allocate (q5(n0 + 1:n1, n1 + 1:n3))
         i1 = k3 * k1
         i3 = k3 * k1
         call egemm1(i1, i3, d1, b2, q5)
         deallocate (d1)
         deallocate (b2)

         x7 = x7 + t1diag1 * q5
         deallocate (q5)
      end if
   end if

   call sum_stripe(2, shape(x3), size(x3), '21', -1.000, x3, &
                   q16)
   deallocate (q16)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3214', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s45(n0 + 1:n1, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
   i1 = k4 * k1 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s45)
   deallocate (d1)
   deallocate (b2)

   if (lvl_t) then
   if (dt3diag3 .ne. 0 .or. t1diag3 .ne. 0) then
      allocate (x10(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
      x10 = 0.0d0
      if (t1diag3 .ne. 0) then
         factor = t1diag3
         call sum_stripe(4, shape(x10), size(x10), '4123', factor, &
                         x10, s45)

      end if
      if (dt3diag3 .ne. 0) then
         factor = dt3diag3
         call sum_shift(4, shape(intm), size(intm), shape(x10), &
                        size(x10), (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', factor, intm, &
                        x10)

      end if
      allocate (f2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(6, shape(t3b), size(t3b), '451236', t3b, f2)
      allocate (z13(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1 * k3 * k3
      i3 = k4 * k1 * k2
      call egemm(i1, i2, i3, x10, f2, z13)
      deallocate (f2)

      v2a = v2a + z13
      call sum_stripe(4, shape(v2a), size(v2a), '1243', -1.000, &
                      v2a, z13)
      deallocate (z13)
      deallocate (x10)
   end if
   end if

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s45), size(s45), '2431', s45, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q17(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q17)
   deallocate (d1)
   deallocate (b2)

   x2 = x2 + q17
   deallocate (q17)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s45), size(s45), '2431', s45, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (s78(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k3
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s78)
   deallocate (d1)
   deallocate (d2)
   deallocate (s45)

   call sum_stripe(4, shape(x12), size(x12), '2314', -1.000, &
                   x12, s78)
   deallocate (s78)

   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (z21(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, x12, b2, z21)
   deallocate (b2)

   call sum_stripe(4, shape(v2a), size(v2a), '2134', 1.000, &
                   v2a, z21)
   v2a = v2a - z21
   call sum_stripe(4, shape(v2a), size(v2a), '2143', -1.000, &
                   v2a, z21)
   call sum_stripe(4, shape(v2a), size(v2a), '1243', 1.000, &
                   v2a, z21)
   deallocate (z21)
   deallocate (x12)

   if (lvl_t) then
   if (t1diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
      allocate (f2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(6, shape(t3b), size(t3b), '412356', t3b, f2)
      allocate (s47(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1 * k1 * k3
      i3 = k3 * k4 * k2
      call egemm(i1, i2, i3, d1, f2, s47)
      deallocate (d1)
      deallocate (f2)

      factor = t1diag4
      call sum_stripe(4, shape(x1), size(x1), '2341', factor, &
                      x1, s47)
      deallocate (s47)
   end if
   end if

   call sum_shift(4, shape(intr), size(intr), shape(x1), &
                  size(x1), (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intr, x1)

   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (z1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, x1, b2, z1)
   deallocate (b2)

   call sum_stripe(4, shape(v2a), size(v2a), '2134', 1.000, &
                   v2a, z1)
   v2a = v2a - z1
   deallocate (z1)
   deallocate (x1)

   if (lvl_t) then
      allocate (x9(n0 + 1:n2, n2 + 1:n3))
      x9 = 0.0d0

      if (t1diag1 .ne. 0) then
         allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
         call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                            (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
         allocate (b2(n0 + 1:n1, n1 + 1:n3))
         call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
         allocate (q6(n0 + 1:n2, n2 + 1:n3))
         i1 = k4 * k2
         i3 = k3 * k1
         call egemm1(i1, i3, d1, b2, q6)
         deallocate (d1)
         deallocate (b2)

         x9 = x9 + t1diag1 * q6
         deallocate (q6)
      end if
   end if

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q7(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q7)
   deallocate (d1)
   deallocate (b2)

   x2 = x2 + q7
   deallocate (q7)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q8(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q8)
   deallocate (d1)
   deallocate (b2)

   x3 = x3 + q8
   deallocate (q8)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q9(n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q9)
   deallocate (d1)
   deallocate (b2)

   if (lvl_t) then
   if (t1diag1 .ne. 0) then
      x7 = x7 + t1diag1 * q9

   end if
   call sum_shift(2, shape(fockr), size(fockr), shape(x7), &
                  size(x7), (/n0, n1/), '12', 1.000, fockr, x7)

   allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '412356', t3a, f2)
   allocate (z9(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i2 = k1 * k1 * k3 * k3
   i3 = k3 * k1
   call egemm2(i2, i3, x7, f2, z9)
   deallocate (f2)

   v2a = v2a + z9
   deallocate (z9)
   deallocate (x7)
   end if

   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q18(n1 + 1:n3, n1 + 1:n3))
   i1 = k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, q9, b2, q18)
   deallocate (b2)
   deallocate (q9)

   call sum_stripe(2, shape(x3), size(x3), '21', -1.000, x3, &
                   q18)
   deallocate (q18)

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
      allocate (q10(n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i3 = k4 * k2
      call egemm1(i1, i3, d1, b2, q10)
      deallocate (d1)
      deallocate (b2)

      x9 = x9 + t1diag1 * q10
      deallocate (q10)

   end if
   call sum_shift(2, shape(fockb), size(fockb), shape(x9), &
                  size(x9), (/n0, n2/), '12', 1.000, fockb, x9)

   allocate (f2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '412356', t3b, f2)
   allocate (z12(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i2 = k1 * k1 * k3 * k3
   i3 = k4 * k2
   call egemm2(i2, i3, x9, f2, z12)
   deallocate (f2)

   v2a = v2a + z12
   deallocate (z12)
   deallocate (x9)
   end if

   if (diag3 .ne. 0) then
      allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))                !ilias: acp d3
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2134', intr, d1)
      allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
      call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
      allocate (q11(n1 + 1:n3, n1 + 1:n3))
      i1 = k3
      i2 = k3
      i3 = k3 * k1 * k1
      call egemm(i1, i2, i3, d1, d2, q11)
      deallocate (d1)
      deallocate (d2)

      allocate (b1(n1 + 1:n3, n1 + 1:n3))
      call reorder_stripe(2, shape(q11), size(q11), '21', q11, b1)
      allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
      allocate (z54(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3
      i2 = k1 * k1 * k3
      i3 = k3
      call egemm(i1, i2, i3, b1, d2, z54)
      deallocate (b1)
      deallocate (d2)

      factor = -0.500 * diag3
      call sum_stripe(4, shape(v2a), size(v2a), '1342', factor, &
                      v2a, z54)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '2341', factor, &
                      v2a, z54)
      deallocate (z54)
      deallocate (q11)
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
      allocate (q11_mc(n1 + 1:n3, n1 + 1:n3))
      i1 = k3
      i2 = k3
      i3 = k3 * k1 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, q11_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (b1_mc(n1 + 1:n3, n1 + 1:n3))
      call reorder_stripe(2, shape(q11_mc), size(q11_mc), '21', q11_mc, &
                          b1_mc)
      allocate (d2_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '1234', t2a_mc, &
                          d2_mc)
      allocate (z54_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3
      i2 = k1 * k1 * k3
      i3 = k3
      call egemm(i1, i2, i3, b1_mc, d2_mc, z54_mc)
      deallocate (q11_mc)
      deallocate (b1_mc)
      deallocate (d2_mc)

      factor = -0.500 * (1.0 - diag3)
      call sum_stripe(4, shape(v2a), size(v2a), '1342', factor, &
                      v2a, z54_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '2341', factor, &
                      v2a, z54_mc)
      deallocate (z54_mc)
      factor = 0
   end if

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (s55(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k1
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, d2, s55)
   deallocate (d1)
   deallocate (d2)

   if (diag5 .ne. 0) then
      factor = 0.500 * diag5
      call sum_stripe(4, shape(x4), size(x4), '3412', factor, &
                      x4, s55)          !ilias: acp d5
      factor = 0
!
   end if
   if (ext_cor .and. diag5 .ne. 1) then
      allocate (d1_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_shift(4, shape(intr), size(intr), shape(d1_mc), &
                         size(d1_mc), (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1_mc)
      allocate (d2_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '1234', t2a_mc, &
                          d2_mc)
      allocate (s55_mc(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
      i1 = k1 * k1
      i2 = k1 * k1
      i3 = k3 * k3
      call egemm(i1, i2, i3, d1_mc, d2_mc, s55_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(4, shape(s55_mc), size(s55_mc), '3412', s55_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3412', t2a_mc, &
                          d2_mc)
      allocate (z5_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      i1 = k1 * k1
      i2 = k3 * k3
      i3 = k1 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, z5_mc)
      deallocate (s55_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      v2a = v2a + 0.250 * (1.0 - diag5) * z5_mc
      deallocate (z5_mc)
      factor = 0
   end if
   call sum_shift(4, shape(intr), size(intr), shape(x4), &
                  size(x4), (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intr, x4)

   allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
   allocate (z5(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k3 * k3
   i3 = k1 * k1
   call egemm(i1, i2, i3, x4, d2, z5)
   deallocate (d2)

   v2a = v2a + 0.500 * z5
   deallocate (z5)
   deallocate (x4)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s55), size(s55), '4312', s55, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s75(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s75)
   deallocate (d1)
   deallocate (b2)
   deallocate (s55)

   call sum_stripe(4, shape(x11), size(x11), '2134', 0.500, &
                   x11, s75)
   deallocate (s75)

   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (z19(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, x11, b2, z19)
   deallocate (b2)

   v2a = v2a + z19
   deallocate (z19)
   deallocate (x11)

   if (diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))                 !ilias: acp d4
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2431', intr, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
      allocate (q12(n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1
      i3 = k3 * k3 * k1
      call egemm(i1, i2, i3, d1, d2, q12)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * diag4
      call sum_stripe(2, shape(x2), size(x2), '21', factor, x2, &
                      q12)
      deallocate (q12)
      factor = 0
!
   end if
   if (ext_cor .and. diag4 .ne. 1) then
      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_shift(4, shape(intr), size(intr), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2431', intr, d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                          d2_mc)
      allocate (q12_mc(n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1
      i3 = k3 * k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, q12_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (b1_mc(n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(2, shape(q12_mc), size(q12_mc), '21', q12_mc, &
                          b1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                          d2_mc)
      allocate (z3_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k3 * k3 * k1
      i3 = k1
      call egemm(i1, i2, i3, b1_mc, d2_mc, z3_mc)
      deallocate (q12_mc)
      deallocate (b1_mc)
      deallocate (d2_mc)

      factor = 0.500 * (1.0 - diag4)
      v2a = v2a + factor * z3_mc
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1243', factor, &
                      v2a, z3_mc)
      deallocate (z3_mc)
      factor = 0
   end if

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
      allocate (s58(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s58)
      deallocate (d1)
      deallocate (d2)

      allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))                 !ilias: acp d1
      call reorder_stripe(4, shape(s58), size(s58), '3412', s58, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
      allocate (z59(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, z59)
      deallocate (d1)
      deallocate (d2)

      factor = -diag1
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, &
                      v2a, z59)               !ilias: acp d1
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1324', factor, &
                      v2a, z59)
      deallocate (z59)
      deallocate (s58)
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
      allocate (s58_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, s58_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(s58_mc), size(s58_mc), '3412', s58_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                          d2_mc)
      allocate (z59_mc(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, z59_mc)
      deallocate (s58_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -(1.0 - diag1)
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, &
                      v2a, z59_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1324', factor, &
                      v2a, z59_mc)
      deallocate (z59_mc)
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
      allocate (s58_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, s58_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(s58_mc), size(s58_mc), '3412', s58_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                          d2_mc)
      allocate (z59_mc(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, z59_mc)
      deallocate (s58_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -diag1
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, &
                      v2a, z59_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1324', factor, &
                      v2a, z59_mc)
      deallocate (z59_mc)
      factor = 0

      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intm, d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                          d2_mc)
      allocate (s58_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, s58_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(s58_mc), size(s58_mc), '3412', s58_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                          d2_mc)
      allocate (z59_mc(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, z59_mc)
      deallocate (s58_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = diag1
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, &
                      v2a, z59_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1324', factor, &
                      v2a, z59_mc)
      deallocate (z59_mc)
      factor = 0
   end if

   if (diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))                 !ilias: acp d3
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
      allocate (q13(n1 + 1:n3, n1 + 1:n3))
      i1 = k3
      i2 = k3
      i3 = k4 * k1 * k2
      call egemm(i1, i2, i3, d1, d2, q13)
      deallocate (d1)
      deallocate (d2)

      factor = -diag3
      call sum_stripe(2, shape(x3), size(x3), '21', factor, x3, &
                      q13)
      deallocate (q13)
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
      allocate (q13_mc(n1 + 1:n3, n1 + 1:n3))
      i1 = k3
      i2 = k3
      i3 = k4 * k1 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, q13_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (b1_mc(n1 + 1:n3, n1 + 1:n3))
      call reorder_stripe(2, shape(q13_mc), size(q13_mc), '21', q13_mc, &
                          b1_mc)
      allocate (d2_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '1234', t2a_mc, &
                          d2_mc)
      allocate (z4_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3
      i2 = k1 * k1 * k3
      i3 = k3
      call egemm(i1, i2, i3, b1_mc, d2_mc, z4_mc)
      deallocate (q13_mc)
      deallocate (b1_mc)
      deallocate (d2_mc)

      factor = -(1.0 - diag3)
      call sum_stripe(4, shape(v2a), size(v2a), '2341', factor, &
                      v2a, z4_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1342', factor, &
                      v2a, z4_mc)
      factor = 0
      deallocate (z4_mc)
   end if

   call sum_shift(2, shape(fockr), size(fockr), shape(x3), &
                  size(x3), (/n1, n1/), '21', 1.000, fockr, x3)

   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (z4(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   i1 = k3
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, x3, d2, z4)
   deallocate (d2)

   call sum_stripe(4, shape(v2a), size(v2a), '2341', 1.000, &
                   v2a, z4)
   call sum_stripe(4, shape(v2a), size(v2a), '1342', -1.000, &
                   v2a, z4)
   deallocate (z4)
   deallocate (x3)

   if (diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))                 !ilias: acp d4
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
      allocate (q14(n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1
      i3 = k3 * k4 * k2
      call egemm(i1, i2, i3, d1, d2, q14)
      deallocate (d1)
      deallocate (d2)

      factor = diag4
      call sum_stripe(2, shape(x2), size(x2), '21', factor, x2, &
                      q14)
      deallocate (q14)
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
      allocate (q14_mc(n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1
      i3 = k3 * k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, q14_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (b1_mc(n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(2, shape(q14_mc), size(q14_mc), '21', q14_mc, &
                          b1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a_mc), size(t2a_mc), '3124', t2a_mc, &
                          d2_mc)
      allocate (z3_mc(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1 * k3 * k3
      i3 = k1
      call egemm(i1, i2, i3, b1_mc, d2_mc, z3_mc)
      deallocate (q14_mc)
      deallocate (b1_mc)
      deallocate (d2_mc)

      factor = (1.0 - diag4)
      v2a = v2a + factor * z3_mc
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1243', factor, &
                      v2a, z3_mc)
      deallocate (z3_mc)
      factor = 0
   end if

   call sum_shift(2, shape(fockr), size(fockr), shape(x2), &
                  size(x2), (/n0, n0/), '12', 1.000, fockr, x2)

   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (z3(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, x2, d2, z3)
   deallocate (d2)

   v2a = v2a + z3
   call sum_stripe(4, shape(v2a), size(v2a), '1243', -1.000, &
                   v2a, z3)
   deallocate (z3)
   deallocate (x2)

   if (diag1 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: acp d1
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
      allocate (s62(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s62)
      deallocate (d1)
      deallocate (d2)

      factor = diag1
      call sum_stripe(4, shape(x6), size(x6), '3412', factor, &
                      x6, s62)           !ilias: acp d1
      deallocate (s62)
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
      allocate (s62_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, s62_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(s62_mc), size(s62_mc), '3412', s62_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                          d2_mc)
      allocate (z8_mc(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z8_mc)
      deallocate (s62_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -(1.0 - diag1)
      call sum_stripe(4, shape(v2a), size(v2a), '2314', factor, &
                      v2a, z8_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1324', factor, &
                      v2a, z8_mc)
      call sum_stripe(4, shape(v2a), size(v2a), '2413', factor, &
                      v2a, z8_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, &
                      v2a, z8_mc)
      deallocate (z8_mc)
      factor = 0
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x6), &
                  size(x6), (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '3142', 1.000, intm, x6)

   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (z8(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k1 * k3
   i3 = k4 * k2
   call egemm(i1, i2, i3, x6, d2, z8)
   deallocate (d2)

   call sum_stripe(4, shape(v2a), size(v2a), '2314', -1.000, &
                   v2a, z8)
   call sum_stripe(4, shape(v2a), size(v2a), '1324', 1.000, &
                   v2a, z8)
   call sum_stripe(4, shape(v2a), size(v2a), '2413', 1.000, &
                   v2a, z8)
   call sum_stripe(4, shape(v2a), size(v2a), '1423', -1.000, &
                   v2a, z8)
   deallocate (z8)
   deallocate (x6)

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
      allocate (s64(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s64)
      deallocate (d1)
      deallocate (d2)

      allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))                 !ilias: acp d1
      call reorder_stripe(4, shape(s64), size(s64), '3412', s64, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
      allocate (z65(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, z65)
      deallocate (d1)
      deallocate (d2)

      factor = -diag1 !ilias: acp d1
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, v2a, z65)
      call sum_stripe(4, shape(v2a), size(v2a), '1324', -factor, v2a, z65)
      deallocate (z65)
      deallocate (s64)
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
      allocate (s64_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(s64_mc), size(s64_mc), '3412', s64_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                          d2_mc)
      allocate (z65_mc(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z65_mc)
      deallocate (s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -(1.0 - diag1)
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, &
                      v2a, z65_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1324', factor, &
                      v2a, z65_mc)
      deallocate (z65_mc)
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
      allocate (s64_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(s64_mc), size(s64_mc), '3412', s64_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                          d2_mc)
      allocate (z65_mc(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z65_mc)
      deallocate (s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -diag1
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, &
                      v2a, z65_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1324', factor, &
                      v2a, z65_mc)
      deallocate (z65_mc)
      factor = 0

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intm, d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                          d2_mc)
      allocate (s64_mc(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(s64_mc), size(s64_mc), '3412', s64_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '3124', t2b_mc, &
                          d2_mc)
      allocate (z65_mc(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
      i1 = k1 * k3
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z65_mc)
      deallocate (s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = diag1
      call sum_stripe(4, shape(v2a), size(v2a), '1423', factor, &
                      v2a, z65_mc)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1324', factor, &
                      v2a, z65_mc)
      deallocate (z65_mc)
      factor = 0
   end if

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (z2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, z2)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(v2a), size(v2a), '3124', 1.000, &
                   v2a, z2)
   call sum_stripe(4, shape(v2a), size(v2a), '4123', -1.000, &
                   v2a, z2)
   deallocate (z2)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '4321', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (z7(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i2 = k1 * k1
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, d2, z7)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(v2a), size(v2a), '3412', 0.500, &
                   v2a, z7)
   deallocate (z7)

   if (lvl_t) then
   if (dt3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2431', intr, d1)
      allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(6, shape(t3a), size(t3a), '412356', t3a, f2)
      allocate (z11(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3
      i2 = k1 * k1 * k3
      i3 = k3 * k3 * k1
      call egemm(i1, i2, i3, d1, f2, z11)
      deallocate (d1)
      deallocate (f2)

      factor = 0.500 * dt3diag4
      call sum_stripe(4, shape(v2a), size(v2a), '2341', factor, &
                      v2a, z11)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1342', factor, &
                      v2a, z11)
      deallocate (z11)

      allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
      allocate (f2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(6, shape(t3b), size(t3b), '412356', t3b, f2)
      allocate (z14(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3
      i2 = k1 * k1 * k3
      i3 = k3 * k4 * k2
      call egemm(i1, i2, i3, d1, f2, z14)
      deallocate (d1)
      deallocate (f2)

      factor = dt3diag4
      call sum_stripe(4, shape(v2a), size(v2a), '2341', factor, &
                      v2a, z14)
      factor = -factor
      call sum_stripe(4, shape(v2a), size(v2a), '1342', factor, &
                      v2a, z14)
      deallocate (z14)
   end if
   end if

   if (lvl_q) then
      allocate (t4a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 21 lines
                    n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
      rewind (ta)
      read (ta) t4a
      allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
      allocate (h2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                   n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(8, shape(t4a), size(t4a), '56123478', t4a, h2)
      allocate (z15(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      i2 = k1 * k1 * k3 * k3
      i3 = k3 * k3 * k1 * k1
      call egemm2(i2, i3, d1, h2, z15)
      deallocate (d1)
      deallocate (h2)
      deallocate (t4a)

      v2a = v2a + 0.250 * z15
      deallocate (z15)
      allocate (t4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 21 lines
                    n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
      rewind (tb)
      read (tb) t4b
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
      allocate (h2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                   n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(8, shape(t4b), size(t4b), '56123478', t4b, h2)
      allocate (z16(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      i2 = k1 * k1 * k3 * k3
      i3 = k3 * k4 * k1 * k2
      call egemm2(i2, i3, d1, h2, z16)
      deallocate (d1)
      deallocate (h2)
      deallocate (t4b)

      v2a = v2a + z16
      deallocate (z16)
      allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the next 20 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      rewind (tc)
      read (tc) t4c
      allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
      allocate (h2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                   n0 + 1:n1, n0 + 1:n1))
      call reorder_stripe(8, shape(t4c), size(t4c), '56123478', t4c, h2)
      allocate (z17(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      i2 = k1 * k1 * k3 * k3
      i3 = k4 * k4 * k2 * k2
      call egemm2(i2, i3, d1, h2, z17)
      deallocate (d1)
      deallocate (h2)

      v2a = v2a + 0.250 * z17
      deallocate (z17)
   end if

   do i = n0 + 1, n1 - 1
   do j = i + 1, n1
   do a = n1 + 1, n3 - 1
   do b = a + 1, n3
      coeleft = fockr(b, b) &
                + fockr(a, a) &
                - fockr(j, j) &
                - fockr(i, i) &
                + shift
      t2a(b, a, j, i) = t2a(b, a, j, i) - v2a(b, a, j, i) / coeleft
      t2a(b, a, i, j) = -t2a(b, a, j, i)
      t2a(a, b, j, i) = -t2a(b, a, j, i)
      t2a(a, b, i, j) = t2a(b, a, j, i)
   end do
   end do
   end do
   end do

end subroutine t2a_update
