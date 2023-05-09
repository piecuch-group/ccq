subroutine t2c_update(n0, n1, n2, n3, k1, k2, k3, k4, lvl_t, lvl_q, shift, &
                      v2c, ext_cor, &
                      fockr, fockb, intr, intb, intm, &
                      diag1, diag2, diag3, diag4, diag5, &
                      t1diag1, t1diag2, t1diag3, t1diag4, &
                      dt3diag3, dt3diag4, &
                      t1a, t1b, &
                      t2b, t2c, &
                      t3c, t3d, &
                      t2b_mc, t2c_mc)

   implicit none

   integer, intent(in) :: n0, n1, n2, n3, k1, k2, k3, k4
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
   real(kind=8) :: t2b(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1)
   real(kind=8) :: t2c(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2)
   real(kind=8) :: t3c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1)
   real(kind=8) :: t3d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2)
   real(kind=8) :: t2b_mc(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1)
   real(kind=8) :: t2c_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2)
   real(kind=8) :: v2c(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2)

   real(kind=8), allocatable :: t4c(:, :, :, :, :, :, :, :)                     !ilias: if no quadruples comment out the following 3 lines
   real(kind=8), allocatable :: t4d(:, :, :, :, :, :, :, :)
   real(kind=8), allocatable :: t4e(:, :, :, :, :, :, :, :)

   real(kind=8), allocatable :: b1(:, :)
   real(kind=8), allocatable :: b1_mc(:, :)
   real(kind=8), allocatable :: b2(:, :)
   real(kind=8), allocatable :: d1(:, :, :, :)
   real(kind=8), allocatable :: d1_mc(:, :, :, :)
   real(kind=8), allocatable :: d2(:, :, :, :)
   real(kind=8), allocatable :: d2_mc(:, :, :, :)
   real(kind=8), allocatable :: f2(:, :, :, :, :, :)
   real(kind=8), allocatable :: h2(:, :, :, :, :, :, :, :)

   integer :: ta, tb, tc, td, te                                       !ilias: if no quadruples comment out the following 2 lines
   parameter(ta=29, tb=30, tc=31, td=32, te=33)

   integer :: i1, i2, i3

   real(kind=8), allocatable :: q1(:, :)
   real(kind=8), allocatable :: q2(:, :)
   real(kind=8), allocatable :: q3(:, :)
   real(kind=8), allocatable :: q4(:, :)
   real(kind=8), allocatable :: q16(:, :)
   real(kind=8), allocatable :: q15(:, :)
   real(kind=8), allocatable :: s22(:, :, :, :)
   real(kind=8), allocatable :: s24(:, :, :, :)
   real(kind=8), allocatable :: s26(:, :, :, :)
   real(kind=8), allocatable :: s28(:, :, :, :)
   real(kind=8), allocatable :: s30(:, :, :, :)
   real(kind=8), allocatable :: q5(:, :)
   real(kind=8), allocatable :: q6(:, :)
   real(kind=8), allocatable :: s34(:, :, :, :)
   real(kind=8), allocatable :: s68(:, :, :, :)
   real(kind=8), allocatable :: s36(:, :, :, :)
   real(kind=8), allocatable :: q7(:, :)
   real(kind=8), allocatable :: s39(:, :, :, :)
   real(kind=8), allocatable :: s70(:, :, :, :)
   real(kind=8), allocatable :: s41(:, :, :, :)
   real(kind=8), allocatable :: q8(:, :)
   real(kind=8), allocatable :: s44(:, :, :, :)
   real(kind=8), allocatable :: s72(:, :, :, :)
   real(kind=8), allocatable :: s46(:, :, :, :)
   real(kind=8), allocatable :: q9(:, :)
   real(kind=8), allocatable :: s49(:, :, :, :)
   real(kind=8), allocatable :: q17(:, :)
   real(kind=8), allocatable :: s76(:, :, :, :)
   real(kind=8), allocatable :: s74(:, :, :, :)
   real(kind=8), allocatable :: s82(:, :, :, :)
   real(kind=8), allocatable :: s51(:, :, :, :)
   real(kind=8), allocatable :: q10(:, :)
   real(kind=8), allocatable :: q18(:, :)
   real(kind=8), allocatable :: s54(:, :, :, :)
   real(kind=8), allocatable :: s54_mc(:, :, :, :)
   real(kind=8), allocatable :: s56(:, :, :, :)
   real(kind=8), allocatable :: s56_mc(:, :, :, :)
   real(kind=8), allocatable :: q11(:, :)
   real(kind=8), allocatable :: q11_mc(:, :)
   real(kind=8), allocatable :: q12(:, :)
   real(kind=8), allocatable :: q12_mc(:, :)
   real(kind=8), allocatable :: q13(:, :)
   real(kind=8), allocatable :: q13_mc(:, :)
   real(kind=8), allocatable :: s61(:, :, :, :)
   real(kind=8), allocatable :: s61_mc(:, :, :, :)
   real(kind=8), allocatable :: s79(:, :, :, :)
   real(kind=8), allocatable :: q14(:, :)
   real(kind=8), allocatable :: q14_mc(:, :)
   real(kind=8), allocatable :: s64(:, :, :, :)
   real(kind=8), allocatable :: s64_mc(:, :, :, :)
   real(kind=8), allocatable :: x1(:, :, :, :)
   real(kind=8), allocatable :: z1(:, :, :, :)
   real(kind=8), allocatable :: z2(:, :, :, :)
   real(kind=8), allocatable :: x2(:, :, :, :)
   real(kind=8), allocatable :: z3(:, :, :, :)
   real(kind=8), allocatable :: z3_mc(:, :, :, :)
   real(kind=8), allocatable :: x3(:, :)
   real(kind=8), allocatable :: z4(:, :, :, :)
   real(kind=8), allocatable :: z4_mc(:, :, :, :)
   real(kind=8), allocatable :: x4(:, :)
   real(kind=8), allocatable :: z5(:, :, :, :)
   real(kind=8), allocatable :: z5_mc(:, :, :, :)
   real(kind=8), allocatable :: x5(:, :, :, :)
   real(kind=8), allocatable :: z6(:, :, :, :)
   real(kind=8), allocatable :: z6_mc(:, :, :, :)
   real(kind=8), allocatable :: x6(:, :, :, :)
   real(kind=8), allocatable :: z7(:, :, :, :)
   real(kind=8), allocatable :: z7_mc(:, :, :, :)
   real(kind=8), allocatable :: z8(:, :, :, :)
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
   real(kind=8), allocatable :: z23(:, :, :, :)
   real(kind=8), allocatable :: x12(:, :, :, :)
   real(kind=8), allocatable :: z25(:, :, :, :)
   real(kind=8), allocatable :: z27(:, :, :, :)
   real(kind=8), allocatable :: x13(:, :, :, :)
   real(kind=8), allocatable :: z29(:, :, :, :)
   real(kind=8), allocatable :: z69(:, :, :, :)
   real(kind=8), allocatable :: z35(:, :, :, :)
   real(kind=8), allocatable :: z47(:, :, :, :)
   real(kind=8), allocatable :: z60(:, :, :, :)
   real(kind=8), allocatable :: z60_mc(:, :, :, :)
   real(kind=8), allocatable :: z65(:, :, :, :)
   real(kind=8), allocatable :: z65_mc(:, :, :, :)

   factor = 0

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q1(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q1)
   deallocate (d1)
   deallocate (b2)

   allocate (x3(n0 + 1:n2, n0 + 1:n2))
   x3 = 0.0d0
   x3 = x3 + q1
   deallocate (q1)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n2 - n0, n2 - n0/), '1342', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q2(n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q2)
   deallocate (d1)
   deallocate (b2)

   allocate (x4(n2 + 1:n3, n2 + 1:n3))
   x4 = 0.0d0
   x4 = x4 + q2
   deallocate (q2)

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
         allocate (q3(n0 + 1:n1, n1 + 1:n3))
         i1 = k3 * k1
         i3 = k3 * k1
         call egemm1(i1, i3, d1, b2, q3)
         deallocate (d1)
         deallocate (b2)

         x7 = x7 + t1diag1 * q3
         deallocate (q3)
      end if
   end if

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q4(n0 + 1:n2, n2 + 1:n3))
   i1 = k4 * k2
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q4)
   deallocate (d1)
   deallocate (b2)

   if (lvl_t) then
      allocate (x9(n0 + 1:n2, n2 + 1:n3))
      x9 = 0.0d0
      if (t1diag1 .ne. 0) then
         x9 = x9 + t1diag1 * q4
      end if
   end if

   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q16(n2 + 1:n3, n2 + 1:n3))
   i1 = k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, q4, b2, q16)
   deallocate (b2)

   call sum_stripe(2, shape(x4), size(x4), '21', -1.000, x4, &
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

   call sum_stripe(2, shape(x3), size(x3), '21', 1.000, x3, &
                   q15)
   deallocate (q15)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s22(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s22)
   deallocate (d1)
   deallocate (b2)

   allocate (x11(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   x11 = 0.0d0
   call sum_stripe(4, shape(x11), size(x11), '2134', 1.000, &
                   x11, s22)
   deallocate (s22)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4213', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s24(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s24)
   deallocate (d1)
   deallocate (b2)

   allocate (x12(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   x12 = 0.0d0
   call sum_stripe(4, shape(x12), size(x12), '3124', 1.000, &
                   x12, s24)
   deallocate (s24)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n2 - n0, n2 - n0/), '3421', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s26(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4 * k4
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s26)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s26), size(s26), '2341', s26, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (z27(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k4
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, z27)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(v2c), size(v2c), '3124', 1.000, &
                   v2c, z27)
   deallocate (z27)
   deallocate (s26)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
   allocate (s28(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k2 * k4
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s28)
   deallocate (d1)
   deallocate (d2)

   allocate (x13(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   x13 = 0.0d0
   call sum_stripe(4, shape(x13), size(x13), '2314', 1.000, &
                   x13, s28)
   deallocate (s28)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n2 - n0/), '4132', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s30(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
   i1 = k4 * k3 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s30)
   deallocate (d1)
   deallocate (b2)

   allocate (x2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x2 = 0.0d0
   call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                   s30)
   deallocate (s30)

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

   call sum_stripe(2, shape(x3), size(x3), '21', 1.000, x3, &
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

   call sum_stripe(2, shape(x4), size(x4), '21', -1.000, x4, &
                   q6)
   deallocate (q6)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s34(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s34)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s34), size(s34), '2314', s34, d1)
   allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
   allocate (z35(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k4 * k4
   i3 = k2 * k2
   call egemm(i1, i2, i3, d1, d2, z35)
   deallocate (d1)
   deallocate (d2)

   v2c = v2c + 0.500 * z35
   call sum_stripe(4, shape(v2c), size(v2c), '1243', -0.500, &
                   v2c, z35)
   deallocate (z35)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s34), size(s34), '3214', s34, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s68(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s68)
   deallocate (d1)
   deallocate (b2)
   deallocate (s34)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s68), size(s68), '2134', s68, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (z69(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, z69)
   deallocate (d1)
   deallocate (b2)

   v2c = v2c + z69
   call sum_stripe(4, shape(v2c), size(v2c), '1243', -1.000, &
                   v2c, z69)
   deallocate (z69)
   deallocate (s68)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intb, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
   allocate (s36(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k2 * k4
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s36)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x12), size(x12), '2314', -1.000, &
                   x12, s36)
   deallocate (s36)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1423', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q7(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q7)
   deallocate (d1)
   deallocate (b2)

   x3 = x3 - q7
   deallocate (q7)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s39(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s39)
   deallocate (d1)
   deallocate (b2)

   allocate (x6(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x6 = 0.0d0
   call sum_stripe(4, shape(x6), size(x6), '4123', 1.000, x6, &
                   s39)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s39), size(s39), '3241', s39, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s70(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s70)
   deallocate (d1)
   deallocate (b2)
   deallocate (s39)

   allocate (x1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   x1 = 0.0d0
   call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                   s70)
   deallocate (s70)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n0 - n0, n2 - n0/), '4321', intb, d1)
   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (s41(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   i1 = k4 * k2
   i2 = k2 * k2
   i3 = k4 * k4
   call egemm(i1, i2, i3, d1, d2, s41)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '3412', 0.500, x1, &
                   s41)
   deallocate (s41)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2341', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q8(n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q8)
   deallocate (d1)
   deallocate (b2)

   x4 = x4 - q8
   deallocate (q8)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4213', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s44(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s44)
   deallocate (d1)
   deallocate (b2)

   if (lvl_t) then
   if (dt3diag3 .ne. 0 .or. t1diag3 .ne. 0) then
      allocate (x8(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
      x8 = 0.0d0

      if (t1diag3 .ne. 0) then
         factor = t1diag3
         call sum_stripe(4, shape(x8), size(x8), '4123', factor, &
                         x8, s44)

      end if
      if (dt3diag3 .ne. 0) then
         factor = dt3diag3
         call sum_shift(4, shape(intm), size(intm), shape(x8), &
                        size(x8), (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', factor, intm, x8)

      end if
      allocate (f2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(6, shape(t3c), size(t3c), '463125', t3c, f2)
      allocate (z10(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2 * k4 * k4
      i3 = k3 * k1 * k2
      call egemm(i1, i2, i3, x8, f2, z10)
      deallocate (f2)

      v2c = v2c + z10
      call sum_stripe(4, shape(v2c), size(v2c), '1243', -1.000, &
                      v2c, z10)
      deallocate (z10)
      deallocate (x8)
   end if
   end if

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s44), size(s44), '3421', s44, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
   allocate (s72(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k2 * k4
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s72)
   deallocate (d1)
   deallocate (d2)
   deallocate (s44)

   call sum_stripe(4, shape(x13), size(x13), '2314', 1.000, &
                   x13, s72)
   deallocate (s72)

   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (z29(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, x13, b2, z29)
   deallocate (b2)

   v2c = v2c + z29
   call sum_stripe(4, shape(v2c), size(v2c), '2134', -1.000, &
                   v2c, z29)
   call sum_stripe(4, shape(v2c), size(v2c), '1243', -1.000, &
                   v2c, z29)
   call sum_stripe(4, shape(v2c), size(v2c), '2143', 1.000, &
                   v2c, z29)
   deallocate (z29)
   deallocate (x13)

   if (lvl_t) then
   if (t1diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
      allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(6, shape(t3c), size(t3c), '613245', t3c, f2)
      allocate (s46(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2 * k2 * k4
      i3 = k3 * k4 * k1
      call egemm(i1, i2, i3, d1, f2, s46)
      deallocate (d1)
      deallocate (f2)

      allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(4, shape(s46), size(s46), '4123', s46, d1)
      allocate (b2(n0 + 1:n2, n2 + 1:n3))
      call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
      allocate (z47(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i1 = k2 * k2 * k4
      i2 = k4
      i3 = k2
      call egemm(i1, i2, i3, d1, b2, z47)
      deallocate (d1)
      deallocate (b2)

      v2c = v2c - t1diag4 * z47
      factor = t1diag4
      call sum_stripe(4, shape(v2c), size(v2c), '2134', factor, &
                      v2c, z47)
      deallocate (z47)
      deallocate (s46)

   end if
   if (t1diag1 .ne. 0) then
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

      x7 = x7 + t1diag1 * q9
      deallocate (q9)

   end if
   call sum_shift(2, shape(fockr), size(fockr), shape(x7), &
                  size(x7), (/n0, n1/), '12', 1.000, fockr, x7)

   allocate (f2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(6, shape(t3c), size(t3c), '631245', t3c, f2)
   allocate (z9(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i2 = k2 * k2 * k4 * k4
   i3 = k3 * k1
   call egemm2(i2, i3, x7, f2, z9)
   deallocate (f2)

   v2c = v2c + z9
   deallocate (z9)
   deallocate (x7)
   end if

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '3214', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s49(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   i1 = k4 * k2 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s49)
   deallocate (d1)
   deallocate (b2)

   if (lvl_t) then
   if (dt3diag3 .ne. 0 .or. t1diag3 .ne. 0) then
      allocate (x10(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      x10 = 0.0d0

      if (t1diag3 .ne. 0) then
         factor = t1diag3
         call sum_stripe(4, shape(x10), size(x10), '4123', factor, &
                         x10, s49)

      end if
      if (dt3diag3 .ne. 0) then
         factor = dt3diag3
         call sum_shift(4, shape(intb), size(intb), shape(x10), &
                        size(x10), (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', factor, intb, &
                        x10)

      end if
      allocate (f2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(6, shape(t3d), size(t3d), '451236', t3d, f2)
      allocate (z13(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2 * k4 * k4
      i3 = k4 * k2 * k2
      call egemm(i1, i2, i3, x10, f2, z13)
      deallocate (f2)

      v2c = v2c + 0.500 * z13
      call sum_stripe(4, shape(v2c), size(v2c), '1243', -0.500, &
                      v2c, z13)
      deallocate (z13)
      deallocate (x10)
   end if
   end if

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s49), size(s49), '3421', s49, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q17(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q17)
   deallocate (d1)
   deallocate (b2)

   x3 = x3 - q17
   deallocate (q17)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s49), size(s49), '2431', s49, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
   allocate (s76(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k2 * k4
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s76)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x12), size(x12), '2314', -1.000, &
                   x12, s76)
   deallocate (s76)

   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (z25(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, x12, b2, z25)
   deallocate (b2)

   call sum_stripe(4, shape(v2c), size(v2c), '2134', 1.000, &
                   v2c, z25)
   v2c = v2c - z25
   call sum_stripe(4, shape(v2c), size(v2c), '2143', -1.000, &
                   v2c, z25)
   call sum_stripe(4, shape(v2c), size(v2c), '1243', 1.000, &
                   v2c, z25)
   deallocate (z25)
   deallocate (x12)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s49), size(s49), '4231', s49, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s74(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s74)
   deallocate (d1)
   deallocate (b2)
   deallocate (s49)

   allocate (x5(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   x5 = 0.0d0
   call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                   s74)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s74), size(s74), '3214', s74, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s82(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s82)
   deallocate (d1)
   deallocate (b2)
   deallocate (s74)

   call sum_stripe(4, shape(x11), size(x11), '2134', 1.000, &
                   x11, s82)
   deallocate (s82)

   if (lvl_t) then
   if (t1diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1)
      allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(6, shape(t3d), size(t3d), '412356', t3d, f2)
      allocate (s51(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2 * k2 * k4
      i3 = k4 * k4 * k2
      call egemm(i1, i2, i3, d1, f2, s51)
      deallocate (d1)
      deallocate (f2)

      factor = 0.500 * t1diag4
      call sum_stripe(4, shape(x1), size(x1), '2341', factor, &
                      x1, s51)
      deallocate (s51)
   end if
   end if

   call sum_shift(4, shape(intb), size(intb), shape(x1), &
                  size(x1), (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intb, x1)

   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (z1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, x1, b2, z1)
   deallocate (b2)

   call sum_stripe(4, shape(v2c), size(v2c), '2134', 1.000, &
                   v2c, z1)
   v2c = v2c - z1
   deallocate (z1)
   deallocate (x1)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q10(n0 + 1:n2, n2 + 1:n3))
   i1 = k4 * k2
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q10)
   deallocate (d1)
   deallocate (b2)

   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q18(n2 + 1:n3, n2 + 1:n3))
   i1 = k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, q10, b2, q18)
   deallocate (b2)
   deallocate (q10)

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

   allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(6, shape(t3d), size(t3d), '412356', t3d, f2)
   allocate (z12(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i2 = k2 * k2 * k4 * k4
   i3 = k4 * k2
   call egemm2(i2, i3, x9, f2, z12)
   deallocate (f2)

   v2c = v2c + z12
   deallocate (z12)
   deallocate (x9)
   end if

   call sum_stripe(2, shape(x4), size(x4), '21', -1.000, x4, &
                   q18)
   deallocate (q18)

   if (diag1 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: acp d1
      if (diag2 .eq. 0) then
         call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                            (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intm, d1)
      else
         call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                            (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
      end if
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
      allocate (s54(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s54)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * diag1
      call sum_stripe(4, shape(x2), size(x2), '3412', factor, &
                      x2, s54)
      deallocate (s54)
      factor = 0
!
   end if
   if (ext_cor .and. diag1 .ne. 1) then
      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
      call reorder_shift(4, shape(intr), size(intr), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                          d2_mc)
      allocate (s54_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, s54_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(s54_mc), size(s54_mc), '3412', s54_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                          d2_mc)
      allocate (z3_mc(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, z3_mc)
      deallocate (s54_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -(1.0 - diag1)
      call sum_stripe(4, shape(v2c), size(v2c), '2314', factor, &
                      v2c, z3_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '2413', factor, &
                      v2c, z3_mc)
      deallocate (z3_mc)
      factor = 0
   end if
!
   if (ext_cor .and. diag1 .eq. 1 .and. diag2 .eq. 0) then
      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
      call reorder_shift(4, shape(intr), size(intr), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                          d2_mc)
      allocate (s54_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, s54_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(s54_mc), size(s54_mc), '3412', s54_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                          d2_mc)
      allocate (z3_mc(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, z3_mc)
      deallocate (s54_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -diag1
      call sum_stripe(4, shape(v2c), size(v2c), '2314', factor, &
                      v2c, z3_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '2413', factor, &
                      v2c, z3_mc)
      deallocate (z3_mc)
      factor = 0

      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intm, d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                          d2_mc)
      allocate (s54_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, s54_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(s54_mc), size(s54_mc), '3412', s54_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                          d2_mc)
      allocate (z3_mc(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, z3_mc)
      deallocate (s54_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = diag1
      call sum_stripe(4, shape(v2c), size(v2c), '2314', factor, &
                      v2c, z3_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '2413', factor, &
                      v2c, z3_mc)
      deallocate (z3_mc)
      factor = 0
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x2), &
                  size(x2), (/n0 - n0, n1 - n0, n2 - n0, n0 - n0/), '1324', 1.000, intm, x2)

   allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
   allocate (z3(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k2 * k4
   i3 = k3 * k1
   call egemm(i1, i2, i3, x2, d2, z3)
   deallocate (d2)

   call sum_stripe(4, shape(v2c), size(v2c), '2314', -1.000, &
                   v2c, z3)
   call sum_stripe(4, shape(v2c), size(v2c), '1324', 1.000, &
                   v2c, z3)
   call sum_stripe(4, shape(v2c), size(v2c), '2413', 1.000, &
                   v2c, z3)
   call sum_stripe(4, shape(v2c), size(v2c), '1423', -1.000, &
                   v2c, z3)
   deallocate (z3)
   deallocate (x2)

   if (diag1 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: acp d1
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
      allocate (s56(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s56)
      deallocate (d1)
      deallocate (d2)

      factor = diag1
      call sum_stripe(4, shape(x6), size(x6), '3412', factor, &
                      x6, s56)
      deallocate (s56)
      factor = 0
!
   end if
   if (ext_cor .and. diag1 .ne. 1) then
      allocate (d1_mc(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1_mc)
      allocate (d2_mc(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b_mc), size(t2b_mc), '4213', t2b_mc, &
                          d2_mc)
      allocate (s56_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, s56_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(s56_mc), size(s56_mc), '3421', s56_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (z7_mc(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z7_mc)
      deallocate (s56_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -(1.0 - diag1)
      call sum_stripe(4, shape(v2c), size(v2c), '2314', factor, &
                      v2c, z7_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1324', factor, &
                      v2c, z7_mc)
      call sum_stripe(4, shape(v2c), size(v2c), '2413', factor, &
                      v2c, z7_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1423', factor, &
                      v2c, z7_mc)
      deallocate (z7_mc)
      factor = 0
   end if

   call sum_shift(4, shape(intb), size(intb), shape(x6), &
                  size(x6), (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '3142', 1.000, intb, x6)

   allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
   allocate (z7(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k2 * k4
   i3 = k4 * k2
   call egemm(i1, i2, i3, x6, d2, z7)
   deallocate (d2)

   call sum_stripe(4, shape(v2c), size(v2c), '2314', -1.000, &
                   v2c, z7)
   call sum_stripe(4, shape(v2c), size(v2c), '1324', 1.000, &
                   v2c, z7)
   call sum_stripe(4, shape(v2c), size(v2c), '2413', 1.000, &
                   v2c, z7)
   call sum_stripe(4, shape(v2c), size(v2c), '1423', -1.000, &
                   v2c, z7)
   deallocate (z7)
   deallocate (x6)

   if (diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))                 !ilias:acp d4
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
      allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
      allocate (q11(n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2
      i3 = k3 * k4 * k1
      call egemm(i1, i2, i3, d1, d2, q11)
      deallocate (d1)
      deallocate (d2)

      factor = diag4
      call sum_stripe(2, shape(x3), size(x3), '21', factor, x3, &
                      q11)
      deallocate (q11)
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
      allocate (q11_mc(n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2
      i3 = k3 * k4 * k1
      call egemm(i1, i2, i3, d1_mc, d2_mc, q11_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (b1_mc(n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(2, shape(q11_mc), size(q11_mc), '21', q11_mc, &
                          b1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (z4_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2 * k4 * k4
      i3 = k2
      call egemm(i1, i2, i3, b1_mc, d2_mc, z4_mc)
      deallocate (q11_mc)
      deallocate (b1_mc)
      deallocate (d2_mc)

      factor = (1.0 - diag4)
      v2c = v2c + factor * z4_mc
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1243', factor, &
                      v2c, z4_mc)
      deallocate (z4_mc)
      factor = 0
   end if

   if (diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))                 !ilias: acp d3
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
      call reorder_stripe(4, shape(t2b), size(t2b), '3421', t2b, d2)
      allocate (q12(n2 + 1:n3, n2 + 1:n3))
      i1 = k4
      i2 = k4
      i3 = k3 * k1 * k2
      call egemm(i1, i2, i3, d1, d2, q12)
      deallocate (d1)
      deallocate (d2)

      factor = -diag3
      call sum_stripe(2, shape(x4), size(x4), '21', factor, x4, &
                      q12)
      deallocate (q12)
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
      allocate (q12_mc(n2 + 1:n3, n2 + 1:n3))
      i1 = k4
      i2 = k4
      i3 = k3 * k1 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, q12_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (b1_mc(n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(2, shape(q12_mc), size(q12_mc), '21', q12_mc, &
                          b1_mc)
      allocate (d2_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '1234', t2c_mc, &
                          d2_mc)
      allocate (z5_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4
      i2 = k2 * k2 * k4
      i3 = k4
      call egemm(i1, i2, i3, b1_mc, d2_mc, z5_mc)
      deallocate (q12_mc)
      deallocate (b1_mc)
      deallocate (d2_mc)

      factor = -(1.0 - diag3)
      call sum_stripe(4, shape(v2c), size(v2c), '2341', factor, &
                      v2c, z5_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1342', factor, &
                      v2c, z5_mc)
      deallocate (z5_mc)
      factor = 0
   end if

   call sum_shift(2, shape(fockb), size(fockb), shape(x4), &
                  size(x4), (/n2, n2/), '21', 1.000, fockb, x4)

   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (z5(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   i1 = k4
   i2 = k2 * k2 * k4
   i3 = k4
   call egemm(i1, i2, i3, x4, d2, z5)
   deallocate (d2)

   call sum_stripe(4, shape(v2c), size(v2c), '2341', 1.000, &
                   v2c, z5)
   call sum_stripe(4, shape(v2c), size(v2c), '1342', -1.000, &
                   v2c, z5)
   deallocate (z5)
   deallocate (x4)

   if (diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))                !ilias: acp d3
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2134', intb, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
      allocate (q13(n2 + 1:n3, n2 + 1:n3))
      i1 = k4
      i2 = k4
      i3 = k4 * k2 * k2
      call egemm(i1, i2, i3, d1, d2, q13)
      deallocate (d1)
      deallocate (d2)

      allocate (b1(n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(2, shape(q13), size(q13), '21', q13, b1)
      allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
      allocate (z60(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4
      i2 = k2 * k2 * k4
      i3 = k4
      call egemm(i1, i2, i3, b1, d2, z60)
      deallocate (b1)
      deallocate (d2)

      factor = -0.500 * diag3
      call sum_stripe(4, shape(v2c), size(v2c), '1342', factor, &
                      v2c, z60)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '2341', factor, &
                      v2c, z60)
      deallocate (z60)
      deallocate (q13)
      factor = 0
!
   end if
   if (ext_cor .and. diag3 .ne. 1) then
      allocate (d1_mc(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_shift(4, shape(intb), size(intb), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2134', intb, d1_mc)
      allocate (d2_mc(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3412', t2c_mc, &
                          d2_mc)
      allocate (q13_mc(n2 + 1:n3, n2 + 1:n3))
      i1 = k4
      i2 = k4
      i3 = k4 * k2 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, q13_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (b1_mc(n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(2, shape(q13_mc), size(q13_mc), '21', q13_mc, &
                          b1_mc)
      allocate (d2_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '1234', t2c_mc, &
                          d2_mc)
      allocate (z60_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4
      i2 = k2 * k2 * k4
      i3 = k4
      call egemm(i1, i2, i3, b1_mc, d2_mc, z60_mc)
      deallocate (q13_mc)
      deallocate (b1_mc)
      deallocate (d2_mc)

      factor = -0.500 * (1.0 - diag3)
      call sum_stripe(4, shape(v2c), size(v2c), '1342', factor, &
                      v2c, z60_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '2341', factor, &
                      v2c, z60_mc)
      deallocate (z60_mc)
      factor = 0
   end if

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (s61(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k2 * k2
   i3 = k4 * k4
   call egemm(i1, i2, i3, d1, d2, s61)
   deallocate (d1)
   deallocate (d2)

   if (diag5 .ne. 0) then
      factor = 0.500 * diag5
      call sum_stripe(4, shape(x5), size(x5), '3412', factor, &
                      x5, s61)          !ilias: acp d5
      factor = 0
!
   end if
   if (ext_cor .and. diag5 .ne. 1) then
      allocate (d1_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1_mc), &
                         size(d1_mc), (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1_mc)
      allocate (d2_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '1234', t2c_mc, &
                          d2_mc)
      allocate (s61_mc(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      i1 = k2 * k2
      i2 = k2 * k2
      i3 = k4 * k4
      call egemm(i1, i2, i3, d1_mc, d2_mc, s61_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(4, shape(s61_mc), size(s61_mc), '3412', s61_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3412', t2c_mc, &
                          d2_mc)
      allocate (z6_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i1 = k2 * k2
      i2 = k4 * k4
      i3 = k2 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z6_mc)
      deallocate (s61_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = 0.250 * (1.0 - diag5)
      v2c = v2c + factor * z6_mc
      deallocate (z6_mc)
      factor = 0
   end if

   call sum_shift(4, shape(intb), size(intb), shape(x5), &
                  size(x5), (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intb, x5)

   allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
   allocate (z6(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k4 * k4
   i3 = k2 * k2
   call egemm(i1, i2, i3, x5, d2, z6)
   deallocate (d2)

   v2c = v2c + 0.500 * z6
   deallocate (z6)
   deallocate (x5)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s61), size(s61), '4312', s61, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s79(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s79)
   deallocate (d1)
   deallocate (b2)
   deallocate (s61)

   call sum_stripe(4, shape(x11), size(x11), '2134', 0.500, &
                   x11, s79)
   deallocate (s79)

   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (z23(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, x11, b2, z23)
   deallocate (b2)

   v2c = v2c + z23
   deallocate (z23)
   deallocate (x11)

   if (diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))                 !ilias: acp d4
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (q14(n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2
      i3 = k4 * k4 * k2
      call egemm(i1, i2, i3, d1, d2, q14)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * diag4
      call sum_stripe(2, shape(x3), size(x3), '21', factor, x3, &
                      q14)
      deallocate (q14)
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
      allocate (q14_mc(n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2
      i3 = k4 * k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, q14_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (b1_mc(n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(2, shape(q14_mc), size(q14_mc), '21', q14_mc, &
                          b1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (z4_mc(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2 * k4 * k4
      i3 = k2
      call egemm(i1, i2, i3, b1_mc, d2_mc, z4_mc)
      deallocate (q14_mc)
      deallocate (b1_mc)
      deallocate (d2_mc)

      factor = 0.500 * (1.0 - diag4)
      v2c = v2c + factor * z4_mc
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1243', factor, &
                      v2c, z4_mc)
      deallocate (z4_mc)
      factor = 0
   end if

   call sum_shift(2, shape(fockb), size(fockb), shape(x3), &
                  size(x3), (/n0, n0/), '12', 1.000, fockb, x3)

   allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
   allocate (z4(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2
   i2 = k2 * k4 * k4
   i3 = k2
   call egemm(i1, i2, i3, x3, d2, z4)
   deallocate (d2)

   v2c = v2c + z4
   call sum_stripe(4, shape(v2c), size(v2c), '1243', -1.000, &
                   v2c, z4)
   deallocate (z4)
   deallocate (x3)

   if (diag1 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: acp d1
      if (diag2 .eq. 0) then
         call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                            (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
      else
         call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                            (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
      end if
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (s64(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s64)
      deallocate (d1)
      deallocate (d2)

      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))                 !ilias: acp d1
      call reorder_stripe(4, shape(s64), size(s64), '3412', s64, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (z65(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, z65)
      deallocate (d1)
      deallocate (d2)

      factor = -diag1
      call sum_stripe(4, shape(v2c), size(v2c), '1423', factor, &
                      v2c, z65)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1324', factor, &
                      v2c, z65)
      deallocate (z65)
      deallocate (s64)
      factor = 0
!
   end if
   if (ext_cor .and. diag1 .ne. 1) then
      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
      call reorder_shift(4, shape(intb), size(intb), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (s64_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(s64_mc), size(s64_mc), '3412', s64_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (z65_mc(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z65_mc)
      deallocate (s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -(1.0 - diag1)
      call sum_stripe(4, shape(v2c), size(v2c), '1423', factor, &
                      v2c, z65_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1324', factor, &
                      v2c, z65_mc)
      deallocate (z65_mc)
      factor = 0
   end if
!
   if (ext_cor .and. diag1 .eq. 1 .and. diag2 .eq. 0) then
      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
      call reorder_shift(4, shape(intb), size(intb), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (s64_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(s64_mc), size(s64_mc), '3412', s64_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (z65_mc(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z65_mc)
      deallocate (s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = -diag1
      call sum_stripe(4, shape(v2c), size(v2c), '1423', factor, &
                      v2c, z65_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1324', factor, &
                      v2c, z65_mc)
      deallocate (z65_mc)
      factor = 0

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1_mc), &
                         size(d1_mc), (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intm, d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (s64_mc(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      allocate (d1_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(s64_mc), size(s64_mc), '3412', s64_mc, &
                          d1_mc)
      allocate (d2_mc(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c_mc), size(t2c_mc), '3124', t2c_mc, &
                          d2_mc)
      allocate (z65_mc(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1_mc, d2_mc, z65_mc)
      deallocate (s64_mc)
      deallocate (d1_mc)
      deallocate (d2_mc)

      factor = diag1
      call sum_stripe(4, shape(v2c), size(v2c), '1423', factor, &
                      v2c, z65_mc)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1324', factor, &
                      v2c, z65_mc)
      deallocate (z65_mc)
      factor = 0
   end if

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '4213', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (z2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k4
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, z2)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(v2c), size(v2c), '3124', 1.000, &
                   v2c, z2)
   call sum_stripe(4, shape(v2c), size(v2c), '4123', -1.000, &
                   v2c, z2)
   deallocate (z2)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n2 - n0, n2 - n0/), '4321', intb, d1)
   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (z8(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i2 = k2 * k2
   i3 = k4 * k4
   call egemm(i1, i2, i3, d1, d2, z8)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(v2c), size(v2c), '3412', 0.500, &
                   v2c, z8)
   deallocate (z8)

   if (lvl_t) then
   if (dt3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n2 - n0/), '1432', intm, d1)
      allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(6, shape(t3c), size(t3c), '613245', t3c, f2)
      allocate (z11(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4
      i2 = k2 * k2 * k4
      i3 = k3 * k4 * k1
      call egemm(i1, i2, i3, d1, f2, z11)
      deallocate (d1)
      deallocate (f2)

      factor = dt3diag4
      call sum_stripe(4, shape(v2c), size(v2c), '2341', factor, &
                      v2c, z11)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1342', factor, &
                      v2c, z11)
      deallocate (z11)

      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
      allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(6, shape(t3d), size(t3d), '412356', t3d, f2)
      allocate (z14(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4
      i2 = k2 * k2 * k4
      i3 = k4 * k4 * k2
      call egemm(i1, i2, i3, d1, f2, z14)
      deallocate (d1)
      deallocate (f2)

      factor = 0.500 * dt3diag4
      call sum_stripe(4, shape(v2c), size(v2c), '2341', factor, &
                      v2c, z14)
      factor = -factor
      call sum_stripe(4, shape(v2c), size(v2c), '1342', factor, &
                      v2c, z14)
      deallocate (z14)
   end if
   end if

   if (lvl_q) then
      allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 21 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      rewind (tc)
      read (tc) t4c
      allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
      allocate (h2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, &
                   n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(8, shape(t4c), size(t4c), '78341256', t4c, h2)
      allocate (z15(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i2 = k2 * k2 * k4 * k4
      i3 = k3 * k3 * k1 * k1
      call egemm2(i2, i3, d1, h2, z15)
      deallocate (d1)
      deallocate (h2)
      deallocate (t4c)

      v2c = v2c + 0.250 * z15
      deallocate (z15)

      allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 21 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
      rewind (td)
      read (td) t4d
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
      allocate (h2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, &
                   n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(8, shape(t4d), size(t4d), '58142367', t4d, h2)
      allocate (z16(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i2 = k2 * k2 * k4 * k4
      i3 = k3 * k4 * k1 * k2
      call egemm2(i2, i3, d1, h2, z16)
      deallocate (d1)
      deallocate (h2)
      deallocate (t4d)

      v2c = v2c + z16
      deallocate (z16)

      allocate (t4e(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, & !ilias: if no quadruples comment out the following 21 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      rewind (te)
      read (te) t4e
      allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
      allocate (h2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, &
                   n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(8, shape(t4e), size(t4e), '56123478', t4e, h2)
      allocate (z17(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      i2 = k2 * k2 * k4 * k4
      i3 = k4 * k4 * k2 * k2
      call egemm2(i2, i3, d1, h2, z17)
      deallocate (d1)
      deallocate (h2)
      deallocate (t4e)

      v2c = v2c + 0.250 * z17
      deallocate (z17)

   end if
   do i = n0 + 1, n2 - 1
   do j = i + 1, n2
   do a = n2 + 1, n3 - 1
   do b = a + 1, n3
      coeleft = fockb(b, b) &
                + fockb(a, a) &
                - fockb(j, j) &
                - fockb(i, i) &
                + shift
      t2c(b, a, j, i) = t2c(b, a, j, i) - v2c(b, a, j, i) / coeleft
      t2c(b, a, i, j) = -t2c(b, a, j, i)
      t2c(a, b, j, i) = -t2c(b, a, j, i)
      t2c(a, b, i, j) = t2c(b, a, j, i)
   end do
   end do
   end do
   end do

end subroutine t2c_update

