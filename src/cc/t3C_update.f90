subroutine t3c_update(n0, n1, n2, n3, k1, k2, k3, k4, lvl_q, shift, v3c, &
                      fockr, fockb, intr, intb, intm, &
                      t1a, t1b, &
                      t2a, t2b, t2c, &
                      t3b, t3c, t3d, &
                      iactocca, iactoccb, iactunoa, iactunob, iactindt, &
                      t2diag3, t2diag4, t2diag5, t3diag1, t3diag2, t3diag3, t3diag4, t3diag5)

   implicit none

   integer, intent(in) :: n0, n1, n2, n3, k1, k2, k3, k4

   integer :: a, b, c, e, f, i, j, k, m, n
   integer :: i3
   integer :: iactocca, iactoccb, iactunoa, iactunob, iactindt
   integer :: iocca, ioccb, iunoa, iunob
   integer, allocatable :: indocc(:, :, :)
   integer, allocatable :: indunocc(:, :, :)
   logical :: lvl_q
   real :: t2diag3, t2diag4, t2diag5
   real :: t3diag1, t3diag2, t3diag3, t3diag4, t3diag5
   real :: factor
   real(kind=8) :: shift, coeleft
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
   real(kind=8) :: t3b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1)
   real(kind=8) :: t3c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1)
   real(kind=8) :: t3d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2)
   real(kind=8) :: v3c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1)

   real(kind=8), allocatable :: t4c(:, :, :, :, :, :, :, :)                     !ilias: if no quadruples comment out the following 2 lines
   real(kind=8), allocatable :: t4d(:, :, :, :, :, :, :, :)

   integer :: ta, tb, tc, td, te                                       !ilias: if no quadruples comment out the following 2 lines
   parameter(ta=29, tb=30, tc=31, td=32, te=33)

   integer :: i1, i2

   real(kind=8), allocatable :: b1(:, :)
   real(kind=8), allocatable :: b2(:, :)
   real(kind=8), allocatable :: d1(:, :, :, :)
   real(kind=8), allocatable :: d2(:, :, :, :)
   real(kind=8), allocatable :: f2(:, :, :, :, :, :)

   real(kind=8), allocatable :: s1(:, :, :, :)
   real(kind=8), allocatable :: s2(:, :, :, :)
   real(kind=8), allocatable :: s3(:, :, :, :)
   real(kind=8), allocatable :: s4(:, :, :, :)
   real(kind=8), allocatable :: s5(:, :, :, :)
   real(kind=8), allocatable :: s6(:, :, :, :)
   real(kind=8), allocatable :: q1(:, :)
   real(kind=8), allocatable :: q2(:, :)
   real(kind=8), allocatable :: s7(:, :, :, :)
   real(kind=8), allocatable :: q3(:, :)
   real(kind=8), allocatable :: s8(:, :, :, :)
   real(kind=8), allocatable :: q4(:, :)
   real(kind=8), allocatable :: s9(:, :, :, :)
   real(kind=8), allocatable :: s98(:, :, :, :)
   real(kind=8), allocatable :: s94(:, :, :, :)
   real(kind=8), allocatable :: s10(:, :, :, :)
   real(kind=8), allocatable :: s99(:, :, :, :)
   real(kind=8), allocatable :: s95(:, :, :, :)
   real(kind=8), allocatable :: s11(:, :, :, :)
   real(kind=8), allocatable :: s100(:, :, :, :)
   real(kind=8), allocatable :: q5(:, :)
   real(kind=8), allocatable :: s12(:, :, :, :)
   real(kind=8), allocatable :: s101(:, :, :, :)
   real(kind=8), allocatable :: q6(:, :)
   real(kind=8), allocatable :: s13(:, :, :, :)
   real(kind=8), allocatable :: s103(:, :, :, :)
   real(kind=8), allocatable :: s102(:, :, :, :)
   real(kind=8), allocatable :: s14(:, :, :, :)
   real(kind=8), allocatable :: s105(:, :, :, :)
   real(kind=8), allocatable :: s104(:, :, :, :)
   real(kind=8), allocatable :: s15(:, :, :, :)
   real(kind=8), allocatable :: s110(:, :, :, :)
   real(kind=8), allocatable :: q25(:, :)
   real(kind=8), allocatable :: s96(:, :, :, :)
   real(kind=8), allocatable :: s16(:, :, :, :)
   real(kind=8), allocatable :: s111(:, :, :, :)
   real(kind=8), allocatable :: q26(:, :)
   real(kind=8), allocatable :: q7(:, :)
   real(kind=8), allocatable :: s112(:, :, :, :)
   real(kind=8), allocatable :: s17(:, :, :, :)
   real(kind=8), allocatable :: s115(:, :, :, :)
   real(kind=8), allocatable :: s114(:, :, :, :)
   real(kind=8), allocatable :: s113(:, :, :, :)
   real(kind=8), allocatable :: q27(:, :)
   real(kind=8), allocatable :: s107(:, :, :, :)
   real(kind=8), allocatable :: s106(:, :, :, :)
   real(kind=8), allocatable :: s150(:, :, :, :)
   real(kind=8), allocatable :: s97(:, :, :, :)
   real(kind=8), allocatable :: s149(:, :, :, :)
   real(kind=8), allocatable :: s148(:, :, :, :)
   real(kind=8), allocatable :: s18(:, :, :, :)
   real(kind=8), allocatable :: s119(:, :, :, :)
   real(kind=8), allocatable :: s117(:, :, :, :)
   real(kind=8), allocatable :: s116(:, :, :, :)
   real(kind=8), allocatable :: q30(:, :)
   real(kind=8), allocatable :: s109(:, :, :, :)
   real(kind=8), allocatable :: s108(:, :, :, :)
   real(kind=8), allocatable :: s151(:, :, :, :)
   real(kind=8), allocatable :: q8(:, :)
   real(kind=8), allocatable :: s120(:, :, :, :)
   real(kind=8), allocatable :: s118(:, :, :, :)
   real(kind=8), allocatable :: q29(:, :)
   real(kind=8), allocatable :: q28(:, :)
   real(kind=8), allocatable :: s19(:, :, :, :)
   real(kind=8), allocatable :: s20(:, :, :, :)
   real(kind=8), allocatable :: s21(:, :, :, :)
   real(kind=8), allocatable :: s22(:, :, :, :)
   real(kind=8), allocatable :: s23(:, :, :, :)
   real(kind=8), allocatable :: s24(:, :, :, :)
   real(kind=8), allocatable :: s25(:, :, :, :)
   real(kind=8), allocatable :: s26(:, :, :, :)
   real(kind=8), allocatable :: s27(:, :, :, :)
   real(kind=8), allocatable :: s28(:, :, :, :)
   real(kind=8), allocatable :: s29(:, :, :, :)
   real(kind=8), allocatable :: s30(:, :, :, :)
   real(kind=8), allocatable :: q9(:, :)
   real(kind=8), allocatable :: q10(:, :)
   real(kind=8), allocatable :: s31(:, :, :, :)
   real(kind=8), allocatable :: s121(:, :, :, :)
   real(kind=8), allocatable :: s32(:, :, :, :)
   real(kind=8), allocatable :: q11(:, :)
   real(kind=8), allocatable :: s33(:, :, :, :)
   real(kind=8), allocatable :: s122(:, :, :, :)
   real(kind=8), allocatable :: s34(:, :, :, :)
   real(kind=8), allocatable :: q12(:, :)
   real(kind=8), allocatable :: s35(:, :, :, :)
   real(kind=8), allocatable :: s123(:, :, :, :)
   real(kind=8), allocatable :: s36(:, :, :, :)
   real(kind=8), allocatable :: s124(:, :, :, :)
   real(kind=8), allocatable :: q13(:, :)
   real(kind=8), allocatable :: s37(:, :, :, :)
   real(kind=8), allocatable :: s126(:, :, :, :)
   real(kind=8), allocatable :: s125(:, :, :, :)
   real(kind=8), allocatable :: s38(:, :, :, :)
   real(kind=8), allocatable :: q14(:, :)
   real(kind=8), allocatable :: s39(:, :, :, :)
   real(kind=8), allocatable :: s135(:, :, :, :)
   real(kind=8), allocatable :: s134(:, :, :, :)
   real(kind=8), allocatable :: s133(:, :, :, :)
   real(kind=8), allocatable :: s131(:, :, :, :)
   real(kind=8), allocatable :: s127(:, :, :, :)
   real(kind=8), allocatable :: s40(:, :, :, :)
   real(kind=8), allocatable :: s138(:, :, :, :)
   real(kind=8), allocatable :: s137(:, :, :, :)
   real(kind=8), allocatable :: s136(:, :, :, :)
   real(kind=8), allocatable :: s132(:, :, :, :)
   real(kind=8), allocatable :: q15(:, :)
   real(kind=8), allocatable :: s139(:, :, :, :)
   real(kind=8), allocatable :: s41(:, :, :, :)
   real(kind=8), allocatable :: s142(:, :, :, :)
   real(kind=8), allocatable :: s141(:, :, :, :)
   real(kind=8), allocatable :: s140(:, :, :, :)
   real(kind=8), allocatable :: q31(:, :)
   real(kind=8), allocatable :: s129(:, :, :, :)
   real(kind=8), allocatable :: s153(:, :, :, :)
   real(kind=8), allocatable :: s128(:, :, :, :)
   real(kind=8), allocatable :: s152(:, :, :, :)
   real(kind=8), allocatable :: s42(:, :, :, :)
   real(kind=8), allocatable :: s146(:, :, :, :)
   real(kind=8), allocatable :: s145(:, :, :, :)
   real(kind=8), allocatable :: s143(:, :, :, :)
   real(kind=8), allocatable :: q32(:, :)
   real(kind=8), allocatable :: s130(:, :, :, :)
   real(kind=8), allocatable :: q16(:, :)
   real(kind=8), allocatable :: s147(:, :, :, :)
   real(kind=8), allocatable :: s144(:, :, :, :)
   real(kind=8), allocatable :: s43(:, :, :, :)
   real(kind=8), allocatable :: s44(:, :, :, :)
   real(kind=8), allocatable :: s45(:, :, :, :)
   real(kind=8), allocatable :: q17(:, :)
   real(kind=8), allocatable :: q18(:, :)
   real(kind=8), allocatable :: s46(:, :, :, :)
   real(kind=8), allocatable :: s47(:, :, :, :)
   real(kind=8), allocatable :: s48(:, :, :, :)
   real(kind=8), allocatable :: s49(:, :, :, :)
   real(kind=8), allocatable :: s50(:, :, :, :)
   real(kind=8), allocatable :: s51(:, :, :, :)
   real(kind=8), allocatable :: s52(:, :, :, :)
   real(kind=8), allocatable :: s53(:, :, :, :)
   real(kind=8), allocatable :: s54(:, :, :, :)
   real(kind=8), allocatable :: s55(:, :, :, :)
   real(kind=8), allocatable :: s56(:, :, :, :)
   real(kind=8), allocatable :: s57(:, :, :, :)
   real(kind=8), allocatable :: s58(:, :, :, :)
   real(kind=8), allocatable :: s59(:, :, :, :)
   real(kind=8), allocatable :: s60(:, :, :, :)
   real(kind=8), allocatable :: s61(:, :, :, :)
   real(kind=8), allocatable :: s62(:, :, :, :)
   real(kind=8), allocatable :: s63(:, :, :, :)
   real(kind=8), allocatable :: s64(:, :, :, :)
   real(kind=8), allocatable :: s65(:, :, :, :)
   real(kind=8), allocatable :: s66(:, :, :, :)
   real(kind=8), allocatable :: s67(:, :, :, :)
   real(kind=8), allocatable :: s68(:, :, :, :)
   real(kind=8), allocatable :: s69(:, :, :, :)
   real(kind=8), allocatable :: s70(:, :, :, :)
   real(kind=8), allocatable :: s71(:, :, :, :)
   real(kind=8), allocatable :: s72(:, :, :, :)
   real(kind=8), allocatable :: s73(:, :, :, :)
   real(kind=8), allocatable :: s74(:, :, :, :)
   real(kind=8), allocatable :: s75(:, :, :, :)
   real(kind=8), allocatable :: s76(:, :, :, :)
   real(kind=8), allocatable :: s77(:, :, :, :)
   real(kind=8), allocatable :: s78(:, :, :, :)
   real(kind=8), allocatable :: q19(:, :)
   real(kind=8), allocatable :: s79(:, :, :, :)
   real(kind=8), allocatable :: s80(:, :, :, :)
   real(kind=8), allocatable :: s81(:, :, :, :)
   real(kind=8), allocatable :: q20(:, :)
   real(kind=8), allocatable :: s82(:, :, :, :)
   real(kind=8), allocatable :: q21(:, :)
   real(kind=8), allocatable :: q22(:, :)
   real(kind=8), allocatable :: s83(:, :, :, :)
   real(kind=8), allocatable :: s84(:, :, :, :)
   real(kind=8), allocatable :: s85(:, :, :, :)
   real(kind=8), allocatable :: s86(:, :, :, :)
   real(kind=8), allocatable :: s87(:, :, :, :)
   real(kind=8), allocatable :: s88(:, :, :, :)
   real(kind=8), allocatable :: s89(:, :, :, :)
   real(kind=8), allocatable :: s90(:, :, :, :)
   real(kind=8), allocatable :: s91(:, :, :, :)
   real(kind=8), allocatable :: s92(:, :, :, :)
   real(kind=8), allocatable :: q23(:, :)
   real(kind=8), allocatable :: s93(:, :, :, :)
   real(kind=8), allocatable :: q24(:, :)
   real(kind=8), allocatable :: x1(:, :, :, :)
   real(kind=8), allocatable :: x2(:, :, :, :)
   real(kind=8), allocatable :: x3(:, :, :, :)
   real(kind=8), allocatable :: x4(:, :, :, :)
   real(kind=8), allocatable :: x5(:, :, :, :)
   real(kind=8), allocatable :: x6(:, :, :, :)
   real(kind=8), allocatable :: x7(:, :, :, :)
   real(kind=8), allocatable :: x8(:, :)
   real(kind=8), allocatable :: x9(:, :)
   real(kind=8), allocatable :: x10(:, :)
   real(kind=8), allocatable :: x11(:, :)
   real(kind=8), allocatable :: x12(:, :, :, :)
   real(kind=8), allocatable :: x13(:, :, :, :)
   real(kind=8), allocatable :: x14(:, :, :, :)
   real(kind=8), allocatable :: x15(:, :, :, :)
   real(kind=8), allocatable :: x16(:, :, :, :)
   real(kind=8), allocatable :: x17(:, :, :, :)
   real(kind=8), allocatable :: x18(:, :, :, :)
   real(kind=8), allocatable :: x19(:, :, :, :)
   real(kind=8), allocatable :: x20(:, :, :, :)
   real(kind=8), allocatable :: x21(:, :)
   real(kind=8), allocatable :: x22(:, :, :, :)
   real(kind=8), allocatable :: x23(:, :, :, :)
   real(kind=8), allocatable :: x24(:, :, :, :)
   real(kind=8), allocatable :: x25(:, :, :, :)
   real(kind=8), allocatable :: x26(:, :)
   real(kind=8), allocatable :: x27(:, :, :, :)
   real(kind=8), allocatable :: x28(:, :, :, :)
   real(kind=8), allocatable :: x29(:, :, :, :)
   real(kind=8), allocatable :: x30(:, :, :, :)
   real(kind=8), allocatable :: x31(:, :, :, :)
   real(kind=8), allocatable :: x32(:, :, :, :)
   real(kind=8), allocatable :: x33(:, :, :, :)
   real(kind=8), allocatable :: x34(:, :, :, :)
   real(kind=8), allocatable :: x35(:, :, :, :)

   allocate (indocc(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   allocate (indunocc(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   indocc = 0
   indunocc = 0
   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         iocca = 0
         ioccb = 0
         if (i .gt. (n1 - iactocca)) iocca = iocca + 1
         if (j .gt. (n2 - iactoccb)) ioccb = ioccb + 1
         if (k .gt. (n2 - iactoccb)) ioccb = ioccb + 1
         if (iocca + ioccb .lt. iactindt) indocc(k, j, i) = 1
      end do
      end do
   end do
   do a = n1 + 1, n3
      do b = n2 + 1, n3 - 1
      do c = b + 1, n3
         iunoa = 0
         iunob = 0
         if (a .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
         if (b .lt. (n2 + iactunob + 1)) iunob = iunob + 1
         if (c .lt. (n2 + iactunob + 1)) iunob = iunob + 1
         if (iunoa + iunob .lt. iactindt) indunocc(c, b, a) = 1
      end do
      end do
   end do

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3124', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s1(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s1)
   deallocate (d1)
   deallocate (b2)

   allocate (x1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   x1 = 0.0d0
   call sum_stripe(4, shape(x1), size(x1), '4123', 1.000, x1, &
                   s1)
   deallocate (s1)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n2 - n0, n0 - n0/), '1324', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s2(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s2)
   deallocate (d1)
   deallocate (b2)

   allocate (x2(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   x2 = 0.0d0
   call sum_stripe(4, shape(x2), size(x2), '3124', -1.000, &
                   x2, s2)
   deallocate (s2)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s3(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s3)
   deallocate (d1)
   deallocate (b2)

   allocate (x5(n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   x5 = 0.0d0
   call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                   x5, s3)
   deallocate (s3)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '1423', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s4(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s4)
   deallocate (d1)
   deallocate (b2)

   allocate (x6(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x6 = 0.0d0
   call sum_stripe(4, shape(x6), size(x6), '3124', -1.000, &
                   x6, s4)
   deallocate (s4)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s5(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s5)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x5), size(x5), '4123', 1.000, x5, &
                   s5)
   deallocate (s5)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n2 - n0, n1 - n0/), '3421', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s6(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k4
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s6)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '4123', 1.000, x6, &
                   s6)
   deallocate (s6)

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

   allocate (x8(n0 + 1:n1, n0 + 1:n1))
   x8 = 0.0d0
   call sum_stripe(2, shape(x8), size(x8), '21', 1.000, x8, &
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

   allocate (x9(n1 + 1:n3, n1 + 1:n3))
   x9 = 0.0d0
   call sum_stripe(2, shape(x9), size(x9), '21', -1.000, x9, &
                   q2)
   deallocate (q2)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1243', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s7(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s7)
   deallocate (d1)
   deallocate (b2)

   allocate (x12(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x12 = 0.0d0
   call sum_stripe(4, shape(x12), size(x12), '3124', -1.000, &
                   x12, s7)
   deallocate (s7)

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

   x8 = x8 - q3
   deallocate (q3)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s8(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s8)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x12), size(x12), '4123', 1.000, &
                   x12, s8)
   deallocate (s8)

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

   x9 = x9 - q4
   deallocate (q4)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s9(n0 + 1:n1, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s9)
   deallocate (d1)
   deallocate (b2)

   allocate (x13(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   x13 = 0.0d0
   call sum_stripe(4, shape(x13), size(x13), '4123', 1.000, &
                   x13, s9)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s9), size(s9), '2341', s9, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s98(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s98)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s98)
   deallocate (s98)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s9), size(s9), '3241', s9, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s94(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s94)
   deallocate (d1)
   deallocate (b2)
   deallocate (s9)

   call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                   x5, s94)
   deallocate (s94)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s10(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s10)
   deallocate (d1)
   deallocate (b2)

   allocate (x14(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   x14 = 0.0d0
   call sum_stripe(4, shape(x14), size(x14), '4123', 1.000, &
                   x14, s10)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s10), size(s10), '3241', s10, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s99(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s99)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                   s99)
   deallocate (s99)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s10), size(s10), '2341', s10, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s95(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s95)
   deallocate (d1)
   deallocate (b2)
   deallocate (s10)

   call sum_stripe(4, shape(x6), size(x6), '3124', -1.000, &
                   x6, s95)
   deallocate (s95)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1234', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s11(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s11)
   deallocate (d1)
   deallocate (b2)

   allocate (x15(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   x15 = 0.0d0
   call sum_stripe(4, shape(x15), size(x15), '3124', -1.000, &
                   x15, s11)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s11), size(s11), '2314', s11, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s100(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s100)
   deallocate (d1)
   deallocate (b2)
   deallocate (s11)

   call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                   s100)
   deallocate (s100)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q5(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q5)
   deallocate (d1)
   deallocate (b2)

   allocate (x10(n0 + 1:n2, n0 + 1:n2))
   x10 = 0.0d0
   x10 = x10 + q5
   deallocate (q5)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n2 - n0/), '1432', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s12(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
   i1 = k4 * k3 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s12)
   deallocate (d1)
   deallocate (b2)

   allocate (x16(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   x16 = 0.0d0
   call sum_stripe(4, shape(x16), size(x16), '4123', -1.000, &
                   x16, s12)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s12), size(s12), '2341', s12, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s101(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k3
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s101)
   deallocate (d1)
   deallocate (b2)
   deallocate (s12)

   call sum_stripe(4, shape(x2), size(x2), '4123', -1.000, &
                   x2, s101)
   deallocate (s101)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n2 - n0, n2 - n0/), '1342', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q6(n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q6)
   deallocate (d1)
   deallocate (b2)

   allocate (x11(n2 + 1:n3, n2 + 1:n3))
   x11 = 0.0d0
   x11 = x11 + q6
   deallocate (q6)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s13(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s13)
   deallocate (d1)
   deallocate (b2)

   allocate (x20(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x20 = 0.0d0
   call sum_stripe(4, shape(x20), size(x20), '3124', -1.000, &
                   x20, s13)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s13), size(s13), '2314', s13, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s103(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s103)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', 1.000, x6, &
                   s103)
   deallocate (s103)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s13), size(s13), '3214', s13, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s102(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s102)
   deallocate (d1)
   deallocate (b2)
   deallocate (s13)

   call sum_stripe(4, shape(x5), size(x5), '3124', -1.000, &
                   x5, s102)
   deallocate (s102)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n1 - n0/), '3241', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s14(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s14)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x20), size(x20), '4123', 1.000, &
                   x20, s14)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s14), size(s14), '2341', s14, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s105(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s105)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                   x6, s105)
   deallocate (s105)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s14), size(s14), '3241', s14, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s104(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s104)
   deallocate (d1)
   deallocate (b2)
   deallocate (s14)

   call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                   s104)
   deallocate (s104)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3214', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s15(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s15)
   deallocate (d1)
   deallocate (b2)

   allocate (x22(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   x22 = 0.0d0
   call sum_stripe(4, shape(x22), size(x22), '4123', 1.000, &
                   x22, s15)

   call sum_shift(4, shape(intr), size(intr), shape(x22), &
                  size(x22), (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2143', 1.000, intr, x22)

   if (lvl_q) then
      allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      rewind (tc)
      read (tc) t4c

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t4c,x22) &
            !$omp private(a,b,c,n,e,m,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n1 + 1, n3
                     do m = n0 + 1, n1
                     do n = n0 + 1, n1
                        sum = sum &
                              - (x22(n, m, e, i) * t4c(c, b, e, a, k, j, n, m)) / 2.0d0!icbakj(-0.500)
                     end do
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4c)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x22)
   end if

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s15), size(s15), '2431', s15, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
   allocate (s110(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k2 * k4
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s110)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                   s110)
   deallocate (s110)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s15), size(s15), '3421', s15, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q25(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q25)
   deallocate (d1)
   deallocate (b2)

   x8 = x8 - q25
   deallocate (q25)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s15), size(s15), '3241', s15, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s96(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s96)
   deallocate (d1)
   deallocate (b2)
   deallocate (s15)

   call sum_stripe(4, shape(x12), size(x12), '3124', -1.000, &
                   x12, s96)
   deallocate (s96)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '1243', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s16(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s16)
   deallocate (d1)
   deallocate (b2)

   allocate (x23(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   x23 = 0.0d0
   call sum_stripe(4, shape(x23), size(x23), '4123', -1.000, &
                   x23, s16)

   call sum_shift(4, shape(intr), size(intr), shape(x23), &
                  size(x23), (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '4132', 1.000, intr, x23)

   if (lvl_q) then
      allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      rewind (tc)
      read (tc) t4c

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t4c,x23) &
            !$omp private(a,b,c,e,m,f,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n1 + 1, n3
                     do f = n1 + 1, n3
                     do m = n0 + 1, n1
                        sum = sum &
                              + (x23(m, f, e, a) * t4c(c, b, f, e, k, j, m, i)) / 2.0d0 !acbkji(+0.500)
                     end do
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4c)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x23)
   end if

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s16), size(s16), '2341', s16, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
   allocate (s111(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i2 = k2 * k4
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s111)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2413', -1.000, &
                   x2, s111)
   deallocate (s111)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s16), size(s16), '2431', s16, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q26(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q26)
   deallocate (d1)
   deallocate (b2)
   deallocate (s16)

   x9 = x9 + q26
   deallocate (q26)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n1 - n0/), '1324', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q7(n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q7)
   deallocate (d1)
   deallocate (b2)

   allocate (x21(n0 + 1:n1, n1 + 1:n3))
   x21 = 0.0d0
   x21 = x21 + q7

   allocate (b1(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(q7), size(q7), '21', q7, b1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (s112(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, b1, d2, s112)
   deallocate (b1)
   deallocate (d2)
   deallocate (q7)

   call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                   s112)
   deallocate (s112)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3214', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s17(n0 + 1:n1, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3))
   i1 = k4 * k1 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s17)
   deallocate (d1)
   deallocate (b2)

   allocate (x27(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   x27 = 0.0d0
   call sum_stripe(4, shape(x27), size(x27), '4123', 1.000, &
                   x27, s17)

   call sum_shift(4, shape(intm), size(intm), shape(x27), &
                  size(x27), (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', 1.000, intm, x27)

   if (lvl_q) then
      allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
      rewind (td)
      read (td) t4d

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x27,t4d) &
            !$omp private(a,b,c,n,e,m,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n2 + 1, n3
                     do m = n0 + 1, n1
                     do n = n0 + 1, n2
                        sum = sum &
                              - x27(n, m, e, i) * t4d(e, c, b, a, n, k, j, m) !icbakj(-1.000)
                     end do
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4d)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x27)
   end if

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s17), size(s17), '2341', s17, d1)
   allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
   allocate (s115(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4
   i2 = k3 * k4
   i3 = k1 * k2
   call egemm(i1, i2, i3, d1, d2, s115)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '2314', 1.000, x6, &
                   s115)
   deallocate (s115)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s17), size(s17), '3421', s17, d1)
   allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
   allocate (s114(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2
   i2 = k2 * k3
   i3 = k4 * k1
   call egemm(i1, i2, i3, d1, d2, s114)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2314', -1.000, &
                   x5, s114)
   deallocate (s114)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s17), size(s17), '2431', s17, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
   allocate (s113(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k2 * k4
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s113)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                   s113)
   deallocate (s113)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s17), size(s17), '2431', s17, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q27(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q27)
   deallocate (d1)
   deallocate (b2)

   x8 = x8 + q27
   deallocate (q27)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s17), size(s17), '2341', s17, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s107(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s107)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x14), size(x14), '3124', -1.000, &
                   x14, s107)
   deallocate (s107)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s17), size(s17), '4231', s17, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s106(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s106)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x13), size(x13), '3124', 1.000, &
                   x13, s106)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s106), size(s106), '2314', s106, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s150(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s150)
   deallocate (d1)
   deallocate (b2)
   deallocate (s106)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s150)
   deallocate (s150)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s17), size(s17), '3241', s17, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s97(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s97)
   deallocate (d1)
   deallocate (b2)
   deallocate (s17)

   call sum_stripe(4, shape(x20), size(x20), '3124', -1.000, &
                   x20, s97)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s97), size(s97), '2314', s97, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s149(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s149)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', 1.000, x6, &
                   s149)
   deallocate (s149)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s97), size(s97), '3214', s97, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s148(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s148)
   deallocate (d1)
   deallocate (b2)
   deallocate (s97)

   call sum_stripe(4, shape(x5), size(x5), '3124', -1.000, &
                   x5, s148)
   deallocate (s148)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '1243', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s18(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s18)
   deallocate (d1)
   deallocate (b2)

   allocate (x28(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   x28 = 0.0d0
   call sum_stripe(4, shape(x28), size(x28), '4123', -1.000, &
                   x28, s18)

   call sum_shift(4, shape(intm), size(intm), shape(x28), &
                  size(x28), (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '4132', 1.000, intm, x28)

   if (lvl_q) then
      allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
      rewind (td)
      read (td) t4d

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                     !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x28,t4d) &
            !$omp private(a,b,c,e,m,f,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n1 + 1, n3
                     do f = n2 + 1, n3
                     do m = n0 + 1, n2
                        sum = sum &
                              + x28(m, f, e, a) * t4d(f, c, b, e, m, k, j, i) !acbkji(+1.000)
                     end do
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4d)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x28)
   end if

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s18), size(s18), '2431', s18, d1)
   allocate (d2(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3214', t2b, d2)
   allocate (s119(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4
   i2 = k1 * k4
   i3 = k3 * k2
   call egemm(i1, i2, i3, d1, d2, s119)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '2413', 1.000, x6, &
                   s119)
   deallocate (s119)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
   call reorder_stripe(4, shape(s18), size(s18), '3421', s18, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s117(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
   i1 = k3 * k2
   i2 = k1 * k2
   i3 = k3 * k4
   call egemm(i1, i2, i3, d1, d2, s117)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '3412', -1.000, &
                   x5, s117)
   deallocate (s117)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s18), size(s18), '2341', s18, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
   allocate (s116(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i2 = k2 * k4
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s116)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2413', -1.000, &
                   x2, s116)
   deallocate (s116)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s18), size(s18), '2341', s18, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q30(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q30)
   deallocate (d1)
   deallocate (b2)

   x9 = x9 - q30
   deallocate (q30)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s18), size(s18), '2341', s18, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s109(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s109)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x16), size(x16), '3124', 1.000, &
                   x16, s109)
   deallocate (s109)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s18), size(s18), '3241', s18, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s108(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s108)
   deallocate (d1)
   deallocate (b2)
   deallocate (s18)

   call sum_stripe(4, shape(x15), size(x15), '4123', -1.000, &
                   x15, s108)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s108), size(s108), '2341', s108, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s151(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s151)
   deallocate (d1)
   deallocate (b2)
   deallocate (s108)

   call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                   s151)
   deallocate (s151)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q8(n0 + 1:n2, n2 + 1:n3))
   i1 = k4 * k2
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q8)
   deallocate (d1)
   deallocate (b2)

   allocate (x26(n0 + 1:n2, n2 + 1:n3))
   x26 = 0.0d0
   x26 = x26 + q8

   allocate (b1(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(q8), size(q8), '21', q8, b1)
   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (s120(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2
   i2 = k2 * k2 * k4
   i3 = k4
   call egemm(i1, i2, i3, b1, d2, s120)
   deallocate (b1)
   deallocate (d2)

   allocate (x31(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   x31 = 0.0d0
   call sum_stripe(4, shape(x31), size(x31), '2341', 1.000, &
                   x31, s120)
   deallocate (s120)

   allocate (b1(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(q8), size(q8), '21', q8, b1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s118(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, b1, d2, s118)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                   s118)
   deallocate (s118)

   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q29(n2 + 1:n3, n2 + 1:n3))
   i1 = k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, q8, b2, q29)
   deallocate (b2)

   call sum_stripe(2, shape(x11), size(x11), '21', -1.000, &
                   x11, q29)
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

   call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                   x10, q28)
   deallocate (q28)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s19(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s19)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s19)
   deallocate (s19)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4123', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s20(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s20)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                   s20)
   deallocate (s20)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2314', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s21(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s21)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                   x2, s21)
   deallocate (s21)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n2 - n0, n1 - n0/), '4321', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s22(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k3
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s22)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                   s22)
   deallocate (s22)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s23(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s23)
   deallocate (d1)
   deallocate (b2)

   allocate (x3(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   x3 = 0.0d0
   call sum_stripe(4, shape(x3), size(x3), '2134', -1.000, &
                   x3, s23)
   deallocate (s23)

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

   allocate (x32(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   x32 = 0.0d0
   call sum_stripe(4, shape(x32), size(x32), '3124', 1.000, &
                   x32, s24)
   deallocate (s24)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2413', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s25(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s25)
   deallocate (d1)
   deallocate (b2)

   allocate (x33(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x33 = 0.0d0
   call sum_stripe(4, shape(x33), size(x33), '3124', 1.000, &
                   x33, s25)
   deallocate (s25)

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

   allocate (x4(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x4 = 0.0d0
   call sum_stripe(4, shape(x4), size(x4), '4123', 1.000, x4, &
                   s26)
   deallocate (s26)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s27(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s27)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                   s27)
   deallocate (s27)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2413', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s28(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s28)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                   x6, s28)
   deallocate (s28)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s29(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s29)
   deallocate (d1)
   deallocate (b2)

   allocate (x7(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x7 = 0.0d0
   call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                   x7, s29)
   deallocate (s29)

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

   call sum_stripe(4, shape(x7), size(x7), '4123', 1.000, x7, &
                   s30)
   deallocate (s30)

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

   call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                   x10, q9)
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

   call sum_stripe(2, shape(x11), size(x11), '21', -1.000, &
                   x11, q10)
   deallocate (q10)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s31(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s31)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x13), size(x13), '3124', 1.000, &
                   x13, s31)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s31), size(s31), '2314', s31, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s121(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s121)
   deallocate (d1)
   deallocate (b2)
   deallocate (s31)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s121)
   deallocate (s121)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s32(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s32)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x14), size(x14), '3124', -1.000, &
                   x14, s32)
   deallocate (s32)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q11(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q11)
   deallocate (d1)
   deallocate (b2)

   x8 = x8 + q11
   deallocate (q11)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s33(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s33)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x15), size(x15), '4123', 1.000, &
                   x15, s33)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s33), size(s33), '2341', s33, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s122(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s122)
   deallocate (d1)
   deallocate (b2)
   deallocate (s33)

   call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                   x2, s122)
   deallocate (s122)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s34(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s34)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x16), size(x16), '3124', -1.000, &
                   x16, s34)
   deallocate (s34)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q12(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q12)
   deallocate (d1)
   deallocate (b2)

   x9 = x9 + q12
   deallocate (q12)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s35(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s35)
   deallocate (d1)
   deallocate (b2)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,s35,t3c) &
         !$omp private(a,b,c,m,n,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  do n = n0 + 1, n2
                     sum = sum &
                           + (s35(k, n, m, j) * t3c(c, b, a, n, m, i) & !kjcbai  (+0.500)
                              - s35(j, n, m, k) * t3c(c, b, a, n, m, i)) / 2.0d0!jkcbai  (-0.500)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s35), size(s35), '3214', s35, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s123(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s123)
   deallocate (d1)
   deallocate (b2)
   deallocate (s35)

   call sum_stripe(4, shape(x32), size(x32), '2134', -1.000, &
                   x32, s123)
   deallocate (s123)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s36(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s36)
   deallocate (d1)
   deallocate (b2)

   allocate (x18(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x18 = 0.0d0
   call sum_stripe(4, shape(x18), size(x18), '3124', -1.000, &
                   x18, s36)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s36), size(s36), '2314', s36, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s124(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s124)
   deallocate (d1)
   deallocate (b2)
   deallocate (s36)

   call sum_stripe(4, shape(x4), size(x4), '2134', 1.000, x4, &
                   s124)
   deallocate (s124)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1423', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q13(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q13)
   deallocate (d1)
   deallocate (b2)

   x10 = x10 - q13
   deallocate (q13)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s37(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s37)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x18), size(x18), '4123', 1.000, &
                   x18, s37)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s37), size(s37), '2341', s37, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s126(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s126)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x33), size(x33), '3124', 1.000, &
                   x33, s126)
   deallocate (s126)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s37), size(s37), '3241', s37, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s125(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s125)
   deallocate (d1)
   deallocate (b2)
   deallocate (s37)

   call sum_stripe(4, shape(x3), size(x3), '3124', 1.000, x3, &
                   s125)
   deallocate (s125)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s38(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s38)
   deallocate (d1)
   deallocate (b2)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,s38) &
         !$omp private(a,b,c,f,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  do f = n2 + 1, n3
                     sum = sum &
                           + (s38(b, f, e, c) * t3c(f, e, a, k, j, i) & !bcakji (+0.500)
                              - s38(c, f, e, b) * t3c(f, e, a, k, j, i)) / 2.0d0!cbakji (-0.500)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (s38)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2341', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q14(n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q14)
   deallocate (d1)
   deallocate (b2)

   x11 = x11 - q14
   deallocate (q14)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4213', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s39(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s39)
   deallocate (d1)
   deallocate (b2)

   allocate (x24(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   x24 = 0.0d0
   call sum_stripe(4, shape(x24), size(x24), '4123', 1.000, &
                   x24, s39)

   call sum_shift(4, shape(intm), size(intm), shape(x24), &
                  size(x24), (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', 1.000, intm, x24)

   if (lvl_q) then
      allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      rewind (tc)
      read (tc) t4c

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 13 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t4c,x24) &
            !$omp private(a,b,c,n,e,m,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n1 + 1, n3
                     do m = n0 + 1, n1
                     do n = n0 + 1, n2
                        sum = sum &
                              + x24(n, m, e, j) * t4c(c, b, e, a, n, k, m, i) & !jcbaki(+1.000)
                              - x24(n, m, e, k) * t4c(c, b, e, a, n, j, m, i) !kcbaji(-1.000)
                     end do
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4c)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x24)
   end if

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(4, shape(s39), size(s39), '2431', s39, d1)
   allocate (d2(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3214', t2b, d2)
   allocate (s135(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1
   i2 = k1 * k4
   i3 = k3 * k2
   call egemm(i1, i2, i3, d1, d2, s135)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '2413', -1.000, &
                   x1, s135)
   deallocate (s135)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s39), size(s39), '3421', s39, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
   allocate (s134(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k2 * k4
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s134)
   deallocate (d1)
   deallocate (d2)

   allocate (x34(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   x34 = 0.0d0
   call sum_stripe(4, shape(x34), size(x34), '2314', 1.000, &
                   x34, s134)
   deallocate (s134)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s39), size(s39), '2341', s39, d1)
   allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
   allocate (s133(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3
   i2 = k3 * k4
   i3 = k1 * k2
   call egemm(i1, i2, i3, d1, d2, s133)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2314', 1.000, x2, &
                   s133)
   deallocate (s133)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s39), size(s39), '3421', s39, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s131(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k1 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s131)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2413', 1.000, x5, &
                   s131)
   deallocate (s131)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s39), size(s39), '2341', s39, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s127(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s127)
   deallocate (d1)
   deallocate (b2)
   deallocate (s39)

   call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                   x7, s127)
   deallocate (s127)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s40(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s40)
   deallocate (d1)
   deallocate (b2)

   allocate (x25(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
   x25 = 0.0d0
   call sum_stripe(4, shape(x25), size(x25), '4123', -1.000, &
                   x25, s40)

   call sum_shift(4, shape(intm), size(intm), shape(x25), &
                  size(x25), (/n0 - n0, n2 - n0, n1 - n0, n2 - n0/), '1432', 1.000, intm, x25)

   if (lvl_q) then
      allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      rewind (tc)
      read (tc) t4c

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 13 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x25,t4c) &
            !$omp private(a,b,c,e,m,f,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n1 + 1, n3
                     do f = n2 + 1, n3
                     do m = n0 + 1, n1
                        sum = sum &
                              + x25(m, f, e, c) * t4c(f, b, e, a, k, j, m, i) & !cbakji(+1.000)
                              - x25(m, f, e, b) * t4c(f, c, e, a, k, j, m, i) !bcakji(-1.000)
                     end do
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4c)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x25)
   end if

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
   call reorder_stripe(4, shape(s40), size(s40), '3421', s40, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s138(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3))
   i1 = k4 * k1
   i2 = k1 * k2
   i3 = k3 * k4
   call egemm(i1, i2, i3, d1, d2, s138)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '3412', -1.000, &
                   x1, s138)
   deallocate (s138)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(s40), size(s40), '2431', s40, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
   allocate (s137(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i2 = k2 * k4
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s137)
   deallocate (d1)
   deallocate (d2)

   allocate (x35(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x35 = 0.0d0
   call sum_stripe(4, shape(x35), size(x35), '3412', 1.000, &
                   x35, s137)
   deallocate (s137)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(s40), size(s40), '2341', s40, d1)
   allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
   allocate (s136(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n2 + 1:n3))
   i1 = k4 * k3
   i2 = k2 * k3
   i3 = k4 * k1
   call egemm(i1, i2, i3, d1, d2, s136)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '3412', 1.000, x2, &
                   s136)
   deallocate (s136)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(s40), size(s40), '2431', s40, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s132(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i2 = k1 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s132)
   deallocate (d1)
   deallocate (d2)
   deallocate (s40)

   call sum_stripe(4, shape(x6), size(x6), '3412', -1.000, &
                   x6, s132)
   deallocate (s132)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q15(n0 + 1:n1, n1 + 1:n3))
   i1 = k3 * k1
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q15)
   deallocate (d1)
   deallocate (b2)

   x21 = x21 + q15

   call sum_shift(2, shape(fockr), size(fockr), shape(x21), &
                  size(x21), (/n0, n1/), '12', 1.000, fockr, x21)

   if (lvl_q) then
      allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      rewind (tc)
      read (tc) t4c

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t4c,x21) &
            !$omp private(a,b,c,e,m,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n1 + 1, n3
                     do m = n0 + 1, n1
                        sum = sum &
                              + x21(m, e) * t4c(c, b, e, a, k, j, m, i) !cbakji (+1.000)
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4c)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x21)
   end if

   allocate (b1(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(q15), size(q15), '21', q15, b1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (s139(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, b1, d2, s139)
   deallocate (b1)
   deallocate (d2)
   deallocate (q15)

   call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                   s139)
   deallocate (s139)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '3214', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s41(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   i1 = k4 * k2 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s41)
   deallocate (d1)
   deallocate (b2)

   allocate (x29(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   x29 = 0.0d0
   call sum_stripe(4, shape(x29), size(x29), '4123', 1.000, &
                   x29, s41)

   call sum_shift(4, shape(intb), size(intb), shape(x29), &
                  size(x29), (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', 1.000, intb, x29)

   if (lvl_q) then
      allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
      rewind (td)
      read (td) t4d

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 13 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x29,t4d) &
            !$omp private(a,b,c,n,e,m,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n2 + 1, n3
                     do m = n0 + 1, n2
                     do n = n0 + 1, n2
                        sum = sum &
                              + (x29(n, m, e, j) * t4d(e, c, b, a, n, m, k, i) & !jcbaki(+0.500)
                                 - x29(n, m, e, k) * t4d(e, c, b, a, n, m, j, i)) / 2.0d0!kcbaji(-0.500)
                     end do
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4d)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x29)
   end if

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s41), size(s41), '3421', s41, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (s142(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k1 * k3
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s142)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2413', -1.000, &
                   x5, s142)
   deallocate (s142)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s41), size(s41), '2431', s41, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
   allocate (s141(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k2 * k4
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s141)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x32), size(x32), '2314', -1.000, &
                   x32, s141)
   deallocate (s141)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s41), size(s41), '2341', s41, d1)
   allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
   allocate (s140(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k4 * k4
   i3 = k2 * k2
   call egemm(i1, i2, i3, d1, d2, s140)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x4), size(x4), '2314', 0.500, x4, &
                   s140)
   deallocate (s140)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s41), size(s41), '3421', s41, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q31(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q31)
   deallocate (d1)
   deallocate (b2)

   x10 = x10 - q31
   deallocate (q31)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s41), size(s41), '3241', s41, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s129(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s129)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x18), size(x18), '3124', -1.000, &
                   x18, s129)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s129), size(s129), '2314', s129, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s153(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s153)
   deallocate (d1)
   deallocate (b2)
   deallocate (s129)

   call sum_stripe(4, shape(x4), size(x4), '2134', 1.000, x4, &
                   s153)
   deallocate (s153)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s41), size(s41), '4231', s41, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s128(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s128)
   deallocate (d1)
   deallocate (b2)
   deallocate (s41)

   allocate (x17(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   x17 = 0.0d0
   call sum_stripe(4, shape(x17), size(x17), '3124', 1.000, &
                   x17, s128)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s128), size(s128), '3214', s128, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s152(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s152)
   deallocate (d1)
   deallocate (b2)
   deallocate (s128)

   call sum_stripe(4, shape(x3), size(x3), '2134', -1.000, &
                   x3, s152)
   deallocate (s152)

   allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '1243', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s42(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4 * k2
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s42)
   deallocate (d1)
   deallocate (b2)

   allocate (x30(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   x30 = 0.0d0
   call sum_stripe(4, shape(x30), size(x30), '4123', -1.000, &
                   x30, s42)

   call sum_shift(4, shape(intb), size(intb), shape(x30), &
                  size(x30), (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '4132', 1.000, intb, x30)

   if (lvl_q) then
      allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
      rewind (td)
      read (td) t4d

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 13 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x30,t4d) &
            !$omp private(a,b,c,e,m,f,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n2 + 1, n3
                     do f = n2 + 1, n3
                     do m = n0 + 1, n2
                        sum = sum &
                              + (x30(m, f, e, c) * t4d(f, e, b, a, m, k, j, i) & !cbakji(+0.500)
                                 - x30(m, f, e, b) * t4d(f, e, c, a, m, k, j, i)) / 2.0d0!bcakji(-0.500)
                     end do
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4d)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x30)
   end if

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(s42), size(s42), '2431', s42, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (s146(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i2 = k1 * k3
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s146)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '3412', 1.000, x6, &
                   s146)
   deallocate (s146)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(4, shape(s42), size(s42), '3421', s42, d1)
   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (s145(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   i1 = k4 * k2
   i2 = k2 * k2
   i3 = k4 * k4
   call egemm(i1, i2, i3, d1, d2, s145)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x3), size(x3), '3412', -0.500, &
                   x3, s145)
   deallocate (s145)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(s42), size(s42), '2341', s42, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
   allocate (s143(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i2 = k2 * k4
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s143)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x33), size(x33), '3412', -1.000, &
                   x33, s143)
   deallocate (s143)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(s42), size(s42), '2431', s42, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q32(n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q32)
   deallocate (d1)
   deallocate (b2)

   x11 = x11 + q32
   deallocate (q32)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   call reorder_stripe(4, shape(s42), size(s42), '2341', s42, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s130(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s130)
   deallocate (d1)
   deallocate (b2)
   deallocate (s42)

   allocate (x19(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
   x19 = 0.0d0
   call sum_stripe(4, shape(x19), size(x19), '3124', 1.000, &
                   x19, s130)
   deallocate (s130)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n2 - n0/), '1324', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q16(n0 + 1:n2, n2 + 1:n3))
   i1 = k4 * k2
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q16)
   deallocate (d1)
   deallocate (b2)

   x26 = x26 + q16

   call sum_shift(2, shape(fockb), size(fockb), shape(x26), &
                  size(x26), (/n0, n2/), '12', 1.000, fockb, x26)

   if (lvl_q) then
      allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, & !ilias: if no quadruples comment out the following 4 lines
                    n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
      rewind (td)
      read (td) t4d

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                      !ilias: if no quadruples comment out the following 12 lines
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x26,t4d) &
            !$omp private(a,b,c,e,m,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do e = n2 + 1, n3
                     do m = n0 + 1, n2
                        sum = sum &
                              + x26(m, e) * t4d(e, c, b, a, m, k, j, i)   !cbakji (+1.000)
                     end do
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (t4d)                                              !ilias: if no quadruples comment out the following 2 lines
      deallocate (x26)
   end if

   allocate (b1(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(q16), size(q16), '21', q16, b1)
   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (s147(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2
   i2 = k2 * k2 * k4
   i3 = k4
   call egemm(i1, i2, i3, b1, d2, s147)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x31), size(x31), '2341', 1.000, &
                   x31, s147)
   deallocate (s147)

   allocate (b1(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(q16), size(q16), '21', q16, b1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s144(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, b1, d2, s144)
   deallocate (b1)
   deallocate (d2)
   deallocate (q16)

   call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                   s144)
   deallocate (s144)

   if (t2diag4 .ne. 0) then         !ilias: testing the particle version of 3cc
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
      allocate (s43(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
      i1 = k2 * k2
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s43)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag4
      call sum_stripe(4, shape(x5), size(x5), '2413', factor, &
                      x5, s43)
      deallocate (s43)
   end if

   if (t2diag3 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n2 - n0, n2 - n0/), '1342', intm, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
      allocate (s44(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
      i1 = k4 * k4
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s44)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag3
      call sum_stripe(4, shape(x6), size(x6), '3412', factor, &
                      x6, s44)
      deallocate (s44)
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
      allocate (s45(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s45)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag1
      call sum_stripe(4, shape(x12), size(x12), '3412', factor, &
                      x12, s45)
      deallocate (s45)
   end if

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '1432', intr, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
      allocate (q17(n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1
      i3 = k3 * k3 * k1
      call egemm(i1, i2, i3, d1, d2, q17)
      deallocate (d1)
      deallocate (d2)

      factor = -0.500 * t3diag4
      call sum_stripe(2, shape(x8), size(x8), '21', factor, x8, &
                      q17)
      deallocate (q17)
   end if

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2134', intr, d1)
      allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
      call reorder_stripe(4, shape(t2a), size(t2a), '3412', t2a, d2)
      allocate (q18(n1 + 1:n3, n1 + 1:n3))
      i1 = k3
      i2 = k3
      i3 = k3 * k1 * k1
      call egemm(i1, i2, i3, d1, d2, q18)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * t3diag3
      call sum_stripe(2, shape(x9), size(x9), '21', factor, x9, &
                      q18)
      deallocate (q18)
   end if

   if (t3diag1 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
      allocate (s46(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k1 * k3
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s46)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag1
      call sum_stripe(4, shape(x20), size(x20), '3412', factor, &
                      x20, s46)
      deallocate (s46)
   end if

   allocate (b1(n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                      size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (s47(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, b1, d2, s47)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                   s47)
   deallocate (s47)

   if (t2diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2413', intr, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
      allocate (s48(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      i1 = k1 * k1
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s48)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag4
      call sum_stripe(4, shape(x1), size(x1), '2314', factor, &
                      x1, s48)
      deallocate (s48)
   end if

   if (t2diag3 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2431', intr, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
      allocate (s49(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
      i1 = k3 * k3
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s49)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag3
      call sum_stripe(4, shape(x2), size(x2), '2413', factor, &
                      x2, s49)
      deallocate (s49)
   end if

   if (t2diag5 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', intm, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
      allocate (s50(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      i1 = k2 * k3
      i2 = k3 * k4
      i3 = k1 * k2
      call egemm(i1, i2, i3, d1, d2, s50)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag5
      call sum_stripe(4, shape(x2), size(x2), '2314', factor, &
                      x2, s50)
      deallocate (s50)
   end if

   if (t2diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1324', intm, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
      allocate (s51(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      i1 = k2 * k2
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s51)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag4
      call sum_stripe(4, shape(x34), size(x34), '2314', factor, &
                      x34, s51)
      deallocate (s51)
   end if

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x34,t2b) &
         !$omp private(a,b,c,n,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n2
                  sum = sum &
                        + x34(n, b, k, j) * t2b(c, a, n, i) & !bkjcai    (+1.000)
                        - x34(n, c, k, j) * t2b(b, a, n, i) & !ckjbai    (-1.000)
                        - x34(n, b, j, k) * t2b(c, a, n, i) & !bjkcai    (-1.000)
                        + x34(n, c, j, k) * t2b(b, a, n, i)     !cjkbai    (+1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x34)

   if (t2diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2314', intm, d1)
      allocate (d2(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3214', t2b, d2)
      allocate (s52(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
      i1 = k2 * k1
      i2 = k1 * k4
      i3 = k3 * k2
      call egemm(i1, i2, i3, d1, d2, s52)
      deallocate (d1)
      deallocate (d2)

      factor = -t2diag4
      call sum_stripe(4, shape(x1), size(x1), '2413', factor, &
                      x1, s52)
      deallocate (s52)
   end if

   if (t2diag3 .ne. 0) then
      allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n2 - n0/), '1432', intm, d1)
      allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
      allocate (s53(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n2 + 1:n3))
      i1 = k4 * k3
      i2 = k2 * k3
      i3 = k4 * k1
      call egemm(i1, i2, i3, d1, d2, s53)
      deallocate (d1)
      deallocate (d2)

      factor = -t2diag3
      call sum_stripe(4, shape(x2), size(x2), '3412', factor, &
                      x2, s53)
      deallocate (s53)

      allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n2 - n0, n2 - n0/), '1342', intm, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
      allocate (s54(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      i1 = k4 * k4
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s54)
      deallocate (d1)
      deallocate (d2)

      call sum_stripe(4, shape(x35), size(x35), '3412', factor, &
                      x35, s54)
      deallocate (s54)

   end if

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x35,t2b) &
         !$omp private(a,b,c,f,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  sum = sum &
                        + x35(f, c, b, k) * t2b(f, a, j, i) & !cbkaji (+1.000)
                        - x35(f, b, c, k) * t2b(f, a, j, i) & !bckaji (-1.000)
                        - x35(f, c, b, j) * t2b(f, a, k, i) & !cbjaki (-1.000)
                        + x35(f, b, c, j) * t2b(f, a, k, i)     !bcjaki (+1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x35)

   if (t2diag5 .ne. 0) then
      allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n2 - n0, n1 - n0, n0 - n0, n2 - n0/), '4312', intm, d1)
      allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
      allocate (s55(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3))
      i1 = k4 * k1
      i2 = k1 * k2
      i3 = k3 * k4
      call egemm(i1, i2, i3, d1, d2, s55)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag5
      call sum_stripe(4, shape(x1), size(x1), '3412', factor, &
                      x1, s55)
      deallocate (s55)
   end if

   allocate (b1(n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                      size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s56(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, b1, d2, s56)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                   s56)
   deallocate (s56)

   allocate (b1(n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                      size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (s57(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   i1 = k2
   i2 = k2 * k2 * k4
   i3 = k4
   call egemm(i1, i2, i3, b1, d2, s57)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x3), size(x3), '2341', 1.000, x3, &
                   s57)
   deallocate (s57)

   if (t2diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n0 + 1:n1))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intm, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (s58(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      i1 = k1 * k1
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s58)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag4
      call sum_stripe(4, shape(x1), size(x1), '2314', factor, &
                      x1, s58)
      deallocate (s58)

      allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1423', intm, d1)
      allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
      allocate (s59(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
      i1 = k1 * k2
      i2 = k2 * k3
      i3 = k4 * k1
      call egemm(i1, i2, i3, d1, d2, s59)
      deallocate (d1)
      deallocate (d2)

      factor = -t2diag4
      call sum_stripe(4, shape(x5), size(x5), '2314', factor, &
                      x5, s59)
      deallocate (s59)
   end if

   if (t2diag5 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intm, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
      allocate (s60(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
      i1 = k1 * k4
      i2 = k3 * k4
      i3 = k1 * k2
      call egemm(i1, i2, i3, d1, d2, s60)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag5
      call sum_stripe(4, shape(x6), size(x6), '2314', factor, &
                      x6, s60)
      deallocate (s60)
   end if

   if (t2diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (s61(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
      i1 = k3 * k3
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s61)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag3
      call sum_stripe(4, shape(x2), size(x2), '2413', factor, &
                      x2, s61)
      deallocate (s61)
   end if

   if (t2diag5 .ne. 0) then
      allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n2 - n0, n1 - n0, n0 - n0, n1 - n0/), '4321', intm, d1)
      allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
      allocate (s62(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
      i1 = k3 * k2
      i2 = k1 * k2
      i3 = k3 * k4
      call egemm(i1, i2, i3, d1, d2, s62)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag5
      call sum_stripe(4, shape(x5), size(x5), '3412', factor, &
                      x5, s62)
      deallocate (s62)
   end if

   if (t2diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n2 - n0, n1 - n0/), '2341', intm, d1)
      allocate (d2(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3214', t2b, d2)
      allocate (s63(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      i1 = k3 * k4
      i2 = k1 * k4
      i3 = k3 * k2
      call egemm(i1, i2, i3, d1, d2, s63)
      deallocate (d1)
      deallocate (d2)

      factor = -t2diag3
      call sum_stripe(4, shape(x6), size(x6), '2413', factor, &
                      x6, s63)
      deallocate (s63)
   end if

   if (t2diag5 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intb, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
      allocate (s64(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      i1 = k2 * k4
      i2 = k4 * k4
      i3 = k2 * k2
      call egemm(i1, i2, i3, d1, d2, s64)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * t2diag5
      call sum_stripe(4, shape(x4), size(x4), '2314', factor, &
                      x4, s64)
      deallocate (s64)
   end if

   if (t2diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2413', intb, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (s65(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      i1 = k2 * k2
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s65)
      deallocate (d1)
      deallocate (d2)

      factor = -t2diag4
      call sum_stripe(4, shape(x32), size(x32), '2314', factor, &
                      x32, s65)
      deallocate (s65)

   end if

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x32,t2b) &
         !$omp private(a,b,c,m,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        + x32(m, c, k, j) * t2b(b, a, m, i) & !ckjbai (+1.000)
                        - x32(m, b, k, j) * t2b(c, a, m, i) & !bkjcai (-1.000)
                        - x32(m, c, j, k) * t2b(b, a, m, i) & !cjkbai (-1.000)
                        + x32(m, b, j, k) * t2b(c, a, m, i)     !bjkcai (+1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x32)

   if (t2diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1423', intb, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
      allocate (s66(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
      i1 = k2 * k2
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s66)
      deallocate (d1)
      deallocate (d2)

      factor = -t2diag4
      call sum_stripe(4, shape(x5), size(x5), '2413', factor, &
                      x5, s66)
      deallocate (s66)
   end if

   if (t2diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (s67(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      i1 = k4 * k4
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s67)
      deallocate (d1)
      deallocate (d2)

      factor = t2diag3
      call sum_stripe(4, shape(x33), size(x33), '3412', factor, &
                      x33, s67)
      deallocate (s67)

   end if
   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x33,t2b) &
         !$omp private(a,b,c,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  sum = sum &
                        + x33(e, c, b, j) * t2b(e, a, k, i) & !ecbjeaki    (+1.000)
                        - x33(e, b, c, j) * t2b(e, a, k, i) & !ebcjeaki    (-1.000)
                        - x33(e, c, b, k) * t2b(e, a, j, i) & !ecbkeaji    (-1.000)
                        + x33(e, b, c, k) * t2b(e, a, j, i)     !ebckeaji    (+1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x33)

   if (t2diag5 .ne. 0) then
      allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n2 - n0, n2 - n0, n0 - n0, n2 - n0/), '4321', intb, d1)
      allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
      allocate (s68(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k2
      i3 = k4 * k4
      call egemm(i1, i2, i3, d1, d2, s68)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * t2diag5
      call sum_stripe(4, shape(x3), size(x3), '3412', factor, &
                      x3, s68)
      deallocate (s68)
   end if

   if (t2diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2341', intb, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
      allocate (s69(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
      i1 = k4 * k4
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s69)
      deallocate (d1)
      deallocate (d2)

      factor = -t2diag3
      call sum_stripe(4, shape(x6), size(x6), '3412', factor, &
                      x6, s69)
      deallocate (s69)
   end if

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '2143', intr, d1)
      allocate (f2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_stripe(6, shape(t3b), size(t3b), '562134', t3b, f2)
      allocate (s70(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
      i1 = k3
      i2 = k2 * k3 * k4
      i3 = k3 * k1 * k1
      call egemm(i1, i2, i3, d1, f2, s70)
      deallocate (d1)
      deallocate (f2)

      factor = -0.500 * t3diag3
      call sum_stripe(4, shape(x2), size(x2), '2341', factor, &
                      x2, s70)
      deallocate (s70)
   end if

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2431', intr, d1)
      allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_stripe(6, shape(t3b), size(t3b), '523146', t3b, f2)
      allocate (s71(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1 * k2 * k4
      i3 = k3 * k3 * k1
      call egemm(i1, i2, i3, d1, f2, s71)
      deallocate (d1)
      deallocate (f2)

      factor = 0.500 * t3diag4
      call sum_stripe(4, shape(x1), size(x1), '2341', factor, &
                      x1, s71)
      deallocate (s71)
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
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
      allocate (s72(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s72)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag1
      call sum_stripe(4, shape(x7), size(x7), '3412', factor, &
                      x7, s72)
      deallocate (s72)
   end if

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
      allocate (f2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_stripe(6, shape(t3c), size(t3c), '461235', t3c, f2)
      allocate (s73(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
      i1 = k3
      i2 = k2 * k3 * k4
      i3 = k4 * k1 * k2
      call egemm(i1, i2, i3, d1, f2, s73)
      deallocate (d1)
      deallocate (f2)

      factor = -t3diag3
      call sum_stripe(4, shape(x2), size(x2), '2341', factor, &
                      x2, s73)
      deallocate (s73)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x2), &
                  size(x2), (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3214', 1.000, intm, x2)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x2,t2b) &
         !$omp private(a,b,c,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  sum = sum &
                        - x2(e, c, a, j) * t2b(b, e, k, i) & !cajbki (-1.000)
                        + x2(e, b, a, j) * t2b(c, e, k, i) & !bajcki (+1.000)
                        + x2(e, c, a, k) * t2b(b, e, j, i) & !cakbji (+1.000)
                        - x2(e, b, a, k) * t2b(c, e, j, i)     !bakcji (-1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x2)

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))                 !ilias: commented out 13 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1)
      allocate (f2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(6, shape(t3c), size(t3c), '463125', t3c, f2)
      allocate (s74(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
      i1 = k4
      i2 = k2 * k4 * k4
      i3 = k3 * k1 * k2
      call egemm(i1, i2, i3, d1, f2, s74)
      deallocate (d1)
      deallocate (f2)

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                       !ilias: commented out 13 lines for 3cc
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,s74,t2b,t3diag3) &
            !$omp private(a,b,c,f,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do f = n2 + 1, n3
                     sum = sum &
                           + s74(c, b, k, f) * t2b(f, a, j, i) & !cbkaji (+1.000)
                           - s74(c, b, j, f) * t2b(f, a, k, i)     !cbjaki (-1.000)
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + t3diag3 * sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (s74)                                               !ilias: commented out this line for 3cc
   end if

   if (t3diag5 .ne. 0) then
      allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
      allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
      allocate (s75(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
      i1 = k1 * k2
      i2 = k1 * k2
      i3 = k3 * k4
      call egemm(i1, i2, i3, d1, d2, s75)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag5
      call sum_stripe(4, shape(x13), size(x13), '3412', factor, &
                      x13, s75)
      deallocate (s75)

   end if
   call sum_shift(4, shape(intm), size(intm), shape(x13), &
                  size(x13), (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intm, x13)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x13,t3diag3) &
         !$omp private(a,b,c,m,n,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n2
                     sum = sum &
                           - x13(n, m, j, i) * t3c(c, b, a, n, k, m) & !jicbak (-1.000)
                           + x13(n, m, k, i) * t3c(c, b, a, n, j, m)   !kicbaj (+1.000)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x13)

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
      allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(6, shape(t3c), size(t3c), '613245', t3c, f2)
      allocate (s76(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2 * k2 * k4
      i3 = k3 * k4 * k1
      call egemm(i1, i2, i3, d1, f2, s76)
      deallocate (d1)
      deallocate (f2)

      factor = t3diag4
      call sum_stripe(4, shape(x31), size(x31), '2341', factor, &
                      x31, s76)
      deallocate (s76)

   end if

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x31,t2b,t3diag3) &
         !$omp private(a,b,c,n,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n2
                  sum = sum &
                        - x31(n, b, k, j) * t2b(c, a, n, i) & !bkjcai (-1.000)
                        + x31(n, c, k, j) * t2b(b, a, n, i)     !ckjbai (+1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x31)

   if (t3diag2 .ne. 0) then
      allocate (d1(n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '2314', intm, d1)
      allocate (d2(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3214', t2b, d2)
      allocate (s77(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3))
      i1 = k4 * k1
      i2 = k1 * k4
      i3 = k3 * k2
      call egemm(i1, i2, i3, d1, d2, s77)
      deallocate (d1)
      deallocate (d2)

      factor = -t3diag2
      call sum_stripe(4, shape(x14), size(x14), '3412', factor, &
                      x14, s77)
      deallocate (s77)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x14), &
                  size(x14), (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '1342', 1.000, intm, x14)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x14,t3diag3) &
         !$omp private(a,b,c,m,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           - x14(m, e, c, i) * t3c(e, b, a, k, j, m) & !cibakj (-1.000)
                           + x14(m, e, b, i) * t3c(e, c, a, k, j, m)   !bicakj (+1.000)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x14)

   if (t3diag1 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
      allocate (s78(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s78)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag1
      call sum_stripe(4, shape(x12), size(x12), '3412', factor, &
                      x12, s78)
      deallocate (s78)
   end if

   call sum_shift(4, shape(intr), size(intr), shape(x12), &
                  size(x12), (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '3142', 1.000, intr, x12)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x12,t3diag3) &
         !$omp private(a,b,c,m,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           + x12(m, e, a, i) * t3c(c, b, e, k, j, m) !aicbkj (+1.000)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x12)

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
      allocate (q19(n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1
      i3 = k3 * k4 * k2
      call egemm(i1, i2, i3, d1, d2, q19)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag4
      call sum_stripe(2, shape(x8), size(x8), '21', factor, x8, &
                      q19)
      deallocate (q19)
   end if

   call sum_shift(2, shape(fockr), size(fockr), shape(x8), &
                  size(x8), (/n0, n0/), '12', 1.000, fockr, x8)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x8,t3c,t3diag3) &
         !$omp private(a,b,c,m,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        - x8(m, i) * t3c(c, b, a, k, j, m)     !icbakj (-1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x8)

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2431', intm, d1)
      allocate (f2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_stripe(6, shape(t3c), size(t3c), '413256', t3c, f2)
      allocate (s79(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
      i1 = k1
      i2 = k1 * k2 * k4
      i3 = k3 * k4 * k2
      call egemm(i1, i2, i3, d1, f2, s79)
      deallocate (d1)
      deallocate (f2)

      factor = t3diag4
      call sum_stripe(4, shape(x1), size(x1), '2341', factor, &
                      x1, s79)
      deallocate (s79)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x1), &
                  size(x1), (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1243', 1.000, intm, x1)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x1,t2b,t3diag3) &
         !$omp private(a,b,c,m,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        + x1(m, c, j, i) * t2b(b, a, k, m) & !cjibak (+1.000)
                        - x1(m, b, j, i) * t2b(c, a, k, m) & !bjicak (-1.000)
                        - x1(m, c, k, i) * t2b(b, a, j, m) & !ckibaj (-1.000)
                        + x1(m, b, k, i) * t2b(c, a, j, m)     !bkicaj (+1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x1)

   if (t3diag1 .ne. 0) then
      allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n1 - n0, n0 - n0, n2 - n0/), '1324', intm, d1)
      allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
      allocate (s80(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k4
      i3 = k3 * k1
      call egemm(i1, i2, i3, d1, d2, s80)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag1
      call sum_stripe(4, shape(x18), size(x18), '3412', factor, &
                      x18, s80)
      deallocate (s80)
   end if

   if (t3diag2 .ne. 0) then
      allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '1423', intm, d1)
      allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
      allocate (s81(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3))
      i1 = k3 * k2
      i2 = k2 * k3
      i3 = k4 * k1
      call egemm(i1, i2, i3, d1, d2, s81)
      deallocate (d1)
      deallocate (d2)

      factor = -t3diag2
      call sum_stripe(4, shape(x15), size(x15), '3412', factor, &
                      x15, s81)
      deallocate (s81)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x15), &
                  size(x15), (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '3124', 1.000, intm, x15)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x15,t3diag3) &
         !$omp private(a,b,c,m,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           + x15(m, e, a, j) * t3c(c, b, e, m, k, i) & !ajcbki (+1.000)
                           - x15(m, e, a, k) * t3c(c, b, e, m, j, i)   !akcbji (-1.000)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x15)

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
      allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
      allocate (q20(n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2
      i3 = k3 * k4 * k1
      call egemm(i1, i2, i3, d1, d2, q20)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag4
      call sum_stripe(2, shape(x10), size(x10), '21', factor, &
                      x10, q20)
      deallocate (q20)
   end if

   if (t3diag5 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
      allocate (s82(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
      i1 = k3 * k4
      i2 = k3 * k4
      i3 = k1 * k2
      call egemm(i1, i2, i3, d1, d2, s82)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag5
      call sum_stripe(4, shape(x16), size(x16), '3412', factor, &
                      x16, s82)
      deallocate (s82)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x16), &
                  size(x16), (/n2 - n0, n1 - n0, n2 - n0, n1 - n0/), '4321', 1.000, intm, x16)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x16,t3c,t3diag3) &
         !$omp private(a,b,c,f,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do f = n2 + 1, n3
                     sum = sum &
                           + x16(f, e, c, a) * t3c(f, b, e, k, j, i) & !cabkji (+1.000)
                           - x16(f, e, b, a) * t3c(f, c, e, k, j, i)   !backji (-1.000)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x16)

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
      call reorder_stripe(4, shape(t2b), size(t2b), '3421', t2b, d2)
      allocate (q21(n2 + 1:n3, n2 + 1:n3))
      i1 = k4
      i2 = k4
      i3 = k3 * k1 * k2
      call egemm(i1, i2, i3, d1, d2, q21)
      deallocate (d1)
      deallocate (d2)

      factor = -t3diag3
      call sum_stripe(2, shape(x11), size(x11), '21', factor, &
                      x11, q21)
      deallocate (q21)

      allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '2143', intm, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
      call reorder_stripe(4, shape(t2b), size(t2b), '3412', t2b, d2)
      allocate (q22(n1 + 1:n3, n1 + 1:n3))
      i1 = k3
      i2 = k3
      i3 = k4 * k1 * k2
      call egemm(i1, i2, i3, d1, d2, q22)
      deallocate (d1)
      deallocate (d2)

      call sum_stripe(2, shape(x9), size(x9), '21', factor, x9, &
                      q22)
      deallocate (q22)
   end if

   call sum_shift(2, shape(fockr), size(fockr), shape(x9), &
                  size(x9), (/n1, n1/), '21', 1.000, fockr, x9)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x9,t3diag3) &
         !$omp private(a,b,c,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  sum = sum &
                        + x9(e, a) * t3c(c, b, e, k, j, i) !acbkji (+1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x9)

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
      allocate (f2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(6, shape(t3d), size(t3d), '451236', t3d, f2)
      allocate (s83(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
      i1 = k4
      i2 = k2 * k4 * k4
      i3 = k4 * k2 * k2
      call egemm(i1, i2, i3, d1, f2, s83)
      deallocate (d1)
      deallocate (f2)

      factor = -0.500 * t3diag3
      call sum_stripe(4, shape(x4), size(x4), '2341', factor, &
                      x4, s83)
      deallocate (s83)
   end if

   call sum_shift(4, shape(intb), size(intb), shape(x4), &
                  size(x4), (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '3241', 1.000, intb, x4)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x4,t2b,t3diag3) &
         !$omp private(a,b,c,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  sum = sum &
                        + x4(e, c, b, j) * t2b(e, a, k, i) & !cbjaki (+1.000)
                        - x4(e, c, b, k) * t2b(e, a, j, i)     !cbkaji (-1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x4)

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1)
      allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(6, shape(t3d), size(t3d), '412356', t3d, f2)
      allocate (s84(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2 * k2 * k4
      i3 = k4 * k4 * k2
      call egemm(i1, i2, i3, d1, f2, s84)
      deallocate (d1)
      deallocate (f2)

      factor = 0.500 * t3diag4
      call sum_stripe(4, shape(x3), size(x3), '2341', factor, &
                      x3, s84)
      deallocate (s84)
   end if

   call sum_shift(4, shape(intb), size(intb), shape(x3), &
                  size(x3), (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intb, x3)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x3,t2b,t3diag3) &
         !$omp private(a,b,c,m,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        + x3(m, c, k, j) * t2b(b, a, m, i) & !ckjbai (+1.000)
                        - x3(m, b, k, j) * t2b(c, a, m, i)     !bkjcai (-1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x3)

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
      allocate (s85(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k1 * k3
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s85)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag1
      call sum_stripe(4, shape(x20), size(x20), '3412', factor, &
                      x20, s85)
      deallocate (s85)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x20), &
                  size(x20), (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '3142', 1.000, intm, x20)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x20,t3d,t3diag3) &
         !$omp private(a,b,c,m,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           + x20(m, e, a, i) * t3d(e, c, b, m, k, j) !aicbkj (+1.000)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x20)

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))                 !ilias: commented out 13 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n1 - n0, n2 - n0/), '2134', intm, d1)
      allocate (f2(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(6, shape(t3b), size(t3b), '452136', t3b, f2)
      allocate (s86(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
      i1 = k4
      i2 = k1 * k3 * k4
      i3 = k3 * k1 * k2
      call egemm(i1, i2, i3, d1, f2, s86)
      deallocate (d1)
      deallocate (f2)

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2                       !ilias: commented out 13 lines for 3cc
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,s86,t2c,t3diag3) &
            !$omp private(a,b,c,f,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do f = n2 + 1, n3
                     sum = sum &
                           + s86(b, a, i, f) * t2c(f, c, k, j) & !baickj (+1.000)
                           - s86(c, a, i, f) * t2c(f, b, k, j)     !caibkj (-1.000)
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + t3diag3 * sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (s86)                                               !ilias: commented out this line for 3cc
   end if

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
      allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_stripe(6, shape(t3b), size(t3b), '512346', t3b, f2)
      allocate (s87(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
      i1 = k2
      i2 = k1 * k2 * k3
      i3 = k3 * k4 * k1
      call egemm(i1, i2, i3, d1, f2, s87)
      deallocate (d1)
      deallocate (f2)

      do i = n0 + 1, n1
         do j = n0 + 1, n2 - 1
         do k = j + 1, n2
            if (indocc(k, j, i) .eq. 1) cycle
            !$omp parallel default(none), &
            !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t2c,s87,t3diag4) &
            !$omp private(a,b,c,n,sum)
            !$omp do
            do a = n1 + 1, n3
               do b = n2 + 1, n3 - 1
               do c = b + 1, n3
                  if (indunocc(c, b, a) .eq. 1) cycle
                  sum = 0.0d0
                  do n = n0 + 1, n2
                     sum = sum &
                           - s87(a, k, i, n) * t2c(c, b, n, j) & !akicbj (-1.000)
                           + s87(a, j, i, n) * t2c(c, b, n, k)     !ajicbk (+1.000)
                  end do
                  v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + t3diag4 * sum
               end do
               end do
            end do
            !$omp end do
            !$omp end parallel
         end do
         end do
      end do

      deallocate (s87)
   end if

   if (t3diag1 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n1, n1 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n0 - n0, n1 - n0/), '2413', intm, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (s88(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3))
      i1 = k3 * k1
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s88)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag1
      call sum_stripe(4, shape(x7), size(x7), '3412', factor, &
                      x7, s88)
      deallocate (s88)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x7), &
                  size(x7), (/n0 - n0, n1 - n0, n2 - n0, n0 - n0/), '1324', 1.000, intm, x7)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x7,t3b,t3diag4) &
         !$omp private(a,b,c,m,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           - x7(m, e, c, j) * t3b(b, e, a, k, m, i) & !cjbaki (-1.000)
                           + x7(m, e, b, j) * t3b(c, e, a, k, m, i) & !bjcaki (+1.000)
                           + x7(m, e, c, k) * t3b(b, e, a, j, m, i) & !ckbaji (+1.000)
                           - x7(m, e, b, k) * t3b(c, e, a, j, m, i)   !bkcaji (-1.000)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x7)

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
      allocate (f2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
      call reorder_stripe(6, shape(t3c), size(t3c), '451236', t3c, f2)
      allocate (s89(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
      i1 = k4
      i2 = k1 * k3 * k4
      i3 = k4 * k2 * k2
      call egemm(i1, i2, i3, d1, f2, s89)
      deallocate (d1)
      deallocate (f2)

      factor = -0.500 * t3diag3
      call sum_stripe(4, shape(x6), size(x6), '2341', factor, &
                      x6, s89)
      deallocate (s89)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x6), &
                  size(x6), (/n2 - n0, n2 - n0, n1 - n0, n0 - n0/), '3241', 1.000, intm, x6)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t2c,x6,t3diag4) &
         !$omp private(a,b,c,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  sum = sum &
                        + x6(e, c, a, i) * t2c(e, b, k, j) & !caibkj (+1.000)
                        - x6(e, b, a, i) * t2c(e, c, k, j)     !baickj (-1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x6)

   if (t3diag5 .ne. 0) then
      allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
      allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
      allocate (s90(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
      i1 = k2 * k2
      i2 = k2 * k2
      i3 = k4 * k4
      call egemm(i1, i2, i3, d1, d2, s90)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * t3diag5
      call sum_stripe(4, shape(x17), size(x17), '3412', factor, &
                      x17, s90)
      deallocate (s90)
   end if

   call sum_shift(4, shape(intb), size(intb), shape(x17), &
                  size(x17), (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intb, x17)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x17,t3diag4) &
         !$omp private(a,b,c,m,n,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  do n = n0 + 1, n2
                     sum = sum &
                           + (x17(n, m, k, j) * t3c(c, b, a, n, m, i)) / 2.0d0!kjcbai(+0.500)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x17)

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2431', intb, d1)
      allocate (f2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
      call reorder_stripe(6, shape(t3c), size(t3c), '412356', t3c, f2)
      allocate (s91(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
      i1 = k2
      i2 = k1 * k2 * k3
      i3 = k4 * k4 * k2
      call egemm(i1, i2, i3, d1, f2, s91)
      deallocate (d1)
      deallocate (f2)

      factor = 0.500 * t3diag4
      call sum_stripe(4, shape(x5), size(x5), '2341', factor, &
                      x5, s91)
      deallocate (s91)
   end if

   call sum_shift(4, shape(intm), size(intm), shape(x5), &
                  size(x5), (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intm, x5)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t2c,x5,t3diag4) &
         !$omp private(a,b,c,m,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        + x5(m, a, j, i) * t2c(c, b, m, k) & !ajicbk (+1.000)
                        - x5(m, a, k, i) * t2c(c, b, m, j)     !akicbj (-1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x5)

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
      allocate (s92(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
      i1 = k4 * k2
      i2 = k2 * k4
      i3 = k4 * k2
      call egemm(i1, i2, i3, d1, d2, s92)
      deallocate (d1)
      deallocate (d2)

      factor = t3diag1
      call sum_stripe(4, shape(x18), size(x18), '3412', factor, &
                      x18, s92)
      deallocate (s92)
   end if

   call sum_shift(4, shape(intb), size(intb), shape(x18), &
                  size(x18), (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '3142', 1.000, intb, x18)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x18,t3diag4) &
         !$omp private(a,b,c,m,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           - x18(m, e, c, j) * t3c(e, b, a, m, k, i) & !cjbaki (-1.000)
                           + x18(m, e, b, j) * t3c(e, c, a, m, k, i) & !bjcaki (+1.000)
                           + x18(m, e, c, k) * t3c(e, b, a, m, j, i) & !ckbaji (+1.000)
                           - x18(m, e, b, k) * t3c(e, c, a, m, j, i)   !bkcaji (-1.000)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x18)

   if (t3diag4 .ne. 0) then
      allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '1432', intb, d1)
      allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
      call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
      allocate (q23(n0 + 1:n2, n0 + 1:n2))
      i1 = k2
      i2 = k2
      i3 = k4 * k4 * k2
      call egemm(i1, i2, i3, d1, d2, q23)
      deallocate (d1)
      deallocate (d2)

      factor = -0.500 * t3diag4
      call sum_stripe(2, shape(x10), size(x10), '21', factor, &
                      x10, q23)
      deallocate (q23)
   end if

   call sum_shift(2, shape(fockb), size(fockb), shape(x10), &
                  size(x10), (/n0, n0/), '12', 1.000, fockb, x10)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,x10,t3c,t3diag4) &
         !$omp private(a,b,c,m,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        + x10(m, j) * t3c(c, b, a, m, k, i) & !jcbaki (+1.000)
                        - x10(m, k) * t3c(c, b, a, m, j, i)     !kcbaji (-1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x10)

   if (t3diag5 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2143', intb, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
      allocate (s93(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
      i1 = k4 * k4
      i2 = k4 * k4
      i3 = k2 * k2
      call egemm(i1, i2, i3, d1, d2, s93)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * t3diag5
      call sum_stripe(4, shape(x19), size(x19), '3412', factor, &
                      x19, s93)
      deallocate (s93)
   end if

   call sum_shift(4, shape(intb), size(intb), shape(x19), &
                  size(x19), (/n2 - n0, n2 - n0, n2 - n0, n2 - n0/), '4321', 1.000, intb, x19)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x19,t3diag4) &
         !$omp private(a,b,c,f,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  do f = n2 + 1, n3
                     sum = sum &
                           + (x19(f, e, c, b) * t3c(f, e, a, k, j, i)) / 2.0d0!cbakji (+0.500)
                  end do
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x19)

   if (t3diag3 .ne. 0) then
      allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))                 !ilias: commented out 16 lines for 3cc
      call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                         (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '2134', intb, d1)
      allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
      call reorder_stripe(4, shape(t2c), size(t2c), '3412', t2c, d2)
      allocate (q24(n2 + 1:n3, n2 + 1:n3))
      i1 = k4
      i2 = k4
      i3 = k4 * k2 * k2
      call egemm(i1, i2, i3, d1, d2, q24)
      deallocate (d1)
      deallocate (d2)

      factor = 0.500 * t3diag3
      call sum_stripe(2, shape(x11), size(x11), '21', factor, &
                      x11, q24)
      deallocate (q24)
   end if

   call sum_shift(2, shape(fockb), size(fockb), shape(x11), &
                  size(x11), (/n2, n2/), '21', 1.000, fockb, x11)

   do i = n0 + 1, n1
      do j = n0 + 1, n2 - 1
      do k = j + 1, n2
         if (indocc(k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,indunocc,v3c,t3c,x11,t3diag4) &
         !$omp private(a,b,c,e,sum)
         !$omp do
         do a = n1 + 1, n3
            do b = n2 + 1, n3 - 1
            do c = b + 1, n3
               if (indunocc(c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  sum = sum &
                        + x11(e, c) * t3c(e, b, a, k, j, i) & !cbakji (+1.000)
                        - x11(e, b) * t3c(e, c, a, k, j, i)     !bcakji (-1.000)
               end do
               v3c(c, b, a, k, j, i) = v3c(c, b, a, k, j, i) + sum
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
   end do

   deallocate (x11)

   do i = n0 + 1, n1
   do j = n0 + 1, n2 - 1
   do k = j + 1, n2
   do a = n1 + 1, n3
   do b = n2 + 1, n3 - 1
   do c = b + 1, n3
      coeleft = fockb(c, c) &
                + fockb(b, b) &
                + fockr(a, a) &
                - fockb(k, k) &
                - fockb(j, j) &
                - fockr(i, i) &
                + shift
      t3c(c, b, a, k, j, i) = t3c(c, b, a, k, j, i) - v3c(c, b, a, k, j, i) / coeleft
      t3c(c, b, a, j, k, i) = -t3c(c, b, a, k, j, i)
      t3c(b, c, a, k, j, i) = -t3c(c, b, a, k, j, i)
      t3c(b, c, a, j, k, i) = t3c(c, b, a, k, j, i)
   end do
   end do
   end do
   end do
   end do
   end do

end subroutine t3c_update

