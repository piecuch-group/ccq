subroutine t4b_update(n0, n1, n2, n3, k1, k2, k3, k4, shift, &
                      fockr, fockb, intr, intb, intm, &
                      t1a, t1b, &
                      t2a, t2b, t2c, &
                      t3a, t3b, t3c, &
                      iactocca, iactoccb, iactunoa, iactunob, iactindq)

   implicit none

   integer, intent(in) :: n0, n1, n2, n3, k1, k2, k3, k4

   integer :: i1, i2, i3
   integer :: a, b, c, d, e, f, i, j, k, l, m, n
   integer :: iactocca, iactoccb, iactunoa, iactunob, iactindq
   integer :: iocca, ioccb, iunoa, iunob
   integer, allocatable :: indocc(:, :, :, :)
   integer, allocatable :: indunocc(:, :, :, :)
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
   real(kind=8) :: t3a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1)
   real(kind=8) :: t3b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1)
   real(kind=8) :: t3c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1)
   real(kind=8), allocatable :: t4a(:, :, :, :, :, :, :, :)
   real(kind=8), allocatable :: t4b(:, :, :, :, :, :, :, :)
   real(kind=8), allocatable :: t4c(:, :, :, :, :, :, :, :)
   real(kind=8), allocatable :: v4b(:, :, :, :, :, :, :, :)

   real(kind=8), allocatable :: b1(:, :)
   real(kind=8), allocatable :: b2(:, :)
   real(kind=8), allocatable :: d1(:, :, :, :)
   real(kind=8), allocatable :: d2(:, :, :, :)
   real(kind=8), allocatable :: f1(:, :, :, :, :, :)
   real(kind=8), allocatable :: f2(:, :, :, :, :, :)
   real(kind=8), allocatable :: h2(:, :, :, :, :, :, :, :)

   integer :: ta, tb, tc, td, te
   parameter(ta=29, tb=30, tc=31, td=32, te=33)

   real(kind=8), allocatable :: s1(:, :, :, :)
   real(kind=8), allocatable :: s2(:, :, :, :)
   real(kind=8), allocatable :: s3(:, :, :, :)
   real(kind=8), allocatable :: s4(:, :, :, :)
   real(kind=8), allocatable :: s5(:, :, :, :)
   real(kind=8), allocatable :: s6(:, :, :, :)
   real(kind=8), allocatable :: s7(:, :, :, :)
   real(kind=8), allocatable :: s8(:, :, :, :)
   real(kind=8), allocatable :: s9(:, :, :, :)
   real(kind=8), allocatable :: s10(:, :, :, :)
   real(kind=8), allocatable :: q1(:, :)
   real(kind=8), allocatable :: q2(:, :)
   real(kind=8), allocatable :: s11(:, :, :, :)
   real(kind=8), allocatable :: u55(:, :, :, :, :, :)
   real(kind=8), allocatable :: s89(:, :, :, :)
   real(kind=8), allocatable :: s12(:, :, :, :)
   real(kind=8), allocatable :: u57(:, :, :, :, :, :)
   real(kind=8), allocatable :: u56(:, :, :, :, :, :)
   real(kind=8), allocatable :: s90(:, :, :, :)
   real(kind=8), allocatable :: q3(:, :)
   real(kind=8), allocatable :: s13(:, :, :, :)
   real(kind=8), allocatable :: u59(:, :, :, :, :, :)
   real(kind=8), allocatable :: u58(:, :, :, :, :, :)
   real(kind=8), allocatable :: s92(:, :, :, :)
   real(kind=8), allocatable :: s91(:, :, :, :)
   real(kind=8), allocatable :: s14(:, :, :, :)
   real(kind=8), allocatable :: u60(:, :, :, :, :, :)
   real(kind=8), allocatable :: q4(:, :)
   real(kind=8), allocatable :: s15(:, :, :, :)
   real(kind=8), allocatable :: u61(:, :, :, :, :, :)
   real(kind=8), allocatable :: s102(:, :, :, :)
   real(kind=8), allocatable :: s93(:, :, :, :)
   real(kind=8), allocatable :: s16(:, :, :, :)
   real(kind=8), allocatable :: u62(:, :, :, :, :, :)
   real(kind=8), allocatable :: s103(:, :, :, :)
   real(kind=8), allocatable :: s94(:, :, :, :)
   real(kind=8), allocatable :: s17(:, :, :, :)
   real(kind=8), allocatable :: u63(:, :, :, :, :, :)
   real(kind=8), allocatable :: s104(:, :, :, :)
   real(kind=8), allocatable :: q5(:, :)
   real(kind=8), allocatable :: s18(:, :, :, :)
   real(kind=8), allocatable :: u64(:, :, :, :, :, :)
   real(kind=8), allocatable :: s105(:, :, :, :)
   real(kind=8), allocatable :: q6(:, :)
   real(kind=8), allocatable :: s19(:, :, :, :)
   real(kind=8), allocatable :: u71(:, :, :, :, :, :)
   real(kind=8), allocatable :: s107(:, :, :, :)
   real(kind=8), allocatable :: s106(:, :, :, :)
   real(kind=8), allocatable :: s20(:, :, :, :)
   real(kind=8), allocatable :: u72(:, :, :, :, :, :)
   real(kind=8), allocatable :: s109(:, :, :, :)
   real(kind=8), allocatable :: s108(:, :, :, :)
   real(kind=8), allocatable :: s21(:, :, :, :)
   real(kind=8), allocatable :: s22(:, :, :, :)
   real(kind=8), allocatable :: s23(:, :, :, :)
   real(kind=8), allocatable :: s24(:, :, :, :)
   real(kind=8), allocatable :: s25(:, :, :, :)
   real(kind=8), allocatable :: s26(:, :, :, :)
   real(kind=8), allocatable :: s27(:, :, :, :)
   real(kind=8), allocatable :: u83(:, :, :, :, :, :)
   real(kind=8), allocatable :: s28(:, :, :, :)
   real(kind=8), allocatable :: u84(:, :, :, :, :, :)
   real(kind=8), allocatable :: q7(:, :)
   real(kind=8), allocatable :: q8(:, :)
   real(kind=8), allocatable :: s29(:, :, :, :)
   real(kind=8), allocatable :: u85(:, :, :, :, :, :)
   real(kind=8), allocatable :: s136(:, :, :, :)
   real(kind=8), allocatable :: s30(:, :, :, :)
   real(kind=8), allocatable :: u86(:, :, :, :, :, :)
   real(kind=8), allocatable :: q9(:, :)
   real(kind=8), allocatable :: s31(:, :, :, :)
   real(kind=8), allocatable :: u87(:, :, :, :, :, :)
   real(kind=8), allocatable :: s137(:, :, :, :)
   real(kind=8), allocatable :: s32(:, :, :, :)
   real(kind=8), allocatable :: u88(:, :, :, :, :, :)
   real(kind=8), allocatable :: q10(:, :)
   real(kind=8), allocatable :: s33(:, :, :, :)
   real(kind=8), allocatable :: q11(:, :)
   real(kind=8), allocatable :: s34(:, :, :, :)
   real(kind=8), allocatable :: q12(:, :)
   real(kind=8), allocatable :: u1(:, :, :, :, :, :)
   real(kind=8), allocatable :: u2(:, :, :, :, :, :)
   real(kind=8), allocatable :: u3(:, :, :, :, :, :)
   real(kind=8), allocatable :: u4(:, :, :, :, :, :)
   real(kind=8), allocatable :: u5(:, :, :, :, :, :)
   real(kind=8), allocatable :: u6(:, :, :, :, :, :)
   real(kind=8), allocatable :: u7(:, :, :, :, :, :)
   real(kind=8), allocatable :: u8(:, :, :, :, :, :)
   real(kind=8), allocatable :: u9(:, :, :, :, :, :)
   real(kind=8), allocatable :: s35(:, :, :, :)
   real(kind=8), allocatable :: s36(:, :, :, :)
   real(kind=8), allocatable :: u10(:, :, :, :, :, :)
   real(kind=8), allocatable :: u11(:, :, :, :, :, :)
   real(kind=8), allocatable :: s37(:, :, :, :)
   real(kind=8), allocatable :: s38(:, :, :, :)
   real(kind=8), allocatable :: u12(:, :, :, :, :, :)
   real(kind=8), allocatable :: s39(:, :, :, :)
   real(kind=8), allocatable :: u13(:, :, :, :, :, :)
   real(kind=8), allocatable :: s40(:, :, :, :)
   real(kind=8), allocatable :: u14(:, :, :, :, :, :)
   real(kind=8), allocatable :: u15(:, :, :, :, :, :)
   real(kind=8), allocatable :: u16(:, :, :, :, :, :)
   real(kind=8), allocatable :: s41(:, :, :, :)
   real(kind=8), allocatable :: u17(:, :, :, :, :, :)
   real(kind=8), allocatable :: s42(:, :, :, :)
   real(kind=8), allocatable :: u18(:, :, :, :, :, :)
   real(kind=8), allocatable :: u19(:, :, :, :, :, :)
   real(kind=8), allocatable :: u20(:, :, :, :, :, :)
   real(kind=8), allocatable :: u100(:, :, :, :, :, :)
   real(kind=8), allocatable :: u98(:, :, :, :, :, :)
   real(kind=8), allocatable :: u97(:, :, :, :, :, :)
   real(kind=8), allocatable :: s119(:, :, :, :)
   real(kind=8), allocatable :: u67(:, :, :, :, :, :)
   real(kind=8), allocatable :: u115(:, :, :, :, :, :)
   real(kind=8), allocatable :: u65(:, :, :, :, :, :)
   real(kind=8), allocatable :: u113(:, :, :, :, :, :)
   real(kind=8), allocatable :: s43(:, :, :, :)
   real(kind=8), allocatable :: u99(:, :, :, :, :, :)
   real(kind=8), allocatable :: s118(:, :, :, :)
   real(kind=8), allocatable :: u21(:, :, :, :, :, :)
   real(kind=8), allocatable :: s44(:, :, :, :)
   real(kind=8), allocatable :: u101(:, :, :, :, :, :)
   real(kind=8), allocatable :: s120(:, :, :, :)
   real(kind=8), allocatable :: s116(:, :, :, :)
   real(kind=8), allocatable :: q13(:, :)
   real(kind=8), allocatable :: s45(:, :, :, :)
   real(kind=8), allocatable :: s117(:, :, :, :)
   real(kind=8), allocatable :: q14(:, :)
   real(kind=8), allocatable :: u22(:, :, :, :, :, :)
   real(kind=8), allocatable :: u105(:, :, :, :, :, :)
   real(kind=8), allocatable :: u104(:, :, :, :, :, :)
   real(kind=8), allocatable :: u103(:, :, :, :, :, :)
   real(kind=8), allocatable :: u102(:, :, :, :, :, :)
   real(kind=8), allocatable :: s141(:, :, :, :)
   real(kind=8), allocatable :: u90(:, :, :, :, :, :)
   real(kind=8), allocatable :: u89(:, :, :, :, :, :)
   real(kind=8), allocatable :: u121(:, :, :, :, :, :)
   real(kind=8), allocatable :: u70(:, :, :, :, :, :)
   real(kind=8), allocatable :: u120(:, :, :, :, :, :)
   real(kind=8), allocatable :: u119(:, :, :, :, :, :)
   real(kind=8), allocatable :: u23(:, :, :, :, :, :)
   real(kind=8), allocatable :: s46(:, :, :, :)
   real(kind=8), allocatable :: u109(:, :, :, :, :, :)
   real(kind=8), allocatable :: s143(:, :, :, :)
   real(kind=8), allocatable :: s142(:, :, :, :)
   real(kind=8), allocatable :: u24(:, :, :, :, :, :)
   real(kind=8), allocatable :: s47(:, :, :, :)
   real(kind=8), allocatable :: s48(:, :, :, :)
   real(kind=8), allocatable :: u25(:, :, :, :, :, :)
   real(kind=8), allocatable :: u26(:, :, :, :, :, :)
   real(kind=8), allocatable :: s49(:, :, :, :)
   real(kind=8), allocatable :: u27(:, :, :, :, :, :)
   real(kind=8), allocatable :: u28(:, :, :, :, :, :)
   real(kind=8), allocatable :: s50(:, :, :, :)
   real(kind=8), allocatable :: u29(:, :, :, :, :, :)
   real(kind=8), allocatable :: s51(:, :, :, :)
   real(kind=8), allocatable :: u30(:, :, :, :, :, :)
   real(kind=8), allocatable :: s52(:, :, :, :)
   real(kind=8), allocatable :: s53(:, :, :, :)
   real(kind=8), allocatable :: s54(:, :, :, :)
   real(kind=8), allocatable :: s55(:, :, :, :)
   real(kind=8), allocatable :: s56(:, :, :, :)
   real(kind=8), allocatable :: u31(:, :, :, :, :, :)
   real(kind=8), allocatable :: u32(:, :, :, :, :, :)
   real(kind=8), allocatable :: s57(:, :, :, :)
   real(kind=8), allocatable :: u33(:, :, :, :, :, :)
   real(kind=8), allocatable :: s58(:, :, :, :)
   real(kind=8), allocatable :: s59(:, :, :, :)
   real(kind=8), allocatable :: u34(:, :, :, :, :, :)
   real(kind=8), allocatable :: u35(:, :, :, :, :, :)
   real(kind=8), allocatable :: s60(:, :, :, :)
   real(kind=8), allocatable :: u36(:, :, :, :, :, :)
   real(kind=8), allocatable :: s61(:, :, :, :)
   real(kind=8), allocatable :: s62(:, :, :, :)
   real(kind=8), allocatable :: u37(:, :, :, :, :, :)
   real(kind=8), allocatable :: s63(:, :, :, :)
   real(kind=8), allocatable :: u38(:, :, :, :, :, :)
   real(kind=8), allocatable :: s64(:, :, :, :)
   real(kind=8), allocatable :: u39(:, :, :, :, :, :)
   real(kind=8), allocatable :: s123(:, :, :, :)
   real(kind=8), allocatable :: u75(:, :, :, :, :, :)
   real(kind=8), allocatable :: u73(:, :, :, :, :, :)
   real(kind=8), allocatable :: u114(:, :, :, :, :, :)
   real(kind=8), allocatable :: u40(:, :, :, :, :, :)
   real(kind=8), allocatable :: s65(:, :, :, :)
   real(kind=8), allocatable :: s124(:, :, :, :)
   real(kind=8), allocatable :: s122(:, :, :, :)
   real(kind=8), allocatable :: u41(:, :, :, :, :, :)
   real(kind=8), allocatable :: u106(:, :, :, :, :, :)
   real(kind=8), allocatable :: s147(:, :, :, :)
   real(kind=8), allocatable :: u80(:, :, :, :, :, :)
   real(kind=8), allocatable :: u42(:, :, :, :, :, :)
   real(kind=8), allocatable :: u107(:, :, :, :, :, :)
   real(kind=8), allocatable :: u94(:, :, :, :, :, :)
   real(kind=8), allocatable :: s130(:, :, :, :)
   real(kind=8), allocatable :: u81(:, :, :, :, :, :)
   real(kind=8), allocatable :: u77(:, :, :, :, :, :)
   real(kind=8), allocatable :: u118(:, :, :, :, :, :)
   real(kind=8), allocatable :: u116(:, :, :, :, :, :)
   real(kind=8), allocatable :: s66(:, :, :, :)
   real(kind=8), allocatable :: u108(:, :, :, :, :, :)
   real(kind=8), allocatable :: s146(:, :, :, :)
   real(kind=8), allocatable :: s129(:, :, :, :)
   real(kind=8), allocatable :: u43(:, :, :, :, :, :)
   real(kind=8), allocatable :: s67(:, :, :, :)
   real(kind=8), allocatable :: s145(:, :, :, :)
   real(kind=8), allocatable :: s131(:, :, :, :)
   real(kind=8), allocatable :: s68(:, :, :, :)
   real(kind=8), allocatable :: s133(:, :, :, :)
   real(kind=8), allocatable :: s126(:, :, :, :)
   real(kind=8), allocatable :: q15(:, :)
   real(kind=8), allocatable :: u44(:, :, :, :, :, :)
   real(kind=8), allocatable :: s69(:, :, :, :)
   real(kind=8), allocatable :: s70(:, :, :, :)
   real(kind=8), allocatable :: s149(:, :, :, :)
   real(kind=8), allocatable :: s127(:, :, :, :)
   real(kind=8), allocatable :: q16(:, :)
   real(kind=8), allocatable :: s71(:, :, :, :)
   real(kind=8), allocatable :: s148(:, :, :, :)
   real(kind=8), allocatable :: s128(:, :, :, :)
   real(kind=8), allocatable :: q17(:, :)
   real(kind=8), allocatable :: q18(:, :)
   real(kind=8), allocatable :: u45(:, :, :, :, :, :)
   real(kind=8), allocatable :: u111(:, :, :, :, :, :)
   real(kind=8), allocatable :: s152(:, :, :, :)
   real(kind=8), allocatable :: u96(:, :, :, :, :, :)
   real(kind=8), allocatable :: u46(:, :, :, :, :, :)
   real(kind=8), allocatable :: s72(:, :, :, :)
   real(kind=8), allocatable :: s153(:, :, :, :)
   real(kind=8), allocatable :: s151(:, :, :, :)
   real(kind=8), allocatable :: s73(:, :, :, :)
   real(kind=8), allocatable :: s74(:, :, :, :)
   real(kind=8), allocatable :: s75(:, :, :, :)
   real(kind=8), allocatable :: s135(:, :, :, :)
   real(kind=8), allocatable :: s134(:, :, :, :)
   real(kind=8), allocatable :: s76(:, :, :, :)
   real(kind=8), allocatable :: q19(:, :)
   real(kind=8), allocatable :: q20(:, :)
   real(kind=8), allocatable :: s77(:, :, :, :)
   real(kind=8), allocatable :: u47(:, :, :, :, :, :)
   real(kind=8), allocatable :: u76(:, :, :, :, :, :)
   real(kind=8), allocatable :: s78(:, :, :, :)
   real(kind=8), allocatable :: u48(:, :, :, :, :, :)
   real(kind=8), allocatable :: u74(:, :, :, :, :, :)
   real(kind=8), allocatable :: s79(:, :, :, :)
   real(kind=8), allocatable :: u49(:, :, :, :, :, :)
   real(kind=8), allocatable :: u68(:, :, :, :, :, :)
   real(kind=8), allocatable :: s80(:, :, :, :)
   real(kind=8), allocatable :: s81(:, :, :, :)
   real(kind=8), allocatable :: s82(:, :, :, :)
   real(kind=8), allocatable :: u50(:, :, :, :, :, :)
   real(kind=8), allocatable :: u93(:, :, :, :, :, :)
   real(kind=8), allocatable :: s83(:, :, :, :)
   real(kind=8), allocatable :: s84(:, :, :, :)
   real(kind=8), allocatable :: u51(:, :, :, :, :, :)
   real(kind=8), allocatable :: u92(:, :, :, :, :, :)
   real(kind=8), allocatable :: u82(:, :, :, :, :, :)
   real(kind=8), allocatable :: s85(:, :, :, :)
   real(kind=8), allocatable :: u52(:, :, :, :, :, :)
   real(kind=8), allocatable :: u78(:, :, :, :, :, :)
   real(kind=8), allocatable :: u53(:, :, :, :, :, :)
   real(kind=8), allocatable :: s86(:, :, :, :)
   real(kind=8), allocatable :: s87(:, :, :, :)
   real(kind=8), allocatable :: s88(:, :, :, :)
   real(kind=8), allocatable :: u54(:, :, :, :, :, :)
   real(kind=8), allocatable :: u95(:, :, :, :, :, :)
   real(kind=8), allocatable :: s95(:, :, :, :)
   real(kind=8), allocatable :: u66(:, :, :, :, :, :)
   real(kind=8), allocatable :: q21(:, :)
   real(kind=8), allocatable :: s97(:, :, :, :)
   real(kind=8), allocatable :: s156(:, :, :, :)
   real(kind=8), allocatable :: s96(:, :, :, :)
   real(kind=8), allocatable :: u112(:, :, :, :, :, :)
   real(kind=8), allocatable :: s155(:, :, :, :)
   real(kind=8), allocatable :: s98(:, :, :, :)
   real(kind=8), allocatable :: q22(:, :)
   real(kind=8), allocatable :: s99(:, :, :, :)
   real(kind=8), allocatable :: s100(:, :, :, :)
   real(kind=8), allocatable :: u79(:, :, :, :, :, :)
   real(kind=8), allocatable :: u69(:, :, :, :, :, :)
   real(kind=8), allocatable :: q23(:, :)
   real(kind=8), allocatable :: s111(:, :, :, :)
   real(kind=8), allocatable :: s110(:, :, :, :)
   real(kind=8), allocatable :: u117(:, :, :, :, :, :)
   real(kind=8), allocatable :: s159(:, :, :, :)
   real(kind=8), allocatable :: s101(:, :, :, :)
   real(kind=8), allocatable :: s158(:, :, :, :)
   real(kind=8), allocatable :: s157(:, :, :, :)
   real(kind=8), allocatable :: s112(:, :, :, :)
   real(kind=8), allocatable :: u91(:, :, :, :, :, :)
   real(kind=8), allocatable :: s138(:, :, :, :)
   real(kind=8), allocatable :: q24(:, :)
   real(kind=8), allocatable :: s113(:, :, :, :)
   real(kind=8), allocatable :: s160(:, :, :, :)
   real(kind=8), allocatable :: s114(:, :, :, :)
   real(kind=8), allocatable :: q27(:, :)
   real(kind=8), allocatable :: s115(:, :, :, :)
   real(kind=8), allocatable :: q25(:, :)
   real(kind=8), allocatable :: s132(:, :, :, :)
   real(kind=8), allocatable :: q26(:, :)
   real(kind=8), allocatable :: q28(:, :)
   real(kind=8), allocatable :: s125(:, :, :, :)
   real(kind=8), allocatable :: s121(:, :, :, :)
   real(kind=8), allocatable :: s139(:, :, :, :)
   real(kind=8), allocatable :: q29(:, :)
   real(kind=8), allocatable :: s140(:, :, :, :)
   real(kind=8), allocatable :: q30(:, :)
   real(kind=8), allocatable :: s154(:, :, :, :)
   real(kind=8), allocatable :: q31(:, :)
   real(kind=8), allocatable :: q32(:, :)
   real(kind=8), allocatable :: s150(:, :, :, :)
   real(kind=8), allocatable :: s144(:, :, :, :)
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
   real(kind=8), allocatable :: x21(:, :, :, :)
   real(kind=8), allocatable :: x22(:, :, :, :)
   real(kind=8), allocatable :: x23(:, :, :, :, :, :)
   real(kind=8), allocatable :: x24(:, :, :, :, :, :)
   real(kind=8), allocatable :: x25(:, :, :, :, :, :)
   real(kind=8), allocatable :: x26(:, :, :, :, :, :)
   real(kind=8), allocatable :: x27(:, :, :, :, :, :)
   real(kind=8), allocatable :: x28(:, :, :, :, :, :)
   real(kind=8), allocatable :: x29(:, :, :, :, :, :)
   real(kind=8), allocatable :: x30(:, :, :, :, :, :)
   real(kind=8), allocatable :: x31(:, :, :, :, :, :)
   real(kind=8), allocatable :: x32(:, :, :, :, :, :)
   real(kind=8), allocatable :: x33(:, :, :, :, :, :)
   real(kind=8), allocatable :: x34(:, :, :, :, :, :)
   real(kind=8), allocatable :: x35(:, :, :, :, :, :)
   real(kind=8), allocatable :: x36(:, :, :, :, :, :)
   real(kind=8), allocatable :: x37(:, :, :, :, :, :)
   real(kind=8), allocatable :: x38(:, :, :, :, :, :)
   real(kind=8), allocatable :: x39(:, :, :, :, :, :)
   real(kind=8), allocatable :: x40(:, :, :, :, :, :)
   real(kind=8), allocatable :: x41(:, :, :, :, :, :)
   real(kind=8), allocatable :: x42(:, :, :, :, :, :)
   real(kind=8), allocatable :: x43(:, :, :, :, :, :)
   real(kind=8), allocatable :: x44(:, :, :, :, :, :)
   real(kind=8), allocatable :: x45(:, :, :, :, :, :)
   real(kind=8), allocatable :: x46(:, :, :, :, :, :)
   real(kind=8), allocatable :: x47(:, :, :, :, :, :)
   real(kind=8), allocatable :: x48(:, :, :, :, :, :)
   real(kind=8), allocatable :: x49(:, :, :, :, :, :)
   real(kind=8), allocatable :: x50(:, :, :, :, :, :)
   real(kind=8), allocatable :: x51(:, :, :, :)
   real(kind=8), allocatable :: x52(:, :, :, :)
   real(kind=8), allocatable :: x53(:, :, :, :, :, :)

   allocate (t4a(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                 n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   allocate (t4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                 n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                 n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   allocate (v4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                 n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))

   rewind (ta)
   rewind (tb)
   rewind (tc)
   read (ta) t4a
   read (tb) t4b
   read (tc) t4c

   allocate (indocc(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   allocate (indunocc(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   indocc = 0
   indunocc = 0
   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         iocca = 0
         ioccb = 0
         if (i .gt. (n1 - iactocca)) iocca = iocca + 1
         if (j .gt. (n1 - iactocca)) iocca = iocca + 1
         if (k .gt. (n1 - iactocca)) iocca = iocca + 1
         if (l .gt. (n2 - iactoccb)) ioccb = ioccb + 1
         if (iocca + ioccb .lt. iactindq) indocc(l, k, j, i) = 1
      end do
      end do
      end do
   end do
   do a = n1 + 1, n3 - 2
      do b = a + 1, n3 - 1
      do c = b + 1, n3
      do d = n2 + 1, n3
         iunoa = 0
         iunob = 0
         if (a .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
         if (b .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
         if (c .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
         if (d .lt. (n2 + iactunob + 1)) iunob = iunob + 1
         if (iunoa + iunob .lt. iactindq) indunocc(d, c, b, a) = 1
      end do
      end do
      end do
   end do

   v4b = 0.0d0

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

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s3(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s3)
   deallocate (d1)
   deallocate (b2)

   allocate (x3(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x3 = 0.0d0
   call sum_stripe(4, shape(x3), size(x3), '2134', -1.000, &
                   x3, s3)
   deallocate (s3)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s4(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s4)
   deallocate (d1)
   deallocate (b2)

   allocate (x21(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x21 = 0.0d0
   call sum_stripe(4, shape(x21), size(x21), '3124', 1.000, &
                   x21, s4)
   deallocate (s4)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2413', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s5(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s5)
   deallocate (d1)
   deallocate (b2)

   allocate (x22(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x22 = 0.0d0
   call sum_stripe(4, shape(x22), size(x22), '3124', 1.000, &
                   x22, s5)
   deallocate (s5)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '3421', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s6(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k3
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s6)
   deallocate (d1)
   deallocate (b2)

   allocate (x4(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x4 = 0.0d0
   call sum_stripe(4, shape(x4), size(x4), '4123', 1.000, x4, &
                   s6)
   deallocate (s6)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s7(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s7)
   deallocate (d1)
   deallocate (b2)

   allocate (x5(n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   x5 = 0.0d0
   call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                   x5, s7)
   deallocate (s7)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '1423', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s8(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s8)
   deallocate (d1)
   deallocate (b2)

   allocate (x6(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x6 = 0.0d0
   call sum_stripe(4, shape(x6), size(x6), '3124', -1.000, &
                   x6, s8)
   deallocate (s8)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s9(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s9)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x5), size(x5), '4123', 1.000, x5, &
                   s9)
   deallocate (s9)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n2 - n0, n1 - n0/), '3421', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s10(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k4
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s10)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '4123', 1.000, x6, &
                   s10)
   deallocate (s10)

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

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s11(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s11)
   deallocate (d1)
   deallocate (b2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,s11,t4b) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n1
                     sum = sum &
                           + (s11(j, m, i, n) * t4b(d, c, b, a, l, n, m, k) & !jmindcbalnmk    (+0.500)
                              - s11(k, m, i, n) * t4b(d, c, b, a, l, n, m, j) & !kmindcbalnmj    (-0.500)
                              - s11(i, m, j, n) * t4b(d, c, b, a, l, n, m, k) & !imjndcbalnmk    (-0.500)
                              + s11(i, m, k, n) * t4b(d, c, b, a, l, n, m, j) & !imkndcbalnmj    (+0.500)
                              + s11(k, m, j, n) * t4b(d, c, b, a, l, n, m, i) & !kmjndcbalnmi    (+0.500)
                              - s11(j, m, k, n) * t4b(d, c, b, a, l, n, m, i)) / 2.0d0!jmkndcbalnmi    (-0.500)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s11), size(s11), '2413', s11, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u55(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u55)
   deallocate (d1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t2b,u55) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n1
                  sum = sum &
                        + u55(c, b, k, n, j, i) * t2b(d, a, l, n) & !cbknjidaln      (+1.000)
                        - u55(c, a, k, n, j, i) * t2b(d, b, l, n) & !caknjidbln      (-1.000)
                        + u55(b, a, k, n, j, i) * t2b(d, c, l, n) & !baknjidcln      (+1.000)
                        - u55(c, b, j, n, k, i) * t2b(d, a, l, n) & !cbjnkidaln      (-1.000)
                        + u55(c, a, j, n, k, i) * t2b(d, b, l, n) & !cajnkidbln      (+1.000)
                        - u55(b, a, j, n, k, i) * t2b(d, c, l, n) & !bajnkidcln      (-1.000)
                        - u55(c, b, k, n, i, j) * t2b(d, a, l, n) & !cbknijdaln      (-1.000)
                        + u55(c, a, k, n, i, j) * t2b(d, b, l, n) & !caknijdbln      (+1.000)
                        - u55(b, a, k, n, i, j) * t2b(d, c, l, n) & !baknijdcln      (-1.000)
                        + u55(c, b, j, n, i, k) * t2b(d, a, l, n) & !cbjnikdaln      (+1.000)
                        - u55(c, a, j, n, i, k) * t2b(d, b, l, n) & !cajnikdbln      (-1.000)
                        + u55(b, a, j, n, i, k) * t2b(d, c, l, n) & !bajnikdcln      (+1.000)
                        + u55(c, b, i, n, k, j) * t2b(d, a, l, n) & !cbinkjdaln      (+1.000)
                        - u55(c, a, i, n, k, j) * t2b(d, b, l, n) & !cainkjdbln      (-1.000)
                        + u55(b, a, i, n, k, j) * t2b(d, c, l, n) & !bainkjdcln      (+1.000)
                        - u55(c, b, i, n, j, k) * t2b(d, a, l, n) & !cbinjkdaln      (-1.000)
                        + u55(c, a, i, n, j, k) * t2b(d, b, l, n) & !cainjkdbln      (+1.000)
                        - u55(b, a, i, n, j, k) * t2b(d, c, l, n)          !bainjkdcln      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (u55)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s11), size(s11), '2413', s11, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s89(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s89)
   deallocate (d1)
   deallocate (b2)
   deallocate (s11)

   call sum_stripe(4, shape(x21), size(x21), '2134', -1.000, &
                   x21, s89)
   deallocate (s89)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s12(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s12)
   deallocate (d1)
   deallocate (b2)

   allocate (x13(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x13 = 0.0d0
   call sum_stripe(4, shape(x13), size(x13), '3241', -1.000, &
                   x13, s12)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s12), size(s12), '2413', s12, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u57(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u57)
   deallocate (d1)
   deallocate (d2)

   allocate (x23(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x23 = 0.0d0
   call sum_stripe(6, shape(x23), size(x23), '245136', &
                   1.000, x23, u57)
   deallocate (u57)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s12), size(s12), '2413', s12, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u56(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u56)
   deallocate (d1)
   deallocate (d2)

   allocate (x24(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x24 = 0.0d0
   call sum_stripe(6, shape(x24), size(x24), '345126', &
                   1.000, x24, u56)
   deallocate (u56)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s12), size(s12), '4213', s12, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s90(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s90)
   deallocate (d1)
   deallocate (b2)
   deallocate (s12)

   call sum_stripe(4, shape(x4), size(x4), '2134', 1.000, x4, &
                   s90)
   deallocate (s90)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q3(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q3)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(2, shape(x8), size(x8), '21', -1.000, x8, &
                   q3)
   deallocate (q3)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s13(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s13)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x13), size(x13), '4231', 1.000, &
                   x13, s13)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s13), size(s13), '2431', s13, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u59(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u59)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x23), size(x23), '245136', &
                   -1.000, x23, u59)
   deallocate (u59)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s13), size(s13), '2431', s13, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u58(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u58)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x24), size(x24), '345126', &
                   -1.000, x24, u58)
   deallocate (u58)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s13), size(s13), '4231', s13, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s92(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s92)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x22), size(x22), '3124', 1.000, &
                   x22, s92)
   deallocate (s92)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s13), size(s13), '2431', s13, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s91(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s91)
   deallocate (d1)
   deallocate (b2)
   deallocate (s13)

   call sum_stripe(4, shape(x3), size(x3), '3124', 1.000, x3, &
                   s91)
   deallocate (s91)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2431', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s14(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s14)
   deallocate (d1)
   deallocate (b2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,s14,t4b) &
         !$omp private(a,b,c,d,e,f,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do f = n1 + 1, n3
                     sum = sum &
                           + (s14(b, f, e, c) * t4b(d, f, e, a, l, k, j, i) & !bfecdfealkji    (+0.500)
                              - s14(a, f, e, c) * t4b(d, f, e, b, l, k, j, i) & !afecdfeblkji    (-0.500)
                              - s14(c, f, e, b) * t4b(d, f, e, a, l, k, j, i) & !cfebdfealkji    (-0.500)
                              + s14(c, f, e, a) * t4b(d, f, e, b, l, k, j, i) & !cfeadfeblkji    (+0.500)
                              + s14(a, f, e, b) * t4b(d, f, e, c, l, k, j, i) & !afebdfeclkji    (+0.500)
                              - s14(b, f, e, a) * t4b(d, f, e, c, l, k, j, i)) / 2.0d0!bfeadfeclkji    (-0.500)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s14), size(s14), '3241', s14, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u60(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k3
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u60)
   deallocate (d1)
   deallocate (d2)
   deallocate (s14)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,u60,t2b) &
         !$omp private(a,b,c,d,f,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n1 + 1, n3
                  sum = sum &
                        + u60(a, j, i, f, c, b) * t2b(d, f, l, k) & !ajifcbdflk      (+1.000)
                        - u60(b, j, i, f, c, a) * t2b(d, f, l, k) & !bjifcadflk      (-1.000)
                        - u60(a, j, i, f, b, c) * t2b(d, f, l, k) & !ajifbcdflk      (-1.000)
                        + u60(b, j, i, f, a, c) * t2b(d, f, l, k) & !bjifacdflk      (+1.000)
                        + u60(c, j, i, f, b, a) * t2b(d, f, l, k) & !cjifbadflk      (+1.000)
                        - u60(c, j, i, f, a, b) * t2b(d, f, l, k) & !cjifabdflk      (-1.000)
                        - u60(a, k, i, f, c, b) * t2b(d, f, l, j) & !akifcbdflj      (-1.000)
                        + u60(b, k, i, f, c, a) * t2b(d, f, l, j) & !bkifcadflj      (+1.000)
                        + u60(a, k, i, f, b, c) * t2b(d, f, l, j) & !akifbcdflj      (+1.000)
                        - u60(b, k, i, f, a, c) * t2b(d, f, l, j) & !bkifacdflj      (-1.000)
                        - u60(c, k, i, f, b, a) * t2b(d, f, l, j) & !ckifbadflj      (-1.000)
                        + u60(c, k, i, f, a, b) * t2b(d, f, l, j) & !ckifabdflj      (+1.000)
                        + u60(a, k, j, f, c, b) * t2b(d, f, l, i) & !akjfcbdfli      (+1.000)
                        - u60(b, k, j, f, c, a) * t2b(d, f, l, i) & !bkjfcadfli      (-1.000)
                        - u60(a, k, j, f, b, c) * t2b(d, f, l, i) & !akjfbcdfli      (-1.000)
                        + u60(b, k, j, f, a, c) * t2b(d, f, l, i) & !bkjfacdfli      (+1.000)
                        + u60(c, k, j, f, b, a) * t2b(d, f, l, i) & !ckjfbadfli      (+1.000)
                        - u60(c, k, j, f, a, b) * t2b(d, f, l, i)          !ckjfabdfli      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (u60)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (q4(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k1 * k3
   call egemm1(i1, i3, d1, b2, q4)
   deallocate (d1)
   deallocate (b2)

   x9 = x9 - q4
   deallocate (q4)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3142', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s15(n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s15)
   deallocate (d1)
   deallocate (b2)

   allocate (x15(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   x15 = 0.0d0
   call sum_stripe(4, shape(x15), size(x15), '4231', 1.000, &
                   x15, s15)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s15), size(s15), '2431', s15, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u61(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u61)
   deallocate (d1)
   deallocate (d2)

   allocate (x25(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x25 = 0.0d0
   call sum_stripe(6, shape(x25), size(x25), '235146', &
                   1.000, x25, u61)
   deallocate (u61)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s15), size(s15), '4231', s15, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s102(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s102)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s102)
   deallocate (s102)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s15), size(s15), '2431', s15, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s93(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s93)
   deallocate (d1)
   deallocate (b2)
   deallocate (s15)

   call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                   x5, s93)
   deallocate (s93)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n2 - n0, n0 - n0/), '3421', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s16(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k4
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s16)
   deallocate (d1)
   deallocate (b2)

   allocate (x16(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   x16 = 0.0d0
   call sum_stripe(4, shape(x16), size(x16), '4231', 1.000, &
                   x16, s16)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s16), size(s16), '2431', s16, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u62(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u62)
   deallocate (d1)
   deallocate (d2)

   allocate (x26(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x26 = 0.0d0
   call sum_stripe(6, shape(x26), size(x26), '345126', &
                   1.000, x26, u62)
   deallocate (u62)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s16), size(s16), '2431', s16, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s103(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s103)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                   s103)
   deallocate (s103)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s16), size(s16), '4231', s16, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s94(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s94)
   deallocate (d1)
   deallocate (b2)
   deallocate (s16)

   call sum_stripe(4, shape(x6), size(x6), '3124', -1.000, &
                   x6, s94)
   deallocate (s94)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s17(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s17)
   deallocate (d1)
   deallocate (b2)

   allocate (x17(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   x17 = 0.0d0
   call sum_stripe(4, shape(x17), size(x17), '3241', -1.000, &
                   x17, s17)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s17), size(s17), '2413', s17, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u63(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k2
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u63)
   deallocate (d1)
   deallocate (d2)

   allocate (x27(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x27 = 0.0d0
   call sum_stripe(6, shape(x27), size(x27), '356124', &
                   1.000, x27, u63)
   deallocate (u63)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s17), size(s17), '4213', s17, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s104(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s104)
   deallocate (d1)
   deallocate (b2)
   deallocate (s17)

   call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                   s104)
   deallocate (s104)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
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
   call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                   x10, q5)
   deallocate (q5)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n2 - n0/), '1432', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s18(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
   i1 = k4 * k3 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s18)
   deallocate (d1)
   deallocate (b2)

   allocate (x18(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   x18 = 0.0d0
   call sum_stripe(4, shape(x18), size(x18), '4123', -1.000, &
                   x18, s18)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s18), size(s18), '3241', s18, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u64(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k4
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u64)
   deallocate (d1)
   deallocate (d2)

   allocate (x28(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x28 = 0.0d0
   call sum_stripe(6, shape(x28), size(x28), '456123', &
                   1.000, x28, u64)
   deallocate (u64)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s18), size(s18), '2341', s18, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s105(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k3
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s105)
   deallocate (d1)
   deallocate (b2)
   deallocate (s18)

   call sum_stripe(4, shape(x2), size(x2), '4123', -1.000, &
                   x2, s105)
   deallocate (s105)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (q6(n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i3 = k1 * k3
   call egemm1(i1, i3, d1, b2, q6)
   deallocate (d1)
   deallocate (b2)

   allocate (x11(n2 + 1:n3, n2 + 1:n3))
   x11 = 0.0d0
   x11 = x11 + q6
   deallocate (q6)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s19(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s19)
   deallocate (d1)
   deallocate (b2)

   allocate (x20(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   x20 = 0.0d0
   call sum_stripe(4, shape(x20), size(x20), '3241', -1.000, &
                   x20, s19)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s19), size(s19), '2413', s19, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u71(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u71)
   deallocate (d1)
   deallocate (d2)

   allocate (x29(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x29 = 0.0d0
   call sum_stripe(6, shape(x29), size(x29), '345126', &
                   1.000, x29, u71)
   deallocate (u71)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s19), size(s19), '4213', s19, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s107(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s107)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', 1.000, x6, &
                   s107)
   deallocate (s107)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s19), size(s19), '2413', s19, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s106(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s106)
   deallocate (d1)
   deallocate (b2)
   deallocate (s19)

   call sum_stripe(4, shape(x5), size(x5), '3124', -1.000, &
                   x5, s106)
   deallocate (s106)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s20(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k4
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s20)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x20), size(x20), '4231', 1.000, &
                   x20, s20)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s20), size(s20), '2431', s20, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u72(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u72)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x29), size(x29), '345126', &
                   -1.000, x29, u72)
   deallocate (u72)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s20), size(s20), '4231', s20, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s109(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s109)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                   x6, s109)
   deallocate (s109)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s20), size(s20), '2431', s20, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s108(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s108)
   deallocate (d1)
   deallocate (b2)
   deallocate (s20)

   call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                   s108)
   deallocate (s108)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s21(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s21)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s21)
   deallocate (s21)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4123', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s22(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s22)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                   s22)
   deallocate (s22)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2314', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s23(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s23)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                   x2, s23)
   deallocate (s23)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n2 - n0, n1 - n0/), '4321', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s24(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k3
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s24)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                   s24)
   deallocate (s24)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s25(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s25)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                   s25)
   deallocate (s25)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2413', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s26(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s26)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                   x6, s26)
   deallocate (s26)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s27(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s27)
   deallocate (d1)
   deallocate (b2)

   allocate (x7(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x7 = 0.0d0
   call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                   x7, s27)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s27), size(s27), '3214', s27, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u83(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u83)
   deallocate (d1)
   deallocate (d2)
   deallocate (s27)

   allocate (x30(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x30 = 0.0d0
   call sum_stripe(6, shape(x30), size(x30), '356124', &
                   1.000, x30, u83)
   deallocate (u83)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s28(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k3
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s28)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x7), size(x7), '4231', 1.000, x7, &
                   s28)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s28), size(s28), '2431', s28, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u84(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u84)
   deallocate (d1)
   deallocate (d2)
   deallocate (s28)

   call sum_stripe(6, shape(x30), size(x30), '356124', &
                   -1.000, x30, u84)
   deallocate (u84)

   allocate (b1(n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                      size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (q7(n0 + 1:n2, n0 + 1:n2))
   i1 = k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, b1, b2, q7)
   deallocate (b1)
   deallocate (b2)

   call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                   x10, q7)
   deallocate (q7)

   allocate (b1(n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                      size(b1), (/n0 - n0, n2 - n0/), '21', fockb, b1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q8(n2 + 1:n3, n2 + 1:n3))
   i1 = k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, b1, b2, q8)
   deallocate (b1)
   deallocate (b2)

   call sum_stripe(2, shape(x11), size(x11), '21', -1.000, &
                   x11, q8)
   deallocate (q8)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s29(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s29)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x15), size(x15), '3241', 1.000, &
                   x15, s29)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s29), size(s29), '2413', s29, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u85(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u85)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x25), size(x25), '235146', &
                   1.000, x25, u85)
   deallocate (u85)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s29), size(s29), '4213', s29, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s136(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s136)
   deallocate (d1)
   deallocate (b2)
   deallocate (s29)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s136)
   deallocate (s136)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s30(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s30)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x16), size(x16), '3124', -1.000, &
                   x16, s30)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s30), size(s30), '3214', s30, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u86(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u86)
   deallocate (d1)
   deallocate (d2)
   deallocate (s30)

   call sum_stripe(6, shape(x26), size(x26), '345126', &
                   -1.000, x26, u86)
   deallocate (u86)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (q9(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k2 * k4
   call egemm1(i1, i3, d1, b2, q9)
   deallocate (d1)
   deallocate (b2)

   x8 = x8 + q9
   deallocate (q9)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s31(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s31)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x17), size(x17), '4231', 1.000, &
                   x17, s31)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s31), size(s31), '2431', s31, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u87(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k2
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u87)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x27), size(x27), '356124', &
                   -1.000, x27, u87)
   deallocate (u87)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s31), size(s31), '4231', s31, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s137(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s137)
   deallocate (d1)
   deallocate (b2)
   deallocate (s31)

   call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                   x2, s137)
   deallocate (s137)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s32(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s32)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x18), size(x18), '3124', -1.000, &
                   x18, s32)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s32), size(s32), '3214', s32, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u88(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k4
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u88)
   deallocate (d1)
   deallocate (d2)
   deallocate (s32)

   call sum_stripe(6, shape(x28), size(x28), '456123', &
                   1.000, x28, u88)
   deallocate (u88)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (q10(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k2 * k4
   call egemm1(i1, i3, d1, b2, q10)
   deallocate (d1)
   deallocate (b2)

   x9 = x9 + q10
   deallocate (q10)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s33(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s33)
   deallocate (d1)
   deallocate (b2)

   allocate (x19(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   x19 = 0.0d0
   call sum_stripe(4, shape(x19), size(x19), '3241', -1.000, &
                   x19, s33)
   deallocate (s33)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q11(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q11)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(2, shape(x10), size(x10), '21', -1.000, &
                   x10, q11)
   deallocate (q11)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '3412', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s34(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k4
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s34)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x19), size(x19), '4231', 1.000, &
                   x19, s34)
   deallocate (s34)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (q12(n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i3 = k2 * k4
   call egemm1(i1, i3, d1, b2, q12)
   deallocate (d1)
   deallocate (b2)

   x11 = x11 - q12
   deallocate (q12)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3124', intm, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u1)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x30), size(x30), '356124', &
                   -1.000, x30, u1)
   deallocate (u1)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intr, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u2)
   deallocate (d1)
   deallocate (d2)

   allocate (x31(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x31 = 0.0d0
   call sum_stripe(6, shape(x31), size(x31), '234156', &
                   1.000, x31, u2)
   deallocate (u2)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u3(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u3)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x24), size(x24), '345126', &
                   -1.000, x24, u3)
   deallocate (u3)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u4(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u4)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x23), size(x23), '245136', &
                   -1.000, x23, u4)
   deallocate (u4)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '3421', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u5(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k3
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u5)
   deallocate (d1)
   deallocate (d2)

   allocate (x32(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x32 = 0.0d0
   call sum_stripe(6, shape(x32), size(x32), '456123', &
                   1.000, x32, u5)
   deallocate (u5)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intm, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u6(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u6)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x25), size(x25), '235146', &
                   1.000, x25, u6)
   deallocate (u6)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4123', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u7(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u7)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x26), size(x26), '345126', &
                   1.000, x26, u7)
   deallocate (u7)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u8(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k2
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u8)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x27), size(x27), '356124', &
                   -1.000, x27, u8)
   deallocate (u8)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n2 - n0, n1 - n0/), '3421', intm, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u9(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k4
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u9)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x28), size(x28), '456123', &
                   -1.000, x28, u9)
   deallocate (u9)

   allocate (b1(n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                      size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (s35(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, b1, d2, s35)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x3), size(x3), '2341', 1.000, x3, &
                   s35)
   deallocate (s35)

   allocate (b1(n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                      size(b1), (/n0 - n0, n1 - n0/), '21', fockr, b1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s36(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   i1 = k3
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, b1, d2, s36)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                   x4, s36)
   deallocate (s36)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u10(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u10)
   deallocate (d1)
   deallocate (d2)

   allocate (x33(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x33 = 0.0d0
   call sum_stripe(6, shape(x33), size(x33), '345261', &
                   1.000, x33, u10)
   deallocate (u10)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intr, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '251346', t3b, f2)
   allocate (u11(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k2 * k3 * k4
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, f2, u11)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x26), size(x26), '234516', &
                   1.000, x26, u11)
   deallocate (u11)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s37(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s37)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x21), size(x21), '2341', 1.000, &
                   x21, s37)
   deallocate (s37)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1243', intr, d1)
   allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2a), size(t2a), '4312', t2a, d2)
   allocate (s38(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k3 * k3
   i3 = k1 * k1
   call egemm(i1, i2, i3, d1, d2, s38)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x4), size(x4), '2314', 0.500, x4, &
                   s38)
   deallocate (s38)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u12(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u12)
   deallocate (d1)
   deallocate (d2)

   allocate (x34(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x34 = 0.0d0
   call sum_stripe(6, shape(x34), size(x34), '456231', &
                   1.000, x34, u12)
   deallocate (u12)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (s39(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k1 * k1
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, d2, s39)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x3), size(x3), '3421', 0.500, x3, &
                   s39)
   deallocate (s39)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intr, d1)
   allocate (f2(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '231456', t3b, f2)
   allocate (u13(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k1 * k1 * k2 * k4
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, f2, u13)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,u13,t2a) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        + (u13(d, l, k, j, c, m) * t2a(b, a, m, i) & !dlkjcmbami      (+0.500)
                           - u13(d, l, k, j, b, m) * t2a(c, a, m, i) & !dlkjbmcami      (-0.500)
                           + u13(d, l, k, j, a, m) * t2a(c, b, m, i) & !dlkjamcbmi      (+0.500)
                           - u13(d, l, k, i, c, m) * t2a(b, a, m, j) & !dlkicmbamj      (-0.500)
                           + u13(d, l, k, i, b, m) * t2a(c, a, m, j) & !dlkibmcamj      (+0.500)
                           - u13(d, l, k, i, a, m) * t2a(c, b, m, j) & !dlkiamcbmj      (-0.500)
                           + u13(d, l, j, i, c, m) * t2a(b, a, m, k) & !dljicmbamk      (+0.500)
                           - u13(d, l, j, i, b, m) * t2a(c, a, m, k) & !dljibmcamk      (-0.500)
                           + u13(d, l, j, i, a, m) * t2a(c, b, m, k)) / 2.0d0   !dljiamcbmk      (+0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (u13)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
   allocate (s40(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i2 = k1 * k3
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, d2, s40)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x22), size(x22), '3412', -1.000, &
                   x22, s40)
   deallocate (s40)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3142', intm, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u14(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k1
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u14)
   deallocate (d1)
   deallocate (d2)

   allocate (x35(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x35 = 0.0d0
   call sum_stripe(6, shape(x35), size(x35), '356241', &
                   1.000, x35, u14)
   deallocate (u14)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n2 - n0, n0 - n0/), '3421', intm, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u15(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k4
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u15)
   deallocate (d1)
   deallocate (d2)

   allocate (x36(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x36 = 0.0d0
   call sum_stripe(6, shape(x36), size(x36), '456231', &
                   1.000, x36, u15)
   deallocate (u15)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '241356', t3b, f2)
   allocate (u16(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1
   i2 = k1 * k1 * k3 * k4
   i3 = k2 * k3
   call egemm(i1, i2, i3, d1, f2, u16)
   deallocate (d1)
   deallocate (f2)

   allocate (x37(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x37 = 0.0d0
   call sum_stripe(6, shape(x37), size(x37), '235614', &
                   1.000, x37, u16)
   deallocate (u16)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s41(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k1 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s41)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2431', 1.000, x5, &
                   s41)
   deallocate (s41)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
   allocate (f2(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '123456', t3b, f2)
   allocate (u17(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4
   i2 = k1 * k1 * k2 * k3
   i3 = k3 * k4
   call egemm(i1, i2, i3, d1, f2, u17)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x37), size(x37), '345621', &
                   -1.000, x37, u17)
   deallocate (u17)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
   allocate (s42(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i2 = k1 * k3
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, d2, s42)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '3412', 1.000, x6, &
                   s42)
   deallocate (s42)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
   allocate (u18(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k2 * k3 * k4
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, f2, u18)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x26), size(x26), '234516', &
                   1.000, x26, u18)
   deallocate (u18)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u19(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k4
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u19)
   deallocate (d1)
   deallocate (d2)

   allocate (x38(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x38 = 0.0d0
   call sum_stripe(6, shape(x38), size(x38), '456231', &
                   1.000, x38, u19)
   deallocate (u19)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u20(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u20)
   deallocate (d1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,u20,t4b) &
         !$omp private(a,b,c,d,n,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n1 + 1, n3
                  do m = n0 + 1, n1
                  do n = n0 + 1, n1
                     sum = sum &
                           + (u20(c, j, i, f, m, n) * t4b(d, f, b, a, l, n, m, k) & !cjifmndfbalnmk  (+0.500)
                              - u20(b, j, i, f, m, n) * t4b(d, f, c, a, l, n, m, k) & !bjifmndfcalnmk  (-0.500)
                              + u20(a, j, i, f, m, n) * t4b(d, f, c, b, l, n, m, k) & !ajifmndfcblnmk  (+0.500)
                              - u20(c, k, i, f, m, n) * t4b(d, f, b, a, l, n, m, j) & !ckifmndfbalnmj  (-0.500)
                              + u20(b, k, i, f, m, n) * t4b(d, f, c, a, l, n, m, j) & !bkifmndfcalnmj  (+0.500)
                              - u20(a, k, i, f, m, n) * t4b(d, f, c, b, l, n, m, j) & !akifmndfcblnmj  (-0.500)
                              + u20(c, k, j, f, m, n) * t4b(d, f, b, a, l, n, m, i) & !ckjfmndfbalnmi  (+0.500)
                              - u20(b, k, j, f, m, n) * t4b(d, f, c, a, l, n, m, i) & !bkjfmndfcalnmi  (-0.500)
                              + u20(a, k, j, f, m, n) * t4b(d, f, c, b, l, n, m, i)) / 2.0d0 !akjfmndfcblnmi  (+0.500)
                  end do
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u20), size(u20), '564123', u20, f1)
   allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2a), size(t2a), '4312', t2a, d2)
   allocate (u100(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k3
   i2 = k3 * k3
   i3 = k1 * k1
   call egemm(i1, i2, i3, f1, d2, u100)
   deallocate (f1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,u100,t2b) &
         !$omp private(a,b,c,d,f,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n1 + 1, n3
                  sum = sum &
                        + (u100(b, a, f, c, j, i) * t2b(d, f, l, k) & !bafcjidflk      (+0.500)
                           - u100(c, a, f, b, j, i) * t2b(d, f, l, k) & !cafbjidflk      (-0.500)
                           + u100(c, b, f, a, j, i) * t2b(d, f, l, k) & !cbfajidflk      (+0.500)
                           - u100(b, a, f, c, k, i) * t2b(d, f, l, j) & !bafckidflj      (-0.500)
                           + u100(c, a, f, b, k, i) * t2b(d, f, l, j) & !cafbkidflj      (+0.500)
                           - u100(c, b, f, a, k, i) * t2b(d, f, l, j) & !cbfakidflj      (-0.500)
                           + u100(b, a, f, c, k, j) * t2b(d, f, l, i) & !bafckjdfli      (+0.500)
                           - u100(c, a, f, b, k, j) * t2b(d, f, l, i) & !cafbkjdfli      (-0.500)
                           + u100(c, b, f, a, k, j) * t2b(d, f, l, i)) / 2.0d0  !cbfakjdfli      (+0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (u100)

   allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u20), size(u20), '541236', u20, f1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u98(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1 * k3
   i2 = k1 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, f1, d2, u98)
   deallocate (f1)
   deallocate (d2)

   allocate (x39(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x39 = 0.0d0
   call sum_stripe(6, shape(x39), size(x39), '342561', &
                   1.000, x39, u98)
   deallocate (u98)

   allocate (f1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u20), size(u20), '465123', u20, f1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
   allocate (u97(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k1
   i2 = k2 * k4
   i3 = k1 * k3
   call egemm(i1, i2, i3, f1, d2, u97)
   deallocate (f1)
   deallocate (d2)

   call sum_stripe(6, shape(x30), size(x30), '241356', &
                   -1.000, x30, u97)
   deallocate (u97)

   allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u20), size(u20), '541236', u20, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s119(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1 * k3
   i3 = k3 * k1
   call egemm1(i1, i3, f1, b2, s119)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(4, shape(x3), size(x3), '2341', -1.000, &
                   x3, s119)
   deallocate (s119)

   allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u20), size(u20), '541236', u20, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u67(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u67)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x34), size(x34), '324561', &
                   -1.000, x34, u67)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x34,t3b) &
         !$omp private(a,b,c,d,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n1 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           + x34(m, f, c, b, j, i) * t3b(d, f, a, l, m, k) & !mfcbjidfalmk    (+1.000)
                           - x34(m, f, c, a, j, i) * t3b(d, f, b, l, m, k) & !mfcajidfblmk    (-1.000)
                           - x34(m, f, b, c, j, i) * t3b(d, f, a, l, m, k) & !mfbcjidfalmk    (-1.000)
                           + x34(m, f, a, c, j, i) * t3b(d, f, b, l, m, k) & !mfacjidfblmk    (+1.000)
                           + x34(m, f, b, a, j, i) * t3b(d, f, c, l, m, k) & !mfbajidfclmk    (+1.000)
                           - x34(m, f, a, b, j, i) * t3b(d, f, c, l, m, k) & !mfabjidfclmk    (-1.000)
                           - x34(m, f, c, b, k, i) * t3b(d, f, a, l, m, j) & !mfcbkidfalmj    (-1.000)
                           + x34(m, f, c, a, k, i) * t3b(d, f, b, l, m, j) & !mfcakidfblmj    (+1.000)
                           + x34(m, f, b, c, k, i) * t3b(d, f, a, l, m, j) & !mfbckidfalmj    (+1.000)
                           - x34(m, f, a, c, k, i) * t3b(d, f, b, l, m, j) & !mfackidfblmj    (-1.000)
                           - x34(m, f, b, a, k, i) * t3b(d, f, c, l, m, j) & !mfbakidfclmj    (-1.000)
                           + x34(m, f, a, b, k, i) * t3b(d, f, c, l, m, j) & !mfabkidfclmj    (+1.000)
                           + x34(m, f, c, b, k, j) * t3b(d, f, a, l, m, i) & !mfcbkjdfalmi    (+1.000)
                           - x34(m, f, c, a, k, j) * t3b(d, f, b, l, m, i) & !mfcakjdfblmi    (-1.000)
                           - x34(m, f, b, c, k, j) * t3b(d, f, a, l, m, i) & !mfbckjdfalmi    (-1.000)
                           + x34(m, f, a, c, k, j) * t3b(d, f, b, l, m, i) & !mfackjdfblmi    (+1.000)
                           + x34(m, f, b, a, k, j) * t3b(d, f, c, l, m, i) & !mfbakjdfclmi    (+1.000)
                           - x34(m, f, a, b, k, j) * t3b(d, f, c, l, m, i)      !mfabkjdfclmi    (-1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x34)

   allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u67), size(u67), '621345', u67, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u115(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u115)
   deallocate (f1)
   deallocate (b2)
   deallocate (u67)

   call sum_stripe(6, shape(x32), size(x32), '213456', &
                   1.000, x32, u115)
   deallocate (u115)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t2b,x32) &
         !$omp private(a,b,c,d,f,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n1 + 1, n3
                  sum = sum &
                        + x32(f, c, b, a, j, i) * t2b(d, f, l, k) & !fcbajidflk      (+1.000)
                        - x32(f, c, a, b, j, i) * t2b(d, f, l, k) & !fcabjidflk      (-1.000)
                        + x32(f, b, a, c, j, i) * t2b(d, f, l, k) & !fbacjidflk      (+1.000)
                        - x32(f, c, b, a, k, i) * t2b(d, f, l, j) & !fcbakidflj      (-1.000)
                        + x32(f, c, a, b, k, i) * t2b(d, f, l, j) & !fcabkidflj      (+1.000)
                        - x32(f, b, a, c, k, i) * t2b(d, f, l, j) & !fbackidflj      (-1.000)
                        + x32(f, c, b, a, k, j) * t2b(d, f, l, i) & !fcbakjdfli      (+1.000)
                        - x32(f, c, a, b, k, j) * t2b(d, f, l, i) & !fcabkjdfli      (-1.000)
                        + x32(f, b, a, c, k, j) * t2b(d, f, l, i)          !fbackjdfli      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x32)

   allocate (f1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u20), size(u20), '451236', u20, f1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (u65(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, f1, b2, u65)
   deallocate (f1)
   deallocate (b2)
   deallocate (u20)

   call sum_stripe(6, shape(x33), size(x33), '623451', &
                   -1.000, x33, u65)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x33,t3b) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n1
                     sum = sum & ! top two switched
                           + (x33(n, m, b, k, j, i) * t3b(d, c, a, l, n, m) & !nmbkjidcalnm    (+0.500)
                              - x33(n, m, c, k, j, i) * t3b(d, b, a, l, n, m) & !nmckjidbalnm    (-0.500)
                              - x33(n, m, a, k, j, i) * t3b(d, c, b, l, n, m) & !nmakjidcblnm    (-0.500)
                              + x33(n, m, c, k, i, j) * t3b(d, b, a, l, n, m) & !nmckijdbalnm    (+0.500)
                              - x33(n, m, b, k, i, j) * t3b(d, c, a, l, n, m) & !nmbkijdcalnm    (-0.500)
                              + x33(n, m, a, k, i, j) * t3b(d, c, b, l, n, m) & !nmakijdcblnm    (+0.500)
                              - x33(n, m, c, j, i, k) * t3b(d, b, a, l, n, m) & !nmcjikdbalnm    (-0.500)
                              + x33(n, m, b, j, i, k) * t3b(d, c, a, l, n, m) & !nmbjikdcalnm    (+0.500)
                              - x33(n, m, a, j, i, k) * t3b(d, c, b, l, n, m)) / 2.0d0 !nmajikdcblnm    (-0.500)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x33)

   allocate (f1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u65), size(u65), '263451', u65, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u113(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1 * k3 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u113)
   deallocate (f1)
   deallocate (b2)
   deallocate (u65)

   call sum_stripe(6, shape(x24), size(x24), '213456', &
                   -1.000, x24, u113)
   deallocate (u113)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x24,t2b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n1
                  sum = sum &
                        - x24(n, c, b, k, j, i) * t2b(d, a, l, n) & !ncbkjidaln      (-1.000)
                        + x24(n, c, a, k, j, i) * t2b(d, b, l, n) & !ncakjidbln      (+1.000)
                        + x24(n, b, c, k, j, i) * t2b(d, a, l, n) & !nbckjidaln      (+1.000)
                        - x24(n, a, c, k, j, i) * t2b(d, b, l, n) & !nackjidbln      (-1.000)
                        - x24(n, b, a, k, j, i) * t2b(d, c, l, n) & !nbakjidcln      (-1.000)
                        + x24(n, a, b, k, j, i) * t2b(d, c, l, n) & !nabkjidcln      (+1.000)
                        + x24(n, c, b, k, i, j) * t2b(d, a, l, n) & !ncbkijdaln      (+1.000)
                        - x24(n, c, a, k, i, j) * t2b(d, b, l, n) & !ncakijdbln      (-1.000)
                        - x24(n, b, c, k, i, j) * t2b(d, a, l, n) & !nbckijdaln      (-1.000)
                        + x24(n, a, c, k, i, j) * t2b(d, b, l, n) & !nackijdbln      (+1.000)
                        + x24(n, b, a, k, i, j) * t2b(d, c, l, n) & !nbakijdcln      (+1.000)
                        - x24(n, a, b, k, i, j) * t2b(d, c, l, n) & !nabkijdcln      (-1.000)
                        - x24(n, c, b, j, i, k) * t2b(d, a, l, n) & !ncbjikdaln      (-1.000)
                        + x24(n, c, a, j, i, k) * t2b(d, b, l, n) & !ncajikdbln      (+1.000)
                        + x24(n, b, c, j, i, k) * t2b(d, a, l, n) & !nbcjikdaln      (+1.000)
                        - x24(n, a, c, j, i, k) * t2b(d, b, l, n) & !nacjikdbln      (-1.000)
                        - x24(n, b, a, j, i, k) * t2b(d, c, l, n) & !nbajikdcln      (-1.000)
                        + x24(n, a, b, j, i, k) * t2b(d, c, l, n)          !nabjikdcln      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x24)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (s43(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k1
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, d2, s43)
   deallocate (d1)
   deallocate (d2)

   allocate (x12(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x12 = 0.0d0
   call sum_stripe(4, shape(x12), size(x12), '3421', 0.500, &
                   x12, s43)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s43), size(s43), '3412', s43, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u99(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u99)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x31), size(x31), '234156', &
                   0.500, x31, u99)
   deallocate (u99)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s43), size(s43), '3412', s43, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s118(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s118)
   deallocate (d1)
   deallocate (b2)
   deallocate (s43)

   call sum_stripe(4, shape(x3), size(x3), '2134', -0.500, &
                   x3, s118)
   deallocate (s118)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
   allocate (h2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(8, shape(t4b), size(t4b), '23614578', t4b, h2)
   allocate (u21(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k1 * k2 * k3 * k4
   i3 = k1 * k3 * k3
   call egemm(i1, i2, i3, d1, h2, u21)
   deallocate (d1)
   deallocate (h2)

   call sum_stripe(6, shape(x37), size(x37), '234561', &
                   -0.500, x37, u21)
   deallocate (u21)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
   allocate (s44(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k1 * k3
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, d2, s44)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x13), size(x13), '3421', 1.000, &
                   x13, s44)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s44), size(s44), '3412', s44, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u101(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u101)
   deallocate (d1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t2a,u101) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n1
                  sum = sum &
                        + u101(d, l, k, n, a, j) * t2a(c, b, n, i) & !dlknajcbni      (+1.000)
                        - u101(d, l, k, n, b, j) * t2a(c, a, n, i) & !dlknbjcani      (-1.000)
                        - u101(d, l, k, n, c, i) * t2a(b, a, n, j) & !dlkncibanj      (-1.000)
                        + u101(d, l, k, n, c, j) * t2a(b, a, n, i) & !dlkncjbani      (+1.000)
                        + u101(d, l, k, n, b, i) * t2a(c, a, n, j) & !dlknbicanj      (+1.000)
                        - u101(d, l, k, n, a, i) * t2a(c, b, n, j) & !dlknaicbnj      (-1.000)
                        - u101(d, l, j, n, a, k) * t2a(c, b, n, i) & !dljnakcbni      (-1.000)
                        + u101(d, l, j, n, b, k) * t2a(c, a, n, i) & !dljnbkcani      (+1.000)
                        + u101(d, l, j, n, c, i) * t2a(b, a, n, k) & !dljncibank      (+1.000)
                        - u101(d, l, j, n, c, k) * t2a(b, a, n, i) & !dljnckbani      (-1.000)
                        - u101(d, l, j, n, b, i) * t2a(c, a, n, k) & !dljnbicank      (-1.000)
                        + u101(d, l, j, n, a, i) * t2a(c, b, n, k) & !dljnaicbnk      (+1.000)
                        + u101(d, l, i, n, a, k) * t2a(c, b, n, j) & !dlinakcbnj      (+1.000)
                        - u101(d, l, i, n, b, k) * t2a(c, a, n, j) & !dlinbkcanj      (-1.000)
                        - u101(d, l, i, n, c, j) * t2a(b, a, n, k) & !dlincjbank      (-1.000)
                        + u101(d, l, i, n, c, k) * t2a(b, a, n, j) & !dlinckbanj      (+1.000)
                        + u101(d, l, i, n, b, j) * t2a(c, a, n, k) & !dlinbjcank      (+1.000)
                        - u101(d, l, i, n, a, j) * t2a(c, b, n, k)         !dlinajcbnk      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (u101)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s44), size(s44), '4312', s44, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s120(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s120)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x22), size(x22), '2134', -1.000, &
                   x22, s120)
   deallocate (s120)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s44), size(s44), '3412', s44, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s116(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s116)
   deallocate (d1)
   deallocate (b2)
   deallocate (s44)

   call sum_stripe(4, shape(x21), size(x21), '4123', -1.000, &
                   x21, s116)
   deallocate (s116)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (q13(n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1
   i3 = k1 * k3 * k3
   call egemm(i1, i2, i3, d1, d2, q13)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(2, shape(x8), size(x8), '21', -0.500, x8, &
                   q13)
   deallocate (q13)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '1243', intr, d1)
   allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2a), size(t2a), '4312', t2a, d2)
   allocate (s45(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i2 = k3 * k3
   i3 = k1 * k1
   call egemm(i1, i2, i3, d1, d2, s45)
   deallocate (d1)
   deallocate (d2)

   allocate (x14(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   x14 = 0.0d0
   call sum_stripe(4, shape(x14), size(x14), '3412', 0.500, &
                   x14, s45)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s45), size(s45), '4312', s45, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s117(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k3
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s117)
   deallocate (d1)
   deallocate (b2)
   deallocate (s45)

   call sum_stripe(4, shape(x4), size(x4), '4123', 0.500, x4, &
                   s117)
   deallocate (s117)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3124', intr, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(4, shape(t2a), size(t2a), '1432', t2a, d2)
   allocate (q14(n1 + 1:n3, n1 + 1:n3))
   i1 = k3
   i2 = k3
   i3 = k1 * k1 * k3
   call egemm(i1, i2, i3, d1, d2, q14)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(2, shape(x9), size(x9), '21', 0.500, x9, &
                   q14)
   deallocate (q14)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
   allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
   allocate (u22(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k4
   i2 = k1 * k1 * k3
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u22)
   deallocate (d1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4c,u22) &
         !$omp private(a,b,c,d,n,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  do m = n0 + 1, n1
                  do n = n0 + 1, n2
                     sum = sum &
                           + u22(c, j, i, f, m, n) * t4c(f, d, b, a, n, l, m, k) & !cjifmnfdbanlmk  (+1.000)
                           - u22(b, j, i, f, m, n) * t4c(f, d, c, a, n, l, m, k) & !bjifmnfdcanlmk  (-1.000)
                           + u22(a, j, i, f, m, n) * t4c(f, d, c, b, n, l, m, k) & !ajifmnfdcbnlmk  (+1.000)
                           - u22(c, k, i, f, m, n) * t4c(f, d, b, a, n, l, m, j) & !ckifmnfdbanlmj  (-1.000)
                           + u22(b, k, i, f, m, n) * t4c(f, d, c, a, n, l, m, j) & !bkifmnfdcanlmj  (+1.000)
                           - u22(a, k, i, f, m, n) * t4c(f, d, c, b, n, l, m, j) & !akifmnfdcbnlmj  (-1.000)
                           + u22(c, k, j, f, m, n) * t4c(f, d, b, a, n, l, m, i) & !ckjfmnfdbanlmi  (+1.000)
                           - u22(b, k, j, f, m, n) * t4c(f, d, c, a, n, l, m, i) & !bkjfmnfdcanlmi  (-1.000)
                           + u22(a, k, j, f, m, n) * t4c(f, d, c, b, n, l, m, i)  !akjfmnfdcbnlmi  (+1.000)
                  end do
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u22), size(u22), '465123', u22, f1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
   allocate (u105(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k1
   i2 = k1 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, f1, d2, u105)
   deallocate (f1)
   deallocate (d2)

   call sum_stripe(6, shape(x39), size(x39), '341256', &
                   -1.000, x39, u105)
   deallocate (u105)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x39,t2b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n1
                  sum = sum &
                        + x39(n, c, b, k, j, i) * t2b(d, a, l, n) & !ncbkjidaln      (+1.000)
                        - x39(n, c, a, k, j, i) * t2b(d, b, l, n) & !ncakjidbln      (-1.000)
                        - x39(n, b, c, k, j, i) * t2b(d, a, l, n) & !nbckjidaln      (-1.000)
                        + x39(n, a, c, k, j, i) * t2b(d, b, l, n) & !nackjidbln      (+1.000)
                        + x39(n, b, a, k, j, i) * t2b(d, c, l, n) & !nbakjidcln      (+1.000)
                        - x39(n, a, b, k, j, i) * t2b(d, c, l, n) & !nabkjidcln      (-1.000)
                        - x39(n, c, b, j, k, i) * t2b(d, a, l, n) & !ncbjkidaln      (-1.000)
                        + x39(n, c, a, j, k, i) * t2b(d, b, l, n) & !ncajkidbln      (+1.000)
                        + x39(n, b, c, j, k, i) * t2b(d, a, l, n) & !nbcjkidaln      (+1.000)
                        - x39(n, a, c, j, k, i) * t2b(d, b, l, n) & !nacjkidbln      (-1.000)
                        - x39(n, b, a, j, k, i) * t2b(d, c, l, n) & !nbajkidcln      (-1.000)
                        + x39(n, a, b, j, k, i) * t2b(d, c, l, n) & !nabjkidcln      (+1.000)
                        - x39(n, b, c, i, k, j) * t2b(d, a, l, n) & !nbcikjdaln      (-1.000)
                        + x39(n, a, c, i, k, j) * t2b(d, b, l, n) & !nacikjdbln      (+1.000)
                        + x39(n, c, b, i, k, j) * t2b(d, a, l, n) & !ncbikjdaln      (+1.000)
                        - x39(n, c, a, i, k, j) * t2b(d, b, l, n) & !ncaikjdbln      (-1.000)
                        - x39(n, a, b, i, k, j) * t2b(d, c, l, n) & !nabikjdcln      (-1.000)
                        + x39(n, b, a, i, k, j) * t2b(d, c, l, n)          !nbaikjdcln      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x39)

   allocate (f1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u22), size(u22), '541236', u22, f1)
   allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
   allocate (u104(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k1 * k3
   i2 = k2 * k3
   i3 = k4 * k1
   call egemm(i1, i2, i3, f1, d2, u104)
   deallocate (f1)
   deallocate (d2)

   call sum_stripe(6, shape(x27), size(x27), '342561', &
                   -1.000, x27, u104)
   deallocate (u104)

   allocate (f1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u22), size(u22), '564123', u22, f1)
   allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
   allocate (u103(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k4
   i2 = k3 * k4
   i3 = k2 * k1
   call egemm(i1, i2, i3, f1, d2, u103)
   deallocate (f1)
   deallocate (d2)

   call sum_stripe(6, shape(x28), size(x28), '241356', &
                   1.000, x28, u103)
   deallocate (u103)

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u22), size(u22), '465123', u22, f1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
   allocate (u102(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k1
   i2 = k2 * k4
   i3 = k2 * k4
   call egemm(i1, i2, i3, f1, d2, u102)
   deallocate (f1)
   deallocate (d2)

   call sum_stripe(6, shape(x30), size(x30), '241356', &
                   -1.000, x30, u102)
   deallocate (u102)

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u22), size(u22), '465123', u22, f1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s141(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k1
   i3 = k2 * k4
   call egemm1(i1, i3, f1, b2, s141)
   deallocate (f1)
   deallocate (b2)

   x3 = x3 + s141
   deallocate (s141)

   allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u22), size(u22), '654123', u22, f1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (u90(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k4 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, f1, b2, u90)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x36), size(x36), '312456', &
                   -1.000, x36, u90)
   deallocate (u90)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x36,t3b) &
         !$omp private(a,b,c,d,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           + x36(m, f, d, c, j, i) * t3b(f, b, a, l, m, k) & !mfdcjifbalmk    (+1.000)
                           - x36(m, f, d, b, j, i) * t3b(f, c, a, l, m, k) & !mfdbjifcalmk    (-1.000)
                           + x36(m, f, d, a, j, i) * t3b(f, c, b, l, m, k) & !mfdajifcblmk    (+1.000)
                           - x36(m, f, d, c, k, i) * t3b(f, b, a, l, m, j) & !mfdckifbalmj    (-1.000)
                           + x36(m, f, d, b, k, i) * t3b(f, c, a, l, m, j) & !mfdbkifcalmj    (+1.000)
                           - x36(m, f, d, a, k, i) * t3b(f, c, b, l, m, j) & !mfdakifcblmj    (-1.000)
                           + x36(m, f, d, c, k, j) * t3b(f, b, a, l, m, i) & !mfdckjfbalmi    (+1.000)
                           - x36(m, f, d, b, k, j) * t3b(f, c, a, l, m, i) & !mfdbkjfcalmi    (-1.000)
                           + x36(m, f, d, a, k, j) * t3b(f, c, b, l, m, i)      !mfdakjfcblmi    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x36)

   allocate (f1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u22), size(u22), '451236', u22, f1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (u89(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k1 * k3 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, f1, b2, u89)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x35), size(x35), '423561', &
                   1.000, x35, u89)

   allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u89), size(u89), '623145', u89, f1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (u121(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k3 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, f1, b2, u121)
   deallocate (f1)
   deallocate (b2)
   deallocate (u89)

   call sum_stripe(6, shape(x30), size(x30), '213456', &
                   1.000, x30, u121)
   deallocate (u121)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x30,t2a) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        - x30(m, d, c, l, j, i) * t2a(b, a, m, k) & !mdcljibamk      (-1.000)
                        + x30(m, d, b, l, j, i) * t2a(c, a, m, k) & !mdbljicamk      (+1.000)
                        - x30(m, d, a, l, j, i) * t2a(c, b, m, k) & !mdaljicbmk      (-1.000)
                        + x30(m, d, c, l, k, i) * t2a(b, a, m, j) & !mdclkibamj      (+1.000)
                        - x30(m, d, b, l, k, i) * t2a(c, a, m, j) & !mdblkicamj      (-1.000)
                        + x30(m, d, a, l, k, i) * t2a(c, b, m, j) & !mdalkicbmj      (+1.000)
                        - x30(m, d, a, l, k, j) * t2a(c, b, m, i) & !mdalkjcbmi      (-1.000)
                        + x30(m, d, b, l, k, j) * t2a(c, a, m, i) & !mdblkjcami      (+1.000)
                        - x30(m, d, c, l, k, j) * t2a(b, a, m, i)          !mdclkjbami      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x30)

   allocate (f1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u22), size(u22), '541236', u22, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u70(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k1 * k3 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u70)
   deallocate (f1)
   deallocate (b2)
   deallocate (u22)

   call sum_stripe(6, shape(x38), size(x38), '324561', &
                   -1.000, x38, u70)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x38,t3c) &
         !$omp private(a,b,c,d,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           + x38(m, f, c, b, j, i) * t3c(f, d, a, m, l, k) & !mfcbjifdamlk    (+1.000)
                           - x38(m, f, c, a, j, i) * t3c(f, d, b, m, l, k) & !mfcajifdbmlk    (-1.000)
                           - x38(m, f, b, c, j, i) * t3c(f, d, a, m, l, k) & !mfbcjifdamlk    (-1.000)
                           + x38(m, f, a, c, j, i) * t3c(f, d, b, m, l, k) & !mfacjifdbmlk    (+1.000)
                           + x38(m, f, b, a, j, i) * t3c(f, d, c, m, l, k) & !mfbajifdcmlk    (+1.000)
                           - x38(m, f, a, b, j, i) * t3c(f, d, c, m, l, k) & !mfabjifdcmlk    (-1.000)
                           - x38(m, f, c, b, k, i) * t3c(f, d, a, m, l, j) & !mfcbkifdamlj    (-1.000)
                           + x38(m, f, c, a, k, i) * t3c(f, d, b, m, l, j) & !mfcakifdbmlj    (+1.000)
                           + x38(m, f, b, c, k, i) * t3c(f, d, a, m, l, j) & !mfbckifdamlj    (+1.000)
                           - x38(m, f, a, c, k, i) * t3c(f, d, b, m, l, j) & !mfackifdbmlj    (-1.000)
                           - x38(m, f, b, a, k, i) * t3c(f, d, c, m, l, j) & !mfbakifdcmlj    (-1.000)
                           + x38(m, f, a, b, k, i) * t3c(f, d, c, m, l, j) & !mfabkifdcmlj    (+1.000)
                           + x38(m, f, c, b, k, j) * t3c(f, d, a, m, l, i) & !mfcbkjfdamli    (+1.000)
                           - x38(m, f, c, a, k, j) * t3c(f, d, b, m, l, i) & !mfcakjfdbmli    (-1.000)
                           - x38(m, f, b, c, k, j) * t3c(f, d, a, m, l, i) & !mfbckjfdamli    (-1.000)
                           + x38(m, f, a, c, k, j) * t3c(f, d, b, m, l, i) & !mfackjfdbmli    (+1.000)
                           + x38(m, f, b, a, k, j) * t3c(f, d, c, m, l, i) & !mfbakjfdcmli    (+1.000)
                           - x38(m, f, a, b, k, j) * t3c(f, d, c, m, l, i)      !mfabkjfdcmli    (-1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x38)

   allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u70), size(u70), '621345', u70, f1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (u120(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, f1, b2, u120)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x28), size(x28), '213456', &
                   -1.000, x28, u120)
   deallocate (u120)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x28,t2b) &
         !$omp private(a,b,c,d,f,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  sum = sum &
                        - x28(f, d, c, b, j, i) * t2b(f, a, l, k) & !fdcbjifalk      (-1.000)
                        + x28(f, d, c, a, j, i) * t2b(f, b, l, k) & !fdcajifblk      (+1.000)
                        + x28(f, d, b, c, j, i) * t2b(f, a, l, k) & !fdbcjifalk      (+1.000)
                        - x28(f, d, a, c, j, i) * t2b(f, b, l, k) & !fdacjifblk      (-1.000)
                        - x28(f, d, b, a, j, i) * t2b(f, c, l, k) & !fdbajifclk      (-1.000)
                        + x28(f, d, a, b, j, i) * t2b(f, c, l, k) & !fdabjifclk      (+1.000)
                        + x28(f, d, c, b, k, i) * t2b(f, a, l, j) & !fdcbkifalj      (+1.000)
                        - x28(f, d, c, a, k, i) * t2b(f, b, l, j) & !fdcakifblj      (-1.000)
                        - x28(f, d, b, c, k, i) * t2b(f, a, l, j) & !fdbckifalj      (-1.000)
                        + x28(f, d, a, c, k, i) * t2b(f, b, l, j) & !fdackifblj      (+1.000)
                        + x28(f, d, b, a, k, i) * t2b(f, c, l, j) & !fdbakifclj      (+1.000)
                        - x28(f, d, a, b, k, i) * t2b(f, c, l, j) & !fdabkifclj      (-1.000)
                        - x28(f, d, c, b, k, j) * t2b(f, a, l, i) & !fdcbkjfali      (-1.000)
                        + x28(f, d, c, a, k, j) * t2b(f, b, l, i) & !fdcakjfbli      (+1.000)
                        + x28(f, d, b, c, k, j) * t2b(f, a, l, i) & !fdbckjfali      (+1.000)
                        - x28(f, d, a, c, k, j) * t2b(f, b, l, i) & !fdackjfbli      (-1.000)
                        - x28(f, d, b, a, k, j) * t2b(f, c, l, i) & !fdbakjfcli      (-1.000)
                        + x28(f, d, a, b, k, j) * t2b(f, c, l, i)          !fdabkjfcli      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x28)

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u70), size(u70), '261345', u70, f1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (u119(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, f1, b2, u119)
   deallocate (f1)
   deallocate (b2)
   deallocate (u70)

   call sum_stripe(6, shape(x27), size(x27), '412356', &
                   1.000, x27, u119)
   deallocate (u119)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x27,t2b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n2
                  sum = sum &
                        + x27(n, c, b, l, j, i) * t2b(d, a, n, k) & !ncbljidank      (+1.000)
                        - x27(n, c, a, l, j, i) * t2b(d, b, n, k) & !ncaljidbnk      (-1.000)
                        - x27(n, b, c, l, j, i) * t2b(d, a, n, k) & !nbcljidank      (-1.000)
                        + x27(n, a, c, l, j, i) * t2b(d, b, n, k) & !nacljidbnk      (+1.000)
                        + x27(n, b, a, l, j, i) * t2b(d, c, n, k) & !nbaljidcnk      (+1.000)
                        - x27(n, a, b, l, j, i) * t2b(d, c, n, k) & !nabljidcnk      (-1.000)
                        - x27(n, c, b, l, k, i) * t2b(d, a, n, j) & !ncblkidanj      (-1.000)
                        + x27(n, c, a, l, k, i) * t2b(d, b, n, j) & !ncalkidbnj      (+1.000)
                        + x27(n, b, c, l, k, i) * t2b(d, a, n, j) & !nbclkidanj      (+1.000)
                        - x27(n, a, c, l, k, i) * t2b(d, b, n, j) & !naclkidbnj      (-1.000)
                        - x27(n, b, a, l, k, i) * t2b(d, c, n, j) & !nbalkidcnj      (-1.000)
                        + x27(n, a, b, l, k, i) * t2b(d, c, n, j) & !nablkidcnj      (+1.000)
                        + x27(n, c, b, l, k, j) * t2b(d, a, n, i) & !ncblkjdani      (+1.000)
                        - x27(n, c, a, l, k, j) * t2b(d, b, n, i) & !ncalkjdbni      (-1.000)
                        - x27(n, b, c, l, k, j) * t2b(d, a, n, i) & !nbclkjdani      (-1.000)
                        + x27(n, a, c, l, k, j) * t2b(d, b, n, i) & !naclkjdbni      (+1.000)
                        + x27(n, b, a, l, k, j) * t2b(d, c, n, i) & !nbalkjdcni      (+1.000)
                        - x27(n, a, b, l, k, j) * t2b(d, c, n, i)          !nablkjdcni      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x27)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
   allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(8, shape(t4c), size(t4c), '13524678', t4c, h2)
   allocate (u23(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k1 * k2 * k3 * k4
   i3 = k2 * k3 * k4
   call egemm(i1, i2, i3, d1, h2, u23)
   deallocate (d1)
   deallocate (h2)

   call sum_stripe(6, shape(x37), size(x37), '234561', &
                   -1.000, x37, u23)
   deallocate (u23)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
   allocate (s46(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k1 * k3
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, d2, s46)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x20), size(x20), '3421', 1.000, &
                   x20, s46)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s46), size(s46), '3412', s46, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u109(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u109)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x29), size(x29), '345126', &
                   -1.000, x29, u109)
   deallocate (u109)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s46), size(s46), '4312', s46, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s143(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s143)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                   x6, s143)
   deallocate (s143)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s46), size(s46), '3412', s46, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s142(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s142)
   deallocate (d1)
   deallocate (b2)
   deallocate (s46)

   call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                   s142)
   deallocate (s142)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u24(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u24)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x29), size(x29), '345126', &
                   -1.000, x29, u24)
   deallocate (u24)

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

   allocate (b1(n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                      size(b1), (/n0 - n0, n1 - n0/), '21', fockr, b1)
   allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
   allocate (s48(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
   i1 = k3
   i2 = k2 * k3 * k4
   i3 = k1
   call egemm(i1, i2, i3, b1, d2, s48)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                   x2, s48)
   deallocate (s48)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intr, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u25(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u25)
   deallocate (d1)
   deallocate (d2)

   allocate (x40(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x40 = 0.0d0
   call sum_stripe(6, shape(x40), size(x40), '345261', &
                   1.000, x40, u25)
   deallocate (u25)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intr, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '142356', t3a, f2)
   allocate (u26(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k1 * k3 * k3
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, f2, u26)
   deallocate (d1)
   deallocate (f2)

   allocate (x41(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x41 = 0.0d0
   call sum_stripe(6, shape(x41), size(x41), '234516', &
                   1.000, x41, u26)
   deallocate (u26)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
   allocate (s49(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k2 * k4
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, d2, s49)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '2341', -1.000, &
                   x1, s49)
   deallocate (s49)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u27(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u27)
   deallocate (d1)
   deallocate (d2)

   allocate (x42(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   x42 = 0.0d0
   call sum_stripe(6, shape(x42), size(x42), '356241', &
                   1.000, x42, u27)
   deallocate (u27)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intr, d1)
   allocate (f2(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '123456', t3a, f2)
   allocate (u28(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k1 * k1 * k1 * k3
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, f2, u28)
   deallocate (d1)
   deallocate (f2)

   allocate (x43(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x43 = 0.0d0
   call sum_stripe(6, shape(x43), size(x43), '345621', &
                   1.000, x43, u28)
   deallocate (u28)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
   allocate (s50(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i2 = k2 * k4
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, d2, s50)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2413', -1.000, &
                   x2, s50)
   deallocate (s50)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
   allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '412356', t3a, f2)
   allocate (u29(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k1 * k1 * k3 * k3
   i3 = k3 * k1
   call egemm(i1, i2, i3, d1, f2, u29)
   deallocate (d1)
   deallocate (f2)

   allocate (x44(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x44 = 0.0d0
   call sum_stripe(6, shape(x44), size(x44), '235641', &
                   1.000, x44, u29)
   deallocate (u29)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
   allocate (s51(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1
   i2 = k1 * k4
   i3 = k2 * k3
   call egemm(i1, i2, i3, d1, d2, s51)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '2413', -1.000, &
                   x1, s51)
   deallocate (s51)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u30(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k3
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u30)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x42), size(x42), '456231', &
                   1.000, x42, u30)
   deallocate (u30)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s52(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4
   i2 = k1 * k2
   i3 = k3 * k4
   call egemm(i1, i2, i3, d1, d2, s52)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '3421', 1.000, x1, &
                   s52)
   deallocate (s52)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1234', intm, d1)
   allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
   allocate (s53(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3
   i2 = k3 * k4
   i3 = k2 * k1
   call egemm(i1, i2, i3, d1, d2, s53)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2314', 1.000, x2, &
                   s53)
   deallocate (s53)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n2 - n0/), '4132', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '1423', t2b, d2)
   allocate (s54(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n2 + 1:n3))
   i1 = k4 * k3
   i2 = k2 * k3
   i3 = k1 * k4
   call egemm(i1, i2, i3, d1, d2, s54)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '3412', -1.000, &
                   x2, s54)
   deallocate (s54)

   allocate (b1(n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                      size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s55(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, b1, d2, s55)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                   s55)
   deallocate (s55)

   allocate (b1(n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                      size(b1), (/n0 - n0, n2 - n0/), '21', fockb, b1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (s56(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
   i1 = k4
   i2 = k1 * k3 * k4
   i3 = k2
   call egemm(i1, i2, i3, b1, d2, s56)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                   x6, s56)
   deallocate (s56)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u31(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k1
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u31)
   deallocate (d1)
   deallocate (d2)

   allocate (x45(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x45 = 0.0d0
   call sum_stripe(6, shape(x45), size(x45), '345261', &
                   1.000, x45, u31)
   deallocate (u31)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
   allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '512346', t3b, f2)
   allocate (u32(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1
   i2 = k1 * k2 * k3 * k3
   i3 = k4 * k1
   call egemm(i1, i2, i3, d1, f2, u32)
   deallocate (d1)
   deallocate (f2)

   allocate (x46(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x46 = 0.0d0
   call sum_stripe(6, shape(x46), size(x46), '234561', &
                   1.000, x46, u32)
   deallocate (u32)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
   allocate (s57(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s57)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x21), size(x21), '2314', -1.000, &
                   x21, s57)
   deallocate (s57)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
   allocate (u33(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k1 * k3 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, f2, u33)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x41), size(x41), '234516', &
                   1.000, x41, u33)
   deallocate (u33)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
   allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
   allocate (s58(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1
   i2 = k2 * k3
   i3 = k4 * k1
   call egemm(i1, i2, i3, d1, d2, s58)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2341', -1.000, &
                   x5, s58)
   deallocate (s58)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intm, d1)
   allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
   allocate (s59(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4
   i2 = k3 * k4
   i3 = k2 * k1
   call egemm(i1, i2, i3, d1, d2, s59)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '2314', 1.000, x6, &
                   s59)
   deallocate (s59)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u34(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k4
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u34)
   deallocate (d1)
   deallocate (d2)

   allocate (x47(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   x47 = 0.0d0
   call sum_stripe(6, shape(x47), size(x47), '356241', &
                   1.000, x47, u34)
   deallocate (u34)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u35(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u35)
   deallocate (d1)
   deallocate (d2)

   allocate (x48(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   x48 = 0.0d0
   call sum_stripe(6, shape(x48), size(x48), '456231', &
                   1.000, x48, u35)
   deallocate (u35)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s60(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3
   i2 = k1 * k2
   i3 = k3 * k4
   call egemm(i1, i2, i3, d1, d2, s60)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '3421', 1.000, x5, &
                   s60)
   deallocate (s60)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
   allocate (f2(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '123456', t3b, f2)
   allocate (u36(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3
   i2 = k1 * k1 * k2 * k3
   i3 = k3 * k4
   call egemm(i1, i2, i3, d1, f2, u36)
   deallocate (d1)
   deallocate (f2)

   allocate (x49(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   x49 = 0.0d0
   call sum_stripe(6, shape(x49), size(x49), '345621', &
                   1.000, x49, u36)
   deallocate (u36)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n1 - n0/), '3241', intm, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
   allocate (s61(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4
   i2 = k1 * k4
   i3 = k2 * k3
   call egemm(i1, i2, i3, d1, d2, s61)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '2413', -1.000, &
                   x6, s61)
   deallocate (s61)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
   allocate (s62(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i2 = k1 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s62)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x22), size(x22), '3412', 1.000, &
                   x22, s62)
   deallocate (s62)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intb, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
   allocate (u37(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k1 * k1 * k3 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, f2, u37)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x44), size(x44), '235614', &
                   1.000, x44, u37)
   deallocate (u37)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (s63(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i2 = k1 * k3
   i3 = k4 * k2
   call egemm(i1, i2, i3, d1, d2, s63)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x5), size(x5), '2431', -1.000, &
                   x5, s63)
   deallocate (s63)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '3412', intb, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u38(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k4
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u38)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x47), size(x47), '456231', &
                   1.000, x47, u38)
   deallocate (u38)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
   allocate (s64(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
   i1 = k4 * k4
   i2 = k1 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s64)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '3412', -1.000, &
                   x6, s64)
   deallocate (s64)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u39(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u39)
   deallocate (d1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,u39,t4a) &
         !$omp private(a,b,c,d,n,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n1 + 1, n3
                  do m = n0 + 1, n1
                  do n = n0 + 1, n1
                     sum = sum & !top two switched
                           + (u39(d, l, j, f, m, n) * t4a(f, c, b, a, n, m, k, i) & !dljfmnfcbanmki  (+0.500)
                              - u39(d, l, i, f, m, n) * t4a(f, c, b, a, n, m, k, j) & !dlifmnfcbanmkj  (-0.500)
                              - u39(d, l, k, f, m, n) * t4a(f, c, b, a, n, m, j, i)) / 2.0d0 !dlkfmnfcbanmji  (-0.500)
                  end do
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u39), size(u39), '541236', u39, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s123(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k4
   i3 = k3 * k1
   call egemm1(i1, i3, f1, b2, s123)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '2341', -1.000, &
                   x1, s123)
   deallocate (s123)

   allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u39), size(u39), '541236', u39, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u75(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k4 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u75)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x42), size(x42), '423561', &
                   -1.000, x42, u75)
   deallocate (u75)

   allocate (f1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u39), size(u39), '451236', u39, f1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (u73(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k4 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, f1, b2, u73)
   deallocate (f1)
   deallocate (b2)
   deallocate (u39)

   call sum_stripe(6, shape(x40), size(x40), '623451', &
                   -1.000, x40, u73)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x40,t3a) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n1
                     sum = sum &
                           + (x40(n, m, d, l, j, i) * t3a(c, b, a, n, m, k) & !nmdljicbanmk    (+0.500)
                              - x40(n, m, d, l, k, i) * t3a(c, b, a, n, m, j) & !nmdlkicbanmj    (-0.500)
                              - x40(n, m, d, l, i, j) * t3a(c, b, a, n, m, k) & !nmdlijcbanmk    (-0.500)
                              + x40(n, m, d, l, i, k) * t3a(c, b, a, n, m, j) & !nmdlikcbanmj    (+0.500)
                              + x40(n, m, d, l, k, j) * t3a(c, b, a, n, m, i) & !nmdlkjcbanmi    (+0.500)
                              - x40(n, m, d, l, j, k) * t3a(c, b, a, n, m, i)) / 2.0d0!nmdljkcbanmi    (-0.500)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x40)

   allocate (f1(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u73), size(u73), '263451', u73, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u114(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k4 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u114)
   deallocate (f1)
   deallocate (b2)
   deallocate (u73)

   call sum_stripe(6, shape(x23), size(x23), '312456', &
                   -1.000, x23, u114)
   deallocate (u114)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t2a,x23) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n1
                  sum = sum &
                        + x23(n, d, c, l, k, i) * t2a(b, a, n, j) & !ndclkibanj      (+1.000)
                        - x23(n, d, b, l, k, i) * t2a(c, a, n, j) & !ndblkicanj      (-1.000)
                        + x23(n, d, a, l, k, i) * t2a(c, b, n, j) & !ndalkicbnj      (+1.000)
                        - x23(n, d, c, l, j, i) * t2a(b, a, n, k) & !ndcljibank      (-1.000)
                        + x23(n, d, b, l, j, i) * t2a(c, a, n, k) & !ndbljicank      (+1.000)
                        - x23(n, d, a, l, j, i) * t2a(c, b, n, k) & !ndaljicbnk      (-1.000)
                        - x23(n, d, c, l, k, j) * t2a(b, a, n, i) & !ndclkjbani      (-1.000)
                        + x23(n, d, b, l, k, j) * t2a(c, a, n, i) & !ndblkjcani      (+1.000)
                        - x23(n, d, a, l, k, j) * t2a(c, b, n, i) & !ndalkjcbni      (-1.000)
                        + x23(n, d, c, l, j, k) * t2a(b, a, n, i) & !ndcljkbani      (+1.000)
                        - x23(n, d, b, l, j, k) * t2a(c, a, n, i) & !ndbljkcani      (-1.000)
                        + x23(n, d, a, l, j, k) * t2a(c, b, n, i) & !ndaljkcbni      (+1.000)
                        + x23(n, d, c, l, i, j) * t2a(b, a, n, k) & !ndclijbank      (+1.000)
                        - x23(n, d, b, l, i, j) * t2a(c, a, n, k) & !ndblijcank      (-1.000)
                        + x23(n, d, a, l, i, j) * t2a(c, b, n, k) & !ndalijcbnk      (+1.000)
                        - x23(n, d, c, l, i, k) * t2a(b, a, n, j) & !ndclikbanj      (-1.000)
                        + x23(n, d, b, l, i, k) * t2a(c, a, n, j) & !ndblikcanj      (+1.000)
                        - x23(n, d, a, l, i, k) * t2a(c, b, n, j)          !ndalikcbnj      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x23)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
   allocate (h2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, &
                n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(8, shape(t4a), size(t4a), '12534678', t4a, h2)
   allocate (u40(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k1 * k1 * k3 * k3
   i3 = k1 * k3 * k3
   call egemm(i1, i2, i3, d1, h2, u40)
   deallocate (d1)
   deallocate (h2)

   allocate (x50(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   x50 = 0.0d0
   call sum_stripe(6, shape(x50), size(x50), '234561', &
                   1.000, x50, u40)
   deallocate (u40)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
   allocate (s65(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k2 * k4
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, d2, s65)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x7), size(x7), '3421', 1.000, x7, &
                   s65)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s65), size(s65), '4312', s65, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s124(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s124)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x2), size(x2), '3124', -1.000, &
                   x2, s124)
   deallocate (s124)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s65), size(s65), '3412', s65, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s122(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s122)
   deallocate (d1)
   deallocate (b2)
   deallocate (s65)

   call sum_stripe(4, shape(x1), size(x1), '4123', 1.000, x1, &
                   s122)
   deallocate (s122)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
   allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
   allocate (u41(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k4
   i2 = k1 * k2 * k4
   i3 = k3
   call egemm(i1, i2, i3, d1, d2, u41)
   deallocate (d1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4b,u41) &
         !$omp private(a,b,c,d,n,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  do m = n0 + 1, n1
                  do n = n0 + 1, n2
                     sum = sum &
                           - u41(d, l, i, f, m, n) * t4b(f, c, b, a, n, m, k, j) & !dlifmnfcbanmkj  (-1.000)
                           + u41(d, l, j, f, m, n) * t4b(f, c, b, a, n, m, k, i) & !dljfmnfcbanmki  (+1.000)
                           - u41(d, l, k, f, m, n) * t4b(f, c, b, a, n, m, j, i)  !dlkfmnfcbanmji  (-1.000)
                  end do
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(u41), size(u41), '465123', u41, f1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
   allocate (u106(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k4 * k1
   i2 = k1 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, f1, d2, u106)
   deallocate (f1)
   deallocate (d2)

   call sum_stripe(6, shape(x26), size(x26), '351246', &
                   1.000, x26, u106)
   deallocate (u106)

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(u41), size(u41), '465123', u41, f1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s147(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k4 * k1
   i3 = k2 * k4
   call egemm1(i1, i3, f1, b2, s147)
   deallocate (f1)
   deallocate (b2)

   x1 = x1 + s147
   deallocate (s147)

   allocate (f1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u41), size(u41), '541236', u41, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u80(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k2 * k4 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u80)
   deallocate (f1)
   deallocate (b2)
   deallocate (u41)

   call sum_stripe(6, shape(x47), size(x47), '423561', &
                   -1.000, x47, u80)
   deallocate (u80)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u42(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k3
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u42)
   deallocate (d1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,u42,t4b) &
         !$omp private(a,b,c,d,e,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do m = n0 + 1, n1
                  do n = n0 + 1, n2
                     sum = sum &
                           - u42(c, l, i, e, m, n) * t4b(d, e, b, a, n, m, k, j) & !cliemndebanmkj  (-1.000)
                           + u42(b, l, i, e, m, n) * t4b(d, e, c, a, n, m, k, j) & !bliemndecanmkj  (+1.000)
                           - u42(a, l, i, e, m, n) * t4b(d, e, c, b, n, m, k, j) & !aliemndecbnmkj  (-1.000)
                           + u42(c, l, j, e, m, n) * t4b(d, e, b, a, n, m, k, i) & !cljemndebanmki  (+1.000)
                           - u42(b, l, j, e, m, n) * t4b(d, e, c, a, n, m, k, i) & !bljemndecanmki  (-1.000)
                           + u42(a, l, j, e, m, n) * t4b(d, e, c, b, n, m, k, i) & !aljemndecbnmki  (+1.000)
                           - u42(c, l, k, e, m, n) * t4b(d, e, b, a, n, m, j, i) & !clkemndebanmji  (-1.000)
                           + u42(b, l, k, e, m, n) * t4b(d, e, c, a, n, m, j, i) & !blkemndecanmji  (+1.000)
                           - u42(a, l, k, e, m, n) * t4b(d, e, c, b, n, m, j, i)  !alkemndecbnmji  (-1.000)
                  end do
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(u42), size(u42), '465123', u42, f1)
   allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
   allocate (u107(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k3 * k1
   i2 = k1 * k4
   i3 = k2 * k3
   call egemm(i1, i2, i3, f1, d2, u107)
   deallocate (f1)
   deallocate (d2)

   call sum_stripe(6, shape(x26), size(x26), '251346', &
                   1.000, x26, u107)
   deallocate (u107)

   allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(u42), size(u42), '654123', u42, f1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (u94(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k3 * k3 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, f1, b2, u94)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x42), size(x42), '312456', &
                   -1.000, x42, u94)
   deallocate (u94)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x42,t3a) &
         !$omp private(a,b,c,d,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n1 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           + x42(m, f, d, c, l, i) * t3a(f, b, a, m, k, j) & !mfdclifbamkj    (+1.000)
                           - x42(m, f, d, b, l, i) * t3a(f, c, a, m, k, j) & !mfdblifcamkj    (-1.000)
                           + x42(m, f, d, a, l, i) * t3a(f, c, b, m, k, j) & !mfdalifcbmkj    (+1.000)
                           - x42(m, f, d, c, l, j) * t3a(f, b, a, m, k, i) & !mfdcljfbamki    (-1.000)
                           + x42(m, f, d, b, l, j) * t3a(f, c, a, m, k, i) & !mfdbljfcamki    (+1.000)
                           - x42(m, f, d, a, l, j) * t3a(f, c, b, m, k, i) & !mfdaljfcbmki    (-1.000)
                           + x42(m, f, d, c, l, k) * t3a(f, b, a, m, j, i) & !mfdclkfbamji    (+1.000)
                           - x42(m, f, d, b, l, k) * t3a(f, c, a, m, j, i) & !mfdblkfcamji    (-1.000)
                           + x42(m, f, d, a, l, k) * t3a(f, c, b, m, j, i)      !mfdalkfcbmji    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x42)

   allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u42), size(u42), '541236', u42, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s130(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k2 * k3
   i3 = k3 * k1
   call egemm1(i1, i3, f1, b2, s130)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                   s130)
   deallocate (s130)

   allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u42), size(u42), '541236', u42, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u81(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k2 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u81)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x48), size(x48), '324561', &
                   -1.000, x48, u81)
   deallocate (u81)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,x48) &
         !$omp private(a,b,c,d,e,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           + x48(m, e, c, b, l, i) * t3b(d, e, a, m, k, j) & !mecblideamkj    (+1.000)
                           - x48(m, e, c, a, l, i) * t3b(d, e, b, m, k, j) & !mecalidebmkj    (-1.000)
                           - x48(m, e, b, c, l, i) * t3b(d, e, a, m, k, j) & !mebclideamkj    (-1.000)
                           + x48(m, e, a, c, l, i) * t3b(d, e, b, m, k, j) & !meaclidebmkj    (+1.000)
                           + x48(m, e, b, a, l, i) * t3b(d, e, c, m, k, j) & !mebalidecmkj    (+1.000)
                           - x48(m, e, a, b, l, i) * t3b(d, e, c, m, k, j) & !meablidecmkj    (-1.000)
                           - x48(m, e, c, b, l, j) * t3b(d, e, a, m, k, i) & !mecbljdeamki    (-1.000)
                           + x48(m, e, c, a, l, j) * t3b(d, e, b, m, k, i) & !mecaljdebmki    (+1.000)
                           + x48(m, e, b, c, l, j) * t3b(d, e, a, m, k, i) & !mebcljdeamki    (+1.000)
                           - x48(m, e, a, c, l, j) * t3b(d, e, b, m, k, i) & !meacljdebmki    (-1.000)
                           - x48(m, e, b, a, l, j) * t3b(d, e, c, m, k, i) & !mebaljdecmki    (-1.000)
                           + x48(m, e, a, b, l, j) * t3b(d, e, c, m, k, i) & !meabljdecmki    (+1.000)
                           + x48(m, e, c, b, l, k) * t3b(d, e, a, m, j, i) & !mecblkdeamji    (+1.000)
                           - x48(m, e, c, a, l, k) * t3b(d, e, b, m, j, i) & !mecalkdebmji    (-1.000)
                           - x48(m, e, b, c, l, k) * t3b(d, e, a, m, j, i) & !mebclkdeamji    (-1.000)
                           + x48(m, e, a, c, l, k) * t3b(d, e, b, m, j, i) & !meaclkdebmji    (+1.000)
                           + x48(m, e, b, a, l, k) * t3b(d, e, c, m, j, i) & !mebalkdecmji    (+1.000)
                           - x48(m, e, a, b, l, k) * t3b(d, e, c, m, j, i)      !meablkdecmji    (-1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x48)

   allocate (f1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u42), size(u42), '451236', u42, f1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (u77(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k2 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, f1, b2, u77)
   deallocate (f1)
   deallocate (b2)
   deallocate (u42)

   call sum_stripe(6, shape(x45), size(x45), '623451', &
                   1.000, x45, u77)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,x45) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n2
                     sum = sum &
                           + x45(n, m, c, l, j, i) * t3b(d, b, a, n, m, k) & !nmcljidbanmk    (+1.000)
                           - x45(n, m, b, l, j, i) * t3b(d, c, a, n, m, k) & !nmbljidcanmk    (-1.000)
                           + x45(n, m, a, l, j, i) * t3b(d, c, b, n, m, k) & !nmaljidcbnmk    (+1.000)
                           - x45(n, m, c, l, k, i) * t3b(d, b, a, n, m, j) & !nmclkidbanmj    (-1.000)
                           + x45(n, m, b, l, k, i) * t3b(d, c, a, n, m, j) & !nmblkidcanmj    (+1.000)
                           - x45(n, m, a, l, k, i) * t3b(d, c, b, n, m, j) & !nmalkidcbnmj    (-1.000)
                           - x45(n, m, c, l, i, j) * t3b(d, b, a, n, m, k) & !nmclijdbanmk    (-1.000)
                           + x45(n, m, b, l, i, j) * t3b(d, c, a, n, m, k) & !nmblijdcanmk    (+1.000)
                           - x45(n, m, a, l, i, j) * t3b(d, c, b, n, m, k) & !nmalijdcbnmk    (-1.000)
                           + x45(n, m, c, l, i, k) * t3b(d, b, a, n, m, j) & !nmclikdbanmj    (+1.000)
                           - x45(n, m, b, l, i, k) * t3b(d, c, a, n, m, j) & !nmblikdcanmj    (-1.000)
                           + x45(n, m, a, l, i, k) * t3b(d, c, b, n, m, j) & !nmalikdcbnmj    (+1.000)
                           + x45(n, m, c, l, k, j) * t3b(d, b, a, n, m, i) & !nmclkjdbanmi    (+1.000)
                           - x45(n, m, b, l, k, j) * t3b(d, c, a, n, m, i) & !nmblkjdcanmi    (-1.000)
                           + x45(n, m, a, l, k, j) * t3b(d, c, b, n, m, i) & !nmalkjdcbnmi    (+1.000)
                           - x45(n, m, c, l, j, k) * t3b(d, b, a, n, m, i) & !nmcljkdbanmi    (-1.000)
                           + x45(n, m, b, l, j, k) * t3b(d, c, a, n, m, i) & !nmbljkdcanmi    (+1.000)
                           - x45(n, m, a, l, j, k) * t3b(d, c, b, n, m, i)      !nmaljkdcbnmi    (-1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x45)

   allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u77), size(u77), '623451', u77, f1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (u118(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k3 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, f1, b2, u118)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x26), size(x26), '213456', &
                   -1.000, x26, u118)
   deallocate (u118)

   allocate (f1(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u77), size(u77), '263451', u77, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u116(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k3 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u116)
   deallocate (f1)
   deallocate (b2)
   deallocate (u77)

   call sum_stripe(6, shape(x29), size(x29), '213456', &
                   1.000, x29, u116)
   deallocate (u116)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x29,t2b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n2
                  sum = sum &
                        - x29(n, c, b, l, j, i) * t2b(d, a, n, k) & !ncbljidank      (-1.000)
                        + x29(n, c, a, l, j, i) * t2b(d, b, n, k) & !ncaljidbnk      (+1.000)
                        + x29(n, b, c, l, j, i) * t2b(d, a, n, k) & !nbcljidank      (+1.000)
                        - x29(n, a, c, l, j, i) * t2b(d, b, n, k) & !nacljidbnk      (-1.000)
                        - x29(n, b, a, l, j, i) * t2b(d, c, n, k) & !nbaljidcnk      (-1.000)
                        + x29(n, a, b, l, j, i) * t2b(d, c, n, k) & !nabljidcnk      (+1.000)
                        - x29(n, c, a, l, k, i) * t2b(d, b, n, j) & !ncalkidbnj      (-1.000)
                        + x29(n, c, b, l, k, i) * t2b(d, a, n, j) & !ncblkidanj      (+1.000)
                        + x29(n, b, a, l, k, i) * t2b(d, c, n, j) & !nbalkidcnj      (+1.000)
                        - x29(n, a, b, l, k, i) * t2b(d, c, n, j) & !nablkidcnj      (-1.000)
                        - x29(n, b, c, l, k, i) * t2b(d, a, n, j) & !nbclkidanj      (-1.000)
                        + x29(n, a, c, l, k, i) * t2b(d, b, n, j) & !naclkidbnj      (+1.000)
                        + x29(n, c, b, l, i, j) * t2b(d, a, n, k) & !ncblijdank      (+1.000)
                        - x29(n, c, a, l, i, j) * t2b(d, b, n, k) & !ncalijdbnk      (-1.000)
                        - x29(n, b, c, l, i, j) * t2b(d, a, n, k) & !nbclijdank      (-1.000)
                        + x29(n, a, c, l, i, j) * t2b(d, b, n, k) & !naclijdbnk      (+1.000)
                        + x29(n, b, a, l, i, j) * t2b(d, c, n, k) & !nbalijdcnk      (+1.000)
                        - x29(n, a, b, l, i, j) * t2b(d, c, n, k) & !nablijdcnk      (-1.000)
                        + x29(n, c, a, l, k, j) * t2b(d, b, n, i) & !ncalkjdbni      (+1.000)
                        - x29(n, c, b, l, k, j) * t2b(d, a, n, i) & !ncblkjdani      (-1.000)
                        - x29(n, b, a, l, k, j) * t2b(d, c, n, i) & !nbalkjdcni      (-1.000)
                        + x29(n, a, b, l, k, j) * t2b(d, c, n, i) & !nablkjdcni      (+1.000)
                        + x29(n, b, c, l, k, j) * t2b(d, a, n, i) & !nbclkjdani      (+1.000)
                        - x29(n, a, c, l, k, j) * t2b(d, b, n, i) & !naclkjdbni      (-1.000)
                        - x29(n, c, b, l, i, k) * t2b(d, a, n, j) & !ncblikdanj      (-1.000)
                        + x29(n, c, a, l, i, k) * t2b(d, b, n, j) & !ncalikdbnj      (+1.000)
                        + x29(n, b, c, l, i, k) * t2b(d, a, n, j) & !nbclikdanj      (+1.000)
                        - x29(n, a, c, l, i, k) * t2b(d, b, n, j) & !naclikdbnj      (-1.000)
                        - x29(n, b, a, l, i, k) * t2b(d, c, n, j) & !nbalikdcnj      (-1.000)
                        + x29(n, a, b, l, i, k) * t2b(d, c, n, j) & !nablikdcnj      (+1.000)
                        - x29(n, c, a, l, j, k) * t2b(d, b, n, i) & !ncaljkdbni      (-1.000)
                        + x29(n, c, b, l, j, k) * t2b(d, a, n, i) & !ncbljkdani      (+1.000)
                        + x29(n, b, a, l, j, k) * t2b(d, c, n, i) & !nbaljkdcni      (+1.000)
                        - x29(n, a, b, l, j, k) * t2b(d, c, n, i) & !nabljkdcni      (-1.000)
                        - x29(n, b, c, l, j, k) * t2b(d, a, n, i) & !nbcljkdani      (-1.000)
                        + x29(n, a, c, l, j, k) * t2b(d, b, n, i)          !nacljkdbni      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x29)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (s66(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1
   i2 = k1 * k2
   i3 = k3 * k4
   call egemm(i1, i2, i3, d1, d2, s66)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x15), size(x15), '3421', 1.000, &
                   x15, s66)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s66), size(s66), '3412', s66, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u108(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u108)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x25), size(x25), '236145', &
                   -1.000, x25, u108)
   deallocate (u108)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s66), size(s66), '4312', s66, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s146(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s146)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s146)
   deallocate (s146)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s66), size(s66), '3412', s66, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s129(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s129)
   deallocate (d1)
   deallocate (b2)
   deallocate (s66)

   call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                   x5, s129)
   deallocate (s129)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
   allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(8, shape(t4b), size(t4b), '12634578', t4b, h2)
   allocate (u43(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2
   i2 = k1 * k1 * k2 * k3 * k3
   i3 = k1 * k3 * k4
   call egemm(i1, i2, i3, d1, h2, u43)
   deallocate (d1)
   deallocate (h2)

   call sum_stripe(6, shape(x44), size(x44), '234561', &
                   1.000, x44, u43)
   deallocate (u43)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3241', intm, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
   allocate (s67(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4
   i2 = k1 * k4
   i3 = k2 * k3
   call egemm(i1, i2, i3, d1, d2, s67)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x16), size(x16), '3421', -1.000, &
                   x16, s67)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s67), size(s67), '3412', s67, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s145(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s145)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x1), size(x1), '3124', -1.000, &
                   x1, s145)
   deallocate (s145)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s67), size(s67), '4312', s67, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s131(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s131)
   deallocate (d1)
   deallocate (b2)
   deallocate (s67)

   call sum_stripe(4, shape(x6), size(x6), '3124', 1.000, x6, &
                   s131)
   deallocate (s131)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
   allocate (s68(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k1 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s68)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x13), size(x13), '3421', 1.000, &
                   x13, s68)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s68), size(s68), '4312', s68, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s133(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s133)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x22), size(x22), '2134', -1.000, &
                   x22, s133)
   deallocate (s133)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,x22) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  sum = sum &
                        - x22(e, c, b, i) * t3b(d, e, a, l, k, j) & !ecbidealkj      (-1.000)
                        + x22(e, c, a, i) * t3b(d, e, b, l, k, j) & !ecaideblkj      (+1.000)
                        + x22(e, b, c, i) * t3b(d, e, a, l, k, j) & !ebcidealkj      (+1.000)
                        - x22(e, a, c, i) * t3b(d, e, b, l, k, j) & !eacideblkj      (-1.000)
                        - x22(e, b, a, i) * t3b(d, e, c, l, k, j) & !ebaideclkj      (-1.000)
                        + x22(e, a, b, i) * t3b(d, e, c, l, k, j) & !eabideclkj      (+1.000)
                        + x22(e, c, b, j) * t3b(d, e, a, l, k, i) & !ecbjdealki      (+1.000)
                        - x22(e, c, a, j) * t3b(d, e, b, l, k, i) & !ecajdeblki      (-1.000)
                        - x22(e, b, c, j) * t3b(d, e, a, l, k, i) & !ebcjdealki      (-1.000)
                        + x22(e, a, c, j) * t3b(d, e, b, l, k, i) & !eacjdeblki      (+1.000)
                        + x22(e, b, a, j) * t3b(d, e, c, l, k, i) & !ebajdeclki      (+1.000)
                        - x22(e, a, b, j) * t3b(d, e, c, l, k, i) & !eabjdeclki      (-1.000)
                        - x22(e, c, b, k) * t3b(d, e, a, l, j, i) & !ecbkdealji      (-1.000)
                        + x22(e, c, a, k) * t3b(d, e, b, l, j, i) & !ecakdeblji      (+1.000)
                        + x22(e, b, c, k) * t3b(d, e, a, l, j, i) & !ebckdealji      (+1.000)
                        - x22(e, a, c, k) * t3b(d, e, b, l, j, i) & !eackdeblji      (-1.000)
                        - x22(e, b, a, k) * t3b(d, e, c, l, j, i) & !ebakdeclji      (-1.000)
                        + x22(e, a, b, k) * t3b(d, e, c, l, j, i)          !eabkdeclji      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x22)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s68), size(s68), '3412', s68, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s126(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s126)
   deallocate (d1)
   deallocate (b2)
   deallocate (s68)

   call sum_stripe(4, shape(x21), size(x21), '4123', -1.000, &
                   x21, s126)
   deallocate (s126)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x21,t3b) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        + x21(m, c, j, i) * t3b(d, b, a, l, m, k) & !mcjidbalmk      (+1.000)
                        - x21(m, b, j, i) * t3b(d, c, a, l, m, k) & !mbjidcalmk      (-1.000)
                        + x21(m, a, j, i) * t3b(d, c, b, l, m, k) & !majidcblmk      (+1.000)
                        - x21(m, c, k, i) * t3b(d, b, a, l, m, j) & !mckidbalmj      (-1.000)
                        + x21(m, b, k, i) * t3b(d, c, a, l, m, j) & !mbkidcalmj      (+1.000)
                        - x21(m, a, k, i) * t3b(d, c, b, l, m, j) & !makidcblmj      (-1.000)
                        - x21(m, c, i, j) * t3b(d, b, a, l, m, k) & !mcijdbalmk      (-1.000)
                        + x21(m, b, i, j) * t3b(d, c, a, l, m, k) & !mbijdcalmk      (+1.000)
                        - x21(m, a, i, j) * t3b(d, c, b, l, m, k) & !maijdcblmk      (-1.000)
                        + x21(m, c, i, k) * t3b(d, b, a, l, m, j) & !mcikdbalmj      (+1.000)
                        - x21(m, b, i, k) * t3b(d, c, a, l, m, j) & !mbikdcalmj      (-1.000)
                        + x21(m, a, i, k) * t3b(d, c, b, l, m, j) & !maikdcblmj      (+1.000)
                        + x21(m, c, k, j) * t3b(d, b, a, l, m, i) & !mckjdbalmi      (+1.000)
                        - x21(m, b, k, j) * t3b(d, c, a, l, m, i) & !mbkjdcalmi      (-1.000)
                        + x21(m, a, k, j) * t3b(d, c, b, l, m, i) & !makjdcblmi      (+1.000)
                        - x21(m, c, j, k) * t3b(d, b, a, l, m, i) & !mcjkdbalmi      (-1.000)
                        + x21(m, b, j, k) * t3b(d, c, a, l, m, i) & !mbjkdcalmi      (+1.000)
                        - x21(m, a, j, k) * t3b(d, c, b, l, m, i)          !majkdcblmi      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x21)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (q15(n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1
   i3 = k2 * k3 * k4
   call egemm(i1, i2, i3, d1, d2, q15)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(2, shape(x8), size(x8), '21', 1.000, x8, &
                   q15)
   deallocate (q15)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
   allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, &
                n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(8, shape(t4b), size(t4b), '12534678', t4b, h2)
   allocate (u44(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k1 * k1 * k3 * k3
   i3 = k2 * k3 * k4
   call egemm(i1, i2, i3, d1, h2, u44)
   deallocate (d1)
   deallocate (h2)

   call sum_stripe(6, shape(x50), size(x50), '234561', &
                   2.000, x50, u44)
   deallocate (u44)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x50,t2b) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum & !top two switched
                        + (x50(m, c, a, k, j, i) * t2b(d, b, l, m) & !mcakjidblm      (+0.500)
                           - x50(m, b, a, k, j, i) * t2b(d, c, l, m) & !mbakjidclm      (-0.500)
                           - x50(m, c, b, k, j, i) * t2b(d, a, l, m)) / 2.0d0   !mcbkjidalm      (-0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x50)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
   allocate (s69(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k2 * k4
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, d2, s69)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x19), size(x19), '3421', 1.000, &
                   x19, s69)
   deallocate (s69)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4132', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '1423', t2b, d2)
   allocate (s70(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3
   i2 = k2 * k3
   i3 = k1 * k4
   call egemm(i1, i2, i3, d1, d2, s70)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x17), size(x17), '3421', -1.000, &
                   x17, s70)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s70), size(s70), '4312', s70, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s149(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s149)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                   s149)
   deallocate (s149)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s70), size(s70), '3412', s70, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s127(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s127)
   deallocate (d1)
   deallocate (b2)
   deallocate (s70)

   call sum_stripe(4, shape(x5), size(x5), '4123', -1.000, &
                   x5, s127)
   deallocate (s127)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '1243', t2b, d2)
   allocate (q16(n0 + 1:n2, n0 + 1:n2))
   i1 = k2
   i2 = k2
   i3 = k1 * k3 * k4
   call egemm(i1, i2, i3, d1, d2, q16)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                   x10, q16)
   deallocate (q16)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '1243', intm, d1)
   allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
   allocate (s71(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4
   i2 = k3 * k4
   i3 = k2 * k1
   call egemm(i1, i2, i3, d1, d2, s71)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x18), size(x18), '3412', 1.000, &
                   x18, s71)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s71), size(s71), '3412', s71, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s148(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k3
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s148)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                   s148)
   deallocate (s148)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s71), size(s71), '4312', s71, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s128(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
   i1 = k3 * k4 * k4
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s128)
   deallocate (d1)
   deallocate (b2)
   deallocate (s71)

   call sum_stripe(4, shape(x6), size(x6), '4123', 1.000, x6, &
                   s128)
   deallocate (s128)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intm, d1)
   allocate (d2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(4, shape(t2b), size(t2b), '2431', t2b, d2)
   allocate (q17(n2 + 1:n3, n2 + 1:n3))
   i1 = k4
   i2 = k4
   i3 = k2 * k1 * k3
   call egemm(i1, i2, i3, d1, d2, q17)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(2, shape(x11), size(x11), '21', -1.000, &
                   x11, q17)
   deallocate (q17)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
   call reorder_stripe(4, shape(t2b), size(t2b), '1432', t2b, d2)
   allocate (q18(n1 + 1:n3, n1 + 1:n3))
   i1 = k3
   i2 = k3
   i3 = k2 * k1 * k4
   call egemm(i1, i2, i3, d1, d2, q18)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(2, shape(x9), size(x9), '21', -1.000, x9, &
                   q18)
   deallocate (q18)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intb, d1)
   allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
   allocate (u45(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k1 * k2 * k3
   i3 = k4
   call egemm(i1, i2, i3, d1, d2, u45)
   deallocate (d1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4c,u45) &
         !$omp private(a,b,c,d,n,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  do m = n0 + 1, n2
                  do n = n0 + 1, n2
                     sum = sum & !top two switched
                           + (u45(b, l, i, f, m, n) * t4c(f, d, c, a, n, m, k, j) & !blifmnfdcanmkj  (+0.500)
                              - u45(c, l, i, f, m, n) * t4c(f, d, b, a, n, m, k, j) & !clifmnfdbanmkj  (-0.500)
                              - u45(a, l, i, f, m, n) * t4c(f, d, c, b, n, m, k, j) & !alifmnfdcbnmkj  (-0.500)
                              + u45(c, l, j, f, m, n) * t4c(f, d, b, a, n, m, k, i) & !cljfmnfdbanmki  (+0.500)
                              - u45(b, l, j, f, m, n) * t4c(f, d, c, a, n, m, k, i) & !bljfmnfdcanmki  (-0.500)
                              + u45(a, l, j, f, m, n) * t4c(f, d, c, b, n, m, k, i) & !aljfmnfdcbnmki  (+0.500)
                              - u45(c, l, k, f, m, n) * t4c(f, d, b, a, n, m, j, i) & !clkfmnfdbanmji  (-0.500)
                              + u45(b, l, k, f, m, n) * t4c(f, d, c, a, n, m, j, i) & !blkfmnfdcanmji  (+0.500)
                              - u45(a, l, k, f, m, n) * t4c(f, d, c, b, n, m, j, i)) / 2.0d0!alkfmnfdcbnmji  (-0.500)
                  end do
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(u45), size(u45), '465123', u45, f1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
   allocate (u111(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k3 * k2
   i2 = k1 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, f1, d2, u111)
   deallocate (f1)
   deallocate (d2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t2b,u111) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        - u111(b, k, m, c, l, i) * t2b(d, a, m, j) & !bkmclidamj      (-1.000)
                        + u111(c, k, m, b, l, i) * t2b(d, a, m, j) & !ckmblidamj      (+1.000)
                        - u111(c, k, m, a, l, i) * t2b(d, b, m, j) & !ckmalidbmj      (-1.000)
                        + u111(b, j, m, c, l, i) * t2b(d, a, m, k) & !bjmclidamk      (+1.000)
                        - u111(c, j, m, b, l, i) * t2b(d, a, m, k) & !cjmblidamk      (-1.000)
                        + u111(c, j, m, a, l, i) * t2b(d, b, m, k) & !cjmalidbmk      (+1.000)
                        + u111(b, k, m, c, l, j) * t2b(d, a, m, i) & !bkmcljdami      (+1.000)
                        + u111(c, k, m, a, l, j) * t2b(d, b, m, i) & !ckmaljdbmi      (+1.000)
                        - u111(c, k, m, b, l, j) * t2b(d, a, m, i) & !ckmbljdami      (-1.000)
                        + u111(c, i, m, b, l, j) * t2b(d, a, m, k) & !cimbljdamk      (+1.000)
                        - u111(c, i, m, a, l, j) * t2b(d, b, m, k) & !cimaljdbmk      (-1.000)
                        - u111(b, i, m, c, l, j) * t2b(d, a, m, k) & !bimcljdamk      (-1.000)
                        - u111(c, j, m, a, l, k) * t2b(d, b, m, i) & !cjmalkdbmi      (-1.000)
                        + u111(c, j, m, b, l, k) * t2b(d, a, m, i) & !cjmblkdami      (+1.000)
                        - u111(b, j, m, c, l, k) * t2b(d, a, m, i) & !bjmclkdami      (-1.000)
                        + u111(c, i, m, a, l, k) * t2b(d, b, m, j) & !cimalkdbmj      (+1.000)
                        - u111(c, i, m, b, l, k) * t2b(d, a, m, j) & !cimblkdamj      (-1.000)
                        + u111(b, i, m, c, l, k) * t2b(d, a, m, j) & !bimclkdamj      (+1.000)
                        + u111(a, k, m, c, l, i) * t2b(d, b, m, j) & !akmclidbmj      (+1.000)
                        - u111(a, k, m, b, l, i) * t2b(d, c, m, j) & !akmblidcmj      (-1.000)
                        + u111(b, k, m, a, l, i) * t2b(d, c, m, j) & !bkmalidcmj      (+1.000)
                        - u111(a, j, m, c, l, i) * t2b(d, b, m, k) & !ajmclidbmk      (-1.000)
                        + u111(a, j, m, b, l, i) * t2b(d, c, m, k) & !ajmblidcmk      (+1.000)
                        - u111(b, j, m, a, l, i) * t2b(d, c, m, k) & !bjmalidcmk      (-1.000)
                        - u111(a, k, m, c, l, j) * t2b(d, b, m, i) & !akmcljdbmi      (-1.000)
                        + u111(a, k, m, b, l, j) * t2b(d, c, m, i) & !akmbljdcmi      (+1.000)
                        - u111(b, k, m, a, l, j) * t2b(d, c, m, i) & !bkmaljdcmi      (-1.000)
                        + u111(a, i, m, c, l, j) * t2b(d, b, m, k) & !aimcljdbmk      (+1.000)
                        - u111(a, i, m, b, l, j) * t2b(d, c, m, k) & !aimbljdcmk      (-1.000)
                        + u111(b, i, m, a, l, j) * t2b(d, c, m, k) & !bimaljdcmk      (+1.000)
                        + u111(a, j, m, c, l, k) * t2b(d, b, m, i) & !ajmclkdbmi      (+1.000)
                        - u111(a, j, m, b, l, k) * t2b(d, c, m, i) & !ajmblkdcmi      (-1.000)
                        + u111(b, j, m, a, l, k) * t2b(d, c, m, i) & !bjmalkdcmi      (+1.000)
                        - u111(a, i, m, c, l, k) * t2b(d, b, m, j) & !aimclkdbmj      (-1.000)
                        + u111(a, i, m, b, l, k) * t2b(d, c, m, j) & !aimblkdcmj      (+1.000)
                        - u111(b, i, m, a, l, k) * t2b(d, c, m, j)         !bimalkdcmj      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (u111)

   allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u45), size(u45), '541236', u45, f1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s152(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k2 * k3
   i3 = k4 * k2
   call egemm1(i1, i3, f1, b2, s152)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(4, shape(x5), size(x5), '2341', -1.000, &
                   x5, s152)
   deallocate (s152)

   allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(6, shape(u45), size(u45), '541236', u45, f1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (u96(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k2 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, f1, b2, u96)
   deallocate (f1)
   deallocate (b2)
   deallocate (u45)

   call sum_stripe(6, shape(x47), size(x47), '324561', &
                   -1.000, x47, u96)
   deallocate (u96)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,x47) &
         !$omp private(a,b,c,d,f,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           + x47(m, f, d, c, l, i) * t3b(f, b, a, m, k, j) & !mfdclifbamkj    (+1.000)
                           - x47(m, f, d, b, l, i) * t3b(f, c, a, m, k, j) & !mfdblifcamkj    (-1.000)
                           + x47(m, f, d, a, l, i) * t3b(f, c, b, m, k, j) & !mfdalifcbmkj    (+1.000)
                           - x47(m, f, d, c, l, j) * t3b(f, b, a, m, k, i) & !mfdcljfbamki    (-1.000)
                           + x47(m, f, d, b, l, j) * t3b(f, c, a, m, k, i) & !mfdbljfcamki    (+1.000)
                           - x47(m, f, d, a, l, j) * t3b(f, c, b, m, k, i) & !mfdaljfcbmki    (-1.000)
                           + x47(m, f, d, c, l, k) * t3b(f, b, a, m, j, i) & !mfdclkfbamji    (+1.000)
                           - x47(m, f, d, b, l, k) * t3b(f, c, a, m, j, i) & !mfdblkfcamji    (-1.000)
                           + x47(m, f, d, a, l, k) * t3b(f, c, b, m, j, i)      !mfdalkfcbmji    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x47)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
   allocate (h2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(8, shape(t4c), size(t4c), '12534678', t4c, h2)
   allocate (u46(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2
   i2 = k1 * k1 * k2 * k3 * k3
   i3 = k2 * k4 * k4
   call egemm(i1, i2, i3, d1, h2, u46)
   deallocate (d1)
   deallocate (h2)

   call sum_stripe(6, shape(x44), size(x44), '234561', &
                   0.500, x44, u46)
   deallocate (u46)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
   allocate (s72(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k1 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s72)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x20), size(x20), '3421', 1.000, &
                   x20, s72)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s72), size(s72), '4312', s72, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s153(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s153)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                   x6, s153)
   deallocate (s153)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s72), size(s72), '3412', s72, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s151(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s151)
   deallocate (d1)
   deallocate (b2)
   deallocate (s72)

   call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                   s151)
   deallocate (s151)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
   allocate (s73(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k2 * k4
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s73)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                   s73)
   deallocate (s73)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
   allocate (s74(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i2 = k2 * k4
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s74)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2413', 1.000, x2, &
                   s74)
   deallocate (s74)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
   allocate (s75(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k2 * k4
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s75)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x7), size(x7), '3421', 1.000, x7, &
                   s75)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s75), size(s75), '4312', s75, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s135(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s135)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x2), size(x2), '3124', -1.000, &
                   x2, s135)
   deallocate (s135)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s75), size(s75), '3412', s75, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s134(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s134)
   deallocate (d1)
   deallocate (b2)
   deallocate (s75)

   call sum_stripe(4, shape(x1), size(x1), '4123', 1.000, x1, &
                   s134)
   deallocate (s134)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
   allocate (s76(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k2 * k4
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, d2, s76)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(4, shape(x19), size(x19), '3421', 1.000, &
                   x19, s76)
   deallocate (s76)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
   allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
   allocate (q19(n0 + 1:n2, n0 + 1:n2))
   i1 = k2
   i2 = k2
   i3 = k2 * k4 * k4
   call egemm(i1, i2, i3, d1, d2, q19)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(2, shape(x10), size(x10), '21', -0.500, &
                   x10, q19)
   deallocate (q19)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intb, d1)
   allocate (d2(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(4, shape(t2c), size(t2c), '1432', t2c, d2)
   allocate (q20(n2 + 1:n3, n2 + 1:n3))
   i1 = k4
   i2 = k4
   i3 = k2 * k2 * k4
   call egemm(i1, i2, i3, d1, d2, q20)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(2, shape(x11), size(x11), '21', 0.500, &
                   x11, q20)
   deallocate (q20)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intr, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(6, shape(t3b), size(t3b), '265134', t3b, f2)
   allocate (s77(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
   i1 = k3
   i2 = k2 * k3 * k4
   i3 = k1 * k1 * k3
   call egemm(i1, i2, i3, d1, f2, s77)
   deallocate (d1)
   deallocate (f2)

   allocate (x51(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   x51 = 0.0d0
   call sum_stripe(4, shape(x51), size(x51), '2341', 1.000, &
                   x51, s77)
   deallocate (s77)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
   allocate (f2(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '123456', t3a, f2)
   allocate (u47(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k1 * k1 * k3
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, f2, u47)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,u47) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n1
                     sum = sum &
                           + (u47(c, k, j, i, m, n) * t3b(d, b, a, l, n, m) & !ckjimndbalnm    (+0.250)
                              - u47(b, k, j, i, m, n) * t3b(d, c, a, l, n, m) & !bkjimndcalnm    (-0.250)
                              + u47(a, k, j, i, m, n) * t3b(d, c, b, l, n, m)) / 4.0d0 !akjimndcblnm    (+0.250)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u47), size(u47), '561234', u47, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u76(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1 * k3 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u76)
   deallocate (f1)
   deallocate (b2)
   deallocate (u47)

   call sum_stripe(6, shape(x43), size(x43), '213456', &
                   -1.000, x43, u76)
   deallocate (u76)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x43,t2b) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        + (x43(m, c, a, k, j, i) * t2b(d, b, l, m) & !mcakjidblm      (+0.500)
                           - x43(m, c, b, k, j, i) * t2b(d, a, l, m) & !mcbkjidalm      (-0.500)
                           - x43(m, b, a, k, j, i) * t2b(d, c, l, m) & !mbakjidclm      (-0.500)
                           + x43(m, a, b, k, j, i) * t2b(d, c, l, m) & !mabkjidclm      (+0.500)
                           + x43(m, b, c, k, j, i) * t2b(d, a, l, m) & !mbckjidalm      (+0.500)
                           - x43(m, a, c, k, j, i) * t2b(d, b, l, m)) / 2.0d0   !mackjidblm      (-0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x43)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
   allocate (f2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '235146', t3b, f2)
   allocate (s78(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k2 * k4
   i3 = k1 * k3 * k3
   call egemm(i1, i2, i3, d1, f2, s78)
   deallocate (d1)
   deallocate (f2)

   allocate (x52(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   x52 = 0.0d0
   call sum_stripe(4, shape(x52), size(x52), '2341', 1.000, &
                   x52, s78)
   deallocate (s78)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '142356', t3a, f2)
   allocate (u48(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i2 = k1 * k1 * k3 * k3
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, f2, u48)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,u48) &
         !$omp private(a,b,c,d,f,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n1 + 1, n3
                  do n = n0 + 1, n1
                     sum = sum &
                           + u48(c, b, j, i, f, n) * t3b(d, f, a, l, n, k) & !cbjifndfalnk    (+1.000)
                           - u48(c, a, j, i, f, n) * t3b(d, f, b, l, n, k) & !cajifndfblnk    (-1.000)
                           + u48(b, a, j, i, f, n) * t3b(d, f, c, l, n, k) & !bajifndfclnk    (+1.000)
                           - u48(c, b, k, i, f, n) * t3b(d, f, a, l, n, j) & !cbkifndfalnj    (-1.000)
                           + u48(c, a, k, i, f, n) * t3b(d, f, b, l, n, j) & !cakifndfblnj    (+1.000)
                           - u48(b, a, k, i, f, n) * t3b(d, f, c, l, n, j) & !bakifndfclnj    (-1.000)
                           + u48(c, b, k, j, f, n) * t3b(d, f, a, l, n, i) & !cbkjfndfalni    (+1.000)
                           - u48(c, a, k, j, f, n) * t3b(d, f, b, l, n, i) & !cakjfndfblni    (-1.000)
                           + u48(b, a, k, j, f, n) * t3b(d, f, c, l, n, i)      !bakjfndfclni    (+1.000)

                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u48), size(u48), '561234', u48, f1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (u74(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k3 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, f1, b2, u74)
   deallocate (f1)
   deallocate (b2)
   deallocate (u48)

   call sum_stripe(6, shape(x41), size(x41), '612345', &
                   1.000, x41, u74)
   deallocate (u74)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
   allocate (f2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '124356', t3a, f2)
   allocate (s79(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k1 * k3
   i3 = k1 * k3 * k3
   call egemm(i1, i2, i3, d1, f2, s79)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(4, shape(x3), size(x3), '2341', -0.500, &
                   x3, s79)
   deallocate (s79)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
   allocate (f2(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '231456', t3b, f2)
   allocate (u49(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k1 * k2 * k4
   i3 = k3 * k3
   call egemm(i1, i2, i3, d1, f2, u49)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3a,u49) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n1
                     sum = sum &
                           + (u49(d, l, k, j, m, n) * t3a(c, b, a, n, m, i) & !dlkjmncbanmi    (+0.250)
                              - u49(d, l, k, i, m, n) * t3a(c, b, a, n, m, j) & !dlkimncbanmj    (-0.250)
                              + u49(d, l, j, i, m, n) * t3a(c, b, a, n, m, k)) / 4.0d0 !dljimncbanmk    (+0.250)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u49), size(u49), '561234', u49, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u68(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k4 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u68)
   deallocate (f1)
   deallocate (b2)
   deallocate (u49)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t2a,u68) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n1
                  sum = sum & !top two switched
                        + (u68(b, n, d, l, k, j) * t2a(c, a, n, i) & !bndlkjcani      (+0.500)
                           - u68(c, n, d, l, k, j) * t2a(b, a, n, i) & !cndlkjbani      (-0.500)
                           - u68(a, n, d, l, k, j) * t2a(c, b, n, i) & !andlkjcbni      (-0.500)
                           + u68(c, n, d, l, k, i) * t2a(b, a, n, j) & !cndlkibanj      (+0.500)
                           - u68(b, n, d, l, k, i) * t2a(c, a, n, j) & !bndlkicanj      (-0.500)
                           + u68(a, n, d, l, k, i) * t2a(c, b, n, j) & !andlkicbnj      (+0.500)
                           - u68(c, n, d, l, j, i) * t2a(b, a, n, k) & !cndljibank      (-0.500)
                           + u68(b, n, d, l, j, i) * t2a(c, a, n, k) & !bndljicank      (+0.500)
                           - u68(a, n, d, l, j, i) * t2a(c, b, n, k)) / 2.0d0   !andljicbnk      (-0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (u68)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3124', intr, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '154236', t3a, f2)
   allocate (s80(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   i1 = k3
   i2 = k1 * k3 * k3
   i3 = k1 * k1 * k3
   call egemm(i1, i2, i3, d1, f2, s80)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(4, shape(x4), size(x4), '2341', 0.500, x4, &
                   s80)
   deallocate (s80)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intm, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(6, shape(t3c), size(t3c), '164235', t3c, f2)
   allocate (s81(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
   i1 = k3
   i2 = k2 * k3 * k4
   i3 = k2 * k1 * k4
   call egemm(i1, i2, i3, d1, f2, s81)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(4, shape(x51), size(x51), '2341', 2.000, &
                   x51, s81)
   deallocate (s81)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x51,t3a) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  sum = sum & !top two switched
                        + (x51(e, d, b, l) * t3a(e, c, a, k, j, i) & !edblecakji      (+0.500)
                           - x51(e, d, a, l) * t3a(e, c, b, k, j, i) & !edalecbkji      (-0.500)
                           - x51(e, d, c, l) * t3a(e, b, a, k, j, i)) / 2.0d0   !edclebakji      (-0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x51)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
   allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3c), size(t3c), '134256', t3c, f2)
   allocate (s82(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k2 * k4
   i3 = k2 * k3 * k4
   call egemm(i1, i2, i3, d1, f2, s82)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(4, shape(x52), size(x52), '2341', 2.000, &
                   x52, s82)
   deallocate (s82)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x52,t3a) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum & !top two switched
                        + (x52(m, d, l, j) * t3a(c, b, a, m, k, i) & !mdljcbamki      (+0.500)
                           - x52(m, d, l, k) * t3a(c, b, a, m, j, i) & !mdlkcbamji      (-0.500)
                           - x52(m, d, l, i) * t3a(c, b, a, m, k, j)) / 2.0d0   !mdlicbamkj      (-0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x52)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3a), size(t3a), '142356', t3a, f2)
   allocate (u50(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k1 * k1 * k3 * k3
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, f2, u50)
   deallocate (d1)
   deallocate (f2)

   allocate (x53(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   x53 = 0.0d0
   call sum_stripe(6, shape(x53), size(x53), '345621', &
                   1.000, x53, u50)

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u50), size(u50), '561234', u50, f1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (u93(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, f1, b2, u93)
   deallocate (f1)
   deallocate (b2)
   deallocate (u50)

   call sum_stripe(6, shape(x44), size(x44), '412356', &
                   1.000, x44, u93)
   deallocate (u93)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intm, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '154236', t3b, f2)
   allocate (s83(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   i1 = k3
   i2 = k1 * k3 * k3
   i3 = k2 * k1 * k4
   call egemm(i1, i2, i3, d1, f2, s83)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,s83,t3b) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  sum = sum &
                        + s83(b, a, k, e) * t3b(d, e, c, l, j, i) & !bakedeclji      (+1.000)
                        - s83(c, a, k, e) * t3b(d, e, b, l, j, i) & !cakedeblji      (-1.000)
                        + s83(c, b, k, e) * t3b(d, e, a, l, j, i) & !cbkedealji      (+1.000)
                        - s83(b, a, j, e) * t3b(d, e, c, l, k, i) & !bajedeclki      (-1.000)
                        + s83(c, a, j, e) * t3b(d, e, b, l, k, i) & !cajedeblki      (+1.000)
                        - s83(c, b, j, e) * t3b(d, e, a, l, k, i) & !cbjedealki      (-1.000)
                        + s83(c, b, i, e) * t3b(d, e, a, l, k, j) & !cbiedealkj      (+1.000)
                        - s83(c, a, i, e) * t3b(d, e, b, l, k, j) & !caiedeblkj      (-1.000)
                        + s83(b, a, i, e) * t3b(d, e, c, l, k, j)          !baiedeclkj      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (s83)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intm, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '254136', t3b, f2)
   allocate (s84(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
   i1 = k4
   i2 = k1 * k3 * k4
   i3 = k2 * k1 * k3
   call egemm(i1, i2, i3, d1, f2, s84)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,s84) &
         !$omp private(a,b,c,d,f,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  sum = sum &
                        - s84(d, a, k, f) * t3b(f, c, b, l, j, i) & !dakffcblji      (-1.000)
                        + s84(d, b, k, f) * t3b(f, c, a, l, j, i) & !dbkffcalji      (+1.000)
                        - s84(d, c, k, f) * t3b(f, b, a, l, j, i) & !dckffbalji      (-1.000)
                        + s84(d, a, j, f) * t3b(f, c, b, l, k, i) & !dajffcblki      (+1.000)
                        - s84(d, b, j, f) * t3b(f, c, a, l, k, i) & !dbjffcalki      (-1.000)
                        + s84(d, c, j, f) * t3b(f, b, a, l, k, i) & !dcjffbalki      (+1.000)
                        - s84(d, c, i, f) * t3b(f, b, a, l, k, j) & !dciffbalkj      (-1.000)
                        + s84(d, b, i, f) * t3b(f, c, a, l, k, j) & !dbiffcalkj      (+1.000)
                        - s84(d, a, i, f) * t3b(f, c, b, l, k, j)          !daiffcblkj      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (s84)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
   allocate (f2(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '123456', t3b, f2)
   allocate (u51(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1
   i2 = k1 * k1 * k2 * k3
   i3 = k3 * k4
   call egemm(i1, i2, i3, d1, f2, u51)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x35), size(x35), '345621', &
                   1.000, x35, u51)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,x35) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n2
                     sum = sum &
                           - x35(n, m, c, l, j, i) * t3b(d, b, a, n, m, k) & !nmcljidbanmk    (-1.000)
                           + x35(n, m, b, l, j, i) * t3b(d, c, a, n, m, k) & !nmbljidcanmk    (+1.000)
                           - x35(n, m, a, l, j, i) * t3b(d, c, b, n, m, k) & !nmaljidcbnmk    (-1.000)
                           + x35(n, m, c, l, k, i) * t3b(d, b, a, n, m, j) & !nmclkidbanmj    (+1.000)
                           - x35(n, m, b, l, k, i) * t3b(d, c, a, n, m, j) & !nmblkidcanmj    (-1.000)
                           + x35(n, m, a, l, k, i) * t3b(d, c, b, n, m, j) & !nmalkidcbnmj    (+1.000)
                           - x35(n, m, c, l, k, j) * t3b(d, b, a, n, m, i) & !nmclkjdbanmi    (-1.000)
                           + x35(n, m, b, l, k, j) * t3b(d, c, a, n, m, i) & !nmblkjdcanmi    (+1.000)
                           - x35(n, m, a, l, k, j) * t3b(d, c, b, n, m, i)      !nmalkjdcbnmi    (-1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x35)

   allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u51), size(u51), '651234', u51, f1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (u92(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k3 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, f1, b2, u92)
   deallocate (f1)
   deallocate (b2)

   call sum_stripe(6, shape(x37), size(x37), '213456', &
                   1.000, x37, u92)
   deallocate (u92)

   allocate (f1(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u51), size(u51), '561234', u51, f1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (u82(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k2 * k3 * k2
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, f1, b2, u82)
   deallocate (f1)
   deallocate (b2)
   deallocate (u51)

   call sum_stripe(6, shape(x49), size(x49), '213456', &
                   -1.000, x49, u82)
   deallocate (u82)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t2b,x49) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        + x49(m, c, a, l, k, j) * t2b(d, b, m, i) & !mcalkjdbmi      (+1.000)
                        - x49(m, c, b, l, k, j) * t2b(d, a, m, i) & !mcblkjdami      (-1.000)
                        - x49(m, b, a, l, k, j) * t2b(d, c, m, i) & !mbalkjdcmi      (-1.000)
                        + x49(m, a, b, l, k, j) * t2b(d, c, m, i) & !mablkjdcmi      (+1.000)
                        + x49(m, b, c, l, k, j) * t2b(d, a, m, i) & !mbclkjdami      (+1.000)
                        - x49(m, a, c, l, k, j) * t2b(d, b, m, i) & !maclkjdbmi      (-1.000)
                        - x49(m, c, a, l, k, i) * t2b(d, b, m, j) & !mcalkidbmj      (-1.000)
                        + x49(m, c, b, l, k, i) * t2b(d, a, m, j) & !mcblkidamj      (+1.000)
                        + x49(m, b, a, l, k, i) * t2b(d, c, m, j) & !mbalkidcmj      (+1.000)
                        - x49(m, a, b, l, k, i) * t2b(d, c, m, j) & !mablkidcmj      (-1.000)
                        - x49(m, b, c, l, k, i) * t2b(d, a, m, j) & !mbclkidamj      (-1.000)
                        + x49(m, a, c, l, k, i) * t2b(d, b, m, j) & !maclkidbmj      (+1.000)
                        + x49(m, c, a, l, j, i) * t2b(d, b, m, k) & !mcaljidbmk      (+1.000)
                        - x49(m, c, b, l, j, i) * t2b(d, a, m, k) & !mcbljidamk      (-1.000)
                        - x49(m, b, a, l, j, i) * t2b(d, c, m, k) & !mbaljidcmk      (-1.000)
                        + x49(m, a, b, l, j, i) * t2b(d, c, m, k) & !mabljidcmk      (+1.000)
                        + x49(m, b, c, l, j, i) * t2b(d, a, m, k) & !mbcljidamk      (+1.000)
                        - x49(m, a, c, l, j, i) * t2b(d, b, m, k)          !macljidbmk      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x49)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
   allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '125346', t3b, f2)
   allocate (s85(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2
   i2 = k1 * k2 * k3
   i3 = k1 * k3 * k4
   call egemm(i1, i2, i3, d1, f2, s85)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,s85,t3b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n2
                  sum = sum &
                        - s85(a, l, k, n) * t3b(d, c, b, n, j, i) & !alkndcbnji      (-1.000)
                        + s85(b, l, k, n) * t3b(d, c, a, n, j, i) & !blkndcanji      (+1.000)
                        - s85(c, l, k, n) * t3b(d, b, a, n, j, i) & !clkndbanji      (-1.000)
                        + s85(a, l, j, n) * t3b(d, c, b, n, k, i) & !aljndcbnki      (+1.000)
                        - s85(b, l, j, n) * t3b(d, c, a, n, k, i) & !bljndcanki      (-1.000)
                        + s85(c, l, j, n) * t3b(d, b, a, n, k, i) & !cljndbanki      (+1.000)
                        - s85(c, l, i, n) * t3b(d, b, a, n, k, j) & !clindbankj      (-1.000)
                        + s85(b, l, i, n) * t3b(d, c, a, n, k, j) & !blindcankj      (+1.000)
                        - s85(a, l, i, n) * t3b(d, c, b, n, k, j)          !alindcbnkj      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (s85)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4132', intm, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '152346', t3b, f2)
   allocate (u52(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3
   i2 = k1 * k2 * k3 * k3
   i3 = k1 * k4
   call egemm(i1, i2, i3, d1, f2, u52)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,u52,t3b) &
         !$omp private(a,b,c,d,e,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do n = n0 + 1, n2
                     sum = sum &
                           - u52(b, a, l, k, e, n) * t3b(d, e, c, n, j, i) & !balkendecnji    (-1.000)
                           + u52(c, a, l, k, e, n) * t3b(d, e, b, n, j, i) & !calkendebnji    (+1.000)
                           - u52(c, b, l, k, e, n) * t3b(d, e, a, n, j, i) & !cblkendeanji    (-1.000)
                           + u52(b, a, l, j, e, n) * t3b(d, e, c, n, k, i) & !baljendecnki    (+1.000)
                           - u52(c, a, l, j, e, n) * t3b(d, e, b, n, k, i) & !caljendebnki    (-1.000)
                           + u52(c, b, l, j, e, n) * t3b(d, e, a, n, k, i) & !cbljendeanki    (+1.000)
                           - u52(c, b, l, i, e, n) * t3b(d, e, a, n, k, j) & !cbliendeankj    (-1.000)
                           + u52(c, a, l, i, e, n) * t3b(d, e, b, n, k, j) & !caliendebnkj    (+1.000)
                           - u52(b, a, l, i, e, n) * t3b(d, e, c, n, k, j)      !baliendecnkj    (-1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   allocate (f1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(u52), size(u52), '561234', u52, f1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (u78(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k3 * k3 * k2
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, f1, b2, u78)
   deallocate (f1)
   deallocate (b2)
   deallocate (u52)

   call sum_stripe(6, shape(x46), size(x46), '612345', &
                   1.000, x46, u78)
   deallocate (u78)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x46,t2b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n2
                  sum = sum &
                        + x46(n, b, a, l, k, i) * t2b(d, c, n, j) & !nbalkidcnj      (+1.000)
                        - x46(n, c, a, l, k, i) * t2b(d, b, n, j) & !ncalkidbnj      (-1.000)
                        + x46(n, c, b, l, k, i) * t2b(d, a, n, j) & !ncblkidanj      (+1.000)
                        - x46(n, b, a, l, j, i) * t2b(d, c, n, k) & !nbaljidcnk      (-1.000)
                        + x46(n, c, a, l, j, i) * t2b(d, b, n, k) & !ncaljidbnk      (+1.000)
                        - x46(n, c, b, l, j, i) * t2b(d, a, n, k) & !ncbljidank      (-1.000)
                        - x46(n, b, a, l, k, j) * t2b(d, c, n, i) & !nbalkjdcni      (-1.000)
                        + x46(n, c, a, l, k, j) * t2b(d, b, n, i) & !ncalkjdbni      (+1.000)
                        - x46(n, c, b, l, k, j) * t2b(d, a, n, i) & !ncblkjdani      (-1.000)
                        + x46(n, b, a, l, j, k) * t2b(d, c, n, i) & !nbaljkdcni      (+1.000)
                        - x46(n, c, a, l, j, k) * t2b(d, b, n, i) & !ncaljkdbni      (-1.000)
                        + x46(n, c, b, l, j, k) * t2b(d, a, n, i) & !ncbljkdani      (+1.000)
                        + x46(n, b, a, l, i, j) * t2b(d, c, n, k) & !nbalijdcnk      (+1.000)
                        - x46(n, c, a, l, i, j) * t2b(d, b, n, k) & !ncalijdbnk      (-1.000)
                        + x46(n, c, b, l, i, j) * t2b(d, a, n, k) & !ncblijdank      (+1.000)
                        - x46(n, b, a, l, i, k) * t2b(d, c, n, j) & !nbalikdcnj      (-1.000)
                        + x46(n, c, a, l, i, k) * t2b(d, b, n, j) & !ncalikdbnj      (+1.000)
                        - x46(n, c, b, l, i, k) * t2b(d, a, n, j)          !ncblikdanj      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x46)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '251346', t3b, f2)
   allocate (u53(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k1 * k2 * k3 * k4
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, f2, u53)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,u53) &
         !$omp private(a,b,c,d,f,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  do n = n0 + 1, n2
                     sum = sum &
                           + u53(d, a, l, k, f, n) * t3b(f, c, b, n, j, i) & !dalkfnfcbnji    (+1.000)
                           - u53(d, b, l, k, f, n) * t3b(f, c, a, n, j, i) & !dblkfnfcanji    (-1.000)
                           + u53(d, c, l, k, f, n) * t3b(f, b, a, n, j, i) & !dclkfnfbanji    (+1.000)
                           - u53(d, a, l, j, f, n) * t3b(f, c, b, n, k, i) & !daljfnfcbnki    (-1.000)
                           + u53(d, b, l, j, f, n) * t3b(f, c, a, n, k, i) & !dbljfnfcanki    (+1.000)
                           - u53(d, c, l, j, f, n) * t3b(f, b, a, n, k, i) & !dcljfnfbanki    (-1.000)
                           + u53(d, c, l, i, f, n) * t3b(f, b, a, n, k, j) & !dclifnfbankj    (+1.000)
                           - u53(d, b, l, i, f, n) * t3b(f, c, a, n, k, j) & !dblifnfcankj    (-1.000)
                           + u53(d, a, l, i, f, n) * t3b(f, c, b, n, k, j)      !dalifnfcbnkj    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (u53)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
   allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '124356', t3b, f2)
   allocate (s86(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1
   i2 = k1 * k1 * k3
   i3 = k2 * k3 * k4
   call egemm(i1, i2, i3, d1, f2, s86)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(4, shape(x3), size(x3), '2341', 1.000, x3, &
                   s86)
   deallocate (s86)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '4123', intb, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(6, shape(t3c), size(t3c), '154236', t3c, f2)
   allocate (s87(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
   i1 = k4
   i2 = k1 * k3 * k4
   i3 = k2 * k2 * k4
   call egemm(i1, i2, i3, d1, f2, s87)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,s87,t3b) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  sum = sum &
                        + (s87(d, b, k, e) * t3b(e, c, a, l, j, i) & !dbkeecalji      (+0.500)
                           - s87(d, a, k, e) * t3b(e, c, b, l, j, i) & !dakeecblji      (-0.500)
                           - s87(d, c, k, e) * t3b(e, b, a, l, j, i) & !dckeebalji      (-0.500)
                           + s87(d, a, j, e) * t3b(e, c, b, l, k, i) & !dajeecblki      (+0.500)
                           - s87(d, b, j, e) * t3b(e, c, a, l, k, i) & !dbjeecalki      (-0.500)
                           + s87(d, c, j, e) * t3b(e, b, a, l, k, i) & !dcjeebalki      (+0.500)
                           - s87(d, a, i, e) * t3b(e, c, b, l, k, j) & !daieecblkj      (-0.500)
                           + s87(d, b, i, e) * t3b(e, c, a, l, k, j) & !dbieecalkj      (+0.500)
                           - s87(d, c, i, e) * t3b(e, b, a, l, k, j)) / 2.0d0   !dcieebalkj      (-0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (s87)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
   allocate (f2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3c), size(t3c), '124356', t3c, f2)
   allocate (s88(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   i1 = k2
   i2 = k1 * k2 * k3
   i3 = k2 * k4 * k4
   call egemm(i1, i2, i3, d1, f2, s88)
   deallocate (d1)
   deallocate (f2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,s88) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        + (s88(b, l, k, m) * t3b(d, c, a, m, j, i) & !blkmdcamji      (+0.500)
                           - s88(a, l, k, m) * t3b(d, c, b, m, j, i) & !alkmdcbmji      (-0.500)
                           - s88(c, l, k, m) * t3b(d, b, a, m, j, i) & !clkmdbamji      (-0.500)
                           + s88(a, l, j, m) * t3b(d, c, b, m, k, i) & !aljmdcbmki      (+0.500)
                           - s88(b, l, j, m) * t3b(d, c, a, m, k, i) & !bljmdcamki      (-0.500)
                           + s88(c, l, j, m) * t3b(d, b, a, m, k, i) & !cljmdbamki      (+0.500)
                           - s88(a, l, i, m) * t3b(d, c, b, m, k, j) & !alimdcbmkj      (-0.500)
                           + s88(b, l, i, m) * t3b(d, c, a, m, k, j) & !blimdcamkj      (+0.500)
                           - s88(c, l, i, m) * t3b(d, b, a, m, k, j)) / 2.0d0   !climdbamkj      (-0.500)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (s88)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
   allocate (u54(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i2 = k1 * k1 * k3 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, f2, u54)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x53), size(x53), '345621', &
                   1.000, x53, u54)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x53,t3c) &
         !$omp private(a,b,c,d,f,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do f = n2 + 1, n3
                  do n = n0 + 1, n2
                     sum = sum &
                           + x53(n, f, c, b, j, i) * t3c(f, d, a, n, l, k) & !nfcbjifdanlk    (+1.000)
                           - x53(n, f, c, a, j, i) * t3c(f, d, b, n, l, k) & !nfcajifdbnlk    (-1.000)
                           + x53(n, f, b, a, j, i) * t3c(f, d, c, n, l, k) & !nfbajifdcnlk    (+1.000)
                           - x53(n, f, c, b, k, i) * t3c(f, d, a, n, l, j) & !nfcbkifdanlj    (-1.000)
                           + x53(n, f, c, a, k, i) * t3c(f, d, b, n, l, j) & !nfcakifdbnlj    (+1.000)
                           - x53(n, f, b, a, k, i) * t3c(f, d, c, n, l, j) & !nfbakifdcnlj    (-1.000)
                           + x53(n, f, c, b, k, j) * t3c(f, d, a, n, l, i) & !nfcbkjfdanli    (+1.000)
                           - x53(n, f, c, a, k, j) * t3c(f, d, b, n, l, i) & !nfcakjfdbnli    (-1.000)
                           + x53(n, f, b, a, k, j) * t3c(f, d, c, n, l, i)      !nfbakjfdcnli    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x53)

   allocate (f1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(u54), size(u54), '561234', u54, f1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (u95(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, f1, b2, u95)
   deallocate (f1)
   deallocate (b2)
   deallocate (u54)

   call sum_stripe(6, shape(x44), size(x44), '412356', &
                   1.000, x44, u95)
   deallocate (u95)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x44,t2b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n2
                  sum = sum &
                        - x44(n, b, a, l, k, j) * t2b(d, c, n, i) & !nbalkjdcni      (-1.000)
                        + x44(n, c, a, l, k, j) * t2b(d, b, n, i) & !ncalkjdbni      (+1.000)
                        - x44(n, c, b, l, k, j) * t2b(d, a, n, i) & !ncblkjdani      (-1.000)
                        + x44(n, b, a, l, k, i) * t2b(d, c, n, j) & !nbalkidcnj      (+1.000)
                        - x44(n, c, a, l, k, i) * t2b(d, b, n, j) & !ncalkidbnj      (-1.000)
                        + x44(n, c, b, l, k, i) * t2b(d, a, n, j) & !ncblkidanj      (+1.000)
                        - x44(n, b, a, l, j, i) * t2b(d, c, n, k) & !nbaljidcnk      (-1.000)
                        + x44(n, c, a, l, j, i) * t2b(d, b, n, k) & !ncaljidbnk      (+1.000)
                        - x44(n, c, b, l, j, i) * t2b(d, a, n, k)          !ncbljidank      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x44)

   allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s95(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s95)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s95), size(s95), '2431', s95, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '251346', t3b, f2)
   allocate (u66(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k2 * k3 * k4
   i3 = k1 * k3
   call egemm(i1, i2, i3, d1, f2, u66)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x26), size(x26), '234516', &
                   1.000, x26, u66)
   deallocate (u66)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s95), size(s95), '3214', s95, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q21(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q21)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(2, shape(x8), size(x8), '21', -1.000, x8, &
                   q21)
   deallocate (q21)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s95), size(s95), '3214', s95, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s97(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s97)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x13), size(x13), '3241', -1.000, &
                   x13, s97)

   call sum_shift(4, shape(intr), size(intr), shape(x13), &
                  size(x13), (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '3142', 1.000, intr, x13)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4b,x13) &
         !$omp private(a,b,c,d,e,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           + x13(m, e, c, i) * t4b(d, e, b, a, l, m, k, j) & !mecidebalmkj    (+1.000)
                           - x13(m, e, b, i) * t4b(d, e, c, a, l, m, k, j) & !mebidecalmkj    (-1.000)
                           + x13(m, e, a, i) * t4b(d, e, c, b, l, m, k, j) & !meaidecblmkj    (+1.000)
                           - x13(m, e, c, j) * t4b(d, e, b, a, l, m, k, i) & !mecjdebalmki    (-1.000)
                           + x13(m, e, b, j) * t4b(d, e, c, a, l, m, k, i) & !mebjdecalmki    (+1.000)
                           - x13(m, e, a, j) * t4b(d, e, c, b, l, m, k, i) & !meajdecblmki    (-1.000)
                           + x13(m, e, c, k) * t4b(d, e, b, a, l, m, j, i) & !meckdebalmji    (+1.000)
                           - x13(m, e, b, k) * t4b(d, e, c, a, l, m, j, i) & !mebkdecalmji    (-1.000)
                           + x13(m, e, a, k) * t4b(d, e, c, b, l, m, j, i)      !meakdecblmji    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x13)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s97), size(s97), '4213', s97, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s156(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s156)
   deallocate (d1)
   deallocate (b2)
   deallocate (s97)

   call sum_stripe(4, shape(x4), size(x4), '2134', 1.000, x4, &
                   s156)
   deallocate (s156)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s95), size(s95), '2314', s95, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s96(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s96)
   deallocate (d1)
   deallocate (b2)
   deallocate (s95)

   call sum_stripe(4, shape(x12), size(x12), '3241', 1.000, &
                   x12, s96)

   call sum_shift(4, shape(intr), size(intr), shape(x12), &
                  size(x12), (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intr, x12)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x12,t4b) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n1
                     sum = sum &
                           + (x12(n, m, j, i) * t4b(d, c, b, a, l, n, m, k) & !nmjidcbalnmk    (+0.500)
                              - x12(n, m, k, i) * t4b(d, c, b, a, l, n, m, j) & !nmkidcbalnmj    (-0.500)
                              + x12(n, m, k, j) * t4b(d, c, b, a, l, n, m, i)) / 2.0d0 !nmkjdcbalnmi    (+0.500)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x12)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s96), size(s96), '2413', s96, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u112(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u112)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x31), size(x31), '234156', &
                   1.000, x31, u112)
   deallocate (u112)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x31,t2b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n1
                  sum = sum &
                        + x31(n, c, b, k, j, i) * t2b(d, a, l, n) & !ncbkjidaln      (+1.000)
                        - x31(n, c, a, k, j, i) * t2b(d, b, l, n) & !ncakjidbln      (-1.000)
                        + x31(n, b, a, k, j, i) * t2b(d, c, l, n) & !nbakjidcln      (+1.000)
                        - x31(n, c, b, j, k, i) * t2b(d, a, l, n) & !ncbjkidaln      (-1.000)
                        + x31(n, c, a, j, k, i) * t2b(d, b, l, n) & !ncajkidbln      (+1.000)
                        - x31(n, b, a, j, k, i) * t2b(d, c, l, n) & !nbajkidcln      (-1.000)
                        + x31(n, c, b, i, k, j) * t2b(d, a, l, n) & !ncbikjdaln      (+1.000)
                        - x31(n, c, a, i, k, j) * t2b(d, b, l, n) & !ncaikjdbln      (-1.000)
                        + x31(n, b, a, i, k, j) * t2b(d, c, l, n)          !nbaikjdcln      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x31)

   allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s96), size(s96), '2413', s96, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s155(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1 * k1
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s155)
   deallocate (d1)
   deallocate (b2)
   deallocate (s96)

   call sum_stripe(4, shape(x3), size(x3), '2134', -1.000, &
                   x3, s155)
   deallocate (s155)

   call sum_shift(4, shape(intr), size(intr), shape(x3), &
                  size(x3), (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intr, x3)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x3,t3b) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        + x3(m, c, j, i) * t3b(d, b, a, l, m, k) & !mcjidbalmk      (+1.000)
                        - x3(m, b, j, i) * t3b(d, c, a, l, m, k) & !mbjidcalmk      (-1.000)
                        + x3(m, a, j, i) * t3b(d, c, b, l, m, k) & !majidcblmk      (+1.000)
                        - x3(m, c, k, i) * t3b(d, b, a, l, m, j) & !mckidbalmj      (-1.000)
                        + x3(m, b, k, i) * t3b(d, c, a, l, m, j) & !mbkidcalmj      (+1.000)
                        - x3(m, a, k, i) * t3b(d, c, b, l, m, j) & !makidcblmj      (-1.000)
                        + x3(m, c, k, j) * t3b(d, b, a, l, m, i) & !mckjdbalmi      (+1.000)
                        - x3(m, b, k, j) * t3b(d, c, a, l, m, i) & !mbkjdcalmi      (-1.000)
                        + x3(m, a, k, j) * t3b(d, c, b, l, m, i)           !makjdcblmi      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x3)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '1432', intr, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s98(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s98)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s98), size(s98), '3421', s98, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (q22(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k1 * k3
   call egemm1(i1, i3, d1, b2, q22)
   deallocate (d1)
   deallocate (b2)

   x9 = x9 + q22
   deallocate (q22)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s98), size(s98), '4231', s98, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s99(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s99)
   deallocate (d1)
   deallocate (b2)
   deallocate (s98)

   call sum_stripe(4, shape(x14), size(x14), '3124', 1.000, &
                   x14, s99)
   deallocate (s99)

   call sum_shift(4, shape(intr), size(intr), shape(x14), &
                  size(x14), (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '4321', 1.000, intr, x14)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4b,x14) &
         !$omp private(a,b,c,d,e,f,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do f = n1 + 1, n3
                     sum = sum &
                           + (x14(f, e, c, b) * t4b(d, f, e, a, l, k, j, i) & !fecbdfealkji    (+0.500)
                              - x14(f, e, c, a) * t4b(d, f, e, b, l, k, j, i) & !fecadfeblkji    (-0.500)
                              + x14(f, e, b, a) * t4b(d, f, e, c, l, k, j, i)) / 2.0d0 !febadfeclkji    (+0.500)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x14)

   allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (s100(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k4
   i2 = k1
   i3 = k3
   call egemm(i1, i2, i3, d1, b2, s100)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s100), size(s100), '2431', s100, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
   allocate (u79(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k1 * k3 * k3
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, f2, u79)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x41), size(x41), '234516', &
                   1.000, x41, u79)
   deallocate (u79)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x41,t2b) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        - x41(m, b, a, k, j, i) * t2b(d, c, l, m) & !mbakjidclm      (-1.000)
                        + x41(m, c, a, k, j, i) * t2b(d, b, l, m) & !mcakjidblm      (+1.000)
                        - x41(m, c, b, k, j, i) * t2b(d, a, l, m) & !mcbkjidalm      (-1.000)
                        + x41(m, b, a, k, i, j) * t2b(d, c, l, m) & !mbakijdclm      (+1.000)
                        - x41(m, c, a, k, i, j) * t2b(d, b, l, m) & !mcakijdblm      (-1.000)
                        + x41(m, c, b, k, i, j) * t2b(d, a, l, m) & !mcbkijdalm      (+1.000)
                        - x41(m, b, a, j, i, k) * t2b(d, c, l, m) & !mbajikdclm      (-1.000)
                        + x41(m, c, a, j, i, k) * t2b(d, b, l, m) & !mcajikdblm      (+1.000)
                        - x41(m, c, b, j, i, k) * t2b(d, a, l, m)          !mcbjikdalm      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x41)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s100), size(s100), '2431', s100, d1)
   allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
   allocate (u69(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i2 = k1 * k2 * k3 * k4
   i3 = k2 * k4
   call egemm(i1, i2, i3, d1, f2, u69)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x26), size(x26), '234516', &
                   1.000, x26, u69)
   deallocate (u69)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x26,t2a) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        + x26(m, d, a, l, k, i) * t2a(c, b, m, j) & !mdalkicbmj      (+1.000)
                        - x26(m, d, b, l, k, i) * t2a(c, a, m, j) & !mdblkicamj      (-1.000)
                        + x26(m, d, c, l, k, i) * t2a(b, a, m, j) & !mdclkibamj      (+1.000)
                        - x26(m, d, a, l, j, i) * t2a(c, b, m, k) & !mdaljicbmk      (-1.000)
                        + x26(m, d, b, l, j, i) * t2a(c, a, m, k) & !mdbljicamk      (+1.000)
                        - x26(m, d, c, l, j, i) * t2a(b, a, m, k) & !mdcljibamk      (-1.000)
                        - x26(m, d, a, l, k, j) * t2a(c, b, m, i) & !mdalkjcbmi      (-1.000)
                        + x26(m, d, b, l, k, j) * t2a(c, a, m, i) & !mdblkjcami      (+1.000)
                        - x26(m, d, c, l, k, j) * t2a(b, a, m, i) & !mdclkjbami      (-1.000)
                        + x26(m, d, a, l, j, k) * t2a(c, b, m, i) & !mdaljkcbmi      (+1.000)
                        - x26(m, d, b, l, j, k) * t2a(c, a, m, i) & !mdbljkcami      (-1.000)
                        + x26(m, d, c, l, j, k) * t2a(b, a, m, i) & !mdcljkbami      (+1.000)
                        + x26(m, d, a, l, i, j) * t2a(c, b, m, k) & !mdalijcbmk      (+1.000)
                        - x26(m, d, b, l, i, j) * t2a(c, a, m, k) & !mdblijcamk      (-1.000)
                        + x26(m, d, c, l, i, j) * t2a(b, a, m, k) & !mdclijbamk      (+1.000)
                        - x26(m, d, a, l, i, k) * t2a(c, b, m, j) & !mdalikcbmj      (-1.000)
                        + x26(m, d, b, l, i, k) * t2a(c, a, m, j) & !mdblikcamj      (+1.000)
                        - x26(m, d, c, l, i, k) * t2a(b, a, m, j)          !mdclikbamj      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x26)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(4, shape(s100), size(s100), '2431', s100, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (q23(n0 + 1:n1, n0 + 1:n1))
   i1 = k1 * k1
   i3 = k2 * k4
   call egemm1(i1, i3, d1, b2, q23)
   deallocate (d1)
   deallocate (b2)

   x8 = x8 + q23
   deallocate (q23)

   call sum_shift(2, shape(fockr), size(fockr), shape(x8), &
                  size(x8), (/n0, n0/), '12', 1.000, fockr, x8)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x8,t4b) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        - x8(m, i) * t4b(d, c, b, a, l, m, k, j) & !midcbalmkj      (-1.000)
                        + x8(m, j) * t4b(d, c, b, a, l, m, k, i) & !mjdcbalmki      (+1.000)
                        - x8(m, k) * t4b(d, c, b, a, l, m, j, i)           !mkdcbalmji      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x8)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s100), size(s100), '4321', s100, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s111(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
   i1 = k1 * k4 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s111)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x16), size(x16), '3124', -1.000, &
                   x16, s111)
   deallocate (s111)

   call sum_shift(4, shape(intm), size(intm), shape(x16), &
                  size(x16), (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '1342', 1.000, intm, x16)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4b,x16) &
         !$omp private(a,b,c,d,e,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           - x16(m, e, d, i) * t4b(e, c, b, a, l, m, k, j) & !mediecbalmkj    (-1.000)
                           + x16(m, e, d, j) * t4b(e, c, b, a, l, m, k, i) & !medjecbalmki    (+1.000)
                           - x16(m, e, d, k) * t4b(e, c, b, a, l, m, j, i)      !medkecbalmji    (-1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x16)

   allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(4, shape(s100), size(s100), '2314', s100, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s110(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k1
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s110)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x15), size(x15), '3241', 1.000, &
                   x15, s110)

   call sum_shift(4, shape(intm), size(intm), shape(x15), &
                  size(x15), (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intm, x15)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4b,x15) &
         !$omp private(a,b,c,d,n,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  do n = n0 + 1, n2
                     sum = sum &
                           + x15(n, m, l, i) * t4b(d, c, b, a, n, m, k, j) & !nmlidcbanmkj    (+1.000)
                           - x15(n, m, l, j) * t4b(d, c, b, a, n, m, k, i) & !nmljdcbanmki    (-1.000)
                           + x15(n, m, l, k) * t4b(d, c, b, a, n, m, j, i)      !nmlkdcbanmji    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x15)

   allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s110), size(s110), '2413', s110, d1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (u117(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k2
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, d1, d2, u117)
   deallocate (d1)
   deallocate (d2)

   call sum_stripe(6, shape(x25), size(x25), '235146', &
                   1.000, x25, u117)
   deallocate (u117)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x25,t2b) &
         !$omp private(a,b,c,d,n,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do n = n0 + 1, n2
                  sum = sum &
                        - x25(n, c, b, l, j, i) * t2b(d, a, n, k) & !ncbljidank      (-1.000)
                        + x25(n, c, a, l, j, i) * t2b(d, b, n, k) & !ncaljidbnk      (+1.000)
                        - x25(n, b, a, l, j, i) * t2b(d, c, n, k) & !nbaljidcnk      (-1.000)
                        + x25(n, c, b, l, k, i) * t2b(d, a, n, j) & !ncblkidanj      (+1.000)
                        - x25(n, c, a, l, k, i) * t2b(d, b, n, j) & !ncalkidbnj      (-1.000)
                        + x25(n, b, a, l, k, i) * t2b(d, c, n, j) & !nbalkidcnj      (+1.000)
                        + x25(n, c, b, l, i, j) * t2b(d, a, n, k) & !ncblijdank      (+1.000)
                        - x25(n, c, a, l, i, j) * t2b(d, b, n, k) & !ncalijdbnk      (-1.000)
                        + x25(n, b, a, l, i, j) * t2b(d, c, n, k) & !nbalijdcnk      (+1.000)
                        - x25(n, c, b, l, i, k) * t2b(d, a, n, j) & !ncblikdanj      (-1.000)
                        + x25(n, c, a, l, i, k) * t2b(d, b, n, j) & !ncalikdbnj      (+1.000)
                        - x25(n, b, a, l, i, k) * t2b(d, c, n, j) & !nbalikdcnj      (-1.000)
                        - x25(n, c, b, l, k, j) * t2b(d, a, n, i) & !ncblkjdani      (-1.000)
                        + x25(n, c, a, l, k, j) * t2b(d, b, n, i) & !ncalkjdbni      (+1.000)
                        - x25(n, b, a, l, k, j) * t2b(d, c, n, i) & !nbalkjdcni      (-1.000)
                        + x25(n, c, b, l, j, k) * t2b(d, a, n, i) & !ncbljkdani      (+1.000)
                        - x25(n, c, a, l, j, k) * t2b(d, b, n, i) & !ncaljkdbni      (-1.000)
                        + x25(n, b, a, l, j, k) * t2b(d, c, n, i)          !nbaljkdcni      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x25)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   call reorder_stripe(4, shape(s110), size(s110), '4213', s110, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s159(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
   i1 = k1 * k2 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s159)
   deallocate (d1)
   deallocate (b2)
   deallocate (s110)

   call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                   x1, s159)
   deallocate (s159)

   call sum_shift(4, shape(intm), size(intm), shape(x1), &
                  size(x1), (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1243', 1.000, intm, x1)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x1,t3a) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        - x1(m, d, l, i) * t3a(c, b, a, m, k, j) & !mdlicbamkj      (-1.000)
                        + x1(m, d, l, j) * t3a(c, b, a, m, k, i) & !mdljcbamki      (+1.000)
                        - x1(m, d, l, k) * t3a(c, b, a, m, j, i)           !mdlkcbamji      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x1)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(4, shape(s100), size(s100), '3214', s100, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s101(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s101)
   deallocate (d1)
   deallocate (b2)
   deallocate (s100)

   call sum_stripe(4, shape(x20), size(x20), '3241', -1.000, &
                   x20, s101)

   call sum_shift(4, shape(intm), size(intm), shape(x20), &
                  size(x20), (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '3142', 1.000, intm, x20)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4c,x20) &
         !$omp private(a,b,c,d,e,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           + x20(m, e, c, i) * t4c(e, d, b, a, m, l, k, j) & !meciedbamlkj    (+1.000)
                           - x20(m, e, b, i) * t4c(e, d, c, a, m, l, k, j) & !mebiedcamlkj    (-1.000)
                           + x20(m, e, a, i) * t4c(e, d, c, b, m, l, k, j) & !meaiedcbmlkj    (+1.000)
                           - x20(m, e, c, j) * t4c(e, d, b, a, m, l, k, i) & !mecjedbamlki    (-1.000)
                           + x20(m, e, b, j) * t4c(e, d, c, a, m, l, k, i) & !mebjedcamlki    (+1.000)
                           - x20(m, e, a, j) * t4c(e, d, c, b, m, l, k, i) & !meajedcbmlki    (-1.000)
                           + x20(m, e, c, k) * t4c(e, d, b, a, m, l, j, i) & !meckedbamlji    (+1.000)
                           - x20(m, e, b, k) * t4c(e, d, c, a, m, l, j, i) & !mebkedcamlji    (-1.000)
                           + x20(m, e, a, k) * t4c(e, d, c, b, m, l, j, i)      !meakedcbmlji    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x20)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s101), size(s101), '4213', s101, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s158(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s158)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x6), size(x6), '2134', 1.000, x6, &
                   s158)
   deallocate (s158)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(s101), size(s101), '2413', s101, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s157(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3 * k2
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s157)
   deallocate (d1)
   deallocate (b2)
   deallocate (s101)

   call sum_stripe(4, shape(x5), size(x5), '3124', -1.000, &
                   x5, s157)
   deallocate (s157)

   call sum_shift(4, shape(intm), size(intm), shape(x5), &
                  size(x5), (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '2143', 1.000, intm, x5)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3b,x5) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        - x5(m, c, l, i) * t3b(d, b, a, m, k, j) & !mclidbamkj      (-1.000)
                        + x5(m, b, l, i) * t3b(d, c, a, m, k, j) & !mblidcamkj      (+1.000)
                        - x5(m, a, l, i) * t3b(d, c, b, m, k, j) & !malidcbmkj      (-1.000)
                        + x5(m, c, l, j) * t3b(d, b, a, m, k, i) & !mcljdbamki      (+1.000)
                        - x5(m, b, l, j) * t3b(d, c, a, m, k, i) & !mbljdcamki      (-1.000)
                        + x5(m, a, l, j) * t3b(d, c, b, m, k, i) & !maljdcbmki      (+1.000)
                        - x5(m, c, l, k) * t3b(d, b, a, m, j, i) & !mclkdbamji      (-1.000)
                        + x5(m, b, l, k) * t3b(d, c, a, m, j, i) & !mblkdcamji      (+1.000)
                        - x5(m, a, l, k) * t3b(d, c, b, m, j, i)           !malkdcbmji      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x5)

   allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s112(n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1 * k3
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s112)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
   call reorder_stripe(4, shape(s112), size(s112), '2431', s112, d1)
   allocate (f2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
   call reorder_stripe(6, shape(t3b), size(t3b), '241356', t3b, f2)
   allocate (u91(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
   i1 = k2 * k1
   i2 = k1 * k1 * k3 * k4
   i3 = k2 * k3
   call egemm(i1, i2, i3, d1, f2, u91)
   deallocate (d1)
   deallocate (f2)

   call sum_stripe(6, shape(x37), size(x37), '235614', &
                   1.000, x37, u91)
   deallocate (u91)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x37,t2a) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n1
                  sum = sum &
                        - x37(m, d, a, l, k, j) * t2a(c, b, m, i) & !mdalkjcbmi      (-1.000)
                        + x37(m, d, b, l, k, j) * t2a(c, a, m, i) & !mdblkjcami      (+1.000)
                        - x37(m, d, c, l, k, j) * t2a(b, a, m, i) & !mdclkjbami      (-1.000)
                        + x37(m, d, a, l, k, i) * t2a(c, b, m, j) & !mdalkicbmj      (+1.000)
                        - x37(m, d, b, l, k, i) * t2a(c, a, m, j) & !mdblkicamj      (-1.000)
                        + x37(m, d, c, l, k, i) * t2a(b, a, m, j) & !mdclkibamj      (+1.000)
                        - x37(m, d, a, l, j, i) * t2a(c, b, m, k) & !mdaljicbmk      (-1.000)
                        + x37(m, d, b, l, j, i) * t2a(c, a, m, k) & !mdbljicamk      (+1.000)
                        - x37(m, d, c, l, j, i) * t2a(b, a, m, k)          !mdcljibamk      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x37)

   allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s112), size(s112), '4321', s112, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s138(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k1
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s138)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                   x7, s138)
   deallocate (s138)

   call sum_shift(4, shape(intm), size(intm), shape(x7), &
                  size(x7), (/n0 - n0, n1 - n0, n2 - n0, n0 - n0/), '1324', 1.000, intm, x7)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x7,t4a) &
         !$omp private(a,b,c,d,e,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do m = n0 + 1, n1
                     sum = sum &
                           + x7(m, e, d, l) * t4a(e, c, b, a, m, k, j, i)       !medlecbamkji    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x7)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s112), size(s112), '3214', s112, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (q24(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k3 * k1
   call egemm1(i1, i3, d1, b2, q24)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                   x10, q24)
   deallocate (q24)

   allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s112), size(s112), '3214', s112, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s113(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k3
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s113)
   deallocate (d1)
   deallocate (b2)
   deallocate (s112)

   call sum_stripe(4, shape(x17), size(x17), '3241', -1.000, &
                   x17, s113)

   call sum_shift(4, shape(intm), size(intm), shape(x17), &
                  size(x17), (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '3124', 1.000, intm, x17)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x17,t4b) &
         !$omp private(a,b,c,d,e,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           - x17(m, e, c, l) * t4b(d, e, b, a, m, k, j, i) & !mecldebamkji    (-1.000)
                           + x17(m, e, b, l) * t4b(d, e, c, a, m, k, j, i) & !mebldecamkji    (+1.000)
                           - x17(m, e, a, l) * t4b(d, e, c, b, m, k, j, i)      !mealdecbmkji    (-1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x17)

   allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(s113), size(s113), '4213', s113, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s160(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k3
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s160)
   deallocate (d1)
   deallocate (b2)
   deallocate (s113)

   call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                   s160)
   deallocate (s160)

   allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
   allocate (b2(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
   allocate (s114(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   i1 = k2 * k3 * k4
   i2 = k3
   i3 = k1
   call egemm(i1, i2, i3, d1, b2, s114)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s114), size(s114), '2431', s114, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (q27(n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3
   i3 = k2 * k4
   call egemm1(i1, i3, d1, b2, q27)
   deallocate (d1)
   deallocate (b2)

   x9 = x9 - q27
   deallocate (q27)

   call sum_shift(2, shape(fockr), size(fockr), shape(x9), &
                  size(x9), (/n1, n1/), '21', 1.000, fockr, x9)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4b,x9) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  sum = sum &
                        + x9(e, c) * t4b(d, e, b, a, l, k, j, i) & !ecdebalkji      (+1.000)
                        - x9(e, b) * t4b(d, e, c, a, l, k, j, i) & !ebdecalkji      (-1.000)
                        + x9(e, a) * t4b(d, e, c, b, l, k, j, i)           !eadecblkji      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x9)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   call reorder_stripe(4, shape(s114), size(s114), '4231', s114, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s115(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
   i1 = k3 * k3 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s115)
   deallocate (d1)
   deallocate (b2)
   deallocate (s114)

   call sum_stripe(4, shape(x18), size(x18), '3124', 1.000, &
                   x18, s115)
   deallocate (s115)

   call sum_shift(4, shape(intm), size(intm), shape(x18), &
                  size(x18), (/n2 - n0, n1 - n0, n2 - n0, n1 - n0/), '4321', 1.000, intm, x18)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x18,t4b) &
         !$omp private(a,b,c,d,e,f,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  do f = n2 + 1, n3
                     sum = sum &
                           + x18(f, e, d, c) * t4b(f, e, b, a, l, k, j, i) & !fedcfebalkji    (+1.000)
                           - x18(f, e, d, b) * t4b(f, e, c, a, l, k, j, i) & !fedbfecalkji    (-1.000)
                           + x18(f, e, d, a) * t4b(f, e, c, b, l, k, j, i)      !fedafecblkji    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x18)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (q25(n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i3 = k1 * k3
   call egemm1(i1, i3, d1, b2, q25)
   deallocate (d1)
   deallocate (b2)

   allocate (b1(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(q25), size(q25), '21', q25, b1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (s132(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
   i1 = k4
   i2 = k1 * k3 * k4
   i3 = k2
   call egemm(i1, i2, i3, b1, d2, s132)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                   x6, s132)
   deallocate (s132)

   allocate (b1(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(q25), size(q25), '21', q25, b1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q26(n2 + 1:n3, n2 + 1:n3))
   i1 = k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, b1, b2, q26)
   deallocate (b1)
   deallocate (b2)
   deallocate (q25)

   call sum_stripe(2, shape(x11), size(x11), '21', -1.000, &
                   x11, q26)
   deallocate (q26)

   allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                      (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
   allocate (b2(n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
   allocate (q28(n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i3 = k1 * k3
   call egemm1(i1, i3, d1, b2, q28)
   deallocate (d1)
   deallocate (b2)

   allocate (b1(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(q28), size(q28), '21', q28, b1)
   allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
   allocate (s125(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
   i1 = k3
   i2 = k2 * k3 * k4
   i3 = k1
   call egemm(i1, i2, i3, b1, d2, s125)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                   x2, s125)
   deallocate (s125)

   allocate (b1(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(q28), size(q28), '21', q28, b1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s121(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   i1 = k3
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, b1, d2, s121)
   deallocate (b1)
   deallocate (d2)
   deallocate (q28)

   call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                   x4, s121)
   deallocate (s121)

   allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (s139(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k2
   i3 = k4
   call egemm(i1, i2, i3, d1, b2, s139)
   deallocate (d1)
   deallocate (b2)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s139), size(s139), '3214', s139, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q29(n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2
   i3 = k4 * k2
   call egemm1(i1, i3, d1, b2, q29)
   deallocate (d1)
   deallocate (b2)

   call sum_stripe(2, shape(x10), size(x10), '21', -1.000, &
                   x10, q29)
   deallocate (q29)

   call sum_shift(2, shape(fockb), size(fockb), shape(x10), &
                  size(x10), (/n0, n0/), '12', 1.000, fockb, x10)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x10,t4b) &
         !$omp private(a,b,c,d,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do m = n0 + 1, n2
                  sum = sum &
                        - x10(m, l) * t4b(d, c, b, a, m, k, j, i)          !mldcbamkji      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x10)

   allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   call reorder_stripe(4, shape(s139), size(s139), '3214', s139, d1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (s140(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
   i1 = k2 * k2 * k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, d1, b2, s140)
   deallocate (d1)
   deallocate (b2)
   deallocate (s139)

   call sum_stripe(4, shape(x19), size(x19), '3241', -1.000, &
                   x19, s140)
   deallocate (s140)

   call sum_shift(4, shape(intb), size(intb), shape(x19), &
                  size(x19), (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '3142', 1.000, intb, x19)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x19,t4b) &
         !$omp private(a,b,c,d,e,m,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  do m = n0 + 1, n2
                     sum = sum &
                           + x19(m, e, d, l) * t4b(e, c, b, a, m, k, j, i)      !medlecbamkji    (+1.000)
                  end do
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x19)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
   call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3241', intb, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (q30(n2 + 1:n3, n0 + 1:n2))
   i1 = k2 * k4
   i3 = k2 * k4
   call egemm1(i1, i3, d1, b2, q30)
   deallocate (d1)
   deallocate (b2)

   allocate (b1(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(q30), size(q30), '21', q30, b1)
   allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
   allocate (s154(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
   i1 = k4
   i2 = k1 * k3 * k4
   i3 = k2
   call egemm(i1, i2, i3, b1, d2, s154)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x6), size(x6), '2341', 1.000, x6, &
                   s154)
   deallocate (s154)

   call sum_shift(4, shape(intm), size(intm), shape(x6), &
                  size(x6), (/n2 - n0, n2 - n0, n1 - n0, n0 - n0/), '3241', 1.000, intm, x6)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x6,t3b) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  sum = sum &
                        + x6(e, d, c, i) * t3b(e, b, a, l, k, j) & !edciebalkj      (+1.000)
                        - x6(e, d, b, i) * t3b(e, c, a, l, k, j) & !edbiecalkj      (-1.000)
                        + x6(e, d, a, i) * t3b(e, c, b, l, k, j) & !edaiecblkj      (+1.000)
                        - x6(e, d, c, j) * t3b(e, b, a, l, k, i) & !edcjebalki      (-1.000)
                        + x6(e, d, b, j) * t3b(e, c, a, l, k, i) & !edbjecalki      (+1.000)
                        - x6(e, d, a, j) * t3b(e, c, b, l, k, i) & !edajecblki      (-1.000)
                        + x6(e, d, c, k) * t3b(e, b, a, l, j, i) & !edckebalji      (+1.000)
                        - x6(e, d, b, k) * t3b(e, c, a, l, j, i) & !edbkecalji      (-1.000)
                        + x6(e, d, a, k) * t3b(e, c, b, l, j, i)           !edakecblji      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x6)

   allocate (b1(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(q30), size(q30), '21', q30, b1)
   allocate (b2(n0 + 1:n2, n2 + 1:n3))
   call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
   allocate (q31(n2 + 1:n3, n2 + 1:n3))
   i1 = k4
   i2 = k4
   i3 = k2
   call egemm(i1, i2, i3, b1, b2, q31)
   deallocate (b1)
   deallocate (b2)
   deallocate (q30)

   call sum_stripe(2, shape(x11), size(x11), '21', 1.000, &
                   x11, q31)
   deallocate (q31)

   call sum_shift(2, shape(fockb), size(fockb), shape(x11), &
                  size(x11), (/n2, n2/), '21', 1.000, fockb, x11)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t4b,x11) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n2 + 1, n3
                  sum = sum &
                        + x11(e, d) * t4b(e, c, b, a, l, k, j, i)          !edecbalkji      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x11)

   allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
   call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                      (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
   allocate (b2(n2 + 1:n3, n0 + 1:n2))
   call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
   allocate (q32(n1 + 1:n3, n0 + 1:n1))
   i1 = k1 * k3
   i3 = k2 * k4
   call egemm1(i1, i3, d1, b2, q32)
   deallocate (d1)
   deallocate (b2)

   allocate (b1(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(q32), size(q32), '21', q32, b1)
   allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
   call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
   allocate (s150(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
   i1 = k3
   i2 = k2 * k3 * k4
   i3 = k1
   call egemm(i1, i2, i3, b1, d2, s150)
   deallocate (b1)
   deallocate (d2)

   call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                   x2, s150)
   deallocate (s150)

   call sum_shift(4, shape(intm), size(intm), shape(x2), &
                  size(x2), (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3214', 1.000, intm, x2)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,t3a,x2) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  sum = sum &
                        + x2(e, d, c, l) * t3a(e, b, a, k, j, i) & !edclebakji      (+1.000)
                        - x2(e, d, b, l) * t3a(e, c, a, k, j, i) & !edblecakji      (-1.000)
                        + x2(e, d, a, l) * t3a(e, c, b, k, j, i)           !edalecbkji      (+1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x2)

   allocate (b1(n0 + 1:n1, n1 + 1:n3))
   call reorder_stripe(2, shape(q32), size(q32), '21', q32, b1)
   allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
   call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
   allocate (s144(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
   i1 = k3
   i2 = k1 * k3 * k3
   i3 = k1
   call egemm(i1, i2, i3, b1, d2, s144)
   deallocate (b1)
   deallocate (d2)
   deallocate (q32)

   call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                   x4, s144)
   deallocate (s144)

   call sum_shift(4, shape(intr), size(intr), shape(x4), &
                  size(x4), (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3241', 1.000, intr, x4)

   do i = n0 + 1, n1 - 2
      do j = i + 1, n1 - 1
      do k = j + 1, n1
      do l = n0 + 1, n2
         if (indocc(l, k, j, i) .eq. 1) cycle
         !$omp parallel default(none), &
         !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4b,x4,t3b) &
         !$omp private(a,b,c,d,e,sum)
         !$omp do
         do a = n1 + 1, n3 - 2
            do b = a + 1, n3 - 1
            do c = b + 1, n3
            do d = n2 + 1, n3
               if (indunocc(d, c, b, a) .eq. 1) cycle
               sum = 0.0d0
               do e = n1 + 1, n3
                  sum = sum &
                        - x4(e, c, b, i) * t3b(d, e, a, l, k, j) & !ecbidealkj      (-1.000)
                        + x4(e, c, a, i) * t3b(d, e, b, l, k, j) & !ecaideblkj      (+1.000)
                        - x4(e, b, a, i) * t3b(d, e, c, l, k, j) & !ebaideclkj      (-1.000)
                        + x4(e, c, b, j) * t3b(d, e, a, l, k, i) & !ecbjdealki      (+1.000)
                        - x4(e, c, a, j) * t3b(d, e, b, l, k, i) & !ecajdeblki      (-1.000)
                        + x4(e, b, a, j) * t3b(d, e, c, l, k, i) & !ebajdeclki      (+1.000)
                        - x4(e, c, b, k) * t3b(d, e, a, l, j, i) & !ecbkdealji      (-1.000)
                        + x4(e, c, a, k) * t3b(d, e, b, l, j, i) & !ecakdeblji      (+1.000)
                        - x4(e, b, a, k) * t3b(d, e, c, l, j, i)           !ebakdeclji      (-1.000)
               end do
               v4b(d, c, b, a, l, k, j, i) = v4b(d, c, b, a, l, k, j, i) + sum
            end do
            end do
            end do
         end do
         !$omp end do
         !$omp end parallel
      end do
      end do
      end do
   end do

   deallocate (x4)

   do i = n0 + 1, n1 - 2
   do j = i + 1, n1 - 1
   do k = j + 1, n1
   do l = n0 + 1, n2
      if (indocc(l, k, j, i) .eq. 1) cycle
      do a = n1 + 1, n3 - 2
      do b = a + 1, n3 - 1
      do c = b + 1, n3
      do d = n2 + 1, n3
         if (indunocc(d, c, b, a) .eq. 1) cycle
         coeleft = fockb(d, d) &
                   + fockr(c, c) &
                   + fockr(b, b) &
                   + fockr(a, a) &
                   - fockb(l, l) &
                   - fockr(k, k) &
                   - fockr(j, j) &
                   - fockr(i, i) &
                   + shift
         t4b(d, c, b, a, l, k, j, i) = &
            t4b(d, c, b, a, l, k, j, i) - v4b(d, c, b, a, l, k, j, i) / coeleft
         t4b(d, c, b, a, l, k, i, j) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, b, a, l, i, j, k) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, b, a, l, i, k, j) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, b, a, l, j, k, i) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, b, a, l, j, i, k) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, a, b, l, k, j, i) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, a, b, l, k, i, j) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, a, b, l, i, j, k) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, a, b, l, i, k, j) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, a, b, l, j, k, i) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, c, a, b, l, j, i, k) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, b, c, l, k, j, i) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, b, c, l, k, i, j) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, b, c, l, i, j, k) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, b, c, l, i, k, j) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, b, c, l, j, k, i) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, b, c, l, j, i, k) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, c, b, l, k, j, i) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, c, b, l, k, i, j) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, c, b, l, i, j, k) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, c, b, l, i, k, j) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, c, b, l, j, k, i) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, a, c, b, l, j, i, k) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, c, a, l, k, j, i) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, c, a, l, k, i, j) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, c, a, l, i, j, k) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, c, a, l, i, k, j) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, c, a, l, j, k, i) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, c, a, l, j, i, k) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, a, c, l, k, j, i) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, a, c, l, k, i, j) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, a, c, l, i, j, k) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, a, c, l, i, k, j) = t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, a, c, l, j, k, i) = -t4b(d, c, b, a, l, k, j, i)
         t4b(d, b, a, c, l, j, i, k) = t4b(d, c, b, a, l, k, j, i)
      end do
      end do
      end do
      end do
   end do
   end do
   end do
   end do
   rewind (tb)
   write (tb) t4b

end subroutine t4b_update

