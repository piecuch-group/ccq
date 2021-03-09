subroutine t4d_update(n0, n1, n2, n3, k1, k2, k3, k4, shift, &
                      fockr, fockb, intr, intb, intm, t1a, t1b, t2a, t2b, t2c, t3a, t3b, t3c, t3d, &
                      iactocca, iactoccb, iactunoa, iactunob, iactindq)

    integer :: a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p
    integer :: iactocca, iactoccb, iactunoa, iactunob, iactindq
    integer :: iocca, ioccb, iunoa, iunob
    integer, allocatable:: indocc(:, :, :, :)
    integer, allocatable:: indunocc(:, :, :, :)
    real(kind=8) :: shift, pp, coeleft, timt1, timt2
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
    real(kind=8), allocatable::t4c(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::t4d(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::t4e(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::v4d(:, :, :, :, :, :, :, :)

    real(kind=8), allocatable::b1(:, :)
    real(kind=8), allocatable::b2(:, :)
    real(kind=8), allocatable::d1(:, :, :, :)
    real(kind=8), allocatable::d2(:, :, :, :)
    real(kind=8), allocatable::f1(:, :, :, :, :, :)
    real(kind=8), allocatable::f2(:, :, :, :, :, :)
    real(kind=8), allocatable::h2(:, :, :, :, :, :, :, :)

    integer :: ta, tb, tc, td, te
    parameter(ta=29, tb=30, tc=31, td=32, te=33)

    real(kind=8), allocatable::s1(:, :, :, :)
    real(kind=8), allocatable::s2(:, :, :, :)
    real(kind=8), allocatable::s3(:, :, :, :)
    real(kind=8), allocatable::s4(:, :, :, :)
    real(kind=8), allocatable::s5(:, :, :, :)
    real(kind=8), allocatable::s6(:, :, :, :)
    real(kind=8), allocatable::q1(:, :)
    real(kind=8), allocatable::q2(:, :)
    real(kind=8), allocatable::s7(:, :, :, :)
    real(kind=8), allocatable::q3(:, :)
    real(kind=8), allocatable::s8(:, :, :, :)
    real(kind=8), allocatable::q4(:, :)
    real(kind=8), allocatable::s9(:, :, :, :)
    real(kind=8), allocatable::u55(:, :, :, :, :, :)
    real(kind=8), allocatable::s95(:, :, :, :)
    real(kind=8), allocatable::s89(:, :, :, :)
    real(kind=8), allocatable::s10(:, :, :, :)
    real(kind=8), allocatable::u56(:, :, :, :, :, :)
    real(kind=8), allocatable::s96(:, :, :, :)
    real(kind=8), allocatable::s90(:, :, :, :)
    real(kind=8), allocatable::s11(:, :, :, :)
    real(kind=8), allocatable::u57(:, :, :, :, :, :)
    real(kind=8), allocatable::s97(:, :, :, :)
    real(kind=8), allocatable::q5(:, :)
    real(kind=8), allocatable::s12(:, :, :, :)
    real(kind=8), allocatable::u58(:, :, :, :, :, :)
    real(kind=8), allocatable::s98(:, :, :, :)
    real(kind=8), allocatable::q6(:, :)
    real(kind=8), allocatable::s13(:, :, :, :)
    real(kind=8), allocatable::u63(:, :, :, :, :, :)
    real(kind=8), allocatable::s100(:, :, :, :)
    real(kind=8), allocatable::s99(:, :, :, :)
    real(kind=8), allocatable::s14(:, :, :, :)
    real(kind=8), allocatable::u64(:, :, :, :, :, :)
    real(kind=8), allocatable::s102(:, :, :, :)
    real(kind=8), allocatable::s101(:, :, :, :)
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
    real(kind=8), allocatable::u69(:, :, :, :, :, :)
    real(kind=8), allocatable::s26(:, :, :, :)
    real(kind=8), allocatable::u70(:, :, :, :, :, :)
    real(kind=8), allocatable::q7(:, :)
    real(kind=8), allocatable::q8(:, :)
    real(kind=8), allocatable::s27(:, :, :, :)
    real(kind=8), allocatable::u71(:, :, :, :, :, :)
    real(kind=8), allocatable::s123(:, :, :, :)
    real(kind=8), allocatable::s28(:, :, :, :)
    real(kind=8), allocatable::u72(:, :, :, :, :, :)
    real(kind=8), allocatable::q9(:, :)
    real(kind=8), allocatable::s29(:, :, :, :)
    real(kind=8), allocatable::u73(:, :, :, :, :, :)
    real(kind=8), allocatable::s124(:, :, :, :)
    real(kind=8), allocatable::s30(:, :, :, :)
    real(kind=8), allocatable::u74(:, :, :, :, :, :)
    real(kind=8), allocatable::q10(:, :)
    real(kind=8), allocatable::s31(:, :, :, :)
    real(kind=8), allocatable::u75(:, :, :, :, :, :)
    real(kind=8), allocatable::s125(:, :, :, :)
    real(kind=8), allocatable::s32(:, :, :, :)
    real(kind=8), allocatable::u77(:, :, :, :, :, :)
    real(kind=8), allocatable::u76(:, :, :, :, :, :)
    real(kind=8), allocatable::s126(:, :, :, :)
    real(kind=8), allocatable::q11(:, :)
    real(kind=8), allocatable::s33(:, :, :, :)
    real(kind=8), allocatable::u79(:, :, :, :, :, :)
    real(kind=8), allocatable::u78(:, :, :, :, :, :)
    real(kind=8), allocatable::s128(:, :, :, :)
    real(kind=8), allocatable::s127(:, :, :, :)
    real(kind=8), allocatable::s34(:, :, :, :)
    real(kind=8), allocatable::u80(:, :, :, :, :, :)
    real(kind=8), allocatable::q12(:, :)
    real(kind=8), allocatable::s35(:, :, :, :)
    real(kind=8), allocatable::s36(:, :, :, :)
    real(kind=8), allocatable::s37(:, :, :, :)
    real(kind=8), allocatable::q13(:, :)
    real(kind=8), allocatable::q14(:, :)
    real(kind=8), allocatable::s38(:, :, :, :)
    real(kind=8), allocatable::u97(:, :, :, :, :, :)
    real(kind=8), allocatable::s136(:, :, :, :)
    real(kind=8), allocatable::s135(:, :, :, :)
    real(kind=8), allocatable::u1(:, :, :, :, :, :)
    real(kind=8), allocatable::u2(:, :, :, :, :, :)
    real(kind=8), allocatable::u3(:, :, :, :, :, :)
    real(kind=8), allocatable::u4(:, :, :, :, :, :)
    real(kind=8), allocatable::u5(:, :, :, :, :, :)
    real(kind=8), allocatable::u6(:, :, :, :, :, :)
    real(kind=8), allocatable::u7(:, :, :, :, :, :)
    real(kind=8), allocatable::u8(:, :, :, :, :, :)
    real(kind=8), allocatable::u9(:, :, :, :, :, :)
    real(kind=8), allocatable::s39(:, :, :, :)
    real(kind=8), allocatable::s40(:, :, :, :)
    real(kind=8), allocatable::u10(:, :, :, :, :, :)
    real(kind=8), allocatable::s41(:, :, :, :)
    real(kind=8), allocatable::u11(:, :, :, :, :, :)
    real(kind=8), allocatable::s42(:, :, :, :)
    real(kind=8), allocatable::u12(:, :, :, :, :, :)
    real(kind=8), allocatable::u13(:, :, :, :, :, :)
    real(kind=8), allocatable::s43(:, :, :, :)
    real(kind=8), allocatable::u14(:, :, :, :, :, :)
    real(kind=8), allocatable::u15(:, :, :, :, :, :)
    real(kind=8), allocatable::s44(:, :, :, :)
    real(kind=8), allocatable::u16(:, :, :, :, :, :)
    real(kind=8), allocatable::s45(:, :, :, :)
    real(kind=8), allocatable::s46(:, :, :, :)
    real(kind=8), allocatable::u17(:, :, :, :, :, :)
    real(kind=8), allocatable::s47(:, :, :, :)
    real(kind=8), allocatable::s48(:, :, :, :)
    real(kind=8), allocatable::s49(:, :, :, :)
    real(kind=8), allocatable::s50(:, :, :, :)
    real(kind=8), allocatable::u18(:, :, :, :, :, :)
    real(kind=8), allocatable::s51(:, :, :, :)
    real(kind=8), allocatable::s52(:, :, :, :)
    real(kind=8), allocatable::u19(:, :, :, :, :, :)
    real(kind=8), allocatable::s53(:, :, :, :)
    real(kind=8), allocatable::s54(:, :, :, :)
    real(kind=8), allocatable::u20(:, :, :, :, :, :)
    real(kind=8), allocatable::u21(:, :, :, :, :, :)
    real(kind=8), allocatable::s55(:, :, :, :)
    real(kind=8), allocatable::u22(:, :, :, :, :, :)
    real(kind=8), allocatable::u23(:, :, :, :, :, :)
    real(kind=8), allocatable::s56(:, :, :, :)
    real(kind=8), allocatable::u24(:, :, :, :, :, :)
    real(kind=8), allocatable::u98(:, :, :, :, :, :)
    real(kind=8), allocatable::s110(:, :, :, :)
    real(kind=8), allocatable::u60(:, :, :, :, :, :)
    real(kind=8), allocatable::u25(:, :, :, :, :, :)
    real(kind=8), allocatable::s57(:, :, :, :)
    real(kind=8), allocatable::s111(:, :, :, :)
    real(kind=8), allocatable::s109(:, :, :, :)
    real(kind=8), allocatable::u26(:, :, :, :, :, :)
    real(kind=8), allocatable::u103(:, :, :, :, :, :)
    real(kind=8), allocatable::u100(:, :, :, :, :, :)
    real(kind=8), allocatable::u99(:, :, :, :, :, :)
    real(kind=8), allocatable::s139(:, :, :, :)
    real(kind=8), allocatable::u83(:, :, :, :, :, :)
    real(kind=8), allocatable::u81(:, :, :, :, :, :)
    real(kind=8), allocatable::u117(:, :, :, :, :, :)
    real(kind=8), allocatable::u62(:, :, :, :, :, :)
    real(kind=8), allocatable::u116(:, :, :, :, :, :)
    real(kind=8), allocatable::u115(:, :, :, :, :, :)
    real(kind=8), allocatable::u27(:, :, :, :, :, :)
    real(kind=8), allocatable::u101(:, :, :, :, :, :)
    real(kind=8), allocatable::u84(:, :, :, :, :, :)
    real(kind=8), allocatable::s116(:, :, :, :)
    real(kind=8), allocatable::s58(:, :, :, :)
    real(kind=8), allocatable::u102(:, :, :, :, :, :)
    real(kind=8), allocatable::s138(:, :, :, :)
    real(kind=8), allocatable::s115(:, :, :, :)
    real(kind=8), allocatable::u28(:, :, :, :, :, :)
    real(kind=8), allocatable::s59(:, :, :, :)
    real(kind=8), allocatable::u105(:, :, :, :, :, :)
    real(kind=8), allocatable::s137(:, :, :, :)
    real(kind=8), allocatable::s117(:, :, :, :)
    real(kind=8), allocatable::s60(:, :, :, :)
    real(kind=8), allocatable::q15(:, :)
    real(kind=8), allocatable::u29(:, :, :, :, :, :)
    real(kind=8), allocatable::s61(:, :, :, :)
    real(kind=8), allocatable::u104(:, :, :, :, :, :)
    real(kind=8), allocatable::s142(:, :, :, :)
    real(kind=8), allocatable::s140(:, :, :, :)
    real(kind=8), allocatable::s62(:, :, :, :)
    real(kind=8), allocatable::s143(:, :, :, :)
    real(kind=8), allocatable::s113(:, :, :, :)
    real(kind=8), allocatable::q16(:, :)
    real(kind=8), allocatable::s63(:, :, :, :)
    real(kind=8), allocatable::s141(:, :, :, :)
    real(kind=8), allocatable::s114(:, :, :, :)
    real(kind=8), allocatable::q17(:, :)
    real(kind=8), allocatable::q18(:, :)
    real(kind=8), allocatable::u30(:, :, :, :, :, :)
    real(kind=8), allocatable::u108(:, :, :, :, :, :)
    real(kind=8), allocatable::u107(:, :, :, :, :, :)
    real(kind=8), allocatable::u106(:, :, :, :, :, :)
    real(kind=8), allocatable::s146(:, :, :, :)
    real(kind=8), allocatable::u89(:, :, :, :, :, :)
    real(kind=8), allocatable::u121(:, :, :, :, :, :)
    real(kind=8), allocatable::u87(:, :, :, :, :, :)
    real(kind=8), allocatable::u119(:, :, :, :, :, :)
    real(kind=8), allocatable::u31(:, :, :, :, :, :)
    real(kind=8), allocatable::s64(:, :, :, :)
    real(kind=8), allocatable::u111(:, :, :, :, :, :)
    real(kind=8), allocatable::s147(:, :, :, :)
    real(kind=8), allocatable::s145(:, :, :, :)
    real(kind=8), allocatable::u32(:, :, :, :, :, :)
    real(kind=8), allocatable::u33(:, :, :, :, :, :)
    real(kind=8), allocatable::u34(:, :, :, :, :, :)
    real(kind=8), allocatable::s65(:, :, :, :)
    real(kind=8), allocatable::s66(:, :, :, :)
    real(kind=8), allocatable::u35(:, :, :, :, :, :)
    real(kind=8), allocatable::u36(:, :, :, :, :, :)
    real(kind=8), allocatable::s67(:, :, :, :)
    real(kind=8), allocatable::u37(:, :, :, :, :, :)
    real(kind=8), allocatable::u38(:, :, :, :, :, :)
    real(kind=8), allocatable::s68(:, :, :, :)
    real(kind=8), allocatable::u39(:, :, :, :, :, :)
    real(kind=8), allocatable::u40(:, :, :, :, :, :)
    real(kind=8), allocatable::s69(:, :, :, :)
    real(kind=8), allocatable::s70(:, :, :, :)
    real(kind=8), allocatable::u41(:, :, :, :, :, :)
    real(kind=8), allocatable::s71(:, :, :, :)
    real(kind=8), allocatable::u42(:, :, :, :, :, :)
    real(kind=8), allocatable::s72(:, :, :, :)
    real(kind=8), allocatable::u43(:, :, :, :, :, :)
    real(kind=8), allocatable::u92(:, :, :, :, :, :)
    real(kind=8), allocatable::s120(:, :, :, :)
    real(kind=8), allocatable::u67(:, :, :, :, :, :)
    real(kind=8), allocatable::u65(:, :, :, :, :, :)
    real(kind=8), allocatable::u114(:, :, :, :, :, :)
    real(kind=8), allocatable::u112(:, :, :, :, :, :)
    real(kind=8), allocatable::u44(:, :, :, :, :, :)
    real(kind=8), allocatable::s73(:, :, :, :)
    real(kind=8), allocatable::s122(:, :, :, :)
    real(kind=8), allocatable::s119(:, :, :, :)
    real(kind=8), allocatable::u45(:, :, :, :, :, :)
    real(kind=8), allocatable::u109(:, :, :, :, :, :)
    real(kind=8), allocatable::s152(:, :, :, :)
    real(kind=8), allocatable::u95(:, :, :, :, :, :)
    real(kind=8), allocatable::u93(:, :, :, :, :, :)
    real(kind=8), allocatable::u120(:, :, :, :, :, :)
    real(kind=8), allocatable::s74(:, :, :, :)
    real(kind=8), allocatable::u110(:, :, :, :, :, :)
    real(kind=8), allocatable::s151(:, :, :, :)
    real(kind=8), allocatable::u46(:, :, :, :, :, :)
    real(kind=8), allocatable::s75(:, :, :, :)
    real(kind=8), allocatable::s153(:, :, :, :)
    real(kind=8), allocatable::s149(:, :, :, :)
    real(kind=8), allocatable::q19(:, :)
    real(kind=8), allocatable::s76(:, :, :, :)
    real(kind=8), allocatable::s150(:, :, :, :)
    real(kind=8), allocatable::q20(:, :)
    real(kind=8), allocatable::u47(:, :, :, :, :, :)
    real(kind=8), allocatable::s77(:, :, :, :)
    real(kind=8), allocatable::s78(:, :, :, :)
    real(kind=8), allocatable::u48(:, :, :, :, :, :)
    real(kind=8), allocatable::u91(:, :, :, :, :, :)
    real(kind=8), allocatable::s79(:, :, :, :)
    real(kind=8), allocatable::s80(:, :, :, :)
    real(kind=8), allocatable::s81(:, :, :, :)
    real(kind=8), allocatable::s82(:, :, :, :)
    real(kind=8), allocatable::u49(:, :, :, :, :, :)
    real(kind=8), allocatable::u86(:, :, :, :, :, :)
    real(kind=8), allocatable::u68(:, :, :, :, :, :)
    real(kind=8), allocatable::s83(:, :, :, :)
    real(kind=8), allocatable::u50(:, :, :, :, :, :)
    real(kind=8), allocatable::u66(:, :, :, :, :, :)
    real(kind=8), allocatable::u51(:, :, :, :, :, :)
    real(kind=8), allocatable::s84(:, :, :, :)
    real(kind=8), allocatable::s85(:, :, :, :)
    real(kind=8), allocatable::u52(:, :, :, :, :, :)
    real(kind=8), allocatable::u96(:, :, :, :, :, :)
    real(kind=8), allocatable::s86(:, :, :, :)
    real(kind=8), allocatable::u53(:, :, :, :, :, :)
    real(kind=8), allocatable::u94(:, :, :, :, :, :)
    real(kind=8), allocatable::s87(:, :, :, :)
    real(kind=8), allocatable::u54(:, :, :, :, :, :)
    real(kind=8), allocatable::u90(:, :, :, :, :, :)
    real(kind=8), allocatable::s88(:, :, :, :)
    real(kind=8), allocatable::s91(:, :, :, :)
    real(kind=8), allocatable::u59(:, :, :, :, :, :)
    real(kind=8), allocatable::q21(:, :)
    real(kind=8), allocatable::s92(:, :, :, :)
    real(kind=8), allocatable::q22(:, :)
    real(kind=8), allocatable::s112(:, :, :, :)
    real(kind=8), allocatable::q23(:, :)
    real(kind=8), allocatable::s93(:, :, :, :)
    real(kind=8), allocatable::u61(:, :, :, :, :, :)
    real(kind=8), allocatable::q24(:, :)
    real(kind=8), allocatable::s104(:, :, :, :)
    real(kind=8), allocatable::s103(:, :, :, :)
    real(kind=8), allocatable::u113(:, :, :, :, :, :)
    real(kind=8), allocatable::s157(:, :, :, :)
    real(kind=8), allocatable::s94(:, :, :, :)
    real(kind=8), allocatable::s156(:, :, :, :)
    real(kind=8), allocatable::s155(:, :, :, :)
    real(kind=8), allocatable::s105(:, :, :, :)
    real(kind=8), allocatable::u85(:, :, :, :, :, :)
    real(kind=8), allocatable::u82(:, :, :, :, :, :)
    real(kind=8), allocatable::s129(:, :, :, :)
    real(kind=8), allocatable::q25(:, :)
    real(kind=8), allocatable::s106(:, :, :, :)
    real(kind=8), allocatable::s158(:, :, :, :)
    real(kind=8), allocatable::s107(:, :, :, :)
    real(kind=8), allocatable::q28(:, :)
    real(kind=8), allocatable::s108(:, :, :, :)
    real(kind=8), allocatable::q26(:, :)
    real(kind=8), allocatable::s121(:, :, :, :)
    real(kind=8), allocatable::s118(:, :, :, :)
    real(kind=8), allocatable::q27(:, :)
    real(kind=8), allocatable::s130(:, :, :, :)
    real(kind=8), allocatable::u88(:, :, :, :, :, :)
    real(kind=8), allocatable::q29(:, :)
    real(kind=8), allocatable::s132(:, :, :, :)
    real(kind=8), allocatable::s160(:, :, :, :)
    real(kind=8), allocatable::s131(:, :, :, :)
    real(kind=8), allocatable::u118(:, :, :, :, :, :)
    real(kind=8), allocatable::s159(:, :, :, :)
    real(kind=8), allocatable::s133(:, :, :, :)
    real(kind=8), allocatable::q30(:, :)
    real(kind=8), allocatable::s134(:, :, :, :)
    real(kind=8), allocatable::q31(:, :)
    real(kind=8), allocatable::s144(:, :, :, :)
    real(kind=8), allocatable::q32(:, :)
    real(kind=8), allocatable::s154(:, :, :, :)
    real(kind=8), allocatable::s148(:, :, :, :)
    real(kind=8), allocatable::x1(:, :, :, :)
    real(kind=8), allocatable::z1(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x2(:, :, :, :)
    real(kind=8), allocatable::z2(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x3(:, :, :, :)
    real(kind=8), allocatable::z3(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x4(:, :, :, :)
    real(kind=8), allocatable::z4(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x5(:, :, :, :)
    real(kind=8), allocatable::z5(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x6(:, :, :, :)
    real(kind=8), allocatable::z6(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x7(:, :, :, :)
    real(kind=8), allocatable::z7(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x8(:, :)
    real(kind=8), allocatable::z8(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x9(:, :)
    real(kind=8), allocatable::z9(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x10(:, :)
    real(kind=8), allocatable::z10(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x11(:, :)
    real(kind=8), allocatable::z11(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x12(:, :, :, :)
    real(kind=8), allocatable::z12(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x13(:, :, :, :)
    real(kind=8), allocatable::z13(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x14(:, :, :, :)
    real(kind=8), allocatable::z14(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x15(:, :, :, :)
    real(kind=8), allocatable::z15(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x16(:, :, :, :)
    real(kind=8), allocatable::z16(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x17(:, :, :, :)
    real(kind=8), allocatable::z17(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x18(:, :, :, :)
    real(kind=8), allocatable::z18(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x19(:, :, :, :)
    real(kind=8), allocatable::z19(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x20(:, :, :, :)
    real(kind=8), allocatable::z20(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x21(:, :, :, :, :, :)
    real(kind=8), allocatable::z205(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x22(:, :, :, :, :, :)
    real(kind=8), allocatable::z206(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x23(:, :, :, :, :, :)
    real(kind=8), allocatable::z208(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x24(:, :, :, :, :, :)
    real(kind=8), allocatable::z223(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x25(:, :, :, :)
    real(kind=8), allocatable::z46(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x26(:, :, :, :)
    real(kind=8), allocatable::z47(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x27(:, :, :, :, :, :)
    real(kind=8), allocatable::z247(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z61(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x28(:, :, :, :, :, :)
    real(kind=8), allocatable::z255(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z65(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x29(:, :, :, :, :, :)
    real(kind=8), allocatable::z81(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x30(:, :, :, :, :, :)
    real(kind=8), allocatable::z84(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x31(:, :, :, :, :, :)
    real(kind=8), allocatable::z86(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x32(:, :, :, :, :, :)
    real(kind=8), allocatable::z88(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x33(:, :, :, :, :, :)
    real(kind=8), allocatable::z89(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x34(:, :, :, :, :, :)
    real(kind=8), allocatable::z91(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x35(:, :, :, :, :, :)
    real(kind=8), allocatable::z94(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x36(:, :, :, :, :, :)
    real(kind=8), allocatable::z105(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x37(:, :, :, :, :, :)
    real(kind=8), allocatable::z108(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x38(:, :, :, :, :, :)
    real(kind=8), allocatable::z112(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z114(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z117(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z118(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x39(:, :, :, :, :, :)
    real(kind=8), allocatable::z120(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z304(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z303(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z302(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z131(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x40(:, :, :, :, :, :)
    real(kind=8), allocatable::z135(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x41(:, :, :, :, :, :)
    real(kind=8), allocatable::z136(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x42(:, :, :, :, :, :)
    real(kind=8), allocatable::z139(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x43(:, :, :, :, :, :)
    real(kind=8), allocatable::z140(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x44(:, :, :, :, :, :)
    real(kind=8), allocatable::z142(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x45(:, :, :, :, :, :)
    real(kind=8), allocatable::z143(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x46(:, :, :, :, :, :)
    real(kind=8), allocatable::z145(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x47(:, :, :, :, :, :)
    real(kind=8), allocatable::z146(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x48(:, :, :, :, :, :)
    real(kind=8), allocatable::z149(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z151(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z153(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z305(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z156(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z169(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z170(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z172(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z173(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z176(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z290(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z177(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z178(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z181(:, :, :, :, :, :, :, :)

    allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    allocate (t4e(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, &
                  n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    allocate (v4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))

    rewind (tc)
    rewind (td)
    rewind (te)
    read (tc) t4c
    read (td) t4d
    read (te) t4e

    allocate (indocc(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    allocate (indunocc(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    indocc = 0
    indunocc = 0
    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        iocca = 0
        ioccb = 0
        if (i .gt. (n1 - iactocca)) iocca = iocca + 1
        if (j .gt. (n2 - iactoccb)) ioccb = ioccb + 1
        if (k .gt. (n2 - iactoccb)) ioccb = ioccb + 1
        if (l .gt. (n2 - iactoccb)) ioccb = ioccb + 1
        if (iocca + ioccb .lt. iactindq) indocc(l, k, j, i) = 1
    end do; end do; end do; end do
    do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
        iunoa = 0
        iunob = 0
        if (a .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (b .lt. (n2 + iactunob + 1)) iunob = iunob + 1
        if (c .lt. (n2 + iactunob + 1)) iunob = iunob + 1
        if (d .lt. (n2 + iactunob + 1)) iunob = iunob + 1
        if (iunoa + iunob .lt. iactindq) indunocc(d, c, b, a) = 1
    end do; end do; end do; end do

    v4d = 0.0d0

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3124', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s1(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
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
    i1 = k2*k4*k3
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
    i1 = k1*k2*k2
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
    i1 = k1*k4*k4
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
    i1 = k2*k3*k2
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
    i1 = k3*k4*k4
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

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s7(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s7)
    deallocate (d1)
    deallocate (b2)

    allocate (x12(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x12 = 0.0d0
    call sum_stripe(4, shape(x12), size(x12), '3241', -1.000, &
                    x12, s7)
    deallocate (s7)

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

    call sum_stripe(2, shape(x8), size(x8), '21', -1.000, x8, &
                    q3)
    deallocate (q3)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s8(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s8)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x12), size(x12), '4231', 1.000, &
                    x12, s8)
    deallocate (s8)

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

    x9 = x9 - q4
    deallocate (q4)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3142', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s9(n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s9)
    deallocate (d1)
    deallocate (b2)

    allocate (x13(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    x13 = 0.0d0
    call sum_stripe(4, shape(x13), size(x13), '4231', 1.000, &
                    x13, s9)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s9), size(s9), '2431', s9, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u55(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u55)
    deallocate (d1)
    deallocate (d2)

    allocate (x21(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x21 = 0.0d0
    call sum_stripe(6, shape(x21), size(x21), '234156', &
                    1.000, x21, u55)
    deallocate (u55)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s9), size(s9), '4231', s9, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s95(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s95)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s95)
    deallocate (s95)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s9), size(s9), '2431', s9, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s89(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s89)
    deallocate (d1)
    deallocate (b2)
    deallocate (s9)

    call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                    x5, s89)
    deallocate (s89)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n0 - n0/), '3421', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s10(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s10)
    deallocate (d1)
    deallocate (b2)

    allocate (x14(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    x14 = 0.0d0
    call sum_stripe(4, shape(x14), size(x14), '4231', 1.000, &
                    x14, s10)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s10), size(s10), '2431', s10, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u56(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u56)
    deallocate (d1)
    deallocate (d2)

    allocate (x22(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x22 = 0.0d0
    call sum_stripe(6, shape(x22), size(x22), '345126', &
                    1.000, x22, u56)
    deallocate (u56)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s10), size(s10), '2431', s10, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s96(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s96)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                    s96)
    deallocate (s96)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s10), size(s10), '4231', s10, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s90(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s90)
    deallocate (d1)
    deallocate (b2)
    deallocate (s10)

    call sum_stripe(4, shape(x6), size(x6), '3124', -1.000, &
                    x6, s90)
    deallocate (s90)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s11(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s11)
    deallocate (d1)
    deallocate (b2)

    allocate (x15(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    x15 = 0.0d0
    call sum_stripe(4, shape(x15), size(x15), '3241', -1.000, &
                    x15, s11)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s11), size(s11), '2413', s11, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u57(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u57)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '246135', &
                    1.000, x21, u57)
    deallocate (u57)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s11), size(s11), '4213', s11, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s97(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s97)
    deallocate (d1)
    deallocate (b2)
    deallocate (s11)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s97)
    deallocate (s97)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q5(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k3*k1
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
    allocate (s12(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
    i1 = k4*k3*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s12)
    deallocate (d1)
    deallocate (b2)

    allocate (x16(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    x16 = 0.0d0
    call sum_stripe(4, shape(x16), size(x16), '4123', -1.000, &
                    x16, s12)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s12), size(s12), '3241', s12, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u58(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u58)
    deallocate (d1)
    deallocate (d2)

    allocate (x23(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x23 = 0.0d0
    call sum_stripe(6, shape(x23), size(x23), '356124', &
                    1.000, x23, u58)
    deallocate (u58)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s12), size(s12), '2341', s12, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s98(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s98)
    deallocate (d1)
    deallocate (b2)
    deallocate (s12)

    call sum_stripe(4, shape(x2), size(x2), '4123', -1.000, &
                    x2, s98)
    deallocate (s98)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q6(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k1*k3
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
    allocate (s13(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s13)
    deallocate (d1)
    deallocate (b2)

    allocate (x20(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x20 = 0.0d0
    call sum_stripe(4, shape(x20), size(x20), '3241', -1.000, &
                    x20, s13)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '2413', s13, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u63(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u63)
    deallocate (d1)
    deallocate (d2)

    allocate (x24(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x24 = 0.0d0
    call sum_stripe(6, shape(x24), size(x24), '245136', &
                    1.000, x24, u63)
    deallocate (u63)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '4213', s13, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s100(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s100)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '2134', 1.000, x6, &
                    s100)
    deallocate (s100)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '2413', s13, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s99(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s99)
    deallocate (d1)
    deallocate (b2)
    deallocate (s13)

    call sum_stripe(4, shape(x5), size(x5), '3124', -1.000, &
                    x5, s99)
    deallocate (s99)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s14(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s14)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x20), size(x20), '4231', 1.000, &
                    x20, s14)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s14), size(s14), '2431', s14, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u64(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u64)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x24), size(x24), '245136', &
                    -1.000, x24, u64)
    deallocate (u64)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s14), size(s14), '4231', s14, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s102(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s102)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                    x6, s102)
    deallocate (s102)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s14), size(s14), '2431', s14, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s101(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s101)
    deallocate (d1)
    deallocate (b2)
    deallocate (s14)

    call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                    s101)
    deallocate (s101)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s15(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s15)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s15)
    deallocate (s15)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4123', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s16(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s16)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                    s16)
    deallocate (s16)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2314', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s17(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s17)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                    x2, s17)
    deallocate (s17)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n1 - n0/), '4321', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s18(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s18)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                    s18)
    deallocate (s18)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s19(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s19)
    deallocate (d1)
    deallocate (b2)

    allocate (x3(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x3 = 0.0d0
    call sum_stripe(4, shape(x3), size(x3), '2134', -1.000, &
                    x3, s19)
    deallocate (s19)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4213', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s20(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s20)
    deallocate (d1)
    deallocate (b2)

    allocate (x25(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x25 = 0.0d0
    call sum_stripe(4, shape(x25), size(x25), '3124', 1.000, &
                    x25, s20)
    deallocate (s20)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2413', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s21(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s21)
    deallocate (d1)
    deallocate (b2)

    allocate (x26(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x26 = 0.0d0
    call sum_stripe(4, shape(x26), size(x26), '3124', 1.000, &
                    x26, s21)
    deallocate (s21)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n2 - n0/), '3421', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s22(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s22)
    deallocate (d1)
    deallocate (b2)

    allocate (x4(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x4 = 0.0d0
    call sum_stripe(4, shape(x4), size(x4), '4123', 1.000, x4, &
                    s22)
    deallocate (s22)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s23(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s23)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                    s23)
    deallocate (s23)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s24(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s24)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                    x6, s24)
    deallocate (s24)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s25(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s25)
    deallocate (d1)
    deallocate (b2)

    allocate (x7(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x7 = 0.0d0
    call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                    x7, s25)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s25), size(s25), '3214', s25, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u69(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u69)
    deallocate (d1)
    deallocate (d2)
    deallocate (s25)

    allocate (x27(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x27 = 0.0d0
    call sum_stripe(6, shape(x27), size(x27), '346125', &
                    1.000, x27, u69)
    deallocate (u69)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s26(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s26)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x7), size(x7), '4231', 1.000, x7, &
                    s26)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s26), size(s26), '2431', s26, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u70(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u70)
    deallocate (d1)
    deallocate (d2)
    deallocate (s26)

    call sum_stripe(6, shape(x27), size(x27), '346125', &
                    -1.000, x27, u70)
    deallocate (u70)

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
    allocate (s27(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s27)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x13), size(x13), '3241', 1.000, &
                    x13, s27)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s27), size(s27), '2413', s27, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u71(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u71)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '234156', &
                    1.000, x21, u71)
    deallocate (u71)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s27), size(s27), '4213', s27, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s123(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s123)
    deallocate (d1)
    deallocate (b2)
    deallocate (s27)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s123)
    deallocate (s123)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s28(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s28)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x14), size(x14), '3124', -1.000, &
                    x14, s28)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s28), size(s28), '3214', s28, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u72(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u72)
    deallocate (d1)
    deallocate (d2)
    deallocate (s28)

    call sum_stripe(6, shape(x22), size(x22), '345126', &
                    -1.000, x22, u72)
    deallocate (u72)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q9(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k2*k4
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
    allocate (s29(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s29)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x15), size(x15), '4231', 1.000, &
                    x15, s29)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s29), size(s29), '2431', s29, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u73(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u73)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '246135', &
                    -1.000, x21, u73)
    deallocate (u73)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s29), size(s29), '4231', s29, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s124(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s124)
    deallocate (d1)
    deallocate (b2)
    deallocate (s29)

    call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                    x2, s124)
    deallocate (s124)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s30(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s30)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x16), size(x16), '3124', -1.000, &
                    x16, s30)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s30), size(s30), '3214', s30, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u74(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u74)
    deallocate (d1)
    deallocate (d2)
    deallocate (s30)

    call sum_stripe(6, shape(x23), size(x23), '356124', &
                    1.000, x23, u74)
    deallocate (u74)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q10(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q10)
    deallocate (d1)
    deallocate (b2)

    x9 = x9 + q10
    deallocate (q10)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s31(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s31)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,s31) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (s31(k, m, j, n)*t4d(d, c, b, a, n, m, l, i) & !kmjndcbanmli    (+0.500)
                         - s31(l, m, j, n)*t4d(d, c, b, a, n, m, k, i) & !lmjndcbanmki    (-0.500)
                         - s31(j, m, k, n)*t4d(d, c, b, a, n, m, l, i) & !jmkndcbanmli    (-0.500)
                         + s31(j, m, l, n)*t4d(d, c, b, a, n, m, k, i) & !jmlndcbanmki    (+0.500)
                         + s31(l, m, k, n)*t4d(d, c, b, a, n, m, j, i) & !lmkndcbanmji    (+0.500)
                         - s31(k, m, l, n)*t4d(d, c, b, a, n, m, j, i))/2.0d0 !kmlndcbanmji    (-0.500)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s31), size(s31), '2413', s31, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u75(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, d1, d2, u75)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '236145', &
                    -1.000, x21, u75)
    deallocate (u75)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s31), size(s31), '2413', s31, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s125(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s125)
    deallocate (d1)
    deallocate (b2)
    deallocate (s31)

    call sum_stripe(4, shape(x25), size(x25), '2134', -1.000, &
                    x25, s125)
    deallocate (s125)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s32(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s32)
    deallocate (d1)
    deallocate (b2)

    allocate (x18(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x18 = 0.0d0
    call sum_stripe(4, shape(x18), size(x18), '3241', -1.000, &
                    x18, s32)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s32), size(s32), '2413', s32, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u77(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u77)
    deallocate (d1)
    deallocate (d2)

    allocate (x28(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    x28 = 0.0d0
    call sum_stripe(6, shape(x28), size(x28), '345126', &
                    1.000, x28, u77)
    deallocate (u77)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s32), size(s32), '2413', s32, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u76(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u76)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '346125', &
                    -1.000, x21, u76)
    deallocate (u76)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s32), size(s32), '4213', s32, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s126(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s126)
    deallocate (d1)
    deallocate (b2)
    deallocate (s32)

    call sum_stripe(4, shape(x4), size(x4), '2134', 1.000, x4, &
                    s126)
    deallocate (s126)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q11(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k4*k2
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
    allocate (s33(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s33)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x18), size(x18), '4231', 1.000, &
                    x18, s33)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s33), size(s33), '2431', s33, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u79(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u79)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '345126', &
                    -1.000, x28, u79)
    deallocate (u79)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s33), size(s33), '2431', s33, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u78(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u78)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '346125', &
                    1.000, x21, u78)
    deallocate (u78)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s33), size(s33), '4231', s33, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s128(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s128)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x26), size(x26), '3124', 1.000, &
                    x26, s128)
    deallocate (s128)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s33), size(s33), '2431', s33, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s127(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s127)
    deallocate (d1)
    deallocate (b2)
    deallocate (s33)

    call sum_stripe(4, shape(x3), size(x3), '3124', 1.000, x3, &
                    s127)
    deallocate (s127)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s34(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s34)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,s34) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do f = n2 + 1, n3
                sum = sum &
                      + (s34(c, f, e, d)*t4d(f, e, b, a, l, k, j, i) & !cfedfebalkji    (+0.500)
                         - s34(b, f, e, d)*t4d(f, e, c, a, l, k, j, i) & !bfedfecalkji    (-0.500)
                         - s34(d, f, e, c)*t4d(f, e, b, a, l, k, j, i) & !dfecfebalkji    (-0.500)
                         + s34(d, f, e, b)*t4d(f, e, c, a, l, k, j, i) & !dfebfecalkji    (+0.500)
                         + s34(b, f, e, c)*t4d(f, e, d, a, l, k, j, i) & !bfecfedalkji    (+0.500)
                         - s34(c, f, e, b)*t4d(f, e, d, a, l, k, j, i))/2.0d0 !cfebfedalkji    (-0.500)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s34), size(s34), '3241', s34, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u80(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u80)
    deallocate (d1)
    deallocate (d2)
    deallocate (s34)

    call sum_stripe(6, shape(x23), size(x23), '456123', &
                    -1.000, x23, u80)
    deallocate (u80)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q12(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q12)
    deallocate (d1)
    deallocate (b2)

    x11 = x11 - q12
    deallocate (q12)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s35(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s35)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x5), size(x5), '2431', 1.000, x5, &
                    s35)
    deallocate (s35)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s36(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s36)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '3412', 1.000, x6, &
                    s36)
    deallocate (s36)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s37(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s37)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x12), size(x12), '3421', 1.000, &
                    x12, s37)
    deallocate (s37)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (q13(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, d2, q13)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x8), size(x8), '21', -0.500, x8, &
                    q13)
    deallocate (q13)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3124', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '1432', t2a, d2)
    allocate (q14(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k1*k1*k3
    call egemm(i1, i2, i3, d1, d2, q14)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x9), size(x9), '21', 0.500, x9, &
                    q14)
    deallocate (q14)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s38(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s38)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x20), size(x20), '3421', 1.000, &
                    x20, s38)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s38), size(s38), '3412', s38, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u97(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u97)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x24), size(x24), '245136', &
                    -1.000, x24, u97)
    deallocate (u97)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s38), size(s38), '4312', s38, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s136(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s136)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                    x6, s136)
    deallocate (s136)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s38), size(s38), '3412', s38, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s135(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s135)
    deallocate (d1)
    deallocate (b2)
    deallocate (s38)

    call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                    s135)
    deallocate (s135)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3124', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u1)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '346125', &
                    -1.000, x27, u1)
    deallocate (u1)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intm, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u2)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '234156', &
                    1.000, x21, u2)
    deallocate (u2)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4123', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u3(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u3)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x22), size(x22), '345126', &
                    1.000, x22, u3)
    deallocate (u3)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u4(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u4)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '246135', &
                    -1.000, x21, u4)
    deallocate (u4)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n1 - n0/), '3421', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u5(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u5)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x23), size(x23), '356124', &
                    -1.000, x23, u5)
    deallocate (u5)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u6(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, d1, d2, u6)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x24), size(x24), '236145', &
                    1.000, x24, u6)
    deallocate (u6)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4213', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u7(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u7)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '346125', &
                    1.000, x21, u7)
    deallocate (u7)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4213', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u8(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u8)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '345126', &
                    -1.000, x28, u8)
    deallocate (u8)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n2 - n0/), '3421', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u9(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u9)
    deallocate (d1)
    deallocate (d2)

    allocate (x29(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x29 = 0.0d0
    call sum_stripe(6, shape(x29), size(x29), '456123', &
                    1.000, x29, u9)
    deallocate (u9)

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (s39(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, b1, d2, s39)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s39)
    deallocate (s39)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n0 - n0, n1 - n0/), '21', fockr, b1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s40(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s40)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s40)
    deallocate (s40)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '361245', t3c, f2)
    allocate (u10(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k2*k4*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u10)
    deallocate (d1)
    deallocate (f2)

    allocate (x30(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x30 = 0.0d0
    call sum_stripe(6, shape(x30), size(x30), '234516', &
                    1.000, x30, u10)
    deallocate (u10)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (s41(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s41)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2341', -1.000, &
                    x1, s41)
    deallocate (s41)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u11(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u11)
    deallocate (d1)
    deallocate (d2)

    allocate (x31(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x31 = 0.0d0
    call sum_stripe(6, shape(x31), size(x31), '356241', &
                    1.000, x31, u11)
    deallocate (u11)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (s42(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s42)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2413', -1.000, &
                    x2, s42)
    deallocate (s42)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u12(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u12)
    deallocate (d1)
    deallocate (d2)

    allocate (x32(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x32 = 0.0d0
    call sum_stripe(6, shape(x32), size(x32), '346251', &
                    1.000, x32, u12)
    deallocate (u12)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (f2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '631245', t3c, f2)
    allocate (u13(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k2*k4*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, f2, u13)
    deallocate (d1)
    deallocate (f2)

    allocate (x33(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    x33 = 0.0d0
    call sum_stripe(6, shape(x33), size(x33), '234561', &
                    1.000, x33, u13)
    deallocate (u13)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
    allocate (s43(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, d2, s43)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2413', -1.000, &
                    x1, s43)
    deallocate (s43)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n0 - n0/), '3421', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u14(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u14)
    deallocate (d1)
    deallocate (d2)

    allocate (x34(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x34 = 0.0d0
    call sum_stripe(6, shape(x34), size(x34), '456231', &
                    1.000, x34, u14)
    deallocate (u14)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u15(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k3
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u15)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x31), size(x31), '456231', &
                    1.000, x31, u15)
    deallocate (u15)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s44(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s44)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '3421', 1.000, x1, &
                    s44)
    deallocate (s44)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '341256', t3c, f2)
    allocate (u16(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2*k4*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, f2, u16)
    deallocate (d1)
    deallocate (f2)

    allocate (x35(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x35 = 0.0d0
    call sum_stripe(6, shape(x35), size(x35), '234615', &
                    1.000, x35, u16)
    deallocate (u16)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (s45(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s45)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x25), size(x25), '2341', -1.000, &
                    x25, s45)
    deallocate (s45)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1234', intm, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
    allocate (s46(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, d1, d2, s46)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2314', 1.000, x2, &
                    s46)
    deallocate (s46)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '132456', t3c, f2)
    allocate (u17(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k1*k2*k2*k4
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u17)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x22), size(x22), '345621', &
                    1.000, x22, u17)
    deallocate (u17)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (s47(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s47)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x26), size(x26), '3412', 1.000, &
                    x26, s47)
    deallocate (s47)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n2 - n0/), '4132', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '1423', t2b, d2)
    allocate (s48(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n2 + 1:n3))
    i1 = k4*k3
    i2 = k2*k3
    i3 = k1*k4
    call egemm(i1, i2, i3, d1, d2, s48)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '3412', -1.000, &
                    x2, s48)
    deallocate (s48)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s49(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, b1, d2, s49)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                    s49)
    deallocate (s49)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n0 - n0, n2 - n0/), '21', fockb, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s50(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s50)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                    x6, s50)
    deallocate (s50)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '142356', t3d, f2)
    allocate (u18(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k2*k4*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u18)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x30), size(x30), '234516', &
                    1.000, x30, u18)
    deallocate (u18)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s51(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k2*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, d1, d2, s51)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x5), size(x5), '2341', -1.000, &
                    x5, s51)
    deallocate (s51)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intm, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
    allocate (s52(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, d1, d2, s52)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '2314', 1.000, x6, &
                    s52)
    deallocate (s52)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u19(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u19)
    deallocate (d1)
    deallocate (d2)

    allocate (x36(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x36 = 0.0d0
    call sum_stripe(6, shape(x36), size(x36), '356241', &
                    1.000, x36, u19)
    deallocate (u19)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s53(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s53)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x5), size(x5), '3421', 1.000, x5, &
                    s53)
    deallocate (s53)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n1 - n0/), '3241', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
    allocate (s54(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4
    i2 = k1*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, d2, s54)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '2413', -1.000, &
                    x6, s54)
    deallocate (s54)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u20(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u20)
    deallocate (d1)
    deallocate (d2)

    allocate (x37(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x37 = 0.0d0
    call sum_stripe(6, shape(x37), size(x37), '346251', &
                    1.000, x37, u20)
    deallocate (u20)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '142356', t3d, f2)
    allocate (u21(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k2*k4*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u21)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x33), size(x33), '234516', &
                    1.000, x33, u21)
    deallocate (u21)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s55(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k3
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s55)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x5), size(x5), '2431', -1.000, &
                    x5, s55)
    deallocate (s55)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '3412', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u22(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u22)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x36), size(x36), '456231', &
                    1.000, x36, u22)
    deallocate (u22)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '4312', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '123456', t3d, f2)
    allocate (u23(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k2*k2*k2*k4
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, f2, u23)
    deallocate (d1)
    deallocate (f2)

    allocate (x38(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    x38 = 0.0d0
    call sum_stripe(6, shape(x38), size(x38), '345621', &
                    1.000, x38, u23)
    deallocate (u23)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s56(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s56)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '3412', -1.000, &
                    x6, s56)
    deallocate (s56)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u24(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u24)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,u24,t4c) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum & !top two switched
                      + (u24(c, j, i, f, m, n)*t4c(d, b, f, a, l, k, n, m) & !cjifmndbfalknm  (+0.500)
                         - u24(d, j, i, f, m, n)*t4c(c, b, f, a, l, k, n, m) & !djifmncbfalknm  (-0.500)
                         - u24(b, j, i, f, m, n)*t4c(d, c, f, a, l, k, n, m) & !bjifmndcfalknm  (-0.500)
                         + u24(d, k, i, f, m, n)*t4c(c, b, f, a, l, j, n, m) & !dkifmncbfaljnm  (+0.500)
                         - u24(c, k, i, f, m, n)*t4c(d, b, f, a, l, j, n, m) & !ckifmndbfaljnm  (-0.500)
                         + u24(b, k, i, f, m, n)*t4c(d, c, f, a, l, j, n, m) & !bkifmndcfaljnm  (+0.500)
                         - u24(d, l, i, f, m, n)*t4c(c, b, f, a, k, j, n, m) & !dlifmncbfakjnm  (-0.500)
                         + u24(c, l, i, f, m, n)*t4c(d, b, f, a, k, j, n, m) & !clifmndbfakjnm  (+0.500)
                         - u24(b, l, i, f, m, n)*t4c(d, c, f, a, k, j, n, m))/2.0d0  !blifmndcfakjnm  (-0.500)
            end do; end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u24), size(u24), '465123', u24, f1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (u98(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k1
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, f1, d2, u98)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '341256', &
                    -1.000, x27, u98)
    deallocate (u98)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u24), size(u24), '541236', u24, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s110(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k4
    i3 = k3*k1
    call egemm1(i1, i3, f1, b2, s110)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2341', -1.000, &
                    x1, s110)
    deallocate (s110)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u24), size(u24), '541236', u24, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u60(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u60)
    deallocate (f1)
    deallocate (b2)
    deallocate (u24)

    call sum_stripe(6, shape(x31), size(x31), '423561', &
                    -1.000, x31, u60)
    deallocate (u60)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
    allocate (h2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(8, shape(t4c), size(t4c), '34712568', t4c, h2)
    allocate (u25(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k2*k4*k4
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, h2, u25)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x30), size(x30), '234561', &
                    0.500, x30, u25)
    deallocate (u25)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (s57(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s57)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x7), size(x7), '3421', 1.000, x7, &
                    s57)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s57), size(s57), '4312', s57, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s111(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s111)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '3124', -1.000, &
                    x2, s111)
    deallocate (s111)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s57), size(s57), '3412', s57, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s109(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s109)
    deallocate (d1)
    deallocate (b2)
    deallocate (s57)

    call sum_stripe(4, shape(x1), size(x1), '4123', 1.000, x1, &
                    s109)
    deallocate (s109)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u26(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u26)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,u26) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      - u26(d, j, i, f, m, n)*t4d(f, c, b, a, n, l, k, m) & !djifmnfcbanlkm  (-1.000)
                      + u26(c, j, i, f, m, n)*t4d(f, d, b, a, n, l, k, m) & !cjifmnfdbanlkm  (+1.000)
                      - u26(b, j, i, f, m, n)*t4d(f, d, c, a, n, l, k, m) & !bjifmnfdcanlkm  (-1.000)
                      + u26(d, k, i, f, m, n)*t4d(f, c, b, a, n, l, j, m) & !dkifmnfcbanljm  (+1.000)
                      - u26(c, k, i, f, m, n)*t4d(f, d, b, a, n, l, j, m) & !ckifmnfdbanljm  (-1.000)
                      + u26(b, k, i, f, m, n)*t4d(f, d, c, a, n, l, j, m) & !bkifmnfdcanljm  (+1.000)
                      - u26(d, l, i, f, m, n)*t4d(f, c, b, a, n, k, j, m) & !dlifmnfcbankjm  (-1.000)
                      + u26(c, l, i, f, m, n)*t4d(f, d, b, a, n, k, j, m) & !clifmnfdbankjm  (+1.000)
                      - u26(b, l, i, f, m, n)*t4d(f, d, c, a, n, k, j, m)  !blifmnfdcankjm  (-1.000)
            end do; end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u26), size(u26), '564123', u26, f1)
    allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
    allocate (u103(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k4
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, f1, d2, u103)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x23), size(x23), '341256', &
                    1.000, x23, u103)
    deallocate (u103)

    allocate (f1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u26), size(u26), '541236', u26, f1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u100(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k4
    i2 = k2*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, f1, d2, u100)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '342561', &
                    -1.000, x21, u100)
    deallocate (u100)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u26), size(u26), '465123', u26, f1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (u99(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k1
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, f1, d2, u99)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '341256', &
                    -1.000, x27, u99)
    deallocate (u99)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u26), size(u26), '465123', u26, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s139(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k1
    i3 = k2*k4
    call egemm1(i1, i3, f1, b2, s139)
    deallocate (f1)
    deallocate (b2)

    x1 = x1 + s139
    deallocate (s139)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u26), size(u26), '654123', u26, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u83(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u83)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x34), size(x34), '312456', &
                    -1.000, x34, u83)
    deallocate (u83)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x34,t3c) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x34(m, f, d, c, j, i)*t3c(f, b, a, l, k, m) & !mfdcjifbalkm    (+1.000)
                      - x34(m, f, d, b, j, i)*t3c(f, c, a, l, k, m) & !mfdbjifcalkm    (-1.000)
                      - x34(m, f, c, d, j, i)*t3c(f, b, a, l, k, m) & !mfcdjifbalkm    (-1.000)
                      + x34(m, f, b, d, j, i)*t3c(f, c, a, l, k, m) & !mfbdjifcalkm    (+1.000)
                      + x34(m, f, c, b, j, i)*t3c(f, d, a, l, k, m) & !mfcbjifdalkm    (+1.000)
                      - x34(m, f, b, c, j, i)*t3c(f, d, a, l, k, m) & !mfbcjifdalkm    (-1.000)
                      - x34(m, f, d, c, k, i)*t3c(f, b, a, l, j, m) & !mfdckifbaljm    (-1.000)
                      + x34(m, f, d, b, k, i)*t3c(f, c, a, l, j, m) & !mfdbkifcaljm    (+1.000)
                      + x34(m, f, c, d, k, i)*t3c(f, b, a, l, j, m) & !mfcdkifbaljm    (+1.000)
                      - x34(m, f, b, d, k, i)*t3c(f, c, a, l, j, m) & !mfbdkifcaljm    (-1.000)
                      - x34(m, f, c, b, k, i)*t3c(f, d, a, l, j, m) & !mfcbkifdaljm    (-1.000)
                      + x34(m, f, b, c, k, i)*t3c(f, d, a, l, j, m) & !mfbckifdaljm    (+1.000)
                      + x34(m, f, d, c, l, i)*t3c(f, b, a, k, j, m) & !mfdclifbakjm    (+1.000)
                      - x34(m, f, d, b, l, i)*t3c(f, c, a, k, j, m) & !mfdblifcakjm    (-1.000)
                      - x34(m, f, c, d, l, i)*t3c(f, b, a, k, j, m) & !mfcdlifbakjm    (-1.000)
                      + x34(m, f, b, d, l, i)*t3c(f, c, a, k, j, m) & !mfbdlifcakjm    (+1.000)
                      + x34(m, f, c, b, l, i)*t3c(f, d, a, k, j, m) & !mfcblifdakjm    (+1.000)
                      - x34(m, f, b, c, l, i)*t3c(f, d, a, k, j, m)      !mfbclifdakjm    (-1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x34)

    allocate (f1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u26), size(u26), '451236', u26, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u81(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u81)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x32), size(x32), '523461', &
                    1.000, x32, u81)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3c,x32) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + x32(n, m, d, k, j, i)*t3c(c, b, a, n, l, m) & !nmdkjicbanlm    (+1.000)
                      - x32(n, m, c, k, j, i)*t3c(d, b, a, n, l, m) & !nmckjidbanlm    (-1.000)
                      + x32(n, m, b, k, j, i)*t3c(d, c, a, n, l, m) & !nmbkjidcanlm    (+1.000)
                      - x32(n, m, d, l, j, i)*t3c(c, b, a, n, k, m) & !nmdljicbankm    (-1.000)
                      + x32(n, m, c, l, j, i)*t3c(d, b, a, n, k, m) & !nmcljidbankm    (+1.000)
                      - x32(n, m, b, l, j, i)*t3c(d, c, a, n, k, m) & !nmbljidcankm    (-1.000)
                      - x32(n, m, d, j, k, i)*t3c(c, b, a, n, l, m) & !nmdjkicbanlm    (-1.000)
                      + x32(n, m, c, j, k, i)*t3c(d, b, a, n, l, m) & !nmcjkidbanlm    (+1.000)
                      - x32(n, m, b, j, k, i)*t3c(d, c, a, n, l, m) & !nmbjkidcanlm    (-1.000)
                      + x32(n, m, d, j, l, i)*t3c(c, b, a, n, k, m) & !nmdjlicbankm    (+1.000)
                      - x32(n, m, c, j, l, i)*t3c(d, b, a, n, k, m) & !nmcjlidbankm    (-1.000)
                      + x32(n, m, b, j, l, i)*t3c(d, c, a, n, k, m) & !nmbjlidcankm    (+1.000)
                      + x32(n, m, d, l, k, i)*t3c(c, b, a, n, j, m) & !nmdlkicbanjm    (+1.000)
                      - x32(n, m, c, l, k, i)*t3c(d, b, a, n, j, m) & !nmclkidbanjm    (-1.000)
                      + x32(n, m, b, l, k, i)*t3c(d, c, a, n, j, m) & !nmblkidcanjm    (+1.000)
                      - x32(n, m, d, k, l, i)*t3c(c, b, a, n, j, m) & !nmdklicbanjm    (-1.000)
                      + x32(n, m, c, k, l, i)*t3c(d, b, a, n, j, m) & !nmcklidbanjm    (+1.000)
                      - x32(n, m, b, k, l, i)*t3c(d, c, a, n, j, m)      !nmbklidcanjm    (-1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x32)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u81), size(u81), '623415', u81, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u117(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u117)
    deallocate (f1)
    deallocate (b2)
    deallocate (u81)

    call sum_stripe(6, shape(x27), size(x27), '213456', &
                    1.000, x27, u117)
    deallocate (u117)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x27,t2b) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x27(m, d, c, k, j, i)*t2b(b, a, l, m) & !mdckjibalm      (-1.000)
                      + x27(m, d, b, k, j, i)*t2b(c, a, l, m) & !mdbkjicalm      (+1.000)
                      + x27(m, c, d, k, j, i)*t2b(b, a, l, m) & !mcdkjibalm      (+1.000)
                      - x27(m, b, d, k, j, i)*t2b(c, a, l, m) & !mbdkjicalm      (-1.000)
                      - x27(m, c, b, k, j, i)*t2b(d, a, l, m) & !mcbkjidalm      (-1.000)
                      + x27(m, b, c, k, j, i)*t2b(d, a, l, m) & !mbckjidalm      (+1.000)
                      + x27(m, d, c, l, j, i)*t2b(b, a, k, m) & !mdcljibakm      (+1.000)
                      - x27(m, d, b, l, j, i)*t2b(c, a, k, m) & !mdbljicakm      (-1.000)
                      - x27(m, c, d, l, j, i)*t2b(b, a, k, m) & !mcdljibakm      (-1.000)
                      + x27(m, b, d, l, j, i)*t2b(c, a, k, m) & !mbdljicakm      (+1.000)
                      + x27(m, c, b, l, j, i)*t2b(d, a, k, m) & !mcbljidakm      (+1.000)
                      - x27(m, b, c, l, j, i)*t2b(d, a, k, m) & !mbcljidakm      (-1.000)
                      + x27(m, d, c, j, k, i)*t2b(b, a, l, m) & !mdcjkibalm      (+1.000)
                      - x27(m, d, b, j, k, i)*t2b(c, a, l, m) & !mdbjkicalm      (-1.000)
                      - x27(m, c, d, j, k, i)*t2b(b, a, l, m) & !mcdjkibalm      (-1.000)
                      + x27(m, b, d, j, k, i)*t2b(c, a, l, m) & !mbdjkicalm      (+1.000)
                      + x27(m, c, b, j, k, i)*t2b(d, a, l, m) & !mcbjkidalm      (+1.000)
                      - x27(m, b, c, j, k, i)*t2b(d, a, l, m) & !mbcjkidalm      (-1.000)
                      - x27(m, d, c, j, l, i)*t2b(b, a, k, m) & !mdcjlibakm      (-1.000)
                      + x27(m, d, b, j, l, i)*t2b(c, a, k, m) & !mdbjlicakm      (+1.000)
                      + x27(m, c, d, j, l, i)*t2b(b, a, k, m) & !mcdjlibakm      (+1.000)
                      - x27(m, b, d, j, l, i)*t2b(c, a, k, m) & !mbdjlicakm      (-1.000)
                      - x27(m, c, b, j, l, i)*t2b(d, a, k, m) & !mcbjlidakm      (-1.000)
                      + x27(m, b, c, j, l, i)*t2b(d, a, k, m) & !mbcjlidakm      (+1.000)
                      - x27(m, d, c, l, k, i)*t2b(b, a, j, m) & !mdclkibajm      (-1.000)
                      + x27(m, d, b, l, k, i)*t2b(c, a, j, m) & !mdblkicajm      (+1.000)
                      + x27(m, c, d, l, k, i)*t2b(b, a, j, m) & !mcdlkibajm      (+1.000)
                      - x27(m, b, d, l, k, i)*t2b(c, a, j, m) & !mbdlkicajm      (-1.000)
                      - x27(m, c, b, l, k, i)*t2b(d, a, j, m) & !mcblkidajm      (-1.000)
                      + x27(m, b, c, l, k, i)*t2b(d, a, j, m) & !mbclkidajm      (+1.000)
                      + x27(m, d, c, k, l, i)*t2b(b, a, j, m) & !mdcklibajm      (+1.000)
                      - x27(m, d, b, k, l, i)*t2b(c, a, j, m) & !mdbklicajm      (-1.000)
                      - x27(m, c, d, k, l, i)*t2b(b, a, j, m) & !mcdklibajm      (-1.000)
                      + x27(m, b, d, k, l, i)*t2b(c, a, j, m) & !mbdklicajm      (+1.000)
                      + x27(m, c, b, k, l, i)*t2b(d, a, j, m) & !mcbklidajm      (+1.000)
                      - x27(m, b, c, k, l, i)*t2b(d, a, j, m)          !mbcklidajm      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x27)

    allocate (f1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u26), size(u26), '541236', u26, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u62(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k4*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u62)
    deallocate (f1)
    deallocate (b2)
    deallocate (u26)

    call sum_stripe(6, shape(x36), size(x36), '423561', &
                    -1.000, x36, u62)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u62), size(u62), '623145', u62, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u116(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u116)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x23), size(x23), '213456', &
                    -1.000, x23, u116)
    deallocate (u116)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,x23) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      + x23(f, d, c, a, j, i)*t2c(f, b, l, k) & !fdcajifblk      (+1.000)
                      - x23(f, d, b, a, j, i)*t2c(f, c, l, k) & !fdbajifclk      (-1.000)
                      - x23(f, c, d, a, j, i)*t2c(f, b, l, k) & !fcdajifblk      (-1.000)
                      + x23(f, b, d, a, j, i)*t2c(f, c, l, k) & !fbdajifclk      (+1.000)
                      + x23(f, c, b, a, j, i)*t2c(f, d, l, k) & !fcbajifdlk      (+1.000)
                      - x23(f, b, c, a, j, i)*t2c(f, d, l, k) & !fbcajifdlk      (-1.000)
                      - x23(f, d, c, a, k, i)*t2c(f, b, l, j) & !fdcakifblj      (-1.000)
                      + x23(f, d, b, a, k, i)*t2c(f, c, l, j) & !fdbakifclj      (+1.000)
                      + x23(f, c, d, a, k, i)*t2c(f, b, l, j) & !fcdakifblj      (+1.000)
                      - x23(f, b, d, a, k, i)*t2c(f, c, l, j) & !fbdakifclj      (-1.000)
                      - x23(f, c, b, a, k, i)*t2c(f, d, l, j) & !fcbakifdlj      (-1.000)
                      + x23(f, b, c, a, k, i)*t2c(f, d, l, j) & !fbcakifdlj      (+1.000)
                      + x23(f, d, c, a, l, i)*t2c(f, b, k, j) & !fdcalifbkj      (+1.000)
                      - x23(f, d, b, a, l, i)*t2c(f, c, k, j) & !fdbalifckj      (-1.000)
                      - x23(f, c, d, a, l, i)*t2c(f, b, k, j) & !fcdalifbkj      (-1.000)
                      + x23(f, b, d, a, l, i)*t2c(f, c, k, j) & !fbdalifckj      (+1.000)
                      + x23(f, c, b, a, l, i)*t2c(f, d, k, j) & !fcbalifdkj      (+1.000)
                      - x23(f, b, c, a, l, i)*t2c(f, d, k, j)          !fbcalifdkj      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x23)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u62), size(u62), '263145', u62, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u115(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u115)
    deallocate (f1)
    deallocate (b2)
    deallocate (u62)

    call sum_stripe(6, shape(x21), size(x21), '512346', &
                    1.000, x21, u115)
    deallocate (u115)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u27(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k3
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u27)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,u27) &
        !$omp private(a,b,c,d,n,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      - u27(a, j, i, e, m, n)*t4d(d, c, b, e, n, l, k, m) & !ajiemndcbenlkm  (-1.000)
                      + u27(a, k, i, e, m, n)*t4d(d, c, b, e, n, l, j, m) & !akiemndcbenljm  (+1.000)
                      - u27(a, l, i, e, m, n)*t4d(d, c, b, e, n, k, j, m)  !aliemndcbenkjm  (-1.000)
            end do; end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u27), size(u27), '541236', u27, f1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (u101(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, f1, d2, u101)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '243561', &
                    -1.000, x21, u101)
    deallocate (u101)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u27), size(u27), '654123', u27, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u84(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u84)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x31), size(x31), '312456', &
                    -1.000, x31, u84)
    deallocate (u84)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u27), size(u27), '541236', u27, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s116(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3
    i3 = k3*k1
    call egemm1(i1, i3, f1, b2, s116)
    deallocate (f1)
    deallocate (b2)
    deallocate (u27)

    call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                    s116)
    deallocate (s116)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s58(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s58)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x13), size(x13), '3421', 1.000, &
                    x13, s58)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s58), size(s58), '3412', s58, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u102(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u102)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '234156', &
                    1.000, x21, u102)
    deallocate (u102)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s58), size(s58), '4312', s58, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s138(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s138)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s138)
    deallocate (s138)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s58), size(s58), '3412', s58, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s115(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s115)
    deallocate (d1)
    deallocate (b2)
    deallocate (s58)

    call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                    x5, s115)
    deallocate (s115)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(8, shape(t4d), size(t4d), '14823567', t4d, h2)
    allocate (u28(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k2*k4*k4
    i3 = k1*k3*k4
    call egemm(i1, i2, i3, d1, h2, u28)
    deallocate (d1)
    deallocate (h2)

    allocate (x39(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    x39 = 0.0d0
    call sum_stripe(6, shape(x39), size(x39), '234561', &
                    1.000, x39, u28)
    deallocate (u28)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3241', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
    allocate (s59(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k1*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, d2, s59)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x14), size(x14), '3421', -1.000, &
                    x14, s59)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s59), size(s59), '3412', s59, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u105(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u105)
    deallocate (d1)
    deallocate (d2)
!
    call sum_stripe(6, shape(x22), size(x22), '345126', &
                    -1.000, x22, u105)
    deallocate (u105)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s59), size(s59), '3412', s59, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s137(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s137)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '3124', -1.000, &
                    x1, s137)
    deallocate (s137)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s59), size(s59), '4312', s59, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s117(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s117)
    deallocate (d1)
    deallocate (b2)
    deallocate (s59)

    call sum_stripe(4, shape(x6), size(x6), '3124', 1.000, x6, &
                    s117)
    deallocate (s117)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s60(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s60)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x12), size(x12), '3421', 1.000, &
                    x12, s60)
    deallocate (s60)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (q15(n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, d2, q15)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x8), size(x8), '21', 1.000, x8, &
                    q15)
    deallocate (q15)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(8, shape(t4d), size(t4d), '14523678', t4d, h2)
    allocate (u29(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k2*k4*k4
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, h2, u29)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x30), size(x30), '234561', &
                    1.000, x30, u29)
    deallocate (u29)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (s61(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s61)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x18), size(x18), '3421', 1.000, &
                    x18, s61)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s61), size(s61), '3412', s61, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u104(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u104)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '345126', &
                    -1.000, x28, u104)
    deallocate (u104)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s61), size(s61), '4312', s61, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s142(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s142)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x26), size(x26), '2134', -1.000, &
                    x26, s142)
    deallocate (s142)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s61), size(s61), '3412', s61, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s140(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s140)
    deallocate (d1)
    deallocate (b2)
    deallocate (s61)

    call sum_stripe(4, shape(x25), size(x25), '4123', -1.000, &
                    x25, s140)
    deallocate (s140)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4132', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '1423', t2b, d2)
    allocate (s62(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k2*k3
    i3 = k1*k4
    call egemm(i1, i2, i3, d1, d2, s62)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x15), size(x15), '3421', -1.000, &
                    x15, s62)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s62), size(s62), '4312', s62, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s143(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s143)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s143)
    deallocate (s143)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s62), size(s62), '3412', s62, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s113(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s113)
    deallocate (d1)
    deallocate (b2)
    deallocate (s62)

    call sum_stripe(4, shape(x5), size(x5), '4123', -1.000, &
                    x5, s113)
    deallocate (s113)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '1243', t2b, d2)
    allocate (q16(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k1*k3*k4
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
    allocate (s63(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, d1, d2, s63)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x16), size(x16), '3412', 1.000, &
                    x16, s63)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s63), size(s63), '3412', s63, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s141(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s141)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                    s141)
    deallocate (s141)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s63), size(s63), '4312', s63, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s114(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s114)
    deallocate (d1)
    deallocate (b2)
    deallocate (s63)

    call sum_stripe(4, shape(x6), size(x6), '4123', 1.000, x6, &
                    s114)
    deallocate (s114)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '2431', t2b, d2)
    allocate (q17(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k2*k1*k3
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
    i3 = k2*k1*k4
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
    allocate (u30(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u30)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,u30,t4e) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (u30(a, k, i, f, m, n)*t4e(f, d, c, b, n, m, l, j) & !akifmnfdcbnmlj  (+0.500)
                         - u30(a, j, i, f, m, n)*t4e(f, d, c, b, n, m, l, k) & !ajifmnfdcbnmlk  (-0.500)
                         - u30(a, l, i, f, m, n)*t4e(f, d, c, b, n, m, k, j))/2.0d0 !alifmnfdcbnmkj  (-0.500)
            end do; end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u30), size(u30), '465123', u30, f1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (u108(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k2
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, f1, d2, u108)
    deallocate (f1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,u108) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - u108(c, l, m, a, j, i)*t2c(d, b, m, k) & !clmajidbmk      (-1.000)
                      - u108(d, k, m, a, j, i)*t2c(c, b, m, l) & !dkmajicbml      (-1.000)
                      + u108(d, l, m, a, j, i)*t2c(c, b, m, k) & !dlmajicbmk      (+1.000)
                      + u108(c, k, m, a, j, i)*t2c(d, b, m, l) & !ckmajidbml      (+1.000)
                      + u108(c, l, m, a, k, i)*t2c(d, b, m, j) & !clmakidbmj      (+1.000)
                      + u108(d, j, m, a, k, i)*t2c(c, b, m, l) & !djmakicbml      (+1.000)
                      - u108(d, l, m, a, k, i)*t2c(c, b, m, j) & !dlmakicbmj      (-1.000)
                      - u108(c, j, m, a, k, i)*t2c(d, b, m, l) & !cjmakidbml      (-1.000)
                      - u108(c, k, m, a, l, i)*t2c(d, b, m, j) & !ckmalidbmj      (-1.000)
                      - u108(d, j, m, a, l, i)*t2c(c, b, m, k) & !djmalicbmk      (-1.000)
                      + u108(d, k, m, a, l, i)*t2c(c, b, m, j) & !dkmalicbmj      (+1.000)
                      + u108(c, j, m, a, l, i)*t2c(d, b, m, k)         !cjmalidbmk      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u108)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u30), size(u30), '541236', u30, f1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (u107(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, f1, d2, u107)
    deallocate (f1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,u107) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - u107(b, l, a, j, i, n)*t2c(d, c, n, k) & !blajindcnk      (-1.000)
                      + u107(b, k, a, j, i, n)*t2c(d, c, n, l) & !bkajindcnl      (+1.000)
                      + u107(b, l, a, k, i, n)*t2c(d, c, n, j) & !blakindcnj      (+1.000)
                      - u107(b, j, a, k, i, n)*t2c(d, c, n, l) & !bjakindcnl      (-1.000)
                      - u107(b, k, a, l, i, n)*t2c(d, c, n, j) & !bkalindcnj      (-1.000)
                      + u107(b, j, a, l, i, n)*t2c(d, c, n, k)         !bjalindcnk      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u107)

    allocate (f1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u30), size(u30), '564123', u30, f1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '4312', t2c, d2)
    allocate (u106(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4
    i2 = k4*k4
    i3 = k2*k2
    call egemm(i1, i2, i3, f1, d2, u106)
    deallocate (f1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,u106) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      + (u106(d, b, f, a, j, i)*t2c(f, c, l, k) & !dbfajifclk      (+0.500)
                         - u106(c, b, f, a, j, i)*t2c(f, d, l, k) & !cbfajifdlk      (-0.500)
                         - u106(d, c, f, a, j, i)*t2c(f, b, l, k) & !dcfajifblk      (-0.500)
                         + u106(c, b, f, a, k, i)*t2c(f, d, l, j) & !cbfakifdlj      (+0.500)
                         - u106(d, b, f, a, k, i)*t2c(f, c, l, j) & !dbfakifclj      (-0.500)
                         + u106(d, c, f, a, k, i)*t2c(f, b, l, j) & !dcfakifblj      (+0.500)
                         - u106(c, b, f, a, l, i)*t2c(f, d, k, j) & !cbfalifdkj      (-0.500)
                         + u106(d, b, f, a, l, i)*t2c(f, c, k, j) & !dbfalifckj      (+0.500)
                         - u106(d, c, f, a, l, i)*t2c(f, b, k, j))/2.0d0  !dcfalifbkj      (-0.500)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u106)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u30), size(u30), '541236', u30, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s146(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3
    i3 = k4*k2
    call egemm1(i1, i3, f1, b2, s146)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x5), size(x5), '2341', -1.000, &
                    x5, s146)
    deallocate (s146)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u30), size(u30), '541236', u30, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u89(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u89)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x36), size(x36), '324561', &
                    -1.000, x36, u89)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u89), size(u89), '621345', u89, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u121(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u121)
    deallocate (f1)
    deallocate (b2)
    deallocate (u89)

    call sum_stripe(6, shape(x29), size(x29), '213456', &
                    1.000, x29, u121)
    deallocate (u121)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,x29) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      - x29(f, d, c, a, j, i)*t2c(f, b, l, k) & !fdcajifblk      (-1.000)
                      + x29(f, d, b, a, j, i)*t2c(f, c, l, k) & !fdbajifclk      (+1.000)
                      - x29(f, c, b, a, j, i)*t2c(f, d, l, k) & !fcbajifdlk      (-1.000)
                      + x29(f, d, c, a, k, i)*t2c(f, b, l, j) & !fdcakifblj      (+1.000)
                      - x29(f, d, b, a, k, i)*t2c(f, c, l, j) & !fdbakifclj      (-1.000)
                      + x29(f, c, b, a, k, i)*t2c(f, d, l, j) & !fcbakifdlj      (+1.000)
                      - x29(f, d, c, a, l, i)*t2c(f, b, k, j) & !fdcalifbkj      (-1.000)
                      + x29(f, d, b, a, l, i)*t2c(f, c, k, j) & !fdbalifckj      (+1.000)
                      - x29(f, c, b, a, l, i)*t2c(f, d, k, j)          !fcbalifdkj      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x29)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u30), size(u30), '451236', u30, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u87(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u87)
    deallocate (f1)
    deallocate (b2)
    deallocate (u30)

    call sum_stripe(6, shape(x37), size(x37), '523461', &
                    -1.000, x37, u87)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x37,t3d) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (x37(n, m, a, k, j, i)*t3d(d, c, b, n, m, l) & !nmakjidcbnml    (+0.500)
                         - x37(n, m, a, l, j, i)*t3d(d, c, b, n, m, k) & !nmaljidcbnmk    (-0.500)
                         - x37(n, m, a, j, k, i)*t3d(d, c, b, n, m, l) & !nmajkidcbnml    (-0.500)
                         + x37(n, m, a, j, l, i)*t3d(d, c, b, n, m, k) & !nmajlidcbnmk    (+0.500)
                         + x37(n, m, a, l, k, i)*t3d(d, c, b, n, m, j) & !nmalkidcbnmj    (+0.500)
                         - x37(n, m, a, k, l, i)*t3d(d, c, b, n, m, j))/2.0d0 !nmaklidcbnmj    (-0.500)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x37)

    allocate (f1(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u87), size(u87), '263415', u87, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u119(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k3*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u119)
    deallocate (f1)
    deallocate (b2)
    deallocate (u87)

    call sum_stripe(6, shape(x21), size(x21), '213456', &
                    1.000, x21, u119)
    deallocate (u119)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
    allocate (h2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(8, shape(t4e), size(t4e), '12534678', t4e, h2)
    allocate (u31(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k2*k4*k4
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, h2, u31)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x39), size(x39), '234561', &
                    0.500, x39, u31)
    deallocate (u31)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x39,t2b) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - x39(n, c, b, l, k, j)*t2b(d, a, n, i) & !ncblkjdani      (-1.000)
                      + x39(n, d, b, l, k, j)*t2b(c, a, n, i) & !ndblkjcani      (+1.000)
                      - x39(n, d, c, l, k, j)*t2b(b, a, n, i)          !ndclkjbani      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x39)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s64(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s64)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x20), size(x20), '3421', 1.000, &
                    x20, s64)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s64), size(s64), '3412', s64, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u111(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u111)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x24), size(x24), '245136', &
                    -1.000, x24, u111)
    deallocate (u111)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s64), size(s64), '4312', s64, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s147(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s147)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '2134', -1.000, &
                    x6, s147)
    deallocate (s147)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s64), size(s64), '3412', s64, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s145(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s145)
    deallocate (d1)
    deallocate (b2)
    deallocate (s64)

    call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                    s145)
    deallocate (s145)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u32(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u32)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x24), size(x24), '245136', &
                    -1.000, x24, u32)
    deallocate (u32)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (f2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '521346', t3b, f2)
    allocate (u33(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k2*k3*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, f2, u33)
    deallocate (d1)
    deallocate (f2)

    allocate (x40(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x40 = 0.0d0
    call sum_stripe(6, shape(x40), size(x40), '234651', &
                    1.000, x40, u33)
    deallocate (u33)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u34(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k3
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u34)
    deallocate (d1)
    deallocate (d2)

    allocate (x41(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x41 = 0.0d0
    call sum_stripe(6, shape(x41), size(x41), '456231', &
                    1.000, x41, u34)
    deallocate (u34)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s65(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, b1, d2, s65)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x3), size(x3), '2341', 1.000, x3, &
                    s65)
    deallocate (s65)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n0 - n0, n2 - n0/), '21', fockb, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s66(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s66)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                    x4, s66)
    deallocate (s66)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u35(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u35)
    deallocate (d1)
    deallocate (d2)

    allocate (x42(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x42 = 0.0d0
    call sum_stripe(6, shape(x42), size(x42), '345261', &
                    1.000, x42, u35)
    deallocate (u35)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
    allocate (f2(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '612345', t3c, f2)
    allocate (u36(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k2*k2*k3*k4
    i3 = k4*k1
    call egemm(i1, i2, i3, d1, f2, u36)
    deallocate (d1)
    deallocate (f2)

    allocate (x43(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x43 = 0.0d0
    call sum_stripe(6, shape(x43), size(x43), '234561', &
                    1.000, x43, u36)
    deallocate (u36)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s67(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s67)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                    s67)
    deallocate (s67)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u37(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u37)
    deallocate (d1)
    deallocate (d2)

    allocate (x44(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x44 = 0.0d0
    call sum_stripe(6, shape(x44), size(x44), '356241', &
                    1.000, x44, u37)
    deallocate (u37)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '132456', t3c, f2)
    allocate (u38(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k1*k2*k2*k4
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u38)
    deallocate (d1)
    deallocate (f2)

    allocate (x45(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x45 = 0.0d0
    call sum_stripe(6, shape(x45), size(x45), '245631', &
                    1.000, x45, u38)
    deallocate (u38)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s68(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s68)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2413', 1.000, x2, &
                    s68)
    deallocate (s68)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u39(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u39)
    deallocate (d1)
    deallocate (d2)

    allocate (x46(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    x46 = 0.0d0
    call sum_stripe(6, shape(x46), size(x46), '345261', &
                    1.000, x46, u39)
    deallocate (u39)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
    allocate (u40(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k2*k3*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u40)
    deallocate (d1)
    deallocate (f2)

    allocate (x47(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x47 = 0.0d0
    call sum_stripe(6, shape(x47), size(x47), '234615', &
                    1.000, x47, u40)
    deallocate (u40)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s69(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s69)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x25), size(x25), '2341', 1.000, &
                    x25, s69)
    deallocate (s69)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intb, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '4312', t2c, d2)
    allocate (s70(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k4*k4
    i3 = k2*k2
    call egemm(i1, i2, i3, d1, d2, s70)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x4), size(x4), '2314', 0.500, x4, &
                    s70)
    deallocate (s70)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '3412', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u41(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u41)
    deallocate (d1)
    deallocate (d2)

    allocate (x48(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x48 = 0.0d0
    call sum_stripe(6, shape(x48), size(x48), '456231', &
                    1.000, x48, u41)
    deallocate (u41)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '4312', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s71(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k2*k2
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, d2, s71)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x3), size(x3), '3421', 0.500, x3, &
                    s71)
    deallocate (s71)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '4312', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '123456', t3c, f2)
    allocate (u42(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k2*k2*k3
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, f2, u42)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,u42) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + (u42(a, l, k, i, d, m)*t2c(c, b, m, j) & !alkidmcbmj      (+0.500)
                         - u42(a, l, k, i, c, m)*t2c(d, b, m, j) & !alkicmdbmj      (-0.500)
                         + u42(a, l, k, i, b, m)*t2c(d, c, m, j) & !alkibmdcmj      (+0.500)
                         - u42(a, l, j, i, d, m)*t2c(c, b, m, k) & !aljidmcbmk      (-0.500)
                         + u42(a, l, j, i, c, m)*t2c(d, b, m, k) & !aljicmdbmk      (+0.500)
                         - u42(a, l, j, i, b, m)*t2c(d, c, m, k) & !aljibmdcmk      (-0.500)
                         + u42(a, k, j, i, d, m)*t2c(c, b, m, l) & !akjidmcbml      (+0.500)
                         - u42(a, k, j, i, c, m)*t2c(d, b, m, l) & !akjicmdbml      (-0.500)
                         + u42(a, k, j, i, b, m)*t2c(d, c, m, l))/2.0d0   !akjibmdcml      (+0.500)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u42)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s72(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s72)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x26), size(x26), '3412', -1.000, &
                    x26, s72)
    deallocate (s72)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u43(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k3
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u43)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4c,u43) &
        !$omp private(a,b,c,d,n,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + u43(d, k, j, e, m, n)*t4c(c, b, e, a, n, l, m, i) & !dkjemncbeanlmi  (+1.000)
                      - u43(c, k, j, e, m, n)*t4c(d, b, e, a, n, l, m, i) & !ckjemndbeanlmi  (-1.000)
                      + u43(b, k, j, e, m, n)*t4c(d, c, e, a, n, l, m, i) & !bkjemndceanlmi  (+1.000)
                      - u43(d, l, j, e, m, n)*t4c(c, b, e, a, n, k, m, i) & !dljemncbeankmi  (-1.000)
                      + u43(c, l, j, e, m, n)*t4c(d, b, e, a, n, k, m, i) & !cljemndbeankmi  (+1.000)
                      - u43(b, l, j, e, m, n)*t4c(d, c, e, a, n, k, m, i) & !bljemndceankmi  (-1.000)
                      + u43(d, l, k, e, m, n)*t4c(c, b, e, a, n, j, m, i) & !dlkemncbeanjmi  (+1.000)
                      - u43(c, l, k, e, m, n)*t4c(d, b, e, a, n, j, m, i) & !clkemndbeanjmi  (-1.000)
                      + u43(b, l, k, e, m, n)*t4c(d, c, e, a, n, j, m, i)  !blkemndceanjmi  (+1.000)
            end do; end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u43), size(u43), '654123', u43, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u92(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u92)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x41), size(x41), '312456', &
                    -1.000, x41, u92)
    deallocate (u92)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3b,x41) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x41(m, e, d, c, k, j)*t3b(b, e, a, l, m, i) & !medckjbealmi    (+1.000)
                      - x41(m, e, d, b, k, j)*t3b(c, e, a, l, m, i) & !medbkjcealmi    (-1.000)
                      - x41(m, e, c, d, k, j)*t3b(b, e, a, l, m, i) & !mecdkjbealmi    (-1.000)
                      + x41(m, e, b, d, k, j)*t3b(c, e, a, l, m, i) & !mebdkjcealmi    (+1.000)
                      + x41(m, e, c, b, k, j)*t3b(d, e, a, l, m, i) & !mecbkjdealmi    (+1.000)
                      - x41(m, e, b, c, k, j)*t3b(d, e, a, l, m, i) & !mebckjdealmi    (-1.000)
                      - x41(m, e, d, c, l, j)*t3b(b, e, a, k, m, i) & !medcljbeakmi    (-1.000)
                      + x41(m, e, d, b, l, j)*t3b(c, e, a, k, m, i) & !medbljceakmi    (+1.000)
                      + x41(m, e, c, d, l, j)*t3b(b, e, a, k, m, i) & !mecdljbeakmi    (+1.000)
                      - x41(m, e, b, d, l, j)*t3b(c, e, a, k, m, i) & !mebdljceakmi    (-1.000)
                      - x41(m, e, c, b, l, j)*t3b(d, e, a, k, m, i) & !mecbljdeakmi    (-1.000)
                      + x41(m, e, b, c, l, j)*t3b(d, e, a, k, m, i) & !mebcljdeakmi    (+1.000)
                      + x41(m, e, d, c, l, k)*t3b(b, e, a, j, m, i) & !medclkbeajmi    (+1.000)
                      - x41(m, e, d, b, l, k)*t3b(c, e, a, j, m, i) & !medblkceajmi    (-1.000)
                      - x41(m, e, c, d, l, k)*t3b(b, e, a, j, m, i) & !mecdlkbeajmi    (-1.000)
                      + x41(m, e, b, d, l, k)*t3b(c, e, a, j, m, i) & !mebdlkceajmi    (+1.000)
                      + x41(m, e, c, b, l, k)*t3b(d, e, a, j, m, i) & !mecblkdeajmi    (+1.000)
                      - x41(m, e, b, c, l, k)*t3b(d, e, a, j, m, i)      !mebclkdeajmi    (-1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x41)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u43), size(u43), '541236', u43, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s120(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4
    i3 = k3*k1
    call egemm1(i1, i3, f1, b2, s120)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x3), size(x3), '2341', 1.000, x3, &
                    s120)
    deallocate (s120)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u43), size(u43), '541236', u43, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u67(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u67)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x44), size(x44), '423561', &
                    -1.000, x44, u67)
    deallocate (u67)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3c,x44) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x44(m, e, d, a, k, j)*t3c(c, b, e, m, l, i) & !medakjcbemli    (+1.000)
                      - x44(m, e, c, a, k, j)*t3c(d, b, e, m, l, i) & !mecakjdbemli    (-1.000)
                      + x44(m, e, b, a, k, j)*t3c(d, c, e, m, l, i) & !mebakjdcemli    (+1.000)
                      - x44(m, e, d, a, l, j)*t3c(c, b, e, m, k, i) & !medaljcbemki    (-1.000)
                      + x44(m, e, c, a, l, j)*t3c(d, b, e, m, k, i) & !mecaljdbemki    (+1.000)
                      - x44(m, e, b, a, l, j)*t3c(d, c, e, m, k, i) & !mebaljdcemki    (-1.000)
                      + x44(m, e, d, a, l, k)*t3c(c, b, e, m, j, i) & !medalkcbemji    (+1.000)
                      - x44(m, e, c, a, l, k)*t3c(d, b, e, m, j, i) & !mecalkdbemji    (-1.000)
                      + x44(m, e, b, a, l, k)*t3c(d, c, e, m, j, i)      !mebalkdcemji    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x44)

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u43), size(u43), '451236', u43, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u65(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u65)
    deallocate (f1)
    deallocate (b2)
    deallocate (u43)

    call sum_stripe(6, shape(x42), size(x42), '623451', &
                    1.000, x42, u65)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u65), size(u65), '623451', u65, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u114(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u114)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x22), size(x22), '213456', &
                    -1.000, x22, u114)
    deallocate (u114)

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u65), size(u65), '263451', u65, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u112(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u112)
    deallocate (f1)
    deallocate (b2)
    deallocate (u65)

    call sum_stripe(6, shape(x24), size(x24), '312456', &
                    1.000, x24, u112)
    deallocate (u112)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(8, shape(t4c), size(t4c), '13724568', t4c, h2)
    allocate (u44(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k2*k3*k4
    i3 = k1*k3*k4
    call egemm(i1, i2, i3, d1, h2, u44)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x43), size(x43), '234561', &
                    -1.000, x43, u44)
    deallocate (u44)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s73(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s73)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x7), size(x7), '3421', 1.000, x7, &
                    s73)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s73), size(s73), '4312', s73, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s122(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s122)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '3124', -1.000, &
                    x2, s122)
    deallocate (s122)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s73), size(s73), '3412', s73, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s119(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s119)
    deallocate (d1)
    deallocate (b2)
    deallocate (s73)

    call sum_stripe(4, shape(x1), size(x1), '4123', 1.000, x1, &
                    s119)
    deallocate (s119)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u45(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u45)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,u45) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (u45(d, k, j, f, m, n)*t4d(f, c, b, a, n, m, l, i) & !dkjfmnfcbanmli  (+0.500)
                         - u45(c, k, j, f, m, n)*t4d(f, d, b, a, n, m, l, i) & !ckjfmnfdbanmli  (-0.500)
                         + u45(b, k, j, f, m, n)*t4d(f, d, c, a, n, m, l, i) & !bkjfmnfdcanmli  (+0.500)
                         - u45(d, l, j, f, m, n)*t4d(f, c, b, a, n, m, k, i) & !dljfmnfcbanmki  (-0.500)
                         + u45(c, l, j, f, m, n)*t4d(f, d, b, a, n, m, k, i) & !cljfmnfdbanmki  (+0.500)
                         - u45(b, l, j, f, m, n)*t4d(f, d, c, a, n, m, k, i) & !bljfmnfdcanmki  (-0.500)
                         + u45(d, l, k, f, m, n)*t4d(f, c, b, a, n, m, j, i) & !dlkfmnfcbanmji  (+0.500)
                         - u45(c, l, k, f, m, n)*t4d(f, d, b, a, n, m, j, i) & !clkfmnfdbanmji  (-0.500)
                         + u45(b, l, k, f, m, n)*t4d(f, d, c, a, n, m, j, i))/2.0d0  !blkfmnfdcanmji  (+0.500)
            end do; end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u45), size(u45), '465123', u45, f1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (u109(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4*k2
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, f1, d2, u109)
    deallocate (f1)
    deallocate (d2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,u109,t2b) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - u109(b, l, m, c, k, j)*t2b(d, a, m, i) & !blmckjdami      (-1.000)
                      + u109(c, l, m, b, k, j)*t2b(d, a, m, i) & !clmbkjdami      (+1.000)
                      + u109(b, l, m, d, k, j)*t2b(c, a, m, i) & !blmdkjcami      (+1.000)
                      - u109(c, l, m, d, k, j)*t2b(b, a, m, i) & !clmdkjbami      (-1.000)
                      - u109(d, l, m, b, k, j)*t2b(c, a, m, i) & !dlmbkjcami      (-1.000)
                      + u109(d, l, m, c, k, j)*t2b(b, a, m, i) & !dlmckjbami      (+1.000)
                      + u109(b, k, m, c, l, j)*t2b(d, a, m, i) & !bkmcljdami      (+1.000)
                      - u109(c, k, m, b, l, j)*t2b(d, a, m, i) & !ckmbljdami      (-1.000)
                      - u109(b, k, m, d, l, j)*t2b(c, a, m, i) & !bkmdljcami      (-1.000)
                      + u109(c, k, m, d, l, j)*t2b(b, a, m, i) & !ckmdljbami      (+1.000)
                      + u109(d, k, m, b, l, j)*t2b(c, a, m, i) & !dkmbljcami      (+1.000)
                      - u109(d, k, m, c, l, j)*t2b(b, a, m, i) & !dkmcljbami      (-1.000)
                      + u109(c, j, m, b, l, k)*t2b(d, a, m, i) & !cjmblkdami      (+1.000)
                      - u109(b, j, m, c, l, k)*t2b(d, a, m, i) & !bjmclkdami      (-1.000)
                      - u109(d, j, m, b, l, k)*t2b(c, a, m, i) & !djmblkcami      (-1.000)
                      + u109(d, j, m, c, l, k)*t2b(b, a, m, i) & !djmclkbami      (+1.000)
                      + u109(b, j, m, d, l, k)*t2b(c, a, m, i) & !bjmdlkcami      (+1.000)
                      - u109(c, j, m, d, l, k)*t2b(b, a, m, i)         !cjmdlkbami      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u109)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u45), size(u45), '541236', u45, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s152(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4
    i3 = k4*k2
    call egemm1(i1, i3, f1, b2, s152)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x3), size(x3), '2341', -1.000, &
                    x3, s152)
    deallocate (s152)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u45), size(u45), '541236', u45, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u95(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u95)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x48), size(x48), '324561', &
                    -1.000, x48, u95)
    deallocate (u95)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3c,x48) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x48(m, f, d, c, k, j)*t3c(f, b, a, m, l, i) & !mfdckjfbamli    (+1.000)
                      - x48(m, f, d, b, k, j)*t3c(f, c, a, m, l, i) & !mfdbkjfcamli    (-1.000)
                      - x48(m, f, c, d, k, j)*t3c(f, b, a, m, l, i) & !mfcdkjfbamli    (-1.000)
                      + x48(m, f, b, d, k, j)*t3c(f, c, a, m, l, i) & !mfbdkjfcamli    (+1.000)
                      + x48(m, f, c, b, k, j)*t3c(f, d, a, m, l, i) & !mfcbkjfdamli    (+1.000)
                      - x48(m, f, b, c, k, j)*t3c(f, d, a, m, l, i) & !mfbckjfdamli    (-1.000)
                      - x48(m, f, d, c, l, j)*t3c(f, b, a, m, k, i) & !mfdcljfbamki    (-1.000)
                      + x48(m, f, d, b, l, j)*t3c(f, c, a, m, k, i) & !mfdbljfcamki    (+1.000)
                      + x48(m, f, c, d, l, j)*t3c(f, b, a, m, k, i) & !mfcdljfbamki    (+1.000)
                      - x48(m, f, b, d, l, j)*t3c(f, c, a, m, k, i) & !mfbdljfcamki    (-1.000)
                      - x48(m, f, c, b, l, j)*t3c(f, d, a, m, k, i) & !mfcbljfdamki    (-1.000)
                      + x48(m, f, b, c, l, j)*t3c(f, d, a, m, k, i) & !mfbcljfdamki    (+1.000)
                      + x48(m, f, d, c, l, k)*t3c(f, b, a, m, j, i) & !mfdclkfbamji    (+1.000)
                      - x48(m, f, d, b, l, k)*t3c(f, c, a, m, j, i) & !mfdblkfcamji    (-1.000)
                      - x48(m, f, c, d, l, k)*t3c(f, b, a, m, j, i) & !mfcdlkfbamji    (-1.000)
                      + x48(m, f, b, d, l, k)*t3c(f, c, a, m, j, i) & !mfbdlkfcamji    (+1.000)
                      + x48(m, f, c, b, l, k)*t3c(f, d, a, m, j, i) & !mfcblkfdamji    (+1.000)
                      - x48(m, f, b, c, l, k)*t3c(f, d, a, m, j, i)      !mfbclkfdamji    (-1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x48)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u45), size(u45), '451236', u45, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u93(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u93)
    deallocate (f1)
    deallocate (b2)
    deallocate (u45)

    call sum_stripe(6, shape(x46), size(x46), '623451', &
                    -1.000, x46, u93)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x46,t3c) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (x46(n, m, c, l, k, j)*t3c(d, b, a, n, m, i) & !nmclkjdbanmi    (+0.500)
                         - x46(n, m, d, l, k, j)*t3c(c, b, a, n, m, i) & !nmdlkjcbanmi    (-0.500)
                         - x46(n, m, b, l, k, j)*t3c(d, c, a, n, m, i) & !nmblkjdcanmi    (-0.500)
                         + x46(n, m, d, l, j, k)*t3c(c, b, a, n, m, i) & !nmdljkcbanmi    (+0.500)
                         - x46(n, m, c, l, j, k)*t3c(d, b, a, n, m, i) & !nmcljkdbanmi    (-0.500)
                         + x46(n, m, b, l, j, k)*t3c(d, c, a, n, m, i) & !nmbljkdcanmi    (+0.500)
                         - x46(n, m, d, k, j, l)*t3c(c, b, a, n, m, i) & !nmdkjlcbanmi    (-0.500)
                         + x46(n, m, c, k, j, l)*t3c(d, b, a, n, m, i) & !nmckjldbanmi    (+0.500)
                         - x46(n, m, b, k, j, l)*t3c(d, c, a, n, m, i))/2.0d0  !nmbkjldcanmi    (-0.500)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x46)

    allocate (f1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u93), size(u93), '263451', u93, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u120(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u120)
    deallocate (f1)
    deallocate (b2)
    deallocate (u93)

    call sum_stripe(6, shape(x28), size(x28), '213456', &
                    -1.000, x28, u120)
    deallocate (u120)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2b,x28) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      + x28(n, d, b, l, k, j)*t2b(c, a, n, i) & !ndblkjcani      (+1.000)
                      - x28(n, d, c, l, k, j)*t2b(b, a, n, i) & !ndclkjbani      (-1.000)
                      - x28(n, c, b, l, k, j)*t2b(d, a, n, i) & !ncblkjdani      (-1.000)
                      + x28(n, b, c, l, k, j)*t2b(d, a, n, i) & !nbclkjdani      (+1.000)
                      + x28(n, c, d, l, k, j)*t2b(b, a, n, i) & !ncdlkjbani      (+1.000)
                      - x28(n, b, d, l, k, j)*t2b(c, a, n, i) & !nbdlkjcani      (-1.000)
                      - x28(n, d, b, l, j, k)*t2b(c, a, n, i) & !ndbljkcani      (-1.000)
                      + x28(n, d, c, l, j, k)*t2b(b, a, n, i) & !ndcljkbani      (+1.000)
                      + x28(n, c, b, l, j, k)*t2b(d, a, n, i) & !ncbljkdani      (+1.000)
                      - x28(n, b, c, l, j, k)*t2b(d, a, n, i) & !nbcljkdani      (-1.000)
                      - x28(n, c, d, l, j, k)*t2b(b, a, n, i) & !ncdljkbani      (-1.000)
                      + x28(n, b, d, l, j, k)*t2b(c, a, n, i) & !nbdljkcani      (+1.000)
                      + x28(n, d, b, k, j, l)*t2b(c, a, n, i) & !ndbkjlcani      (+1.000)
                      - x28(n, d, c, k, j, l)*t2b(b, a, n, i) & !ndckjlbani      (-1.000)
                      - x28(n, c, b, k, j, l)*t2b(d, a, n, i) & !ncbkjldani      (-1.000)
                      + x28(n, b, c, k, j, l)*t2b(d, a, n, i) & !nbckjldani      (+1.000)
                      + x28(n, c, d, k, j, l)*t2b(b, a, n, i) & !ncdkjlbani      (+1.000)
                      - x28(n, b, d, k, j, l)*t2b(c, a, n, i)          !nbdkjlcani      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x28)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s74(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k2
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, d2, s74)
    deallocate (d1)
    deallocate (d2)

    allocate (x17(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    x17 = 0.0d0
    call sum_stripe(4, shape(x17), size(x17), '3421', 0.500, &
                    x17, s74)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s74), size(s74), '3412', s74, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u110(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, d1, d2, u110)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x24), size(x24), '236145', &
                    0.500, x24, u110)
    deallocate (u110)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s74), size(s74), '3412', s74, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s151(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s151)
    deallocate (d1)
    deallocate (b2)
    deallocate (s74)

    call sum_stripe(4, shape(x3), size(x3), '2134', -0.500, &
                    x3, s151)
    deallocate (s151)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
    allocate (h2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(8, shape(t4d), size(t4d), '12534678', t4d, h2)
    allocate (u46(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k2*k3*k4
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, h2, u46)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x45), size(x45), '234561', &
                    0.500, x45, u46)
    deallocate (u46)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,x45) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x45(m, b, a, l, k, i)*t2c(d, c, m, j) & !mbalkidcmj      (+1.000)
                      - x45(m, c, a, l, k, i)*t2c(d, b, m, j) & !mcalkidbmj      (-1.000)
                      + x45(m, d, a, l, k, i)*t2c(c, b, m, j) & !mdalkicbmj      (+1.000)
                      - x45(m, b, a, l, j, i)*t2c(d, c, m, k) & !mbaljidcmk      (-1.000)
                      + x45(m, c, a, l, j, i)*t2c(d, b, m, k) & !mcaljidbmk      (+1.000)
                      - x45(m, d, a, l, j, i)*t2c(c, b, m, k) & !mdaljicbmk      (-1.000)
                      + x45(m, b, a, k, j, i)*t2c(d, c, m, l) & !mbakjidcml      (+1.000)
                      - x45(m, c, a, k, j, i)*t2c(d, b, m, l) & !mcakjidbml      (-1.000)
                      + x45(m, d, a, k, j, i)*t2c(c, b, m, l)          !mdakjicbml      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x45)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s75(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s75)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x18), size(x18), '3421', 1.000, &
                    x18, s75)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s75), size(s75), '4312', s75, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s153(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s153)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x26), size(x26), '2134', -1.000, &
                    x26, s153)
    deallocate (s153)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x26,t3c) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      - x26(e, d, c, j)*t3c(e, b, a, l, k, i) & !edcjebalki      (-1.000)
                      + x26(e, d, b, j)*t3c(e, c, a, l, k, i) & !edbjecalki      (+1.000)
                      + x26(e, c, d, j)*t3c(e, b, a, l, k, i) & !ecdjebalki      (+1.000)
                      - x26(e, b, d, j)*t3c(e, c, a, l, k, i) & !ebdjecalki      (-1.000)
                      - x26(e, c, b, j)*t3c(e, d, a, l, k, i) & !ecbjedalki      (-1.000)
                      + x26(e, b, c, j)*t3c(e, d, a, l, k, i) & !ebcjedalki      (+1.000)
                      + x26(e, d, c, k)*t3c(e, b, a, l, j, i) & !edckebalji      (+1.000)
                      - x26(e, d, b, k)*t3c(e, c, a, l, j, i) & !edbkecalji      (-1.000)
                      - x26(e, c, d, k)*t3c(e, b, a, l, j, i) & !ecdkebalji      (-1.000)
                      + x26(e, b, d, k)*t3c(e, c, a, l, j, i) & !ebdkecalji      (+1.000)
                      + x26(e, c, b, k)*t3c(e, d, a, l, j, i) & !ecbkedalji      (+1.000)
                      - x26(e, b, c, k)*t3c(e, d, a, l, j, i) & !ebckedalji      (-1.000)
                      - x26(e, d, c, l)*t3c(e, b, a, k, j, i) & !edclebakji      (-1.000)
                      + x26(e, d, b, l)*t3c(e, c, a, k, j, i) & !edblecakji      (+1.000)
                      + x26(e, c, d, l)*t3c(e, b, a, k, j, i) & !ecdlebakji      (+1.000)
                      - x26(e, b, d, l)*t3c(e, c, a, k, j, i) & !ebdlecakji      (-1.000)
                      - x26(e, c, b, l)*t3c(e, d, a, k, j, i) & !ecbledakji      (-1.000)
                      + x26(e, b, c, l)*t3c(e, d, a, k, j, i)          !ebcledakji      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x26)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s75), size(s75), '3412', s75, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s149(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s149)
    deallocate (d1)
    deallocate (b2)
    deallocate (s75)

    call sum_stripe(4, shape(x25), size(x25), '4123', -1.000, &
                    x25, s149)
    deallocate (s149)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3c,x25) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x25(m, d, k, j)*t3c(c, b, a, m, l, i) & !mdkjcbamli      (+1.000)
                      - x25(m, c, k, j)*t3c(d, b, a, m, l, i) & !mckjdbamli      (-1.000)
                      + x25(m, b, k, j)*t3c(d, c, a, m, l, i) & !mbkjdcamli      (+1.000)
                      - x25(m, d, l, j)*t3c(c, b, a, m, k, i) & !mdljcbamki      (-1.000)
                      + x25(m, c, l, j)*t3c(d, b, a, m, k, i) & !mcljdbamki      (+1.000)
                      - x25(m, b, l, j)*t3c(d, c, a, m, k, i) & !mbljdcamki      (-1.000)
                      - x25(m, d, j, k)*t3c(c, b, a, m, l, i) & !mdjkcbamli      (-1.000)
                      + x25(m, c, j, k)*t3c(d, b, a, m, l, i) & !mcjkdbamli      (+1.000)
                      - x25(m, b, j, k)*t3c(d, c, a, m, l, i) & !mbjkdcamli      (-1.000)
                      + x25(m, d, j, l)*t3c(c, b, a, m, k, i) & !mdjlcbamki      (+1.000)
                      - x25(m, c, j, l)*t3c(d, b, a, m, k, i) & !mcjldbamki      (-1.000)
                      + x25(m, b, j, l)*t3c(d, c, a, m, k, i) & !mbjldcamki      (+1.000)
                      + x25(m, d, l, k)*t3c(c, b, a, m, j, i) & !mdlkcbamji      (+1.000)
                      - x25(m, c, l, k)*t3c(d, b, a, m, j, i) & !mclkdbamji      (-1.000)
                      + x25(m, b, l, k)*t3c(d, c, a, m, j, i) & !mblkdcamji      (+1.000)
                      - x25(m, d, k, l)*t3c(c, b, a, m, j, i) & !mdklcbamji      (-1.000)
                      + x25(m, c, k, l)*t3c(d, b, a, m, j, i) & !mckldbamji      (+1.000)
                      - x25(m, b, k, l)*t3c(d, c, a, m, j, i)          !mbkldcamji      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x25)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (q19(n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, d2, q19)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x10), size(x10), '21', -0.500, &
                    x10, q19)
    deallocate (q19)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '1243', intb, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '4312', t2c, d2)
    allocate (s76(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k4*k4
    i3 = k2*k2
    call egemm(i1, i2, i3, d1, d2, s76)
    deallocate (d1)
    deallocate (d2)

    allocate (x19(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    x19 = 0.0d0
    call sum_stripe(4, shape(x19), size(x19), '3412', 0.500, &
                    x19, s76)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s76), size(s76), '4312', s76, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s150(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s150)
    deallocate (d1)
    deallocate (b2)
    deallocate (s76)

    call sum_stripe(4, shape(x4), size(x4), '4123', 0.500, x4, &
                    s150)
    deallocate (s150)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '1432', t2c, d2)
    allocate (q20(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k2*k2*k4
    call egemm(i1, i2, i3, d1, d2, q20)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(2, shape(x11), size(x11), '21', 0.500, &
                    x11, q20)
    deallocate (q20)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '251346', t3b, f2)
    allocate (u47(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k2*k3*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u47)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x31), size(x31), '345621', &
                    1.000, x31, u47)
    deallocate (u47)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (f2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '235146', t3b, f2)
    allocate (s77(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k4
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, f2, s77)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x1), size(x1), '2341', -0.500, &
                    x1, s77)
    deallocate (s77)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3124', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(6, shape(t3b), size(t3b), '265134', t3b, f2)
    allocate (s78(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k1*k1*k3
    call egemm(i1, i2, i3, d1, f2, s78)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x2), size(x2), '2341', 0.500, x2, &
                    s78)
    deallocate (s78)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '251346', t3b, f2)
    allocate (u48(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k2*k3*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u48)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x36), size(x36), '345621', &
                    1.000, x36, u48)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u48), size(u48), '561234', u48, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u91(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u91)
    deallocate (f1)
    deallocate (b2)
    deallocate (u48)

    call sum_stripe(6, shape(x40), size(x40), '512346', &
                    1.000, x40, u91)
    deallocate (u91)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,x40) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      + x40(n, b, a, l, j, i)*t2c(d, c, n, k) & !nbaljidcnk      (+1.000)
                      - x40(n, c, a, l, j, i)*t2c(d, b, n, k) & !ncaljidbnk      (-1.000)
                      + x40(n, d, a, l, j, i)*t2c(c, b, n, k) & !ndaljicbnk      (+1.000)
                      - x40(n, b, a, k, j, i)*t2c(d, c, n, l) & !nbakjidcnl      (-1.000)
                      + x40(n, c, a, k, j, i)*t2c(d, b, n, l) & !ncakjidbnl      (+1.000)
                      - x40(n, d, a, k, j, i)*t2c(c, b, n, l) & !ndakjicbnl      (-1.000)
                      - x40(n, b, a, l, k, i)*t2c(d, c, n, j) & !nbalkidcnj      (-1.000)
                      + x40(n, c, a, l, k, i)*t2c(d, b, n, j) & !ncalkidbnj      (+1.000)
                      - x40(n, d, a, l, k, i)*t2c(c, b, n, j) & !ndalkicbnj      (-1.000)
                      + x40(n, b, a, k, l, i)*t2c(d, c, n, j) & !nbaklidcnj      (+1.000)
                      - x40(n, c, a, k, l, i)*t2c(d, b, n, j) & !ncaklidbnj      (-1.000)
                      + x40(n, d, a, k, l, i)*t2c(c, b, n, j) & !ndaklicbnj      (+1.000)
                      + x40(n, b, a, j, k, i)*t2c(d, c, n, l) & !nbajkidcnl      (+1.000)
                      - x40(n, c, a, j, k, i)*t2c(d, b, n, l) & !ncajkidbnl      (-1.000)
                      + x40(n, d, a, j, k, i)*t2c(c, b, n, l) & !ndajkicbnl      (+1.000)
                      - x40(n, b, a, j, l, i)*t2c(d, c, n, k) & !nbajlidcnk      (-1.000)
                      + x40(n, c, a, j, l, i)*t2c(d, b, n, k) & !ncajlidbnk      (+1.000)
                      - x40(n, d, a, j, l, i)*t2c(c, b, n, k)          !ndajlicbnk      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x40)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '125346', t3b, f2)
    allocate (s79(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k3
    i3 = k1*k3*k4
    call egemm(i1, i2, i3, d1, f2, s79)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                    s79)
    deallocate (s79)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '254136', t3b, f2)
    allocate (s80(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2*k1*k3
    call egemm(i1, i2, i3, d1, f2, s80)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                    x6, s80)
    deallocate (s80)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '164235', t3c, f2)
    allocate (s81(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k2*k1*k4
    call egemm(i1, i2, i3, d1, f2, s81)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,s81,t3c) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      - s81(b, a, l, e)*t3c(d, c, e, k, j, i) & !baledcekji      (-1.000)
                      + s81(c, a, l, e)*t3c(d, b, e, k, j, i) & !caledbekji      (+1.000)
                      - s81(d, a, l, e)*t3c(c, b, e, k, j, i) & !dalecbekji      (-1.000)
                      + s81(b, a, k, e)*t3c(d, c, e, l, j, i) & !bakedcelji      (+1.000)
                      - s81(c, a, k, e)*t3c(d, b, e, l, j, i) & !cakedbelji      (-1.000)
                      + s81(d, a, k, e)*t3c(c, b, e, l, j, i) & !dakecbelji      (+1.000)
                      - s81(b, a, j, e)*t3c(d, c, e, l, k, i) & !bajedcelki      (-1.000)
                      + s81(c, a, j, e)*t3c(d, b, e, l, k, i) & !cajedbelki      (+1.000)
                      - s81(d, a, j, e)*t3c(c, b, e, l, k, i)          !dajecbelki      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s81)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '364125', t3c, f2)
    allocate (s82(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2*k1*k3
    call egemm(i1, i2, i3, d1, f2, s82)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,s82,t3c) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      + s82(c, b, l, f)*t3c(f, d, a, k, j, i) & !cblffdakji      (+1.000)
                      - s82(d, b, l, f)*t3c(f, c, a, k, j, i) & !dblffcakji      (-1.000)
                      + s82(d, c, l, f)*t3c(f, b, a, k, j, i) & !dclffbakji      (+1.000)
                      - s82(c, b, k, f)*t3c(f, d, a, l, j, i) & !cbkffdalji      (-1.000)
                      + s82(d, b, k, f)*t3c(f, c, a, l, j, i) & !dbkffcalji      (+1.000)
                      - s82(d, c, k, f)*t3c(f, b, a, l, j, i) & !dckffbalji      (-1.000)
                      + s82(c, b, j, f)*t3c(f, d, a, l, k, i) & !cbjffdalki      (+1.000)
                      - s82(d, b, j, f)*t3c(f, c, a, l, k, i) & !dbjffcalki      (-1.000)
                      + s82(d, c, j, f)*t3c(f, b, a, l, k, i)          !dcjffbalki      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s82)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '132456', t3c, f2)
    allocate (u49(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2*k2*k4
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u49)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x42), size(x42), '345621', &
                    1.000, x42, u49)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x42,t3c) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      - x42(n, m, d, k, j, i)*t3c(c, b, a, n, l, m) & !nmdkjicbanlm    (-1.000)
                      + x42(n, m, c, k, j, i)*t3c(d, b, a, n, l, m) & !nmckjidbanlm    (+1.000)
                      - x42(n, m, b, k, j, i)*t3c(d, c, a, n, l, m) & !nmbkjidcanlm    (-1.000)
                      + x42(n, m, d, l, j, i)*t3c(c, b, a, n, k, m) & !nmdljicbankm    (+1.000)
                      - x42(n, m, c, l, j, i)*t3c(d, b, a, n, k, m) & !nmcljidbankm    (-1.000)
                      + x42(n, m, b, l, j, i)*t3c(d, c, a, n, k, m) & !nmbljidcankm    (+1.000)
                      - x42(n, m, d, l, k, i)*t3c(c, b, a, n, j, m) & !nmdlkicbanjm    (-1.000)
                      + x42(n, m, c, l, k, i)*t3c(d, b, a, n, j, m) & !nmclkidbanjm    (+1.000)
                      - x42(n, m, b, l, k, i)*t3c(d, c, a, n, j, m)      !nmblkidcanjm    (-1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x42)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u49), size(u49), '651234', u49, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u86(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u86)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x22), size(x22), '213456', &
                    -1.000, x22, u86)
    deallocate (u86)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x22,t2b) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x22(m, d, b, l, k, i)*t2b(c, a, j, m) & !mdblkicajm      (+1.000)
                      - x22(m, d, c, l, k, i)*t2b(b, a, j, m) & !mdclkibajm      (-1.000)
                      - x22(m, c, b, l, k, i)*t2b(d, a, j, m) & !mcblkidajm      (-1.000)
                      + x22(m, b, c, l, k, i)*t2b(d, a, j, m) & !mbclkidajm      (+1.000)
                      + x22(m, c, d, l, k, i)*t2b(b, a, j, m) & !mcdlkibajm      (+1.000)
                      - x22(m, b, d, l, k, i)*t2b(c, a, j, m) & !mbdlkicajm      (-1.000)
                      - x22(m, d, b, l, j, i)*t2b(c, a, k, m) & !mdbljicakm      (-1.000)
                      + x22(m, d, c, l, j, i)*t2b(b, a, k, m) & !mdcljibakm      (+1.000)
                      + x22(m, c, b, l, j, i)*t2b(d, a, k, m) & !mcbljidakm      (+1.000)
                      - x22(m, b, c, l, j, i)*t2b(d, a, k, m) & !mbcljidakm      (-1.000)
                      - x22(m, c, d, l, j, i)*t2b(b, a, k, m) & !mcdljibakm      (-1.000)
                      + x22(m, b, d, l, j, i)*t2b(c, a, k, m) & !mbdljicakm      (+1.000)
                      + x22(m, d, b, k, j, i)*t2b(c, a, l, m) & !mdbkjicalm      (+1.000)
                      - x22(m, d, c, k, j, i)*t2b(b, a, l, m) & !mdckjibalm      (-1.000)
                      - x22(m, c, b, k, j, i)*t2b(d, a, l, m) & !mcbkjidalm      (-1.000)
                      + x22(m, b, c, k, j, i)*t2b(d, a, l, m) & !mbckjidalm      (+1.000)
                      + x22(m, c, d, k, j, i)*t2b(b, a, l, m) & !mcdkjibalm      (+1.000)
                      - x22(m, b, d, k, j, i)*t2b(c, a, l, m)          !mbdkjicalm      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x22)

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u49), size(u49), '561234', u49, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u68(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u68)
    deallocate (f1)
    deallocate (b2)
    deallocate (u49)

    call sum_stripe(6, shape(x43), size(x43), '312456', &
                    1.000, x43, u68)
    deallocate (u68)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '136245', t3c, f2)
    allocate (s83(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k1*k3*k4
    call egemm(i1, i2, i3, d1, f2, s83)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,s83,t3c) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      + s83(b, l, k, n)*t3c(d, c, a, n, j, i) & !blkndcanji      (+1.000)
                      - s83(c, l, k, n)*t3c(d, b, a, n, j, i) & !clkndbanji      (-1.000)
                      + s83(d, l, k, n)*t3c(c, b, a, n, j, i) & !dlkncbanji      (+1.000)
                      - s83(b, l, j, n)*t3c(d, c, a, n, k, i) & !bljndcanki      (-1.000)
                      + s83(c, l, j, n)*t3c(d, b, a, n, k, i) & !cljndbanki      (+1.000)
                      - s83(d, l, j, n)*t3c(c, b, a, n, k, i) & !dljncbanki      (-1.000)
                      + s83(b, k, j, n)*t3c(d, c, a, n, l, i) & !bkjndcanli      (+1.000)
                      - s83(c, k, j, n)*t3c(d, b, a, n, l, i) & !ckjndbanli      (-1.000)
                      + s83(d, k, j, n)*t3c(c, b, a, n, l, i)          !dkjncbanli      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s83)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4132', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '162345', t3c, f2)
    allocate (u50(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k2*k2*k3*k4
    i3 = k1*k4
    call egemm(i1, i2, i3, d1, f2, u50)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,u50,t3c) &
        !$omp private(a,b,c,d,n,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do n = n0 + 1, n2
                sum = sum &
                      - u50(b, a, l, k, e, n)*t3c(d, c, e, n, j, i) & !balkendcenji    (-1.000)
                      + u50(c, a, l, k, e, n)*t3c(d, b, e, n, j, i) & !calkendbenji    (+1.000)
                      - u50(d, a, l, k, e, n)*t3c(c, b, e, n, j, i) & !dalkencbenji    (-1.000)
                      + u50(b, a, l, j, e, n)*t3c(d, c, e, n, k, i) & !baljendcenki    (+1.000)
                      - u50(c, a, l, j, e, n)*t3c(d, b, e, n, k, i) & !caljendbenki    (-1.000)
                      + u50(d, a, l, j, e, n)*t3c(c, b, e, n, k, i) & !daljencbenki    (+1.000)
                      - u50(b, a, k, j, e, n)*t3c(d, c, e, n, l, i) & !bakjendcenli    (-1.000)
                      + u50(c, a, k, j, e, n)*t3c(d, b, e, n, l, i) & !cakjendbenli    (+1.000)
                      - u50(d, a, k, j, e, n)*t3c(c, b, e, n, l, i)      !dakjencbenli    (-1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u50), size(u50), '561234', u50, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u66(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k3*k4*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u66)
    deallocate (f1)
    deallocate (b2)
    deallocate (u50)

    call sum_stripe(6, shape(x43), size(x43), '612345', &
                    1.000, x43, u66)
    deallocate (u66)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,x43) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - x43(n, b, a, l, k, i)*t2c(d, c, n, j) & !nbalkidcnj      (-1.000)
                      + x43(n, c, a, l, k, i)*t2c(d, b, n, j) & !ncalkidbnj      (+1.000)
                      - x43(n, d, a, l, k, i)*t2c(c, b, n, j) & !ndalkicbnj      (-1.000)
                      + x43(n, b, a, l, j, i)*t2c(d, c, n, k) & !nbaljidcnk      (+1.000)
                      - x43(n, c, a, l, j, i)*t2c(d, b, n, k) & !ncaljidbnk      (-1.000)
                      + x43(n, d, a, l, j, i)*t2c(c, b, n, k) & !ndaljicbnk      (+1.000)
                      - x43(n, b, a, k, j, i)*t2c(d, c, n, l) & !nbakjidcnl      (-1.000)
                      + x43(n, c, a, k, j, i)*t2c(d, b, n, l) & !ncakjidbnl      (+1.000)
                      - x43(n, d, a, k, j, i)*t2c(c, b, n, l)          !ndakjicbnl      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x43)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
    allocate (u51(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k2*k3*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u51)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x31), size(x31), '345621', &
                    1.000, x31, u51)
    deallocate (u51)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x31,t3c) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x31(m, f, d, a, j, i)*t3c(c, b, f, l, k, m) & !mfdajicbflkm    (+1.000)
                      - x31(m, f, c, a, j, i)*t3c(d, b, f, l, k, m) & !mfcajidbflkm    (-1.000)
                      + x31(m, f, b, a, j, i)*t3c(d, c, f, l, k, m) & !mfbajidcflkm    (+1.000)
                      - x31(m, f, d, a, k, i)*t3c(c, b, f, l, j, m) & !mfdakicbfljm    (-1.000)
                      + x31(m, f, c, a, k, i)*t3c(d, b, f, l, j, m) & !mfcakidbfljm    (+1.000)
                      - x31(m, f, b, a, k, i)*t3c(d, c, f, l, j, m) & !mfbakidcfljm    (-1.000)
                      + x31(m, f, d, a, l, i)*t3c(c, b, f, k, j, m) & !mfdalicbfkjm    (+1.000)
                      - x31(m, f, c, a, l, i)*t3c(d, b, f, k, j, m) & !mfcalidbfkjm    (-1.000)
                      + x31(m, f, b, a, l, i)*t3c(d, c, f, k, j, m)      !mfbalidcfkjm    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x31)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '134256', t3c, f2)
    allocate (s84(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k4
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, f2, s84)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s84)
    deallocate (s84)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '4123', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '154236', t3d, f2)
    allocate (s85(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2*k2*k4
    call egemm(i1, i2, i3, d1, f2, s85)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3c,s85) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      + (s85(c, b, l, e)*t3c(e, d, a, k, j, i) & !cbleedakji      (+0.500)
                         - s85(d, b, l, e)*t3c(e, c, a, k, j, i) & !dbleecakji      (-0.500)
                         + s85(d, c, l, e)*t3c(e, b, a, k, j, i) & !dcleebakji      (+0.500)
                         - s85(c, b, k, e)*t3c(e, d, a, l, j, i) & !cbkeedalji      (-0.500)
                         + s85(d, b, k, e)*t3c(e, c, a, l, j, i) & !dbkeecalji      (+0.500)
                         - s85(d, c, k, e)*t3c(e, b, a, l, j, i) & !dckeebalji      (-0.500)
                         + s85(c, b, j, e)*t3c(e, d, a, l, k, i) & !cbjeedalki      (+0.500)
                         - s85(d, b, j, e)*t3c(e, c, a, l, k, i) & !dbjeecalki      (-0.500)
                         + s85(d, c, j, e)*t3c(e, b, a, l, k, i))/2.0d0   !dcjeebalki      (+0.500)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s85)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '123456', t3c, f2)
    allocate (u52(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k2*k2*k3
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, f2, u52)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3d,u52) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (u52(a, k, j, i, m, n)*t3d(d, c, b, n, m, l) & !akjimndcbnml    (+0.250)
                         - u52(a, l, j, i, m, n)*t3d(d, c, b, n, m, k) & !aljimndcbnmk    (-0.250)
                         + u52(a, l, k, i, m, n)*t3d(d, c, b, n, m, j))/4.0d0 !alkimndcbnmj    (+0.250)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u52), size(u52), '561234', u52, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u96(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k3*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u96)
    deallocate (f1)
    deallocate (b2)
    deallocate (u52)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,u96,t2c) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum & !top two switched
                      + (u96(c, n, a, l, k, i)*t2c(d, b, n, j) & !cnalkidbnj      (+0.500)
                         - u96(d, n, a, l, k, i)*t2c(c, b, n, j) & !dnalkicbnj      (-0.500)
                         - u96(b, n, a, l, k, i)*t2c(d, c, n, j) & !bnalkidcnj      (-0.500)
                         + u96(d, n, a, l, j, i)*t2c(c, b, n, k) & !dnaljicbnk      (+0.500)
                         - u96(c, n, a, l, j, i)*t2c(d, b, n, k) & !cnaljidbnk      (-0.500)
                         + u96(b, n, a, l, j, i)*t2c(d, c, n, k) & !bnaljidcnk      (+0.500)
                         - u96(d, n, a, k, j, i)*t2c(c, b, n, l) & !dnakjicbnl      (-0.500)
                         + u96(c, n, a, k, j, i)*t2c(d, b, n, l) & !cnakjidbnl      (+0.500)
                         - u96(b, n, a, k, j, i)*t2c(d, c, n, l))/2.0d0   !bnakjidcnl      (-0.500)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u96)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '124356', t3d, f2)
    allocate (s86(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, f2, s86)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,s86,t3c) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + (s86(b, l, k, m)*t3c(d, c, a, m, j, i) & !blkmdcamji      (+0.500)
                         - s86(c, l, k, m)*t3c(d, b, a, m, j, i) & !clkmdbamji      (-0.500)
                         + s86(d, l, k, m)*t3c(c, b, a, m, j, i) & !dlkmcbamji      (+0.500)
                         - s86(b, l, j, m)*t3c(d, c, a, m, k, i) & !bljmdcamki      (-0.500)
                         + s86(c, l, j, m)*t3c(d, b, a, m, k, i) & !cljmdbamki      (+0.500)
                         - s86(d, l, j, m)*t3c(c, b, a, m, k, i) & !dljmcbamki      (-0.500)
                         + s86(b, k, j, m)*t3c(d, c, a, m, l, i) & !bkjmdcamli      (+0.500)
                         - s86(c, k, j, m)*t3c(d, b, a, m, l, i) & !ckjmdbamli      (-0.500)
                         + s86(d, k, j, m)*t3c(c, b, a, m, l, i))/2.0d0   !dkjmcbamli      (+0.500)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s86)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
    allocate (u53(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k2*k3*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u53)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x36), size(x36), '345621', &
                    1.000, x36, u53)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x36,t3d) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x36(m, f, d, a, j, i)*t3d(f, c, b, m, l, k) & !mfdajifcbmlk    (+1.000)
                      - x36(m, f, c, a, j, i)*t3d(f, d, b, m, l, k) & !mfcajifdbmlk    (-1.000)
                      + x36(m, f, b, a, j, i)*t3d(f, d, c, m, l, k) & !mfbajifdcmlk    (+1.000)
                      - x36(m, f, d, a, k, i)*t3d(f, c, b, m, l, j) & !mfdakifcbmlj    (-1.000)
                      + x36(m, f, c, a, k, i)*t3d(f, d, b, m, l, j) & !mfcakifdbmlj    (+1.000)
                      - x36(m, f, b, a, k, i)*t3d(f, d, c, m, l, j) & !mfbakifdcmlj    (-1.000)
                      + x36(m, f, d, a, l, i)*t3d(f, c, b, m, k, j) & !mfdalifcbmkj    (+1.000)
                      - x36(m, f, c, a, l, i)*t3d(f, d, b, m, k, j) & !mfcalifdbmkj    (-1.000)
                      + x36(m, f, b, a, l, i)*t3d(f, d, c, m, k, j)      !mfbalifdcmkj    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x36)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u53), size(u53), '561234', u53, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u94(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u94)
    deallocate (f1)
    deallocate (b2)
    deallocate (u53)

    call sum_stripe(6, shape(x47), size(x47), '512346', &
                    1.000, x47, u94)
    deallocate (u94)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,x47) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x47(m, b, a, l, j, i)*t2c(d, c, m, k) & !mbaljidcmk      (+1.000)
                      - x47(m, c, a, l, j, i)*t2c(d, b, m, k) & !mcaljidbmk      (-1.000)
                      + x47(m, d, a, l, j, i)*t2c(c, b, m, k) & !mdaljicbmk      (+1.000)
                      - x47(m, b, a, k, j, i)*t2c(d, c, m, l) & !mbakjidcml      (-1.000)
                      + x47(m, c, a, k, j, i)*t2c(d, b, m, l) & !mcakjidbml      (+1.000)
                      - x47(m, d, a, k, j, i)*t2c(c, b, m, l) & !mdakjicbml      (-1.000)
                      - x47(m, b, a, l, k, i)*t2c(d, c, m, j) & !mbalkidcmj      (-1.000)
                      + x47(m, c, a, l, k, i)*t2c(d, b, m, j) & !mcalkidbmj      (+1.000)
                      - x47(m, d, a, l, k, i)*t2c(c, b, m, j) & !mdalkicbmj      (-1.000)
                      + x47(m, b, a, k, l, i)*t2c(d, c, m, j) & !mbaklidcmj      (+1.000)
                      - x47(m, c, a, k, l, i)*t2c(d, b, m, j) & !mcaklidbmj      (-1.000)
                      + x47(m, d, a, k, l, i)*t2c(c, b, m, j) & !mdaklicbmj      (+1.000)
                      + x47(m, b, a, j, k, i)*t2c(d, c, m, l) & !mbajkidcml      (+1.000)
                      - x47(m, c, a, j, k, i)*t2c(d, b, m, l) & !mcajkidbml      (-1.000)
                      + x47(m, d, a, j, k, i)*t2c(c, b, m, l) & !mdajkicbml      (+1.000)
                      - x47(m, b, a, j, l, i)*t2c(d, c, m, k) & !mbajlidcmk      (-1.000)
                      + x47(m, c, a, j, l, i)*t2c(d, b, m, k) & !mcajlidbmk      (+1.000)
                      - x47(m, d, a, j, l, i)*t2c(c, b, m, k)          !mdajlicbmk      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x47)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '124356', t3c, f2)
    allocate (s87(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k3
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, f2, s87)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x5), size(x5), '2341', -0.500, &
                    x5, s87)
    deallocate (s87)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '123456', t3d, f2)
    allocate (u54(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k2*k2*k4
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, f2, u54)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3c,u54) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (u54(b, l, k, j, m, n)*t3c(d, c, a, n, m, i) & !blkjmndcanmi    (+0.250)
                         - u54(c, l, k, j, m, n)*t3c(d, b, a, n, m, i) & !clkjmndbanmi    (-0.250)
                         + u54(d, l, k, j, m, n)*t3c(c, b, a, n, m, i))/4.0d0 !dlkjmncbanmi    (+0.250)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u54), size(u54), '561234', u54, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u90(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u90)
    deallocate (f1)
    deallocate (b2)
    deallocate (u54)

    call sum_stripe(6, shape(x38), size(x38), '213456', &
                    -1.000, x38, u90)
    deallocate (u90)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x38,t2b) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + (x38(m, d, b, l, k, j)*t2b(c, a, m, i) & !mdblkjcami      (+0.500)
                         - x38(m, d, c, l, k, j)*t2b(b, a, m, i) & !mdclkjbami      (-0.500)
                         - x38(m, c, b, l, k, j)*t2b(d, a, m, i) & !mcblkjdami      (-0.500)
                         + x38(m, b, c, l, k, j)*t2b(d, a, m, i) & !mbclkjdami      (+0.500)
                         + x38(m, c, d, l, k, j)*t2b(b, a, m, i) & !mcdlkjbami      (+0.500)
                         - x38(m, b, d, l, k, j)*t2b(c, a, m, i))/2.0d0   !mbdlkjcami      (-0.500)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x38)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '154236', t3c, f2)
    allocate (s88(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2*k2*k4
    call egemm(i1, i2, i3, d1, f2, s88)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x6), size(x6), '2341', 0.500, x6, &
                    s88)
    deallocate (s88)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s91(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s91)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s91), size(s91), '2431', s91, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '361245', t3c, f2)
    allocate (u59(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k2*k4*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u59)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x30), size(x30), '234516', &
                    1.000, x30, u59)
    deallocate (u59)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s91), size(s91), '3214', s91, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q21(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q21)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(2, shape(x8), size(x8), '21', -1.000, x8, &
                    q21)
    deallocate (q21)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s91), size(s91), '3214', s91, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s92(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s92)
    deallocate (d1)
    deallocate (b2)
    deallocate (s91)

    call sum_stripe(4, shape(x12), size(x12), '3241', -1.000, &
                    x12, s92)
    deallocate (s92)

  call sum_shift(4,shape(intr),size(intr),shape(x12), &
   size(x12),(/n0-n0,n1-n0,n1-n0,n0-n0/),'3142',1.000,intr,x12)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x12) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x12(m, e, a, i)*t4d(d, c, b, e, l, k, j, m)      !meaidcbelkjm    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x12)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3241', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q22(n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i3 = k1*k3
    call egemm1(i1, i3, d1, b2, q22)
    deallocate (d1)
    deallocate (b2)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q22), size(q22), '21', q22, b1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s112(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s112)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2341', 1.000, x2, &
                    s112)
    deallocate (s112)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q22), size(q22), '21', q22, b1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q23(n1 + 1:n3, n1 + 1:n3))
    i1 = k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, b1, b2, q23)
    deallocate (b1)
    deallocate (b2)
    deallocate (q22)

    call sum_stripe(2, shape(x9), size(x9), '21', 1.000, x9, &
                    q23)
    deallocate (q23)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s93(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s93)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s93), size(s93), '2431', s93, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '142356', t3d, f2)
    allocate (u61(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k2*k4*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u61)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x30), size(x30), '234516', &
                    1.000, x30, u61)
    deallocate (u61)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x30,t2b) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x30(m, c, b, l, k, i)*t2b(d, a, j, m) & !mcblkidajm      (-1.000)
                      + x30(m, d, b, l, k, i)*t2b(c, a, j, m) & !mdblkicajm      (+1.000)
                      - x30(m, d, c, l, k, i)*t2b(b, a, j, m) & !mdclkibajm      (-1.000)
                      + x30(m, c, b, l, j, i)*t2b(d, a, k, m) & !mcbljidakm      (+1.000)
                      - x30(m, d, b, l, j, i)*t2b(c, a, k, m) & !mdbljicakm      (-1.000)
                      + x30(m, d, c, l, j, i)*t2b(b, a, k, m) & !mdcljibakm      (+1.000)
                      - x30(m, c, b, k, j, i)*t2b(d, a, l, m) & !mcbkjidalm      (-1.000)
                      + x30(m, d, b, k, j, i)*t2b(c, a, l, m) & !mdbkjicalm      (+1.000)
                      - x30(m, d, c, k, j, i)*t2b(b, a, l, m)          !mdckjibalm      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x30)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s93), size(s93), '2431', s93, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q24(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q24)
    deallocate (d1)
    deallocate (b2)

    x8 = x8 + q24
    deallocate (q24)

  call sum_shift(2,shape(fockr),size(fockr),shape(x8), &
   size(x8),(/n0,n0/),'12',1.000,fockr,x8)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x8) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x8(m, i)*t4d(d, c, b, a, l, k, j, m)           !midcbalkjm      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x8)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s93), size(s93), '4321', s93, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s104(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s104)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x14), size(x14), '3124', -1.000, &
                    x14, s104)
    deallocate (s104)

  call sum_shift(4,shape(intm),size(intm),shape(x14), &
   size(x14),(/n0-n0,n2-n0,n2-n0,n0-n0/),'1342',1.000,intm,x14)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x14) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      - x14(m, e, d, i)*t4d(e, c, b, a, l, k, j, m) & !mediecbalkjm    (-1.000)
                      + x14(m, e, c, i)*t4d(e, d, b, a, l, k, j, m) & !meciedbalkjm    (+1.000)
                      - x14(m, e, b, i)*t4d(e, d, c, a, l, k, j, m)      !mebiedcalkjm    (-1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x14)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s93), size(s93), '2314', s93, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s103(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s103)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x13), size(x13), '3241', 1.000, &
                    x13, s103)

  call sum_shift(4,shape(intm),size(intm),shape(x13), &
   size(x13),(/n0-n0,n0-n0,n0-n0,n0-n0/),'2143',1.000,intm,x13)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x13,t4d) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + x13(n, m, j, i)*t4d(d, c, b, a, n, l, k, m) & !nmjidcbanlkm    (+1.000)
                      - x13(n, m, k, i)*t4d(d, c, b, a, n, l, j, m) & !nmkidcbanljm    (-1.000)
                      + x13(n, m, l, i)*t4d(d, c, b, a, n, k, j, m)      !nmlidcbankjm    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x13)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s103), size(s103), '2413', s103, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u113(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u113)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x21), size(x21), '234156', &
                    1.000, x21, u113)
    deallocate (u113)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2c,x21) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      + x21(n, d, a, k, j, i)*t2c(c, b, n, l) & !ndakjicbnl      (+1.000)
                      - x21(n, c, a, k, j, i)*t2c(d, b, n, l) & !ncakjidbnl      (-1.000)
                      + x21(n, b, a, k, j, i)*t2c(d, c, n, l) & !nbakjidcnl      (+1.000)
                      - x21(n, d, a, l, j, i)*t2c(c, b, n, k) & !ndaljicbnk      (-1.000)
                      + x21(n, c, a, l, j, i)*t2c(d, b, n, k) & !ncaljidbnk      (+1.000)
                      - x21(n, b, a, l, j, i)*t2c(d, c, n, k) & !nbaljidcnk      (-1.000)
                      - x21(n, d, a, j, k, i)*t2c(c, b, n, l) & !ndajkicbnl      (-1.000)
                      + x21(n, c, a, j, k, i)*t2c(d, b, n, l) & !ncajkidbnl      (+1.000)
                      - x21(n, b, a, j, k, i)*t2c(d, c, n, l) & !nbajkidcnl      (-1.000)
                      + x21(n, d, a, j, l, i)*t2c(c, b, n, k) & !ndajlicbnk      (+1.000)
                      - x21(n, c, a, j, l, i)*t2c(d, b, n, k) & !ncajlidbnk      (-1.000)
                      + x21(n, b, a, j, l, i)*t2c(d, c, n, k) & !nbajlidcnk      (+1.000)
                      + x21(n, d, a, l, k, i)*t2c(c, b, n, j) & !ndalkicbnj      (+1.000)
                      - x21(n, c, a, l, k, i)*t2c(d, b, n, j) & !ncalkidbnj      (-1.000)
                      + x21(n, b, a, l, k, i)*t2c(d, c, n, j) & !nbalkidcnj      (+1.000)
                      - x21(n, d, a, k, l, i)*t2c(c, b, n, j) & !ndaklicbnj      (-1.000)
                      + x21(n, c, a, k, l, i)*t2c(d, b, n, j) & !ncaklidbnj      (+1.000)
                      - x21(n, b, a, k, l, i)*t2c(d, c, n, j)          !nbaklidcnj      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x21)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s103), size(s103), '4213', s103, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s157(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s157)
    deallocate (d1)
    deallocate (b2)
    deallocate (s103)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s157)
    deallocate (s157)

  call sum_shift(4,shape(intm),size(intm),shape(x1), &
   size(x1),(/n0-n0,n2-n0,n0-n0,n0-n0/),'1243',1.000,intm,x1)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x1,t3c) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x1(m, d, j, i)*t3c(c, b, a, l, k, m) & !mdjicbalkm      (-1.000)
                      + x1(m, c, j, i)*t3c(d, b, a, l, k, m) & !mcjidbalkm      (+1.000)
                      - x1(m, b, j, i)*t3c(d, c, a, l, k, m) & !mbjidcalkm      (-1.000)
                      + x1(m, d, k, i)*t3c(c, b, a, l, j, m) & !mdkicbaljm      (+1.000)
                      - x1(m, c, k, i)*t3c(d, b, a, l, j, m) & !mckidbaljm      (-1.000)
                      + x1(m, b, k, i)*t3c(d, c, a, l, j, m) & !mbkidcaljm      (+1.000)
                      - x1(m, d, l, i)*t3c(c, b, a, k, j, m) & !mdlicbakjm      (-1.000)
                      + x1(m, c, l, i)*t3c(d, b, a, k, j, m) & !mclidbakjm      (+1.000)
                      - x1(m, b, l, i)*t3c(d, c, a, k, j, m)           !mblidcakjm      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x1)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s93), size(s93), '3214', s93, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s94(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s94)
    deallocate (d1)
    deallocate (b2)
    deallocate (s93)

    call sum_stripe(4, shape(x20), size(x20), '3241', -1.000, &
                    x20, s94)

  call sum_shift(4,shape(intm),size(intm),shape(x20), &
   size(x20),(/n0-n0,n2-n0,n1-n0,n0-n0/),'3142',1.000,intm,x20)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4e,x20) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x20(m, e, a, i)*t4e(e, d, c, b, m, l, k, j)      !meaiedcbmlkj    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x20)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s94), size(s94), '4213', s94, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s156(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s156)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x6), size(x6), '2134', 1.000, x6, &
                    s156)
    deallocate (s156)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s94), size(s94), '2413', s94, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s155(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s155)
    deallocate (d1)
    deallocate (b2)
    deallocate (s94)

    call sum_stripe(4, shape(x5), size(x5), '3124', -1.000, &
                    x5, s155)
    deallocate (s155)

  call sum_shift(4,shape(intm),size(intm),shape(x5), &
   size(x5),(/n0-n0,n1-n0,n0-n0,n0-n0/),'2143',1.000,intm,x5)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x5,t3d) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - x5(m, a, j, i)*t3d(d, c, b, m, l, k) & !majidcbmlk      (-1.000)
                      + x5(m, a, k, i)*t3d(d, c, b, m, l, j) & !makidcbmlj      (+1.000)
                      - x5(m, a, l, i)*t3d(d, c, b, m, k, j)           !malidcbmkj      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x5)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s105(n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s105)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s105), size(s105), '2431', s105, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '341256', t3c, f2)
    allocate (u85(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2*k4*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, f2, u85)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x35), size(x35), '234615', &
                    1.000, x35, u85)
    deallocate (u85)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x35,t2b) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x35(m, c, b, l, j, i)*t2b(d, a, k, m) & !mcbljidakm      (+1.000)
                      - x35(m, d, b, l, j, i)*t2b(c, a, k, m) & !mdbljicakm      (-1.000)
                      + x35(m, d, c, l, j, i)*t2b(b, a, k, m) & !mdcljibakm      (+1.000)
                      - x35(m, c, b, k, j, i)*t2b(d, a, l, m) & !mcbkjidalm      (-1.000)
                      + x35(m, d, b, k, j, i)*t2b(c, a, l, m) & !mdbkjicalm      (+1.000)
                      - x35(m, d, c, k, j, i)*t2b(b, a, l, m) & !mdckjibalm      (-1.000)
                      - x35(m, c, b, l, k, i)*t2b(d, a, j, m) & !mcblkidajm      (-1.000)
                      + x35(m, d, b, l, k, i)*t2b(c, a, j, m) & !mdblkicajm      (+1.000)
                      - x35(m, d, c, l, k, i)*t2b(b, a, j, m) & !mdclkibajm      (-1.000)
                      + x35(m, c, b, k, l, i)*t2b(d, a, j, m) & !mcbklidajm      (+1.000)
                      - x35(m, d, b, k, l, i)*t2b(c, a, j, m) & !mdbklicajm      (-1.000)
                      + x35(m, d, c, k, l, i)*t2b(b, a, j, m) & !mdcklibajm      (+1.000)
                      + x35(m, c, b, j, k, i)*t2b(d, a, l, m) & !mcbjkidalm      (+1.000)
                      - x35(m, d, b, j, k, i)*t2b(c, a, l, m) & !mdbjkicalm      (-1.000)
                      + x35(m, d, c, j, k, i)*t2b(b, a, l, m) & !mdcjkibalm      (+1.000)
                      - x35(m, c, b, j, l, i)*t2b(d, a, k, m) & !mcbjlidakm      (-1.000)
                      + x35(m, d, b, j, l, i)*t2b(c, a, k, m) & !mdbjlicakm      (+1.000)
                      - x35(m, d, c, j, l, i)*t2b(b, a, k, m)          !mdcjlibakm      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x35)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s105), size(s105), '3214', s105, d1)
    allocate (f2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '631245', t3c, f2)
    allocate (u82(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k2*k4*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, f2, u82)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x33), size(x33), '234561', &
                    1.000, x33, u82)
    deallocate (u82)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s105), size(s105), '4321', s105, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s129(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s129)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                    x7, s129)
    deallocate (s129)

  call sum_shift(4,shape(intm),size(intm),shape(x7), &
   size(x7),(/n0-n0,n1-n0,n2-n0,n0-n0/),'1324',1.000,intm,x7)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x7,t4c) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x7(m, e, d, j)*t4c(c, b, e, a, l, k, m, i) & !medjcbealkmi    (+1.000)
                      - x7(m, e, c, j)*t4c(d, b, e, a, l, k, m, i) & !mecjdbealkmi    (-1.000)
                      + x7(m, e, b, j)*t4c(d, c, e, a, l, k, m, i) & !mebjdcealkmi    (+1.000)
                      - x7(m, e, d, k)*t4c(c, b, e, a, l, j, m, i) & !medkcbealjmi    (-1.000)
                      + x7(m, e, c, k)*t4c(d, b, e, a, l, j, m, i) & !meckdbealjmi    (+1.000)
                      - x7(m, e, b, k)*t4c(d, c, e, a, l, j, m, i) & !mebkdcealjmi    (-1.000)
                      + x7(m, e, d, l)*t4c(c, b, e, a, k, j, m, i) & !medlcbeakjmi    (+1.000)
                      - x7(m, e, c, l)*t4c(d, b, e, a, k, j, m, i) & !mecldbeakjmi    (-1.000)
                      + x7(m, e, b, l)*t4c(d, c, e, a, k, j, m, i)       !mebldceakjmi    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x7)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s105), size(s105), '3214', s105, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q25(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q25)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                    x10, q25)
    deallocate (q25)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s105), size(s105), '3214', s105, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s106(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s106)
    deallocate (d1)
    deallocate (b2)
    deallocate (s105)

    call sum_stripe(4, shape(x15), size(x15), '3241', -1.000, &
                    x15, s106)

  call sum_shift(4,shape(intm),size(intm),shape(x15), &
   size(x15),(/n0-n0,n1-n0,n1-n0,n0-n0/),'3124',1.000,intm,x15)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x15) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      - x15(m, e, a, j)*t4d(d, c, b, e, m, l, k, i) & !meajdcbemlki    (-1.000)
                      + x15(m, e, a, k)*t4d(d, c, b, e, m, l, j, i) & !meakdcbemlji    (+1.000)
                      - x15(m, e, a, l)*t4d(d, c, b, e, m, k, j, i)      !mealdcbemkji    (-1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x15)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s106), size(s106), '4213', s106, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s158(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s158)
    deallocate (d1)
    deallocate (b2)
    deallocate (s106)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s158)
    deallocate (s158)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s107(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s107)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s107), size(s107), '2431', s107, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q28(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q28)
    deallocate (d1)
    deallocate (b2)

    x9 = x9 - q28
    deallocate (q28)

  call sum_shift(2,shape(fockr),size(fockr),shape(x9), &
   size(x9),(/n1,n1/),'21',1.000,fockr,x9)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x9) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x9(e, a)*t4d(d, c, b, e, l, k, j, i)           !eadcbelkji      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x9)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s107), size(s107), '4231', s107, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s108(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s108)
    deallocate (d1)
    deallocate (b2)
    deallocate (s107)

    call sum_stripe(4, shape(x16), size(x16), '3124', 1.000, &
                    x16, s108)
    deallocate (s108)

  call sum_shift(4,shape(intm),size(intm),shape(x16), &
   size(x16),(/n2-n0,n1-n0,n2-n0,n1-n0/),'4321',1.000,intm,x16)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x16) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do f = n2 + 1, n3
                sum = sum &
                      + x16(f, e, d, a)*t4d(f, c, b, e, l, k, j, i) & !fedafcbelkji    (+1.000)
                      - x16(f, e, c, a)*t4d(f, d, b, e, l, k, j, i) & !fecafdbelkji    (-1.000)
                      + x16(f, e, b, a)*t4d(f, d, c, e, l, k, j, i)      !febafdcelkji    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x16)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q26(n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i3 = k1*k3
    call egemm1(i1, i3, d1, b2, q26)
    deallocate (d1)
    deallocate (b2)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(q26), size(q26), '21', q26, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s121(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s121)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                    x4, s121)
    deallocate (s121)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(q26), size(q26), '21', q26, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s118(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s118)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                    x6, s118)
    deallocate (s118)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(q26), size(q26), '21', q26, b1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q27(n2 + 1:n3, n2 + 1:n3))
    i1 = k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, b1, b2, q27)
    deallocate (b1)
    deallocate (b2)
    deallocate (q26)

    call sum_stripe(2, shape(x11), size(x11), '21', -1.000, &
                    x11, q27)
    deallocate (q27)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s130(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s130)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s130), size(s130), '2431', s130, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '142356', t3d, f2)
    allocate (u88(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k2*k4*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u88)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x33), size(x33), '234516', &
                    1.000, x33, u88)
    deallocate (u88)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t2b,x33) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - x33(n, c, b, l, k, j)*t2b(d, a, n, i) & !ncblkjdani      (-1.000)
                      + x33(n, d, b, l, k, j)*t2b(c, a, n, i) & !ndblkjcani      (+1.000)
                      - x33(n, d, c, l, k, j)*t2b(b, a, n, i) & !ndclkjbani      (-1.000)
                      + x33(n, c, b, l, j, k)*t2b(d, a, n, i) & !ncbljkdani      (+1.000)
                      - x33(n, d, b, l, j, k)*t2b(c, a, n, i) & !ndbljkcani      (-1.000)
                      + x33(n, d, c, l, j, k)*t2b(b, a, n, i) & !ndcljkbani      (+1.000)
                      - x33(n, c, b, k, j, l)*t2b(d, a, n, i) & !ncbkjldani      (-1.000)
                      + x33(n, d, b, k, j, l)*t2b(c, a, n, i) & !ndbkjlcani      (+1.000)
                      - x33(n, d, c, k, j, l)*t2b(b, a, n, i)          !ndckjlbani      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x33)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s130), size(s130), '3214', s130, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q29(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q29)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(2, shape(x10), size(x10), '21', -1.000, &
                    x10, q29)
    deallocate (q29)

  call sum_shift(2,shape(fockb),size(fockb),shape(x10), &
   size(x10),(/n0,n0/),'12',1.000,fockb,x10)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x10) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - x10(m, j)*t4d(d, c, b, a, m, l, k, i) & !mjdcbamlki      (-1.000)
                      + x10(m, k)*t4d(d, c, b, a, m, l, j, i) & !mkdcbamlji      (+1.000)
                      - x10(m, l)*t4d(d, c, b, a, m, k, j, i)          !mldcbamkji      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x10)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s130), size(s130), '3214', s130, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s132(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s132)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x18), size(x18), '3241', -1.000, &
                    x18, s132)

  call sum_shift(4,shape(intb),size(intb),shape(x18), &
   size(x18),(/n0-n0,n2-n0,n2-n0,n0-n0/),'3142',1.000,intb,x18)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x18) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x18(m, e, d, j)*t4d(e, c, b, a, m, l, k, i) & !medjecbamlki    (+1.000)
                      - x18(m, e, c, j)*t4d(e, d, b, a, m, l, k, i) & !mecjedbamlki    (-1.000)
                      + x18(m, e, b, j)*t4d(e, d, c, a, m, l, k, i) & !mebjedcamlki    (+1.000)
                      - x18(m, e, d, k)*t4d(e, c, b, a, m, l, j, i) & !medkecbamlji    (-1.000)
                      + x18(m, e, c, k)*t4d(e, d, b, a, m, l, j, i) & !meckedbamlji    (+1.000)
                      - x18(m, e, b, k)*t4d(e, d, c, a, m, l, j, i) & !mebkedcamlji    (-1.000)
                      + x18(m, e, d, l)*t4d(e, c, b, a, m, k, j, i) & !medlecbamkji    (+1.000)
                      - x18(m, e, c, l)*t4d(e, d, b, a, m, k, j, i) & !mecledbamkji    (-1.000)
                      + x18(m, e, b, l)*t4d(e, d, c, a, m, k, j, i)      !mebledcamkji    (+1.000)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x18)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s132), size(s132), '4213', s132, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s160(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s160)
    deallocate (d1)
    deallocate (b2)
    deallocate (s132)

    call sum_stripe(4, shape(x4), size(x4), '2134', 1.000, x4, &
                    s160)
    deallocate (s160)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s130), size(s130), '2314', s130, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s131(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s131)
    deallocate (d1)
    deallocate (b2)
    deallocate (s130)

    call sum_stripe(4, shape(x17), size(x17), '3241', 1.000, &
                    x17, s131)

  call sum_shift(4,shape(intb),size(intb),shape(x17), &
   size(x17),(/n0-n0,n0-n0,n0-n0,n0-n0/),'2143',1.000,intb,x17)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x17,t4d) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (x17(n, m, k, j)*t4d(d, c, b, a, n, m, l, i) & !nmkjdcbanmli    (+0.500)
                         - x17(n, m, l, j)*t4d(d, c, b, a, n, m, k, i) & !nmljdcbanmki    (-0.500)
                         + x17(n, m, l, k)*t4d(d, c, b, a, n, m, j, i))/2.0d0  !nmlkdcbanmji    (+0.500)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x17)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s131), size(s131), '2413', s131, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u118(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, d1, d2, u118)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x24), size(x24), '236145', &
                    1.000, x24, u118)
    deallocate (u118)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x24,t2c) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - x24(n, d, a, k, j, i)*t2c(c, b, n, l) & !ndakjicbnl      (-1.000)
                      + x24(n, c, a, k, j, i)*t2c(d, b, n, l) & !ncakjidbnl      (+1.000)
                      - x24(n, b, a, k, j, i)*t2c(d, c, n, l) & !nbakjidcnl      (-1.000)
                      + x24(n, d, a, l, j, i)*t2c(c, b, n, k) & !ndaljicbnk      (+1.000)
                      - x24(n, c, a, l, j, i)*t2c(d, b, n, k) & !ncaljidbnk      (-1.000)
                      + x24(n, b, a, l, j, i)*t2c(d, c, n, k) & !nbaljidcnk      (+1.000)
                      - x24(n, b, a, l, k, i)*t2c(d, c, n, j) & !nbalkidcnj      (-1.000)
                      + x24(n, c, a, l, k, i)*t2c(d, b, n, j) & !ncalkidbnj      (+1.000)
                      - x24(n, d, a, l, k, i)*t2c(c, b, n, j)          !ndalkicbnj      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x24)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s131), size(s131), '2413', s131, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s159(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s159)
    deallocate (d1)
    deallocate (b2)
    deallocate (s131)

    call sum_stripe(4, shape(x3), size(x3), '2134', -1.000, &
                    x3, s159)
    deallocate (s159)

  call sum_shift(4,shape(intb),size(intb),shape(x3), &
   size(x3),(/n0-n0,n2-n0,n0-n0,n0-n0/),'2143',1.000,intb,x3)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x3,t3c) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x3(m, d, k, j)*t3c(c, b, a, m, l, i) & !mdkjcbamli      (+1.000)
                      - x3(m, c, k, j)*t3c(d, b, a, m, l, i) & !mckjdbamli      (-1.000)
                      + x3(m, b, k, j)*t3c(d, c, a, m, l, i) & !mbkjdcamli      (+1.000)
                      - x3(m, d, l, j)*t3c(c, b, a, m, k, i) & !mdljcbamki      (-1.000)
                      + x3(m, c, l, j)*t3c(d, b, a, m, k, i) & !mcljdbamki      (+1.000)
                      - x3(m, b, l, j)*t3c(d, c, a, m, k, i) & !mbljdcamki      (-1.000)
                      + x3(m, d, l, k)*t3c(c, b, a, m, j, i) & !mdlkcbamji      (+1.000)
                      - x3(m, c, l, k)*t3c(d, b, a, m, j, i) & !mclkdbamji      (-1.000)
                      + x3(m, b, l, k)*t3c(d, c, a, m, j, i)           !mblkdcamji      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x3)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '1432', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s133(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s133)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s133), size(s133), '3421', s133, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q30(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q30)
    deallocate (d1)
    deallocate (b2)

    x11 = x11 + q30
    deallocate (q30)

  call sum_shift(2,shape(fockb),size(fockb),shape(x11), &
   size(x11),(/n2,n2/),'21',1.000,fockb,x11)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x11,t4d) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      + x11(e, d)*t4d(e, c, b, a, l, k, j, i) & !edecbalkji      (+1.000)
                      - x11(e, c)*t4d(e, d, b, a, l, k, j, i) & !ecedbalkji      (-1.000)
                      + x11(e, b)*t4d(e, d, c, a, l, k, j, i)          !ebedcalkji      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x11)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s133), size(s133), '4231', s133, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s134(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s134)
    deallocate (d1)
    deallocate (b2)
    deallocate (s133)

    call sum_stripe(4, shape(x19), size(x19), '3124', 1.000, &
                    x19, s134)
    deallocate (s134)

  call sum_shift(4,shape(intb),size(intb),shape(x19), &
   size(x19),(/n2-n0,n2-n0,n2-n0,n2-n0/),'4321',1.000,intb,x19)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t4d,x19) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do f = n2 + 1, n3
                sum = sum &
                      + (x19(f, e, d, c)*t4d(f, e, b, a, l, k, j, i) & !fedcfebalkji    (+0.500)
                         - x19(f, e, d, b)*t4d(f, e, c, a, l, k, j, i) & !fedbfecalkji    (-0.500)
                         + x19(f, e, c, b)*t4d(f, e, d, a, l, k, j, i))/2.0d0 !fecbfedalkji    (+0.500)
            end do; end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x19)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q31(n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q31)
    deallocate (d1)
    deallocate (b2)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q31), size(q31), '21', q31, b1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s144(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s144)
    deallocate (b1)
    deallocate (d2)
    deallocate (q31)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s144)
    deallocate (s144)

  call sum_shift(4,shape(intm),size(intm),shape(x2), &
   size(x2),(/n1-n0,n2-n0,n1-n0,n0-n0/),'3214',1.000,intm,x2)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x2,t3c) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x2(e, d, a, j)*t3c(c, b, e, l, k, i) & !edajcbelki      (+1.000)
                      - x2(e, c, a, j)*t3c(d, b, e, l, k, i) & !ecajdbelki      (-1.000)
                      + x2(e, b, a, j)*t3c(d, c, e, l, k, i) & !ebajdcelki      (+1.000)
                      - x2(e, d, a, k)*t3c(c, b, e, l, j, i) & !edakcbelji      (-1.000)
                      + x2(e, c, a, k)*t3c(d, b, e, l, j, i) & !ecakdbelji      (+1.000)
                      - x2(e, b, a, k)*t3c(d, c, e, l, j, i) & !ebakdcelji      (-1.000)
                      + x2(e, d, a, l)*t3c(c, b, e, k, j, i) & !edalcbekji      (+1.000)
                      - x2(e, c, a, l)*t3c(d, b, e, k, j, i) & !ecaldbekji      (-1.000)
                      + x2(e, b, a, l)*t3c(d, c, e, k, j, i)           !ebaldcekji      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q32(n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q32)
    deallocate (d1)
    deallocate (b2)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(q32), size(q32), '21', q32, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s154(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s154)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                    x4, s154)
    deallocate (s154)

  call sum_shift(4,shape(intb),size(intb),shape(x4), &
   size(x4),(/n2-n0,n2-n0,n2-n0,n0-n0/),'3241',1.000,intb,x4)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,x4,t3c) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      - x4(e, d, c, j)*t3c(e, b, a, l, k, i) & !edcjebalki      (-1.000)
                      + x4(e, d, b, j)*t3c(e, c, a, l, k, i) & !edbjecalki      (+1.000)
                      - x4(e, c, b, j)*t3c(e, d, a, l, k, i) & !ecbjedalki      (-1.000)
                      + x4(e, d, c, k)*t3c(e, b, a, l, j, i) & !edckebalji      (+1.000)
                      - x4(e, d, b, k)*t3c(e, c, a, l, j, i) & !edbkecalji      (-1.000)
                      + x4(e, c, b, k)*t3c(e, d, a, l, j, i) & !ecbkedalji      (+1.000)
                      - x4(e, d, c, l)*t3c(e, b, a, k, j, i) & !edclebakji      (-1.000)
                      + x4(e, d, b, l)*t3c(e, c, a, k, j, i) & !edblecakji      (+1.000)
                      - x4(e, c, b, l)*t3c(e, d, a, k, j, i)           !ecbledakji      (-1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x4)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(q32), size(q32), '21', q32, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s148(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s148)
    deallocate (b1)
    deallocate (d2)
    deallocate (q32)

    call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                    x6, s148)
    deallocate (s148)

  call sum_shift(4,shape(intm),size(intm),shape(x6), &
   size(x6),(/n2-n0,n2-n0,n1-n0,n0-n0/),'3241',1.000,intm,x6)

    do i = n0 + 1, n1; do j = n0 + 1, n2 - 2; do k = j + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4d,t3d,x6) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3; do b = n2 + 1, n3 - 2; do c = b + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      + x6(e, d, a, i)*t3d(e, c, b, l, k, j) & !edaiecblkj      (+1.000)
                      - x6(e, c, a, i)*t3d(e, d, b, l, k, j) & !ecaiedblkj      (-1.000)
                      + x6(e, b, a, i)*t3d(e, d, c, l, k, j)           !ebaiedclkj      (+1.000)
            end do
            v4d(d, c, b, a, l, k, j, i) = v4d(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x6)

    do i = n0 + 1, n1
    do j = n0 + 1, n2 - 2
    do k = j + 1, n2 - 1
    do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        do a = n1 + 1, n3
        do b = n2 + 1, n3 - 2
        do c = b + 1, n3 - 1
        do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            coeleft = fockb(d, d) &
                      + fockb(c, c) &
                      + fockb(b, b) &
                      + fockr(a, a) &
                      - fockb(l, l) &
                      - fockb(k, k) &
                      - fockb(j, j) &
                      - fockr(i, i) &
                      + shift
            t4d(d, c, b, a, l, k, j, i) = t4d(d, c, b, a, l, k, j, i) - &
                                          v4d(d, c, b, a, l, k, j, i)/coeleft
            t4d(d, c, b, a, l, j, k, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(d, c, b, a, j, k, l, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(d, c, b, a, j, l, k, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(d, c, b, a, k, l, j, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(d, c, b, a, k, j, l, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(d, b, c, a, l, k, j, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(d, b, c, a, l, j, k, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(d, b, c, a, j, k, l, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(d, b, c, a, j, l, k, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(d, b, c, a, k, l, j, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(d, b, c, a, k, j, l, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(b, c, d, a, l, k, j, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(b, c, d, a, l, j, k, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(b, c, d, a, j, k, l, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(b, c, d, a, j, l, k, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(b, c, d, a, k, l, j, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(b, c, d, a, k, j, l, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(b, d, c, a, l, k, j, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(b, d, c, a, l, j, k, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(b, d, c, a, j, k, l, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(b, d, c, a, j, l, k, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(b, d, c, a, k, l, j, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(b, d, c, a, k, j, l, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(c, d, b, a, l, k, j, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(c, d, b, a, l, j, k, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(c, d, b, a, j, k, l, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(c, d, b, a, j, l, k, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(c, d, b, a, k, l, j, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(c, d, b, a, k, j, l, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(c, b, d, a, l, k, j, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(c, b, d, a, l, j, k, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(c, b, d, a, j, k, l, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(c, b, d, a, j, l, k, i) = t4d(d, c, b, a, l, k, j, i)
            t4d(c, b, d, a, k, l, j, i) = -t4d(d, c, b, a, l, k, j, i)
            t4d(c, b, d, a, k, j, l, i) = t4d(d, c, b, a, l, k, j, i)
        end do
        end do
        end do
        end do
    end do
    end do
    end do
    end do
    rewind (td)
    write (td) t4d

end subroutine t4d_update

