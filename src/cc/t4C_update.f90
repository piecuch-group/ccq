subroutine t4c_update(n0, n1, n2, n3, k1, k2, k3, k4, shift, &
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
    real(kind=8), allocatable::t4a(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::t4b(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::t4c(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::t4d(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::t4e(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::v4c(:, :, :, :, :, :, :, :)

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
    real(kind=8), allocatable::s7(:, :, :, :)
    real(kind=8), allocatable::s8(:, :, :, :)
    real(kind=8), allocatable::s9(:, :, :, :)
    real(kind=8), allocatable::s10(:, :, :, :)
    real(kind=8), allocatable::q1(:, :)
    real(kind=8), allocatable::q2(:, :)
    real(kind=8), allocatable::s11(:, :, :, :)
    real(kind=8), allocatable::u92(:, :, :, :, :, :)
    real(kind=8), allocatable::s109(:, :, :, :)
    real(kind=8), allocatable::s12(:, :, :, :)
    real(kind=8), allocatable::u93(:, :, :, :, :, :)
    real(kind=8), allocatable::s110(:, :, :, :)
    real(kind=8), allocatable::q3(:, :)
    real(kind=8), allocatable::s13(:, :, :, :)
    real(kind=8), allocatable::u94(:, :, :, :, :, :)
    real(kind=8), allocatable::s112(:, :, :, :)
    real(kind=8), allocatable::s111(:, :, :, :)
    real(kind=8), allocatable::s14(:, :, :, :)
    real(kind=8), allocatable::u95(:, :, :, :, :, :)
    real(kind=8), allocatable::q4(:, :)
    real(kind=8), allocatable::s15(:, :, :, :)
    real(kind=8), allocatable::u96(:, :, :, :, :, :)
    real(kind=8), allocatable::u84(:, :, :, :, :, :)
    real(kind=8), allocatable::s122(:, :, :, :)
    real(kind=8), allocatable::s113(:, :, :, :)
    real(kind=8), allocatable::s16(:, :, :, :)
    real(kind=8), allocatable::u97(:, :, :, :, :, :)
    real(kind=8), allocatable::u85(:, :, :, :, :, :)
    real(kind=8), allocatable::s123(:, :, :, :)
    real(kind=8), allocatable::s114(:, :, :, :)
    real(kind=8), allocatable::s17(:, :, :, :)
    real(kind=8), allocatable::u98(:, :, :, :, :, :)
    real(kind=8), allocatable::u86(:, :, :, :, :, :)
    real(kind=8), allocatable::s124(:, :, :, :)
    real(kind=8), allocatable::q5(:, :)
    real(kind=8), allocatable::s18(:, :, :, :)
    real(kind=8), allocatable::u99(:, :, :, :, :, :)
    real(kind=8), allocatable::u87(:, :, :, :, :, :)
    real(kind=8), allocatable::s125(:, :, :, :)
    real(kind=8), allocatable::q6(:, :)
    real(kind=8), allocatable::s19(:, :, :, :)
    real(kind=8), allocatable::u101(:, :, :, :, :, :)
    real(kind=8), allocatable::u100(:, :, :, :, :, :)
    real(kind=8), allocatable::s127(:, :, :, :)
    real(kind=8), allocatable::s126(:, :, :, :)
    real(kind=8), allocatable::s20(:, :, :, :)
    real(kind=8), allocatable::u103(:, :, :, :, :, :)
    real(kind=8), allocatable::u102(:, :, :, :, :, :)
    real(kind=8), allocatable::s129(:, :, :, :)
    real(kind=8), allocatable::s128(:, :, :, :)
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
    real(kind=8), allocatable::s31(:, :, :, :)
    real(kind=8), allocatable::u120(:, :, :, :, :, :)
    real(kind=8), allocatable::u118(:, :, :, :, :, :)
    real(kind=8), allocatable::s32(:, :, :, :)
    real(kind=8), allocatable::u121(:, :, :, :, :, :)
    real(kind=8), allocatable::u119(:, :, :, :, :, :)
    real(kind=8), allocatable::q7(:, :)
    real(kind=8), allocatable::q8(:, :)
    real(kind=8), allocatable::s33(:, :, :, :)
    real(kind=8), allocatable::u130(:, :, :, :, :, :)
    real(kind=8), allocatable::u122(:, :, :, :, :, :)
    real(kind=8), allocatable::s158(:, :, :, :)
    real(kind=8), allocatable::s34(:, :, :, :)
    real(kind=8), allocatable::u131(:, :, :, :, :, :)
    real(kind=8), allocatable::u123(:, :, :, :, :, :)
    real(kind=8), allocatable::q9(:, :)
    real(kind=8), allocatable::s35(:, :, :, :)
    real(kind=8), allocatable::u132(:, :, :, :, :, :)
    real(kind=8), allocatable::u124(:, :, :, :, :, :)
    real(kind=8), allocatable::s159(:, :, :, :)
    real(kind=8), allocatable::s36(:, :, :, :)
    real(kind=8), allocatable::u133(:, :, :, :, :, :)
    real(kind=8), allocatable::u125(:, :, :, :, :, :)
    real(kind=8), allocatable::q10(:, :)
    real(kind=8), allocatable::s37(:, :, :, :)
    real(kind=8), allocatable::u134(:, :, :, :, :, :)
    real(kind=8), allocatable::s160(:, :, :, :)
    real(kind=8), allocatable::s38(:, :, :, :)
    real(kind=8), allocatable::u135(:, :, :, :, :, :)
    real(kind=8), allocatable::s161(:, :, :, :)
    real(kind=8), allocatable::q11(:, :)
    real(kind=8), allocatable::s39(:, :, :, :)
    real(kind=8), allocatable::u136(:, :, :, :, :, :)
    real(kind=8), allocatable::s163(:, :, :, :)
    real(kind=8), allocatable::s162(:, :, :, :)
    real(kind=8), allocatable::s40(:, :, :, :)
    real(kind=8), allocatable::u137(:, :, :, :, :, :)
    real(kind=8), allocatable::q12(:, :)
    real(kind=8), allocatable::u1(:, :, :, :, :, :)
    real(kind=8), allocatable::u2(:, :, :, :, :, :)
    real(kind=8), allocatable::u3(:, :, :, :, :, :)
    real(kind=8), allocatable::u4(:, :, :, :, :, :)
    real(kind=8), allocatable::u5(:, :, :, :, :, :)
    real(kind=8), allocatable::u6(:, :, :, :, :, :)
    real(kind=8), allocatable::s41(:, :, :, :)
    real(kind=8), allocatable::s42(:, :, :, :)
    real(kind=8), allocatable::u7(:, :, :, :, :, :)
    real(kind=8), allocatable::s43(:, :, :, :)
    real(kind=8), allocatable::s44(:, :, :, :)
    real(kind=8), allocatable::u8(:, :, :, :, :, :)
    real(kind=8), allocatable::s45(:, :, :, :)
    real(kind=8), allocatable::s46(:, :, :, :)
    real(kind=8), allocatable::u9(:, :, :, :, :, :)
    real(kind=8), allocatable::u10(:, :, :, :, :, :)
    real(kind=8), allocatable::u11(:, :, :, :, :, :)
    real(kind=8), allocatable::s47(:, :, :, :)
    real(kind=8), allocatable::u12(:, :, :, :, :, :)
    real(kind=8), allocatable::s48(:, :, :, :)
    real(kind=8), allocatable::u13(:, :, :, :, :, :)
    real(kind=8), allocatable::u14(:, :, :, :, :, :)
    real(kind=8), allocatable::u15(:, :, :, :, :, :)
    real(kind=8), allocatable::u152(:, :, :, :, :, :)
    real(kind=8), allocatable::s139(:, :, :, :)
    real(kind=8), allocatable::u89(:, :, :, :, :, :)
    real(kind=8), allocatable::s49(:, :, :, :)
    real(kind=8), allocatable::u153(:, :, :, :, :, :)
    real(kind=8), allocatable::s138(:, :, :, :)
    real(kind=8), allocatable::u16(:, :, :, :, :, :)
    real(kind=8), allocatable::s50(:, :, :, :)
    real(kind=8), allocatable::u155(:, :, :, :, :, :)
    real(kind=8), allocatable::s140(:, :, :, :)
    real(kind=8), allocatable::s136(:, :, :, :)
    real(kind=8), allocatable::q13(:, :)
    real(kind=8), allocatable::s51(:, :, :, :)
    real(kind=8), allocatable::u156(:, :, :, :, :, :)
    real(kind=8), allocatable::s137(:, :, :, :)
    real(kind=8), allocatable::q14(:, :)
    real(kind=8), allocatable::u17(:, :, :, :, :, :)
    real(kind=8), allocatable::u159(:, :, :, :, :, :)
    real(kind=8), allocatable::u158(:, :, :, :, :, :)
    real(kind=8), allocatable::u157(:, :, :, :, :, :)
    real(kind=8), allocatable::s170(:, :, :, :)
    real(kind=8), allocatable::u127(:, :, :, :, :, :)
    real(kind=8), allocatable::u126(:, :, :, :, :, :)
    real(kind=8), allocatable::u189(:, :, :, :, :, :)
    real(kind=8), allocatable::u91(:, :, :, :, :, :)
    real(kind=8), allocatable::u184(:, :, :, :, :, :)
    real(kind=8), allocatable::u183(:, :, :, :, :, :)
    real(kind=8), allocatable::u18(:, :, :, :, :, :)
    real(kind=8), allocatable::s52(:, :, :, :)
    real(kind=8), allocatable::u164(:, :, :, :, :, :)
    real(kind=8), allocatable::u162(:, :, :, :, :, :)
    real(kind=8), allocatable::s172(:, :, :, :)
    real(kind=8), allocatable::s171(:, :, :, :)
    real(kind=8), allocatable::u19(:, :, :, :, :, :)
    real(kind=8), allocatable::u20(:, :, :, :, :, :)
    real(kind=8), allocatable::u21(:, :, :, :, :, :)
    real(kind=8), allocatable::u22(:, :, :, :, :, :)
    real(kind=8), allocatable::u23(:, :, :, :, :, :)
    real(kind=8), allocatable::u24(:, :, :, :, :, :)
    real(kind=8), allocatable::u25(:, :, :, :, :, :)
    real(kind=8), allocatable::u26(:, :, :, :, :, :)
    real(kind=8), allocatable::u27(:, :, :, :, :, :)
    real(kind=8), allocatable::u28(:, :, :, :, :, :)
    real(kind=8), allocatable::u29(:, :, :, :, :, :)
    real(kind=8), allocatable::u30(:, :, :, :, :, :)
    real(kind=8), allocatable::s53(:, :, :, :)
    real(kind=8), allocatable::s54(:, :, :, :)
    real(kind=8), allocatable::u31(:, :, :, :, :, :)
    real(kind=8), allocatable::u32(:, :, :, :, :, :)
    real(kind=8), allocatable::s55(:, :, :, :)
    real(kind=8), allocatable::u33(:, :, :, :, :, :)
    real(kind=8), allocatable::u34(:, :, :, :, :, :)
    real(kind=8), allocatable::s56(:, :, :, :)
    real(kind=8), allocatable::u35(:, :, :, :, :, :)
    real(kind=8), allocatable::u36(:, :, :, :, :, :)
    real(kind=8), allocatable::s57(:, :, :, :)
    real(kind=8), allocatable::u37(:, :, :, :, :, :)
    real(kind=8), allocatable::u38(:, :, :, :, :, :)
    real(kind=8), allocatable::s58(:, :, :, :)
    real(kind=8), allocatable::u39(:, :, :, :, :, :)
    real(kind=8), allocatable::s59(:, :, :, :)
    real(kind=8), allocatable::s60(:, :, :, :)
    real(kind=8), allocatable::u40(:, :, :, :, :, :)
    real(kind=8), allocatable::s61(:, :, :, :)
    real(kind=8), allocatable::s62(:, :, :, :)
    real(kind=8), allocatable::s63(:, :, :, :)
    real(kind=8), allocatable::s64(:, :, :, :)
    real(kind=8), allocatable::u41(:, :, :, :, :, :)
    real(kind=8), allocatable::u42(:, :, :, :, :, :)
    real(kind=8), allocatable::s65(:, :, :, :)
    real(kind=8), allocatable::u43(:, :, :, :, :, :)
    real(kind=8), allocatable::s66(:, :, :, :)
    real(kind=8), allocatable::s67(:, :, :, :)
    real(kind=8), allocatable::u44(:, :, :, :, :, :)
    real(kind=8), allocatable::u45(:, :, :, :, :, :)
    real(kind=8), allocatable::s68(:, :, :, :)
    real(kind=8), allocatable::u46(:, :, :, :, :, :)
    real(kind=8), allocatable::s69(:, :, :, :)
    real(kind=8), allocatable::s70(:, :, :, :)
    real(kind=8), allocatable::u47(:, :, :, :, :, :)
    real(kind=8), allocatable::u48(:, :, :, :, :, :)
    real(kind=8), allocatable::s71(:, :, :, :)
    real(kind=8), allocatable::u49(:, :, :, :, :, :)
    real(kind=8), allocatable::u50(:, :, :, :, :, :)
    real(kind=8), allocatable::s72(:, :, :, :)
    real(kind=8), allocatable::u51(:, :, :, :, :, :)
    real(kind=8), allocatable::u154(:, :, :, :, :, :)
    real(kind=8), allocatable::s143(:, :, :, :)
    real(kind=8), allocatable::u106(:, :, :, :, :, :)
    real(kind=8), allocatable::u178(:, :, :, :, :, :)
    real(kind=8), allocatable::u104(:, :, :, :, :, :)
    real(kind=8), allocatable::u177(:, :, :, :, :, :)
    real(kind=8), allocatable::u52(:, :, :, :, :, :)
    real(kind=8), allocatable::s73(:, :, :, :)
    real(kind=8), allocatable::s144(:, :, :, :)
    real(kind=8), allocatable::s142(:, :, :, :)
    real(kind=8), allocatable::u53(:, :, :, :, :, :)
    real(kind=8), allocatable::u167(:, :, :, :, :, :)
    real(kind=8), allocatable::u166(:, :, :, :, :, :)
    real(kind=8), allocatable::u165(:, :, :, :, :, :)
    real(kind=8), allocatable::u160(:, :, :, :, :, :)
    real(kind=8), allocatable::s176(:, :, :, :)
    real(kind=8), allocatable::u140(:, :, :, :, :, :)
    real(kind=8), allocatable::u138(:, :, :, :, :, :)
    real(kind=8), allocatable::u190(:, :, :, :, :, :)
    real(kind=8), allocatable::u111(:, :, :, :, :, :)
    real(kind=8), allocatable::u188(:, :, :, :, :, :)
    real(kind=8), allocatable::u187(:, :, :, :, :, :)
    real(kind=8), allocatable::u54(:, :, :, :, :, :)
    real(kind=8), allocatable::u169(:, :, :, :, :, :)
    real(kind=8), allocatable::u168(:, :, :, :, :, :)
    real(kind=8), allocatable::u141(:, :, :, :, :, :)
    real(kind=8), allocatable::s150(:, :, :, :)
    real(kind=8), allocatable::u112(:, :, :, :, :, :)
    real(kind=8), allocatable::u108(:, :, :, :, :, :)
    real(kind=8), allocatable::u186(:, :, :, :, :, :)
    real(kind=8), allocatable::u179(:, :, :, :, :, :)
    real(kind=8), allocatable::s74(:, :, :, :)
    real(kind=8), allocatable::u170(:, :, :, :, :, :)
    real(kind=8), allocatable::u161(:, :, :, :, :, :)
    real(kind=8), allocatable::s175(:, :, :, :)
    real(kind=8), allocatable::s149(:, :, :, :)
    real(kind=8), allocatable::u55(:, :, :, :, :, :)
    real(kind=8), allocatable::s75(:, :, :, :)
    real(kind=8), allocatable::u163(:, :, :, :, :, :)
    real(kind=8), allocatable::s174(:, :, :, :)
    real(kind=8), allocatable::s151(:, :, :, :)
    real(kind=8), allocatable::s76(:, :, :, :)
    real(kind=8), allocatable::s153(:, :, :, :)
    real(kind=8), allocatable::s146(:, :, :, :)
    real(kind=8), allocatable::q15(:, :)
    real(kind=8), allocatable::u56(:, :, :, :, :, :)
    real(kind=8), allocatable::s77(:, :, :, :)
    real(kind=8), allocatable::s179(:, :, :, :)
    real(kind=8), allocatable::s177(:, :, :, :)
    real(kind=8), allocatable::s78(:, :, :, :)
    real(kind=8), allocatable::s180(:, :, :, :)
    real(kind=8), allocatable::s147(:, :, :, :)
    real(kind=8), allocatable::q16(:, :)
    real(kind=8), allocatable::s79(:, :, :, :)
    real(kind=8), allocatable::s178(:, :, :, :)
    real(kind=8), allocatable::s148(:, :, :, :)
    real(kind=8), allocatable::q17(:, :)
    real(kind=8), allocatable::q18(:, :)
    real(kind=8), allocatable::u57(:, :, :, :, :, :)
    real(kind=8), allocatable::u173(:, :, :, :, :, :)
    real(kind=8), allocatable::u172(:, :, :, :, :, :)
    real(kind=8), allocatable::u171(:, :, :, :, :, :)
    real(kind=8), allocatable::s183(:, :, :, :)
    real(kind=8), allocatable::u146(:, :, :, :, :, :)
    real(kind=8), allocatable::u193(:, :, :, :, :, :)
    real(kind=8), allocatable::u144(:, :, :, :, :, :)
    real(kind=8), allocatable::u192(:, :, :, :, :, :)
    real(kind=8), allocatable::u58(:, :, :, :, :, :)
    real(kind=8), allocatable::s80(:, :, :, :)
    real(kind=8), allocatable::u175(:, :, :, :, :, :)
    real(kind=8), allocatable::s184(:, :, :, :)
    real(kind=8), allocatable::s182(:, :, :, :)
    real(kind=8), allocatable::u59(:, :, :, :, :, :)
    real(kind=8), allocatable::u60(:, :, :, :, :, :)
    real(kind=8), allocatable::s81(:, :, :, :)
    real(kind=8), allocatable::s82(:, :, :, :)
    real(kind=8), allocatable::u61(:, :, :, :, :, :)
    real(kind=8), allocatable::u62(:, :, :, :, :, :)
    real(kind=8), allocatable::s83(:, :, :, :)
    real(kind=8), allocatable::u63(:, :, :, :, :, :)
    real(kind=8), allocatable::u64(:, :, :, :, :, :)
    real(kind=8), allocatable::s84(:, :, :, :)
    real(kind=8), allocatable::u65(:, :, :, :, :, :)
    real(kind=8), allocatable::s85(:, :, :, :)
    real(kind=8), allocatable::s86(:, :, :, :)
    real(kind=8), allocatable::u66(:, :, :, :, :, :)
    real(kind=8), allocatable::s87(:, :, :, :)
    real(kind=8), allocatable::s88(:, :, :, :)
    real(kind=8), allocatable::u67(:, :, :, :, :, :)
    real(kind=8), allocatable::u149(:, :, :, :, :, :)
    real(kind=8), allocatable::s155(:, :, :, :)
    real(kind=8), allocatable::u116(:, :, :, :, :, :)
    real(kind=8), allocatable::u114(:, :, :, :, :, :)
    real(kind=8), allocatable::u182(:, :, :, :, :, :)
    real(kind=8), allocatable::u180(:, :, :, :, :, :)
    real(kind=8), allocatable::u68(:, :, :, :, :, :)
    real(kind=8), allocatable::s89(:, :, :, :)
    real(kind=8), allocatable::s157(:, :, :, :)
    real(kind=8), allocatable::s154(:, :, :, :)
    real(kind=8), allocatable::u69(:, :, :, :, :, :)
    real(kind=8), allocatable::s189(:, :, :, :)
    real(kind=8), allocatable::u151(:, :, :, :, :, :)
    real(kind=8), allocatable::s90(:, :, :, :)
    real(kind=8), allocatable::u174(:, :, :, :, :, :)
    real(kind=8), allocatable::s188(:, :, :, :)
    real(kind=8), allocatable::u70(:, :, :, :, :, :)
    real(kind=8), allocatable::s91(:, :, :, :)
    real(kind=8), allocatable::s190(:, :, :, :)
    real(kind=8), allocatable::s186(:, :, :, :)
    real(kind=8), allocatable::q19(:, :)
    real(kind=8), allocatable::s92(:, :, :, :)
    real(kind=8), allocatable::s187(:, :, :, :)
    real(kind=8), allocatable::q20(:, :)
    real(kind=8), allocatable::u71(:, :, :, :, :, :)
    real(kind=8), allocatable::s93(:, :, :, :)
    real(kind=8), allocatable::s94(:, :, :, :)
    real(kind=8), allocatable::u72(:, :, :, :, :, :)
    real(kind=8), allocatable::u148(:, :, :, :, :, :)
    real(kind=8), allocatable::s95(:, :, :, :)
    real(kind=8), allocatable::u73(:, :, :, :, :, :)
    real(kind=8), allocatable::u107(:, :, :, :, :, :)
    real(kind=8), allocatable::s96(:, :, :, :)
    real(kind=8), allocatable::u74(:, :, :, :, :, :)
    real(kind=8), allocatable::u105(:, :, :, :, :, :)
    real(kind=8), allocatable::s97(:, :, :, :)
    real(kind=8), allocatable::s98(:, :, :, :)
    real(kind=8), allocatable::u75(:, :, :, :, :, :)
    real(kind=8), allocatable::u143(:, :, :, :, :, :)
    real(kind=8), allocatable::u117(:, :, :, :, :, :)
    real(kind=8), allocatable::s99(:, :, :, :)
    real(kind=8), allocatable::u76(:, :, :, :, :, :)
    real(kind=8), allocatable::u142(:, :, :, :, :, :)
    real(kind=8), allocatable::u77(:, :, :, :, :, :)
    real(kind=8), allocatable::s100(:, :, :, :)
    real(kind=8), allocatable::s101(:, :, :, :)
    real(kind=8), allocatable::u78(:, :, :, :, :, :)
    real(kind=8), allocatable::u139(:, :, :, :, :, :)
    real(kind=8), allocatable::u79(:, :, :, :, :, :)
    real(kind=8), allocatable::u115(:, :, :, :, :, :)
    real(kind=8), allocatable::s102(:, :, :, :)
    real(kind=8), allocatable::u80(:, :, :, :, :, :)
    real(kind=8), allocatable::u129(:, :, :, :, :, :)
    real(kind=8), allocatable::u113(:, :, :, :, :, :)
    real(kind=8), allocatable::s103(:, :, :, :)
    real(kind=8), allocatable::s104(:, :, :, :)
    real(kind=8), allocatable::s105(:, :, :, :)
    real(kind=8), allocatable::s106(:, :, :, :)
    real(kind=8), allocatable::u81(:, :, :, :, :, :)
    real(kind=8), allocatable::u150(:, :, :, :, :, :)
    real(kind=8), allocatable::s107(:, :, :, :)
    real(kind=8), allocatable::u82(:, :, :, :, :, :)
    real(kind=8), allocatable::u147(:, :, :, :, :, :)
    real(kind=8), allocatable::s108(:, :, :, :)
    real(kind=8), allocatable::u83(:, :, :, :, :, :)
    real(kind=8), allocatable::u145(:, :, :, :, :, :)
    real(kind=8), allocatable::s115(:, :, :, :)
    real(kind=8), allocatable::u88(:, :, :, :, :, :)
    real(kind=8), allocatable::q21(:, :)
    real(kind=8), allocatable::s117(:, :, :, :)
    real(kind=8), allocatable::s193(:, :, :, :)
    real(kind=8), allocatable::s116(:, :, :, :)
    real(kind=8), allocatable::u176(:, :, :, :, :, :)
    real(kind=8), allocatable::s192(:, :, :, :)
    real(kind=8), allocatable::s118(:, :, :, :)
    real(kind=8), allocatable::q22(:, :)
    real(kind=8), allocatable::s119(:, :, :, :)
    real(kind=8), allocatable::s120(:, :, :, :)
    real(kind=8), allocatable::u110(:, :, :, :, :, :)
    real(kind=8), allocatable::u109(:, :, :, :, :, :)
    real(kind=8), allocatable::u90(:, :, :, :, :, :)
    real(kind=8), allocatable::q23(:, :)
    real(kind=8), allocatable::s131(:, :, :, :)
    real(kind=8), allocatable::s130(:, :, :, :)
    real(kind=8), allocatable::u185(:, :, :, :, :, :)
    real(kind=8), allocatable::u181(:, :, :, :, :, :)
    real(kind=8), allocatable::s196(:, :, :, :)
    real(kind=8), allocatable::s121(:, :, :, :)
    real(kind=8), allocatable::s195(:, :, :, :)
    real(kind=8), allocatable::s194(:, :, :, :)
    real(kind=8), allocatable::s132(:, :, :, :)
    real(kind=8), allocatable::u128(:, :, :, :, :, :)
    real(kind=8), allocatable::s164(:, :, :, :)
    real(kind=8), allocatable::q24(:, :)
    real(kind=8), allocatable::s133(:, :, :, :)
    real(kind=8), allocatable::s197(:, :, :, :)
    real(kind=8), allocatable::s134(:, :, :, :)
    real(kind=8), allocatable::q27(:, :)
    real(kind=8), allocatable::s135(:, :, :, :)
    real(kind=8), allocatable::q25(:, :)
    real(kind=8), allocatable::s156(:, :, :, :)
    real(kind=8), allocatable::s152(:, :, :, :)
    real(kind=8), allocatable::q26(:, :)
    real(kind=8), allocatable::q28(:, :)
    real(kind=8), allocatable::s145(:, :, :, :)
    real(kind=8), allocatable::s141(:, :, :, :)
    real(kind=8), allocatable::s165(:, :, :, :)
    real(kind=8), allocatable::q29(:, :)
    real(kind=8), allocatable::s167(:, :, :, :)
    real(kind=8), allocatable::s199(:, :, :, :)
    real(kind=8), allocatable::s166(:, :, :, :)
    real(kind=8), allocatable::u191(:, :, :, :, :, :)
    real(kind=8), allocatable::s198(:, :, :, :)
    real(kind=8), allocatable::s168(:, :, :, :)
    real(kind=8), allocatable::q30(:, :)
    real(kind=8), allocatable::s169(:, :, :, :)
    real(kind=8), allocatable::q31(:, :)
    real(kind=8), allocatable::s181(:, :, :, :)
    real(kind=8), allocatable::s173(:, :, :, :)
    real(kind=8), allocatable::q32(:, :)
    real(kind=8), allocatable::s191(:, :, :, :)
    real(kind=8), allocatable::s185(:, :, :, :)
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
    real(kind=8), allocatable::x8(:, :, :, :)
    real(kind=8), allocatable::z8(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x9(:, :, :, :)
    real(kind=8), allocatable::z9(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x10(:, :)
    real(kind=8), allocatable::z10(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x11(:, :)
    real(kind=8), allocatable::z11(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x12(:, :)
    real(kind=8), allocatable::z12(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x13(:, :)
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
    real(kind=8), allocatable::x21(:, :, :, :)
    real(kind=8), allocatable::z21(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x22(:, :, :, :)
    real(kind=8), allocatable::z22(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x23(:, :, :, :)
    real(kind=8), allocatable::z23(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x24(:, :, :, :)
    real(kind=8), allocatable::z24(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x25(:, :, :, :)
    real(kind=8), allocatable::z28(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x26(:, :, :, :)
    real(kind=8), allocatable::z29(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z278(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z37(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x27(:, :, :, :, :, :)
    real(kind=8), allocatable::z279(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z281(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z41(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x28(:, :, :, :, :, :)
    real(kind=8), allocatable::z282(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x29(:, :, :, :, :, :)
    real(kind=8), allocatable::z264(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x30(:, :, :, :, :, :)
    real(kind=8), allocatable::z265(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x31(:, :, :, :, :, :)
    real(kind=8), allocatable::z266(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x32(:, :, :, :, :, :)
    real(kind=8), allocatable::z285(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x33(:, :, :, :, :, :)
    real(kind=8), allocatable::z267(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x34(:, :, :, :, :, :)
    real(kind=8), allocatable::z287(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x35(:, :, :, :, :, :)
    real(kind=8), allocatable::z286(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x36(:, :, :, :)
    real(kind=8), allocatable::z56(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x37(:, :, :, :)
    real(kind=8), allocatable::z57(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x38(:, :, :, :, :, :)
    real(kind=8), allocatable::z334(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x39(:, :, :, :, :, :)
    real(kind=8), allocatable::z332(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x40(:, :, :, :, :, :)
    real(kind=8), allocatable::z350(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z352(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z71(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z355(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z75(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x41(:, :, :, :, :, :)
    real(kind=8), allocatable::z85(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x42(:, :, :, :, :, :)
    real(kind=8), allocatable::z88(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x43(:, :, :, :, :, :)
    real(kind=8), allocatable::z91(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x44(:, :, :, :, :, :)
    real(kind=8), allocatable::z92(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x45(:, :, :, :, :, :)
    real(kind=8), allocatable::z93(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x46(:, :, :, :, :, :)
    real(kind=8), allocatable::z98(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z99(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x47(:, :, :, :, :, :)
    real(kind=8), allocatable::z389(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x48(:, :, :, :, :, :)
    real(kind=8), allocatable::z392(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z106(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x49(:, :, :, :, :, :)
    real(kind=8), allocatable::z116(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x50(:, :, :, :, :, :)
    real(kind=8), allocatable::z118(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x51(:, :, :, :, :, :)
    real(kind=8), allocatable::z123(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x52(:, :, :, :, :, :)
    real(kind=8), allocatable::z124(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x53(:, :, :, :, :, :)
    real(kind=8), allocatable::z126(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x54(:, :, :, :, :, :)
    real(kind=8), allocatable::z129(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x55(:, :, :, :, :, :)
    real(kind=8), allocatable::z132(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x56(:, :, :, :, :, :)
    real(kind=8), allocatable::z143(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x57(:, :, :, :, :, :)
    real(kind=8), allocatable::z149(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x58(:, :, :, :, :, :)
    real(kind=8), allocatable::z150(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x59(:, :, :, :, :, :)
    real(kind=8), allocatable::z152(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x60(:, :, :, :, :, :)
    real(kind=8), allocatable::z155(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z161(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x61(:, :, :, :, :, :)
    real(kind=8), allocatable::z401(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z164(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z165(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z178(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x62(:, :, :, :, :, :)
    real(kind=8), allocatable::z181(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x63(:, :, :, :, :, :)
    real(kind=8), allocatable::z182(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x64(:, :, :, :, :, :)
    real(kind=8), allocatable::z185(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x65(:, :, :, :, :, :)
    real(kind=8), allocatable::z188(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z189(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x66(:, :, :, :, :, :)
    real(kind=8), allocatable::z194(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z197(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z200(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z202(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x67(:, :, :, :, :, :)
    real(kind=8), allocatable::z207(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::x68(:, :, :, :, :, :)
    real(kind=8), allocatable::z210(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z211(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z295(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z212(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z214(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z216(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z218(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z224(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z232(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z233(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z234(:, :, :, :, :, :, :, :)
    real(kind=8), allocatable::z235(:, :, :, :, :, :, :, :)

    allocate (t4b(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    allocate (t4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    allocate (t4d(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, &
                  n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))

    allocate (indocc(n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    allocate (indunocc(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    allocate (v4c(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, &
                  n0 + 1:n1, n0 + 1:n1))

    indocc = 0
    indunocc = 0
    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        iocca = 0
        ioccb = 0
        if (i .gt. (n1 - iactocca)) iocca = iocca + 1
        if (j .gt. (n1 - iactocca)) iocca = iocca + 1
        if (k .gt. (n2 - iactoccb)) ioccb = ioccb + 1
        if (l .gt. (n2 - iactoccb)) ioccb = ioccb + 1
        if (iocca + ioccb .lt. iactindq) indocc(l, k, j, i) = 1
    end do; end do; end do; end do
    do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
        iunoa = 0
        iunob = 0
        if (a .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (b .lt. (n1 + iactunoa + 1)) iunoa = iunoa + 1
        if (c .lt. (n2 + iactunob + 1)) iunob = iunob + 1
        if (d .lt. (n2 + iactunob + 1)) iunob = iunob + 1
        if (iunoa + iunob .lt. iactindq) indunocc(d, c, b, a) = 1
    end do; end do; end do; end do

    rewind (tb)
    rewind (tc)
    rewind (td)
    read (tb) t4b
    read (tc) t4c
    read (td) t4d

    v4c = 0.0d0

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

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s3(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s3)
    deallocate (d1)
    deallocate (b2)

    allocate (x5(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x5 = 0.0d0
    call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                    x5, s3)
    deallocate (s3)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s4(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s4)
    deallocate (d1)
    deallocate (b2)

    allocate (x25(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x25 = 0.0d0
    call sum_stripe(4, shape(x25), size(x25), '3124', 1.000, &
                    x25, s4)
    deallocate (s4)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '2413', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s5(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s5)
    deallocate (d1)
    deallocate (b2)

    allocate (x26(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x26 = 0.0d0
    call sum_stripe(4, shape(x26), size(x26), '3124', 1.000, &
                    x26, s5)
    deallocate (s5)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '3421', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s6(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s6)
    deallocate (d1)
    deallocate (b2)

    allocate (x6(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x6 = 0.0d0
    call sum_stripe(4, shape(x6), size(x6), '4123', 1.000, x6, &
                    s6)
    deallocate (s6)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s7(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s7)
    deallocate (d1)
    deallocate (b2)

    allocate (x7(n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x7 = 0.0d0
    call sum_stripe(4, shape(x7), size(x7), '2134', -1.000, &
                    x7, s7)
    deallocate (s7)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '1423', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s8(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s8)
    deallocate (d1)
    deallocate (b2)

    allocate (x8(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x8 = 0.0d0
    call sum_stripe(4, shape(x8), size(x8), '3124', -1.000, &
                    x8, s8)
    deallocate (s8)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s9(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s9)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x7), size(x7), '4123', 1.000, x7, &
                    s9)
    deallocate (s9)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n1 - n0/), '3421', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s10(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s10)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '4123', 1.000, x8, &
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

    allocate (x10(n0 + 1:n1, n0 + 1:n1))
    x10 = 0.0d0
    call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                    x10, q1)
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

    allocate (x11(n1 + 1:n3, n1 + 1:n3))
    x11 = 0.0d0
    call sum_stripe(2, shape(x11), size(x11), '21', -1.000, &
                    x11, q2)
    deallocate (q2)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s11(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s11)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,s11) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (s11(j, m, i, n)*t4c(d, c, b, a, l, k, n, m) & !jmindcbalknm    (+0.500)
                         - s11(i, m, j, n)*t4c(d, c, b, a, l, k, n, m))/2.0d0 !imjndcbalknm    (-0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s11), size(s11), '2413', s11, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u92(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u92)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t2b,u92) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      - u92(c, a, l, n, j, i)*t2b(d, b, k, n) & !calnjidbkn      (-1.000)
                      - u92(d, a, k, n, j, i)*t2b(c, b, l, n) & !daknjicbln      (-1.000)
                      + u92(d, a, l, n, j, i)*t2b(c, b, k, n) & !dalnjicbkn      (+1.000)
                      + u92(c, a, k, n, j, i)*t2b(d, b, l, n) & !caknjidbln      (+1.000)
                      + u92(c, a, l, n, i, j)*t2b(d, b, k, n) & !calnijdbkn      (+1.000)
                      + u92(d, a, k, n, i, j)*t2b(c, b, l, n) & !daknijcbln      (+1.000)
                      - u92(d, a, l, n, i, j)*t2b(c, b, k, n) & !dalnijcbkn      (-1.000)
                      - u92(c, a, k, n, i, j)*t2b(d, b, l, n)          !caknijdbln      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u92)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s11), size(s11), '2413', s11, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s109(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s109)
    deallocate (d1)
    deallocate (b2)
    deallocate (s11)

    call sum_stripe(4, shape(x25), size(x25), '2134', -1.000, &
                    x25, s109)
    deallocate (s109)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s12(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s12)
    deallocate (d1)
    deallocate (b2)

    allocate (x15(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x15 = 0.0d0
    call sum_stripe(4, shape(x15), size(x15), '3241', -1.000, &
                    x15, s12)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s12), size(s12), '2413', s12, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u93(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u93)
    deallocate (d1)
    deallocate (d2)

    allocate (x27(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x27 = 0.0d0
    call sum_stripe(6, shape(x27), size(x27), '245136', &
                    1.000, x27, u93)
    deallocate (u93)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s12), size(s12), '4213', s12, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s110(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s110)
    deallocate (d1)
    deallocate (b2)
    deallocate (s12)

    call sum_stripe(4, shape(x6), size(x6), '2134', 1.000, x6, &
                    s110)
    deallocate (s110)

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

    call sum_stripe(2, shape(x10), size(x10), '21', -1.000, &
                    x10, q3)
    deallocate (q3)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s13(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s13)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x15), size(x15), '4231', 1.000, &
                    x15, s13)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '2431', s13, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u94(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u94)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '245136', &
                    -1.000, x27, u94)
    deallocate (u94)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '4231', s13, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s112(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s112)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x26), size(x26), '3124', 1.000, &
                    x26, s112)
    deallocate (s112)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s13), size(s13), '2431', s13, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s111(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s111)
    deallocate (d1)
    deallocate (b2)
    deallocate (s13)

    call sum_stripe(4, shape(x5), size(x5), '3124', 1.000, x5, &
                    s111)
    deallocate (s111)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n1 - n0/), '2431', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s14(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s14)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,s14) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do f = n1 + 1, n3
                sum = sum &
                      + (s14(a, f, e, b)*t4c(d, c, f, e, l, k, j, i) & !afebdcfelkji    (+0.500)
                         - s14(b, f, e, a)*t4c(d, c, f, e, l, k, j, i))/2.0d0 !bfeadcfelkji    (-0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s14), size(s14), '3241', s14, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u95(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u95)
    deallocate (d1)
    deallocate (d2)
    deallocate (s14)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,u95,t2b) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum &
                      - u95(d, k, i, f, b, a)*t2b(c, f, l, j) & !dkifbacflj      (-1.000)
                      + u95(d, k, i, f, a, b)*t2b(c, f, l, j) & !dkifabcflj      (+1.000)
                      + u95(c, k, i, f, b, a)*t2b(d, f, l, j) & !ckifbadflj      (+1.000)
                      - u95(c, k, i, f, a, b)*t2b(d, f, l, j) & !ckifabdflj      (-1.000)
                      + u95(d, l, i, f, b, a)*t2b(c, f, k, j) & !dlifbacfkj      (+1.000)
                      - u95(d, l, i, f, a, b)*t2b(c, f, k, j) & !dlifabcfkj      (-1.000)
                      - u95(c, l, i, f, b, a)*t2b(d, f, k, j) & !clifbadfkj      (-1.000)
                      + u95(c, l, i, f, a, b)*t2b(d, f, k, j)          !clifabdfkj      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u95)

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

    x11 = x11 - q4
    deallocate (q4)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3142', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s15(n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s15)
    deallocate (d1)
    deallocate (b2)

    allocate (x17(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    x17 = 0.0d0
    call sum_stripe(4, shape(x17), size(x17), '4231', 1.000, &
                    x17, s15)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s15), size(s15), '2431', s15, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u96(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u96)
    deallocate (d1)
    deallocate (d2)

    allocate (x28(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x28 = 0.0d0
    call sum_stripe(6, shape(x28), size(x28), '234156', &
                    1.000, x28, u96)
    deallocate (u96)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s15), size(s15), '2431', s15, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u84(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u84)
    deallocate (d1)
    deallocate (d2)

    allocate (x29(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x29 = 0.0d0
    call sum_stripe(6, shape(x29), size(x29), '235146', &
                    1.000, x29, u84)
    deallocate (u84)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s15), size(s15), '4231', s15, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s122(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s122)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s122)
    deallocate (s122)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s15), size(s15), '2431', s15, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s113(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s113)
    deallocate (d1)
    deallocate (b2)
    deallocate (s15)

    call sum_stripe(4, shape(x7), size(x7), '2134', -1.000, &
                    x7, s113)
    deallocate (s113)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n0 - n0/), '3421', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s16(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s16)
    deallocate (d1)
    deallocate (b2)

    allocate (x18(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    x18 = 0.0d0
    call sum_stripe(4, shape(x18), size(x18), '4231', 1.000, &
                    x18, s16)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s16), size(s16), '2431', s16, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u97(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u97)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '345126', &
                    1.000, x27, u97)
    deallocate (u97)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s16), size(s16), '2431', s16, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u85(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u85)
    deallocate (d1)
    deallocate (d2)

    allocate (x30(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x30 = 0.0d0
    call sum_stripe(6, shape(x30), size(x30), '345126', &
                    1.000, x30, u85)
    deallocate (u85)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s16), size(s16), '2431', s16, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s123(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s123)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '3124', 1.000, x1, &
                    s123)
    deallocate (s123)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s16), size(s16), '4231', s16, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s114(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s114)
    deallocate (d1)
    deallocate (b2)
    deallocate (s16)

    call sum_stripe(4, shape(x8), size(x8), '3124', -1.000, &
                    x8, s114)
    deallocate (s114)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s17(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s17)
    deallocate (d1)
    deallocate (b2)

    allocate (x19(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    x19 = 0.0d0
    call sum_stripe(4, shape(x19), size(x19), '3241', -1.000, &
                    x19, s17)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s17), size(s17), '2413', s17, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u98(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u98)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '246135', &
                    1.000, x28, u98)
    deallocate (u98)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s17), size(s17), '2413', s17, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u86(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u86)
    deallocate (d1)
    deallocate (d2)

    allocate (x31(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x31 = 0.0d0
    call sum_stripe(6, shape(x31), size(x31), '356124', &
                    1.000, x31, u86)
    deallocate (u86)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s17), size(s17), '4213', s17, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s124(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s124)
    deallocate (d1)
    deallocate (b2)
    deallocate (s17)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s124)
    deallocate (s124)

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

    allocate (x12(n0 + 1:n2, n0 + 1:n2))
    x12 = 0.0d0
    call sum_stripe(2, shape(x12), size(x12), '21', 1.000, &
                    x12, q5)
    deallocate (q5)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n2 - n0/), '1432', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s18(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n2 + 1:n3))
    i1 = k4*k3*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s18)
    deallocate (d1)
    deallocate (b2)

    allocate (x20(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    x20 = 0.0d0
    call sum_stripe(4, shape(x20), size(x20), '4123', -1.000, &
                    x20, s18)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s18), size(s18), '2341', s18, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u99(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k3
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u99)
    deallocate (d1)
    deallocate (d2)

    allocate (x32(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x32 = 0.0d0
    call sum_stripe(6, shape(x32), size(x32), '456123', &
                    1.000, x32, u99)
    deallocate (u99)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s18), size(s18), '3241', s18, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u87(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u87)
    deallocate (d1)
    deallocate (d2)

    allocate (x33(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x33 = 0.0d0
    call sum_stripe(6, shape(x33), size(x33), '456123', &
                    1.000, x33, u87)
    deallocate (u87)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s18), size(s18), '2341', s18, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s125(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s125)
    deallocate (d1)
    deallocate (b2)
    deallocate (s18)

    call sum_stripe(4, shape(x2), size(x2), '4123', -1.000, &
                    x2, s125)
    deallocate (s125)

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

    allocate (x13(n2 + 1:n3, n2 + 1:n3))
    x13 = 0.0d0
    x13 = x13 + q6
    deallocate (q6)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s19(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s19)
    deallocate (d1)
    deallocate (b2)

    allocate (x24(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    x24 = 0.0d0
    call sum_stripe(4, shape(x24), size(x24), '3241', -1.000, &
                    x24, s19)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s19), size(s19), '2413', s19, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u101(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u101)
    deallocate (d1)
    deallocate (d2)

    allocate (x34(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x34 = 0.0d0
    call sum_stripe(6, shape(x34), size(x34), '245136', &
                    1.000, x34, u101)
    deallocate (u101)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s19), size(s19), '2413', s19, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u100(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u100)
    deallocate (d1)
    deallocate (d2)

    allocate (x35(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x35 = 0.0d0
    call sum_stripe(6, shape(x35), size(x35), '345126', &
                    1.000, x35, u100)
    deallocate (u100)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s19), size(s19), '4213', s19, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s127(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s127)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '2134', 1.000, x8, &
                    s127)
    deallocate (s127)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s19), size(s19), '2413', s19, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s126(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s126)
    deallocate (d1)
    deallocate (b2)
    deallocate (s19)

    call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                    x7, s126)
    deallocate (s126)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s20(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s20)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x24), size(x24), '4231', 1.000, &
                    x24, s20)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s20), size(s20), '2431', s20, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u103(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u103)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x34), size(x34), '245136', &
                    -1.000, x34, u103)
    deallocate (u103)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s20), size(s20), '2431', s20, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u102(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u102)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x35), size(x35), '345126', &
                    -1.000, x35, u102)
    deallocate (u102)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s20), size(s20), '4231', s20, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s129(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s129)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '2134', -1.000, &
                    x8, s129)
    deallocate (s129)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s20), size(s20), '2431', s20, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s128(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s128)
    deallocate (d1)
    deallocate (b2)
    deallocate (s20)

    call sum_stripe(4, shape(x7), size(x7), '3124', 1.000, x7, &
                    s128)
    deallocate (s128)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '2143', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s21(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
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
    i1 = k1*k4*k1
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
    i1 = k2*k3*k3
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
    i1 = k3*k4*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s24)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                    s24)
    deallocate (s24)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s25(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s25)
    deallocate (d1)
    deallocate (b2)

    allocate (x3(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x3 = 0.0d0
    call sum_stripe(4, shape(x3), size(x3), '2134', -1.000, &
                    x3, s25)
    deallocate (s25)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4213', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s26(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s26)
    deallocate (d1)
    deallocate (b2)

    allocate (x36(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x36 = 0.0d0
    call sum_stripe(4, shape(x36), size(x36), '3124', 1.000, &
                    x36, s26)
    deallocate (s26)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n0 - n0/), '2413', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s27(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s27)
    deallocate (d1)
    deallocate (b2)

    allocate (x37(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x37 = 0.0d0
    call sum_stripe(4, shape(x37), size(x37), '3124', 1.000, &
                    x37, s27)
    deallocate (s27)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n2 - n0/), '3421', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s28(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s28)
    deallocate (d1)
    deallocate (b2)

    allocate (x4(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x4 = 0.0d0
    call sum_stripe(4, shape(x4), size(x4), '4123', 1.000, x4, &
                    s28)
    deallocate (s28)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s29(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s29)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x7), size(x7), '3124', 1.000, x7, &
                    s29)
    deallocate (s29)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '2413', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s30(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s30)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '2134', -1.000, &
                    x8, s30)
    deallocate (s30)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '2134', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s31(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s31)
    deallocate (d1)
    deallocate (b2)

    allocate (x9(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x9 = 0.0d0
    call sum_stripe(4, shape(x9), size(x9), '3124', -1.000, &
                    x9, s31)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s31), size(s31), '3214', s31, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u120(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u120)
    deallocate (d1)
    deallocate (d2)

    allocate (x38(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x38 = 0.0d0
    call sum_stripe(6, shape(x38), size(x38), '346125', &
                    1.000, x38, u120)
    deallocate (u120)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s31), size(s31), '3214', s31, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u118(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u118)
    deallocate (d1)
    deallocate (d2)
    deallocate (s31)

    allocate (x39(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x39 = 0.0d0
    call sum_stripe(6, shape(x39), size(x39), '356124', &
                    1.000, x39, u118)
    deallocate (u118)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s32(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s32)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x9), size(x9), '4231', 1.000, x9, &
                    s32)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s32), size(s32), '2431', s32, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u121(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u121)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x38), size(x38), '346125', &
                    -1.000, x38, u121)
    deallocate (u121)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s32), size(s32), '2431', s32, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u119(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u119)
    deallocate (d1)
    deallocate (d2)
    deallocate (s32)

    call sum_stripe(6, shape(x39), size(x39), '356124', &
                    -1.000, x39, u119)
    deallocate (u119)

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

    call sum_stripe(2, shape(x12), size(x12), '21', 1.000, &
                    x12, q7)
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

    call sum_stripe(2, shape(x13), size(x13), '21', -1.000, &
                    x13, q8)
    deallocate (q8)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s33(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s33)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x17), size(x17), '3241', 1.000, &
                    x17, s33)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s33), size(s33), '2413', s33, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u130(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u130)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '234156', &
                    1.000, x28, u130)
    deallocate (u130)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s33), size(s33), '2413', s33, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u122(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u122)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x29), size(x29), '235146', &
                    1.000, x29, u122)
    deallocate (u122)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s33), size(s33), '4213', s33, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s158(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s158)
    deallocate (d1)
    deallocate (b2)
    deallocate (s33)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s158)
    deallocate (s158)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '2143', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s34(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s34)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x18), size(x18), '3124', -1.000, &
                    x18, s34)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s34), size(s34), '3214', s34, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u131(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u131)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '345126', &
                    -1.000, x27, u131)
    deallocate (u131)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s34), size(s34), '3214', s34, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u123(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u123)
    deallocate (d1)
    deallocate (d2)
    deallocate (s34)

    call sum_stripe(6, shape(x30), size(x30), '345126', &
                    -1.000, x30, u123)
    deallocate (u123)

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

    x10 = x10 + q9
    deallocate (q9)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s35(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s35)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x19), size(x19), '4231', 1.000, &
                    x19, s35)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s35), size(s35), '2431', s35, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u132(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u132)
    deallocate (d1)
    deallocate (d2)

    allocate (x40(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x40 = 0.0d0
    call sum_stripe(6, shape(x40), size(x40), '246135', &
                    1.000, x40, u132)
    deallocate (u132)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s35), size(s35), '2431', s35, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u124(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u124)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x31), size(x31), '356124', &
                    -1.000, x31, u124)
    deallocate (u124)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s35), size(s35), '4231', s35, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s159(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s159)
    deallocate (d1)
    deallocate (b2)
    deallocate (s35)

    call sum_stripe(4, shape(x2), size(x2), '2134', -1.000, &
                    x2, s159)
    deallocate (s159)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n1 - n0/), '2431', intm, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s36(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s36)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x20), size(x20), '3124', -1.000, &
                    x20, s36)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s36), size(s36), '2314', s36, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u133(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k3
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u133)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x32), size(x32), '456123', &
                    1.000, x32, u133)
    deallocate (u133)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s36), size(s36), '3214', s36, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u125(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u125)
    deallocate (d1)
    deallocate (d2)
    deallocate (s36)

    call sum_stripe(6, shape(x33), size(x33), '456123', &
                    1.000, x33, u125)
    deallocate (u125)

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

    x11 = x11 + q10
    deallocate (q10)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s37(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s37)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,s37) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (s37(l, m, k, n)*t4c(d, c, b, a, n, m, j, i) & !lmkndcbanmji    (+0.500)
                         - s37(k, m, l, n)*t4c(d, c, b, a, n, m, j, i))/2.0d0 !kmlndcbanmji    (-0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s37), size(s37), '2413', s37, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u134(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, d1, d2, u134)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,u134,t2b) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - u134(c, a, j, n, l, k)*t2b(d, b, n, i) & !cajnlkdbni      (-1.000)
                      - u134(d, a, i, n, l, k)*t2b(c, b, n, j) & !dainlkcbnj      (-1.000)
                      + u134(d, a, j, n, l, k)*t2b(c, b, n, i) & !dajnlkcbni      (+1.000)
                      + u134(c, a, i, n, l, k)*t2b(d, b, n, j) & !cainlkdbnj      (+1.000)
                      + u134(c, a, j, n, k, l)*t2b(d, b, n, i) & !cajnkldbni      (+1.000)
                      + u134(d, a, i, n, k, l)*t2b(c, b, n, j) & !dainklcbnj      (+1.000)
                      - u134(d, a, j, n, k, l)*t2b(c, b, n, i) & !dajnklcbni      (-1.000)
                      - u134(c, a, i, n, k, l)*t2b(d, b, n, j)         !cainkldbnj      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u134)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s37), size(s37), '2413', s37, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s160(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s160)
    deallocate (d1)
    deallocate (b2)
    deallocate (s37)

    call sum_stripe(4, shape(x36), size(x36), '2134', -1.000, &
                    x36, s160)
    deallocate (s160)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s38(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s38)
    deallocate (d1)
    deallocate (b2)

    allocate (x22(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    x22 = 0.0d0
    call sum_stripe(4, shape(x22), size(x22), '3241', -1.000, &
                    x22, s38)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s38), size(s38), '2413', s38, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u135(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u135)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '346125', &
                    -1.000, x28, u135)
    deallocate (u135)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s38), size(s38), '4213', s38, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s161(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s161)
    deallocate (d1)
    deallocate (b2)
    deallocate (s38)

    call sum_stripe(4, shape(x4), size(x4), '2134', 1.000, x4, &
                    s161)
    deallocate (s161)

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

    call sum_stripe(2, shape(x12), size(x12), '21', -1.000, &
                    x12, q11)
    deallocate (q11)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '3412', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s39(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s39)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x22), size(x22), '4231', 1.000, &
                    x22, s39)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s39), size(s39), '2431', s39, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u136(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u136)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x40), size(x40), '346125', &
                    -1.000, x40, u136)
    deallocate (u136)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s39), size(s39), '4231', s39, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s163(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s163)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x37), size(x37), '3124', 1.000, &
                    x37, s163)
    deallocate (s163)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s39), size(s39), '2431', s39, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s162(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s162)
    deallocate (d1)
    deallocate (b2)
    deallocate (s39)

    call sum_stripe(4, shape(x3), size(x3), '3124', 1.000, x3, &
                    s162)
    deallocate (s162)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n2 - n0, n2 - n0/), '2431', intb, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s40(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s40)
    deallocate (d1)
    deallocate (b2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,s40,t4c) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do f = n2 + 1, n3
                sum = sum &
                      + (s40(c, f, e, d)*t4c(f, e, b, a, l, k, j, i) & !cfedfebalkji    (+0.500)
                         - s40(d, f, e, c)*t4c(f, e, b, a, l, k, j, i))/2.0d0 !dfecfebalkji    (-0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s40), size(s40), '3241', s40, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u137(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u137)
    deallocate (d1)
    deallocate (d2)
    deallocate (s40)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,u137,t2b) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      + u137(a, l, j, f, d, c)*t2b(f, b, k, i) & !aljfdcfbki      (+1.000)
                      + u137(a, k, i, f, d, c)*t2b(f, b, l, j) & !akifdcfblj      (+1.000)
                      - u137(a, l, j, f, c, d)*t2b(f, b, k, i) & !aljfcdfbki      (-1.000)
                      - u137(a, k, i, f, c, d)*t2b(f, b, l, j) & !akifcdfblj      (-1.000)
                      - u137(a, k, j, f, d, c)*t2b(f, b, l, i) & !akjfdcfbli      (-1.000)
                      - u137(a, l, i, f, d, c)*t2b(f, b, k, j) & !alifdcfbkj      (-1.000)
                      + u137(a, k, j, f, c, d)*t2b(f, b, l, i) & !akjfcdfbli      (+1.000)
                      + u137(a, l, i, f, c, d)*t2b(f, b, k, j)         !alifcdfbkj      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u137)

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

    x13 = x13 - q12
    deallocate (q12)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3124', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u1)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x39), size(x39), '356124', &
                    -1.000, x39, u1)
    deallocate (u1)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3124', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u2(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u2)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x38), size(x38), '346125', &
                    -1.000, x38, u2)
    deallocate (u2)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intm, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u3(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u3)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x29), size(x29), '235146', &
                    1.000, x29, u3)
    deallocate (u3)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4123', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u4(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u4)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x30), size(x30), '345126', &
                    1.000, x30, u4)
    deallocate (u4)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u5(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u5)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x31), size(x31), '356124', &
                    -1.000, x31, u5)
    deallocate (u5)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n1 - n0/), '3421', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u6(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u6)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x33), size(x33), '456123', &
                    -1.000, x33, u6)
    deallocate (u6)

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s41(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, b1, d2, s41)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                    s41)
    deallocate (s41)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n0 - n0, n1 - n0/), '21', fockr, b1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s42(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s42)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                    x6, s42)
    deallocate (s42)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '361245', t3c, f2)
    allocate (u7(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k2*k4*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u7)
    deallocate (d1)
    deallocate (f2)

    allocate (x41(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x41 = 0.0d0
    call sum_stripe(6, shape(x41), size(x41), '234516', &
                    1.000, x41, u7)
    deallocate (u7)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s43(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s43)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x25), size(x25), '2341', 1.000, &
                    x25, s43)
    deallocate (s43)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1243', intr, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '4312', t2a, d2)
    allocate (s44(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k3*k3
    i3 = k1*k1
    call egemm(i1, i2, i3, d1, d2, s44)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x6), size(x6), '2314', 0.500, x6, &
                    s44)
    deallocate (s44)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u8(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u8)
    deallocate (d1)
    deallocate (d2)

    allocate (x42(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x42 = 0.0d0
    call sum_stripe(6, shape(x42), size(x42), '456231', &
                    1.000, x42, u8)
    deallocate (u8)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s45(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k1
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, d2, s45)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x5), size(x5), '3421', 0.500, x5, &
                    s45)
    deallocate (s45)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s46(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s46)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x26), size(x26), '3412', -1.000, &
                    x26, s46)
    deallocate (s46)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u9(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k1
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u9)
    deallocate (d1)
    deallocate (d2)

    allocate (x43(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x43 = 0.0d0
    call sum_stripe(6, shape(x43), size(x43), '356241', &
                    1.000, x43, u9)
    deallocate (u9)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n0 - n0/), '3421', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u10(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u10)
    deallocate (d1)
    deallocate (d2)

    allocate (x44(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x44 = 0.0d0
    call sum_stripe(6, shape(x44), size(x44), '456231', &
                    1.000, x44, u10)
    deallocate (u10)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '341256', t3c, f2)
    allocate (u11(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2*k4*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, f2, u11)
    deallocate (d1)
    deallocate (f2)

    allocate (x45(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x45 = 0.0d0
    call sum_stripe(6, shape(x45), size(x45), '234615', &
                    1.000, x45, u11)
    deallocate (u11)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s47(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s47)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x7), size(x7), '2431', 1.000, x7, &
                    s47)
    deallocate (s47)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '132456', t3c, f2)
    allocate (u12(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k1*k2*k2*k4
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u12)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x30), size(x30), '345621', &
                    1.000, x30, u12)
    deallocate (u12)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s48(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s48)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x8), size(x8), '3412', 1.000, x8, &
                    s48)
    deallocate (s48)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '142356', t3d, f2)
    allocate (u13(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k2*k4*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u13)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x41), size(x41), '234516', &
                    1.000, x41, u13)
    deallocate (u13)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u14(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u14)
    deallocate (d1)
    deallocate (d2)

    allocate (x46(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x46 = 0.0d0
    call sum_stripe(6, shape(x46), size(x46), '456231', &
                    1.000, x46, u14)
    deallocate (u14)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u15(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u15)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,u15) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (u15(b, j, i, f, m, n)*t4c(d, c, f, a, l, k, n, m) & !bjifmndcfalknm  (+0.500)
                         - u15(a, j, i, f, m, n)*t4c(d, c, f, b, l, k, n, m))/2.0d0 !ajifmndcfblknm  (-0.500)
            end do; end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u15), size(u15), '465123', u15, f1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (u152(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k1
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, f1, d2, u152)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x39), size(x39), '241356', &
                    -1.000, x39, u152)
    deallocate (u152)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u15), size(u15), '541236', u15, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s139(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1*k3
    i3 = k3*k1
    call egemm1(i1, i3, f1, b2, s139)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x5), size(x5), '2341', -1.000, &
                    x5, s139)
    deallocate (s139)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u15), size(u15), '541236', u15, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u89(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u89)
    deallocate (f1)
    deallocate (b2)
    deallocate (u15)

    call sum_stripe(6, shape(x42), size(x42), '324561', &
                    -1.000, x42, u89)
    deallocate (u89)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x42) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x42(m, f, b, a, j, i)*t3c(d, c, f, l, k, m) & !mfbajidcflkm    (+1.000)
                      - x42(m, f, a, b, j, i)*t3c(d, c, f, l, k, m)      !mfabjidcflkm    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x42)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (s49(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k1
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, d2, s49)
    deallocate (d1)
    deallocate (d2)

    allocate (x14(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    x14 = 0.0d0
    call sum_stripe(4, shape(x14), size(x14), '3421', 0.500, &
                    x14, s49)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s49), size(s49), '3412', s49, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u153(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u153)
    deallocate (d1)
    deallocate (d2)

    allocate (x47(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x47 = 0.0d0
    call sum_stripe(6, shape(x47), size(x47), '234156', &
                    1.000, x47, u153)
    deallocate (u153)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s49), size(s49), '3412', s49, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s138(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s138)
    deallocate (d1)
    deallocate (b2)
    deallocate (s49)

    call sum_stripe(4, shape(x5), size(x5), '2134', -0.500, &
                    x5, s138)
    deallocate (s138)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
    allocate (h2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(8, shape(t4c), size(t4c), '34712568', t4c, h2)
    allocate (u16(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k2*k4*k4
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, h2, u16)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x41), size(x41), '234561', &
                    0.500, x41, u16)
    deallocate (u16)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s50(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s50)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x15), size(x15), '3421', 1.000, &
                    x15, s50)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s50), size(s50), '3412', s50, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u155(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u155)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '245136', &
                    -1.000, x27, u155)
    deallocate (u155)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s50), size(s50), '4312', s50, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s140(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s140)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x26), size(x26), '2134', -1.000, &
                    x26, s140)
    deallocate (s140)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s50), size(s50), '3412', s50, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s136(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s136)
    deallocate (d1)
    deallocate (b2)
    deallocate (s50)

    call sum_stripe(4, shape(x25), size(x25), '4123', -1.000, &
                    x25, s136)
    deallocate (s136)

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

    call sum_stripe(2, shape(x10), size(x10), '21', -0.500, &
                    x10, q13)
    deallocate (q13)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n1 - n0/), '1243', intr, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2a), size(t2a), '4312', t2a, d2)
    allocate (s51(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k3*k3
    i3 = k1*k1
    call egemm(i1, i2, i3, d1, d2, s51)
    deallocate (d1)
    deallocate (d2)

    allocate (x16(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    x16 = 0.0d0
    call sum_stripe(4, shape(x16), size(x16), '3412', 0.500, &
                    x16, s51)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s51), size(s51), '4312', s51, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u156(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u156)
    deallocate (d1)
    deallocate (d2)

    allocate (x48(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x48 = 0.0d0
    call sum_stripe(6, shape(x48), size(x48), '256134', &
                    1.000, x48, u156)
    deallocate (u156)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s51), size(s51), '4312', s51, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s137(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s137)
    deallocate (d1)
    deallocate (b2)
    deallocate (s51)

    call sum_stripe(4, shape(x6), size(x6), '4123', 0.500, x6, &
                    s137)
    deallocate (s137)

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

    call sum_stripe(2, shape(x11), size(x11), '21', 0.500, &
                    x11, q14)
    deallocate (q14)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
    allocate (d2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1234', t2a, d2)
    allocate (u17(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k1*k1*k3
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u17)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4d,u17) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + u17(b, j, i, f, m, n)*t4d(f, d, c, a, n, l, k, m) & !bjifmnfdcanlkm  (+1.000)
                      - u17(a, j, i, f, m, n)*t4d(f, d, c, b, n, l, k, m)  !ajifmnfdcbnlkm  (-1.000)
            end do; end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u17), size(u17), '564123', u17, f1)
    allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
    allocate (u159(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k4
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, f1, d2, u159)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x33), size(x33), '241356', &
                    1.000, x33, u159)
    deallocate (u159)

    allocate (f1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u17), size(u17), '541236', u17, f1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u158(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1*k3
    i2 = k2*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, f1, d2, u158)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x31), size(x31), '342561', &
                    -1.000, x31, u158)
    deallocate (u158)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u17), size(u17), '465123', u17, f1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (u157(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k1
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, f1, d2, u157)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x39), size(x39), '241356', &
                    -1.000, x39, u157)
    deallocate (u157)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u17), size(u17), '465123', u17, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s170(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k1
    i3 = k2*k4
    call egemm1(i1, i3, f1, b2, s170)
    deallocate (f1)
    deallocate (b2)

    x5 = x5 + s170
    deallocate (s170)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u17), size(u17), '654123', u17, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u127(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u127)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x44), size(x44), '312456', &
                    -1.000, x44, u127)
    deallocate (u127)

    allocate (f1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u17), size(u17), '451236', u17, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u126(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1*k3*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u126)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x43), size(x43), '423561', &
                    1.000, x43, u126)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u126), size(u126), '623145', u126, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u189(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u189)
    deallocate (f1)
    deallocate (b2)
    deallocate (u126)

    call sum_stripe(6, shape(x39), size(x39), '213456', &
                    1.000, x39, u189)
    deallocate (u189)

    allocate (f1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u17), size(u17), '541236', u17, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u91(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1*k3*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u91)
    deallocate (f1)
    deallocate (b2)
    deallocate (u17)

    call sum_stripe(6, shape(x46), size(x46), '324561', &
                    -1.000, x46, u91)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x46,t3d) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x46(m, f, b, a, j, i)*t3d(f, d, c, m, l, k) & !mfbajifdcmlk    (+1.000)
                      - x46(m, f, a, b, j, i)*t3d(f, d, c, m, l, k)      !mfabjifdcmlk    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x46)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u91), size(u91), '621345', u91, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u184(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u184)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x33), size(x33), '213456', &
                    -1.000, x33, u184)
    deallocate (u184)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x33,t2c) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      - x33(f, d, b, a, j, i)*t2c(f, c, l, k) & !fdbajifclk      (-1.000)
                      + x33(f, d, a, b, j, i)*t2c(f, c, l, k) & !fdabjifclk      (+1.000)
                      + x33(f, c, b, a, j, i)*t2c(f, d, l, k) & !fcbajifdlk      (+1.000)
                      - x33(f, c, a, b, j, i)*t2c(f, d, l, k)          !fcabjifdlk      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x33)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u91), size(u91), '261345', u91, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u183(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u183)
    deallocate (f1)
    deallocate (b2)
    deallocate (u91)

    call sum_stripe(6, shape(x31), size(x31), '412356', &
                    1.000, x31, u183)
    deallocate (u183)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(8, shape(t4d), size(t4d), '14523678', t4d, h2)
    allocate (u18(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k2*k4*k4
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, h2, u18)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x41), size(x41), '234561', &
                    1.000, x41, u18)
    deallocate (u18)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '1324', t2a, d2)
    allocate (s52(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s52)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x24), size(x24), '3421', 1.000, &
                    x24, s52)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s52), size(s52), '3412', s52, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u164(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u164)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x34), size(x34), '245136', &
                    -1.000, x34, u164)
    deallocate (u164)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s52), size(s52), '3412', s52, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u162(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u162)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x35), size(x35), '345126', &
                    -1.000, x35, u162)
    deallocate (u162)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s52), size(s52), '4312', s52, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s172(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s172)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '2134', -1.000, &
                    x8, s172)
    deallocate (s172)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s52), size(s52), '3412', s52, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s171(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s171)
    deallocate (d1)
    deallocate (b2)
    deallocate (s52)

    call sum_stripe(4, shape(x7), size(x7), '3124', 1.000, x7, &
                    s171)
    deallocate (s171)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intr, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u19(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u19)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x47), size(x47), '234156', &
                    2.000, x47, u19)
    deallocate (u19)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intr, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u20(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u20)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '245136', &
                    -1.000, x27, u20)
    deallocate (u20)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n1 - n0/), '3421', intr, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u21(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u21)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x48), size(x48), '256134', &
                    2.000, x48, u21)
    deallocate (u21)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intm, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u22(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u22)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '234156', &
                    1.000, x28, u22)
    deallocate (u22)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4123', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u23(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u23)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '345126', &
                    1.000, x27, u23)
    deallocate (u23)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3214', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u24(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u24)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x40), size(x40), '246135', &
                    1.000, x40, u24)
    deallocate (u24)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n1 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u25(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k3
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u25)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x32), size(x32), '456123', &
                    -1.000, x32, u25)
    deallocate (u25)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x32,t2b) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x32(e, d, b, a, l, j)*t2b(c, e, k, i) & !edbaljceki      (+1.000)
                      - x32(e, d, a, b, l, j)*t2b(c, e, k, i) & !edabljceki      (-1.000)
                      + x32(e, d, b, a, k, i)*t2b(c, e, l, j) & !edbakicelj      (+1.000)
                      - x32(e, d, a, b, k, i)*t2b(c, e, l, j) & !edabkicelj      (-1.000)
                      - x32(e, c, b, a, l, j)*t2b(d, e, k, i) & !ecbaljdeki      (-1.000)
                      + x32(e, c, a, b, l, j)*t2b(d, e, k, i) & !ecabljdeki      (+1.000)
                      - x32(e, c, b, a, k, i)*t2b(d, e, l, j) & !ecbakidelj      (-1.000)
                      + x32(e, c, a, b, k, i)*t2b(d, e, l, j) & !ecabkidelj      (+1.000)
                      - x32(e, d, b, a, k, j)*t2b(c, e, l, i) & !edbakjceli      (-1.000)
                      + x32(e, d, a, b, k, j)*t2b(c, e, l, i) & !edabkjceli      (+1.000)
                      - x32(e, d, b, a, l, i)*t2b(c, e, k, j) & !edbalicekj      (-1.000)
                      + x32(e, d, a, b, l, i)*t2b(c, e, k, j) & !edablicekj      (+1.000)
                      + x32(e, c, b, a, k, j)*t2b(d, e, l, i) & !ecbakjdeli      (+1.000)
                      - x32(e, c, a, b, k, j)*t2b(d, e, l, i) & !ecabkjdeli      (-1.000)
                      + x32(e, c, b, a, l, i)*t2b(d, e, k, j) & !ecbalidekj      (+1.000)
                      - x32(e, c, a, b, l, i)*t2b(d, e, k, j)          !ecablidekj      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x32)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n0 - n0, n0 - n0/), '1243', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u26(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, d1, d2, u26)
    deallocate (d1)
    deallocate (d2)

    allocate (x49(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x49 = 0.0d0
    call sum_stripe(6, shape(x49), size(x49), '236145', &
                    1.000, x49, u26)
    deallocate (u26)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '4213', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u27(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u27)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x40), size(x40), '346125', &
                    -1.000, x40, u27)
    deallocate (u27)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n2 - n0/), '3421', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u28(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u28)
    deallocate (d1)
    deallocate (d2)

    allocate (x50(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x50 = 0.0d0
    call sum_stripe(6, shape(x50), size(x50), '456123', &
                    1.000, x50, u28)
    deallocate (u28)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u29(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u29)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x35), size(x35), '345126', &
                    -1.000, x35, u29)
    deallocate (u29)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4213', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u30(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u30)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x34), size(x34), '245136', &
                    -1.000, x34, u30)
    deallocate (u30)

    allocate (b1(n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n1 - n0, n0 - n0/), '12', fockr, b1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (s53(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, b1, d2, s53)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s53)
    deallocate (s53)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(2, shape(fockr), size(fockr), shape(b1), &
                       size(b1), (/n0 - n0, n1 - n0/), '21', fockr, b1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s54(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s54)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s54)
    deallocate (s54)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intr, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u31(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u31)
    deallocate (d1)
    deallocate (d2)

    allocate (x51(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x51 = 0.0d0
    call sum_stripe(6, shape(x51), size(x51), '345261', &
                    1.000, x51, u31)
    deallocate (u31)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '251346', t3b, f2)
    allocate (u32(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k2*k3*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u32)
    deallocate (d1)
    deallocate (f2)

    allocate (x52(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x52 = 0.0d0
    call sum_stripe(6, shape(x52), size(x52), '234516', &
                    1.000, x52, u32)
    deallocate (u32)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1432', intr, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (s55(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s55)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2341', -1.000, &
                    x1, s55)
    deallocate (s55)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '3412', intr, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u33(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u33)
    deallocate (d1)
    deallocate (d2)

    allocate (x53(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x53 = 0.0d0
    call sum_stripe(6, shape(x53), size(x53), '356241', &
                    1.000, x53, u33)
    deallocate (u33)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intr, d1)
    allocate (f2(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '231456', t3b, f2)
    allocate (u34(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k1*k2*k4
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, f2, u34)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x39), size(x39), '245631', &
                    -0.500, x39, u34)
    deallocate (u34)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n1 - n0/), '3241', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (s56(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s56)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2413', -1.000, &
                    x2, s56)
    deallocate (s56)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u35(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k1
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u35)
    deallocate (d1)
    deallocate (d2)

    allocate (x54(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x54 = 0.0d0
    call sum_stripe(6, shape(x54), size(x54), '346251', &
                    1.000, x54, u35)
    deallocate (u35)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (f2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '521346', t3b, f2)
    allocate (u36(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k2*k3*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, f2, u36)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x28), size(x28), '234651', &
                    -1.000, x28, u36)
    deallocate (u36)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
    allocate (s57(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, d2, s57)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2413', -1.000, &
                    x1, s57)
    deallocate (s57)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n2 - n0, n0 - n0/), '3421', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u37(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u37)
    deallocate (d1)
    deallocate (d2)

    allocate (x55(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x55 = 0.0d0
    call sum_stripe(6, shape(x55), size(x55), '456231', &
                    1.000, x55, u37)
    deallocate (u37)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u38(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k3
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u38)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x53), size(x53), '456231', &
                    1.000, x53, u38)
    deallocate (u38)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s58(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s58)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '3421', 1.000, x1, &
                    s58)
    deallocate (s58)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n0 - n0/), '3214', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '241356', t3b, f2)
    allocate (u39(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k1*k3*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, f2, u39)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x39), size(x39), '235614', &
                    1.000, x39, u39)
    deallocate (u39)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (s59(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, d2, s59)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x36), size(x36), '2341', -1.000, &
                    x36, s59)
    deallocate (s59)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n1 - n0, n0 - n0/), '1234', intm, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
    allocate (s60(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, d1, d2, s60)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2314', 1.000, x2, &
                    s60)
    deallocate (s60)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '123456', t3b, f2)
    allocate (u40(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k1*k1*k2*k3
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u40)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x39), size(x39), '345621', &
                    -1.000, x39, u40)
    deallocate (u40)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n2 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (s61(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s61)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x37), size(x37), '3412', 1.000, &
                    x37, s61)
    deallocate (s61)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n2 - n0/), '4132', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '1423', t2b, d2)
    allocate (s62(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n2 + 1:n3))
    i1 = k4*k3
    i2 = k2*k3
    i3 = k1*k4
    call egemm(i1, i2, i3, d1, d2, s62)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '3412', -1.000, &
                    x2, s62)
    deallocate (s62)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s63(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, b1, d2, s63)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x7), size(x7), '2341', 1.000, x7, &
                    s63)
    deallocate (s63)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n0 - n0, n2 - n0/), '21', fockb, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s64(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s64)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x8), size(x8), '2341', -1.000, &
                    x8, s64)
    deallocate (s64)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u41(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u41)
    deallocate (d1)
    deallocate (d2)

    allocate (x56(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x56 = 0.0d0
    call sum_stripe(6, shape(x56), size(x56), '345261', &
                    1.000, x56, u41)
    deallocate (u41)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
    allocate (f2(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '612345', t3c, f2)
    allocate (u42(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k2*k2*k3*k4
    i3 = k4*k1
    call egemm(i1, i2, i3, d1, f2, u42)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x34), size(x34), '234561', &
                    1.000, x34, u42)
    deallocate (u42)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s65(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s65)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x25), size(x25), '2314', -1.000, &
                    x25, s65)
    deallocate (s65)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
    allocate (u43(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k2*k3*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u43)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x52), size(x52), '234516', &
                    1.000, x52, u43)
    deallocate (u43)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s66(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k2*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, d1, d2, s66)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x7), size(x7), '2341', -1.000, &
                    x7, s66)
    deallocate (s66)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intm, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
    allocate (s67(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, d1, d2, s67)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x8), size(x8), '2314', 1.000, x8, &
                    s67)
    deallocate (s67)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n1 - n0, n0 - n0/), '3412', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u44(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u44)
    deallocate (d1)
    deallocate (d2)

    allocate (x57(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x57 = 0.0d0
    call sum_stripe(6, shape(x57), size(x57), '356241', &
                    1.000, x57, u44)
    deallocate (u44)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u45(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u45)
    deallocate (d1)
    deallocate (d2)

    allocate (x58(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x58 = 0.0d0
    call sum_stripe(6, shape(x58), size(x58), '456231', &
                    1.000, x58, u45)
    deallocate (u45)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s68(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s68)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x7), size(x7), '3421', 1.000, x7, &
                    s68)
    deallocate (s68)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '132456', t3c, f2)
    allocate (u46(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k1*k2*k2*k4
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u46)
    deallocate (d1)
    deallocate (f2)

    allocate (x59(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x59 = 0.0d0
    call sum_stripe(6, shape(x59), size(x59), '245631', &
                    1.000, x59, u46)
    deallocate (u46)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n1 - n0/), '3241', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
    allocate (s69(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4
    i2 = k1*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, d2, s69)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x8), size(x8), '2413', -1.000, &
                    x8, s69)
    deallocate (s69)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s70(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s70)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x26), size(x26), '3412', 1.000, &
                    x26, s70)
    deallocate (s70)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u47(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u47)
    deallocate (d1)
    deallocate (d2)

    allocate (x60(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x60 = 0.0d0
    call sum_stripe(6, shape(x60), size(x60), '346251', &
                    1.000, x60, u47)
    deallocate (u47)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
    allocate (u48(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k2*k3*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u48)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x40), size(x40), '234615', &
                    1.000, x40, u48)
    deallocate (u48)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s71(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k3
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s71)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x7), size(x7), '2431', -1.000, &
                    x7, s71)
    deallocate (s71)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '3412', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u49(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u49)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x57), size(x57), '456231', &
                    1.000, x57, u49)
    deallocate (u49)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '4312', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '123456', t3c, f2)
    allocate (u50(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k2*k2*k3
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, f2, u50)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x59), size(x59), '345621', &
                    0.500, x59, u50)
    deallocate (u50)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s72(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s72)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x8), size(x8), '3412', -1.000, &
                    x8, s72)
    deallocate (s72)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u51(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u51)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,u51,t4b) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum & !top two switched
                      + (u51(c, k, i, f, m, n)*t4b(d, f, b, a, l, n, m, j) & !ckifmndfbalnmj  (+0.500)
                         - u51(d, k, i, f, m, n)*t4b(c, f, b, a, l, n, m, j) & !dkifmncfbalnmj  (-0.500)
                         + u51(d, l, i, f, m, n)*t4b(c, f, b, a, k, n, m, j) & !dlifmncfbaknmj  (+0.500)
                         - u51(c, l, i, f, m, n)*t4b(d, f, b, a, k, n, m, j) & !clifmndfbaknmj  (-0.500)
                         + u51(d, k, j, f, m, n)*t4b(c, f, b, a, l, n, m, i) & !dkjfmncfbalnmi  (+0.500)
                         - u51(c, k, j, f, m, n)*t4b(d, f, b, a, l, n, m, i) & !ckjfmndfbalnmi  (-0.500)
                         - u51(d, l, j, f, m, n)*t4b(c, f, b, a, k, n, m, i) & !dljfmncfbaknmi  (-0.500)
                         + u51(c, l, j, f, m, n)*t4b(d, f, b, a, k, n, m, i))/2.0d0 !cljfmndfbaknmi  (+0.500)

            end do; end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u51), size(u51), '465123', u51, f1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (u154(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k1
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, f1, d2, u154)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x38), size(x38), '341256', &
                    -1.000, x38, u154)
    deallocate (u154)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u51), size(u51), '541236', u51, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s143(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k4
    i3 = k3*k1
    call egemm1(i1, i3, f1, b2, s143)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2341', -1.000, &
                    x1, s143)
    deallocate (s143)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u51), size(u51), '541236', u51, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u106(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u106)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x53), size(x53), '423561', &
                    -1.000, x53, u106)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u106), size(u106), '623145', u106, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u178(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u178)
    deallocate (f1)
    deallocate (b2)
    deallocate (u106)

    call sum_stripe(6, shape(x48), size(x48), '312456', &
                    2.000, x48, u178)
    deallocate (u178)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x48,t2b) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum & !top two switched
                      + (x48(f, c, b, a, k, i)*t2b(d, f, l, j) & !fcbakidflj      (+0.500)
                         - x48(f, d, b, a, k, i)*t2b(c, f, l, j) & !fdbakicflj      (-0.500)
                         + x48(f, d, b, a, l, i)*t2b(c, f, k, j) & !fdbalicfkj      (+0.500)
                         - x48(f, c, b, a, l, i)*t2b(d, f, k, j))/2.0d0   !fcbalidfkj      (-0.500)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x48)

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u51), size(u51), '451236', u51, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u104(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u104)
    deallocate (f1)
    deallocate (b2)
    deallocate (u51)

    call sum_stripe(6, shape(x51), size(x51), '623451', &
                    -1.000, x51, u104)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x51,t3b) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum & !top two switched
                      + (x51(n, m, c, k, j, i)*t3b(d, b, a, l, n, m) & !nmckjidbalnm    (+0.500)
                         - x51(n, m, d, k, j, i)*t3b(c, b, a, l, n, m) & !nmdkjicbalnm    (-0.500)
                         + x51(n, m, d, l, j, i)*t3b(c, b, a, k, n, m) & !nmdljicbaknm    (+0.500)
                         - x51(n, m, c, l, j, i)*t3b(d, b, a, k, n, m) & !nmcljidbaknm    (-0.500)
                         + x51(n, m, d, k, i, j)*t3b(c, b, a, l, n, m) & !nmdkijcbalnm    (+0.500)
                         - x51(n, m, c, k, i, j)*t3b(d, b, a, l, n, m) & !nmckijdbalnm    (-0.500)
                         - x51(n, m, d, l, i, j)*t3b(c, b, a, k, n, m) & !nmdlijcbaknm    (-0.500)
                         + x51(n, m, c, l, i, j)*t3b(d, b, a, k, n, m))/2.0d0 !nmclijdbaknm    (+0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x51)

    allocate (f1(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u104), size(u104), '263451', u104, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u177(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k4*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u177)
    deallocate (f1)
    deallocate (b2)
    deallocate (u104)

    call sum_stripe(6, shape(x27), size(x27), '312456', &
                    -1.000, x27, u177)
    deallocate (u177)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
    allocate (h2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(8, shape(t4b), size(t4b), '23614578', t4b, h2)
    allocate (u52(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k2*k3*k4
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, h2, u52)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x39), size(x39), '234561', &
                    -0.500, x39, u52)
    deallocate (u52)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (s73(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s73)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x9), size(x9), '3421', 1.000, x9, &
                    s73)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s73), size(s73), '4312', s73, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s144(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s144)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '3124', -1.000, &
                    x2, s144)
    deallocate (s144)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s73), size(s73), '3412', s73, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s142(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s142)
    deallocate (d1)
    deallocate (b2)
    deallocate (s73)

    call sum_stripe(4, shape(x1), size(x1), '4123', 1.000, x1, &
                    s142)
    deallocate (s142)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
    allocate (d2(n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2134', t2b, d2)
    allocate (u53(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k1*k2*k4
    i3 = k3
    call egemm(i1, i2, i3, d1, d2, u53)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,u53) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      - u53(d, k, i, f, m, n)*t4c(f, c, b, a, n, l, m, j) & !dkifmnfcbanlmj  (-1.000)
                      + u53(c, k, i, f, m, n)*t4c(f, d, b, a, n, l, m, j) & !ckifmnfdbanlmj  (+1.000)
                      + u53(d, l, i, f, m, n)*t4c(f, c, b, a, n, k, m, j) & !dlifmnfcbankmj  (+1.000)
                      - u53(c, l, i, f, m, n)*t4c(f, d, b, a, n, k, m, j) & !clifmnfdbankmj  (-1.000)
                      + u53(d, k, j, f, m, n)*t4c(f, c, b, a, n, l, m, i) & !dkjfmnfcbanlmi  (+1.000)
                      - u53(c, k, j, f, m, n)*t4c(f, d, b, a, n, l, m, i) & !ckjfmnfdbanlmi  (-1.000)
                      - u53(d, l, j, f, m, n)*t4c(f, c, b, a, n, k, m, i) & !dljfmnfcbankmi  (-1.000)
                      + u53(c, l, j, f, m, n)*t4c(f, d, b, a, n, k, m, i)  !cljfmnfdbankmi  (+1.000)
            end do; end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u53), size(u53), '465123', u53, f1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (u167(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k1
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, f1, d2, u167)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '351246', &
                    1.000, x27, u167)
    deallocate (u167)

    allocate (f1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u53), size(u53), '541236', u53, f1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u166(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k4
    i2 = k2*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, f1, d2, u166)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '342561', &
                    -1.000, x28, u166)
    deallocate (u166)

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u53), size(u53), '564123', u53, f1)
    allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
    allocate (u165(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k4
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, f1, d2, u165)
    deallocate (f1)
    deallocate (d2)

    allocate (x61(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    x61 = 0.0d0
    call sum_stripe(6, shape(x61), size(x61), '341256', &
                    1.000, x61, u165)
    deallocate (u165)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u53), size(u53), '465123', u53, f1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (u160(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k1
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, f1, d2, u160)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x38), size(x38), '341256', &
                    -1.000, x38, u160)
    deallocate (u160)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u53), size(u53), '465123', u53, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s176(n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k1
    i3 = k2*k4
    call egemm1(i1, i3, f1, b2, s176)
    deallocate (f1)
    deallocate (b2)

    x1 = x1 + s176
    deallocate (s176)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u53), size(u53), '654123', u53, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u140(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k4*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u140)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x55), size(x55), '312456', &
                    -1.000, x55, u140)
    deallocate (u140)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x55) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x55(m, f, d, c, k, i)*t3b(f, b, a, l, m, j) & !mfdckifbalmj    (+1.000)
                      - x55(m, f, c, d, k, i)*t3b(f, b, a, l, m, j) & !mfcdkifbalmj    (-1.000)
                      - x55(m, f, d, c, l, i)*t3b(f, b, a, k, m, j) & !mfdclifbakmj    (-1.000)
                      + x55(m, f, c, d, l, i)*t3b(f, b, a, k, m, j) & !mfcdlifbakmj    (+1.000)
                      - x55(m, f, d, c, k, j)*t3b(f, b, a, l, m, i) & !mfdckjfbalmi    (-1.000)
                      + x55(m, f, c, d, k, j)*t3b(f, b, a, l, m, i) & !mfcdkjfbalmi    (+1.000)
                      + x55(m, f, d, c, l, j)*t3b(f, b, a, k, m, i) & !mfdcljfbakmi    (+1.000)
                      - x55(m, f, c, d, l, j)*t3b(f, b, a, k, m, i)      !mfcdljfbakmi    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x55)

    allocate (f1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u53), size(u53), '451236', u53, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u138(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u138)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x54), size(x54), '523461', &
                    1.000, x54, u138)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x54) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      - x54(n, m, d, l, k, i)*t3b(c, b, a, n, m, j) & !nmdlkicbanmj    (-1.000)
                      + x54(n, m, c, l, k, i)*t3b(d, b, a, n, m, j) & !nmclkidbanmj    (+1.000)
                      + x54(n, m, d, k, l, i)*t3b(c, b, a, n, m, j) & !nmdklicbanmj    (+1.000)
                      - x54(n, m, c, k, l, i)*t3b(d, b, a, n, m, j) & !nmcklidbanmj    (-1.000)
                      + x54(n, m, d, l, k, j)*t3b(c, b, a, n, m, i) & !nmdlkjcbanmi    (+1.000)
                      - x54(n, m, c, l, k, j)*t3b(d, b, a, n, m, i) & !nmclkjdbanmi    (-1.000)
                      - x54(n, m, d, k, l, j)*t3b(c, b, a, n, m, i) & !nmdkljcbanmi    (-1.000)
                      + x54(n, m, c, k, l, j)*t3b(d, b, a, n, m, i)      !nmckljdbanmi    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x54)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u138), size(u138), '623415', u138, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u190(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u190)
    deallocate (f1)
    deallocate (b2)
    deallocate (u138)

    call sum_stripe(6, shape(x38), size(x38), '213456', &
                    1.000, x38, u190)
    deallocate (u190)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x38,t2a) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x38(m, d, c, l, k, j)*t2a(b, a, m, i) & !mdclkjbami      (-1.000)
                      + x38(m, c, d, l, k, j)*t2a(b, a, m, i) & !mcdlkjbami      (+1.000)
                      + x38(m, d, c, k, l, j)*t2a(b, a, m, i) & !mdckljbami      (+1.000)
                      - x38(m, c, d, k, l, j)*t2a(b, a, m, i) & !mcdkljbami      (-1.000)
                      + x38(m, d, c, l, k, i)*t2a(b, a, m, j) & !mdclkibamj      (+1.000)
                      - x38(m, c, d, l, k, i)*t2a(b, a, m, j) & !mcdlkibamj      (-1.000)
                      - x38(m, d, c, k, l, i)*t2a(b, a, m, j) & !mdcklibamj      (-1.000)
                      + x38(m, c, d, k, l, i)*t2a(b, a, m, j)          !mcdklibamj      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x38)

    allocate (f1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u53), size(u53), '541236', u53, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u111(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k4*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u111)
    deallocate (f1)
    deallocate (b2)
    deallocate (u53)

    call sum_stripe(6, shape(x57), size(x57), '423561', &
                    -1.000, x57, u111)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u111), size(u111), '623145', u111, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u188(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u188)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x61), size(x61), '213456', &
                    -1.000, x61, u188)
    deallocate (u188)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x61,t2b) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      - x61(f, d, c, a, k, i)*t2b(f, b, l, j) & !fdcakifblj      (-1.000)
                      + x61(f, d, c, b, k, i)*t2b(f, a, l, j) & !fdcbkifalj      (+1.000)
                      - x61(f, d, c, a, l, j)*t2b(f, b, k, i) & !fdcaljfbki      (-1.000)
                      + x61(f, d, c, b, l, j)*t2b(f, a, k, i) & !fdcbljfaki      (+1.000)
                      + x61(f, c, d, a, k, i)*t2b(f, b, l, j) & !fcdakifblj      (+1.000)
                      - x61(f, c, d, b, k, i)*t2b(f, a, l, j) & !fcdbkifalj      (-1.000)
                      + x61(f, c, d, a, l, j)*t2b(f, b, k, i) & !fcdaljfbki      (+1.000)
                      - x61(f, c, d, b, l, j)*t2b(f, a, k, i) & !fcdbljfaki      (-1.000)
                      + x61(f, d, c, a, l, i)*t2b(f, b, k, j) & !fdcalifbkj      (+1.000)
                      - x61(f, d, c, b, l, i)*t2b(f, a, k, j) & !fdcblifakj      (-1.000)
                      + x61(f, d, c, a, k, j)*t2b(f, b, l, i) & !fdcakjfbli      (+1.000)
                      - x61(f, d, c, b, k, j)*t2b(f, a, l, i) & !fdcbkjfali      (-1.000)
                      - x61(f, c, d, a, l, i)*t2b(f, b, k, j) & !fcdalifbkj      (-1.000)
                      + x61(f, c, d, b, l, i)*t2b(f, a, k, j) & !fcdblifakj      (+1.000)
                      - x61(f, c, d, a, k, j)*t2b(f, b, l, i) & !fcdakjfbli      (-1.000)
                      + x61(f, c, d, b, k, j)*t2b(f, a, l, i)          !fcdbkjfali      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x61)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u111), size(u111), '263145', u111, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u187(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u187)
    deallocate (f1)
    deallocate (b2)
    deallocate (u111)

    call sum_stripe(6, shape(x28), size(x28), '512346', &
                    1.000, x28, u187)
    deallocate (u187)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u54(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k3
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u54)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,u54,t4c) &
        !$omp private(a,b,c,d,n,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                    sum = sum &
                          - u54(b, k, i, e, m, n)*t4c(d, c, e, a, n, l, m, j) & !bkiemndceanlmj  (-1.000)
                          + u54(a, k, i, e, m, n)*t4c(d, c, e, b, n, l, m, j) & !akiemndcebnlmj  (+1.000)
                          + u54(b, l, i, e, m, n)*t4c(d, c, e, a, n, k, m, j) & !bliemndceankmj  (+1.000)
                          - u54(a, l, i, e, m, n)*t4c(d, c, e, b, n, k, m, j) & !aliemndcebnkmj  (-1.000)
                          + u54(b, k, j, e, m, n)*t4c(d, c, e, a, n, l, m, i) & !bkjemndceanlmi  (+1.000)
                          - u54(a, k, j, e, m, n)*t4c(d, c, e, b, n, l, m, i) & !akjemndcebnlmi  (-1.000)
                          - u54(b, l, j, e, m, n)*t4c(d, c, e, a, n, k, m, i) & !bljemndceankmi  (-1.000)
                          + u54(a, l, j, e, m, n)*t4c(d, c, e, b, n, k, m, i)  !aljemndcebnkmi  (+1.000)
                end do; end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u54), size(u54), '465123', u54, f1)
    allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
    allocate (u169(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k1
    i2 = k1*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, f1, d2, u169)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x27), size(x27), '251346', &
                    1.000, x27, u169)
    deallocate (u169)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u54), size(u54), '541236', u54, f1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4213', t2b, d2)
    allocate (u168(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3
    i2 = k2*k4
    i3 = k3*k1
    call egemm(i1, i2, i3, f1, d2, u168)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '243561', &
                    -1.000, x28, u168)
    deallocate (u168)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u54), size(u54), '654123', u54, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u141(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u141)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x53), size(x53), '312456', &
                    -1.000, x53, u141)
    deallocate (u141)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x53) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x53(m, f, d, b, k, i)*t3b(c, f, a, l, m, j) & !mfdbkicfalmj    (+1.000)
                      - x53(m, f, d, a, k, i)*t3b(c, f, b, l, m, j) & !mfdakicfblmj    (-1.000)
                      - x53(m, f, c, b, k, i)*t3b(d, f, a, l, m, j) & !mfcbkidfalmj    (-1.000)
                      + x53(m, f, c, a, k, i)*t3b(d, f, b, l, m, j) & !mfcakidfblmj    (+1.000)
                      - x53(m, f, d, b, l, i)*t3b(c, f, a, k, m, j) & !mfdblicfakmj    (-1.000)
                      + x53(m, f, d, a, l, i)*t3b(c, f, b, k, m, j) & !mfdalicfbkmj    (+1.000)
                      + x53(m, f, c, b, l, i)*t3b(d, f, a, k, m, j) & !mfcblidfakmj    (+1.000)
                      - x53(m, f, c, a, l, i)*t3b(d, f, b, k, m, j) & !mfcalidfbkmj    (-1.000)
                      - x53(m, f, d, b, k, j)*t3b(c, f, a, l, m, i) & !mfdbkjcfalmi    (-1.000)
                      + x53(m, f, d, a, k, j)*t3b(c, f, b, l, m, i) & !mfdakjcfblmi    (+1.000)
                      + x53(m, f, c, b, k, j)*t3b(d, f, a, l, m, i) & !mfcbkjdfalmi    (+1.000)
                      - x53(m, f, c, a, k, j)*t3b(d, f, b, l, m, i) & !mfcakjdfblmi    (-1.000)
                      + x53(m, f, d, b, l, j)*t3b(c, f, a, k, m, i) & !mfdbljcfakmi    (+1.000)
                      - x53(m, f, d, a, l, j)*t3b(c, f, b, k, m, i) & !mfdaljcfbkmi    (-1.000)
                      - x53(m, f, c, b, l, j)*t3b(d, f, a, k, m, i) & !mfcbljdfakmi    (-1.000)
                      + x53(m, f, c, a, l, j)*t3b(d, f, b, k, m, i)      !mfcaljdfbkmi    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x53)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u54), size(u54), '541236', u54, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s150(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3
    i3 = k3*k1
    call egemm1(i1, i3, f1, b2, s150)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x7), size(x7), '2341', 1.000, x7, &
                    s150)
    deallocate (s150)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u54), size(u54), '541236', u54, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u112(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u112)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x58), size(x58), '324561', &
                    -1.000, x58, u112)
    deallocate (u112)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x58) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x58(m, e, b, a, k, i)*t3c(d, c, e, m, l, j) & !mebakidcemlj    (+1.000)
                      - x58(m, e, a, b, k, i)*t3c(d, c, e, m, l, j) & !meabkidcemlj    (-1.000)
                      - x58(m, e, b, a, l, i)*t3c(d, c, e, m, k, j) & !mebalidcemkj    (-1.000)
                      + x58(m, e, a, b, l, i)*t3c(d, c, e, m, k, j) & !meablidcemkj    (+1.000)
                      - x58(m, e, b, a, k, j)*t3c(d, c, e, m, l, i) & !mebakjdcemli    (-1.000)
                      + x58(m, e, a, b, k, j)*t3c(d, c, e, m, l, i) & !meabkjdcemli    (+1.000)
                      + x58(m, e, b, a, l, j)*t3c(d, c, e, m, k, i) & !mebaljdcemki    (+1.000)
                      - x58(m, e, a, b, l, j)*t3c(d, c, e, m, k, i)      !meabljdcemki    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x58)

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u54), size(u54), '451236', u54, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u108(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u108)
    deallocate (f1)
    deallocate (b2)
    deallocate (u54)

    call sum_stripe(6, shape(x56), size(x56), '623451', &
                    1.000, x56, u108)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x56) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      - x56(n, m, b, k, j, i)*t3c(d, c, a, n, l, m) & !nmbkjidcanlm    (-1.000)
                      + x56(n, m, a, k, j, i)*t3c(d, c, b, n, l, m) & !nmakjidcbnlm    (+1.000)
                      + x56(n, m, b, l, j, i)*t3c(d, c, a, n, k, m) & !nmbljidcankm    (+1.000)
                      - x56(n, m, a, l, j, i)*t3c(d, c, b, n, k, m) & !nmaljidcbnkm    (-1.000)
                      + x56(n, m, b, k, i, j)*t3c(d, c, a, n, l, m) & !nmbkijdcanlm    (+1.000)
                      - x56(n, m, a, k, i, j)*t3c(d, c, b, n, l, m) & !nmakijdcbnlm    (-1.000)
                      - x56(n, m, b, l, i, j)*t3c(d, c, a, n, k, m) & !nmblijdcankm    (-1.000)
                      + x56(n, m, a, l, i, j)*t3c(d, c, b, n, k, m)      !nmalijdcbnkm    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x56)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u108), size(u108), '623451', u108, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u186(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u186)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x27), size(x27), '213456', &
                    -1.000, x27, u186)
    deallocate (u186)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x27,t2b) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + x27(n, d, b, k, j, i)*t2b(c, a, l, n) & !ndbkjicaln      (+1.000)
                      - x27(n, d, a, k, j, i)*t2b(c, b, l, n) & !ndakjicbln      (-1.000)
                      - x27(n, c, b, k, j, i)*t2b(d, a, l, n) & !ncbkjidaln      (-1.000)
                      + x27(n, c, a, k, j, i)*t2b(d, b, l, n) & !ncakjidbln      (+1.000)
                      - x27(n, d, b, l, j, i)*t2b(c, a, k, n) & !ndbljicakn      (-1.000)
                      + x27(n, d, a, l, j, i)*t2b(c, b, k, n) & !ndaljicbkn      (+1.000)
                      + x27(n, c, b, l, j, i)*t2b(d, a, k, n) & !ncbljidakn      (+1.000)
                      - x27(n, c, a, l, j, i)*t2b(d, b, k, n) & !ncaljidbkn      (-1.000)
                      - x27(n, d, b, k, i, j)*t2b(c, a, l, n) & !ndbkijcaln      (-1.000)
                      + x27(n, d, a, k, i, j)*t2b(c, b, l, n) & !ndakijcbln      (+1.000)
                      + x27(n, c, b, k, i, j)*t2b(d, a, l, n) & !ncbkijdaln      (+1.000)
                      - x27(n, c, a, k, i, j)*t2b(d, b, l, n) & !ncakijdbln      (-1.000)
                      + x27(n, d, b, l, i, j)*t2b(c, a, k, n) & !ndblijcakn      (+1.000)
                      - x27(n, d, a, l, i, j)*t2b(c, b, k, n) & !ndalijcbkn      (-1.000)
                      - x27(n, c, b, l, i, j)*t2b(d, a, k, n) & !ncblijdakn      (-1.000)
                      + x27(n, c, a, l, i, j)*t2b(d, b, k, n)          !ncalijdbkn      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x27)

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u108), size(u108), '263451', u108, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u179(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k3*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u179)
    deallocate (f1)
    deallocate (b2)
    deallocate (u108)

    call sum_stripe(6, shape(x35), size(x35), '213456', &
                    1.000, x35, u179)
    deallocate (u179)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (s74(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, d2, s74)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x17), size(x17), '3421', 1.000, &
                    x17, s74)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s74), size(s74), '3412', s74, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u170(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u170)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '234156', &
                    1.000, x28, u170)
    deallocate (u170)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s74), size(s74), '3412', s74, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u161(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u161)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x29), size(x29), '236145', &
                    -1.000, x29, u161)
    deallocate (u161)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s74), size(s74), '4312', s74, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s175(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s175)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s175)
    deallocate (s175)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s74), size(s74), '3412', s74, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s149(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s149)
    deallocate (d1)
    deallocate (b2)
    deallocate (s74)

    call sum_stripe(4, shape(x7), size(x7), '2134', -1.000, &
                    x7, s149)
    deallocate (s149)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(8, shape(t4c), size(t4c), '13724568', t4c, h2)
    allocate (u55(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k2*k3*k4
    i3 = k1*k3*k4
    call egemm(i1, i2, i3, d1, h2, u55)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x34), size(x34), '234561', &
                    -1.000, x34, u55)
    deallocate (u55)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3241', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '2314', t2b, d2)
    allocate (s75(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k1*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, d2, s75)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x18), size(x18), '3421', -1.000, &
                    x18, s75)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s75), size(s75), '3412', s75, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u163(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u163)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x30), size(x30), '345126', &
                    -1.000, x30, u163)
    deallocate (u163)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s75), size(s75), '3412', s75, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s174(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s174)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x1), size(x1), '3124', -1.000, &
                    x1, s174)
    deallocate (s174)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s75), size(s75), '4312', s75, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s151(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s151)
    deallocate (d1)
    deallocate (b2)
    deallocate (s75)

    call sum_stripe(4, shape(x8), size(x8), '3124', 1.000, x8, &
                    s151)
    deallocate (s151)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s76(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s76)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x15), size(x15), '3421', 1.000, &
                    x15, s76)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s76), size(s76), '4312', s76, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s153(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s153)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x26), size(x26), '2134', -1.000, &
                    x26, s153)
    deallocate (s153)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x26) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x26(e, b, a, i)*t3c(d, c, e, l, k, j) & !ebaidcelkj      (+1.000)
                      - x26(e, a, b, i)*t3c(d, c, e, l, k, j) & !eabidcelkj      (-1.000)
                      - x26(e, b, a, j)*t3c(d, c, e, l, k, i) & !ebajdcelki      (-1.000)
                      + x26(e, a, b, j)*t3c(d, c, e, l, k, i)          !eabjdcelki      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x26)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s76), size(s76), '3412', s76, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s146(n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s146)
    deallocate (d1)
    deallocate (b2)
    deallocate (s76)

    call sum_stripe(4, shape(x25), size(x25), '4123', -1.000, &
                    x25, s146)
    deallocate (s146)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x25) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x25(m, b, j, i)*t3c(d, c, a, l, k, m) & !mbjidcalkm      (+1.000)
                      - x25(m, a, j, i)*t3c(d, c, b, l, k, m) & !majidcblkm      (-1.000)
                      - x25(m, b, i, j)*t3c(d, c, a, l, k, m) & !mbijdcalkm      (-1.000)
                      + x25(m, a, i, j)*t3c(d, c, b, l, k, m)          !maijdcblkm      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x25)

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

    call sum_stripe(2, shape(x10), size(x10), '21', 1.000, &
                    x10, q15)
    deallocate (q15)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(8, shape(t4c), size(t4c), '13524678', t4c, h2)
    allocate (u56(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k2*k3*k4
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, h2, u56)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x39), size(x39), '234561', &
                    -1.000, x39, u56)
    deallocate (u56)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (d2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '2413', t2b, d2)
    allocate (s77(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k2*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, d2, s77)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x22), size(x22), '3421', 1.000, &
                    x22, s77)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s77), size(s77), '4312', s77, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s179(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s179)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x37), size(x37), '2134', -1.000, &
                    x37, s179)
    deallocate (s179)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s77), size(s77), '3412', s77, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s177(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s177)
    deallocate (d1)
    deallocate (b2)
    deallocate (s77)

    call sum_stripe(4, shape(x36), size(x36), '4123', -1.000, &
                    x36, s177)
    deallocate (s177)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4132', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '1423', t2b, d2)
    allocate (s78(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k2*k3
    i3 = k1*k4
    call egemm(i1, i2, i3, d1, d2, s78)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x19), size(x19), '3421', -1.000, &
                    x19, s78)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s78), size(s78), '4312', s78, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s180(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s180)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s180)
    deallocate (s180)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s78), size(s78), '3412', s78, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s147(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s147)
    deallocate (d1)
    deallocate (b2)
    deallocate (s78)

    call sum_stripe(4, shape(x7), size(x7), '4123', -1.000, &
                    x7, s147)
    deallocate (s147)

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

    call sum_stripe(2, shape(x12), size(x12), '21', 1.000, &
                    x12, q16)
    deallocate (q16)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n1 - n0/), '1243', intm, d1)
    allocate (d2(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(t2b), size(t2b), '4312', t2b, d2)
    allocate (s79(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4
    i2 = k3*k4
    i3 = k2*k1
    call egemm(i1, i2, i3, d1, d2, s79)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x20), size(x20), '3412', 1.000, &
                    x20, s79)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s79), size(s79), '3412', s79, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s178(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s178)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '4123', 1.000, x2, &
                    s178)
    deallocate (s178)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s79), size(s79), '4312', s79, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s148(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3))
    i1 = k3*k4*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s148)
    deallocate (d1)
    deallocate (b2)
    deallocate (s79)

    call sum_stripe(4, shape(x8), size(x8), '4123', 1.000, x8, &
                    s148)
    deallocate (s148)

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

    call sum_stripe(2, shape(x13), size(x13), '21', -1.000, &
                    x13, q17)
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

    call sum_stripe(2, shape(x11), size(x11), '21', -1.000, &
                    x11, q18)
    deallocate (q18)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intb, d1)
    allocate (d2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1234', t2b, d2)
    allocate (u57(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k1*k2*k3
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u57)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4d,u57) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (u57(a, k, i, f, m, n)*t4d(f, d, c, b, n, m, l, j) & !akifmnfdcbnmlj  (+0.500)
                         - u57(b, k, i, f, m, n)*t4d(f, d, c, a, n, m, l, j) & !bkifmnfdcanmlj  (-0.500)
                         + u57(b, l, i, f, m, n)*t4d(f, d, c, a, n, m, k, j) & !blifmnfdcanmkj  (+0.500)
                         - u57(a, l, i, f, m, n)*t4d(f, d, c, b, n, m, k, j) & !alifmnfdcbnmkj  (-0.500)
                         + u57(b, k, j, f, m, n)*t4d(f, d, c, a, n, m, l, i) & !bkjfmnfdcanmli  (+0.500)
                         - u57(a, k, j, f, m, n)*t4d(f, d, c, b, n, m, l, i) & !akjfmnfdcbnmli  (-0.500)
                         - u57(b, l, j, f, m, n)*t4d(f, d, c, a, n, m, k, i) & !bljfmnfdcanmki  (-0.500)
                         + u57(a, l, j, f, m, n)*t4d(f, d, c, b, n, m, k, i))/2.0d0 !aljfmnfdcbnmki  (+0.500)
            end do; end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u57), size(u57), '541236', u57, f1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u173(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3
    i2 = k1*k3
    i3 = k4*k2
    call egemm(i1, i2, i3, f1, d2, u173)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x35), size(x35), '352461', &
                    1.000, x35, u173)
    deallocate (u173)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t2c,x35) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      + x35(n, b, a, k, j, i)*t2c(d, c, n, l) & !nbakjidcnl      (+1.000)
                      - x35(n, a, b, k, j, i)*t2c(d, c, n, l) & !nabkjidcnl      (-1.000)
                      - x35(n, b, a, l, j, i)*t2c(d, c, n, k) & !nbaljidcnk      (-1.000)
                      + x35(n, a, b, l, j, i)*t2c(d, c, n, k) & !nabljidcnk      (+1.000)
                      - x35(n, b, a, k, i, j)*t2c(d, c, n, l) & !nbakijdcnl      (-1.000)
                      + x35(n, a, b, k, i, j)*t2c(d, c, n, l) & !nabkijdcnl      (+1.000)
                      + x35(n, b, a, l, i, j)*t2c(d, c, n, k) & !nbalijdcnk      (+1.000)
                      - x35(n, a, b, l, i, j)*t2c(d, c, n, k)          !nablijdcnk      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x35)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u57), size(u57), '465123', u57, f1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (u172(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k2
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, f1, d2, u172)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x40), size(x40), '241356', &
                    1.000, x40, u172)
    deallocate (u172)

    allocate (f1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u57), size(u57), '564123', u57, f1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '4312', t2c, d2)
    allocate (u171(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4
    i2 = k4*k4
    i3 = k2*k2
    call egemm(i1, i2, i3, f1, d2, u171)
    deallocate (f1)
    deallocate (d2)

    call sum_stripe(6, shape(x50), size(x50), '231456', &
                    0.500, x50, u171)
    deallocate (u171)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u57), size(u57), '541236', u57, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s183(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3
    i3 = k4*k2
    call egemm1(i1, i3, f1, b2, s183)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x7), size(x7), '2341', -1.000, &
                    x7, s183)
    deallocate (s183)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u57), size(u57), '541236', u57, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u146(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u146)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x57), size(x57), '324561', &
                    -1.000, x57, u146)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u146), size(u146), '621345', u146, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u193(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u193)
    deallocate (f1)
    deallocate (b2)
    deallocate (u146)

    call sum_stripe(6, shape(x50), size(x50), '213456', &
                    1.000, x50, u193)
    deallocate (u193)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x50,t2b) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      + x50(f, d, c, a, l, j)*t2b(f, b, k, i) & !fdcaljfbki      (+1.000)
                      + x50(f, d, c, a, k, i)*t2b(f, b, l, j) & !fdcakifblj      (+1.000)
                      - x50(f, d, c, a, k, j)*t2b(f, b, l, i) & !fdcakjfbli      (-1.000)
                      - x50(f, d, c, a, l, i)*t2b(f, b, k, j)          !fdcalifbkj      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x50)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(6, shape(u57), size(u57), '451236', u57, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u144(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k2*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u144)
    deallocate (f1)
    deallocate (b2)
    deallocate (u57)

    call sum_stripe(6, shape(x60), size(x60), '523461', &
                    -1.000, x60, u144)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x60) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum & !top two switched
                      + (x60(n, m, a, l, k, i)*t3c(d, c, b, n, m, j) & !nmalkidcbnmj    (+0.500)
                         - x60(n, m, b, l, k, i)*t3c(d, c, a, n, m, j) & !nmblkidcanmj    (-0.500)
                         + x60(n, m, b, k, l, i)*t3c(d, c, a, n, m, j) & !nmbklidcanmj    (+0.500)
                         - x60(n, m, a, k, l, i)*t3c(d, c, b, n, m, j) & !nmaklidcbnmj    (-0.500)
                         + x60(n, m, b, l, k, j)*t3c(d, c, a, n, m, i) & !nmblkjdcanmi    (+0.500)
                         - x60(n, m, a, l, k, j)*t3c(d, c, b, n, m, i) & !nmalkjdcbnmi    (-0.500)
                         - x60(n, m, b, k, l, j)*t3c(d, c, a, n, m, i) & !nmbkljdcanmi    (-0.500)
                         + x60(n, m, a, k, l, j)*t3c(d, c, b, n, m, i))/2.0d0 !nmakljdcbnmi    (+0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x60)

    allocate (f1(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u144), size(u144), '263415', u144, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u192(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k3*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u192)
    deallocate (f1)
    deallocate (b2)
    deallocate (u144)

    call sum_stripe(6, shape(x28), size(x28), '213456', &
                    1.000, x28, u192)
    deallocate (u192)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
    allocate (h2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(8, shape(t4d), size(t4d), '12534678', t4d, h2)
    allocate (u58(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k2*k3*k4
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, h2, u58)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x59), size(x59), '234561', &
                    0.500, x59, u58)
    deallocate (u58)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t2b,x59) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - x59(m, c, b, l, k, j)*t2b(d, a, m, i) & !mcblkjdami      (-1.000)
                      + x59(m, c, a, l, k, j)*t2b(d, b, m, i) & !mcalkjdbmi      (+1.000)
                      + x59(m, d, b, l, k, j)*t2b(c, a, m, i) & !mdblkjcami      (+1.000)
                      - x59(m, d, a, l, k, j)*t2b(c, b, m, i) & !mdalkjcbmi      (-1.000)
                      + x59(m, c, b, l, k, i)*t2b(d, a, m, j) & !mcblkidamj      (+1.000)
                      - x59(m, c, a, l, k, i)*t2b(d, b, m, j) & !mcalkidbmj      (-1.000)
                      - x59(m, d, b, l, k, i)*t2b(c, a, m, j) & !mdblkicamj      (-1.000)
                      + x59(m, d, a, l, k, i)*t2b(c, b, m, j)          !mdalkicbmj      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x59)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '1324', t2b, d2)
    allocate (s80(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s80)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x24), size(x24), '3421', 1.000, &
                    x24, s80)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s80), size(s80), '3412', s80, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u175(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u175)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x34), size(x34), '245136', &
                    -1.000, x34, u175)
    deallocate (u175)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s80), size(s80), '4312', s80, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s184(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s184)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '2134', -1.000, &
                    x8, s184)
    deallocate (s184)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s80), size(s80), '3412', s80, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s182(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s182)
    deallocate (d1)
    deallocate (b2)
    deallocate (s80)

    call sum_stripe(4, shape(x7), size(x7), '3124', 1.000, x7, &
                    s182)
    deallocate (s182)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n0 - n0, n0 - n0/), '1342', intm, d1)
    allocate (f2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '412356', t3a, f2)
    allocate (u59(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k1*k3*k3
    i3 = k3*k1
    call egemm(i1, i2, i3, d1, f2, u59)
    deallocate (d1)
    deallocate (f2)

    allocate (x62(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    x62 = 0.0d0
    call sum_stripe(6, shape(x62), size(x62), '235641', &
                    1.000, x62, u59)
    deallocate (u59)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n2 - n0, n0 - n0/), '4321', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u60(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k3
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u60)
    deallocate (d1)
    deallocate (d2)

    allocate (x63(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x63 = 0.0d0
    call sum_stripe(6, shape(x63), size(x63), '456231', &
                    1.000, x63, u60)
    deallocate (u60)

    allocate (b1(n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n2 - n0, n0 - n0/), '12', fockb, b1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s81(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, b1, d2, s81)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x3), size(x3), '2341', 1.000, x3, &
                    s81)
    deallocate (s81)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(2, shape(fockb), size(fockb), shape(b1), &
                       size(b1), (/n0 - n0, n2 - n0/), '21', fockb, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s82(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s82)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                    x4, s82)
    deallocate (s82)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4132', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u61(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u61)
    deallocate (d1)
    deallocate (d2)

    allocate (x64(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    x64 = 0.0d0
    call sum_stripe(6, shape(x64), size(x64), '345261', &
                    1.000, x64, u61)
    deallocate (u61)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intm, d1)
    allocate (f2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '512346', t3b, f2)
    allocate (u62(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2*k3*k3
    i3 = k4*k1
    call egemm(i1, i2, i3, d1, f2, u62)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x29), size(x29), '234561', &
                    1.000, x29, u62)
    deallocate (u62)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s83(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s83)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x1), size(x1), '2314', 1.000, x1, &
                    s83)
    deallocate (s83)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u63(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u63)
    deallocate (d1)
    deallocate (d2)

    allocate (x65(n0 + 1:n2, n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x65 = 0.0d0
    call sum_stripe(6, shape(x65), size(x65), '356241', &
                    1.000, x65, u63)
    deallocate (u63)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n1 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '123456', t3b, f2)
    allocate (u64(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k1*k1*k2*k3
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u64)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t2c,u64) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - u64(a, l, j, i, b, m)*t2c(d, c, m, k) & !aljibmdcmk      (-1.000)
                      + u64(b, l, j, i, a, m)*t2c(d, c, m, k) & !bljiamdcmk      (+1.000)
                      + u64(a, k, j, i, b, m)*t2c(d, c, m, l) & !akjibmdcml      (+1.000)
                      - u64(b, k, j, i, a, m)*t2c(d, c, m, l)          !bkjiamdcml      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u64)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n1 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s84(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s84)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2413', 1.000, x2, &
                    s84)
    deallocate (s84)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n0 - n0/), '4213', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
    allocate (u65(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k1*k3*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u65)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x62), size(x62), '235614', &
                    1.000, x62, u65)
    deallocate (u65)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n0 - n0, n0 - n0/), '1432', intb, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s85(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k4
    i3 = k4*k2
    call egemm(i1, i2, i3, d1, d2, s85)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x36), size(x36), '2341', 1.000, &
                    x36, s85)
    deallocate (s85)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n0 - n0/), '1243', intb, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '4312', t2c, d2)
    allocate (s86(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k4*k4
    i3 = k2*k2
    call egemm(i1, i2, i3, d1, d2, s86)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x4), size(x4), '2314', 0.500, x4, &
                    s86)
    deallocate (s86)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '3412', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u66(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u66)
    deallocate (d1)
    deallocate (d2)

    allocate (x66(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    x66 = 0.0d0
    call sum_stripe(6, shape(x66), size(x66), '456231', &
                    1.000, x66, u66)
    deallocate (u66)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n2 - n0, n0 - n0/), '4312', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s87(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k2*k2
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, d2, s87)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x3), size(x3), '3421', 0.500, x3, &
                    s87)
    deallocate (s87)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n2 - n0/), '3241', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s88(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s88)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x37), size(x37), '3412', -1.000, &
                    x37, s88)
    deallocate (s88)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u67(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k3
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u67)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4b,u67) &
        !$omp private(a,b,c,d,n,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + u67(d, l, k, e, m, n)*t4b(c, e, b, a, n, m, j, i) & !dlkemncebanmji  (+1.000)
                      - u67(c, l, k, e, m, n)*t4b(d, e, b, a, n, m, j, i)  !clkemndebanmji  (-1.000)
            end do; end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u67), size(u67), '654123', u67, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u149(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u149)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x63), size(x63), '312456', &
                    -1.000, x63, u149)
    deallocate (u149)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x63,t3a) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x63(m, e, d, c, l, k)*t3a(e, b, a, m, j, i) & !medclkebamji    (+1.000)
                      - x63(m, e, c, d, l, k)*t3a(e, b, a, m, j, i)      !mecdlkebamji    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x63)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u67), size(u67), '541236', u67, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s155(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4
    i3 = k3*k1
    call egemm1(i1, i3, f1, b2, s155)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x3), size(x3), '2341', 1.000, x3, &
                    s155)
    deallocate (s155)

    allocate (f1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u67), size(u67), '541236', u67, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u116(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u116)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x65), size(x65), '423561', &
                    -1.000, x65, u116)
    deallocate (u116)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x65) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x65(m, e, d, b, l, k)*t3b(c, e, a, m, j, i) & !medblkceamji    (+1.000)
                      - x65(m, e, d, a, l, k)*t3b(c, e, b, m, j, i) & !medalkcebmji    (-1.000)
                      - x65(m, e, c, b, l, k)*t3b(d, e, a, m, j, i) & !mecblkdeamji    (-1.000)
                      + x65(m, e, c, a, l, k)*t3b(d, e, b, m, j, i)      !mecalkdebmji    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x65)

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u67), size(u67), '451236', u67, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u114(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u114)
    deallocate (f1)
    deallocate (b2)
    deallocate (u67)

    call sum_stripe(6, shape(x64), size(x64), '623451', &
                    1.000, x64, u114)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u114), size(u114), '623451', u114, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u182(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u182)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x30), size(x30), '213456', &
                    -1.000, x30, u182)
    deallocate (u182)

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u114), size(u114), '263451', u114, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u180(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u180)
    deallocate (f1)
    deallocate (b2)
    deallocate (u114)

    call sum_stripe(6, shape(x34), size(x34), '312456', &
                    1.000, x34, u180)
    deallocate (u180)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (h2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(8, shape(t4b), size(t4b), '12634578', t4b, h2)
    allocate (u68(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k1*k2*k3*k3
    i3 = k1*k3*k4
    call egemm(i1, i2, i3, d1, h2, u68)
    deallocate (d1)
    deallocate (h2)

    call sum_stripe(6, shape(x62), size(x62), '234561', &
                    1.000, x62, u68)
    deallocate (u68)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s89(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s89)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x9), size(x9), '3421', 1.000, x9, &
                    s89)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s89), size(s89), '4312', s89, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s157(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s157)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x2), size(x2), '3124', -1.000, &
                    x2, s157)
    deallocate (s157)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s89), size(s89), '3412', s89, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s154(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s154)
    deallocate (d1)
    deallocate (b2)
    deallocate (s89)

    call sum_stripe(4, shape(x1), size(x1), '4123', 1.000, x1, &
                    s154)
    deallocate (s154)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (u69(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k2*k2*k4
    i3 = k4
    call egemm(i1, i2, i3, d1, d2, u69)
    deallocate (d1)
    deallocate (d2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,u69) &
        !$omp private(a,b,c,d,n,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (u69(d, l, k, f, m, n)*t4c(f, c, b, a, n, m, j, i) & !dlkfmnfcbanmji  (+0.500)
                         - u69(c, l, k, f, m, n)*t4c(f, d, b, a, n, m, j, i))/2.0d0 !clkfmnfdbanmji  (-0.500)
            end do; end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u69), size(u69), '541236', u69, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s189(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4
    i3 = k4*k2
    call egemm1(i1, i3, f1, b2, s189)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(4, shape(x3), size(x3), '2341', -1.000, &
                    x3, s189)
    deallocate (s189)

    allocate (f1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(u69), size(u69), '541236', u69, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u151(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u151)
    deallocate (f1)
    deallocate (b2)
    deallocate (u69)

    call sum_stripe(6, shape(x66), size(x66), '324561', &
                    -1.000, x66, u151)
    deallocate (u151)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x66) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x66(m, f, d, c, l, k)*t3b(f, b, a, m, j, i) & !mfdclkfbamji    (+1.000)
                      - x66(m, f, c, d, l, k)*t3b(f, b, a, m, j, i)      !mfcdlkfbamji    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x66)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
    allocate (d2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1234', t2c, d2)
    allocate (s90(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k2*k2
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, d2, s90)
    deallocate (d1)
    deallocate (d2)

    allocate (x21(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    x21 = 0.0d0
    call sum_stripe(4, shape(x21), size(x21), '3421', 0.500, &
                    x21, s90)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s90), size(s90), '3412', s90, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u174(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, d1, d2, u174)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x49), size(x49), '236145', &
                    0.500, x49, u174)
    deallocate (u174)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s90), size(s90), '3412', s90, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s188(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s188)
    deallocate (d1)
    deallocate (b2)
    deallocate (s90)

    call sum_stripe(4, shape(x3), size(x3), '2134', -0.500, &
                    x3, s188)
    deallocate (s188)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
    allocate (h2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, &
                 n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(8, shape(t4c), size(t4c), '12534678', t4c, h2)
    allocate (u70(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k1*k2*k3*k3
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, h2, u70)
    deallocate (d1)
    deallocate (h2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,u70,t2c) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum & !top two switched
                      + (u70(b, a, k, j, i, m)*t2c(d, c, m, l) & !bakjimdcml  (+0.500)
                         - u70(b, a, l, j, i, m)*t2c(d, c, m, k))/2.0d0   !baljimdcmk  (-0.500)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u70)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (d2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '1324', t2c, d2)
    allocate (s91(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k2*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, d2, s91)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(4, shape(x22), size(x22), '3421', 1.000, &
                    x22, s91)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s91), size(s91), '4312', s91, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s190(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s190)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x37), size(x37), '2134', -1.000, &
                    x37, s190)
    deallocate (s190)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x37) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      + x37(e, d, c, k)*t3b(e, b, a, l, j, i) & !edckebalji      (+1.000)
                      - x37(e, c, d, k)*t3b(e, b, a, l, j, i) & !ecdkebalji      (-1.000)
                      - x37(e, d, c, l)*t3b(e, b, a, k, j, i) & !edclebakji      (-1.000)
                      + x37(e, c, d, l)*t3b(e, b, a, k, j, i)          !ecdlebakji      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x37)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s91), size(s91), '3412', s91, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s186(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s186)
    deallocate (d1)
    deallocate (b2)
    deallocate (s91)

    call sum_stripe(4, shape(x36), size(x36), '4123', -1.000, &
                    x36, s186)
    deallocate (s186)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x36) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x36(m, d, l, k)*t3b(c, b, a, m, j, i) & !mdlkcbamji      (+1.000)
                      - x36(m, c, l, k)*t3b(d, b, a, m, j, i) & !mclkdbamji      (-1.000)
                      - x36(m, d, k, l)*t3b(c, b, a, m, j, i) & !mdklcbamji      (-1.000)
                      + x36(m, c, k, l)*t3b(d, b, a, m, j, i)          !mckldbamji      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x36)

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

    call sum_stripe(2, shape(x12), size(x12), '21', -0.500, &
                    x12, q19)
    deallocate (q19)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n0 - n0, n0 - n0, n2 - n0, n2 - n0/), '1243', intb, d1)
    allocate (d2(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(t2c), size(t2c), '4312', t2c, d2)
    allocate (s92(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i2 = k4*k4
    i3 = k2*k2
    call egemm(i1, i2, i3, d1, d2, s92)
    deallocate (d1)
    deallocate (d2)

    allocate (x23(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    x23 = 0.0d0
    call sum_stripe(4, shape(x23), size(x23), '3412', 0.500, &
                    x23, s92)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s92), size(s92), '4312', s92, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s187(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s187)
    deallocate (d1)
    deallocate (b2)
    deallocate (s92)

    call sum_stripe(4, shape(x4), size(x4), '4123', 0.500, x4, &
                    s187)
    deallocate (s187)

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

    call sum_stripe(2, shape(x13), size(x13), '21', 0.500, &
                    x13, q20)
    deallocate (q20)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '142356', t3a, f2)
    allocate (u71(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k1*k3*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u71)
    deallocate (d1)
    deallocate (f2)

    allocate (x67(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x67 = 0.0d0
    call sum_stripe(6, shape(x67), size(x67), '345621', &
                    1.000, x67, u71)
    deallocate (u71)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (f2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '124356', t3a, f2)
    allocate (s93(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, f2, s93)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x5), size(x5), '2341', -0.500, &
                    x5, s93)
    deallocate (s93)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3124', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '154236', t3a, f2)
    allocate (s94(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k1*k1*k3
    call egemm(i1, i2, i3, d1, f2, s94)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x6), size(x6), '2341', 0.500, x6, &
                    s94)
    deallocate (s94)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3a), size(t3a), '142356', t3a, f2)
    allocate (u72(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k1*k3*k3
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u72)
    deallocate (d1)
    deallocate (f2)

    allocate (x68(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    x68 = 0.0d0
    call sum_stripe(6, shape(x68), size(x68), '345621', &
                    1.000, x68, u72)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u72), size(u72), '561234', u72, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u148(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u148)
    deallocate (f1)
    deallocate (b2)
    deallocate (u72)

    call sum_stripe(6, shape(x62), size(x62), '412356', &
                    1.000, x62, u148)
    deallocate (u148)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n1 - n0/), '3124', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(6, shape(t3b), size(t3b), '265134', t3b, f2)
    allocate (s95(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k1*k1*k3
    call egemm(i1, i2, i3, d1, f2, s95)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,s95) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3
                sum = sum &
                      + (s95(c, a, l, f)*t3b(d, f, b, k, j, i) & !calfdfbkji      (+0.500)
                         - s95(c, b, l, f)*t3b(d, f, a, k, j, i) & !cblfdfakji      (-0.500)
                         - s95(d, a, l, f)*t3b(c, f, b, k, j, i) & !dalfcfbkji      (-0.500)
                         + s95(d, b, l, f)*t3b(c, f, a, k, j, i) & !dblfcfakji      (+0.500)
                         - s95(c, a, k, f)*t3b(d, f, b, l, j, i) & !cakfdfblji      (-0.500)
                         + s95(c, b, k, f)*t3b(d, f, a, l, j, i) & !cbkfdfalji      (+0.500)
                         + s95(d, a, k, f)*t3b(c, f, b, l, j, i) & !dakfcfblji      (+0.500)
                         - s95(d, b, k, f)*t3b(c, f, a, l, j, i))/2.0d0   !dbkfcfalji      (-0.500)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s95)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intr, d1)
    allocate (f2(n1 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '231456', t3b, f2)
    allocate (u73(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k1*k2*k4
    i3 = k3*k3
    call egemm(i1, i2, i3, d1, f2, u73)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,u73) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum & !top two switched
                      + (u73(c, k, j, i, m, n)*t3b(d, b, a, l, n, m) & !ckjimndbalnm    (+0.250)
                         - u73(d, k, j, i, m, n)*t3b(c, b, a, l, n, m) & !dkjimncbalnm    (-0.250)
                         + u73(d, l, j, i, m, n)*t3b(c, b, a, k, n, m) & !dljimncbaknm    (+0.250)
                         - u73(c, l, j, i, m, n)*t3b(d, b, a, k, n, m))/4.0d0 !cljimndbaknm    (-0.250)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u73), size(u73), '561234', u73, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u107(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k4*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u107)
    deallocate (f1)
    deallocate (b2)
    deallocate (u73)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,u107,t2b) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum &
                      + (u107(b, n, c, l, j, i)*t2b(d, a, k, n) & !bncljidakn      (+0.500)
                         - u107(a, n, c, l, j, i)*t2b(d, b, k, n) & !ancljidbkn      (-0.500)
                         - u107(b, n, d, l, j, i)*t2b(c, a, k, n) & !bndljicakn      (-0.500)
                         + u107(a, n, d, l, j, i)*t2b(c, b, k, n) & !andljicbkn      (+0.500)
                         - u107(b, n, c, k, j, i)*t2b(d, a, l, n) & !bnckjidaln      (-0.500)
                         + u107(a, n, c, k, j, i)*t2b(d, b, l, n) & !anckjidbln      (+0.500)
                         + u107(b, n, d, k, j, i)*t2b(c, a, l, n) & !bndkjicaln      (+0.500)
                         - u107(a, n, d, k, j, i)*t2b(c, b, l, n))/2.0d0  !andkjicbln      (-0.500)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (u107)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intr, d1)
    allocate (f2(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '235146', t3b, f2)
    allocate (s96(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k4
    i3 = k1*k3*k3
    call egemm(i1, i2, i3, d1, f2, s96)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x1), size(x1), '2341', 0.500, x1, &
                    s96)
    deallocate (s96)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '251346', t3b, f2)
    allocate (u74(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k2*k3*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u74)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,u74) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do n = n0 + 1, n1
                sum = sum &
                      + u74(c, a, l, j, f, n)*t3b(d, f, b, k, n, i) & !caljfndfbkni    (+1.000)
                      - u74(d, a, k, i, f, n)*t3b(c, f, b, l, n, j) & !dakifncfblnj    (-1.000)
                      - u74(d, a, l, j, f, n)*t3b(c, f, b, k, n, i) & !daljfncfbkni    (-1.000)
                      + u74(c, a, k, i, f, n)*t3b(d, f, b, l, n, j) & !cakifndfblnj    (+1.000)
                      - u74(c, a, k, j, f, n)*t3b(d, f, b, l, n, i) & !cakjfndfblni    (-1.000)
                      + u74(d, a, l, i, f, n)*t3b(c, f, b, k, n, j) & !dalifncfbknj    (+1.000)
                      + u74(d, a, k, j, f, n)*t3b(c, f, b, l, n, i) & !dakjfncfblni    (+1.000)
                      - u74(c, a, l, i, f, n)*t3b(d, f, b, k, n, j)      !califndfbknj    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u74), size(u74), '561234', u74, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u105(n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u105)
    deallocate (f1)
    deallocate (b2)
    deallocate (u74)

    call sum_stripe(6, shape(x52), size(x52), '612345', &
                    1.000, x52, u105)
    deallocate (u105)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '164235', t3c, f2)
    allocate (s97(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k2*k1*k4
    call egemm(i1, i2, i3, d1, f2, s97)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s97)
    deallocate (s97)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '364125', t3c, f2)
    allocate (s98(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2*k1*k3
    call egemm(i1, i2, i3, d1, f2, s98)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,s98,t3b) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      + s98(d, c, l, f)*t3b(f, b, a, k, j, i) & !dclffbakji      (+1.000)
                      - s98(d, c, k, f)*t3b(f, b, a, l, j, i)          !dckffbalji      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s98)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '123456', t3b, f2)
    allocate (u75(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k1*k2*k3
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u75)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x43), size(x43), '345621', &
                    1.000, x43, u75)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x43) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + x43(n, m, b, k, j, i)*t3c(d, c, a, n, l, m) & !nmbkjidcanlm    (+1.000)
                      - x43(n, m, a, k, j, i)*t3c(d, c, b, n, l, m) & !nmakjidcbnlm    (-1.000)
                      - x43(n, m, b, l, j, i)*t3c(d, c, a, n, k, m) & !nmbljidcankm    (-1.000)
                      + x43(n, m, a, l, j, i)*t3c(d, c, b, n, k, m)      !nmaljidcbnkm    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x43)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u75), size(u75), '651234', u75, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u143(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u143)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x39), size(x39), '213456', &
                    1.000, x39, u143)
    deallocate (u143)

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u75), size(u75), '561234', u75, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u117(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k2*k3*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u117)
    deallocate (f1)
    deallocate (b2)
    deallocate (u75)

!
    call sum_stripe(6, shape(x31), size(x31), '213456', &
                    1.000, x31, u117)
    deallocate (u117)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x31,t2c) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - x31(n, b, a, k, j, i)*t2c(d, c, n, l) & !nbakjidcnl      (-1.000)
                      + x31(n, a, b, k, j, i)*t2c(d, c, n, l) & !nabkjidcnl      (+1.000)
                      + x31(n, b, a, l, j, i)*t2c(d, c, n, k) & !nbaljidcnk      (+1.000)
                      - x31(n, a, b, l, j, i)*t2c(d, c, n, k)          !nabljidcnk      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x31)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '136245', t3c, f2)
    allocate (s99(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k1*k3*k4
    call egemm(i1, i2, i3, d1, f2, s99)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,s99) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - s99(c, l, k, n)*t3b(d, b, a, n, j, i) & !clkndbanji      (-1.000)
                      + s99(d, l, k, n)*t3b(c, b, a, n, j, i)          !dlkncbanji      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s99)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3241', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '241356', t3b, f2)
    allocate (u76(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4
    i2 = k1*k1*k3*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, f2, u76)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x44), size(x44), '345621', &
                    -1.000, x44, u76)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x44) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x44(m, f, d, b, j, i)*t3c(f, c, a, l, k, m) & !mfdbjifcalkm    (+1.000)
                      - x44(m, f, d, a, j, i)*t3c(f, c, b, l, k, m) & !mfdajifcblkm    (-1.000)
                      - x44(m, f, c, b, j, i)*t3c(f, d, a, l, k, m) & !mfcbjifdalkm    (-1.000)
                      + x44(m, f, c, a, j, i)*t3c(f, d, b, l, k, m)      !mfcajifdblkm    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x44)

    allocate (f1(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u76), size(u76), '561234', u76, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u142(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k4*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u142)
    deallocate (f1)
    deallocate (b2)
    deallocate (u76)

    call sum_stripe(6, shape(x39), size(x39), '412356', &
                    1.000, x39, u142)
    deallocate (u142)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t2b,x39) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x39(m, d, b, k, j, i)*t2b(c, a, l, m) & !mdbkjicalm      (+1.000)
                      - x39(m, d, a, k, j, i)*t2b(c, b, l, m) & !mdakjicblm      (-1.000)
                      - x39(m, c, b, k, j, i)*t2b(d, a, l, m) & !mcbkjidalm      (-1.000)
                      + x39(m, c, a, k, j, i)*t2b(d, b, l, m) & !mcakjidblm      (+1.000)
                      - x39(m, d, b, l, j, i)*t2b(c, a, k, m) & !mdbljicakm      (-1.000)
                      + x39(m, d, a, l, j, i)*t2b(c, b, k, m) & !mdaljicbkm      (+1.000)
                      + x39(m, c, b, l, j, i)*t2b(d, a, k, m) & !mcbljidakm      (+1.000)
                      - x39(m, c, a, l, j, i)*t2b(d, b, k, m)          !mcaljidbkm      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x39)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4231', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
    allocate (u77(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i2 = k1*k1*k3*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u77)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x67), size(x67), '345621', &
                    1.000, x67, u77)
    deallocate (u77)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x67) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n1 + 1, n3; do n = n0 + 1, n1
                sum = sum &
                      + x67(n, f, b, a, j, i)*t3c(d, c, f, l, k, n)      !nfbajidcflkn    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x67)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '124356', t3b, f2)
    allocate (s100(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k1*k3
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, f2, s100)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x5), size(x5), '2341', 1.000, x5, &
                    s100)
    deallocate (s100)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4321', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '134256', t3c, f2)
    allocate (s101(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1
    i2 = k1*k2*k4
    i3 = k2*k3*k4
    call egemm(i1, i2, i3, d1, f2, s101)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x1), size(x1), '2341', 1.000, x1, &
                    s101)
    deallocate (s101)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '251346', t3b, f2)
    allocate (u78(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k2*k3*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u78)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x57), size(x57), '345621', &
                    1.000, x57, u78)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x57) &
        !$omp private(a,b,c,d,m,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x57(m, f, d, b, k, i)*t3c(f, c, a, m, l, j) & !mfdbkifcamlj    (+1.000)
                      - x57(m, f, d, a, k, i)*t3c(f, c, b, m, l, j) & !mfdakifcbmlj    (-1.000)
                      - x57(m, f, c, b, k, i)*t3c(f, d, a, m, l, j) & !mfcbkifdamlj    (-1.000)
                      + x57(m, f, c, a, k, i)*t3c(f, d, b, m, l, j) & !mfcakifdbmlj    (+1.000)
                      - x57(m, f, d, b, l, i)*t3c(f, c, a, m, k, j) & !mfdblifcamkj    (-1.000)
                      + x57(m, f, d, a, l, i)*t3c(f, c, b, m, k, j) & !mfdalifcbmkj    (+1.000)
                      + x57(m, f, c, b, l, i)*t3c(f, d, a, m, k, j) & !mfcblifdamkj    (+1.000)
                      - x57(m, f, c, a, l, i)*t3c(f, d, b, m, k, j) & !mfcalifdbmkj    (-1.000)
                      - x57(m, f, d, b, k, j)*t3c(f, c, a, m, l, i) & !mfdbkjfcamli    (-1.000)
                      + x57(m, f, d, a, k, j)*t3c(f, c, b, m, l, i) & !mfdakjfcbmli    (+1.000)
                      + x57(m, f, c, b, k, j)*t3c(f, d, a, m, l, i) & !mfcbkjfdamli    (+1.000)
                      - x57(m, f, c, a, k, j)*t3c(f, d, b, m, l, i) & !mfcakjfdbmli    (-1.000)
                      + x57(m, f, d, b, l, j)*t3c(f, c, a, m, k, i) & !mfdbljfcamki    (+1.000)
                      - x57(m, f, d, a, l, j)*t3c(f, c, b, m, k, i) & !mfdaljfcbmki    (-1.000)
                      - x57(m, f, c, b, l, j)*t3c(f, d, a, m, k, i) & !mfcbljfdamki    (-1.000)
                      + x57(m, f, c, a, l, j)*t3c(f, d, b, m, k, i)      !mfcaljfdbmki    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x57)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u78), size(u78), '561234', u78, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u139(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u139)
    deallocate (f1)
    deallocate (b2)
    deallocate (u78)

    call sum_stripe(6, shape(x28), size(x28), '512346', &
                    -1.000, x28, u139)
    deallocate (u139)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n1 - n0, n0 - n0/), '4132', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '152346', t3b, f2)
    allocate (u79(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3
    i2 = k1*k2*k3*k3
    i3 = k1*k4
    call egemm(i1, i2, i3, d1, f2, u79)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,u79,t3c) &
        !$omp private(a,b,c,d,n,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do n = n0 + 1, n2
                sum = sum &
                      - u79(b, a, k, i, e, n)*t3c(d, c, e, n, l, j) & !bakiendcenlj    (-1.000)
                      + u79(b, a, l, i, e, n)*t3c(d, c, e, n, k, j) & !baliendcenkj    (+1.000)
                      + u79(b, a, k, j, e, n)*t3c(d, c, e, n, l, i) & !bakjendcenli    (+1.000)
                      - u79(b, a, l, j, e, n)*t3c(d, c, e, n, k, i)      !baljendcenki    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n1 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u79), size(u79), '561234', u79, f1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (u115(n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k3*k2
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, f1, b2, u115)
    deallocate (f1)
    deallocate (b2)
    deallocate (u79)

    call sum_stripe(6, shape(x29), size(x29), '612345', &
                    1.000, x29, u115)
    deallocate (u115)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '125346', t3b, f2)
    allocate (s102(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k3
    i3 = k1*k3*k4
    call egemm(i1, i2, i3, d1, f2, s102)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x7), size(x7), '2341', 1.000, x7, &
                    s102)
    deallocate (s102)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (f2(n2 + 1:n3, n1 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '132456', t3c, f2)
    allocate (u80(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2*k2*k4
    i3 = k3*k4
    call egemm(i1, i2, i3, d1, f2, u80)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x64), size(x64), '345621', &
                    1.000, x64, u80)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x64,t3b) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + x64(n, m, d, l, k, i)*t3b(c, b, a, n, m, j) & !nmdlkicbanmj    (+1.000)
                      - x64(n, m, c, l, k, i)*t3b(d, b, a, n, m, j) & !nmclkidbanmj    (-1.000)
                      - x64(n, m, d, l, k, j)*t3b(c, b, a, n, m, i) & !nmdlkjcbanmi    (-1.000)
                      + x64(n, m, c, l, k, j)*t3b(d, b, a, n, m, i)      !nmclkjdbanmi    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x64)

    allocate (f1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u80), size(u80), '651234', u80, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u129(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u129)
    deallocate (f1)
    deallocate (b2)

    call sum_stripe(6, shape(x30), size(x30), '213456', &
                    -1.000, x30, u129)
    deallocate (u129)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x30,t2a) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x30(m, d, c, l, k, i)*t2a(b, a, m, j) & !mdclkibamj      (+1.000)
                      - x30(m, c, d, l, k, i)*t2a(b, a, m, j) & !mcdlkibamj      (-1.000)
                      - x30(m, d, c, l, k, j)*t2a(b, a, m, i) & !mdclkjbami      (-1.000)
                      + x30(m, c, d, l, k, j)*t2a(b, a, m, i)          !mcdlkjbami      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x30)

    allocate (f1(n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u80), size(u80), '561234', u80, f1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (u113(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k4*k2
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, f1, b2, u113)
    deallocate (f1)
    deallocate (b2)
    deallocate (u80)

    call sum_stripe(6, shape(x34), size(x34), '312456', &
                    1.000, x34, u113)
    deallocate (u113)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intm, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '254136', t3b, f2)
    allocate (s103(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2*k1*k3
    call egemm(i1, i2, i3, d1, f2, s103)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x8), size(x8), '2341', -1.000, &
                    x8, s103)
    deallocate (s103)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n1 - n0/), '4123', intm, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '154236', t3b, f2)
    allocate (s104(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k2*k1*k4
    call egemm(i1, i2, i3, d1, f2, s104)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                    x6, s104)
    deallocate (s104)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '4123', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '154236', t3d, f2)
    allocate (s105(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2*k2*k4
    call egemm(i1, i2, i3, d1, f2, s105)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x4), size(x4), '2341', -0.500, &
                    x4, s105)
    deallocate (s105)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4321', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '124356', t3d, f2)
    allocate (s106(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2
    i2 = k2*k2*k4
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, f2, s106)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(4, shape(x3), size(x3), '2341', 0.500, x3, &
                    s106)
    deallocate (s106)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(t3b), size(t3b), '142356', t3b, f2)
    allocate (u81(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k1*k3*k3
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u81)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x68), size(x68), '345621', &
                    1.000, x68, u81)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x68,t3d) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do n = n0 + 1, n2
                sum = sum &
                      + x68(n, f, b, a, j, i)*t3d(f, d, c, n, l, k)      !nfbajifdcnlk    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x68)

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(6, shape(u81), size(u81), '561234', u81, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u150(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u150)
    deallocate (f1)
    deallocate (b2)
    deallocate (u81)

    call sum_stripe(6, shape(x62), size(x62), '412356', &
                    1.000, x62, u150)
    deallocate (u150)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x62,t2c) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      + x62(n, b, a, k, j, i)*t2c(d, c, n, l) & !nbakjidcnl      (+1.000)
                      - x62(n, b, a, l, j, i)*t2c(d, c, n, k)          !nbaljidcnk      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x62)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n0 - n0, n2 - n0/), '3124', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '154236', t3c, f2)
    allocate (s107(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2*k2*k4
    call egemm(i1, i2, i3, d1, f2, s107)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,s107) &
        !$omp private(a,b,c,d,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3
                sum = sum &
                      + (s107(c, a, j, f)*t3c(f, d, b, l, k, i) & !cajffdblki      (+0.500)
                         - s107(c, b, j, f)*t3c(f, d, a, l, k, i) & !cbjffdalki      (-0.500)
                         - s107(d, a, j, f)*t3c(f, c, b, l, k, i) & !dajffcblki      (-0.500)
                         + s107(d, b, j, f)*t3c(f, c, a, l, k, i) & !dbjffcalki      (+0.500)
                         - s107(d, b, i, f)*t3c(f, c, a, l, k, j) & !dbiffcalkj      (-0.500)
                         + s107(d, a, i, f)*t3c(f, c, b, l, k, j) & !daiffcblkj      (+0.500)
                         + s107(c, b, i, f)*t3c(f, d, a, l, k, j) & !cbiffdalkj      (+0.500)
                         - s107(c, a, i, f)*t3c(f, d, b, l, k, j))/2.0d0  !caiffdblkj      (-0.500)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s107)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '123456', t3c, f2)
    allocate (u82(n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i2 = k1*k2*k2*k3
    i3 = k4*k4
    call egemm(i1, i2, i3, d1, f2, u82)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,u82) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum & !top two switched
                      + (u82(a, l, k, i, m, n)*t3c(d, c, b, n, m, j) & !alkimndcbnmj    (+0.250)
                         - u82(b, l, k, i, m, n)*t3c(d, c, a, n, m, j) & !blkimndcanmj    (-0.250)
                         - u82(a, l, k, j, m, n)*t3c(d, c, b, n, m, i) & !alkjmndcbnmi    (-0.250)
                         + u82(b, l, k, j, m, n)*t3c(d, c, a, n, m, i))/4.0d0 !blkjmndcanmi    (+0.250)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u82), size(u82), '561234', u82, f1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (u147(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2*k3*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, f1, b2, u147)
    deallocate (f1)
    deallocate (b2)
    deallocate (u82)

    call sum_stripe(6, shape(x34), size(x34), '213456', &
                    0.500, x34, u147)
    deallocate (u147)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '4312', intb, d1)
    allocate (f2(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '124356', t3c, f2)
    allocate (s108(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2
    i2 = k1*k2*k3
    i3 = k2*k4*k4
    call egemm(i1, i2, i3, d1, f2, s108)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,s108) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum & !top two switched
                      + (s108(b, l, j, n)*t3c(d, c, a, n, k, i) & !bljndcanki      (+0.500)
                         - s108(a, l, j, n)*t3c(d, c, b, n, k, i) & !aljndcbnki      (-0.500)
                         + s108(b, k, i, n)*t3c(d, c, a, n, l, j) & !bkindcanlj      (+0.500)
                         - s108(a, k, i, n)*t3c(d, c, b, n, l, j) & !akindcbnlj      (-0.500)
                         + s108(a, k, j, n)*t3c(d, c, b, n, l, i) & !akjndcbnli      (+0.500)
                         - s108(b, k, j, n)*t3c(d, c, a, n, l, i) & !bkjndcanli      (-0.500)
                         - s108(b, l, i, n)*t3c(d, c, a, n, k, j) & !blindcankj      (-0.500)
                         + s108(a, l, i, n)*t3c(d, c, b, n, k, j))/2.0d0  !alindcbnkj      (+0.500)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (s108)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intb, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
    allocate (u83(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i2 = k1*k2*k3*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u83)
    deallocate (d1)
    deallocate (f2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,u83) &
        !$omp private(a,b,c,d,n,f,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do f = n2 + 1, n3; do n = n0 + 1, n2
                sum = sum &
                      + u83(c, a, l, j, f, n)*t3c(f, d, b, n, k, i) & !caljfnfdbnki    (+1.000)
                      - u83(d, a, k, i, f, n)*t3c(f, c, b, n, l, j) & !dakifnfcbnlj    (-1.000)
                      - u83(d, a, l, j, f, n)*t3c(f, c, b, n, k, i) & !daljfnfcbnki    (-1.000)
                      + u83(c, a, k, i, f, n)*t3c(f, d, b, n, l, j) & !cakifnfdbnlj    (+1.000)
                      - u83(c, a, k, j, f, n)*t3c(f, d, b, n, l, i) & !cakjfnfdbnli    (-1.000)
                      + u83(d, a, l, i, f, n)*t3c(f, c, b, n, k, j) & !dalifnfcbnkj    (+1.000)
                      + u83(d, a, k, j, f, n)*t3c(f, c, b, n, l, i) & !dakjfnfcbnli    (+1.000)
                      - u83(c, a, l, i, f, n)*t3c(f, d, b, n, k, j)      !califnfdbnkj    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    allocate (f1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(u83), size(u83), '561234', u83, f1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (u145(n0 + 1:n2, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k3*k4*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, f1, b2, u145)
    deallocate (f1)
    deallocate (b2)
    deallocate (u83)

    call sum_stripe(6, shape(x40), size(x40), '512346', &
                    1.000, x40, u145)
    deallocate (u145)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x40,t2b) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x40(m, d, b, l, k, i)*t2b(c, a, m, j) & !mdblkicamj      (+1.000)
                      - x40(m, d, a, l, k, i)*t2b(c, b, m, j) & !mdalkicbmj      (-1.000)
                      - x40(m, c, b, l, k, i)*t2b(d, a, m, j) & !mcblkidamj      (-1.000)
                      + x40(m, c, a, l, k, i)*t2b(d, b, m, j) & !mcalkidbmj      (+1.000)
                      + x40(m, c, b, l, k, j)*t2b(d, a, m, i) & !mcblkjdami      (+1.000)
                      - x40(m, c, a, l, k, j)*t2b(d, b, m, i) & !mcalkjdbmi      (-1.000)
                      - x40(m, d, b, l, k, j)*t2b(c, a, m, i) & !mdblkjcami      (-1.000)
                      + x40(m, d, a, l, k, j)*t2b(c, b, m, i) & !mdalkjcbmi      (+1.000)
                      - x40(m, d, b, k, l, i)*t2b(c, a, m, j) & !mdbklicamj      (-1.000)
                      + x40(m, d, a, k, l, i)*t2b(c, b, m, j) & !mdaklicbmj      (+1.000)
                      + x40(m, c, b, k, l, i)*t2b(d, a, m, j) & !mcbklidamj      (+1.000)
                      - x40(m, c, a, k, l, i)*t2b(d, b, m, j) & !mcaklidbmj      (-1.000)
                      - x40(m, c, b, k, l, j)*t2b(d, a, m, i) & !mcbkljdami      (-1.000)
                      + x40(m, c, a, k, l, j)*t2b(d, b, m, i) & !mcakljdbmi      (+1.000)
                      + x40(m, d, b, k, l, j)*t2b(c, a, m, i) & !mdbkljcami      (+1.000)
                      - x40(m, d, a, k, l, j)*t2b(c, b, m, i)          !mdakljcbmi      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x40)

    allocate (d1(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n1 - n0, n0 - n0, n0 - n0/), '3412', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s115(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s115)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s115), size(s115), '2431', s115, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '361245', t3c, f2)
    allocate (u88(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k2*k4*k4
    i3 = k1*k3
    call egemm(i1, i2, i3, d1, f2, u88)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x41), size(x41), '234516', &
                    1.000, x41, u88)
    deallocate (u88)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s115), size(s115), '3214', s115, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q21(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q21)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(2, shape(x10), size(x10), '21', -1.000, &
                    x10, q21)
    deallocate (q21)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s115), size(s115), '3214', s115, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s117(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s117)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x15), size(x15), '3241', -1.000, &
                    x15, s117)

  call sum_shift(4,shape(intr),size(intr),shape(x15), &
   size(x15),(/n0-n0,n1-n0,n1-n0,n0-n0/),'3142',1.000,intr,x15)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,x15) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      - x15(m, e, b, i)*t4c(d, c, e, a, l, k, m, j) & !mebidcealkmj    (-1.000)
                      + x15(m, e, a, i)*t4c(d, c, e, b, l, k, m, j) & !meaidceblkmj    (+1.000)
                      + x15(m, e, b, j)*t4c(d, c, e, a, l, k, m, i) & !mebjdcealkmi    (+1.000)
                      - x15(m, e, a, j)*t4c(d, c, e, b, l, k, m, i)      !meajdceblkmi    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x15)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s117), size(s117), '4213', s117, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s193(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s193)
    deallocate (d1)
    deallocate (b2)
    deallocate (s117)

    call sum_stripe(4, shape(x6), size(x6), '2134', 1.000, x6, &
                    s193)
    deallocate (s193)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s115), size(s115), '2314', s115, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s116(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s116)
    deallocate (d1)
    deallocate (b2)
    deallocate (s115)

    call sum_stripe(4, shape(x14), size(x14), '3241', 1.000, &
                    x14, s116)

  call sum_shift(4,shape(intr),size(intr),shape(x14), &
   size(x14),(/n0-n0,n0-n0,n0-n0,n0-n0/),'2143',1.000,intr,x14)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,x14) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n1
                sum = sum &
                      + (x14(n, m, j, i)*t4c(d, c, b, a, l, k, n, m))/2.0d0 !nmjidcbalknm    (+0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x14)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s116), size(s116), '2413', s116, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u176(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u176)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x47), size(x47), '234156', &
                    2.000, x47, u176)
    deallocate (u176)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x47,t2b) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n1
                sum = sum & !order 3,1,2,4
                      + (x47(n, d, a, l, j, i)*t2b(c, b, k, n) & !ndaljicbkn      (+0.500)
                         - x47(n, c, a, l, j, i)*t2b(d, b, k, n) & !ncaljidbkn      (-0.500)
                         - x47(n, d, a, k, j, i)*t2b(c, b, l, n) & !ndakjicbln      (-0.500)
                         + x47(n, c, a, k, j, i)*t2b(d, b, l, n))/2.0d0    !ncakjidbln      (+0.500)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x47)

    allocate (d1(n0 + 1:n1, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s116), size(s116), '2413', s116, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s192(n1 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1*k1
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s192)
    deallocate (d1)
    deallocate (b2)
    deallocate (s116)

    call sum_stripe(4, shape(x5), size(x5), '2134', -1.000, &
                    x5, s192)
    deallocate (s192)

  call sum_shift(4,shape(intr),size(intr),shape(x5), &
   size(x5),(/n0-n0,n1-n0,n0-n0,n0-n0/),'2143',1.000,intr,x5)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x5) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x5(m, b, j, i)*t3c(d, c, a, l, k, m) & !mbjidcalkm      (+1.000)
                      - x5(m, a, j, i)*t3c(d, c, b, l, k, m)           !majidcblkm      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x5)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n0 - n0, n1 - n0, n1 - n0, n0 - n0/), '1432', intr, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s118(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s118)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s118), size(s118), '3421', s118, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q22(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k1*k3
    call egemm1(i1, i3, d1, b2, q22)
    deallocate (d1)
    deallocate (b2)

    x11 = x11 + q22
    deallocate (q22)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s118), size(s118), '4231', s118, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s119(n1 + 1:n3, n1 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s119)
    deallocate (d1)
    deallocate (b2)
    deallocate (s118)

    call sum_stripe(4, shape(x16), size(x16), '3124', 1.000, &
                    x16, s119)
    deallocate (s119)

  call sum_shift(4,shape(intr),size(intr),shape(x16), &
   size(x16),(/n1-n0,n1-n0,n1-n0,n1-n0/),'4321',1.000,intr,x16)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x16,t4c) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do f = n1 + 1, n3
                sum = sum &
                      + (x16(f, e, b, a)*t4c(d, c, f, e, l, k, j, i))/2.0d0      !febadcfelkji    (+0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x16)

    allocate (d1(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (s120(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k1
    i3 = k3
    call egemm(i1, i2, i3, d1, b2, s120)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s120), size(s120), '2431', s120, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '142356', t3c, f2)
    allocate (u110(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k1*k2*k3*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u110)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x52), size(x52), '234516', &
                    1.000, x52, u110)
    deallocate (u110)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t2b,x52) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x52(m, c, a, l, j, i)*t2b(d, b, k, m) & !mcaljidbkm      (-1.000)
                      + x52(m, c, b, l, j, i)*t2b(d, a, k, m) & !mcbljidakm      (+1.000)
                      + x52(m, d, a, l, j, i)*t2b(c, b, k, m) & !mdaljicbkm      (+1.000)
                      - x52(m, d, b, l, j, i)*t2b(c, a, k, m) & !mdbljicakm      (-1.000)
                      + x52(m, c, a, k, j, i)*t2b(d, b, l, m) & !mcakjidblm      (+1.000)
                      - x52(m, c, b, k, j, i)*t2b(d, a, l, m) & !mcbkjidalm      (-1.000)
                      - x52(m, d, a, k, j, i)*t2b(c, b, l, m) & !mdakjicblm      (-1.000)
                      + x52(m, d, b, k, j, i)*t2b(c, a, l, m) & !mdbkjicalm      (+1.000)
                      + x52(m, c, a, l, i, j)*t2b(d, b, k, m) & !mcalijdbkm      (+1.000)
                      - x52(m, c, b, l, i, j)*t2b(d, a, k, m) & !mcblijdakm      (-1.000)
                      - x52(m, d, a, l, i, j)*t2b(c, b, k, m) & !mdalijcbkm      (-1.000)
                      + x52(m, d, b, l, i, j)*t2b(c, a, k, m) & !mdblijcakm      (+1.000)
                      - x52(m, c, a, k, i, j)*t2b(d, b, l, m) & !mcakijdblm      (-1.000)
                      + x52(m, c, b, k, i, j)*t2b(d, a, l, m) & !mcbkijdalm      (+1.000)
                      + x52(m, d, a, k, i, j)*t2b(c, b, l, m) & !mdakijcblm      (+1.000)
                      - x52(m, d, b, k, i, j)*t2b(c, a, l, m)          !mdbkijcalm      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x52)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s120), size(s120), '3214', s120, d1)
    allocate (f2(n0 + 1:n1, n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3c), size(t3c), '612345', t3c, f2)
    allocate (u109(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k2*k2*k3*k4
    i3 = k4*k1
    call egemm(i1, i2, i3, d1, f2, u109)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x34), size(x34), '234561', &
                    1.000, x34, u109)
    deallocate (u109)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x34,t2b) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - x34(n, c, b, l, k, i)*t2b(d, a, n, j) & !ncblkidanj      (-1.000)
                      + x34(n, c, a, l, k, i)*t2b(d, b, n, j) & !ncalkidbnj      (+1.000)
                      + x34(n, d, b, l, k, i)*t2b(c, a, n, j) & !ndblkicanj      (+1.000)
                      - x34(n, d, a, l, k, i)*t2b(c, b, n, j) & !ndalkicbnj      (-1.000)
                      + x34(n, c, b, l, k, j)*t2b(d, a, n, i) & !ncblkjdani      (+1.000)
                      - x34(n, c, a, l, k, j)*t2b(d, b, n, i) & !ncalkjdbni      (-1.000)
                      - x34(n, d, b, l, k, j)*t2b(c, a, n, i) & !ndblkjcani      (-1.000)
                      + x34(n, d, a, l, k, j)*t2b(c, b, n, i)          !ndalkjcbni      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x34)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s120), size(s120), '2431', s120, d1)
    allocate (f2(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(6, shape(t3d), size(t3d), '142356', t3d, f2)
    allocate (u90(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i2 = k2*k2*k4*k4
    i3 = k2*k4
    call egemm(i1, i2, i3, d1, f2, u90)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x41), size(x41), '234516', &
                    1.000, x41, u90)
    deallocate (u90)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x41,t2a) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x41(m, d, c, l, k, i)*t2a(b, a, m, j) & !mdclkibamj      (+1.000)
                      - x41(m, d, c, l, k, j)*t2a(b, a, m, i)          !mdclkjbami      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x41)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1))
    call reorder_stripe(4, shape(s120), size(s120), '2431', s120, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q23(n0 + 1:n1, n0 + 1:n1))
    i1 = k1*k1
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q23)
    deallocate (d1)
    deallocate (b2)

    x10 = x10 + q23
    deallocate (q23)

  call sum_shift(2,shape(fockr),size(fockr),shape(x10), &
   size(x10),(/n0,n0/),'12',1.000,fockr,x10)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x10,t4c) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      + x10(m, i)*t4c(d, c, b, a, l, k, m, j) & !midcbalkmj      (+1.000)
                      - x10(m, j)*t4c(d, c, b, a, l, k, m, i)          !mjdcbalkmi      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x10)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s120), size(s120), '4321', s120, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s131(n2 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n1))
    i1 = k1*k4*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s131)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x18), size(x18), '3124', -1.000, &
                    x18, s131)
    deallocate (s131)

  call sum_shift(4,shape(intm),size(intm),shape(x18), &
   size(x18),(/n0-n0,n2-n0,n2-n0,n0-n0/),'1342',1.000,intm,x18)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x18,t4c) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      + x18(m, e, d, i)*t4c(e, c, b, a, l, k, m, j) & !mediecbalkmj    (+1.000)
                      - x18(m, e, c, i)*t4c(e, d, b, a, l, k, m, j) & !meciedbalkmj    (-1.000)
                      - x18(m, e, d, j)*t4c(e, c, b, a, l, k, m, i) & !medjecbalkmi    (-1.000)
                      + x18(m, e, c, j)*t4c(e, d, b, a, l, k, m, i)      !mecjedbalkmi    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x18)

    allocate (d1(n2 + 1:n3, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s120), size(s120), '2314', s120, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s130(n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k1
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s130)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x17), size(x17), '3241', 1.000, &
                    x17, s130)

  call sum_shift(4,shape(intm),size(intm),shape(x17), &
   size(x17),(/n0-n0,n0-n0,n0-n0,n0-n0/),'2143',1.000,intm,x17)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x17,t4c) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1; do n = n0 + 1, n2
                sum = sum &
                      + x17(n, m, k, i)*t4c(d, c, b, a, n, l, m, j) & !nmkidcbanlmj    (+1.000)
                      - x17(n, m, l, i)*t4c(d, c, b, a, n, k, m, j) & !nmlidcbankmj    (-1.000)
                      - x17(n, m, k, j)*t4c(d, c, b, a, n, l, m, i) & !nmkjdcbanlmi    (-1.000)
                      + x17(n, m, l, j)*t4c(d, c, b, a, n, k, m, i)      !nmljdcbankmi    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x17)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s130), size(s130), '2413', s130, d1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (u185(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u185)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x28), size(x28), '234156', &
                    1.000, x28, u185)
    deallocate (u185)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x28,t2b) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - x28(n, c, a, l, k, i)*t2b(d, b, n, j) & !ncalkidbnj      (-1.000)
                      + x28(n, c, b, l, k, i)*t2b(d, a, n, j) & !ncblkidanj      (+1.000)
                      + x28(n, d, a, l, k, i)*t2b(c, b, n, j) & !ndalkicbnj      (+1.000)
                      - x28(n, d, b, l, k, i)*t2b(c, a, n, j) & !ndblkicanj      (-1.000)
                      + x28(n, c, a, k, l, i)*t2b(d, b, n, j) & !ncaklidbnj      (+1.000)
                      - x28(n, c, b, k, l, i)*t2b(d, a, n, j) & !ncbklidanj      (-1.000)
                      - x28(n, d, a, k, l, i)*t2b(c, b, n, j) & !ndaklicbnj      (-1.000)
                      + x28(n, d, b, k, l, i)*t2b(c, a, n, j) & !ndbklicanj      (+1.000)
                      + x28(n, c, a, l, k, j)*t2b(d, b, n, i) & !ncalkjdbni      (+1.000)
                      - x28(n, c, b, l, k, j)*t2b(d, a, n, i) & !ncblkjdani      (-1.000)
                      - x28(n, d, a, l, k, j)*t2b(c, b, n, i) & !ndalkjcbni      (-1.000)
                      + x28(n, d, b, l, k, j)*t2b(c, a, n, i) & !ndblkjcani      (+1.000)
                      - x28(n, c, a, k, l, j)*t2b(d, b, n, i) & !ncakljdbni      (-1.000)
                      + x28(n, c, b, k, l, j)*t2b(d, a, n, i) & !ncbkljdani      (+1.000)
                      + x28(n, d, a, k, l, j)*t2b(c, b, n, i) & !ndakljcbni      (+1.000)
                      - x28(n, d, b, k, l, j)*t2b(c, a, n, i)          !ndbkljcani      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x28)

    allocate (d1(n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s130), size(s130), '2413', s130, d1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (u181(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k2
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, d1, d2, u181)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x29), size(x29), '235146', &
                    1.000, x29, u181)
    deallocate (u181)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t2c,x29) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      + x29(n, b, a, k, j, i)*t2c(d, c, n, l) & !nbakjidcnl      (+1.000)
                      - x29(n, b, a, l, j, i)*t2c(d, c, n, k) & !nbaljidcnk      (-1.000)
                      - x29(n, b, a, k, i, j)*t2c(d, c, n, l) & !nbakijdcnl      (-1.000)
                      + x29(n, b, a, l, i, j)*t2c(d, c, n, k)          !nbalijdcnk      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x29)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(4, shape(s130), size(s130), '4213', s130, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s196(n2 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n1))
    i1 = k1*k2*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s196)
    deallocate (d1)
    deallocate (b2)
    deallocate (s130)

    call sum_stripe(4, shape(x1), size(x1), '2134', -1.000, &
                    x1, s196)
    deallocate (s196)

  call sum_shift(4,shape(intm),size(intm),shape(x1), &
   size(x1),(/n0-n0,n2-n0,n0-n0,n0-n0/),'1243',1.000,intm,x1)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x1,t3b) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x1(m, d, k, i)*t3b(c, b, a, l, m, j) & !mdkicbalmj      (-1.000)
                      + x1(m, c, k, i)*t3b(d, b, a, l, m, j) & !mckidbalmj      (+1.000)
                      + x1(m, d, l, i)*t3b(c, b, a, k, m, j) & !mdlicbakmj      (+1.000)
                      - x1(m, c, l, i)*t3b(d, b, a, k, m, j) & !mclidbakmj      (-1.000)
                      + x1(m, d, k, j)*t3b(c, b, a, l, m, i) & !mdkjcbalmi      (+1.000)
                      - x1(m, c, k, j)*t3b(d, b, a, l, m, i) & !mckjdbalmi      (-1.000)
                      - x1(m, d, l, j)*t3b(c, b, a, k, m, i) & !mdljcbakmi      (-1.000)
                      + x1(m, c, l, j)*t3b(d, b, a, k, m, i)           !mcljdbakmi      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x1)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s120), size(s120), '3214', s120, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s121(n1 + 1:n3, n2 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s121)
    deallocate (d1)
    deallocate (b2)
    deallocate (s120)

    call sum_stripe(4, shape(x24), size(x24), '3241', -1.000, &
                    x24, s121)

  call sum_shift(4,shape(intm),size(intm),shape(x24), &
   size(x24),(/n0-n0,n2-n0,n1-n0,n0-n0/),'3142',1.000,intm,x24)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4d,x24) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      - x24(m, e, b, i)*t4d(e, d, c, a, m, l, k, j) & !mebiedcamlkj    (-1.000)
                      + x24(m, e, a, i)*t4d(e, d, c, b, m, l, k, j) & !meaiedcbmlkj    (+1.000)
                      + x24(m, e, b, j)*t4d(e, d, c, a, m, l, k, i) & !mebjedcamlki    (+1.000)
                      - x24(m, e, a, j)*t4d(e, d, c, b, m, l, k, i)      !meajedcbmlki    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x24)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s121), size(s121), '4213', s121, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s195(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s195)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x8), size(x8), '2134', 1.000, x8, &
                    s195)
    deallocate (s195)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(s121), size(s121), '2413', s121, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s194(n0 + 1:n2, n0 + 1:n2, n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s194)
    deallocate (d1)
    deallocate (b2)
    deallocate (s121)

    call sum_stripe(4, shape(x7), size(x7), '3124', -1.000, &
                    x7, s194)
    deallocate (s194)

  call sum_shift(4,shape(intm),size(intm),shape(x7), &
   size(x7),(/n0-n0,n1-n0,n0-n0,n0-n0/),'2143',1.000,intm,x7)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x7) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      - x7(m, b, k, i)*t3c(d, c, a, m, l, j) & !mbkidcamlj      (-1.000)
                      + x7(m, a, k, i)*t3c(d, c, b, m, l, j) & !makidcbmlj      (+1.000)
                      + x7(m, b, l, i)*t3c(d, c, a, m, k, j) & !mblidcamkj      (+1.000)
                      - x7(m, a, l, i)*t3c(d, c, b, m, k, j) & !malidcbmkj      (-1.000)
                      + x7(m, b, k, j)*t3c(d, c, a, m, l, i) & !mbkjdcamli      (+1.000)
                      - x7(m, a, k, j)*t3c(d, c, b, m, l, i) & !makjdcbmli      (-1.000)
                      - x7(m, b, l, j)*t3c(d, c, a, m, k, i) & !mbljdcamki      (-1.000)
                      + x7(m, a, l, j)*t3c(d, c, b, m, k, i)           !maljdcbmki      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x7)

    allocate (d1(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n2 - n0, n1 - n0, n0 - n0, n0 - n0/), '4312', intm, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s132(n0 + 1:n2, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1*k3
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s132)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n1 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n2))
    call reorder_stripe(4, shape(s132), size(s132), '2431', s132, d1)
    allocate (f2(n1 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1))
    call reorder_stripe(6, shape(t3c), size(t3c), '341256', t3c, f2)
    allocate (u128(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n1, n0 + 1:n1, n0 + 1:n2))
    i1 = k2*k1
    i2 = k1*k2*k4*k4
    i3 = k2*k3
    call egemm(i1, i2, i3, d1, f2, u128)
    deallocate (d1)
    deallocate (f2)

    call sum_stripe(6, shape(x45), size(x45), '234615', &
                    1.000, x45, u128)
    deallocate (u128)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t2a,x45) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n1
                sum = sum &
                      - x45(m, d, c, l, k, j)*t2a(b, a, m, i) & !mdclkjbami      (-1.000)
                      + x45(m, d, c, k, l, j)*t2a(b, a, m, i) & !mdckljbami      (+1.000)
                      + x45(m, d, c, l, k, i)*t2a(b, a, m, j) & !mdclkibamj      (+1.000)
                      - x45(m, d, c, k, l, i)*t2a(b, a, m, j)          !mdcklibamj      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x45)

    allocate (d1(n0 + 1:n2, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s132), size(s132), '4321', s132, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s164(n2 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k1
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s164)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x9), size(x9), '3124', -1.000, &
                    x9, s164)
    deallocate (s164)

  call sum_shift(4,shape(intm),size(intm),shape(x9), &
   size(x9),(/n0-n0,n1-n0,n2-n0,n0-n0/),'1324',1.000,intm,x9)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x9,t4b) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n1
                sum = sum &
                      - x9(m, e, d, k)*t4b(c, e, b, a, l, m, j, i) & !medkcebalmji    (-1.000)
                      + x9(m, e, c, k)*t4b(d, e, b, a, l, m, j, i) & !meckdebalmji    (+1.000)
                      + x9(m, e, d, l)*t4b(c, e, b, a, k, m, j, i) & !medlcebakmji    (+1.000)
                      - x9(m, e, c, l)*t4b(d, e, b, a, k, m, j, i)       !mecldebakmji    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x9)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s132), size(s132), '3214', s132, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (q24(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k3*k1
    call egemm1(i1, i3, d1, b2, q24)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(2, shape(x12), size(x12), '21', 1.000, &
                    x12, q24)
    deallocate (q24)

    allocate (d1(n0 + 1:n1, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s132), size(s132), '3214', s132, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s133(n1 + 1:n3, n1 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k3
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s133)
    deallocate (d1)
    deallocate (b2)
    deallocate (s132)

    call sum_stripe(4, shape(x19), size(x19), '3241', -1.000, &
                    x19, s133)

  call sum_shift(4,shape(intm),size(intm),shape(x19), &
   size(x19),(/n0-n0,n1-n0,n1-n0,n0-n0/),'3124',1.000,intm,x19)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,x19) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      + x19(m, e, b, k)*t4c(d, c, e, a, m, l, j, i) & !mebkdceamlji    (+1.000)
                      - x19(m, e, a, k)*t4c(d, c, e, b, m, l, j, i) & !meakdcebmlji    (-1.000)
                      - x19(m, e, b, l)*t4c(d, c, e, a, m, k, j, i) & !mebldceamkji    (-1.000)
                      + x19(m, e, a, l)*t4c(d, c, e, b, m, k, j, i)      !mealdcebmkji    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x19)

    allocate (d1(n0 + 1:n2, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s133), size(s133), '4213', s133, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s197(n2 + 1:n3, n1 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k3
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s197)
    deallocate (d1)
    deallocate (b2)
    deallocate (s133)

    call sum_stripe(4, shape(x2), size(x2), '2134', 1.000, x2, &
                    s197)
    deallocate (s197)

    allocate (d1(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n0 - n0, n2 - n0, n1 - n0, n0 - n0/), '1432', intm, d1)
    allocate (b2(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(t1a), size(t1a), '21', t1a, b2)
    allocate (s134(n1 + 1:n3, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    i1 = k2*k3*k4
    i2 = k3
    i3 = k1
    call egemm(i1, i2, i3, d1, b2, s134)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s134), size(s134), '2431', s134, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q27(n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q27)
    deallocate (d1)
    deallocate (b2)

    x11 = x11 - q27
    deallocate (q27)

  call sum_shift(2,shape(fockr),size(fockr),shape(x11), &
   size(x11),(/n1,n1/),'21',1.000,fockr,x11)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x11,t4c) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x11(e, b)*t4c(d, c, e, a, l, k, j, i) & !ebdcealkji      (+1.000)
                      - x11(e, a)*t4c(d, c, e, b, l, k, j, i)          !eadceblkji      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x11)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    call reorder_stripe(4, shape(s134), size(s134), '4231', s134, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s135(n2 + 1:n3, n2 + 1:n3, n1 + 1:n3, n1 + 1:n3))
    i1 = k3*k3*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s135)
    deallocate (d1)
    deallocate (b2)
    deallocate (s134)

    call sum_stripe(4, shape(x20), size(x20), '3124', 1.000, &
                    x20, s135)
    deallocate (s135)

  call sum_shift(4,shape(intm),size(intm),shape(x20), &
   size(x20),(/n2-n0,n1-n0,n2-n0,n1-n0/),'4321',1.000,intm,x20)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x20,t4c) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3; do f = n2 + 1, n3
                sum = sum &
                      + x20(f, e, d, b)*t4c(f, c, e, a, l, k, j, i) & !fedbfcealkji    (+1.000)
                      - x20(f, e, d, a)*t4c(f, c, e, b, l, k, j, i) & !fedafceblkji    (-1.000)
                      - x20(f, e, c, b)*t4c(f, d, e, a, l, k, j, i) & !fecbfdealkji    (-1.000)
                      + x20(f, e, c, a)*t4c(f, d, e, b, l, k, j, i)      !fecafdeblkji    (+1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x20)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n2 + 1:n3, n0 + 1:n2))
    call reorder_shift(4, shape(intm), size(intm), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n2 - n0, n0 - n0/), '3142', intm, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q25(n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4
    i3 = k1*k3
    call egemm1(i1, i3, d1, b2, q25)
    deallocate (d1)
    deallocate (b2)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(q25), size(q25), '21', q25, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2c), size(t2c), '3124', t2c, d2)
    allocate (s156(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s156)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                    x4, s156)
    deallocate (s156)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(q25), size(q25), '21', q25, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s152(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s152)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x8), size(x8), '2341', -1.000, &
                    x8, s152)
    deallocate (s152)

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

    call sum_stripe(2, shape(x13), size(x13), '21', -1.000, &
                    x13, q26)
    deallocate (q26)

    allocate (d1(n1 + 1:n3, n0 + 1:n1, n1 + 1:n3, n0 + 1:n1))
    call reorder_shift(4, shape(intr), size(intr), shape(d1), size(d1), &
                       (/n1 - n0, n0 - n0, n1 - n0, n0 - n0/), '3142', intr, d1)
    allocate (b2(n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(2, shape(t1a), size(t1a), '12', t1a, b2)
    allocate (q28(n1 + 1:n3, n0 + 1:n1))
    i1 = k1*k3
    i3 = k1*k3
    call egemm1(i1, i3, d1, b2, q28)
    deallocate (d1)
    deallocate (b2)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q28), size(q28), '21', q28, b1)
    allocate (d2(n0 + 1:n1, n2 + 1:n3, n1 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(t2b), size(t2b), '4123', t2b, d2)
    allocate (s145(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s145)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s145)
    deallocate (s145)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q28), size(q28), '21', q28, b1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s141(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s141)
    deallocate (b1)
    deallocate (d2)
    deallocate (q28)

    call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                    x6, s141)
    deallocate (s141)

    allocate (d1(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_shift(4, shape(intb), size(intb), shape(d1), size(d1), &
                       (/n2 - n0, n2 - n0, n0 - n0, n0 - n0/), '3412', intb, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s165(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s165)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s165), size(s165), '3214', s165, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (q29(n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2
    i3 = k4*k2
    call egemm1(i1, i3, d1, b2, q29)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(2, shape(x12), size(x12), '21', -1.000, &
                    x12, q29)
    deallocate (q29)

  call sum_shift(2,shape(fockb),size(fockb),shape(x12), &
   size(x12),(/n0,n0/),'12',1.000,fockb,x12)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x12,t4c) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x12(m, k)*t4c(d, c, b, a, m, l, j, i) & !mkdcbamlji      (+1.000)
                      - x12(m, l)*t4c(d, c, b, a, m, k, j, i)          !mldcbamkji      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x12)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s165), size(s165), '3214', s165, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s167(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s167)
    deallocate (d1)
    deallocate (b2)

    call sum_stripe(4, shape(x22), size(x22), '3241', -1.000, &
                    x22, s167)

  call sum_shift(4,shape(intb),size(intb),shape(x22), &
   size(x22),(/n0-n0,n2-n0,n2-n0,n0-n0/),'3142',1.000,intb,x22)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,x22) &
        !$omp private(a,b,c,d,m,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do m = n0 + 1, n2
                sum = sum &
                      - x22(m, e, d, k)*t4c(e, c, b, a, m, l, j, i) & !medkecbamlji    (-1.000)
                      + x22(m, e, c, k)*t4c(e, d, b, a, m, l, j, i) & !meckedbamlji    (+1.000)
                      + x22(m, e, d, l)*t4c(e, c, b, a, m, k, j, i) & !medlecbamkji    (+1.000)
                      - x22(m, e, c, l)*t4c(e, d, b, a, m, k, j, i)      !mecledbamkji    (-1.000)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x22)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(4, shape(s167), size(s167), '4213', s167, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s199(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s199)
    deallocate (d1)
    deallocate (b2)
    deallocate (s167)

    call sum_stripe(4, shape(x4), size(x4), '2134', 1.000, x4, &
                    s199)
    deallocate (s199)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s165), size(s165), '2314', s165, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (s166(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k2
    i3 = k4
    call egemm(i1, i2, i3, d1, b2, s166)
    deallocate (d1)
    deallocate (b2)
    deallocate (s165)

    call sum_stripe(4, shape(x21), size(x21), '3241', 1.000, &
                    x21, s166)

  call sum_shift(4,shape(intb),size(intb),shape(x21), &
   size(x21),(/n0-n0,n0-n0,n0-n0,n0-n0/),'2143',1.000,intb,x21)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x21,t4c) &
        !$omp private(a,b,c,d,n,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2; do n = n0 + 1, n2
                sum = sum &
                      + (x21(n, m, l, k)*t4c(d, c, b, a, n, m, j, i))/2.0d0  !nmlkdcbanmji    (+0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x21)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s166), size(s166), '2413', s166, d1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (u191(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, d1, d2, u191)
    deallocate (d1)
    deallocate (d2)

    call sum_stripe(6, shape(x49), size(x49), '236145', &
                    1.000, x49, u191)
    deallocate (u191)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,x49,t2b) &
        !$omp private(a,b,c,d,n,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do n = n0 + 1, n2
                sum = sum &
                      - x49(n, c, a, l, k, j)*t2b(d, b, n, i) & !ncalkjdbni      (-1.000)
                      - x49(n, d, a, l, k, i)*t2b(c, b, n, j) & !ndalkicbnj      (-1.000)
                      + x49(n, d, a, l, k, j)*t2b(c, b, n, i) & !ndalkjcbni      (+1.000)
                      + x49(n, c, a, l, k, i)*t2b(d, b, n, j)          !ncalkidbnj      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x49)

    allocate (d1(n0 + 1:n2, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    call reorder_stripe(4, shape(s166), size(s166), '2413', s166, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s198(n2 + 1:n3, n0 + 1:n2, n0 + 1:n2, n0 + 1:n2))
    i1 = k2*k2*k2
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s198)
    deallocate (d1)
    deallocate (b2)
    deallocate (s166)

    call sum_stripe(4, shape(x3), size(x3), '2134', -1.000, &
                    x3, s198)
    deallocate (s198)

  call sum_shift(4,shape(intb),size(intb),shape(x3), &
   size(x3),(/n0-n0,n2-n0,n0-n0,n0-n0/),'2143',1.000,intb,x3)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x3) &
        !$omp private(a,b,c,d,m,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do m = n0 + 1, n2
                sum = sum &
                      + x3(m, d, l, k)*t3b(c, b, a, m, j, i) & !mdlkcbamji      (+1.000)
                      - x3(m, c, l, k)*t3b(d, b, a, m, j, i)           !mclkdbamji      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
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
    allocate (s168(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n0 + 1:n2))
    i1 = k2*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s168)
    deallocate (d1)
    deallocate (b2)

    allocate (d1(n2 + 1:n3, n0 + 1:n2, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s168), size(s168), '3421', s168, d1)
    allocate (b2(n2 + 1:n3, n0 + 1:n2))
    call reorder_stripe(2, shape(t1b), size(t1b), '12', t1b, b2)
    allocate (q30(n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4
    i3 = k2*k4
    call egemm1(i1, i3, d1, b2, q30)
    deallocate (d1)
    deallocate (b2)

    x13 = x13 + q30
    deallocate (q30)

  call sum_shift(2,shape(fockb),size(fockb),shape(x13), &
   size(x13),(/n2,n2/),'21',1.000,fockb,x13)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,x13) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      + x13(e, d)*t4c(e, c, b, a, l, k, j, i) & !edecbalkji      (+1.000)
                      - x13(e, c)*t4c(e, d, b, a, l, k, j, i)          !ecedbalkji      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x13)

    allocate (d1(n0 + 1:n2, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    call reorder_stripe(4, shape(s168), size(s168), '4231', s168, d1)
    allocate (b2(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(t1b), size(t1b), '21', t1b, b2)
    allocate (s169(n2 + 1:n3, n2 + 1:n3, n2 + 1:n3, n2 + 1:n3))
    i1 = k4*k4*k4
    i2 = k4
    i3 = k2
    call egemm(i1, i2, i3, d1, b2, s169)
    deallocate (d1)
    deallocate (b2)
    deallocate (s168)

    call sum_stripe(4, shape(x23), size(x23), '3124', 1.000, &
                    x23, s169)
    deallocate (s169)

  call sum_shift(4,shape(intb),size(intb),shape(x23), &
   size(x23),(/n2-n0,n2-n0,n2-n0,n2-n0/),'4321',1.000,intb,x23)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t4c,x23) &
        !$omp private(a,b,c,d,f,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3; do f = n2 + 1, n3
                sum = sum &
                      + (x23(f, e, d, c)*t4c(f, e, b, a, l, k, j, i))/2.0d0 !fedcfebalkji    (+0.500)
            end do; end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x23)

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
    allocate (s181(n2 + 1:n3, n1 + 1:n3, n0 + 1:n2, n1 + 1:n3))
    i1 = k3
    i2 = k2*k3*k4
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s181)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x2), size(x2), '2341', -1.000, &
                    x2, s181)
    deallocate (s181)

  call sum_shift(4,shape(intm),size(intm),shape(x2), &
   size(x2),(/n1-n0,n2-n0,n1-n0,n0-n0/),'3214',1.000,intm,x2)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x2) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      - x2(e, d, b, k)*t3b(c, e, a, l, j, i) & !edbkcealji      (-1.000)
                      + x2(e, d, a, k)*t3b(c, e, b, l, j, i) & !edakceblji      (+1.000)
                      + x2(e, c, b, k)*t3b(d, e, a, l, j, i) & !ecbkdealji      (+1.000)
                      - x2(e, c, a, k)*t3b(d, e, b, l, j, i) & !ecakdeblji      (-1.000)
                      + x2(e, d, b, l)*t3b(c, e, a, k, j, i) & !edblceakji      (+1.000)
                      - x2(e, d, a, l)*t3b(c, e, b, k, j, i) & !edalcebkji      (-1.000)
                      - x2(e, c, b, l)*t3b(d, e, a, k, j, i) & !ecbldeakji      (-1.000)
                      + x2(e, c, a, l)*t3b(d, e, b, k, j, i)           !ecaldebkji      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x2)

    allocate (b1(n0 + 1:n1, n1 + 1:n3))
    call reorder_stripe(2, shape(q31), size(q31), '21', q31, b1)
    allocate (d2(n0 + 1:n1, n1 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2a), size(t2a), '3124', t2a, d2)
    allocate (s173(n1 + 1:n3, n1 + 1:n3, n0 + 1:n1, n1 + 1:n3))
    i1 = k3
    i2 = k1*k3*k3
    i3 = k1
    call egemm(i1, i2, i3, b1, d2, s173)
    deallocate (b1)
    deallocate (d2)
    deallocate (q31)

    call sum_stripe(4, shape(x6), size(x6), '2341', -1.000, &
                    x6, s173)
    deallocate (s173)

  call sum_shift(4,shape(intr),size(intr),shape(x6), &
   size(x6),(/n1-n0,n1-n0,n1-n0,n0-n0/),'3241',1.000,intr,x6)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x6) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n1 + 1, n3
                sum = sum &
                      + x6(e, b, a, i)*t3c(d, c, e, l, k, j) & !ebaidcelkj      (+1.000)
                      - x6(e, b, a, j)*t3c(d, c, e, l, k, i)           !ebajdcelki      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x6)

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
    allocate (s191(n2 + 1:n3, n2 + 1:n3, n0 + 1:n2, n2 + 1:n3))
    i1 = k4
    i2 = k2*k4*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s191)
    deallocate (b1)
    deallocate (d2)

    call sum_stripe(4, shape(x4), size(x4), '2341', -1.000, &
                    x4, s191)
    deallocate (s191)

  call sum_shift(4,shape(intb),size(intb),shape(x4), &
   size(x4),(/n2-n0,n2-n0,n2-n0,n0-n0/),'3241',1.000,intb,x4)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3b,x4) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      + x4(e, d, c, k)*t3b(e, b, a, l, j, i) & !edckebalji      (+1.000)
                      - x4(e, d, c, l)*t3b(e, b, a, k, j, i)           !edclebakji      (-1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x4)

    allocate (b1(n0 + 1:n2, n2 + 1:n3))
    call reorder_stripe(2, shape(q32), size(q32), '21', q32, b1)
    allocate (d2(n0 + 1:n2, n2 + 1:n3, n1 + 1:n3, n0 + 1:n1))
    call reorder_stripe(4, shape(t2b), size(t2b), '3124', t2b, d2)
    allocate (s185(n2 + 1:n3, n1 + 1:n3, n0 + 1:n1, n2 + 1:n3))
    i1 = k4
    i2 = k1*k3*k4
    i3 = k2
    call egemm(i1, i2, i3, b1, d2, s185)
    deallocate (b1)
    deallocate (d2)
    deallocate (q32)

    call sum_stripe(4, shape(x8), size(x8), '2341', -1.000, &
                    x8, s185)
    deallocate (s185)

  call sum_shift(4,shape(intm),size(intm),shape(x8), &
   size(x8),(/n2-n0,n2-n0,n1-n0,n0-n0/),'3241',1.000,intm,x8)

    do i = n0 + 1, n1 - 1; do j = i + 1, n1; do k = n0 + 1, n2 - 1; do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        !$omp parallel default(none), &
        !$omp shared(n0,n1,n2,n3,i,j,k,l,indunocc,v4c,t3c,x8) &
        !$omp private(a,b,c,d,e,sum)
        !$omp do
        do a = n1 + 1, n3 - 1; do b = a + 1, n3; do c = n2 + 1, n3 - 1; do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            sum = 0.0d0
            do e = n2 + 1, n3
                sum = sum &
                      - x8(e, d, b, i)*t3c(e, c, a, l, k, j) & !edbiecalkj      (-1.000)
                      + x8(e, d, a, i)*t3c(e, c, b, l, k, j) & !edaiecblkj      (+1.000)
                      + x8(e, c, b, i)*t3c(e, d, a, l, k, j) & !ecbiedalkj      (+1.000)
                      - x8(e, c, a, i)*t3c(e, d, b, l, k, j) & !ecaiedblkj      (-1.000)
                      + x8(e, d, b, j)*t3c(e, c, a, l, k, i) & !edbjecalki      (+1.000)
                      - x8(e, d, a, j)*t3c(e, c, b, l, k, i) & !edajecblki      (-1.000)
                      - x8(e, c, b, j)*t3c(e, d, a, l, k, i) & !ecbjedalki      (-1.000)
                      + x8(e, c, a, j)*t3c(e, d, b, l, k, i)           !ecajedblki      (+1.000)
            end do
            v4c(d, c, b, a, l, k, j, i) = v4c(d, c, b, a, l, k, j, i) + sum
        end do; end do; end do; end do
        !$omp end do
        !$omp end parallel
    end do; end do; end do; end do

    deallocate (x8)

    do i = n0 + 1, n1 - 1
    do j = i + 1, n1
    do k = n0 + 1, n2 - 1
    do l = k + 1, n2
        if (indocc(l, k, j, i) .eq. 1) cycle
        do a = n1 + 1, n3 - 1
        do b = a + 1, n3
        do c = n2 + 1, n3 - 1
        do d = c + 1, n3
            if (indunocc(d, c, b, a) .eq. 1) cycle
            coeleft = fockb(d, d) &
                      + fockb(c, c) &
                      + fockr(b, b) &
                      + fockr(a, a) &
                      - fockb(l, l) &
                      - fockb(k, k) &
                      - fockr(j, j) &
                      - fockr(i, i) &
                      + shift
            t4c(d, c, b, a, l, k, j, i) = &
                t4c(d, c, b, a, l, k, j, i) - v4c(d, c, b, a, l, k, j, i)/coeleft
            t4c(d, c, b, a, l, k, i, j) = -t4c(d, c, b, a, l, k, j, i)
            t4c(d, c, b, a, k, l, j, i) = -t4c(d, c, b, a, l, k, j, i)
            t4c(d, c, b, a, k, l, i, j) = t4c(d, c, b, a, l, k, j, i)
            t4c(d, c, a, b, l, k, j, i) = -t4c(d, c, b, a, l, k, j, i)
            t4c(d, c, a, b, l, k, i, j) = t4c(d, c, b, a, l, k, j, i)
            t4c(d, c, a, b, k, l, j, i) = t4c(d, c, b, a, l, k, j, i)
            t4c(d, c, a, b, k, l, i, j) = -t4c(d, c, b, a, l, k, j, i)
            t4c(c, d, b, a, l, k, j, i) = -t4c(d, c, b, a, l, k, j, i)
            t4c(c, d, b, a, l, k, i, j) = t4c(d, c, b, a, l, k, j, i)
            t4c(c, d, b, a, k, l, j, i) = t4c(d, c, b, a, l, k, j, i)
            t4c(c, d, b, a, k, l, i, j) = -t4c(d, c, b, a, l, k, j, i)
            t4c(c, d, a, b, l, k, j, i) = t4c(d, c, b, a, l, k, j, i)
            t4c(c, d, a, b, l, k, i, j) = -t4c(d, c, b, a, l, k, j, i)
            t4c(c, d, a, b, k, l, j, i) = -t4c(d, c, b, a, l, k, j, i)
            t4c(c, d, a, b, k, l, i, j) = t4c(d, c, b, a, l, k, j, i)
        end do
        end do
        end do
        end do
    end do
    end do
    end do
    end do
    rewind (tc)
    write (tc) t4c

end subroutine t4c_update

