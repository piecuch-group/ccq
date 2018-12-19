       subroutine t4b_update(n0,n1,n2,n3,k1,k2,k3,k4,shift,
     & fockr,fockb,intr,intb,intm,t1a,t1b,t2a,t2b,t2c,t3a,t3b,t3c,t3d,
     & iactocca,iactoccb,iactunoa,iactunob,iactindq)
!    & t4a,t4b,t4c,t4d,t4e)
c
       integer a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p
       integer iactocca,iactoccb,iactunoa,iactunob,iactindq
       integer iocca,ioccb,iunoa,iunob
       integer, allocatable:: indocc(:,:,:,:)
       integer, allocatable:: indunocc(:,:,:,:)
       real*8 shift,pp,coeleft,timt1,timt2
       real*8 fockr(n3,n3)
       real*8 fockb(n3,n3)
       real*8 sum
       real*8 intr(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 intb(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 intm(n0+1:n3,n0+1:n3,n0+1:n3,n0+1:n3)
       real*8 t1a(n1+1:n3,n0+1:n1)
       real*8 t1b(n2+1:n3,n0+1:n2)
       real*8 t2a(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1)
       real*8 t2b(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1)
       real*8 t2c(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2)
       real*8 t3a(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1)
       real*8 t3b(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1)
       real*8 t3c(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1)
       real*8 t3d(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2)
       real*8,allocatable::t4a(:,:,:,:,:,:,:,:)
!       real*8 t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1)
       real*8,allocatable::t4b(:,:,:,:,:,:,:,:)
!       real*8 t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1)
       real*8,allocatable::t4c(:,:,:,:,:,:,:,:)
!       real*8 t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1)
!       real*8,allocatable::t4d(:,:,:,:,:,:,:,:)
!       real*8 t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1)
!       real*8,allocatable::t4e(:,:,:,:,:,:,:,:)
!       real*8 t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2)
       real*8,allocatable::v4b(:,:,:,:,:,:,:,:)
!       real*8 v4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1)
c
       real*8,allocatable::b1(:,:)
       real*8,allocatable::b2(:,:)
       real*8,allocatable::d1(:,:,:,:)
       real*8,allocatable::d2(:,:,:,:)
       real*8,allocatable::f1(:,:,:,:,:,:)
       real*8,allocatable::f2(:,:,:,:,:,:)
       real*8,allocatable::h2(:,:,:,:,:,:,:,:)
c
       integer ta,tb,tc,td,te
       parameter(ta=29,tb=30,tc=31,td=32,te=33)
c
       real*8,allocatable::s1(:,:,:,:)
       real*8,allocatable::s2(:,:,:,:)
       real*8,allocatable::s3(:,:,:,:)
       real*8,allocatable::s4(:,:,:,:)
       real*8,allocatable::s5(:,:,:,:)
       real*8,allocatable::s6(:,:,:,:)
       real*8,allocatable::s7(:,:,:,:)
       real*8,allocatable::s8(:,:,:,:)
       real*8,allocatable::s9(:,:,:,:)
       real*8,allocatable::s10(:,:,:,:)
       real*8,allocatable::q1(:,:)
       real*8,allocatable::q2(:,:)
       real*8,allocatable::s11(:,:,:,:)
       real*8,allocatable::u55(:,:,:,:,:,:)
       real*8,allocatable::s89(:,:,:,:)
       real*8,allocatable::s12(:,:,:,:)
       real*8,allocatable::u57(:,:,:,:,:,:)
       real*8,allocatable::u56(:,:,:,:,:,:)
       real*8,allocatable::s90(:,:,:,:)
       real*8,allocatable::q3(:,:)
       real*8,allocatable::s13(:,:,:,:)
       real*8,allocatable::u59(:,:,:,:,:,:)
       real*8,allocatable::u58(:,:,:,:,:,:)
       real*8,allocatable::s92(:,:,:,:)
       real*8,allocatable::s91(:,:,:,:)
       real*8,allocatable::s14(:,:,:,:)
       real*8,allocatable::u60(:,:,:,:,:,:)
       real*8,allocatable::q4(:,:)
       real*8,allocatable::s15(:,:,:,:)
       real*8,allocatable::u61(:,:,:,:,:,:)
       real*8,allocatable::s102(:,:,:,:)
       real*8,allocatable::s93(:,:,:,:)
       real*8,allocatable::s16(:,:,:,:)
       real*8,allocatable::u62(:,:,:,:,:,:)
       real*8,allocatable::s103(:,:,:,:)
       real*8,allocatable::s94(:,:,:,:)
       real*8,allocatable::s17(:,:,:,:)
       real*8,allocatable::u63(:,:,:,:,:,:)
       real*8,allocatable::s104(:,:,:,:)
       real*8,allocatable::q5(:,:)
       real*8,allocatable::s18(:,:,:,:)
       real*8,allocatable::u64(:,:,:,:,:,:)
       real*8,allocatable::s105(:,:,:,:)
       real*8,allocatable::q6(:,:)
       real*8,allocatable::s19(:,:,:,:)
       real*8,allocatable::u71(:,:,:,:,:,:)
       real*8,allocatable::s107(:,:,:,:)
       real*8,allocatable::s106(:,:,:,:)
       real*8,allocatable::s20(:,:,:,:)
       real*8,allocatable::u72(:,:,:,:,:,:)
       real*8,allocatable::s109(:,:,:,:)
       real*8,allocatable::s108(:,:,:,:)
       real*8,allocatable::s21(:,:,:,:)
       real*8,allocatable::s22(:,:,:,:)
       real*8,allocatable::s23(:,:,:,:)
       real*8,allocatable::s24(:,:,:,:)
       real*8,allocatable::s25(:,:,:,:)
       real*8,allocatable::s26(:,:,:,:)
       real*8,allocatable::s27(:,:,:,:)
       real*8,allocatable::u83(:,:,:,:,:,:)
       real*8,allocatable::s28(:,:,:,:)
       real*8,allocatable::u84(:,:,:,:,:,:)
       real*8,allocatable::q7(:,:)
       real*8,allocatable::q8(:,:)
       real*8,allocatable::s29(:,:,:,:)
       real*8,allocatable::u85(:,:,:,:,:,:)
       real*8,allocatable::s136(:,:,:,:)
       real*8,allocatable::s30(:,:,:,:)
       real*8,allocatable::u86(:,:,:,:,:,:)
       real*8,allocatable::q9(:,:)
       real*8,allocatable::s31(:,:,:,:)
       real*8,allocatable::u87(:,:,:,:,:,:)
       real*8,allocatable::s137(:,:,:,:)
       real*8,allocatable::s32(:,:,:,:)
       real*8,allocatable::u88(:,:,:,:,:,:)
       real*8,allocatable::q10(:,:)
       real*8,allocatable::s33(:,:,:,:)
       real*8,allocatable::q11(:,:)
       real*8,allocatable::s34(:,:,:,:)
       real*8,allocatable::q12(:,:)
       real*8,allocatable::u1(:,:,:,:,:,:)
       real*8,allocatable::u2(:,:,:,:,:,:)
       real*8,allocatable::u3(:,:,:,:,:,:)
       real*8,allocatable::u4(:,:,:,:,:,:)
       real*8,allocatable::u5(:,:,:,:,:,:)
       real*8,allocatable::u6(:,:,:,:,:,:)
       real*8,allocatable::u7(:,:,:,:,:,:)
       real*8,allocatable::u8(:,:,:,:,:,:)
       real*8,allocatable::u9(:,:,:,:,:,:)
       real*8,allocatable::s35(:,:,:,:)
       real*8,allocatable::s36(:,:,:,:)
       real*8,allocatable::u10(:,:,:,:,:,:)
       real*8,allocatable::u11(:,:,:,:,:,:)
       real*8,allocatable::s37(:,:,:,:)
       real*8,allocatable::s38(:,:,:,:)
       real*8,allocatable::u12(:,:,:,:,:,:)
       real*8,allocatable::s39(:,:,:,:)
       real*8,allocatable::u13(:,:,:,:,:,:)
       real*8,allocatable::s40(:,:,:,:)
       real*8,allocatable::u14(:,:,:,:,:,:)
       real*8,allocatable::u15(:,:,:,:,:,:)
       real*8,allocatable::u16(:,:,:,:,:,:)
       real*8,allocatable::s41(:,:,:,:)
       real*8,allocatable::u17(:,:,:,:,:,:)
       real*8,allocatable::s42(:,:,:,:)
       real*8,allocatable::u18(:,:,:,:,:,:)
       real*8,allocatable::u19(:,:,:,:,:,:)
       real*8,allocatable::u20(:,:,:,:,:,:)
       real*8,allocatable::u100(:,:,:,:,:,:)
       real*8,allocatable::u98(:,:,:,:,:,:)
       real*8,allocatable::u97(:,:,:,:,:,:)
       real*8,allocatable::s119(:,:,:,:)
       real*8,allocatable::u67(:,:,:,:,:,:)
       real*8,allocatable::u115(:,:,:,:,:,:)
       real*8,allocatable::u65(:,:,:,:,:,:)
       real*8,allocatable::u113(:,:,:,:,:,:)
       real*8,allocatable::s43(:,:,:,:)
       real*8,allocatable::u99(:,:,:,:,:,:)
       real*8,allocatable::s118(:,:,:,:)
       real*8,allocatable::u21(:,:,:,:,:,:)
       real*8,allocatable::s44(:,:,:,:)
       real*8,allocatable::u101(:,:,:,:,:,:)
       real*8,allocatable::s120(:,:,:,:)
       real*8,allocatable::s116(:,:,:,:)
       real*8,allocatable::q13(:,:)
       real*8,allocatable::s45(:,:,:,:)
       real*8,allocatable::s117(:,:,:,:)
       real*8,allocatable::q14(:,:)
       real*8,allocatable::u22(:,:,:,:,:,:)
       real*8,allocatable::u105(:,:,:,:,:,:)
       real*8,allocatable::u104(:,:,:,:,:,:)
       real*8,allocatable::u103(:,:,:,:,:,:)
       real*8,allocatable::u102(:,:,:,:,:,:)
       real*8,allocatable::s141(:,:,:,:)
       real*8,allocatable::u90(:,:,:,:,:,:)
       real*8,allocatable::u89(:,:,:,:,:,:)
       real*8,allocatable::u121(:,:,:,:,:,:)
       real*8,allocatable::u70(:,:,:,:,:,:)
       real*8,allocatable::u120(:,:,:,:,:,:)
       real*8,allocatable::u119(:,:,:,:,:,:)
       real*8,allocatable::u23(:,:,:,:,:,:)
       real*8,allocatable::s46(:,:,:,:)
       real*8,allocatable::u109(:,:,:,:,:,:)
       real*8,allocatable::s143(:,:,:,:)
       real*8,allocatable::s142(:,:,:,:)
       real*8,allocatable::u24(:,:,:,:,:,:)
       real*8,allocatable::s47(:,:,:,:)
       real*8,allocatable::s48(:,:,:,:)
       real*8,allocatable::u25(:,:,:,:,:,:)
       real*8,allocatable::u26(:,:,:,:,:,:)
       real*8,allocatable::s49(:,:,:,:)
       real*8,allocatable::u27(:,:,:,:,:,:)
       real*8,allocatable::u28(:,:,:,:,:,:)
       real*8,allocatable::s50(:,:,:,:)
       real*8,allocatable::u29(:,:,:,:,:,:)
       real*8,allocatable::s51(:,:,:,:)
       real*8,allocatable::u30(:,:,:,:,:,:)
       real*8,allocatable::s52(:,:,:,:)
       real*8,allocatable::s53(:,:,:,:)
       real*8,allocatable::s54(:,:,:,:)
       real*8,allocatable::s55(:,:,:,:)
       real*8,allocatable::s56(:,:,:,:)
       real*8,allocatable::u31(:,:,:,:,:,:)
       real*8,allocatable::u32(:,:,:,:,:,:)
       real*8,allocatable::s57(:,:,:,:)
       real*8,allocatable::u33(:,:,:,:,:,:)
       real*8,allocatable::s58(:,:,:,:)
       real*8,allocatable::s59(:,:,:,:)
       real*8,allocatable::u34(:,:,:,:,:,:)
       real*8,allocatable::u35(:,:,:,:,:,:)
       real*8,allocatable::s60(:,:,:,:)
       real*8,allocatable::u36(:,:,:,:,:,:)
       real*8,allocatable::s61(:,:,:,:)
       real*8,allocatable::s62(:,:,:,:)
       real*8,allocatable::u37(:,:,:,:,:,:)
       real*8,allocatable::s63(:,:,:,:)
       real*8,allocatable::u38(:,:,:,:,:,:)
       real*8,allocatable::s64(:,:,:,:)
       real*8,allocatable::u39(:,:,:,:,:,:)
       real*8,allocatable::s123(:,:,:,:)
       real*8,allocatable::u75(:,:,:,:,:,:)
       real*8,allocatable::u73(:,:,:,:,:,:)
       real*8,allocatable::u114(:,:,:,:,:,:)
       real*8,allocatable::u40(:,:,:,:,:,:)
       real*8,allocatable::s65(:,:,:,:)
       real*8,allocatable::s124(:,:,:,:)
       real*8,allocatable::s122(:,:,:,:)
       real*8,allocatable::u41(:,:,:,:,:,:)
       real*8,allocatable::u106(:,:,:,:,:,:)
       real*8,allocatable::s147(:,:,:,:)
       real*8,allocatable::u80(:,:,:,:,:,:)
       real*8,allocatable::u42(:,:,:,:,:,:)
       real*8,allocatable::u107(:,:,:,:,:,:)
       real*8,allocatable::u94(:,:,:,:,:,:)
       real*8,allocatable::s130(:,:,:,:)
       real*8,allocatable::u81(:,:,:,:,:,:)
       real*8,allocatable::u77(:,:,:,:,:,:)
       real*8,allocatable::u118(:,:,:,:,:,:)
       real*8,allocatable::u116(:,:,:,:,:,:)
       real*8,allocatable::s66(:,:,:,:)
       real*8,allocatable::u108(:,:,:,:,:,:)
       real*8,allocatable::s146(:,:,:,:)
       real*8,allocatable::s129(:,:,:,:)
       real*8,allocatable::u43(:,:,:,:,:,:)
       real*8,allocatable::s67(:,:,:,:)
       real*8,allocatable::s145(:,:,:,:)
       real*8,allocatable::s131(:,:,:,:)
       real*8,allocatable::s68(:,:,:,:)
       real*8,allocatable::s133(:,:,:,:)
       real*8,allocatable::s126(:,:,:,:)
       real*8,allocatable::q15(:,:)
       real*8,allocatable::u44(:,:,:,:,:,:)
       real*8,allocatable::s69(:,:,:,:)
       real*8,allocatable::s70(:,:,:,:)
       real*8,allocatable::s149(:,:,:,:)
       real*8,allocatable::s127(:,:,:,:)
       real*8,allocatable::q16(:,:)
       real*8,allocatable::s71(:,:,:,:)
       real*8,allocatable::s148(:,:,:,:)
       real*8,allocatable::s128(:,:,:,:)
       real*8,allocatable::q17(:,:)
       real*8,allocatable::q18(:,:)
       real*8,allocatable::u45(:,:,:,:,:,:)
       real*8,allocatable::u111(:,:,:,:,:,:)
       real*8,allocatable::u110(:,:,:,:,:,:)
       real*8,allocatable::s152(:,:,:,:)
       real*8,allocatable::u96(:,:,:,:,:,:)
       real*8,allocatable::u46(:,:,:,:,:,:)
       real*8,allocatable::s72(:,:,:,:)
       real*8,allocatable::s153(:,:,:,:)
       real*8,allocatable::s151(:,:,:,:)
       real*8,allocatable::s73(:,:,:,:)
       real*8,allocatable::s74(:,:,:,:)
       real*8,allocatable::s75(:,:,:,:)
       real*8,allocatable::s135(:,:,:,:)
       real*8,allocatable::s134(:,:,:,:)
       real*8,allocatable::s76(:,:,:,:)
       real*8,allocatable::q19(:,:)
       real*8,allocatable::q20(:,:)
       real*8,allocatable::s77(:,:,:,:)
       real*8,allocatable::u47(:,:,:,:,:,:)
       real*8,allocatable::u76(:,:,:,:,:,:)
       real*8,allocatable::s78(:,:,:,:)
       real*8,allocatable::u48(:,:,:,:,:,:)
       real*8,allocatable::u74(:,:,:,:,:,:)
       real*8,allocatable::s79(:,:,:,:)
       real*8,allocatable::u49(:,:,:,:,:,:)
       real*8,allocatable::u68(:,:,:,:,:,:)
       real*8,allocatable::s80(:,:,:,:)
       real*8,allocatable::s81(:,:,:,:)
       real*8,allocatable::s82(:,:,:,:)
       real*8,allocatable::u50(:,:,:,:,:,:)
       real*8,allocatable::u93(:,:,:,:,:,:)
       real*8,allocatable::s83(:,:,:,:)
       real*8,allocatable::s84(:,:,:,:)
       real*8,allocatable::u51(:,:,:,:,:,:)
       real*8,allocatable::u92(:,:,:,:,:,:)
       real*8,allocatable::u82(:,:,:,:,:,:)
       real*8,allocatable::s85(:,:,:,:)
       real*8,allocatable::u52(:,:,:,:,:,:)
       real*8,allocatable::u78(:,:,:,:,:,:)
       real*8,allocatable::u53(:,:,:,:,:,:)
       real*8,allocatable::s86(:,:,:,:)
       real*8,allocatable::s87(:,:,:,:)
       real*8,allocatable::s88(:,:,:,:)
       real*8,allocatable::u54(:,:,:,:,:,:)
       real*8,allocatable::u95(:,:,:,:,:,:)
       real*8,allocatable::s95(:,:,:,:)
       real*8,allocatable::u66(:,:,:,:,:,:)
       real*8,allocatable::q21(:,:)
       real*8,allocatable::s97(:,:,:,:)
       real*8,allocatable::s156(:,:,:,:)
       real*8,allocatable::s96(:,:,:,:)
       real*8,allocatable::u112(:,:,:,:,:,:)
       real*8,allocatable::s155(:,:,:,:)
       real*8,allocatable::s98(:,:,:,:)
       real*8,allocatable::q22(:,:)
       real*8,allocatable::s99(:,:,:,:)
       real*8,allocatable::s100(:,:,:,:)
       real*8,allocatable::u79(:,:,:,:,:,:)
       real*8,allocatable::u69(:,:,:,:,:,:)
       real*8,allocatable::q23(:,:)
       real*8,allocatable::s111(:,:,:,:)
       real*8,allocatable::s110(:,:,:,:)
       real*8,allocatable::u117(:,:,:,:,:,:)
       real*8,allocatable::s159(:,:,:,:)
       real*8,allocatable::s101(:,:,:,:)
       real*8,allocatable::s158(:,:,:,:)
       real*8,allocatable::s157(:,:,:,:)
       real*8,allocatable::s112(:,:,:,:)
       real*8,allocatable::u91(:,:,:,:,:,:)
       real*8,allocatable::s138(:,:,:,:)
       real*8,allocatable::q24(:,:)
       real*8,allocatable::s113(:,:,:,:)
       real*8,allocatable::s160(:,:,:,:)
       real*8,allocatable::s114(:,:,:,:)
       real*8,allocatable::q27(:,:)
       real*8,allocatable::s115(:,:,:,:)
       real*8,allocatable::q25(:,:)
       real*8,allocatable::s132(:,:,:,:)
       real*8,allocatable::q26(:,:)
       real*8,allocatable::q28(:,:)
       real*8,allocatable::s125(:,:,:,:)
       real*8,allocatable::s121(:,:,:,:)
       real*8,allocatable::s139(:,:,:,:)
       real*8,allocatable::q29(:,:)
       real*8,allocatable::s140(:,:,:,:)
       real*8,allocatable::q30(:,:)
       real*8,allocatable::s154(:,:,:,:)
       real*8,allocatable::q31(:,:)
       real*8,allocatable::q32(:,:)
       real*8,allocatable::s150(:,:,:,:)
       real*8,allocatable::s144(:,:,:,:)
       real*8,allocatable::x1(:,:,:,:)
       real*8,allocatable::z1(:,:,:,:,:,:,:,:)
       real*8,allocatable::x2(:,:,:,:)
       real*8,allocatable::z2(:,:,:,:,:,:,:,:)
       real*8,allocatable::x3(:,:,:,:)
       real*8,allocatable::z3(:,:,:,:,:,:,:,:)
       real*8,allocatable::x4(:,:,:,:)
       real*8,allocatable::z4(:,:,:,:,:,:,:,:)
       real*8,allocatable::x5(:,:,:,:)
       real*8,allocatable::z5(:,:,:,:,:,:,:,:)
       real*8,allocatable::x6(:,:,:,:)
       real*8,allocatable::z6(:,:,:,:,:,:,:,:)
       real*8,allocatable::x7(:,:,:,:)
       real*8,allocatable::z7(:,:,:,:,:,:,:,:)
       real*8,allocatable::x8(:,:)
       real*8,allocatable::z8(:,:,:,:,:,:,:,:)
       real*8,allocatable::x9(:,:)
       real*8,allocatable::z9(:,:,:,:,:,:,:,:)
       real*8,allocatable::x10(:,:)
       real*8,allocatable::z10(:,:,:,:,:,:,:,:)
       real*8,allocatable::x11(:,:)
       real*8,allocatable::z11(:,:,:,:,:,:,:,:)
       real*8,allocatable::x12(:,:,:,:)
       real*8,allocatable::z12(:,:,:,:,:,:,:,:)
       real*8,allocatable::x13(:,:,:,:)
       real*8,allocatable::z13(:,:,:,:,:,:,:,:)
       real*8,allocatable::x14(:,:,:,:)
       real*8,allocatable::z14(:,:,:,:,:,:,:,:)
       real*8,allocatable::x15(:,:,:,:)
       real*8,allocatable::z15(:,:,:,:,:,:,:,:)
       real*8,allocatable::x16(:,:,:,:)
       real*8,allocatable::z16(:,:,:,:,:,:,:,:)
       real*8,allocatable::x17(:,:,:,:)
       real*8,allocatable::z17(:,:,:,:,:,:,:,:)
       real*8,allocatable::x18(:,:,:,:)
       real*8,allocatable::z18(:,:,:,:,:,:,:,:)
       real*8,allocatable::x19(:,:,:,:)
       real*8,allocatable::z19(:,:,:,:,:,:,:,:)
       real*8,allocatable::x20(:,:,:,:)
       real*8,allocatable::z20(:,:,:,:,:,:,:,:)
       real*8,allocatable::x21(:,:,:,:)
       real*8,allocatable::z24(:,:,:,:,:,:,:,:)
       real*8,allocatable::x22(:,:,:,:)
       real*8,allocatable::z25(:,:,:,:,:,:,:,:)
       real*8,allocatable::z211(:,:,:,:,:,:,:,:)
       real*8,allocatable::z33(:,:,:,:,:,:,:,:)
       real*8,allocatable::x23(:,:,:,:,:,:)
       real*8,allocatable::z213(:,:,:,:,:,:,:,:)
       real*8,allocatable::x24(:,:,:,:,:,:)
       real*8,allocatable::z212(:,:,:,:,:,:,:,:)
       real*8,allocatable::z216(:,:,:,:,:,:,:,:)
       real*8,allocatable::z37(:,:,:,:,:,:,:,:)
       real*8,allocatable::x25(:,:,:,:,:,:)
       real*8,allocatable::z217(:,:,:,:,:,:,:,:)
       real*8,allocatable::x26(:,:,:,:,:,:)
       real*8,allocatable::z218(:,:,:,:,:,:,:,:)
       real*8,allocatable::x27(:,:,:,:,:,:)
       real*8,allocatable::z219(:,:,:,:,:,:,:,:)
       real*8,allocatable::x28(:,:,:,:,:,:)
       real*8,allocatable::z220(:,:,:,:,:,:,:,:)
       real*8,allocatable::x29(:,:,:,:,:,:)
       real*8,allocatable::z233(:,:,:,:,:,:,:,:)
       real*8,allocatable::x30(:,:,:,:,:,:)
       real*8,allocatable::z265(:,:,:,:,:,:,:,:)
       real*8,allocatable::x31(:,:,:,:,:,:)
       real*8,allocatable::z68(:,:,:,:,:,:,:,:)
       real*8,allocatable::x32(:,:,:,:,:,:)
       real*8,allocatable::z71(:,:,:,:,:,:,:,:)
       real*8,allocatable::x33(:,:,:,:,:,:)
       real*8,allocatable::z78(:,:,:,:,:,:,:,:)
       real*8,allocatable::x34(:,:,:,:,:,:)
       real*8,allocatable::z82(:,:,:,:,:,:,:,:)
       real*8,allocatable::z84(:,:,:,:,:,:,:,:)
       real*8,allocatable::x35(:,:,:,:,:,:)
       real*8,allocatable::z86(:,:,:,:,:,:,:,:)
       real*8,allocatable::x36(:,:,:,:,:,:)
       real*8,allocatable::z87(:,:,:,:,:,:,:,:)
       real*8,allocatable::x37(:,:,:,:,:,:)
       real*8,allocatable::z88(:,:,:,:,:,:,:,:)
       real*8,allocatable::x38(:,:,:,:,:,:)
       real*8,allocatable::z93(:,:,:,:,:,:,:,:)
       real*8,allocatable::z296(:,:,:,:,:,:,:,:)
       real*8,allocatable::x39(:,:,:,:,:,:)
       real*8,allocatable::z294(:,:,:,:,:,:,:,:)
       real*8,allocatable::z94(:,:,:,:,:,:,:,:)
       real*8,allocatable::z297(:,:,:,:,:,:,:,:)
       real*8,allocatable::z101(:,:,:,:,:,:,:,:)
       real*8,allocatable::x40(:,:,:,:,:,:)
       real*8,allocatable::z107(:,:,:,:,:,:,:,:)
       real*8,allocatable::x41(:,:,:,:,:,:)
       real*8,allocatable::z108(:,:,:,:,:,:,:,:)
       real*8,allocatable::x42(:,:,:,:,:,:)
       real*8,allocatable::z110(:,:,:,:,:,:,:,:)
       real*8,allocatable::x43(:,:,:,:,:,:)
       real*8,allocatable::z111(:,:,:,:,:,:,:,:)
       real*8,allocatable::x44(:,:,:,:,:,:)
       real*8,allocatable::z113(:,:,:,:,:,:,:,:)
       real*8,allocatable::x45(:,:,:,:,:,:)
       real*8,allocatable::z121(:,:,:,:,:,:,:,:)
       real*8,allocatable::x46(:,:,:,:,:,:)
       real*8,allocatable::z122(:,:,:,:,:,:,:,:)
       real*8,allocatable::x47(:,:,:,:,:,:)
       real*8,allocatable::z127(:,:,:,:,:,:,:,:)
       real*8,allocatable::x48(:,:,:,:,:,:)
       real*8,allocatable::z128(:,:,:,:,:,:,:,:)
       real*8,allocatable::x49(:,:,:,:,:,:)
       real*8,allocatable::z130(:,:,:,:,:,:,:,:)
       real*8,allocatable::z137(:,:,:,:,:,:,:,:)
       real*8,allocatable::x50(:,:,:,:,:,:)
       real*8,allocatable::z138(:,:,:,:,:,:,:,:)
       real*8,allocatable::z140(:,:,:,:,:,:,:,:)
       real*8,allocatable::z141(:,:,:,:,:,:,:,:)
       real*8,allocatable::z307(:,:,:,:,:,:,:,:)
       real*8,allocatable::z154(:,:,:,:,:,:,:,:)
       real*8,allocatable::x51(:,:,:,:)
       real*8,allocatable::z163(:,:,:,:,:,:,:,:)
       real*8,allocatable::z164(:,:,:,:,:,:,:,:)
       real*8,allocatable::x52(:,:,:,:)
       real*8,allocatable::z165(:,:,:,:,:,:,:,:)
       real*8,allocatable::z166(:,:,:,:,:,:,:,:)
       real*8,allocatable::z228(:,:,:,:,:,:,:,:)
       real*8,allocatable::z168(:,:,:,:,:,:,:,:)
       real*8,allocatable::x53(:,:,:,:,:,:)
       real*8,allocatable::z172(:,:,:,:,:,:,:,:)
       real*8,allocatable::z173(:,:,:,:,:,:,:,:)
       real*8,allocatable::z174(:,:,:,:,:,:,:,:)
       real*8,allocatable::z176(:,:,:,:,:,:,:,:)
       real*8,allocatable::z177(:,:,:,:,:,:,:,:)
       real*8,allocatable::z178(:,:,:,:,:,:,:,:)
       real*8,allocatable::z180(:,:,:,:,:,:,:,:)
       real*8,allocatable::z181(:,:,:,:,:,:,:,:)
c
       allocate(t4a(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(t4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(t4c(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n2,n0+1:n1,n0+1:n1))
!       allocate(t4d(n2+1:n3,n2+1:n3,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n1))
!       allocate(t4e(n2+1:n3,n2+1:n3,n2+1:n3,n2+1:n3,
!     & n0+1:n2,n0+1:n2,n0+1:n2,n0+1:n2))
       allocate(v4b(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
     & n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
c
       rewind(ta)
       rewind(tb)
       rewind(tc)
!       rewind(td)
!       rewind(te)
       read(ta)t4a
       read(tb)t4b
       read(tc)t4c
!       read(td)t4d
!       read(te)t4e
c
       allocate(indocc(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       allocate(indunocc(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       indocc=0
       indunocc=0
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        iocca=0
        ioccb=0
        if(i.gt.(n1-iactocca))iocca=iocca+1
        if(j.gt.(n1-iactocca))iocca=iocca+1
        if(k.gt.(n1-iactocca))iocca=iocca+1
        if(l.gt.(n2-iactoccb))ioccb=ioccb+1
        if(iocca+ioccb.lt.iactindq)indocc(l,k,j,i)=1
       enddo;enddo;enddo;enddo
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           iunoa=0
           iunob=0
           if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(b.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(c.lt.(n1+iactunoa+1))iunoa=iunoa+1
           if(d.lt.(n2+iactunob+1))iunob=iunob+1
           if(iunoa+iunob.lt.iactindq)indunocc(d,c,b,a)=1
          enddo;enddo;enddo;enddo
c
       v4b=0.0d0
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s1(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s1)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x1(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       x1=0.0d0
       call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s1, 1.000)
       deallocate(s1)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder1324(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n2,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s2(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s2)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x2(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       x2=0.0d0
       call sum3124(n1,n3,n2,n3,n1,n3,n0,n2,x2,s2,-1.000)
       deallocate(s2)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s3(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s3)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x3(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x3=0.0d0
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x3,s3,-1.000)
       deallocate(s3)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s4(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s4)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x21(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       x21=0.0d0
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x21,s4, 1.000)
       deallocate(s4)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s5(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s5)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x22(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x22=0.0d0
       call sum3124(n1,n3,n1,n3,n1,n3,n0,n1,x22,s5, 1.000)
       deallocate(s5)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s6(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s6)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x4(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       x4=0.0d0
       call sum4123(n1,n3,n1,n3,n1,n3,n0,n1,x4,s6, 1.000)
       deallocate(s6)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n0,n2,n0,n1,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s7(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s7)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x5(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       x5=0.0d0
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x5,s7,-1.000)
       deallocate(s7)
c
       allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder1423(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n2,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s8(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s8)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x6(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       x6=0.0d0
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x6,s8,-1.000)
       deallocate(s8)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s9(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s9)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n0,n2,n1,n3,n0,n2,n0,n1,x5,s9, 1.000)
       deallocate(s9)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s10(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s10)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n2,n3,n2,n3,n1,n3,n0,n1,x6,s10, 1.000)
       deallocate(s10)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q1(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,b1,b2,q1)
       deallocate(b1)
       deallocate(b2)
c
       allocate(x8(n0+1:n1,n0+1:n1))
       x8=0.0d0
       call sum21(n0,n1,n0,n1,x8,q1, 1.000)
       deallocate(q1)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q2(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,b1,b2,q2)
       deallocate(b1)
       deallocate(b2)
c
       allocate(x9(n1+1:n3,n1+1:n3))
       x9=0.0d0
       call sum21(n1,n3,n1,n3,x9,q2,-1.000)
       deallocate(q2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s11(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s11)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder4213(n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n0,n1,n0,n1,s11,d1)
!       allocate(h2(n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder67123458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,t4b,h2)
!       allocate(z33(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1
!       i2=k1*k2*k3*k3*k3*k4
!       i3=k1*k1
!       call egemm(i1,i2,i3,d1,h2,z33)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (s11(j,m,i,n)*t4b(d,c,b,a,l,n,m,k)      !jmindcbalnmk    (+0.500)
     &     - s11(k,m,i,n)*t4b(d,c,b,a,l,n,m,j)       !kmindcbalnmj    (-0.500)
     &     - s11(i,m,j,n)*t4b(d,c,b,a,l,n,m,k)       !imjndcbalnmk    (-0.500)
     &     + s11(i,m,k,n)*t4b(d,c,b,a,l,n,m,j)       !imkndcbalnmj    (+0.500)
     &     + s11(k,m,j,n)*t4b(d,c,b,a,l,n,m,i)       !kmjndcbalnmi    (+0.500)
     &     - s11(j,m,k,n)*t4b(d,c,b,a,l,n,m,i))/2.0d0!jmkndcbalnmi    (-0.500)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4b=v4b+0.500*z33
!       call sum12345768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z33,-0.500)
!       call sum12345687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z33,-0.500)
!       call sum12345786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z33, 0.500)
!       call sum12345867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z33, 0.500)
!       call sum12345876(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z33,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z33(d,c,b,a,l,k,j,i)      ! 12345678 (+0.500)
!     & -z33(d,c,b,a,l,j,k,i)       ! 12345768 (-0.500)
!     & -z33(d,c,b,a,l,k,i,j)       ! 12345687 (-0.500)
!     & +z33(d,c,b,a,l,j,i,k)       ! 12345786 (+0.500)
!     & +z33(d,c,b,a,l,i,k,j)       ! 12345867 (+0.500)
!     & -z33(d,c,b,a,l,i,j,k))/2.0d0! 12345876 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z33)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s11,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u55(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u55)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u55,f1)
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z211(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z211)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + u55(c,b,k,n,j,i)*t2b(d,a,l,n)          !cbknjidaln      (+1.000)
     &     - u55(c,a,k,n,j,i)*t2b(d,b,l,n)          !caknjidbln      (-1.000)
     &     + u55(b,a,k,n,j,i)*t2b(d,c,l,n)          !baknjidcln      (+1.000)
     &     - u55(c,b,j,n,k,i)*t2b(d,a,l,n)          !cbjnkidaln      (-1.000)
     &     + u55(c,a,j,n,k,i)*t2b(d,b,l,n)          !cajnkidbln      (+1.000)
     &     - u55(b,a,j,n,k,i)*t2b(d,c,l,n)          !bajnkidcln      (-1.000)
     &     - u55(c,b,k,n,i,j)*t2b(d,a,l,n)          !cbknijdaln      (-1.000)
     &     + u55(c,a,k,n,i,j)*t2b(d,b,l,n)          !caknijdbln      (+1.000)
     &     - u55(b,a,k,n,i,j)*t2b(d,c,l,n)          !baknijdcln      (-1.000)
     &     + u55(c,b,j,n,i,k)*t2b(d,a,l,n)          !cbjnikdaln      (+1.000)
     &     - u55(c,a,j,n,i,k)*t2b(d,b,l,n)          !cajnikdbln      (-1.000)
     &     + u55(b,a,j,n,i,k)*t2b(d,c,l,n)          !bajnikdcln      (+1.000)
     &     + u55(c,b,i,n,k,j)*t2b(d,a,l,n)          !cbinkjdaln      (+1.000)
     &     - u55(c,a,i,n,k,j)*t2b(d,b,l,n)          !cainkjdbln      (-1.000)
     &     + u55(b,a,i,n,k,j)*t2b(d,c,l,n)          !bainkjdcln      (+1.000)
     &     - u55(c,b,i,n,j,k)*t2b(d,a,l,n)          !cbinjkdaln      (-1.000)
     &     + u55(c,a,i,n,j,k)*t2b(d,b,l,n)          !cainjkdbln      (+1.000)
     &     - u55(b,a,i,n,j,k)*t2b(d,c,l,n)          !bainjkdcln      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14523678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum13524678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
!       call sum12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum14523768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
!       call sum13524768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum12534768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
!       call sum14523687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
!       call sum13524687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum12534687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
!       call sum14523786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum13524786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
!       call sum12534786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum14523867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum13524867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
!       call sum12534867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum14523876(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
!       call sum13524876(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211, 1.000)
!       call sum12534876(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z211,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z211(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & -z211(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z211(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z211(d,a,l,c,b,j,k,i)       ! 14523768 (-1.000)
!     & +z211(d,b,l,c,a,j,k,i)       ! 13524768 (+1.000)
!     & -z211(d,c,l,b,a,j,k,i)       ! 12534768 (-1.000)
!     & -z211(d,a,l,c,b,k,i,j)       ! 14523687 (-1.000)
!     & +z211(d,b,l,c,a,k,i,j)       ! 13524687 (+1.000)
!     & -z211(d,c,l,b,a,k,i,j)       ! 12534687 (-1.000)
!     & +z211(d,a,l,c,b,j,i,k)       ! 14523786 (+1.000)
!     & -z211(d,b,l,c,a,j,i,k)       ! 13524786 (-1.000)
!     & +z211(d,c,l,b,a,j,i,k)       ! 12534786 (+1.000)
!     & +z211(d,a,l,c,b,i,k,j)       ! 14523867 (+1.000)
!     & -z211(d,b,l,c,a,i,k,j)       ! 13524867 (-1.000)
!     & +z211(d,c,l,b,a,i,k,j)       ! 12534867 (+1.000)
!     & -z211(d,a,l,c,b,i,j,k)       ! 14523876 (-1.000)
!     & +z211(d,b,l,c,a,i,j,k)       ! 13524876 (+1.000)
!     & -z211(d,c,l,b,a,i,j,k)       ! 12534876 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z211)
       deallocate(u55)
c
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s11,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s89(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s89)
       deallocate(d1)
       deallocate(b2)
       deallocate(s11)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x21,s89,-1.000)
       deallocate(s89)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s12(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s12)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x13(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       x13=0.0d0
       call sum3241(n0,n1,n1,n3,n1,n3,n0,n1,x13,s12,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s12,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u57(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u57)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x23(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x23=0.0d0
       call sum245136(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x23,u57,1.000)
       deallocate(u57)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s12,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u56(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u56)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x24(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       x24=0.0d0
       call sum345126(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x24,u56,1.000)
       deallocate(u56)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s12,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s90(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s90)
       deallocate(d1)
       deallocate(b2)
       deallocate(s12)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x4,s90, 1.000)
       deallocate(s90)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q3(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q3)
       deallocate(d1)
       deallocate(b2)
c
       call sum21(n0,n1,n0,n1,x8,q3,-1.000)
       deallocate(q3)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s13(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s13)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n1,n1,n3,n1,n3,n0,n1,x13,s13, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s13,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u59(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u59)
       deallocate(d1)
       deallocate(d2)
c
      call sum245136(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x23,u59,-1.000)
       deallocate(u59)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s13,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u58(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u58)
       deallocate(d1)
       deallocate(d2)
c
      call sum345126(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x24,u58,-1.000)
       deallocate(u58)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n1,n3,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s13,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s92(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s92)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n1,n3,n1,n3,n0,n1,x22,s92, 1.000)
       deallocate(s92)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s13,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s91(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s91)
       deallocate(d1)
       deallocate(b2)
       deallocate(s13)
c
       call sum3124(n0,n1,n1,n3,n0,n1,n0,n1,x3,s91, 1.000)
       deallocate(s91)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s14(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s14)
       deallocate(d1)
       deallocate(b2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
!       call reorder2341(n1,n3,n1,n3,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n1,n3,s14,d1)
!       allocate(h2(n1+1:n3,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder23145678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z37(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,
!     & n1+1:n3,n1+1:n3))
!       i1=k3*k3
!       i2=k1*k1*k1*k2*k3*k4
!       i3=k3*k3
!       call egemm(i1,i2,i3,d1,h2,z37)
!       deallocate(d1)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3
             sum=sum
     &     + (s14(b,f,e,c)*t4b(d,f,e,a,l,k,j,i)      !bfecdfealkji    (+0.500)
     &     - s14(a,f,e,c)*t4b(d,f,e,b,l,k,j,i)       !afecdfeblkji    (-0.500)
     &     - s14(c,f,e,b)*t4b(d,f,e,a,l,k,j,i)       !cfebdfealkji    (-0.500)
     &     + s14(c,f,e,a)*t4b(d,f,e,b,l,k,j,i)       !cfeadfeblkji    (+0.500)
     &     + s14(a,f,e,b)*t4b(d,f,e,c,l,k,j,i)       !afebdfeclkji    (+0.500)
     &     - s14(b,f,e,a)*t4b(d,f,e,c,l,k,j,i))/2.0d0!bfeadfeclkji    (-0.500)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14567823(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z37, 0.500)
!       call sum13567824(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z37,-0.500)
!       call sum14567832(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z37,-0.500)
!       call sum13567842(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z37, 0.500)
!       call sum12567834(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z37, 0.500)
!       call sum12567843(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z37,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z37(d,a,l,k,j,i,c,b)      ! 14567823 (+0.500)
!     & -z37(d,b,l,k,j,i,c,a)       ! 13567824 (-0.500)
!     & -z37(d,a,l,k,j,i,b,c)       ! 14567832 (-0.500)
!     & +z37(d,b,l,k,j,i,a,c)       ! 13567842 (+0.500)
!     & +z37(d,c,l,k,j,i,b,a)       ! 12567834 (+0.500)
!     & -z37(d,c,l,k,j,i,a,b))/2.0d0! 12567843 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z37)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3241(n1,n3,n1,n3,n1,n3,n1,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,s14,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u60(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u60)
       deallocate(d1)
       deallocate(d2)
       deallocate(s14)
c
!       allocate(f1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder456123(n1,n3,n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u60,f1)
!       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z216(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k3
!       i2=k1*k2*k4
!       i3=k3
!       call egemm(i1,i2,i3,f1,d2,z216)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     + u60(a,j,i,f,c,b)*t2b(d,f,l,k)          !ajifcbdflk      (+1.000)
     &     - u60(b,j,i,f,c,a)*t2b(d,f,l,k)          !bjifcadflk      (-1.000)
     &     - u60(a,j,i,f,b,c)*t2b(d,f,l,k)          !ajifbcdflk      (-1.000)
     &     + u60(b,j,i,f,a,c)*t2b(d,f,l,k)          !bjifacdflk      (+1.000)
     &     + u60(c,j,i,f,b,a)*t2b(d,f,l,k)          !cjifbadflk      (+1.000)
     &     - u60(c,j,i,f,a,b)*t2b(d,f,l,k)          !cjifabdflk      (-1.000)
     &     - u60(a,k,i,f,c,b)*t2b(d,f,l,j)          !akifcbdflj      (-1.000)
     &     + u60(b,k,i,f,c,a)*t2b(d,f,l,j)          !bkifcadflj      (+1.000)
     &     + u60(a,k,i,f,b,c)*t2b(d,f,l,j)          !akifbcdflj      (+1.000)
     &     - u60(b,k,i,f,a,c)*t2b(d,f,l,j)          !bkifacdflj      (-1.000)
     &     - u60(c,k,i,f,b,a)*t2b(d,f,l,j)          !ckifbadflj      (-1.000)
     &     + u60(c,k,i,f,a,b)*t2b(d,f,l,j)          !ckifabdflj      (+1.000)
     &     + u60(a,k,j,f,c,b)*t2b(d,f,l,i)          !akjfcbdfli      (+1.000)
     &     - u60(b,k,j,f,c,a)*t2b(d,f,l,i)          !bkjfcadfli      (-1.000)
     &     - u60(a,k,j,f,b,c)*t2b(d,f,l,i)          !akjfbcdfli      (-1.000)
     &     + u60(b,k,j,f,a,c)*t2b(d,f,l,i)          !bkjfacdfli      (+1.000)
     &     + u60(c,k,j,f,b,a)*t2b(d,f,l,i)          !ckjfbadfli      (+1.000)
     &     - u60(c,k,j,f,a,b)*t2b(d,f,l,i)          !ckjfabdfli      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum15623478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15624378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
!       call sum15632478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
!       call sum15642378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15634278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15643278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
!       call sum15723468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
!       call sum15724368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15732468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15742368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
!       call sum15734268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
!       call sum15743268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15823467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15824367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
!       call sum15832467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
!       call sum15842367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15834267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216, 1.000)
!       call sum15843267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z216,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z216(d,l,k,c,b,a,j,i)       ! 15623478 (+1.000)
!     & -z216(d,l,k,c,a,b,j,i)       ! 15624378 (-1.000)
!     & -z216(d,l,k,b,c,a,j,i)       ! 15632478 (-1.000)
!     & +z216(d,l,k,a,c,b,j,i)       ! 15642378 (+1.000)
!     & +z216(d,l,k,b,a,c,j,i)       ! 15634278 (+1.000)
!     & -z216(d,l,k,a,b,c,j,i)       ! 15643278 (-1.000)
!     & -z216(d,l,j,c,b,a,k,i)       ! 15723468 (-1.000)
!     & +z216(d,l,j,c,a,b,k,i)       ! 15724368 (+1.000)
!     & +z216(d,l,j,b,c,a,k,i)       ! 15732468 (+1.000)
!     & -z216(d,l,j,a,c,b,k,i)       ! 15742368 (-1.000)
!     & -z216(d,l,j,b,a,c,k,i)       ! 15734268 (-1.000)
!     & +z216(d,l,j,a,b,c,k,i)       ! 15743268 (+1.000)
!     & +z216(d,l,i,c,b,a,k,j)       ! 15823467 (+1.000)
!     & -z216(d,l,i,c,a,b,k,j)       ! 15824367 (-1.000)
!     & -z216(d,l,i,b,c,a,k,j)       ! 15832467 (-1.000)
!     & +z216(d,l,i,a,c,b,k,j)       ! 15842367 (+1.000)
!     & +z216(d,l,i,b,a,c,k,j)       ! 15834267 (+1.000)
!     & -z216(d,l,i,a,b,c,k,j)       ! 15843267 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z216)
       deallocate(u60)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q4(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q4)
       deallocate(d1)
       deallocate(b2)
c
       x9=x9-q4
       deallocate(q4)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s15(n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s15)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x15(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       x15=0.0d0
       call sum4231(n0,n2,n0,n1,n0,n2,n0,n1,x15,s15, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s15,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u61(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u61)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x25(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x25=0.0d0
       call sum235146(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x25,u61,1.000)
       deallocate(u61)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4231(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s15,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s102(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s102)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s102,-1.000)
       deallocate(s102)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2431(n0,n1,n0,n1,n0,n2,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s15,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s93(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s93)
       deallocate(d1)
       deallocate(b2)
       deallocate(s15)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x5,s93,-1.000)
       deallocate(s93)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n0,n1,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s16(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s16)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x16(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       x16=0.0d0
       call sum4231(n0,n1,n2,n3,n2,n3,n0,n1,x16,s16, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s16,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u62(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u62)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x26(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x26=0.0d0
       call sum345126(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u62,1.000)
       deallocate(u62)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s16,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s103(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s103)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s103, 1.000)
       deallocate(s103)
c
       allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n2,n3,n2,n3,n0,n1,
     & n0,n1,n2,n3,n2,n3,n0,n1,s16,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s94(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s94)
       deallocate(d1)
       deallocate(b2)
       deallocate(s16)
c
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x6,s94,-1.000)
       deallocate(s94)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s17(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s17)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x17(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       x17=0.0d0
       call sum3241(n0,n2,n1,n3,n1,n3,n0,n2,x17,s17,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder2413(n1,n3,n1,n3,n0,n2,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s17,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u63(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u63)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x27(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x27=0.0d0
       call sum356124(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u63,1.000)
       deallocate(u63)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4213(n1,n3,n1,n3,n0,n2,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s17,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s104(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s104)
       deallocate(d1)
       deallocate(b2)
       deallocate(s17)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s104, 1.000)
       deallocate(s104)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q5(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q5)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x10(n0+1:n2,n0+1:n2))
       x10=0.0d0
       call sum21(n0,n2,n0,n2,x10,q5, 1.000)
       deallocate(q5)
c
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n2+1:n3))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n2,n3,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s18(n1+1:n3,n2+1:n3,n1+1:n3,n2+1:n3))
       i1=k4*k3*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s18)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x18(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       x18=0.0d0
       call sum4123(n2,n3,n1,n3,n2,n3,n1,n3,x18,s18,-1.000)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3241(n1,n3,n2,n3,n1,n3,n2,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s18,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u64(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u64)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x28(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x28=0.0d0
       call sum456123(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x28,u64,1.000)
       deallocate(u64)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder2341(n1,n3,n2,n3,n1,n3,n2,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s18,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s105(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s105)
       deallocate(d1)
       deallocate(b2)
       deallocate(s18)
c
       call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s105,-1.000)
       deallocate(s105)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n2,n3,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q6(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q6)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x11(n2+1:n3,n2+1:n3))
       x11=0.0d0
       x11=x11+q6
       deallocate(q6)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s19(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s19)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x20(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       x20=0.0d0
       call sum3241(n0,n2,n2,n3,n1,n3,n0,n1,x20,s19,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s19,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u71(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u71)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x29(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x29=0.0d0
       call sum345126(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u71,1.000)
       deallocate(u71)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n2,n3,n0,n1,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s19,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s107(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s107)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s107, 1.000)
       deallocate(s107)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s19,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s106(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s106)
       deallocate(d1)
       deallocate(b2)
       deallocate(s19)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s106,-1.000)
       deallocate(s106)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s20(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s20)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n2,n3,n1,n3,n0,n1,x20,s20, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s20,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u72(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u72)
       deallocate(d1)
       deallocate(d2)
c
      call sum345126(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u72,-1.000)
       deallocate(u72)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n1,n2,n3,n1,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s20,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s109(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s109)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s109,-1.000)
       deallocate(s109)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s20,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s108(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s108)
       deallocate(d1)
       deallocate(b2)
       deallocate(s20)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s108, 1.000)
       deallocate(s108)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n0,n2,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s21(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s21)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s21,-1.000)
       deallocate(s21)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s22(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s22)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s22, 1.000)
       deallocate(s22)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder2314(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s23(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s23)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s23,-1.000)
       deallocate(s23)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s24(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s24)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s24, 1.000)
       deallocate(s24)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s25(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s25)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s25, 1.000)
       deallocate(s25)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder2413(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s26(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s26)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s26,-1.000)
       deallocate(s26)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder2134(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s27(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s27)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x7(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       x7=0.0d0
       call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x7,s27,-1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3214(n2,n3,n0,n1,n1,n3,n0,n2,
     & n1,n3,n0,n1,n2,n3,n0,n2,s27,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u83(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u83)
       deallocate(d1)
       deallocate(d2)
       deallocate(s27)
c
       allocate(x30(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x30=0.0d0
       call sum356124(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x30,u83,1.000)
       deallocate(u83)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s28(n0+1:n2,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s28)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n1,n1,n3,n2,n3,n0,n2,x7,s28, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n2,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s28,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u84(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u84)
       deallocate(d1)
       deallocate(d2)
       deallocate(s28)
c
      call sum356124(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x30,u84,-1.000)
       deallocate(u84)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q7(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,b1,b2,q7)
       deallocate(b1)
       deallocate(b2)
c
       call sum21(n0,n2,n0,n2,x10,q7, 1.000)
       deallocate(q7)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n2,n2,n3,fockb,b1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q8(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,b1,b2,q8)
       deallocate(b1)
       deallocate(b2)
c
       call sum21(n2,n3,n2,n3,x11,q8,-1.000)
       deallocate(q8)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n1,n0,n2,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s29(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s29)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n2,n0,n1,n0,n2,n0,n1,x15,s29, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2413(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s29,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u85(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u85)
       deallocate(d1)
       deallocate(d2)
c
       call sum235146(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x25,u85,1.000)
       deallocate(u85)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4213(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s29,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s136(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s136)
       deallocate(d1)
       deallocate(b2)
       deallocate(s29)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s136,-1.000)
       deallocate(s136)
c
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder2143(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s30(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s30)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n2,n3,n0,n1,x16,s30,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3214(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s30,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u86(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u86)
       deallocate(d1)
       deallocate(d2)
       deallocate(s30)
c
      call sum345126(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u86,-1.000)
       deallocate(u86)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q9(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q9)
       deallocate(d1)
       deallocate(b2)
c
       x8=x8+q9
       deallocate(q9)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s31(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s31)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n1,n3,n1,n3,n0,n2,x17,s31, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s31,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u87(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u87)
       deallocate(d1)
       deallocate(d2)
c
      call sum356124(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u87,-1.000)
       deallocate(u87)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4231(n0,n2,n1,n3,n1,n3,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s31,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s137(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s137)
       deallocate(d1)
       deallocate(b2)
       deallocate(s31)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s137,-1.000)
       deallocate(s137)
c
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder2431(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n1,n3,n1,n3,intm,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s32(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s32)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n2,n3,n1,n3,n2,n3,n1,n3,x18,s32,-1.000)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3214(n2,n3,n2,n3,n1,n3,n1,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s32,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u88(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u88)
       deallocate(d1)
       deallocate(d2)
       deallocate(s32)
c
       call sum456123(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x28,u88,1.000)
       deallocate(u88)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q10(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q10)
       deallocate(d1)
       deallocate(b2)
c
       x9=x9+q10
       deallocate(q10)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s33(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s33)
       deallocate(d1)
       deallocate(b2)
c
       allocate(x19(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       x19=0.0d0
       call sum3241(n0,n2,n2,n3,n2,n3,n0,n2,x19,s33,-1.000)
       deallocate(s33)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q11(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q11)
       deallocate(d1)
       deallocate(b2)
c
       call sum21(n0,n2,n0,n2,x10,q11,-1.000)
       deallocate(q11)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s34(n0+1:n2,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s34)
       deallocate(d1)
       deallocate(b2)
c
       call sum4231(n0,n2,n2,n3,n2,n3,n0,n2,x19,s34, 1.000)
       deallocate(s34)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q12(n2+1:n3,n2+1:n3))
       i1=k4*k4
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q12)
       deallocate(d1)
       deallocate(b2)
c
       x11=x11-q12
       deallocate(q12)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u1)
       deallocate(d1)
       deallocate(d2)
c
       call sum356124(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x30,u1,-1.000)
       deallocate(u1)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u2)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x31(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       x31=0.0d0
       call sum234156(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x31,u2, 1.000)
       deallocate(u2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u3(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u3)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x24,u3,-1.000)
       deallocate(u3)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u4(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u4)
       deallocate(d1)
       deallocate(d2)
c
       call sum245136(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x23,u4,-1.000)
       deallocate(u4)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u5(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u5)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x32(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x32=0.0d0
       call sum456123(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x32,u5, 1.000)
       deallocate(u5)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n0,n2,n0,n1,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u6(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u6)
       deallocate(d1)
       deallocate(d2)
c
       call sum235146(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x25,u6, 1.000)
       deallocate(u6)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u7(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u7)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u7, 1.000)
       deallocate(u7)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u8(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u8)
       deallocate(d1)
       deallocate(d2)
c
       call sum356124(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u8,-1.000)
       deallocate(u8)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u9(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u9)
       deallocate(d1)
       deallocate(d2)
c
       call sum456123(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x28,u9,-1.000)
       deallocate(u9)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s35(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s35)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x3,s35, 1.000)
       deallocate(s35)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s36(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s36)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x4,s36,-1.000)
       deallocate(s36)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u10(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u10)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x33(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       x33=0.0d0
       call sum345261(n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,x33,u10,1.000)
       deallocate(u10)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder251346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u11(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k2*k3*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u11)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u11,1.000)
       deallocate(u11)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s37(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s37)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x21,s37, 1.000)
       deallocate(s37)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s38(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s38)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n1,n3,n1,n3,n1,n3,n0,n1,x4,s38, 0.500)
       deallocate(s38)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u12(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u12)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x34(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x34=0.0d0
       call sum456231(n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x34,u12,1.000)
       deallocate(u12)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s39(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s39)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n0,n1,n0,n1,x3,s39, 0.500)
       deallocate(s39)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder231456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u13(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1*k2*k4
       i3=k3*k3
       call egemm(i1,i2,i3,d1,f2,u13)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder615234(n2,n3,n0,n2,n0,n1,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,u13,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z84(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z84)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + (u13(d,l,k,j,c,m)*t2a(b,a,m,i)         !dlkjcmbami      (+0.500)
     &     - u13(d,l,k,j,b,m)*t2a(c,a,m,i)          !dlkjbmcami      (-0.500)
     &     + u13(d,l,k,j,a,m)*t2a(c,b,m,i)          !dlkjamcbmi      (+0.500)
     &     - u13(d,l,k,i,c,m)*t2a(b,a,m,j)          !dlkicmbamj      (-0.500)
     &     + u13(d,l,k,i,b,m)*t2a(c,a,m,j)          !dlkibmcamj      (+0.500)
     &     - u13(d,l,k,i,a,m)*t2a(c,b,m,j)          !dlkiamcbmj      (-0.500)
     &     + u13(d,l,j,i,c,m)*t2a(b,a,m,k)          !dljicmbamk      (+0.500)
     &     - u13(d,l,j,i,b,m)*t2a(c,a,m,k)          !dljibmcamk      (-0.500)
     &     + u13(d,l,j,i,a,m)*t2a(c,b,m,k))/2.0d0   !dljiamcbmk      (+0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34812567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84, 0.500)
!       call sum24813567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84,-0.500)
!       call sum23814567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84, 0.500)
!       call sum34712568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84,-0.500)
!       call sum24713568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84, 0.500)
!       call sum23714568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84,-0.500)
!       call sum34612578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84, 0.500)
!       call sum24613578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84,-0.500)
!       call sum23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z84, 0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z84(b,a,i,d,c,l,k,j)      ! 34812567 (+0.500)
!     & -z84(c,a,i,d,b,l,k,j)       ! 24813567 (-0.500)
!     & +z84(c,b,i,d,a,l,k,j)       ! 23814567 (+0.500)
!     & -z84(b,a,j,d,c,l,k,i)       ! 34712568 (-0.500)
!     & +z84(c,a,j,d,b,l,k,i)       ! 24713568 (+0.500)
!     & -z84(c,b,j,d,a,l,k,i)       ! 23714568 (-0.500)
!     & +z84(b,a,k,d,c,l,j,i)       ! 34612578 (+0.500)
!     & -z84(c,a,k,d,b,l,j,i)       ! 24613578 (-0.500)
!     & +z84(c,b,k,d,a,l,j,i))/2.0d0! 23614578 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z84)
       deallocate(u13)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s40(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s40)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n1,n3,n1,n3,n0,n1,x22,s40,-1.000)
       deallocate(s40)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u14(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2*k1
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u14)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x35(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x35=0.0d0
       call sum356241(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x35,u14,1.000)
       deallocate(u14)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder3421(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u15(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u15)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x36(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x36=0.0d0
       call sum456231(n0,n1,n2,n3,n2,n3,n1,n3,n0,n1,n0,n1,x36,u15,1.000)
       deallocate(u15)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder241356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n2,n2,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u16(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k1*k3*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,f2,u16)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x37(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x37=0.0d0
       call sum235614(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x37,u16,1.000)
       deallocate(u16)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s41(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s41)
       deallocate(d1)
       deallocate(d2)
c
       call sum2431(n0,n2,n1,n3,n0,n2,n0,n1,x5,s41, 1.000)
       deallocate(s41)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u17(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k1*k2*k3
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u17)
       deallocate(d1)
       deallocate(f2)
c
      call sum345621(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x37,u17,-1.000)
       deallocate(u17)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n2,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s42(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s42)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x6,s42, 1.000)
       deallocate(s42)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u18(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u18)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u18,1.000)
       deallocate(u18)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u19(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u19)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x38(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x38=0.0d0
       call sum456231(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x38,u19,1.000)
       deallocate(u19)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u20(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u20)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder546123(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,u20,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder72613458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t4b,h2)
!       allocate(z94(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k2*k3*k3*k4
!       i3=k1*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z94)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (u20(c,j,i,f,m,n)*t4b(d,f,b,a,l,n,m,k)       !cjifmndfbalnmk  (+0.500)
     &     - u20(b,j,i,f,m,n)*t4b(d,f,c,a,l,n,m,k)        !bjifmndfcalnmk  (-0.500)
     &     + u20(a,j,i,f,m,n)*t4b(d,f,c,b,l,n,m,k)        !ajifmndfcblnmk  (+0.500)
     &     - u20(c,k,i,f,m,n)*t4b(d,f,b,a,l,n,m,j)        !ckifmndfbalnmj  (-0.500)
     &     + u20(b,k,i,f,m,n)*t4b(d,f,c,a,l,n,m,j)        !bkifmndfcalnmj  (+0.500)
     &     - u20(a,k,i,f,m,n)*t4b(d,f,c,b,l,n,m,j)        !akifmndfcblnmj  (-0.500)
     &     + u20(c,k,j,f,m,n)*t4b(d,f,b,a,l,n,m,i)        !ckjfmndfbalnmi  (+0.500)
     &     - u20(b,k,j,f,m,n)*t4b(d,f,c,a,l,n,m,i)        !bkjfmndfcalnmi  (-0.500)
     &     + u20(a,k,j,f,m,n)*t4b(d,f,c,b,l,n,m,i))/2.0d0 !akjfmndfcblnmi  (+0.500)
             enddo;enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13456278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94, 0.500)
!       call sum12456378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94,-0.500)
!       call sum12356478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94, 0.500)
!       call sum13457268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94,-0.500)
!       call sum12457368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94, 0.500)
!       call sum12357468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94,-0.500)
!       call sum13458267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94, 0.500)
!       call sum12458367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94,-0.500)
!       call sum12358467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z94, 0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z94(d,b,a,l,k,c,j,i)      ! 13456278 (+0.500)
!     & -z94(d,c,a,l,k,b,j,i)       ! 12456378 (-0.500)
!     & +z94(d,c,b,l,k,a,j,i)       ! 12356478 (+0.500)
!     & -z94(d,b,a,l,j,c,k,i)       ! 13457268 (-0.500)
!     & +z94(d,c,a,l,j,b,k,i)       ! 12457368 (+0.500)
!     & -z94(d,c,b,l,j,a,k,i)       ! 12357468 (-0.500)
!     & +z94(d,b,a,l,i,c,k,j)       ! 13458267 (+0.500)
!     & -z94(d,c,a,l,i,b,k,j)       ! 12458367 (-0.500)
!     & +z94(d,c,b,l,i,a,k,j))/2.0d0! 12358467 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z94)
c
       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder564123(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,u20,f1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(u100(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,f1,d2,u100)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder341256(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u100,f1)
!       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z296(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k3
!       i2=k1*k2*k4
!       i3=k3
!       call egemm(i1,i2,i3,f1,d2,z296)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     + (u100(b,a,f,c,j,i)*t2b(d,f,l,k)        !bafcjidflk      (+0.500)
     &     - u100(c,a,f,b,j,i)*t2b(d,f,l,k)         !cafbjidflk      (-0.500)
     &     + u100(c,b,f,a,j,i)*t2b(d,f,l,k)         !cbfajidflk      (+0.500)
     &     - u100(b,a,f,c,k,i)*t2b(d,f,l,j)         !bafckidflj      (-0.500)
     &     + u100(c,a,f,b,k,i)*t2b(d,f,l,j)         !cafbkidflj      (+0.500)
     &     - u100(c,b,f,a,k,i)*t2b(d,f,l,j)         !cbfakidflj      (-0.500)
     &     + u100(b,a,f,c,k,j)*t2b(d,f,l,i)         !bafckjdfli      (+0.500)
     &     - u100(c,a,f,b,k,j)*t2b(d,f,l,i)         !cafbkjdfli      (-0.500)
     &     + u100(c,b,f,a,k,j)*t2b(d,f,l,i))/2.0d0  !cbfakjdfli      (+0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum15623478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296, 0.500)
!       call sum15632478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296,-0.500)
!       call sum15642378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296, 0.500)
!       call sum15723468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296,-0.500)
!       call sum15732468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296, 0.500)
!       call sum15742368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296,-0.500)
!       call sum15823467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296, 0.500)
!       call sum15832467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296,-0.500)
!       call sum15842367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z296, 0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z296(d,l,k,c,b,a,j,i)      ! 15623478 (+0.500)
!     & -z296(d,l,k,b,c,a,j,i)       ! 15632478 (-0.500)
!     & +z296(d,l,k,a,c,b,j,i)       ! 15642378 (+0.500)
!     & -z296(d,l,j,c,b,a,k,i)       ! 15723468 (-0.500)
!     & +z296(d,l,j,b,c,a,k,i)       ! 15732468 (+0.500)
!     & -z296(d,l,j,a,c,b,k,i)       ! 15742368 (-0.500)
!     & +z296(d,l,i,c,b,a,k,j)       ! 15823467 (+0.500)
!     & -z296(d,l,i,b,c,a,k,j)       ! 15832467 (-0.500)
!     & +z296(d,l,i,a,c,b,k,j))/2.0d0! 15842367 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z296)
       deallocate(u100)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder541236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u20,f1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u98(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3
       i2=k1*k3
       i3=k3*k1
       call egemm(i1,i2,i3,f1,d2,u98)
       deallocate(f1)
       deallocate(d2)
c
       allocate(x39(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       x39=0.0d0
       call sum342561(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x39,u98,1.000)
       deallocate(u98)
c
       allocate(f1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,u20,f1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(u97(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,f1,d2,u97)
       deallocate(f1)
       deallocate(d2)
c
      call sum241356(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x30,u97,-1.000)
       deallocate(u97)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder541236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u20,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s119(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s119)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x3,s119,-1.000)
       deallocate(s119)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder541236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,u20,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u67(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u67)
       deallocate(f1)
       deallocate(b2)
c
      call sum324561(n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,x34,u67,-1.000)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z82(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k2*k3*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x34,f2,z82)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x34(m,f,c,b,j,i)*t3b(d,f,a,l,m,k)      !mfcbjidfalmk    (+1.000)
     &     - x34(m,f,c,a,j,i)*t3b(d,f,b,l,m,k)      !mfcajidfblmk    (-1.000)
     &     - x34(m,f,b,c,j,i)*t3b(d,f,a,l,m,k)      !mfbcjidfalmk    (-1.000)
     &     + x34(m,f,a,c,j,i)*t3b(d,f,b,l,m,k)      !mfacjidfblmk    (+1.000)
     &     + x34(m,f,b,a,j,i)*t3b(d,f,c,l,m,k)      !mfbajidfclmk    (+1.000)
     &     - x34(m,f,a,b,j,i)*t3b(d,f,c,l,m,k)      !mfabjidfclmk    (-1.000)
     &     - x34(m,f,c,b,k,i)*t3b(d,f,a,l,m,j)      !mfcbkidfalmj    (-1.000)
     &     + x34(m,f,c,a,k,i)*t3b(d,f,b,l,m,j)      !mfcakidfblmj    (+1.000)
     &     + x34(m,f,b,c,k,i)*t3b(d,f,a,l,m,j)      !mfbckidfalmj    (+1.000)
     &     - x34(m,f,a,c,k,i)*t3b(d,f,b,l,m,j)      !mfackidfblmj    (-1.000)
     &     - x34(m,f,b,a,k,i)*t3b(d,f,c,l,m,j)      !mfbakidfclmj    (-1.000)
     &     + x34(m,f,a,b,k,i)*t3b(d,f,c,l,m,j)      !mfabkidfclmj    (+1.000)
     &     + x34(m,f,c,b,k,j)*t3b(d,f,a,l,m,i)      !mfcbkjdfalmi    (+1.000)
     &     - x34(m,f,c,a,k,j)*t3b(d,f,b,l,m,i)      !mfcakjdfblmi    (-1.000)
     &     - x34(m,f,b,c,k,j)*t3b(d,f,a,l,m,i)      !mfbckjdfalmi    (-1.000)
     &     + x34(m,f,a,c,k,j)*t3b(d,f,b,l,m,i)      !mfackjdfblmi    (+1.000)
     &     + x34(m,f,b,a,k,j)*t3b(d,f,c,l,m,i)      !mfbakjdfclmi    (+1.000)
     &     - x34(m,f,a,b,k,j)*t3b(d,f,c,l,m,i)      !mfabkjdfclmi    (-1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14562378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum13562478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
!       call sum14563278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
!       call sum13564278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum12563478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum12564378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
!       call sum14572368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
!       call sum13572468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum14573268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum13574268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
!       call sum12573468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
!       call sum12574368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum14582367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum13582467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
!       call sum14583267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
!       call sum13584267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum12583467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82, 1.000)
!       call sum12584367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z82,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z82(d,a,l,k,c,b,j,i)       ! 14562378 (+1.000)
!     & -z82(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & -z82(d,a,l,k,b,c,j,i)       ! 14563278 (-1.000)
!     & +z82(d,b,l,k,a,c,j,i)       ! 13564278 (+1.000)
!     & +z82(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z82(d,c,l,k,a,b,j,i)       ! 12564378 (-1.000)
!     & -z82(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z82(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & +z82(d,a,l,j,b,c,k,i)       ! 14573268 (+1.000)
!     & -z82(d,b,l,j,a,c,k,i)       ! 13574268 (-1.000)
!     & -z82(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z82(d,c,l,j,a,b,k,i)       ! 12574368 (+1.000)
!     & +z82(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z82(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & -z82(d,a,l,i,b,c,k,j)       ! 14583267 (-1.000)
!     & +z82(d,b,l,i,a,c,k,j)       ! 13584267 (+1.000)
!     & +z82(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z82(d,c,l,i,a,b,k,j)       ! 12584367 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z82)
       deallocate(x34)
c
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder621345(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u67,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u115(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u115)
       deallocate(f1)
       deallocate(b2)
       deallocate(u67)
c
       call sum213456(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,
     & x32,u115,1.000)
       deallocate(u115)
c
!       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z71(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k3
!       i2=k1*k2*k4
!       i3=k3
!       call egemm(i1,i2,i3,x32,d2,z71)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3
             sum=sum
     &     + x32(f,c,b,a,j,i)*t2b(d,f,l,k)          !fcbajidflk      (+1.000)
     &     - x32(f,c,a,b,j,i)*t2b(d,f,l,k)          !fcabjidflk      (-1.000)
     &     + x32(f,b,a,c,j,i)*t2b(d,f,l,k)          !fbacjidflk      (+1.000)
     &     - x32(f,c,b,a,k,i)*t2b(d,f,l,j)          !fcbakidflj      (-1.000)
     &     + x32(f,c,a,b,k,i)*t2b(d,f,l,j)          !fcabkidflj      (+1.000)
     &     - x32(f,b,a,c,k,i)*t2b(d,f,l,j)          !fbackidflj      (-1.000)
     &     + x32(f,c,b,a,k,j)*t2b(d,f,l,i)          !fcbakjdfli      (+1.000)
     &     - x32(f,c,a,b,k,j)*t2b(d,f,l,i)          !fcabkjdfli      (-1.000)
     &     + x32(f,b,a,c,k,j)*t2b(d,f,l,i)          !fbackjdfli      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum15623478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71, 1.000)
!       call sum15624378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71,-1.000)
!       call sum15634278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71, 1.000)
!       call sum15723468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71,-1.000)
!       call sum15724368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71, 1.000)
!       call sum15734268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71,-1.000)
!       call sum15823467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71, 1.000)
!       call sum15824367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71,-1.000)
!       call sum15834267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z71, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z71(d,l,k,c,b,a,j,i)       ! 15623478 (+1.000)
!     & -z71(d,l,k,c,a,b,j,i)       ! 15624378 (-1.000)
!     & +z71(d,l,k,b,a,c,j,i)       ! 15634278 (+1.000)
!     & -z71(d,l,j,c,b,a,k,i)       ! 15723468 (-1.000)
!     & +z71(d,l,j,c,a,b,k,i)       ! 15724368 (+1.000)
!     & -z71(d,l,j,b,a,c,k,i)       ! 15734268 (-1.000)
!     & +z71(d,l,i,c,b,a,k,j)       ! 15823467 (+1.000)
!     & -z71(d,l,i,c,a,b,k,j)       ! 15824367 (-1.000)
!     & +z71(d,l,i,b,a,c,k,j)       ! 15834267 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z71)
       deallocate(x32)
c
       allocate(f1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder451236(n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u20,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u65(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u65)
       deallocate(f1)
       deallocate(b2)
       deallocate(u20)
c
       call sum623451(n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,
     & x33,u65,-1.000)
c
!       allocate(f2(n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
!       call reorder561234(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,t3b,f2)
!       allocate(z78(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3
!       i2=k2*k3*k3*k4
!       i3=k1*k1
!       call egemm(i1,i2,i3,x33,f2,z78)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum                             ! top two switched
     &     + (x33(n,m,b,k,j,i)*t3b(d,c,a,l,n,m)       !nmbkjidcalnm    (+0.500)
     &     - x33(n,m,c,k,j,i)*t3b(d,b,a,l,n,m)        !nmckjidbalnm    (-0.500)
     &     - x33(n,m,a,k,j,i)*t3b(d,c,b,l,n,m)        !nmakjidcblnm    (-0.500)
     &     + x33(n,m,c,k,i,j)*t3b(d,b,a,l,n,m)        !nmckijdbalnm    (+0.500)
     &     - x33(n,m,b,k,i,j)*t3b(d,c,a,l,n,m)        !nmbkijdcalnm    (-0.500)
     &     + x33(n,m,a,k,i,j)*t3b(d,c,b,l,n,m)        !nmakijdcblnm    (+0.500)
     &     - x33(n,m,c,j,i,k)*t3b(d,b,a,l,n,m)        !nmcjikdbalnm    (-0.500)
     &     + x33(n,m,b,j,i,k)*t3b(d,c,a,l,n,m)        !nmbjikdcalnm    (+0.500)
     &     - x33(n,m,a,j,i,k)*t3b(d,c,b,l,n,m))/2.0d0 !nmajikdcblnm    (-0.500)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13452678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78,-0.500)
!       call sum12453678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78, 0.500)
!       call sum12354678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78,-0.500)
!       call sum13452687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78, 0.500)
!       call sum12453687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78,-0.500)
!       call sum12354687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78, 0.500)
!       call sum13452786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78,-0.500)
!       call sum12453786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78, 0.500)
!       call sum12354786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z78,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z78(d,c,a,l,b,k,j,i)      ! 12453678 (+0.500) top two switched
!     & -z78(d,b,a,l,c,k,j,i)       ! 13452678 (-0.500)
!     & -z78(d,c,b,l,a,k,j,i)       ! 12354678 (-0.500)
!     & +z78(d,b,a,l,c,k,i,j)       ! 13452687 (+0.500)
!     & -z78(d,c,a,l,b,k,i,j)       ! 12453687 (-0.500)
!     & +z78(d,c,b,l,a,k,i,j)       ! 12354687 (+0.500)
!     & -z78(d,b,a,l,c,j,i,k)       ! 13452786 (-0.500)
!     & +z78(d,c,a,l,b,j,i,k)       ! 12453786 (+0.500)
!     & -z78(d,c,b,l,a,j,i,k))/2.0d0! 12354786 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z78)
       deallocate(x33)
c
       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder263451(n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u65,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u113(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u113)
       deallocate(f1)
       deallocate(b2)
       deallocate(u65)
c
       call sum213456(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & x24,u113,-1.000)
       deallocate(u113)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z212(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x24,d2,z212)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     - x24(n,c,b,k,j,i)*t2b(d,a,l,n)          !ncbkjidaln      (-1.000)
     &     + x24(n,c,a,k,j,i)*t2b(d,b,l,n)          !ncakjidbln      (+1.000)
     &     + x24(n,b,c,k,j,i)*t2b(d,a,l,n)          !nbckjidaln      (+1.000)
     &     - x24(n,a,c,k,j,i)*t2b(d,b,l,n)          !nackjidbln      (-1.000)
     &     - x24(n,b,a,k,j,i)*t2b(d,c,l,n)          !nbakjidcln      (-1.000)
     &     + x24(n,a,b,k,j,i)*t2b(d,c,l,n)          !nabkjidcln      (+1.000)
     &     + x24(n,c,b,k,i,j)*t2b(d,a,l,n)          !ncbkijdaln      (+1.000)
     &     - x24(n,c,a,k,i,j)*t2b(d,b,l,n)          !ncakijdbln      (-1.000)
     &     - x24(n,b,c,k,i,j)*t2b(d,a,l,n)          !nbckijdaln      (-1.000)
     &     + x24(n,a,c,k,i,j)*t2b(d,b,l,n)          !nackijdbln      (+1.000)
     &     + x24(n,b,a,k,i,j)*t2b(d,c,l,n)          !nbakijdcln      (+1.000)
     &     - x24(n,a,b,k,i,j)*t2b(d,c,l,n)          !nabkijdcln      (-1.000)
     &     - x24(n,c,b,j,i,k)*t2b(d,a,l,n)          !ncbjikdaln      (-1.000)
     &     + x24(n,c,a,j,i,k)*t2b(d,b,l,n)          !ncajikdbln      (+1.000)
     &     + x24(n,b,c,j,i,k)*t2b(d,a,l,n)          !nbcjikdaln      (+1.000)
     &     - x24(n,a,c,j,i,k)*t2b(d,b,l,n)          !nacjikdbln      (-1.000)
     &     - x24(n,b,a,j,i,k)*t2b(d,c,l,n)          !nbajikdcln      (-1.000)
     &     + x24(n,a,b,j,i,k)*t2b(d,c,l,n)          !nabjikdcln      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14523678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum13524678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
!       call sum14532678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
!       call sum13542678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum12543678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
!       call sum14523687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
!       call sum13524687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum14532687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum13542687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
!       call sum12534687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
!       call sum12543687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum14523786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum13524786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
!       call sum14532786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
!       call sum13542786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum12534786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212,-1.000)
!       call sum12543786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z212, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z212(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z212(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & +z212(d,a,l,b,c,k,j,i)       ! 14532678 (+1.000)
!     & -z212(d,b,l,a,c,k,j,i)       ! 13542678 (-1.000)
!     & -z212(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z212(d,c,l,a,b,k,j,i)       ! 12543678 (+1.000)
!     & +z212(d,a,l,c,b,k,i,j)       ! 14523687 (+1.000)
!     & -z212(d,b,l,c,a,k,i,j)       ! 13524687 (-1.000)
!     & -z212(d,a,l,b,c,k,i,j)       ! 14532687 (-1.000)
!     & +z212(d,b,l,a,c,k,i,j)       ! 13542687 (+1.000)
!     & +z212(d,c,l,b,a,k,i,j)       ! 12534687 (+1.000)
!     & -z212(d,c,l,a,b,k,i,j)       ! 12543687 (-1.000)
!     & -z212(d,a,l,c,b,j,i,k)       ! 14523786 (-1.000)
!     & +z212(d,b,l,c,a,j,i,k)       ! 13524786 (+1.000)
!     & +z212(d,a,l,b,c,j,i,k)       ! 14532786 (+1.000)
!     & -z212(d,b,l,a,c,j,i,k)       ! 13542786 (-1.000)
!     & -z212(d,c,l,b,a,j,i,k)       ! 12534786 (-1.000)
!     & +z212(d,c,l,a,b,j,i,k)       ! 12543786 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z212)
       deallocate(x24)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(s43(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1
       i3=k3*k3
       call egemm(i1,i2,i3,d1,d2,s43)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x12(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       x12=0.0d0
       call sum3421(n0,n1,n0,n1,n0,n1,n0,n1,x12,s43, 0.500)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s43,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u99(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u99)
       deallocate(d1)
       deallocate(d2)
c
       call sum234156(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x31,u99,0.500)
       deallocate(u99)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s43,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s118(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s118)
       deallocate(d1)
       deallocate(b2)
       deallocate(s43)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x3,s118,-0.500)
       deallocate(s118)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(h2(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n1,n0+1:n1))
       call reorder23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4b,h2)
       allocate(u21(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k2*k3*k4
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,h2,u21)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x37,u21,-0.500)
       deallocate(u21)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s44(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s44)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n1,n3,n0,n1,x13,s44, 1.000)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s44,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u101(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u101)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder415236(n2,n3,n0,n2,n0,n1,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,u101,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z297(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z297)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + u101(d,l,k,n,a,j)*t2a(c,b,n,i)         !dlknajcbni      (+1.000)
     &     - u101(d,l,k,n,b,j)*t2a(c,a,n,i)         !dlknbjcani      (-1.000)
     &     - u101(d,l,k,n,c,i)*t2a(b,a,n,j)         !dlkncibanj      (-1.000)
     &     + u101(d,l,k,n,c,j)*t2a(b,a,n,i)         !dlkncjbani      (+1.000)
     &     + u101(d,l,k,n,b,i)*t2a(c,a,n,j)         !dlknbicanj      (+1.000)
     &     - u101(d,l,k,n,a,i)*t2a(c,b,n,j)         !dlknaicbnj      (-1.000)
     &     - u101(d,l,j,n,a,k)*t2a(c,b,n,i)         !dljnakcbni      (-1.000)
     &     + u101(d,l,j,n,b,k)*t2a(c,a,n,i)         !dljnbkcani      (+1.000)
     &     + u101(d,l,j,n,c,i)*t2a(b,a,n,k)         !dljncibank      (+1.000)
     &     - u101(d,l,j,n,c,k)*t2a(b,a,n,i)         !dljnckbani      (-1.000)
     &     - u101(d,l,j,n,b,i)*t2a(c,a,n,k)         !dljnbicank      (-1.000)
     &     + u101(d,l,j,n,a,i)*t2a(c,b,n,k)         !dljnaicbnk      (+1.000)
     &     + u101(d,l,i,n,a,k)*t2a(c,b,n,j)         !dlinakcbnj      (+1.000)
     &     - u101(d,l,i,n,b,k)*t2a(c,a,n,j)         !dlinbkcanj      (-1.000)
     &     - u101(d,l,i,n,c,j)*t2a(b,a,n,k)         !dlincjbank      (-1.000)
     &     + u101(d,l,i,n,c,k)*t2a(b,a,n,j)         !dlinckbanj      (+1.000)
     &     + u101(d,l,i,n,b,j)*t2a(c,a,n,k)         !dlinbjcank      (+1.000)
     &     - u101(d,l,i,n,a,j)*t2a(c,b,n,k)         !dlinajcbnk      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23814567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum24813567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
!       call sum34712568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
!       call sum34812567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum24713568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum23714568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
!       call sum23814576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
!       call sum24813576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum34612578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum34812576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
!       call sum24613578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
!       call sum23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum23714586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum24713586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
!       call sum34612587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
!       call sum34712586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum24613587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297, 1.000)
!       call sum23614587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z297,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z297(c,b,i,d,a,l,k,j)       ! 23814567 (+1.000)
!     & -z297(c,a,i,d,b,l,k,j)       ! 24813567 (-1.000)
!     & -z297(b,a,j,d,c,l,k,i)       ! 34712568 (-1.000)
!     & +z297(b,a,i,d,c,l,k,j)       ! 34812567 (+1.000)
!     & +z297(c,a,j,d,b,l,k,i)       ! 24713568 (+1.000)
!     & -z297(c,b,j,d,a,l,k,i)       ! 23714568 (-1.000)
!     & -z297(c,b,i,d,a,l,j,k)       ! 23814576 (-1.000)
!     & +z297(c,a,i,d,b,l,j,k)       ! 24813576 (+1.000)
!     & +z297(b,a,k,d,c,l,j,i)       ! 34612578 (+1.000)
!     & -z297(b,a,i,d,c,l,j,k)       ! 34812576 (-1.000)
!     & -z297(c,a,k,d,b,l,j,i)       ! 24613578 (-1.000)
!     & +z297(c,b,k,d,a,l,j,i)       ! 23614578 (+1.000)
!     & +z297(c,b,j,d,a,l,i,k)       ! 23714586 (+1.000)
!     & -z297(c,a,j,d,b,l,i,k)       ! 24713586 (-1.000)
!     & -z297(b,a,k,d,c,l,i,j)       ! 34612587 (-1.000)
!     & +z297(b,a,j,d,c,l,i,k)       ! 34712586 (+1.000)
!     & +z297(c,a,k,d,b,l,i,j)       ! 24613587 (+1.000)
!     & -z297(c,b,k,d,a,l,i,j)       ! 23614587 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z297)
       deallocate(u101)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s44,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s120(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s120)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x22,s120,-1.000)
       deallocate(s120)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s44,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s116(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s116)
       deallocate(d1)
       deallocate(b2)
       deallocate(s44)
c
       call sum4123(n0,n1,n1,n3,n0,n1,n0,n1,x21,s116,-1.000)
       deallocate(s116)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(q13(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,d2,q13)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n0,n1,n0,n1,x8,q13,-0.500)
       deallocate(q13)
c
       allocate(d1(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n1,n3,t2a,d2)
       allocate(s45(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k3*k3
       i3=k1*k1
       call egemm(i1,i2,i3,d1,d2,s45)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x14(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       x14=0.0d0
       call sum3412(n1,n3,n1,n3,n1,n3,n1,n3,x14,s45, 0.500)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder4312(n1,n3,n1,n3,n1,n3,n1,n3,
     & n1,n3,n1,n3,n1,n3,n1,n3,s45,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s117(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s117)
       deallocate(d1)
       deallocate(b2)
       deallocate(s45)
c
       call sum4123(n1,n3,n1,n3,n1,n3,n0,n1,x4,s117, 0.500)
       deallocate(s117)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder1432(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n1,n3,t2a,d2)
       allocate(q14(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k1*k1*k3
       call egemm(i1,i2,i3,d1,d2,q14)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n1,n3,n1,n3,x9,q14, 0.500)
       deallocate(q14)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1234(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n0,n1,t2a,d2)
       allocate(u22(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1*k1*k3
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u22)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder546123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
!     & n0,n1,n2,n3,n0,n2,n1,n3,n0,n1,n0,n1,u22,f1)
!       allocate(h2(n0+1:n1,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder71523468(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n0,n2,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t4c,h2)
!       allocate(z101(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k2*k3*k3*k4
!       i3=k2*k4*k1
!       call egemm(i1,i2,i3,f1,h2,z101)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + u22(c,j,i,f,m,n)*t4c(f,d,b,a,n,l,m,k)  !cjifmnfdbanlmk  (+1.000)
     &     - u22(b,j,i,f,m,n)*t4c(f,d,c,a,n,l,m,k)  !bjifmnfdcanlmk  (-1.000)
     &     + u22(a,j,i,f,m,n)*t4c(f,d,c,b,n,l,m,k)  !ajifmnfdcbnlmk  (+1.000)
     &     - u22(c,k,i,f,m,n)*t4c(f,d,b,a,n,l,m,j)  !ckifmnfdbanlmj  (-1.000)
     &     + u22(b,k,i,f,m,n)*t4c(f,d,c,a,n,l,m,j)  !bkifmnfdcanlmj  (+1.000)
     &     - u22(a,k,i,f,m,n)*t4c(f,d,c,b,n,l,m,j)  !akifmnfdcbnlmj  (-1.000)
     &     + u22(c,k,j,f,m,n)*t4c(f,d,b,a,n,l,m,i)  !ckjfmnfdbanlmi  (+1.000)
     &     - u22(b,k,j,f,m,n)*t4c(f,d,c,a,n,l,m,i)  !bkjfmnfdcanlmi  (-1.000)
     &     + u22(a,k,j,f,m,n)*t4c(f,d,c,b,n,l,m,i)  !akjfmnfdcbnlmi  (+1.000)
             enddo;enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13456278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101, 1.000)
!       call sum12456378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101,-1.000)
!       call sum12356478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101, 1.000)
!       call sum13457268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101,-1.000)
!       call sum12457368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101, 1.000)
!       call sum12357468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101,-1.000)
!       call sum13458267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101, 1.000)
!       call sum12458367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101,-1.000)
!       call sum12358467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z101, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z101(d,b,a,l,k,c,j,i)       ! 13456278 (+1.000)
!     & -z101(d,c,a,l,k,b,j,i)       ! 12456378 (-1.000)
!     & +z101(d,c,b,l,k,a,j,i)       ! 12356478 (+1.000)
!     & -z101(d,b,a,l,j,c,k,i)       ! 13457268 (-1.000)
!     & +z101(d,c,a,l,j,b,k,i)       ! 12457368 (+1.000)
!     & -z101(d,c,b,l,j,a,k,i)       ! 12357468 (-1.000)
!     & +z101(d,b,a,l,i,c,k,j)       ! 13458267 (+1.000)
!     & -z101(d,c,a,l,i,b,k,j)       ! 12458367 (-1.000)
!     & +z101(d,c,b,l,i,a,k,j)       ! 12358467 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z101)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,u22,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(u105(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u105)
       deallocate(f1)
       deallocate(d2)
c
       call sum341256(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & x39,u105,-1.000)
       deallocate(u105)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z294(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x39,d2,z294)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + x39(n,c,b,k,j,i)*t2b(d,a,l,n)          !ncbkjidaln      (+1.000)
     &     - x39(n,c,a,k,j,i)*t2b(d,b,l,n)          !ncakjidbln      (-1.000)
     &     - x39(n,b,c,k,j,i)*t2b(d,a,l,n)          !nbckjidaln      (-1.000)
     &     + x39(n,a,c,k,j,i)*t2b(d,b,l,n)          !nackjidbln      (+1.000)
     &     + x39(n,b,a,k,j,i)*t2b(d,c,l,n)          !nbakjidcln      (+1.000)
     &     - x39(n,a,b,k,j,i)*t2b(d,c,l,n)          !nabkjidcln      (-1.000)
     &     - x39(n,c,b,j,k,i)*t2b(d,a,l,n)          !ncbjkidaln      (-1.000)
     &     + x39(n,c,a,j,k,i)*t2b(d,b,l,n)          !ncajkidbln      (+1.000)
     &     + x39(n,b,c,j,k,i)*t2b(d,a,l,n)          !nbcjkidaln      (+1.000)
     &     - x39(n,a,c,j,k,i)*t2b(d,b,l,n)          !nacjkidbln      (-1.000)
     &     - x39(n,b,a,j,k,i)*t2b(d,c,l,n)          !nbajkidcln      (-1.000)
     &     + x39(n,a,b,j,k,i)*t2b(d,c,l,n)          !nabjkidcln      (+1.000)
     &     - x39(n,b,c,i,k,j)*t2b(d,a,l,n)          !nbcikjdaln      (-1.000)
     &     + x39(n,a,c,i,k,j)*t2b(d,b,l,n)          !nacikjdbln      (+1.000)
     &     + x39(n,c,b,i,k,j)*t2b(d,a,l,n)          !ncbikjdaln      (+1.000)
     &     - x39(n,c,a,i,k,j)*t2b(d,b,l,n)          !ncaikjdbln      (-1.000)
     &     - x39(n,a,b,i,k,j)*t2b(d,c,l,n)          !nabikjdcln      (-1.000)
     &     + x39(n,b,a,i,k,j)*t2b(d,c,l,n)          !nbaikjdcln      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14523678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
!       call sum13524678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum14532678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum13542678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
!       call sum12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
!       call sum12543678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum14523768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum13524768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
!       call sum14532768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
!       call sum13542768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum12534768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum12543768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
!       call sum14532867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum13542867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
!       call sum14523867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
!       call sum13524867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum12543867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294,-1.000)
!       call sum12534867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z294, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z294(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & -z294(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & -z294(d,a,l,b,c,k,j,i)       ! 14532678 (-1.000)
!     & +z294(d,b,l,a,c,k,j,i)       ! 13542678 (+1.000)
!     & +z294(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z294(d,c,l,a,b,k,j,i)       ! 12543678 (-1.000)
!     & -z294(d,a,l,c,b,j,k,i)       ! 14523768 (-1.000)
!     & +z294(d,b,l,c,a,j,k,i)       ! 13524768 (+1.000)
!     & +z294(d,a,l,b,c,j,k,i)       ! 14532768 (+1.000)
!     & -z294(d,b,l,a,c,j,k,i)       ! 13542768 (-1.000)
!     & -z294(d,c,l,b,a,j,k,i)       ! 12534768 (-1.000)
!     & +z294(d,c,l,a,b,j,k,i)       ! 12543768 (+1.000)
!     & -z294(d,a,l,b,c,i,k,j)       ! 14532867 (-1.000)
!     & +z294(d,b,l,a,c,i,k,j)       ! 13542867 (+1.000)
!     & +z294(d,a,l,c,b,i,k,j)       ! 14523867 (+1.000)
!     & -z294(d,b,l,c,a,i,k,j)       ! 13524867 (-1.000)
!     & -z294(d,c,l,a,b,i,k,j)       ! 12543867 (-1.000)
!     & +z294(d,c,l,b,a,i,k,j)       ! 12534867 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z294)
       deallocate(x39)
c
       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,u22,f1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(u104(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1*k3
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,f1,d2,u104)
       deallocate(f1)
       deallocate(d2)
c
!!!!!!!!!!! the following term was explicitly changed by nicholas bauman. when compared
!!!!!!!!!!! with it complimentary term in t4d the two terms made the same but opposite
!!!!!!!!!!! in value contribution to the change (v4b/d). while not confirmed, it is
!!!!!!!!!!! believed that these stems from the automatic factorization and relabeling of
!!!!!!!!!!! indicies that often happens in the derivation process.
!!!!!!!!!!!      call sum342561(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u104,1.000)
       call sum342561(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x27,u104,-1.000)
       deallocate(u104)
c
       allocate(f1(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder564123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n0,n2,n2,n3,n1,n3,n0,n1,n0,n1,u22,f1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(u103(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,f1,d2,u103)
       deallocate(f1)
       deallocate(d2)
c
      call sum241356(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x28,u103,1.000)
       deallocate(u103)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,u22,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(u102(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u102)
       deallocate(f1)
       deallocate(d2)
c
       call sum241356(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x30,u102,-1.000)
       deallocate(u102)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder465123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,u22,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s141(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k1
       i3=k2*k4
       call egemm1(i1,i3,f1,b2,s141)
       deallocate(f1)
       deallocate(b2)
c
       x3=x3+s141
       deallocate(s141)
c
       allocate(f1(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder654123(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n1,n3,n0,n1,n0,n1,u22,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u90(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u90)
       deallocate(f1)
       deallocate(b2)
c
      call sum312456(n0,n1,n2,n3,n2,n3,n1,n3,n0,n1,n0,n1,x36,u90,-1.000)
       deallocate(u90)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z87(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k4
!       i2=k1*k2*k3*k3
!       i3=k4*k1
!       call egemm(i1,i2,i3,x36,f2,z87)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1
             sum=sum
     &     + x36(m,f,d,c,j,i)*t3b(f,b,a,l,m,k)      !mfdcjifbalmk    (+1.000)
     &     - x36(m,f,d,b,j,i)*t3b(f,c,a,l,m,k)      !mfdbjifcalmk    (-1.000)
     &     + x36(m,f,d,a,j,i)*t3b(f,c,b,l,m,k)      !mfdajifcblmk    (+1.000)
     &     - x36(m,f,d,c,k,i)*t3b(f,b,a,l,m,j)      !mfdckifbalmj    (-1.000)
     &     + x36(m,f,d,b,k,i)*t3b(f,c,a,l,m,j)      !mfdbkifcalmj    (+1.000)
     &     - x36(m,f,d,a,k,i)*t3b(f,c,b,l,m,j)      !mfdakifcblmj    (-1.000)
     &     + x36(m,f,d,c,k,j)*t3b(f,b,a,l,m,i)      !mfdckjfbalmi    (+1.000)
     &     - x36(m,f,d,b,k,j)*t3b(f,c,a,l,m,i)      !mfdbkjfcalmi    (-1.000)
     &     + x36(m,f,d,a,k,j)*t3b(f,c,b,l,m,i)      !mfdakjfcblmi    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34561278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87, 1.000)
!       call sum24561378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87,-1.000)
!       call sum23561478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87, 1.000)
!       call sum34571268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87,-1.000)
!       call sum24571368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87, 1.000)
!       call sum23571468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87,-1.000)
!       call sum34581267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87, 1.000)
!       call sum24581367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87,-1.000)
!       call sum23581467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z87, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z87(b,a,l,k,d,c,j,i)       ! 34561278 (+1.000)
!     & -z87(c,a,l,k,d,b,j,i)       ! 24561378 (-1.000)
!     & +z87(c,b,l,k,d,a,j,i)       ! 23561478 (+1.000)
!     & -z87(b,a,l,j,d,c,k,i)       ! 34571268 (-1.000)
!     & +z87(c,a,l,j,d,b,k,i)       ! 24571368 (+1.000)
!     & -z87(c,b,l,j,d,a,k,i)       ! 23571468 (-1.000)
!     & +z87(b,a,l,i,d,c,k,j)       ! 34581267 (+1.000)
!     & -z87(c,a,l,i,d,b,k,j)       ! 24581367 (-1.000)
!     & +z87(c,b,l,i,d,a,k,j)       ! 23581467 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z87)
       deallocate(x36)
c
       allocate(f1(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder451236(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n1,n1,n3,n0,n1,n0,n1,n0,n2,u22,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u89(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1*k3*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u89)
       deallocate(f1)
       deallocate(b2)
c
       call sum423561(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x35,u89,1.000)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder623145(n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,u89,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u121(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u121)
       deallocate(f1)
       deallocate(b2)
       deallocate(u89)
c
      call sum213456(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x30,u121,1.000)
       deallocate(u121)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z265(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x30,d2,z265)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x30(m,d,c,l,j,i)*t2a(b,a,m,k)          !mdcljibamk      (-1.000)
     &     + x30(m,d,b,l,j,i)*t2a(c,a,m,k)          !mdbljicamk      (+1.000)
     &     - x30(m,d,a,l,j,i)*t2a(c,b,m,k)          !mdaljicbmk      (-1.000)
     &     + x30(m,d,c,l,k,i)*t2a(b,a,m,j)          !mdclkibamj      (+1.000)
     &     - x30(m,d,b,l,k,i)*t2a(c,a,m,j)          !mdblkicamj      (-1.000)
     &     + x30(m,d,a,l,k,i)*t2a(c,b,m,j)          !mdalkicbmj      (+1.000)
     &     - x30(m,d,a,l,k,j)*t2a(c,b,m,i)          !mdalkjcbmi      (-1.000)
     &     + x30(m,d,b,l,k,j)*t2a(c,a,m,i)          !mdblkjcami      (+1.000)
     &     - x30(m,d,c,l,k,j)*t2a(b,a,m,i)          !mdclkjbami      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34612578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265,-1.000)
!       call sum24613578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265, 1.000)
!       call sum23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265,-1.000)
!       call sum34712568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265, 1.000)
!       call sum24713568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265,-1.000)
!       call sum23714568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265, 1.000)
!       call sum23814567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265,-1.000)
!       call sum24813567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265, 1.000)
!       call sum34812567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z265,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z265(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & +z265(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z265(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z265(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z265(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z265(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z265(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z265(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z265(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z265)
       deallocate(x30)
c
       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n1,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,u22,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u70(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1*k3*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u70)
       deallocate(f1)
       deallocate(b2)
       deallocate(u22)
c
      call sum324561(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x38,u70,-1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z93(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k2*k3*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x38,f2,z93)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x38(m,f,c,b,j,i)*t3c(f,d,a,m,l,k)      !mfcbjifdamlk    (+1.000)
     &     - x38(m,f,c,a,j,i)*t3c(f,d,b,m,l,k)      !mfcajifdbmlk    (-1.000)
     &     - x38(m,f,b,c,j,i)*t3c(f,d,a,m,l,k)      !mfbcjifdamlk    (-1.000)
     &     + x38(m,f,a,c,j,i)*t3c(f,d,b,m,l,k)      !mfacjifdbmlk    (+1.000)
     &     + x38(m,f,b,a,j,i)*t3c(f,d,c,m,l,k)      !mfbajifdcmlk    (+1.000)
     &     - x38(m,f,a,b,j,i)*t3c(f,d,c,m,l,k)      !mfabjifdcmlk    (-1.000)
     &     - x38(m,f,c,b,k,i)*t3c(f,d,a,m,l,j)      !mfcbkifdamlj    (-1.000)
     &     + x38(m,f,c,a,k,i)*t3c(f,d,b,m,l,j)      !mfcakifdbmlj    (+1.000)
     &     + x38(m,f,b,c,k,i)*t3c(f,d,a,m,l,j)      !mfbckifdamlj    (+1.000)
     &     - x38(m,f,a,c,k,i)*t3c(f,d,b,m,l,j)      !mfackifdbmlj    (-1.000)
     &     - x38(m,f,b,a,k,i)*t3c(f,d,c,m,l,j)      !mfbakifdcmlj    (-1.000)
     &     + x38(m,f,a,b,k,i)*t3c(f,d,c,m,l,j)      !mfabkifdcmlj    (+1.000)
     &     + x38(m,f,c,b,k,j)*t3c(f,d,a,m,l,i)      !mfcbkjfdamli    (+1.000)
     &     - x38(m,f,c,a,k,j)*t3c(f,d,b,m,l,i)      !mfcakjfdbmli    (-1.000)
     &     - x38(m,f,b,c,k,j)*t3c(f,d,a,m,l,i)      !mfbckjfdamli    (-1.000)
     &     + x38(m,f,a,c,k,j)*t3c(f,d,b,m,l,i)      !mfackjfdbmli    (+1.000)
     &     + x38(m,f,b,a,k,j)*t3c(f,d,c,m,l,i)      !mfbakjfdcmli    (+1.000)
     &     - x38(m,f,a,b,k,j)*t3c(f,d,c,m,l,i)      !mfabkjfdcmli    (-1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14562378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum13562478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
!       call sum14563278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
!       call sum13564278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum12563478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum12564378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
!       call sum14572368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
!       call sum13572468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum14573268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum13574268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
!       call sum12573468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
!       call sum12574368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum14582367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum13582467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
!       call sum14583267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
!       call sum13584267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum12583467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93, 1.000)
!       call sum12584367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z93,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z93(d,a,l,k,c,b,j,i)       ! 14562378 (+1.000)
!     & -z93(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & -z93(d,a,l,k,b,c,j,i)       ! 14563278 (-1.000)
!     & +z93(d,b,l,k,a,c,j,i)       ! 13564278 (+1.000)
!     & +z93(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z93(d,c,l,k,a,b,j,i)       ! 12564378 (-1.000)
!     & -z93(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z93(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & +z93(d,a,l,j,b,c,k,i)       ! 14573268 (+1.000)
!     & -z93(d,b,l,j,a,c,k,i)       ! 13574268 (-1.000)
!     & -z93(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z93(d,c,l,j,a,b,k,i)       ! 12574368 (+1.000)
!     & +z93(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z93(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & -z93(d,a,l,i,b,c,k,j)       ! 14583267 (-1.000)
!     & +z93(d,b,l,i,a,c,k,j)       ! 13584267 (+1.000)
!     & +z93(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!     & -z93(d,c,l,i,a,b,k,j)       ! 12584367 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z93)
       deallocate(x38)
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder621345(n1,n3,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,
     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,u70,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u120(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u120)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n2,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,
     & x28,u120,-1.000)
       deallocate(u120)
c
!       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
!       allocate(z220(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3*k4
!       i2=k1*k2*k3
!       i3=k4
!       call egemm(i1,i2,i3,x28,d2,z220)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     - x28(f,d,c,b,j,i)*t2b(f,a,l,k)          !fdcbjifalk      (-1.000)
     &     + x28(f,d,c,a,j,i)*t2b(f,b,l,k)          !fdcajifblk      (+1.000)
     &     + x28(f,d,b,c,j,i)*t2b(f,a,l,k)          !fdbcjifalk      (+1.000)
     &     - x28(f,d,a,c,j,i)*t2b(f,b,l,k)          !fdacjifblk      (-1.000)
     &     - x28(f,d,b,a,j,i)*t2b(f,c,l,k)          !fdbajifclk      (-1.000)
     &     + x28(f,d,a,b,j,i)*t2b(f,c,l,k)          !fdabjifclk      (+1.000)
     &     + x28(f,d,c,b,k,i)*t2b(f,a,l,j)          !fdcbkifalj      (+1.000)
     &     - x28(f,d,c,a,k,i)*t2b(f,b,l,j)          !fdcakifblj      (-1.000)
     &     - x28(f,d,b,c,k,i)*t2b(f,a,l,j)          !fdbckifalj      (-1.000)
     &     + x28(f,d,a,c,k,i)*t2b(f,b,l,j)          !fdackifblj      (+1.000)
     &     + x28(f,d,b,a,k,i)*t2b(f,c,l,j)          !fdbakifclj      (+1.000)
     &     - x28(f,d,a,b,k,i)*t2b(f,c,l,j)          !fdabkifclj      (-1.000)
     &     - x28(f,d,c,b,k,j)*t2b(f,a,l,i)          !fdcbkjfali      (-1.000)
     &     + x28(f,d,c,a,k,j)*t2b(f,b,l,i)          !fdcakjfbli      (+1.000)
     &     + x28(f,d,b,c,k,j)*t2b(f,a,l,i)          !fdbckjfali      (+1.000)
     &     - x28(f,d,a,c,k,j)*t2b(f,b,l,i)          !fdackjfbli      (-1.000)
     &     - x28(f,d,b,a,k,j)*t2b(f,c,l,i)          !fdbakjfcli      (-1.000)
     &     + x28(f,d,a,b,k,j)*t2b(f,c,l,i)          !fdabkjfcli      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum45612378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum35612478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
!       call sum45613278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
!       call sum35614278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum25613478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum25614378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
!       call sum45712368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
!       call sum35712468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum45713268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum35714268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
!       call sum25713468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
!       call sum25714368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum45812367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum35812467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
!       call sum45813267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
!       call sum35814267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum25813467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220,-1.000)
!       call sum25814367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z220, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z220(a,l,k,d,c,b,j,i)       ! 45612378 (-1.000)
!     & +z220(b,l,k,d,c,a,j,i)       ! 35612478 (+1.000)
!     & +z220(a,l,k,d,b,c,j,i)       ! 45613278 (+1.000)
!     & -z220(b,l,k,d,a,c,j,i)       ! 35614278 (-1.000)
!     & -z220(c,l,k,d,b,a,j,i)       ! 25613478 (-1.000)
!     & +z220(c,l,k,d,a,b,j,i)       ! 25614378 (+1.000)
!     & +z220(a,l,j,d,c,b,k,i)       ! 45712368 (+1.000)
!     & -z220(b,l,j,d,c,a,k,i)       ! 35712468 (-1.000)
!     & -z220(a,l,j,d,b,c,k,i)       ! 45713268 (-1.000)
!     & +z220(b,l,j,d,a,c,k,i)       ! 35714268 (+1.000)
!     & +z220(c,l,j,d,b,a,k,i)       ! 25713468 (+1.000)
!     & -z220(c,l,j,d,a,b,k,i)       ! 25714368 (-1.000)
!     & -z220(a,l,i,d,c,b,k,j)       ! 45812367 (-1.000)
!     & +z220(b,l,i,d,c,a,k,j)       ! 35812467 (+1.000)
!     & +z220(a,l,i,d,b,c,k,j)       ! 45813267 (+1.000)
!     & -z220(b,l,i,d,a,c,k,j)       ! 35814267 (-1.000)
!     & -z220(c,l,i,d,b,a,k,j)       ! 25813467 (-1.000)
!     & +z220(c,l,i,d,a,b,k,j)       ! 25814367 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z220)
       deallocate(x28)
c
c
       allocate(f1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder261345(n1,n3,n2,n3,n1,n3,n0,n1,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,u70,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u119(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u119)
       deallocate(f1)
       deallocate(b2)
       deallocate(u70)
c
      call sum412356(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x27,u119,1.000)
       deallocate(u119)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z219(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x27,d2,z219)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + x27(n,c,b,l,j,i)*t2b(d,a,n,k)          !ncbljidank      (+1.000)
     &     - x27(n,c,a,l,j,i)*t2b(d,b,n,k)          !ncaljidbnk      (-1.000)
     &     - x27(n,b,c,l,j,i)*t2b(d,a,n,k)          !nbcljidank      (-1.000)
     &     + x27(n,a,c,l,j,i)*t2b(d,b,n,k)          !nacljidbnk      (+1.000)
     &     + x27(n,b,a,l,j,i)*t2b(d,c,n,k)          !nbaljidcnk      (+1.000)
     &     - x27(n,a,b,l,j,i)*t2b(d,c,n,k)          !nabljidcnk      (-1.000)
     &     - x27(n,c,b,l,k,i)*t2b(d,a,n,j)          !ncblkidanj      (-1.000)
     &     + x27(n,c,a,l,k,i)*t2b(d,b,n,j)          !ncalkidbnj      (+1.000)
     &     + x27(n,b,c,l,k,i)*t2b(d,a,n,j)          !nbclkidanj      (+1.000)
     &     - x27(n,a,c,l,k,i)*t2b(d,b,n,j)          !naclkidbnj      (-1.000)
     &     - x27(n,b,a,l,k,i)*t2b(d,c,n,j)          !nbalkidcnj      (-1.000)
     &     + x27(n,a,b,l,k,i)*t2b(d,c,n,j)          !nablkidcnj      (+1.000)
     &     + x27(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (+1.000)
     &     - x27(n,c,a,l,k,j)*t2b(d,b,n,i)          !ncalkjdbni      (-1.000)
     &     - x27(n,b,c,l,k,j)*t2b(d,a,n,i)          !nbclkjdani      (-1.000)
     &     + x27(n,a,c,l,k,j)*t2b(d,b,n,i)          !naclkjdbni      (+1.000)
     &     + x27(n,b,a,l,k,j)*t2b(d,c,n,i)          !nbalkjdcni      (+1.000)
     &     - x27(n,a,b,l,k,j)*t2b(d,c,n,i)          !nablkjdcni      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14623578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum13624578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
!       call sum14632578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
!       call sum13642578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum12643578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
!       call sum14723568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
!       call sum13724568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum14732568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum13742568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
!       call sum12734568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
!       call sum12743568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum14823567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum13824567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
!       call sum14832567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
!       call sum13842567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum12834567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219, 1.000)
!       call sum12843567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z219,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z219(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z219(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & -z219(d,a,k,b,c,l,j,i)       ! 14632578 (-1.000)
!     & +z219(d,b,k,a,c,l,j,i)       ! 13642578 (+1.000)
!     & +z219(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z219(d,c,k,a,b,l,j,i)       ! 12643578 (-1.000)
!     & -z219(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & +z219(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & +z219(d,a,j,b,c,l,k,i)       ! 14732568 (+1.000)
!     & -z219(d,b,j,a,c,l,k,i)       ! 13742568 (-1.000)
!     & -z219(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z219(d,c,j,a,b,l,k,i)       ! 12743568 (+1.000)
!     & +z219(d,a,i,c,b,l,k,j)       ! 14823567 (+1.000)
!     & -z219(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & -z219(d,a,i,b,c,l,k,j)       ! 14832567 (-1.000)
!     & +z219(d,b,i,a,c,l,k,j)       ! 13842567 (+1.000)
!     & +z219(d,c,i,b,a,l,k,j)       ! 12834567 (+1.000)
!     & -z219(d,c,i,a,b,l,k,j)       ! 12843567 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z219)
       deallocate(x27)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n1,n0+1:n1))
       call reorder13524678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4c,h2)
       allocate(u23(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k2*k3*k4
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,h2,u23)
       deallocate(d1)
       deallocate(h2)
c
      call sum234561(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x37,u23,-1.000)
       deallocate(u23)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder1324(n1,n3,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,t2a,d2)
       allocate(s46(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s46)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n1,n3,n0,n1,x20,s46, 1.000)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s46,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u109(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u109)
       deallocate(d1)
       deallocate(d2)
c
       call sum345126(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x29,u109,-1.000)
       deallocate(u109)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n2,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s46,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s143(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s143)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s143,-1.000)
       deallocate(s143)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s46,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s142(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s142)
       deallocate(d1)
       deallocate(b2)
       deallocate(s46)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s142, 1.000)
       deallocate(s142)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u24(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u24)
       deallocate(d1)
       deallocate(d2)
c
      call sum345126(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u24,-1.000)
       deallocate(u24)
c
       allocate(b1(n1+1:n3,n0+1:n1))
       call reorder12(0,n3,0,n3,
     & n1,n3,n0,n1,fockr,b1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s47(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,b1,d2,s47)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s47, 1.000)
       deallocate(s47)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n1,n1,n3,fockr,b1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s48(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s48)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s48,-1.000)
       deallocate(s48)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u25(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u25)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x40(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x40=0.0d0
       call sum345261(n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,x40,u25,1.000)
       deallocate(u25)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u26(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k3*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u26)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x41(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       x41=0.0d0
       call sum234516(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x41,u26,1.000)
       deallocate(u26)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4213(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,t2b,d2)
       allocate(s49(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k3*k1
       call egemm(i1,i2,i3,d1,d2,s49)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s49,-1.000)
       deallocate(s49)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u27(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u27)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x42(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x42=0.0d0
       call sum356241(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x42,u27,1.000)
       deallocate(u27)
c
       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
       allocate(u28(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1*k1*k3
       i3=k3*k3
       call egemm(i1,i2,i3,d1,f2,u28)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x43(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       x43=0.0d0
       call sum345621(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x43,u28,1.000)
       deallocate(u28)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n1,n3,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s50(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s50)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s50,-1.000)
       deallocate(s50)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder1342(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n0,n2,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u29(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k1*k3*k3
       i3=k3*k1
       call egemm(i1,i2,i3,d1,f2,u29)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x44(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x44=0.0d0
       call sum235641(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x44,u29,1.000)
       deallocate(u29)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s51(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s51)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n0,n1,n2,n3,n0,n2,n0,n1,x1,s51,-1.000)
       deallocate(s51)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u30(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u30)
       deallocate(d1)
       deallocate(d2)
c
       call sum456231(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x42,u30,1.000)
       deallocate(u30)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n2,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s52(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s52)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n2,n3,n0,n2,n0,n1,x1,s52, 1.000)
       deallocate(s52)
c
       allocate(d1(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder1234(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n1,n3,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(s53(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s53)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n1,n3,n2,n3,n1,n3,n0,n2,x2,s53, 1.000)
       deallocate(s53)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n2+1:n3))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n2,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder1423(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n1,n1,n3,n0,n2,t2b,d2)
       allocate(s54(n1+1:n3,n0+1:n2,n1+1:n3,n2+1:n3))
       i1=k4*k3
       i2=k2*k3
       i3=k1*k4
       call egemm(i1,i2,i3,d1,d2,s54)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n2,n3,n1,n3,n0,n2,x2,s54,-1.000)
       deallocate(s54)
c
       allocate(b1(n2+1:n3,n0+1:n2))
       call reorder12(0,n3,0,n3,
     & n2,n3,n0,n2,fockb,b1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s55(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,b1,d2,s55)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s55, 1.000)
       deallocate(s55)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(0,n3,0,n3,
     & n0,n2,n2,n3,fockb,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s56(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s56)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s56,-1.000)
       deallocate(s56)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u31(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u31)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x45(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x45=0.0d0
       call sum345261(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x45,u31,1.000)
       deallocate(u31)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u32(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2*k3*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1,f2,u32)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x46(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x46=0.0d0
       call sum234561(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x46,u32,1.000)
       deallocate(u32)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s57(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s57)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n1,n3,n0,n1,n0,n1,x21,s57,-1.000)
       deallocate(s57)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u33(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u33)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x41,u33,1.000)
       deallocate(u33)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s58(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k2*k3
       i3=k4*k1
       call egemm(i1,i2,i3,d1,d2,s58)
       deallocate(d1)
       deallocate(d2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s58,-1.000)
       deallocate(s58)
c
       allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n0,n1,intm,d1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(s59(n2+1:n3,n1+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s59)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n2,n3,n2,n3,n1,n3,n0,n1,x6,s59, 1.000)
       deallocate(s59)
c
       allocate(d1(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u34(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u34)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x47(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x47=0.0d0
       call sum356241(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x47,u34,1.000)
       deallocate(u34)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u35(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u35)
       deallocate(d1)
       deallocate(d2)
c
       allocate(x48(n0+1:n2,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       x48=0.0d0
       call sum456231(n0,n2,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,x48,u35,1.000)
       deallocate(u35)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s60(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s60)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n1,n3,n0,n2,n0,n1,x5,s60, 1.000)
       deallocate(s60)
c
       allocate(d1(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n1,n3,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u36(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k1*k2*k3
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u36)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x49(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       x49=0.0d0
       call sum345621(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x49,u36,1.000)
       deallocate(u36)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s61(n2+1:n3,n0+1:n1,n2+1:n3,n1+1:n3))
       i1=k3*k4
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s61)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n2,n3,n2,n3,n1,n3,n0,n1,x6,s61,-1.000)
       deallocate(s61)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s62(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s62)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n1,n3,n1,n3,n1,n3,n0,n1,x22,s62, 1.000)
       deallocate(s62)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n0+1:n2))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u37(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u37)
       deallocate(d1)
       deallocate(f2)
c
       call sum235614(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x44,u37,1.000)
       deallocate(u37)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n2,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s63(n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2))
       i1=k2*k2
       i2=k1*k3
       i3=k4*k2
       call egemm(i1,i2,i3,d1,d2,s63)
       deallocate(d1)
       deallocate(d2)
c
       call sum2431(n0,n2,n1,n3,n0,n2,n0,n1,x5,s63,-1.000)
       deallocate(s63)
c
       allocate(d1(n2+1:n3,n2+1:n3,n2+1:n3,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u38(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u38)
       deallocate(d1)
       deallocate(d2)
c
       call sum456231(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x47,u38,1.000)
       deallocate(u38)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n2+1:n3))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s64(n1+1:n3,n0+1:n1,n2+1:n3,n2+1:n3))
       i1=k4*k4
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s64)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n2,n3,n1,n3,n0,n1,x6,s64,-1.000)
       deallocate(s64)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u39(n2+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u39)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,u39,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder61523478(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4a,h2)
!       allocate(z137(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k1*k1*k3*k3*k3
!       i3=k1*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z137)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1; do n=n0+1,n1
             sum=sum                              !top two switched
     &     + (u39(d,l,j,f,m,n)*t4a(f,c,b,a,n,m,k,i)       !dljfmnfcbanmki  (+0.500)
     &     - u39(d,l,i,f,m,n)*t4a(f,c,b,a,n,m,k,j)        !dlifmnfcbanmkj  (-0.500)
     &     - u39(d,l,k,f,m,n)*t4a(f,c,b,a,n,m,j,i))/2.0d0 !dlkfmnfcbanmji  (-0.500)
             enddo;enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23467158(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z137,-0.500)
!       call sum23468157(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z137, 0.500)
!       call sum23478156(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z137,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z137(c,b,a,k,i,d,l,j)      ! 23468157 (+0.500) top two switched
!     & -z137(c,b,a,k,j,d,l,i)       ! 23467158 (-0.500)
!     & -z137(c,b,a,j,i,d,l,k))/2.0d0! 23478156 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z137)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder541236(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,u39,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s123(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s123)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x1,s123,-1.000)
       deallocate(s123)
c
       allocate(f1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder541236(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,u39,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u75(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u75)
       deallocate(f1)
       deallocate(b2)
c
      call sum423561(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x42,u75,-1.000)
       deallocate(u75)
c
       allocate(f1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder451236(n2,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,u39,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u73(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u73)
       deallocate(f1)
       deallocate(b2)
       deallocate(u39)
c
      call sum623451(n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,x40,u73,-1.000)
c
!       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
!       allocate(z107(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k4
!       i2=k1*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,x40,f2,z107)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (x40(n,m,d,l,j,i)*t3a(c,b,a,n,m,k)      !nmdljicbanmk    (+0.500)
     &     - x40(n,m,d,l,k,i)*t3a(c,b,a,n,m,j)       !nmdlkicbanmj    (-0.500)
     &     - x40(n,m,d,l,i,j)*t3a(c,b,a,n,m,k)       !nmdlijcbanmk    (-0.500)
     &     + x40(n,m,d,l,i,k)*t3a(c,b,a,n,m,j)       !nmdlikcbanmj    (+0.500)
     &     + x40(n,m,d,l,k,j)*t3a(c,b,a,n,m,i)       !nmdlkjcbanmi    (+0.500)
     &     - x40(n,m,d,l,j,k)*t3a(c,b,a,n,m,i))/2.0d0!nmdljkcbanmi    (-0.500)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23461578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z107, 0.500)
!       call sum23471568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z107,-0.500)
!       call sum23461587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z107,-0.500)
!       call sum23471586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z107, 0.500)
!       call sum23481567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z107, 0.500)
!       call sum23481576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z107,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z107(c,b,a,k,d,l,j,i)      ! 23461578 (+0.500)
!     & -z107(c,b,a,j,d,l,k,i)       ! 23471568 (-0.500)
!     & -z107(c,b,a,k,d,l,i,j)       ! 23461587 (-0.500)
!     & +z107(c,b,a,j,d,l,i,k)       ! 23471586 (+0.500)
!     & +z107(c,b,a,i,d,l,k,j)       ! 23481567 (+0.500)
!     & -z107(c,b,a,i,d,l,j,k))/2.0d0! 23481576 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z107)
       deallocate(x40)
c
c
       allocate(f1(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder263451(n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,u73,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u114(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u114)
       deallocate(f1)
       deallocate(b2)
       deallocate(u73)
c
       call sum312456(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x23,u114,-1.000)
       deallocate(u114)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z213(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x23,d2,z213)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + x23(n,d,c,l,k,i)*t2a(b,a,n,j)          !ndclkibanj      (+1.000)
     &     - x23(n,d,b,l,k,i)*t2a(c,a,n,j)          !ndblkicanj      (-1.000)
     &     + x23(n,d,a,l,k,i)*t2a(c,b,n,j)          !ndalkicbnj      (+1.000)
     &     - x23(n,d,c,l,j,i)*t2a(b,a,n,k)          !ndcljibank      (-1.000)
     &     + x23(n,d,b,l,j,i)*t2a(c,a,n,k)          !ndbljicank      (+1.000)
     &     - x23(n,d,a,l,j,i)*t2a(c,b,n,k)          !ndaljicbnk      (-1.000)
     &     - x23(n,d,c,l,k,j)*t2a(b,a,n,i)          !ndclkjbani      (-1.000)
     &     + x23(n,d,b,l,k,j)*t2a(c,a,n,i)          !ndblkjcani      (+1.000)
     &     - x23(n,d,a,l,k,j)*t2a(c,b,n,i)          !ndalkjcbni      (-1.000)
     &     + x23(n,d,c,l,j,k)*t2a(b,a,n,i)          !ndcljkbani      (+1.000)
     &     - x23(n,d,b,l,j,k)*t2a(c,a,n,i)          !ndbljkcani      (-1.000)
     &     + x23(n,d,a,l,j,k)*t2a(c,b,n,i)          !ndaljkcbni      (+1.000)
     &     + x23(n,d,c,l,i,j)*t2a(b,a,n,k)          !ndclijbank      (+1.000)
     &     - x23(n,d,b,l,i,j)*t2a(c,a,n,k)          !ndblijcank      (-1.000)
     &     + x23(n,d,a,l,i,j)*t2a(c,b,n,k)          !ndalijcbnk      (+1.000)
     &     - x23(n,d,c,l,i,k)*t2a(b,a,n,j)          !ndclikbanj      (-1.000)
     &     + x23(n,d,b,l,i,k)*t2a(c,a,n,j)          !ndblikcanj      (+1.000)
     &     - x23(n,d,a,l,i,k)*t2a(c,b,n,j)          !ndalikcbnj      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34712568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum24713568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
!       call sum23714568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum34612578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
!       call sum24613578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
!       call sum34812567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
!       call sum24813567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum23814567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
!       call sum34812576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum24813576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
!       call sum23814576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum34612587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum24613587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
!       call sum23614587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum34712586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
!       call sum24713586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213, 1.000)
!       call sum23714586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z213,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z213(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z213(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z213(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z213(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & +z213(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z213(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & -z213(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z213(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z213(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z213(b,a,i,d,c,l,j,k)       ! 34812576 (+1.000)
!     & -z213(c,a,i,d,b,l,j,k)       ! 24813576 (-1.000)
!     & +z213(c,b,i,d,a,l,j,k)       ! 23814576 (+1.000)
!     & +z213(b,a,k,d,c,l,i,j)       ! 34612587 (+1.000)
!     & -z213(c,a,k,d,b,l,i,j)       ! 24613587 (-1.000)
!     & +z213(c,b,k,d,a,l,i,j)       ! 23614587 (+1.000)
!     & -z213(b,a,j,d,c,l,i,k)       ! 34712586 (-1.000)
!     & +z213(c,a,j,d,b,l,i,k)       ! 24713586 (+1.000)
!     & -z213(c,b,j,d,a,l,i,k)       ! 23714586 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z213)
       deallocate(x23)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(h2(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,
     & n0+1:n1,n0+1:n1))
       call reorder12534678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4a,h2)
       allocate(u40(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k1*k3*k3
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,h2,u40)
       deallocate(d1)
       deallocate(h2)
c
       allocate(x50(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       x50=0.0d0
       call sum234561(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x50,u40,1.000)
       deallocate(u40)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s65(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s65)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n2,n3,n0,n2,x7,s65, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n1,n3,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,s65,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s124(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s124)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n2,n3,n1,n3,n0,n2,x2,s124,-1.000)
       deallocate(s124)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n1,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s65,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s122(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s122)
       deallocate(d1)
       deallocate(b2)
       deallocate(s65)
c
       call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s122, 1.000)
       deallocate(s122)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder2134(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n2,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u41(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1*k2*k4
       i3=k3
       call egemm(i1,i2,i3,d1,d2,u41)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
!     & n0,n1,n2,n3,n0,n2,n2,n3,n0,n2,n0,n1,u41,f1)
!       allocate(h2(n0+1:n1,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder61523478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n0,n2,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4b,h2)
!       allocate(z140(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k1*k1*k3*k3*k3
!       i3=k2*k4*k1
!       call egemm(i1,i2,i3,f1,h2,z140)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - u41(d,l,i,f,m,n)*t4b(f,c,b,a,n,m,k,j)  !dlifmnfcbanmkj  (-1.000)
     &     + u41(d,l,j,f,m,n)*t4b(f,c,b,a,n,m,k,i)  !dljfmnfcbanmki  (+1.000)
     &     - u41(d,l,k,f,m,n)*t4b(f,c,b,a,n,m,j,i)  !dlkfmnfcbanmji  (-1.000)
             enddo;enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23467158(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z140,-1.000)
!       call sum23468157(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z140, 1.000)
!       call sum23478156(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z140,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z140(c,b,a,k,j,d,l,i)       ! 23467158 (-1.000)
!     & +z140(c,b,a,k,i,d,l,j)       ! 23468157 (+1.000)
!     & -z140(c,b,a,j,i,d,l,k)       ! 23478156 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z140)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,u41,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(u106(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u106)
       deallocate(f1)
       deallocate(d2)
c
      call sum351246(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u106,1.000)
       deallocate(u106)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n1,u41,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s147(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k4*k1
       i3=k2*k4
       call egemm1(i1,i3,f1,b2,s147)
       deallocate(f1)
       deallocate(b2)
c
       x1=x1+s147
       deallocate(s147)
c
       allocate(f1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n2,n3,n0,n2,n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n2,n3,n0,n2,n0,n1,n0,n2,u41,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u80(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u80)
       deallocate(f1)
       deallocate(b2)
       deallocate(u41)
c
      call sum423561(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x47,u80,-1.000)
       deallocate(u80)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u42(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k3
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u42)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
!     & n0,n1,n1,n3,n0,n2,n1,n3,n0,n2,n0,n1,u42,f1)
!       allocate(h2(n0+1:n1,n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder62513478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4b,h2)
!       allocate(z141(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k1*k3*k3*k4
!       i3=k2*k3*k1
!       call egemm(i1,i2,i3,f1,h2,z141)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - u42(c,l,i,e,m,n)*t4b(d,e,b,a,n,m,k,j)  !cliemndebanmkj  (-1.000)
     &     + u42(b,l,i,e,m,n)*t4b(d,e,c,a,n,m,k,j)  !bliemndecanmkj  (+1.000)
     &     - u42(a,l,i,e,m,n)*t4b(d,e,c,b,n,m,k,j)  !aliemndecbnmkj  (-1.000)
     &     + u42(c,l,j,e,m,n)*t4b(d,e,b,a,n,m,k,i)  !cljemndebanmki  (+1.000)
     &     - u42(b,l,j,e,m,n)*t4b(d,e,c,a,n,m,k,i)  !bljemndecanmki  (-1.000)
     &     + u42(a,l,j,e,m,n)*t4b(d,e,c,b,n,m,k,i)  !aljemndecbnmki  (+1.000)
     &     - u42(c,l,k,e,m,n)*t4b(d,e,b,a,n,m,j,i)  !clkemndebanmji  (-1.000)
     &     + u42(b,l,k,e,m,n)*t4b(d,e,c,a,n,m,j,i)  !blkemndecanmji  (+1.000)
     &     - u42(a,l,k,e,m,n)*t4b(d,e,c,b,n,m,j,i)  !alkemndecbnmji  (-1.000)
             enddo;enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13467258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141,-1.000)
!       call sum12467358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141, 1.000)
!       call sum12367458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141,-1.000)
!       call sum13468257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141, 1.000)
!       call sum12468357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141,-1.000)
!       call sum12368457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141, 1.000)
!       call sum13478256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141,-1.000)
!       call sum12478356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141, 1.000)
!       call sum12378456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z141,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z141(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z141(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z141(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!     & +z141(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z141(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z141(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
!     & -z141(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z141(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z141(d,c,b,j,i,a,l,k)       ! 12378456 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z141)
c
       allocate(f1(n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,u42,f1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(u107(n2+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k1
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,f1,d2,u107)
       deallocate(f1)
       deallocate(d2)
c
      call sum251346(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u107,1.000)
       deallocate(u107)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder654123(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,u42,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u94(n2+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u94)
       deallocate(f1)
       deallocate(b2)
c
      call sum312456(n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,x42,u94,-1.000)
       deallocate(u94)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z110(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k1*k1*k3*k3
!       i3=k3*k1
!       call egemm(i1,i2,i3,x42,f2,z110)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x42(m,f,d,c,l,i)*t3a(f,b,a,m,k,j)      !mfdclifbamkj    (+1.000)
     &     - x42(m,f,d,b,l,i)*t3a(f,c,a,m,k,j)      !mfdblifcamkj    (-1.000)
     &     + x42(m,f,d,a,l,i)*t3a(f,c,b,m,k,j)      !mfdalifcbmkj    (+1.000)
     &     - x42(m,f,d,c,l,j)*t3a(f,b,a,m,k,i)      !mfdcljfbamki    (-1.000)
     &     + x42(m,f,d,b,l,j)*t3a(f,c,a,m,k,i)      !mfdbljfcamki    (+1.000)
     &     - x42(m,f,d,a,l,j)*t3a(f,c,b,m,k,i)      !mfdaljfcbmki    (-1.000)
     &     + x42(m,f,d,c,l,k)*t3a(f,b,a,m,j,i)      !mfdclkfbamji    (+1.000)
     &     - x42(m,f,d,b,l,k)*t3a(f,c,a,m,j,i)      !mfdblkfcamji    (-1.000)
     &     + x42(m,f,d,a,l,k)*t3a(f,c,b,m,j,i)      !mfdalkfcbmji    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34671258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110, 1.000)
!       call sum24671358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110,-1.000)
!       call sum23671458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110, 1.000)
!       call sum34681257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110,-1.000)
!       call sum24681357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110, 1.000)
!       call sum23681457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110,-1.000)
!       call sum34781256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110, 1.000)
!       call sum24781356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110,-1.000)
!       call sum23781456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z110, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z110(b,a,k,j,d,c,l,i)       ! 34671258 (+1.000)
!     & -z110(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & +z110(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & -z110(b,a,k,i,d,c,l,j)       ! 34681257 (-1.000)
!     & +z110(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & -z110(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z110(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z110(c,a,j,i,d,b,l,k)       ! 24781356 (-1.000)
!     & +z110(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z110)
       deallocate(x42)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n2,u42,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s130(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i3=k3*k1
       call egemm1(i1,i3,f1,b2,s130)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s130, 1.000)
       deallocate(s130)
c
       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n2,u42,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u81(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u81)
       deallocate(f1)
       deallocate(b2)
c
      call sum324561(n0,n2,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,x48,u81,-1.000)
       deallocate(u81)
c
!       allocate(f2(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder421356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n1,n3,n2,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z128(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k3
!       i2=k1*k1*k3*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,x48,f2,z128)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n2
             sum=sum
     &     + x48(m,e,c,b,l,i)*t3b(d,e,a,m,k,j)      !mecblideamkj    (+1.000)
     &     - x48(m,e,c,a,l,i)*t3b(d,e,b,m,k,j)      !mecalidebmkj    (-1.000)
     &     - x48(m,e,b,c,l,i)*t3b(d,e,a,m,k,j)      !mebclideamkj    (-1.000)
     &     + x48(m,e,a,c,l,i)*t3b(d,e,b,m,k,j)      !meaclidebmkj    (+1.000)
     &     + x48(m,e,b,a,l,i)*t3b(d,e,c,m,k,j)      !mebalidecmkj    (+1.000)
     &     - x48(m,e,a,b,l,i)*t3b(d,e,c,m,k,j)      !meablidecmkj    (-1.000)
     &     - x48(m,e,c,b,l,j)*t3b(d,e,a,m,k,i)      !mecbljdeamki    (-1.000)
     &     + x48(m,e,c,a,l,j)*t3b(d,e,b,m,k,i)      !mecaljdebmki    (+1.000)
     &     + x48(m,e,b,c,l,j)*t3b(d,e,a,m,k,i)      !mebcljdeamki    (+1.000)
     &     - x48(m,e,a,c,l,j)*t3b(d,e,b,m,k,i)      !meacljdebmki    (-1.000)
     &     - x48(m,e,b,a,l,j)*t3b(d,e,c,m,k,i)      !mebaljdecmki    (-1.000)
     &     + x48(m,e,a,b,l,j)*t3b(d,e,c,m,k,i)      !meabljdecmki    (+1.000)
     &     + x48(m,e,c,b,l,k)*t3b(d,e,a,m,j,i)      !mecblkdeamji    (+1.000)
     &     - x48(m,e,c,a,l,k)*t3b(d,e,b,m,j,i)      !mecalkdebmji    (-1.000)
     &     - x48(m,e,b,c,l,k)*t3b(d,e,a,m,j,i)      !mebclkdeamji    (-1.000)
     &     + x48(m,e,a,c,l,k)*t3b(d,e,b,m,j,i)      !meaclkdebmji    (+1.000)
     &     + x48(m,e,b,a,l,k)*t3b(d,e,c,m,j,i)      !mebalkdecmji    (+1.000)
     &     - x48(m,e,a,b,l,k)*t3b(d,e,c,m,j,i)      !meablkdecmji    (-1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14672358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum13672458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
!       call sum14673258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
!       call sum13674258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum12673458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum12674358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
!       call sum14682357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
!       call sum13682457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum14683257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum13684257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
!       call sum12683457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
!       call sum12684357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum14782356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum13782456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
!       call sum14783256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
!       call sum13784256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum12783456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128, 1.000)
!       call sum12784356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z128,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z128(d,a,k,j,c,b,l,i)       ! 14672358 (+1.000)
!     & -z128(d,b,k,j,c,a,l,i)       ! 13672458 (-1.000)
!     & -z128(d,a,k,j,b,c,l,i)       ! 14673258 (-1.000)
!     & +z128(d,b,k,j,a,c,l,i)       ! 13674258 (+1.000)
!     & +z128(d,c,k,j,b,a,l,i)       ! 12673458 (+1.000)
!     & -z128(d,c,k,j,a,b,l,i)       ! 12674358 (-1.000)
!     & -z128(d,a,k,i,c,b,l,j)       ! 14682357 (-1.000)
!     & +z128(d,b,k,i,c,a,l,j)       ! 13682457 (+1.000)
!     & +z128(d,a,k,i,b,c,l,j)       ! 14683257 (+1.000)
!     & -z128(d,b,k,i,a,c,l,j)       ! 13684257 (-1.000)
!     & -z128(d,c,k,i,b,a,l,j)       ! 12683457 (-1.000)
!     & +z128(d,c,k,i,a,b,l,j)       ! 12684357 (+1.000)
!     & +z128(d,a,j,i,c,b,l,k)       ! 14782356 (+1.000)
!     & -z128(d,b,j,i,c,a,l,k)       ! 13782456 (-1.000)
!     & -z128(d,a,j,i,b,c,l,k)       ! 14783256 (-1.000)
!     & +z128(d,b,j,i,a,c,l,k)       ! 13784256 (+1.000)
!     & +z128(d,c,j,i,b,a,l,k)       ! 12783456 (+1.000)
!     & -z128(d,c,j,i,a,b,l,k)       ! 12784356 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z128)
       deallocate(x48)
c
       allocate(f1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder451236(n1,n3,n0,n2,n0,n1,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n1,n1,n3,n0,n2,n0,n1,n0,n2,u42,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u77(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u77)
       deallocate(f1)
       deallocate(b2)
       deallocate(u42)
c
       call sum623451(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x45,u77,1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)
!       allocate(z121(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3
!       i2=k1*k3*k3*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x45,f2,z121)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + x45(n,m,c,l,j,i)*t3b(d,b,a,n,m,k)      !nmcljidbanmk    (+1.000)
     &     - x45(n,m,b,l,j,i)*t3b(d,c,a,n,m,k)      !nmbljidcanmk    (-1.000)
     &     + x45(n,m,a,l,j,i)*t3b(d,c,b,n,m,k)      !nmaljidcbnmk    (+1.000)
     &     - x45(n,m,c,l,k,i)*t3b(d,b,a,n,m,j)      !nmclkidbanmj    (-1.000)
     &     + x45(n,m,b,l,k,i)*t3b(d,c,a,n,m,j)      !nmblkidcanmj    (+1.000)
     &     - x45(n,m,a,l,k,i)*t3b(d,c,b,n,m,j)      !nmalkidcbnmj    (-1.000)
     &     - x45(n,m,c,l,i,j)*t3b(d,b,a,n,m,k)      !nmclijdbanmk    (-1.000)
     &     + x45(n,m,b,l,i,j)*t3b(d,c,a,n,m,k)      !nmblijdcanmk    (+1.000)
     &     - x45(n,m,a,l,i,j)*t3b(d,c,b,n,m,k)      !nmalijdcbnmk    (-1.000)
     &     + x45(n,m,c,l,i,k)*t3b(d,b,a,n,m,j)      !nmclikdbanmj    (+1.000)
     &     - x45(n,m,b,l,i,k)*t3b(d,c,a,n,m,j)      !nmblikdcanmj    (-1.000)
     &     + x45(n,m,a,l,i,k)*t3b(d,c,b,n,m,j)      !nmalikdcbnmj    (+1.000)
     &     + x45(n,m,c,l,k,j)*t3b(d,b,a,n,m,i)      !nmclkjdbanmi    (+1.000)
     &     - x45(n,m,b,l,k,j)*t3b(d,c,a,n,m,i)      !nmblkjdcanmi    (-1.000)
     &     + x45(n,m,a,l,k,j)*t3b(d,c,b,n,m,i)      !nmalkjdcbnmi    (+1.000)
     &     - x45(n,m,c,l,j,k)*t3b(d,b,a,n,m,i)      !nmcljkdbanmi    (-1.000)
     &     + x45(n,m,b,l,j,k)*t3b(d,c,a,n,m,i)      !nmbljkdcanmi    (+1.000)
     &     - x45(n,m,a,l,j,k)*t3b(d,c,b,n,m,i)      !nmaljkdcbnmi    (-1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13462578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum12463578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
!       call sum12364578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum13472568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
!       call sum12473568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum12374568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
!       call sum13462587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
!       call sum12463587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum12364587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
!       call sum13472586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum12473586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
!       call sum12374586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum13482567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum12483567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
!       call sum12384567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum13482576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
!       call sum12483576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121, 1.000)
!       call sum12384576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z121,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z121(d,b,a,k,c,l,j,i)       ! 13462578 (+1.000)
!     & -z121(d,c,a,k,b,l,j,i)       ! 12463578 (-1.000)
!     & +z121(d,c,b,k,a,l,j,i)       ! 12364578 (+1.000)
!     & -z121(d,b,a,j,c,l,k,i)       ! 13472568 (-1.000)
!     & +z121(d,c,a,j,b,l,k,i)       ! 12473568 (+1.000)
!     & -z121(d,c,b,j,a,l,k,i)       ! 12374568 (-1.000)
!     & -z121(d,b,a,k,c,l,i,j)       ! 13462587 (-1.000)
!     & +z121(d,c,a,k,b,l,i,j)       ! 12463587 (+1.000)
!     & -z121(d,c,b,k,a,l,i,j)       ! 12364587 (-1.000)
!     & +z121(d,b,a,j,c,l,i,k)       ! 13472586 (+1.000)
!     & -z121(d,c,a,j,b,l,i,k)       ! 12473586 (-1.000)
!     & +z121(d,c,b,j,a,l,i,k)       ! 12374586 (+1.000)
!     & +z121(d,b,a,i,c,l,k,j)       ! 13482567 (+1.000)
!     & -z121(d,c,a,i,b,l,k,j)       ! 12483567 (-1.000)
!     & +z121(d,c,b,i,a,l,k,j)       ! 12384567 (+1.000)
!     & -z121(d,b,a,i,c,l,j,k)       ! 13482576 (-1.000)
!     & +z121(d,c,a,i,b,l,j,k)       ! 12483576 (+1.000)
!     & -z121(d,c,b,i,a,l,j,k)       ! 12384576 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z121)
       deallocate(x45)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder623451(n0,n1,n0,n1,n1,n3,n0,n2,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,u77,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u118(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u118)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x26,u118,-1.000)
       deallocate(u118)
c
       allocate(f1(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder263451(n0,n1,n0,n1,n1,n3,n0,n2,n0,n1,n0,n2,
     & n0,n1,n0,n2,n1,n3,n0,n2,n0,n1,n0,n1,u77,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u116(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u116)
       deallocate(f1)
       deallocate(b2)
       deallocate(u77)
c
      call sum213456(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x29,u116,1.000)
       deallocate(u116)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z233(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x29,d2,z233)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x29(n,c,b,l,j,i)*t2b(d,a,n,k)          !ncbljidank      (-1.000)
     &     + x29(n,c,a,l,j,i)*t2b(d,b,n,k)          !ncaljidbnk      (+1.000)
     &     + x29(n,b,c,l,j,i)*t2b(d,a,n,k)          !nbcljidank      (+1.000)
     &     - x29(n,a,c,l,j,i)*t2b(d,b,n,k)          !nacljidbnk      (-1.000)
     &     - x29(n,b,a,l,j,i)*t2b(d,c,n,k)          !nbaljidcnk      (-1.000)
     &     + x29(n,a,b,l,j,i)*t2b(d,c,n,k)          !nabljidcnk      (+1.000)
     &     - x29(n,c,a,l,k,i)*t2b(d,b,n,j)          !ncalkidbnj      (-1.000)
     &     + x29(n,c,b,l,k,i)*t2b(d,a,n,j)          !ncblkidanj      (+1.000)
     &     + x29(n,b,a,l,k,i)*t2b(d,c,n,j)          !nbalkidcnj      (+1.000)
     &     - x29(n,a,b,l,k,i)*t2b(d,c,n,j)          !nablkidcnj      (-1.000)
     &     - x29(n,b,c,l,k,i)*t2b(d,a,n,j)          !nbclkidanj      (-1.000)
     &     + x29(n,a,c,l,k,i)*t2b(d,b,n,j)          !naclkidbnj      (+1.000)
     &     + x29(n,c,b,l,i,j)*t2b(d,a,n,k)          !ncblijdank      (+1.000)
     &     - x29(n,c,a,l,i,j)*t2b(d,b,n,k)          !ncalijdbnk      (-1.000)
     &     - x29(n,b,c,l,i,j)*t2b(d,a,n,k)          !nbclijdank      (-1.000)
     &     + x29(n,a,c,l,i,j)*t2b(d,b,n,k)          !naclijdbnk      (+1.000)
     &     + x29(n,b,a,l,i,j)*t2b(d,c,n,k)          !nbalijdcnk      (+1.000)
     &     - x29(n,a,b,l,i,j)*t2b(d,c,n,k)          !nablijdcnk      (-1.000)
     &     + x29(n,c,a,l,k,j)*t2b(d,b,n,i)          !ncalkjdbni      (+1.000)
     &     - x29(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (-1.000)
     &     - x29(n,b,a,l,k,j)*t2b(d,c,n,i)          !nbalkjdcni      (-1.000)
     &     + x29(n,a,b,l,k,j)*t2b(d,c,n,i)          !nablkjdcni      (+1.000)
     &     + x29(n,b,c,l,k,j)*t2b(d,a,n,i)          !nbclkjdani      (+1.000)
     &     - x29(n,a,c,l,k,j)*t2b(d,b,n,i)          !naclkjdbni      (-1.000)
     &     - x29(n,c,b,l,i,k)*t2b(d,a,n,j)          !ncblikdanj      (-1.000)
     &     + x29(n,c,a,l,i,k)*t2b(d,b,n,j)          !ncalikdbnj      (+1.000)
     &     + x29(n,b,c,l,i,k)*t2b(d,a,n,j)          !nbclikdanj      (+1.000)
     &     - x29(n,a,c,l,i,k)*t2b(d,b,n,j)          !naclikdbnj      (-1.000)
     &     - x29(n,b,a,l,i,k)*t2b(d,c,n,j)          !nbalikdcnj      (-1.000)
     &     + x29(n,a,b,l,i,k)*t2b(d,c,n,j)          !nablikdcnj      (+1.000)
     &     - x29(n,c,a,l,j,k)*t2b(d,b,n,i)          !ncaljkdbni      (-1.000)
     &     + x29(n,c,b,l,j,k)*t2b(d,a,n,i)          !ncbljkdani      (+1.000)
     &     + x29(n,b,a,l,j,k)*t2b(d,c,n,i)          !nbaljkdcni      (+1.000)
     &     - x29(n,a,b,l,j,k)*t2b(d,c,n,i)          !nabljkdcni      (-1.000)
     &     - x29(n,b,c,l,j,k)*t2b(d,a,n,i)          !nbcljkdani      (-1.000)
     &     + x29(n,a,c,l,j,k)*t2b(d,b,n,i)          !nacljkdbni      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14623578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum13624578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum14632578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum13642578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum12643578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum13724568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum14723568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum12734568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum12743568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum14732568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum13742568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum14623587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum13624587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum14632587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum13642587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum12634587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum12643587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum13824567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum14823567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum12834567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum12843567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum14832567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum13842567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum14723586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum13724586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum14732586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum13742586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum12734586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum12743586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum13824576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum14823576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum12834576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
!       call sum12843576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum14832576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233,-1.000)
!       call sum13842576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z233, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z233(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & +z233(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & +z233(d,a,k,b,c,l,j,i)       ! 14632578 (+1.000)
!     & -z233(d,b,k,a,c,l,j,i)       ! 13642578 (-1.000)
!     & -z233(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z233(d,c,k,a,b,l,j,i)       ! 12643578 (+1.000)
!     & -z233(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z233(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z233(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z233(d,c,j,a,b,l,k,i)       ! 12743568 (-1.000)
!     & -z233(d,a,j,b,c,l,k,i)       ! 14732568 (-1.000)
!     & +z233(d,b,j,a,c,l,k,i)       ! 13742568 (+1.000)
!     & +z233(d,a,k,c,b,l,i,j)       ! 14623587 (+1.000)
!     & -z233(d,b,k,c,a,l,i,j)       ! 13624587 (-1.000)
!     & -z233(d,a,k,b,c,l,i,j)       ! 14632587 (-1.000)
!     & +z233(d,b,k,a,c,l,i,j)       ! 13642587 (+1.000)
!     & +z233(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!     & -z233(d,c,k,a,b,l,i,j)       ! 12643587 (-1.000)
!     & +z233(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z233(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z233(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z233(d,c,i,a,b,l,k,j)       ! 12843567 (+1.000)
!     & +z233(d,a,i,b,c,l,k,j)       ! 14832567 (+1.000)
!     & -z233(d,b,i,a,c,l,k,j)       ! 13842567 (-1.000)
!     & -z233(d,a,j,c,b,l,i,k)       ! 14723586 (-1.000)
!     & +z233(d,b,j,c,a,l,i,k)       ! 13724586 (+1.000)
!     & +z233(d,a,j,b,c,l,i,k)       ! 14732586 (+1.000)
!     & -z233(d,b,j,a,c,l,i,k)       ! 13742586 (-1.000)
!     & -z233(d,c,j,b,a,l,i,k)       ! 12734586 (-1.000)
!     & +z233(d,c,j,a,b,l,i,k)       ! 12743586 (+1.000)
!     & -z233(d,b,i,c,a,l,j,k)       ! 13824576 (-1.000)
!     & +z233(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & +z233(d,c,i,b,a,l,j,k)       ! 12834576 (+1.000)
!     & -z233(d,c,i,a,b,l,j,k)       ! 12843576 (-1.000)
!     & -z233(d,a,i,b,c,l,j,k)       ! 14832576 (-1.000)
!     & +z233(d,b,i,a,c,l,j,k)       ! 13842576 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z233)
       deallocate(x29)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(s66(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k2
       i3=k3*k4
       call egemm(i1,i2,i3,d1,d2,s66)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n0,n1,n0,n2,n0,n1,x15,s66, 1.000)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3412(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s66,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u108(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u108)
       deallocate(d1)
       deallocate(d2)
c
       call sum236145(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x25,u108,-1.000)
       deallocate(u108)
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4312(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s66,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s146(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s146)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s146,-1.000)
       deallocate(s146)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder3412(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s66,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s129(n1+1:n3,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s129)
       deallocate(d1)
       deallocate(b2)
       deallocate(s66)
c
       call sum2134(n0,n2,n1,n3,n0,n2,n0,n1,x5,s129,-1.000)
       deallocate(s129)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n1,n0+1:n1))
       call reorder12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4b,h2)
       allocate(u43(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k1*k2*k3*k3
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,h2,u43)
       deallocate(d1)
       deallocate(h2)
c
       call sum234561(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x44,u43,1.000)
       deallocate(u43)
c
       allocate(d1(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n2,n2,n3,n0,n1,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n1))
       call reorder2314(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n2,n2,n3,n0,n1,t2b,d2)
       allocate(s67(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4
       i2=k1*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,d2,s67)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n2,n3,n2,n3,n0,n1,x16,s67,-1.000)
c
       allocate(d1(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder3412(n2,n3,n0,n1,n2,n3,n0,n1,
     & n2,n3,n0,n1,n2,n3,n0,n1,s67,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s145(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s145)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n0,n2,n0,n1,x1,s145,-1.000)
       deallocate(s145)
c
       allocate(d1(n0+1:n1,n2+1:n3,n2+1:n3,n0+1:n1))
       call reorder4312(n2,n3,n0,n1,n2,n3,n0,n1,
     & n0,n1,n2,n3,n2,n3,n0,n1,s67,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s131(n1+1:n3,n2+1:n3,n2+1:n3,n0+1:n1))
       i1=k1*k4*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s131)
       deallocate(d1)
       deallocate(b2)
       deallocate(s67)
c
       call sum3124(n2,n3,n2,n3,n1,n3,n0,n1,x6,s131, 1.000)
       deallocate(s131)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s68(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s68)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n1,n3,n0,n1,x13,s68, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s68,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s133(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s133)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x22,s133,-1.000)
       deallocate(s133)
c
!       allocate(f2(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder213456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z25(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k2*k3*k4
!       i3=k3
!       call egemm(i1,i2,i3,x22,f2,z25)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     - x22(e,c,b,i)*t3b(d,e,a,l,k,j)          !ecbidealkj      (-1.000)
     &     + x22(e,c,a,i)*t3b(d,e,b,l,k,j)          !ecaideblkj      (+1.000)
     &     + x22(e,b,c,i)*t3b(d,e,a,l,k,j)          !ebcidealkj      (+1.000)
     &     - x22(e,a,c,i)*t3b(d,e,b,l,k,j)          !eacideblkj      (-1.000)
     &     - x22(e,b,a,i)*t3b(d,e,c,l,k,j)          !ebaideclkj      (-1.000)
     &     + x22(e,a,b,i)*t3b(d,e,c,l,k,j)          !eabideclkj      (+1.000)
     &     + x22(e,c,b,j)*t3b(d,e,a,l,k,i)          !ecbjdealki      (+1.000)
     &     - x22(e,c,a,j)*t3b(d,e,b,l,k,i)          !ecajdeblki      (-1.000)
     &     - x22(e,b,c,j)*t3b(d,e,a,l,k,i)          !ebcjdealki      (-1.000)
     &     + x22(e,a,c,j)*t3b(d,e,b,l,k,i)          !eacjdeblki      (+1.000)
     &     + x22(e,b,a,j)*t3b(d,e,c,l,k,i)          !ebajdeclki      (+1.000)
     &     - x22(e,a,b,j)*t3b(d,e,c,l,k,i)          !eabjdeclki      (-1.000)
     &     - x22(e,c,b,k)*t3b(d,e,a,l,j,i)          !ecbkdealji      (-1.000)
     &     + x22(e,c,a,k)*t3b(d,e,b,l,j,i)          !ecakdeblji      (+1.000)
     &     + x22(e,b,c,k)*t3b(d,e,a,l,j,i)          !ebckdealji      (+1.000)
     &     - x22(e,a,c,k)*t3b(d,e,b,l,j,i)          !eackdeblji      (-1.000)
     &     - x22(e,b,a,k)*t3b(d,e,c,l,j,i)          !ebakdeclji      (-1.000)
     &     + x22(e,a,b,k)*t3b(d,e,c,l,j,i)          !eabkdeclji      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14567238(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum13567248(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
!       call sum14567328(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
!       call sum13567428(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum12567348(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum12567438(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
!       call sum14568237(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
!       call sum13568247(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum14568327(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum13568427(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
!       call sum12568347(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
!       call sum12568437(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum14578236(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum13578246(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
!       call sum14578326(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
!       call sum13578426(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum12578346(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25,-1.000)
!       call sum12578436(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z25, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z25(d,a,l,k,j,c,b,i)       ! 14567238 (-1.000)
!     & +z25(d,b,l,k,j,c,a,i)       ! 13567248 (+1.000)
!     & +z25(d,a,l,k,j,b,c,i)       ! 14567328 (+1.000)
!     & -z25(d,b,l,k,j,a,c,i)       ! 13567428 (-1.000)
!     & -z25(d,c,l,k,j,b,a,i)       ! 12567348 (-1.000)
!     & +z25(d,c,l,k,j,a,b,i)       ! 12567438 (+1.000)
!     & +z25(d,a,l,k,i,c,b,j)       ! 14568237 (+1.000)
!     & -z25(d,b,l,k,i,c,a,j)       ! 13568247 (-1.000)
!     & -z25(d,a,l,k,i,b,c,j)       ! 14568327 (-1.000)
!     & +z25(d,b,l,k,i,a,c,j)       ! 13568427 (+1.000)
!     & +z25(d,c,l,k,i,b,a,j)       ! 12568347 (+1.000)
!     & -z25(d,c,l,k,i,a,b,j)       ! 12568437 (-1.000)
!     & -z25(d,a,l,j,i,c,b,k)       ! 14578236 (-1.000)
!     & +z25(d,b,l,j,i,c,a,k)       ! 13578246 (+1.000)
!     & +z25(d,a,l,j,i,b,c,k)       ! 14578326 (+1.000)
!     & -z25(d,b,l,j,i,a,c,k)       ! 13578426 (-1.000)
!     & -z25(d,c,l,j,i,b,a,k)       ! 12578346 (-1.000)
!     & +z25(d,c,l,j,i,a,b,k)       ! 12578436 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z25)
       deallocate(x22)
c
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n0,n1,s68,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s126(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s126)
       deallocate(d1)
       deallocate(b2)
       deallocate(s68)
c
       call sum4123(n0,n1,n1,n3,n0,n1,n0,n1,x21,s126,-1.000)
       deallocate(s126)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z24(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k2*k3*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x21,f2,z24)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x21(m,c,j,i)*t3b(d,b,a,l,m,k)          !mcjidbalmk      (+1.000)
     &     - x21(m,b,j,i)*t3b(d,c,a,l,m,k)          !mbjidcalmk      (-1.000)
     &     + x21(m,a,j,i)*t3b(d,c,b,l,m,k)          !majidcblmk      (+1.000)
     &     - x21(m,c,k,i)*t3b(d,b,a,l,m,j)          !mckidbalmj      (-1.000)
     &     + x21(m,b,k,i)*t3b(d,c,a,l,m,j)          !mbkidcalmj      (+1.000)
     &     - x21(m,a,k,i)*t3b(d,c,b,l,m,j)          !makidcblmj      (-1.000)
     &     - x21(m,c,i,j)*t3b(d,b,a,l,m,k)          !mcijdbalmk      (-1.000)
     &     + x21(m,b,i,j)*t3b(d,c,a,l,m,k)          !mbijdcalmk      (+1.000)
     &     - x21(m,a,i,j)*t3b(d,c,b,l,m,k)          !maijdcblmk      (-1.000)
     &     + x21(m,c,i,k)*t3b(d,b,a,l,m,j)          !mcikdbalmj      (+1.000)
     &     - x21(m,b,i,k)*t3b(d,c,a,l,m,j)          !mbikdcalmj      (-1.000)
     &     + x21(m,a,i,k)*t3b(d,c,b,l,m,j)          !maikdcblmj      (+1.000)
     &     + x21(m,c,k,j)*t3b(d,b,a,l,m,i)          !mckjdbalmi      (+1.000)
     &     - x21(m,b,k,j)*t3b(d,c,a,l,m,i)          !mbkjdcalmi      (-1.000)
     &     + x21(m,a,k,j)*t3b(d,c,b,l,m,i)          !makjdcblmi      (+1.000)
     &     - x21(m,c,j,k)*t3b(d,b,a,l,m,i)          !mcjkdbalmi      (-1.000)
     &     + x21(m,b,j,k)*t3b(d,c,a,l,m,i)          !mbjkdcalmi      (+1.000)
     &     - x21(m,a,j,k)*t3b(d,c,b,l,m,i)          !majkdcblmi      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13456278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum12456378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
!       call sum12356478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum13457268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
!       call sum12457368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum12357468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
!       call sum13456287(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
!       call sum12456387(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum12356487(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
!       call sum13457286(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum12457386(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
!       call sum12357486(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum13458267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum12458367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
!       call sum12358467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum13458276(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
!       call sum12458376(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24, 1.000)
!       call sum12358476(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z24,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z24(d,b,a,l,k,c,j,i)       ! 13456278 (+1.000)
!     & -z24(d,c,a,l,k,b,j,i)       ! 12456378 (-1.000)
!     & +z24(d,c,b,l,k,a,j,i)       ! 12356478 (+1.000)
!     & -z24(d,b,a,l,j,c,k,i)       ! 13457268 (-1.000)
!     & +z24(d,c,a,l,j,b,k,i)       ! 12457368 (+1.000)
!     & -z24(d,c,b,l,j,a,k,i)       ! 12357468 (-1.000)
!     & -z24(d,b,a,l,k,c,i,j)       ! 13456287 (-1.000)
!     & +z24(d,c,a,l,k,b,i,j)       ! 12456387 (+1.000)
!     & -z24(d,c,b,l,k,a,i,j)       ! 12356487 (-1.000)
!     & +z24(d,b,a,l,j,c,i,k)       ! 13457286 (+1.000)
!     & -z24(d,c,a,l,j,b,i,k)       ! 12457386 (-1.000)
!     & +z24(d,c,b,l,j,a,i,k)       ! 12357486 (+1.000)
!     & +z24(d,b,a,l,i,c,k,j)       ! 13458267 (+1.000)
!     & -z24(d,c,a,l,i,b,k,j)       ! 12458367 (-1.000)
!     & +z24(d,c,b,l,i,a,k,j)       ! 12358467 (+1.000)
!     & -z24(d,b,a,l,i,c,j,k)       ! 13458276 (-1.000)
!     & +z24(d,c,a,l,i,b,j,k)       ! 12458376 (+1.000)
!     & -z24(d,c,b,l,i,a,j,k)       ! 12358476 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z24)
       deallocate(x21)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(q15(n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,d2,q15)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n0,n1,n0,n1,x8,q15, 1.000)
       deallocate(q15)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(h2(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
     & n0+1:n1,n0+1:n1))
       call reorder12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4b,h2)
       allocate(u44(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k1*k3*k3
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,h2,u44)
       deallocate(d1)
       deallocate(h2)
c
       call sum234561(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x50,u44,2.000)
       deallocate(u44)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z138(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x50,d2,z138)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum                                !top two switched
     &     + (x50(m,c,a,k,j,i)*t2b(d,b,l,m)         !mcakjidblm      (+0.500)
     &     - x50(m,b,a,k,j,i)*t2b(d,c,l,m)          !mbakjidclm      (-0.500)
     &     - x50(m,c,b,k,j,i)*t2b(d,a,l,m))/2.0d0   !mcbkjidalm      (-0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z138,-0.500)
!       call sum13524678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z138, 0.500)
!       call sum14523678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z138,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z138(d,b,l,c,a,k,j,i)      ! 13524678 (+0.500) top two switched
!     & -z138(d,c,l,b,a,k,j,i)       ! 12534678 (-0.500)
!     & -z138(d,a,l,c,b,k,j,i))/2.0d0! 14523678 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z138)
       deallocate(x50)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder2413(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,t2b,d2)
       allocate(s69(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,d2,s69)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n2,n3,n0,n2,x19,s69, 1.000)
       deallocate(s69)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder1423(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n1,n1,n3,n0,n2,t2b,d2)
       allocate(s70(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k2*k3
       i3=k1*k4
       call egemm(i1,i2,i3,d1,d2,s70)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n1,n3,n1,n3,n0,n2,x17,s70,-1.000)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4312(n1,n3,n0,n2,n1,n3,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s70,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s149(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s149)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s149, 1.000)
       deallocate(s149)
c
       allocate(d1(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2))
       call reorder3412(n1,n3,n0,n2,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n0,n2,s70,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s127(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2))
       i1=k2*k3*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s127)
       deallocate(d1)
       deallocate(b2)
       deallocate(s70)
c
       call sum4123(n0,n2,n1,n3,n0,n2,n0,n1,x5,s127,-1.000)
       deallocate(s127)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder1243(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n1,n0,n2,t2b,d2)
       allocate(q16(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,d2,q16)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n0,n2,n0,n2,x10,q16, 1.000)
       deallocate(q16)
c
       allocate(d1(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder1243(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n0,n2,n2,n3,n1,n3,intm,d1)
       allocate(d2(n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n0,n2,n2,n3,n1,n3,t2b,d2)
       allocate(s71(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4
       i2=k3*k4
       i3=k2*k1
       call egemm(i1,i2,i3,d1,d2,s71)
       deallocate(d1)
       deallocate(d2)
c
       call sum3412(n2,n3,n1,n3,n2,n3,n1,n3,x18,s71, 1.000)
c
       allocate(d1(n2+1:n3,n1+1:n3,n2+1:n3,n1+1:n3))
       call reorder3412(n2,n3,n1,n3,n2,n3,n1,n3,
     & n2,n3,n1,n3,n2,n3,n1,n3,s71,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s148(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s148)
       deallocate(d1)
       deallocate(b2)
c
       call sum4123(n1,n3,n2,n3,n1,n3,n0,n2,x2,s148, 1.000)
       deallocate(s148)
c
       allocate(d1(n1+1:n3,n2+1:n3,n2+1:n3,n1+1:n3))
       call reorder4312(n2,n3,n1,n3,n2,n3,n1,n3,
     & n1,n3,n2,n3,n2,n3,n1,n3,s71,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s128(n0+1:n1,n2+1:n3,n2+1:n3,n1+1:n3))
       i1=k3*k4*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s128)
       deallocate(d1)
       deallocate(b2)
       deallocate(s71)
c
       call sum4123(n2,n3,n2,n3,n1,n3,n0,n1,x6,s128, 1.000)
       deallocate(s128)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n2,n3,intm,d1)
       allocate(d2(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       call reorder2431(n2,n3,n1,n3,n0,n2,n0,n1,
     & n1,n3,n0,n1,n0,n2,n2,n3,t2b,d2)
       allocate(q17(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2*k1*k3
       call egemm(i1,i2,i3,d1,d2,q17)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n2,n3,n2,n3,x11,q17,-1.000)
       deallocate(q17)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n2,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder1432(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n1,n0,n2,n1,n3,t2b,d2)
       allocate(q18(n1+1:n3,n1+1:n3))
       i1=k3
       i2=k3
       i3=k2*k1*k4
       call egemm(i1,i2,i3,d1,d2,q18)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n1,n3,n1,n3,x9,q18,-1.000)
       deallocate(q18)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder1234(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n0,n1,t2b,d2)
       allocate(u45(n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k1*k2*k3
       i3=k4
       call egemm(i1,i2,i3,d1,d2,u45)
       deallocate(d1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder546123(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
!     & n0,n2,n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,u45,f1)
!       allocate(h2(n0+1:n2,n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder61523478(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4c,h2)
!       allocate(z154(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k1*k3*k3*k4
!       i3=k2*k4*k2
!       call egemm(i1,i2,i3,f1,h2,z154)
!       deallocate(f1)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2;do n=n0+1,n2
             sum=sum                               !top two switched
     &     + (u45(b,l,i,f,m,n)*t4c(f,d,c,a,n,m,k,j)      !blifmnfdcanmkj  (+0.500)
     &     - u45(c,l,i,f,m,n)*t4c(f,d,b,a,n,m,k,j)       !clifmnfdbanmkj  (-0.500)
     &     - u45(a,l,i,f,m,n)*t4c(f,d,c,b,n,m,k,j)       !alifmnfdcbnmkj  (-0.500)
     &     + u45(c,l,j,f,m,n)*t4c(f,d,b,a,n,m,k,i)       !cljfmnfdbanmki  (+0.500)
     &     - u45(b,l,j,f,m,n)*t4c(f,d,c,a,n,m,k,i)       !bljfmnfdcanmki  (-0.500)
     &     + u45(a,l,j,f,m,n)*t4c(f,d,c,b,n,m,k,i)       !aljfmnfdcbnmki  (+0.500)
     &     - u45(c,l,k,f,m,n)*t4c(f,d,b,a,n,m,j,i)       !clkfmnfdbanmji  (-0.500)
     &     + u45(b,l,k,f,m,n)*t4c(f,d,c,a,n,m,j,i)       !blkfmnfdcanmji  (+0.500)
     &     - u45(a,l,k,f,m,n)*t4c(f,d,c,b,n,m,j,i))/2.0d0!alkfmnfdcbnmji  (-0.500)
             enddo;enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13467258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154,-0.500)
!       call sum12467358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154, 0.500)
!       call sum12367458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154,-0.500)
!       call sum13468257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154, 0.500)
!       call sum12468357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154,-0.500)
!       call sum12368457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154, 0.500)
!       call sum13478256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154,-0.500)
!       call sum12478356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154, 0.500)
!       call sum12378456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z154,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z154(d,c,a,k,j,b,l,i)      ! 12467358 (+0.500) top two switched
!     & -z154(d,b,a,k,j,c,l,i)       ! 13467258 (-0.500)
!     & -z154(d,c,b,k,j,a,l,i)       ! 12367458 (-0.500)
!     & +z154(d,b,a,k,i,c,l,j)       ! 13468257 (+0.500)
!     & -z154(d,c,a,k,i,b,l,j)       ! 12468357 (-0.500)
!     & +z154(d,c,b,k,i,a,l,j)       ! 12368457 (+0.500)
!     & -z154(d,b,a,j,i,c,l,k)       ! 13478256 (-0.500)
!     & +z154(d,c,a,j,i,b,l,k)       ! 12478356 (+0.500)
!     & -z154(d,c,b,j,i,a,l,k))/2.0d0! 12378456 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z154)
c
       allocate(f1(n2+1:n3,n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder465123(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n1,n3,n0,n2,n0,n1,u45,f1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(u111(n1+1:n3,n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k2
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,f1,d2,u111)
       deallocate(f1)
       deallocate(d2)
c
!       allocate(f1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder341526(n1,n3,n0,n1,n0,n2,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,u111,f1)
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z307(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,f1,d2,z307)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - u111(b,k,m,c,l,i)*t2b(d,a,m,j)         !bkmclidamj      (-1.000)
     &     + u111(c,k,m,b,l,i)*t2b(d,a,m,j)         !ckmblidamj      (+1.000)
     &     - u111(c,k,m,a,l,i)*t2b(d,b,m,j)         !ckmalidbmj      (-1.000)
     &     + u111(b,j,m,c,l,i)*t2b(d,a,m,k)         !bjmclidamk      (+1.000)
     &     - u111(c,j,m,b,l,i)*t2b(d,a,m,k)         !cjmblidamk      (-1.000)
     &     + u111(c,j,m,a,l,i)*t2b(d,b,m,k)         !cjmalidbmk      (+1.000)
     &     + u111(b,k,m,c,l,j)*t2b(d,a,m,i)         !bkmcljdami      (+1.000)
     &     + u111(c,k,m,a,l,j)*t2b(d,b,m,i)         !ckmaljdbmi      (+1.000)
     &     - u111(c,k,m,b,l,j)*t2b(d,a,m,i)         !ckmbljdami      (-1.000)
     &     + u111(c,i,m,b,l,j)*t2b(d,a,m,k)         !cimbljdamk      (+1.000)
     &     - u111(c,i,m,a,l,j)*t2b(d,b,m,k)         !cimaljdbmk      (-1.000)
     &     - u111(b,i,m,c,l,j)*t2b(d,a,m,k)         !bimcljdamk      (-1.000)
     &     - u111(c,j,m,a,l,k)*t2b(d,b,m,i)         !cjmalkdbmi      (-1.000)
     &     + u111(c,j,m,b,l,k)*t2b(d,a,m,i)         !cjmblkdami      (+1.000)
     &     - u111(b,j,m,c,l,k)*t2b(d,a,m,i)         !bjmclkdami      (-1.000)
     &     + u111(c,i,m,a,l,k)*t2b(d,b,m,j)         !cimalkdbmj      (+1.000)
     &     - u111(c,i,m,b,l,k)*t2b(d,a,m,j)         !cimblkdamj      (-1.000)
     &     + u111(b,i,m,c,l,k)*t2b(d,a,m,j)         !bimclkdamj      (+1.000)
     &     + u111(a,k,m,c,l,i)*t2b(d,b,m,j)         !akmclidbmj      (+1.000)
     &     - u111(a,k,m,b,l,i)*t2b(d,c,m,j)         !akmblidcmj      (-1.000)
     &     + u111(b,k,m,a,l,i)*t2b(d,c,m,j)         !bkmalidcmj      (+1.000)
     &     - u111(a,j,m,c,l,i)*t2b(d,b,m,k)         !ajmclidbmk      (-1.000)
     &     + u111(a,j,m,b,l,i)*t2b(d,c,m,k)         !ajmblidcmk      (+1.000)
     &     - u111(b,j,m,a,l,i)*t2b(d,c,m,k)         !bjmalidcmk      (-1.000)
     &     - u111(a,k,m,c,l,j)*t2b(d,b,m,i)         !akmcljdbmi      (-1.000)
     &     + u111(a,k,m,b,l,j)*t2b(d,c,m,i)         !akmbljdcmi      (+1.000)
     &     - u111(b,k,m,a,l,j)*t2b(d,c,m,i)         !bkmaljdcmi      (-1.000)
     &     + u111(a,i,m,c,l,j)*t2b(d,b,m,k)         !aimcljdbmk      (+1.000)
     &     - u111(a,i,m,b,l,j)*t2b(d,c,m,k)         !aimbljdcmk      (-1.000)
     &     + u111(b,i,m,a,l,j)*t2b(d,c,m,k)         !bimaljdcmk      (+1.000)
     &     + u111(a,j,m,c,l,k)*t2b(d,b,m,i)         !ajmclkdbmi      (+1.000)
     &     - u111(a,j,m,b,l,k)*t2b(d,c,m,i)         !ajmblkdcmi      (-1.000)
     &     + u111(b,j,m,a,l,k)*t2b(d,c,m,i)         !bjmalkdcmi      (+1.000)
     &     - u111(a,i,m,c,l,k)*t2b(d,b,m,j)         !aimclkdbmj      (-1.000)
     &     + u111(a,i,m,b,l,k)*t2b(d,c,m,j)         !aimblkdcmj      (+1.000)
     &     - u111(b,i,m,a,l,k)*t2b(d,c,m,j)         !bimalkdcmj      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14723568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum14732568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum13742568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum14623578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum14632578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum13642578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum14823567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum13842567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum14832567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum14632587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum13642587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum14623587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum13842576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum14832576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum14823576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum13742586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum14732586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum14723586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!!!!!!!!   please see note below. the missing permutations are 
!!!!!!!!   (ab),(acb),(ac),(jk)(ab),(jk)(acb),(jk)(ac),
!!!!!!!!   (ij)(ab),(ij)(acb),(ij)(ac),(ijk)(ab),(ijk)(acb),(ijk)(ac),
!!!!!!!!   (ikj)(ab),(ikj)(acb),(ikj)(ac),(ik)(ab),(ik)(acb),(ik)(ac)
!       call sum13724568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum12734568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum12743568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum13624578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum12643578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum13824567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum12834567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum12843567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum13624587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum12634587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum12643587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum13824576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum12834576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum12843576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum13724586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
!       call sum12734586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307, 1.000)
!       call sum12743586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z307,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z307(d,a,j,b,c,l,k,i)       ! 14732568 (+1.000) top two switched
!     & -z307(d,a,j,c,b,l,k,i)       ! 14723568 (-1.000)
!     & -z307(d,b,j,a,c,l,k,i)       ! 13742568 (-1.000)
!     & +z307(d,a,k,c,b,l,j,i)       ! 14623578 (+1.000)
!     & -z307(d,a,k,b,c,l,j,i)       ! 14632578 (-1.000)
!     & +z307(d,b,k,a,c,l,j,i)       ! 13642578 (+1.000)
!     & +z307(d,a,i,c,b,l,k,j)       ! 14823567 (+1.000)
!     & +z307(d,b,i,a,c,l,k,j)       ! 13842567 (+1.000)
!     & -z307(d,a,i,b,c,l,k,j)       ! 14832567 (-1.000)
!     & +z307(d,a,k,b,c,l,i,j)       ! 14632587 (+1.000)
!     & -z307(d,b,k,a,c,l,i,j)       ! 13642587 (-1.000)
!     & -z307(d,a,k,c,b,l,i,j)       ! 14623587 (-1.000)
!     & -z307(d,b,i,a,c,l,j,k)       ! 13842576 (-1.000)
!     & +z307(d,a,i,b,c,l,j,k)       ! 14832576 (+1.000)
!     & -z307(d,a,i,c,b,l,j,k)       ! 14823576 (-1.000)
!     & +z307(d,b,j,a,c,l,i,k)       ! 13742586 (+1.000)
!     & -z307(d,a,j,b,c,l,i,k)       ! 14732586 (-1.000)
!     & +z307(d,a,j,c,b,l,i,k)       ! 14723586 (+1.000)
!     & +z307(d,b,j,c,a,l,k,i)       ! 13724568 (+1.000)
!     & -z307(d,c,j,b,a,l,k,i)       ! 12734568 (-1.000)
!     & +z307(d,c,j,a,b,l,k,i)       ! 12743568 (+1.000)
!     & -z307(d,b,k,c,a,l,j,i)       ! 13624578 (-1.000)
!     & +z307(d,c,k,b,a,l,j,i)       ! 12634578 (+1.000)
!     & -z307(d,c,k,a,b,l,j,i)       ! 12643578 (-1.000)
!     & -z307(d,b,i,c,a,l,k,j)       ! 13824567 (-1.000)
!     & +z307(d,c,i,b,a,l,k,j)       ! 12834567 (+1.000)
!     & -z307(d,c,i,a,b,l,k,j)       ! 12843567 (-1.000)
!     & +z307(d,b,k,c,a,l,i,j)       ! 13624587 (+1.000)
!     & -z307(d,c,k,b,a,l,i,j)       ! 12634587 (-1.000)
!     & +z307(d,c,k,a,b,l,i,j)       ! 12643587 (+1.000)
!     & +z307(d,b,i,c,a,l,j,k)       ! 13824576 (+1.000)
!     & -z307(d,c,i,b,a,l,j,k)       ! 12834576 (-1.000)
!     & +z307(d,c,i,a,b,l,j,k)       ! 12843576 (+1.000)
!     & -z307(d,b,j,c,a,l,i,k)       ! 13724586 (-1.000)
!     & +z307(d,c,j,b,a,l,i,k)       ! 12734586 (+1.000)
!     & -z307(d,c,j,a,b,l,i,k)       ! 12743586 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z307)
       deallocate(u111)
c
c
c
!!!!!!!!! the following term has been explicitely changed by nicholas bauman.
!!!!!!!!! this is the same as u111/z307. during the factorization the terms was 
!!!!!!!!! split by factorization based on anti symmetrizers of the entire term.
!!!!!!!!! while not proven, it is believed (nb) that during the spliting and
!!!!!!!!! relabeling of incidies certain anti symmetrizers were messed up. this is
!!!!!!!!! supported by the fact that if you swap two of the indices below in the sum
!!!!!!!!! routine, much, but not all of the missing terms will appear. if you swap
!!!!!!!!! another set of indices you can get some of the others but may mess with others.
!!!!!!!!! all of the terms are present somewhere in u110 however there is not one
!!!!!!!!! overall swap of incidies that will return the entire set of missing terms.
!!!!!!!!! therefore instead of seperating hte terms through factorization i am
!!!!!!!!! recombining this term with its other half u111 of z307.
!!!!!!!!!b!       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
!!!!!!!!!b!       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
!!!!!!!!!b!     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u45,f1)
!!!!!!!!!b!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!!!!!!!!!b!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!!!!!!!!!b!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!!!!!!!!!b!       allocate(u110(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
!!!!!!!!!b!       i1=k2*k1*k2*k3
!!!!!!!!!b!       i2=k1*k3
!!!!!!!!!b!       i3=k4*k2
!!!!!!!!!b!       call egemm(i1,i2,i3,f1,d2,u110)
!!!!!!!!!b!       deallocate(f1)
!!!!!!!!!b!       deallocate(d2)
!!!!!!!!!b!c
!!!!!!!!!b!       call sum352461(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!!!!!!!!!b!     & x46,u110,-1.000)
!!!!!!!!!b!       deallocate(u110)
c
c
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u45,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s152(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3
       i3=k4*k2
       call egemm1(i1,i3,f1,b2,s152)
       deallocate(f1)
       deallocate(b2)
c
       call sum2341(n0,n2,n1,n3,n0,n2,n0,n1,x5,s152,-1.000)
       deallocate(s152)
c
       allocate(f1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder541236(n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,n0,n2,u45,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u96(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2*k1*k2*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u96)
       deallocate(f1)
       deallocate(b2)
       deallocate(u45)
c
      call sum324561(n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,x47,u96,-1.000)
       deallocate(u96)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z127(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k1*k1*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,x47,f2,z127)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x47(m,f,d,c,l,i)*t3b(f,b,a,m,k,j)      !mfdclifbamkj    (+1.000)
     &     - x47(m,f,d,b,l,i)*t3b(f,c,a,m,k,j)      !mfdblifcamkj    (-1.000)
     &     + x47(m,f,d,a,l,i)*t3b(f,c,b,m,k,j)      !mfdalifcbmkj    (+1.000)
     &     - x47(m,f,d,c,l,j)*t3b(f,b,a,m,k,i)      !mfdcljfbamki    (-1.000)
     &     + x47(m,f,d,b,l,j)*t3b(f,c,a,m,k,i)      !mfdbljfcamki    (+1.000)
     &     - x47(m,f,d,a,l,j)*t3b(f,c,b,m,k,i)      !mfdaljfcbmki    (-1.000)
     &     + x47(m,f,d,c,l,k)*t3b(f,b,a,m,j,i)      !mfdclkfbamji    (+1.000)
     &     - x47(m,f,d,b,l,k)*t3b(f,c,a,m,j,i)      !mfdblkfcamji    (-1.000)
     &     + x47(m,f,d,a,l,k)*t3b(f,c,b,m,j,i)      !mfdalkfcbmji    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34671258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127, 1.000)
!       call sum24671358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127,-1.000)
!       call sum23671458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127, 1.000)
!       call sum34681257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127,-1.000)
!       call sum24681357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127, 1.000)
!       call sum23681457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127,-1.000)
!       call sum34781256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127, 1.000)
!       call sum24781356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127,-1.000)
!       call sum23781456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z127, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z127(b,a,k,j,d,c,l,i)       ! 34671258 (+1.000)
!     & -z127(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & +z127(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!     & -z127(b,a,k,i,d,c,l,j)       ! 34681257 (-1.000)
!     & +z127(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & -z127(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z127(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z127(c,a,j,i,d,b,l,k)       ! 24781356 (-1.000)
!     & +z127(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z127)
       deallocate(x47)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(h2(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,
     & n0+1:n1,n0+1:n1))
       call reorder12534678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n0,n1,n2,n3,n2,n3,n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4c,h2)
       allocate(u46(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k1*k2*k3*k3
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,h2,u46)
       deallocate(d1)
       deallocate(h2)
c
       call sum234561(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x44,u46,0.500)
       deallocate(u46)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder1324(n2,n3,n1,n3,n0,n2,n0,n1,
     & n2,n3,n0,n2,n1,n3,n0,n1,t2b,d2)
       allocate(s72(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s72)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n1,n3,n0,n1,x20,s72, 1.000)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4312(n1,n3,n0,n1,n2,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s72,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s153(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s153)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s153,-1.000)
       deallocate(s153)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder3412(n1,n3,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s72,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s151(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s151)
       deallocate(d1)
       deallocate(b2)
       deallocate(s72)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s151, 1.000)
       deallocate(s151)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder4213(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n1,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s73(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s73)
       deallocate(d1)
       deallocate(d2)
c
       call sum2314(n0,n1,n2,n3,n0,n2,n0,n1,x1,s73, 1.000)
       deallocate(s73)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n1,n3,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s74(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       i1=k3*k3
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s74)
       deallocate(d1)
       deallocate(d2)
c
       call sum2413(n1,n3,n2,n3,n1,n3,n0,n2,x2,s74, 1.000)
       deallocate(s74)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s75(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s75)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n1,n1,n3,n2,n3,n0,n2,x7,s75, 1.000)
c
       allocate(d1(n0+1:n1,n1+1:n3,n2+1:n3,n0+1:n2))
       call reorder4312(n2,n3,n0,n2,n1,n3,n0,n1,
     & n0,n1,n1,n3,n2,n3,n0,n2,s75,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s135(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2))
       i1=k2*k4*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s135)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n1,n3,n2,n3,n1,n3,n0,n2,x2,s135,-1.000)
       deallocate(s135)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3412(n2,n3,n0,n2,n1,n3,n0,n1,
     & n1,n3,n0,n1,n2,n3,n0,n2,s75,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s134(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s134)
       deallocate(d1)
       deallocate(b2)
       deallocate(s75)
c
       call sum4123(n0,n1,n2,n3,n0,n2,n0,n1,x1,s134, 1.000)
       deallocate(s134)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder1324(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n2,n3,n0,n2,t2c,d2)
       allocate(s76(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k2*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,d2,s76)
       deallocate(d1)
       deallocate(d2)
c
       call sum3421(n0,n2,n2,n3,n2,n3,n0,n2,x19,s76, 1.000)
       deallocate(s76)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(d2(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder1234(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n2,n3,n0,n2,n0,n2,t2c,d2)
       allocate(q19(n0+1:n2,n0+1:n2))
       i1=k2
       i2=k2
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,d2,q19)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n0,n2,n0,n2,x10,q19,-0.500)
       deallocate(q19)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
       allocate(d2(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder1432(n2,n3,n2,n3,n0,n2,n0,n2,
     & n2,n3,n0,n2,n0,n2,n2,n3,t2c,d2)
       allocate(q20(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2*k2*k4
       call egemm(i1,i2,i3,d1,d2,q20)
       deallocate(d1)
       deallocate(d2)
c
       call sum21(n2,n3,n2,n3,x11,q20, 0.500)
       deallocate(q20)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder265134(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n2,n3,n1,n3,n0,n2,t3b,f2)
       allocate(s77(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1*k1*k3
       call egemm(i1,i2,i3,d1,f2,s77)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x51(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       x51=0.0d0
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x51,s77, 1.000)
       deallocate(s77)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
       allocate(u47(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k1*k3
       i3=k3*k3
       call egemm(i1,i2,i3,d1,f2,u47)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder651234(n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u47,f1)
!       allocate(f2(n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
!       call reorder561234(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,t3b,f2)
!       allocate(z164(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3
!       i2=k2*k3*k3*k4
!       i3=k1*k1
!       call egemm(i1,i2,i3,f1,f2,z164)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (u47(c,k,j,i,m,n)*t3b(d,b,a,l,n,m)       !ckjimndbalnm    (+0.250)
     &     - u47(b,k,j,i,m,n)*t3b(d,c,a,l,n,m)        !bkjimndcalnm    (-0.250)
     &     + u47(a,k,j,i,m,n)*t3b(d,c,b,l,n,m))/4.0d0 !akjimndcblnm    (+0.250)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13452678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z164, 0.250)
!       call sum12453678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z164,-0.250)
!       call sum12354678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z164, 0.250)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z164(d,b,a,l,c,k,j,i)      ! 13452678 (+0.250)
!     & -z164(d,c,a,l,b,k,j,i)       ! 12453678 (-0.250)
!     & +z164(d,c,b,l,a,k,j,i))/4.0d0! 12354678 (+0.250)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z164)
c
       allocate(f1(n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n0,n1,n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n1,n3,n0,n1,n0,n1,n0,n1,u47,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u76(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1*k3*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u76)
       deallocate(f1)
       deallocate(b2)
       deallocate(u47)
c
      call sum213456(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x43,u76,-1.000)
       deallocate(u76)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z111(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x43,d2,z111)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + (x43(m,c,a,k,j,i)*t2b(d,b,l,m)         !mcakjidblm      (+0.500)
     &     - x43(m,c,b,k,j,i)*t2b(d,a,l,m)          !mcbkjidalm      (-0.500)
     &     - x43(m,b,a,k,j,i)*t2b(d,c,l,m)          !mbakjidclm      (-0.500)
     &     + x43(m,a,b,k,j,i)*t2b(d,c,l,m)          !mabkjidclm      (+0.500)
     &     + x43(m,b,c,k,j,i)*t2b(d,a,l,m)          !mbckjidalm      (+0.500)
     &     - x43(m,a,c,k,j,i)*t2b(d,b,l,m))/2.0d0   !mackjidblm      (-0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13524678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z111, 0.500)
!       call sum14523678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z111,-0.500)
!       call sum12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z111,-0.500)
!       call sum12543678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z111, 0.500)
!       call sum14532678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z111, 0.500)
!       call sum13542678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z111,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z111(d,b,l,c,a,k,j,i)      ! 13524678 (+0.500)
!     & -z111(d,a,l,c,b,k,j,i)       ! 14523678 (-0.500)
!     & -z111(d,c,l,b,a,k,j,i)       ! 12534678 (-0.500)
!     & +z111(d,c,l,a,b,k,j,i)       ! 12543678 (+0.500)
!     & +z111(d,a,l,b,c,k,j,i)       ! 14532678 (+0.500)
!     & -z111(d,b,l,a,c,k,j,i))/2.0d0! 13542678 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z111)
       deallocate(x43)
c
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder235146(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,t3b,f2)
       allocate(s78(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,f2,s78)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x52(n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1))
       x52=0.0d0
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x52,s78, 1.000)
       deallocate(s78)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u48(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n0+1:n1))
       i1=k1*k3
       i2=k1*k1*k3*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u48)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder651234(n1,n3,n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,u48,f1)
!       allocate(f2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder521346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z166(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k2*k3*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,f1,f2,z166)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n1+1,n3;do n=n0+1,n1
             sum=sum
     &     + u48(c,b,j,i,f,n)*t3b(d,f,a,l,n,k)      !cbjifndfalnk    (+1.000)
     &     - u48(c,a,j,i,f,n)*t3b(d,f,b,l,n,k)      !cajifndfblnk    (-1.000)
     &     + u48(b,a,j,i,f,n)*t3b(d,f,c,l,n,k)      !bajifndfclnk    (+1.000)
     &     - u48(c,b,k,i,f,n)*t3b(d,f,a,l,n,j)      !cbkifndfalnj    (-1.000)
     &     + u48(c,a,k,i,f,n)*t3b(d,f,b,l,n,j)      !cakifndfblnj    (+1.000)
     &     - u48(b,a,k,i,f,n)*t3b(d,f,c,l,n,j)      !bakifndfclnj    (-1.000)
     &     + u48(c,b,k,j,f,n)*t3b(d,f,a,l,n,i)      !cbkjfndfalni    (+1.000)
     &     - u48(c,a,k,j,f,n)*t3b(d,f,b,l,n,i)      !cakjfndfblni    (-1.000)
     &     + u48(b,a,k,j,f,n)*t3b(d,f,c,l,n,i)      !bakjfndfclni    (+1.000)

             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14562378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166, 1.000)
!       call sum13562478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166,-1.000)
!       call sum12563478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166, 1.000)
!       call sum14572368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166,-1.000)
!       call sum13572468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166, 1.000)
!       call sum12573468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166,-1.000)
!       call sum14582367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166, 1.000)
!       call sum13582467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166,-1.000)
!       call sum12583467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z166, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z166(d,a,l,k,c,b,j,i)       ! 14562378 (+1.000)
!     & -z166(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & +z166(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z166(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z166(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z166(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z166(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z166(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z166(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z166)
c
       allocate(f1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n1,n3,n0,n1,n0,n1,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,u48,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u74(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u74)
       deallocate(f1)
       deallocate(b2)
       deallocate(u48)
c
       call sum612345(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x41,u74,1.000)
       deallocate(u74)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder124356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n1,n3,n0,n1,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(s79(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k1*k3*k3
       call egemm(i1,i2,i3,d1,f2,s79)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x3,s79,-0.500)
       deallocate(s79)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(f2(n1+1:n3,n1+1:n3,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder231456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n1,n3,n2,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u49(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k2*k4
       i3=k3*k3
       call egemm(i1,i2,i3,d1,f2,u49)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder651234(n2,n3,n0,n2,n0,n1,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,u49,f1)
!       allocate(f2(n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,t3a,f2)
!       allocate(z168(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k4
!       i2=k1*k3*k3*k3
!       i3=k1*k1
!       call egemm(i1,i2,i3,f1,f2,z168)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (u49(d,l,k,j,m,n)*t3a(c,b,a,n,m,i)       !dlkjmncbanmi    (+0.250)
     &     - u49(d,l,k,i,m,n)*t3a(c,b,a,n,m,j)        !dlkimncbanmj    (-0.250)
     &     + u49(d,l,j,i,m,n)*t3a(c,b,a,n,m,k))/4.0d0 !dljimncbanmk    (+0.250)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23481567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z168, 0.250)
!       call sum23471568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z168,-0.250)
!       call sum23461578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z168, 0.250)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z168(c,b,a,i,d,l,k,j)      ! 23481567 (+0.250)
!     & -z168(c,b,a,j,d,l,k,i)       ! 23471568 (-0.250)
!     & +z168(c,b,a,k,d,l,j,i))/4.0d0! 23461578 (+0.250)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z168)
c
       allocate(f1(n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder561234(n2,n3,n0,n2,n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,u49,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u68(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k4*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u68)
       deallocate(f1)
       deallocate(b2)
       deallocate(u49)
c
!       allocate(f1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder231456(n1,n3,n0,n1,n2,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,u68,f1)
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z228(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,f1,d2,z228)
!       deallocate(f1)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum                                !top two switched
     &     + (u68(b,n,d,l,k,j)*t2a(c,a,n,i)         !bndlkjcani      (+0.500)
     &     - u68(c,n,d,l,k,j)*t2a(b,a,n,i)          !cndlkjbani      (-0.500)
     &     - u68(a,n,d,l,k,j)*t2a(c,b,n,i)          !andlkjcbni      (-0.500)
     &     + u68(c,n,d,l,k,i)*t2a(b,a,n,j)          !cndlkibanj      (+0.500)
     &     - u68(b,n,d,l,k,i)*t2a(c,a,n,j)          !bndlkicanj      (-0.500)
     &     + u68(a,n,d,l,k,i)*t2a(c,b,n,j)          !andlkicbnj      (+0.500)
     &     - u68(c,n,d,l,j,i)*t2a(b,a,n,k)          !cndljibank      (-0.500)
     &     + u68(b,n,d,l,j,i)*t2a(c,a,n,k)          !bndljicank      (+0.500)
     &     - u68(a,n,d,l,j,i)*t2a(c,b,n,k))/2.0d0   !andljicbnk      (-0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34812567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228,-0.500)
!       call sum24813567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228, 0.500)
!       call sum23814567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228,-0.500)
!       call sum34712568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228, 0.500)
!       call sum24713568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228,-0.500)
!       call sum23714568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228, 0.500)
!       call sum34612578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228,-0.500)
!       call sum24613578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228, 0.500)
!       call sum23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z228,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z228(c,a,i,d,b,l,k,j)      ! 24813567 (+0.500) top two switched
!     & -z228(b,a,i,d,c,l,k,j)       ! 34812567 (-0.500)
!     & -z228(c,b,i,d,a,l,k,j)       ! 23814567 (-0.500)
!     & +z228(b,a,j,d,c,l,k,i)       ! 34712568 (+0.500)
!     & -z228(c,a,j,d,b,l,k,i)       ! 24713568 (-0.500)
!     & +z228(c,b,j,d,a,l,k,i)       ! 23714568 (+0.500)
!     & -z228(b,a,k,d,c,l,j,i)       ! 34612578 (-0.500)
!     & +z228(c,a,k,d,b,l,j,i)       ! 24613578 (+0.500)
!     & -z228(c,b,k,d,a,l,j,i))/2.0d0! 23614578 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z228)
       deallocate(u68)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n1,n1,n3,intr,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n1,n3,n1,n3,n0,n1,t3a,f2)
       allocate(s80(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1*k1*k3
       call egemm(i1,i2,i3,d1,f2,s80)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x4,s80, 0.500)
       deallocate(s80)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n2,n1,n3,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder164235(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n1,n0,n2,n2,n3,n1,n3,n0,n2,t3c,f2)
       allocate(s81(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k2*k1*k4
       call egemm(i1,i2,i3,d1,f2,s81)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x51,s81, 2.000)
       deallocate(s81)
c
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z163(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3*k4
!       i2=k1*k1*k1*k3*k3
!       i3=k3
!       call egemm(i1,i2,i3,x51,f2,z163)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum                                !top two switched
     &     + (x51(e,d,b,l)*t3a(e,c,a,k,j,i)         !edblecakji      (+0.500)
     &     - x51(e,d,a,l)*t3a(e,c,b,k,j,i)          !edalecbkji      (-0.500)
     &     - x51(e,d,c,l)*t3a(e,b,a,k,j,i))/2.0d0   !edclebakji      (-0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23678145(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z163,-0.500)
!       call sum24678135(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z163, 0.500)
!       call sum34678125(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z163,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z163(c,a,k,j,i,d,b,l)      ! 24678135 (+0.500) top two switched
!     & -z163(c,b,k,j,i,d,a,l)       ! 23678145 (-0.500)
!     & -z163(b,a,k,j,i,d,c,l))/2.0d0! 34678125 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z163)
       deallocate(x51)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n1))
       call reorder134256(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n1,n3,n0,n2,n2,n3,n0,n2,n0,n1,t3c,f2)
       allocate(s82(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k2*k4
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,f2,s82)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n2,n3,n0,n2,n0,n1,x52,s82, 2.000)
       deallocate(s82)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z165(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k1*k1*k3*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x52,f2,z165)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum                                !top two switched
     &     + (x52(m,d,l,j)*t3a(c,b,a,m,k,i)         !mdljcbamki      (+0.500)
     &     - x52(m,d,l,k)*t3a(c,b,a,m,j,i)          !mdlkcbamji      (-0.500)
     &     - x52(m,d,l,i)*t3a(c,b,a,m,k,j))/2.0d0   !mdlicbamkj      (-0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23478156(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z165,-0.500)
!       call sum23468157(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z165, 0.500)
!       call sum23467158(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z165,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z165(c,b,a,k,i,d,l,j)      ! 23468157 (+0.500) top two switched
!     & -z165(c,b,a,j,i,d,l,k)       ! 23478156 (-0.500)
!     & -z165(c,b,a,k,j,d,l,i))/2.0d0! 23467158 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z165)
       deallocate(x52)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
       allocate(u50(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k1*k3*k3
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u50)
       deallocate(d1)
       deallocate(f2)
c
       allocate(x53(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       x53=0.0d0
       call sum345621(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x53,u50,1.000)
c
       allocate(f1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n1,n3,n0,n1,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,u50,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u93(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u93)
       deallocate(f1)
       deallocate(b2)
       deallocate(u50)
c
       call sum412356(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x44,u93,1.000)
       deallocate(u93)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n0,n2,n1,n3,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n1,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n1,n0,n2,n1,n3,n1,n3,n0,n1,t3b,f2)
       allocate(s83(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k2*k1*k4
       call egemm(i1,i2,i3,d1,f2,s83)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder4123(n1,n3,n1,n3,n0,n1,n1,n3,
!     & n1,n3,n1,n3,n1,n3,n0,n1,s83,d1)
!       allocate(f2(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder213456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z173(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k2*k3*k4
!       i3=k3
!       call egemm(i1,i2,i3,d1,f2,z173)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + s83(b,a,k,e)*t3b(d,e,c,l,j,i)          !bakedeclji      (+1.000)
     &     - s83(c,a,k,e)*t3b(d,e,b,l,j,i)          !cakedeblji      (-1.000)
     &     + s83(c,b,k,e)*t3b(d,e,a,l,j,i)          !cbkedealji      (+1.000)
     &     - s83(b,a,j,e)*t3b(d,e,c,l,k,i)          !bajedeclki      (-1.000)
     &     + s83(c,a,j,e)*t3b(d,e,b,l,k,i)          !cajedeblki      (+1.000)
     &     - s83(c,b,j,e)*t3b(d,e,a,l,k,i)          !cbjedealki      (-1.000)
     &     + s83(c,b,i,e)*t3b(d,e,a,l,k,j)          !cbiedealkj      (+1.000)
     &     - s83(c,a,i,e)*t3b(d,e,b,l,k,j)          !caiedeblkj      (-1.000)
     &     + s83(b,a,i,e)*t3b(d,e,c,l,k,j)          !baiedeclkj      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12578346(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173, 1.000)
!       call sum13578246(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173,-1.000)
!       call sum14578236(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173, 1.000)
!       call sum12568347(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173,-1.000)
!       call sum13568247(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173, 1.000)
!       call sum14568237(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173,-1.000)
!       call sum14567238(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173, 1.000)
!       call sum13567248(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173,-1.000)
!       call sum12567348(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z173, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z173(d,c,l,j,i,b,a,k)       ! 12578346 (+1.000)
!     & -z173(d,b,l,j,i,c,a,k)       ! 13578246 (-1.000)
!     & +z173(d,a,l,j,i,c,b,k)       ! 14578236 (+1.000)
!     & -z173(d,c,l,k,i,b,a,j)       ! 12568347 (-1.000)
!     & +z173(d,b,l,k,i,c,a,j)       ! 13568247 (+1.000)
!     & -z173(d,a,l,k,i,c,b,j)       ! 14568237 (-1.000)
!     & +z173(d,a,l,k,j,c,b,i)       ! 14567238 (+1.000)
!     & -z173(d,b,l,k,j,c,a,i)       ! 13567248 (-1.000)
!     & +z173(d,c,l,k,j,b,a,i)       ! 12567348 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z173)
       deallocate(s83)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3))
       call reorder3124(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n0,n2,n2,n3,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder254136(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n2,n2,n3,n1,n3,n0,n1,t3b,f2)
       allocate(s84(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2*k1*k3
       call egemm(i1,i2,i3,d1,f2,s84)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder4123(n2,n3,n1,n3,n0,n1,n2,n3,
!     & n2,n3,n2,n3,n1,n3,n0,n1,s84,d1)
!       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z174(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k4
!       i2=k1*k1*k2*k3*k3
!       i3=k4
!       call egemm(i1,i2,i3,d1,f2,z174)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3
             sum=sum
     &     - s84(d,a,k,f)*t3b(f,c,b,l,j,i)          !dakffcblji      (-1.000)
     &     + s84(d,b,k,f)*t3b(f,c,a,l,j,i)          !dbkffcalji      (+1.000)
     &     - s84(d,c,k,f)*t3b(f,b,a,l,j,i)          !dckffbalji      (-1.000)
     &     + s84(d,a,j,f)*t3b(f,c,b,l,k,i)          !dajffcblki      (+1.000)
     &     - s84(d,b,j,f)*t3b(f,c,a,l,k,i)          !dbjffcalki      (-1.000)
     &     + s84(d,c,j,f)*t3b(f,b,a,l,k,i)          !dcjffbalki      (+1.000)
     &     - s84(d,c,i,f)*t3b(f,b,a,l,k,j)          !dciffbalkj      (-1.000)
     &     + s84(d,b,i,f)*t3b(f,c,a,l,k,j)          !dbiffcalkj      (+1.000)
     &     - s84(d,a,i,f)*t3b(f,c,b,l,k,j)          !daiffcblkj      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23578146(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174,-1.000)
!       call sum24578136(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174, 1.000)
!       call sum34578126(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174,-1.000)
!       call sum23568147(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174, 1.000)
!       call sum24568137(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174,-1.000)
!       call sum34568127(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174, 1.000)
!       call sum34567128(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174,-1.000)
!       call sum24567138(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174, 1.000)
!       call sum23567148(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z174,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z174(c,b,l,j,i,d,a,k)       ! 23578146 (-1.000)
!     & +z174(c,a,l,j,i,d,b,k)       ! 24578136 (+1.000)
!     & -z174(b,a,l,j,i,d,c,k)       ! 34578126 (-1.000)
!     & +z174(c,b,l,k,i,d,a,j)       ! 23568147 (+1.000)
!     & -z174(c,a,l,k,i,d,b,j)       ! 24568137 (-1.000)
!     & +z174(b,a,l,k,i,d,c,j)       ! 34568127 (+1.000)
!     & -z174(b,a,l,k,j,d,c,i)       ! 34567128 (-1.000)
!     & +z174(c,a,l,k,j,d,b,i)       ! 24567138 (+1.000)
!     & -z174(c,b,l,k,j,d,a,i)       ! 23567148 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z174)
       deallocate(s84)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
       allocate(u51(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k1*k2*k3
       i3=k3*k4
       call egemm(i1,i2,i3,d1,f2,u51)
       deallocate(d1)
       deallocate(f2)
c
       call sum345621(n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,x35,u51,1.000)
c
!       allocate(f2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder451236(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n0,n1,t3b,f2)
!       allocate(z86(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3
!       i2=k1*k3*k3*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x35,f2,z86)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     - x35(n,m,c,l,j,i)*t3b(d,b,a,n,m,k)      !nmcljidbanmk    (-1.000)
     &     + x35(n,m,b,l,j,i)*t3b(d,c,a,n,m,k)      !nmbljidcanmk    (+1.000)
     &     - x35(n,m,a,l,j,i)*t3b(d,c,b,n,m,k)      !nmaljidcbnmk    (-1.000)
     &     + x35(n,m,c,l,k,i)*t3b(d,b,a,n,m,j)      !nmclkidbanmj    (+1.000)
     &     - x35(n,m,b,l,k,i)*t3b(d,c,a,n,m,j)      !nmblkidcanmj    (-1.000)
     &     + x35(n,m,a,l,k,i)*t3b(d,c,b,n,m,j)      !nmalkidcbnmj    (+1.000)
     &     - x35(n,m,c,l,k,j)*t3b(d,b,a,n,m,i)      !nmclkjdbanmi    (-1.000)
     &     + x35(n,m,b,l,k,j)*t3b(d,c,a,n,m,i)      !nmblkjdcanmi    (+1.000)
     &     - x35(n,m,a,l,k,j)*t3b(d,c,b,n,m,i)      !nmalkjdcbnmi    (-1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13462578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86,-1.000)
!       call sum12463578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86, 1.000)
!       call sum12364578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86,-1.000)
!       call sum13472568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86, 1.000)
!       call sum12473568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86,-1.000)
!       call sum12374568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86, 1.000)
!       call sum13482567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86,-1.000)
!       call sum12483567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86, 1.000)
!       call sum12384567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z86,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z86(d,b,a,k,c,l,j,i)       ! 13462578 (-1.000)
!     & +z86(d,c,a,k,b,l,j,i)       ! 12463578 (+1.000)
!     & -z86(d,c,b,k,a,l,j,i)       ! 12364578 (-1.000)
!     & +z86(d,b,a,j,c,l,k,i)       ! 13472568 (+1.000)
!     & -z86(d,c,a,j,b,l,k,i)       ! 12473568 (-1.000)
!     & +z86(d,c,b,j,a,l,k,i)       ! 12374568 (+1.000)
!     & -z86(d,b,a,i,c,l,k,j)       ! 13482567 (-1.000)
!     & +z86(d,c,a,i,b,l,k,j)       ! 12483567 (+1.000)
!     & -z86(d,c,b,i,a,l,k,j)       ! 12384567 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z86)
       deallocate(x35)
c
       allocate(f1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder651234(n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,n0,n1,n0,n1,u51,f1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(u92(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,f1,b2,u92)
       deallocate(f1)
       deallocate(b2)
c
       call sum213456(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x37,u92,1.000)
       deallocate(u92)
c
       allocate(f1(n0+1:n1,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n1,n3,n0,n2,n0,n1,n0,n1,u51,f1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(u82(n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       i1=k1*k1*k2*k3*k2
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,f1,b2,u82)
       deallocate(f1)
       deallocate(b2)
       deallocate(u51)
c
!!!!!!!!!!the sign of this term was explicitly change. the corresponding term in t4d is u86(u49)
       call sum213456(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & x49,u82,-1.000)
       deallocate(u82)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z130(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x49,d2,z130)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + x49(m,c,a,l,k,j)*t2b(d,b,m,i)          !mcalkjdbmi      (+1.000)
     &     - x49(m,c,b,l,k,j)*t2b(d,a,m,i)          !mcblkjdami      (-1.000)
     &     - x49(m,b,a,l,k,j)*t2b(d,c,m,i)          !mbalkjdcmi      (-1.000)
     &     + x49(m,a,b,l,k,j)*t2b(d,c,m,i)          !mablkjdcmi      (+1.000)
     &     + x49(m,b,c,l,k,j)*t2b(d,a,m,i)          !mbclkjdami      (+1.000)
     &     - x49(m,a,c,l,k,j)*t2b(d,b,m,i)          !maclkjdbmi      (-1.000)
     &     - x49(m,c,a,l,k,i)*t2b(d,b,m,j)          !mcalkidbmj      (-1.000)
     &     + x49(m,c,b,l,k,i)*t2b(d,a,m,j)          !mcblkidamj      (+1.000)
     &     + x49(m,b,a,l,k,i)*t2b(d,c,m,j)          !mbalkidcmj      (+1.000)
     &     - x49(m,a,b,l,k,i)*t2b(d,c,m,j)          !mablkidcmj      (-1.000)
     &     - x49(m,b,c,l,k,i)*t2b(d,a,m,j)          !mbclkidamj      (-1.000)
     &     + x49(m,a,c,l,k,i)*t2b(d,b,m,j)          !maclkidbmj      (+1.000)
     &     + x49(m,c,a,l,j,i)*t2b(d,b,m,k)          !mcaljidbmk      (+1.000)
     &     - x49(m,c,b,l,j,i)*t2b(d,a,m,k)          !mcbljidamk      (-1.000)
     &     - x49(m,b,a,l,j,i)*t2b(d,c,m,k)          !mbaljidcmk      (-1.000)
     &     + x49(m,a,b,l,j,i)*t2b(d,c,m,k)          !mabljidcmk      (+1.000)
     &     + x49(m,b,c,l,j,i)*t2b(d,a,m,k)          !mbcljidamk      (+1.000)
     &     - x49(m,a,c,l,j,i)*t2b(d,b,m,k)          !macljidbmk      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13824567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum14823567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
!       call sum12834567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
!       call sum12843567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum14832567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum13842567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
!       call sum13724568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
!       call sum14723568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum12734568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum12743568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
!       call sum14732568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
!       call sum13742568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum13624578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum14623578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
!       call sum12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
!       call sum12643578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum14632578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130, 1.000)
!       call sum13642578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z130,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z130(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z130(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & -z130(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z130(d,c,i,a,b,l,k,j)       ! 12843567 (+1.000)
!     & +z130(d,a,i,b,c,l,k,j)       ! 14832567 (+1.000)
!     & -z130(d,b,i,a,c,l,k,j)       ! 13842567 (-1.000)
!     & -z130(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z130(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & +z130(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z130(d,c,j,a,b,l,k,i)       ! 12743568 (-1.000)
!     & -z130(d,a,j,b,c,l,k,i)       ! 14732568 (-1.000)
!     & +z130(d,b,j,a,c,l,k,i)       ! 13742568 (+1.000)
!     & +z130(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z130(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z130(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z130(d,c,k,a,b,l,j,i)       ! 12643578 (+1.000)
!     & +z130(d,a,k,b,c,l,j,i)       ! 14632578 (+1.000)
!     & -z130(d,b,k,a,c,l,j,i)       ! 13642578 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z130)
       deallocate(x49)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder125346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n0,n1,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(s85(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k1*k3*k4
       call egemm(i1,i2,i3,d1,f2,s85)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder4123(n1,n3,n0,n2,n0,n1,n0,n2,
!     & n0,n2,n1,n3,n0,n2,n0,n1,s85,d1)
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z176(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k1*k3*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,f2,z176)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - s85(a,l,k,n)*t3b(d,c,b,n,j,i)          !alkndcbnji      (-1.000)
     &     + s85(b,l,k,n)*t3b(d,c,a,n,j,i)          !blkndcanji      (+1.000)
     &     - s85(c,l,k,n)*t3b(d,b,a,n,j,i)          !clkndbanji      (-1.000)
     &     + s85(a,l,j,n)*t3b(d,c,b,n,k,i)          !aljndcbnki      (+1.000)
     &     - s85(b,l,j,n)*t3b(d,c,a,n,k,i)          !bljndcanki      (-1.000)
     &     + s85(c,l,j,n)*t3b(d,b,a,n,k,i)          !cljndbanki      (+1.000)
     &     - s85(c,l,i,n)*t3b(d,b,a,n,k,j)          !clindbankj      (-1.000)
     &     + s85(b,l,i,n)*t3b(d,c,a,n,k,j)          !blindcankj      (+1.000)
     &     - s85(a,l,i,n)*t3b(d,c,b,n,k,j)          !alindcbnkj      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12378456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176,-1.000)
!       call sum12478356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176, 1.000)
!       call sum13478256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176,-1.000)
!       call sum12368457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176, 1.000)
!       call sum12468357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176,-1.000)
!       call sum13468257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176, 1.000)
!       call sum13467258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176,-1.000)
!       call sum12467358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176, 1.000)
!       call sum12367458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z176,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z176(d,c,b,j,i,a,l,k)       ! 12378456 (-1.000)
!     & +z176(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z176(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z176(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
!     & -z176(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z176(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z176(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z176(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z176(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z176)
       deallocate(s85)
c
       allocate(d1(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4132(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n1,n1,n3,n0,n2,intm,d1)
       allocate(f2(n2+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder152346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n1,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u52(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3
       i2=k1*k2*k3*k3
       i3=k1*k4
       call egemm(i1,i2,i3,d1,f2,u52)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder651234(n1,n3,n1,n3,n0,n2,n0,n1,n1,n3,n0,n2,
!     & n0,n2,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,u52,f1)
!       allocate(f2(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder421356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n1,n3,n2,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z177(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k3
!       i2=k1*k1*k3*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,f1,f2,z177)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do n=n0+1,n2
             sum=sum
     &     - u52(b,a,l,k,e,n)*t3b(d,e,c,n,j,i)      !balkendecnji    (-1.000)
     &     + u52(c,a,l,k,e,n)*t3b(d,e,b,n,j,i)      !calkendebnji    (+1.000)
     &     - u52(c,b,l,k,e,n)*t3b(d,e,a,n,j,i)      !cblkendeanji    (-1.000)
     &     + u52(b,a,l,j,e,n)*t3b(d,e,c,n,k,i)      !baljendecnki    (+1.000)
     &     - u52(c,a,l,j,e,n)*t3b(d,e,b,n,k,i)      !caljendebnki    (-1.000)
     &     + u52(c,b,l,j,e,n)*t3b(d,e,a,n,k,i)      !cbljendeanki    (+1.000)
     &     - u52(c,b,l,i,e,n)*t3b(d,e,a,n,k,j)      !cbliendeankj    (-1.000)
     &     + u52(c,a,l,i,e,n)*t3b(d,e,b,n,k,j)      !caliendebnkj    (+1.000)
     &     - u52(b,a,l,i,e,n)*t3b(d,e,c,n,k,j)      !baliendecnkj    (-1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12783456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177,-1.000)
!       call sum13782456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177, 1.000)
!       call sum14782356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177,-1.000)
!       call sum12683457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177, 1.000)
!       call sum13682457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177,-1.000)
!       call sum14682357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177, 1.000)
!       call sum14672358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177,-1.000)
!       call sum13672458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177, 1.000)
!       call sum12673458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z177,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z177(d,c,j,i,b,a,l,k)       ! 12783456 (-1.000)
!     & +z177(d,b,j,i,c,a,l,k)       ! 13782456 (+1.000)
!     & -z177(d,a,j,i,c,b,l,k)       ! 14782356 (-1.000)
!     & +z177(d,c,k,i,b,a,l,j)       ! 12683457 (+1.000)
!     & -z177(d,b,k,i,c,a,l,j)       ! 13682457 (-1.000)
!     & +z177(d,a,k,i,c,b,l,j)       ! 14682357 (+1.000)
!     & -z177(d,a,k,j,c,b,l,i)       ! 14672358 (-1.000)
!     & +z177(d,b,k,j,c,a,l,i)       ! 13672458 (+1.000)
!     & -z177(d,c,k,j,b,a,l,i)       ! 12673458 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z177)
c
       allocate(f1(n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder561234(n1,n3,n1,n3,n0,n2,n0,n1,n1,n3,n0,n2,
     & n1,n3,n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,u52,f1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(u78(n0+1:n1,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       i1=k1*k2*k3*k3*k2
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,f1,b2,u78)
       deallocate(f1)
       deallocate(b2)
       deallocate(u52)
c
       call sum612345(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x46,u78,1.000)
       deallocate(u78)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z122(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x46,d2,z122)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     + x46(n,b,a,l,k,i)*t2b(d,c,n,j)          !nbalkidcnj      (+1.000)
     &     - x46(n,c,a,l,k,i)*t2b(d,b,n,j)          !ncalkidbnj      (-1.000)
     &     + x46(n,c,b,l,k,i)*t2b(d,a,n,j)          !ncblkidanj      (+1.000)
     &     - x46(n,b,a,l,j,i)*t2b(d,c,n,k)          !nbaljidcnk      (-1.000)
     &     + x46(n,c,a,l,j,i)*t2b(d,b,n,k)          !ncaljidbnk      (+1.000)
     &     - x46(n,c,b,l,j,i)*t2b(d,a,n,k)          !ncbljidank      (-1.000)
     &     - x46(n,b,a,l,k,j)*t2b(d,c,n,i)          !nbalkjdcni      (-1.000)
     &     + x46(n,c,a,l,k,j)*t2b(d,b,n,i)          !ncalkjdbni      (+1.000)
     &     - x46(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (-1.000)
     &     + x46(n,b,a,l,j,k)*t2b(d,c,n,i)          !nbaljkdcni      (+1.000)
     &     - x46(n,c,a,l,j,k)*t2b(d,b,n,i)          !ncaljkdbni      (-1.000)
     &     + x46(n,c,b,l,j,k)*t2b(d,a,n,i)          !ncbljkdani      (+1.000)
     &     + x46(n,b,a,l,i,j)*t2b(d,c,n,k)          !nbalijdcnk      (+1.000)
     &     - x46(n,c,a,l,i,j)*t2b(d,b,n,k)          !ncalijdbnk      (-1.000)
     &     + x46(n,c,b,l,i,j)*t2b(d,a,n,k)          !ncblijdank      (+1.000)
     &     - x46(n,b,a,l,i,k)*t2b(d,c,n,j)          !nbalikdcnj      (-1.000)
     &     + x46(n,c,a,l,i,k)*t2b(d,b,n,j)          !ncalikdbnj      (+1.000)
     &     - x46(n,c,b,l,i,k)*t2b(d,a,n,j)          !ncblikdanj      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12734568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum13724568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
!       call sum14723568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
!       call sum13624578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum14623578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
!       call sum12834567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
!       call sum13824567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum14823567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
!       call sum12834576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum13824576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
!       call sum14823576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum12634587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum13624587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
!       call sum14623587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum12734586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
!       call sum13724586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122, 1.000)
!       call sum14723586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z122,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z122(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z122(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z122(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & -z122(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z122(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z122(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & -z122(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z122(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z122(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & +z122(d,c,i,b,a,l,j,k)       ! 12834576 (+1.000)
!     & -z122(d,b,i,c,a,l,j,k)       ! 13824576 (-1.000)
!     & +z122(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & +z122(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!     & -z122(d,b,k,c,a,l,i,j)       ! 13624587 (-1.000)
!     & +z122(d,a,k,c,b,l,i,j)       ! 14623587 (+1.000)
!     & -z122(d,c,j,b,a,l,i,k)       ! 12734586 (-1.000)
!     & +z122(d,b,j,c,a,l,i,k)       ! 13724586 (+1.000)
!     & -z122(d,a,j,c,b,l,i,k)       ! 14723586 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z122)
       deallocate(x46)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder251346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u53(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k2*k3*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u53)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(f1(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder651234(n2,n3,n1,n3,n0,n2,n0,n1,n2,n3,n0,n2,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,u53,f1)
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z178(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3*k4
!       i2=k1*k1*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,f1,f2,z178)
!       deallocate(f1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do n=n0+1,n2
             sum=sum
     &     + u53(d,a,l,k,f,n)*t3b(f,c,b,n,j,i)      !dalkfnfcbnji    (+1.000)
     &     - u53(d,b,l,k,f,n)*t3b(f,c,a,n,j,i)      !dblkfnfcanji    (-1.000)
     &     + u53(d,c,l,k,f,n)*t3b(f,b,a,n,j,i)      !dclkfnfbanji    (+1.000)
     &     - u53(d,a,l,j,f,n)*t3b(f,c,b,n,k,i)      !daljfnfcbnki    (-1.000)
     &     + u53(d,b,l,j,f,n)*t3b(f,c,a,n,k,i)      !dbljfnfcanki    (+1.000)
     &     - u53(d,c,l,j,f,n)*t3b(f,b,a,n,k,i)      !dcljfnfbanki    (-1.000)
     &     + u53(d,c,l,i,f,n)*t3b(f,b,a,n,k,j)      !dclifnfbankj    (+1.000)
     &     - u53(d,b,l,i,f,n)*t3b(f,c,a,n,k,j)      !dblifnfcankj    (-1.000)
     &     + u53(d,a,l,i,f,n)*t3b(f,c,b,n,k,j)      !dalifnfcbnkj    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23781456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178, 1.000)
!       call sum24781356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178,-1.000)
!       call sum34781256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178, 1.000)
!       call sum23681457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178,-1.000)
!       call sum24681357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178, 1.000)
!       call sum34681257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178,-1.000)
!       call sum34671258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178, 1.000)
!       call sum24671358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178,-1.000)
!       call sum23671458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z178, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z178(c,b,j,i,d,a,l,k)       ! 23781456 (+1.000)
!     & -z178(c,a,j,i,d,b,l,k)       ! 24781356 (-1.000)
!     & +z178(b,a,j,i,d,c,l,k)       ! 34781256 (+1.000)
!     & -z178(c,b,k,i,d,a,l,j)       ! 23681457 (-1.000)
!     & +z178(c,a,k,i,d,b,l,j)       ! 24681357 (+1.000)
!     & -z178(b,a,k,i,d,c,l,j)       ! 34681257 (-1.000)
!     & +z178(b,a,k,j,d,c,l,i)       ! 34671258 (+1.000)
!     & -z178(c,a,k,j,d,b,l,i)       ! 24671358 (-1.000)
!     & +z178(c,b,k,j,d,a,l,i)       ! 23671458 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z178)
       deallocate(u53)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n2,n0,n1,intm,d1)
       allocate(f2(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder124356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n1,n3,n0,n2,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(s86(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1
       i2=k1*k1*k3
       i3=k2*k3*k4
       call egemm(i1,i2,i3,d1,f2,s86)
       deallocate(d1)
       deallocate(f2)
c
       call sum2341(n0,n1,n1,n3,n0,n1,n0,n1,x3,s86, 1.000)
       deallocate(s86)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3))
       call reorder4123(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n0,n2,n2,n3,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder154236(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n0,n2,n2,n3,n1,n3,n0,n1,t3c,f2)
       allocate(s87(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2*k2*k4
       call egemm(i1,i2,i3,d1,f2,s87)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder4123(n2,n3,n1,n3,n0,n1,n2,n3,
!     & n2,n3,n2,n3,n1,n3,n0,n1,s87,d1)
!       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z180(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k4
!       i2=k1*k1*k2*k3*k3
!       i3=k4
!       call egemm(i1,i2,i3,d1,f2,z180)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + (s87(d,b,k,e)*t3b(e,c,a,l,j,i)         !dbkeecalji      (+0.500)
     &     - s87(d,a,k,e)*t3b(e,c,b,l,j,i)          !dakeecblji      (-0.500)
     &     - s87(d,c,k,e)*t3b(e,b,a,l,j,i)          !dckeebalji      (-0.500)
     &     + s87(d,a,j,e)*t3b(e,c,b,l,k,i)          !dajeecblki      (+0.500)
     &     - s87(d,b,j,e)*t3b(e,c,a,l,k,i)          !dbjeecalki      (-0.500)
     &     + s87(d,c,j,e)*t3b(e,b,a,l,k,i)          !dcjeebalki      (+0.500)
     &     - s87(d,a,i,e)*t3b(e,c,b,l,k,j)          !daieecblkj      (-0.500)
     &     + s87(d,b,i,e)*t3b(e,c,a,l,k,j)          !dbieecalkj      (+0.500)
     &     - s87(d,c,i,e)*t3b(e,b,a,l,k,j))/2.0d0   !dcieebalkj      (-0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23578146(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180,-0.500)
!       call sum24578136(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180, 0.500)
!       call sum34578126(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180,-0.500)
!       call sum23568147(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180, 0.500)
!       call sum24568137(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180,-0.500)
!       call sum34568127(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180, 0.500)
!       call sum23567148(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180,-0.500)
!       call sum24567138(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180, 0.500)
!       call sum34567128(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z180,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z180(c,a,l,j,i,d,b,k)      ! 24578136 (+0.500) top two switched
!     & -z180(c,b,l,j,i,d,a,k)       ! 23578146 (-0.500)
!     & -z180(b,a,l,j,i,d,c,k)       ! 34578126 (-0.500)
!     & +z180(c,b,l,k,i,d,a,j)       ! 23568147 (+0.500)
!     & -z180(c,a,l,k,i,d,b,j)       ! 24568137 (-0.500)
!     & +z180(b,a,l,k,i,d,c,j)       ! 34568127 (+0.500)
!     & -z180(c,b,l,k,j,d,a,i)       ! 23567148 (-0.500)
!     & +z180(c,a,l,k,j,d,b,i)       ! 24567138 (+0.500)
!     & -z180(b,a,l,k,j,d,c,i))/2.0d0! 34567128 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z180)
       deallocate(s87)
c
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder4321(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder124356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n2,n3,n0,n2,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(s88(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       i1=k2
       i2=k1*k2*k3
       i3=k2*k4*k4
       call egemm(i1,i2,i3,d1,f2,s88)
       deallocate(d1)
       deallocate(f2)
c
!       allocate(d1(n0+1:n2,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder4123(n1,n3,n0,n2,n0,n1,n0,n2,
!     & n0,n2,n1,n3,n0,n2,n0,n1,s88,d1)
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z181(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k1*k3*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,d1,f2,z181)
!       deallocate(d1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     + (s88(b,l,k,m)*t3b(d,c,a,m,j,i)         !blkmdcamji      (+0.500)
     &     - s88(a,l,k,m)*t3b(d,c,b,m,j,i)          !alkmdcbmji      (-0.500)
     &     - s88(c,l,k,m)*t3b(d,b,a,m,j,i)          !clkmdbamji      (-0.500)
     &     + s88(a,l,j,m)*t3b(d,c,b,m,k,i)          !aljmdcbmki      (+0.500)
     &     - s88(b,l,j,m)*t3b(d,c,a,m,k,i)          !bljmdcamki      (-0.500)
     &     + s88(c,l,j,m)*t3b(d,b,a,m,k,i)          !cljmdbamki      (+0.500)
     &     - s88(a,l,i,m)*t3b(d,c,b,m,k,j)          !alimdcbmkj      (-0.500)
     &     + s88(b,l,i,m)*t3b(d,c,a,m,k,j)          !blimdcamkj      (+0.500)
     &     - s88(c,l,i,m)*t3b(d,b,a,m,k,j))/2.0d0   !climdbamkj      (-0.500)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12378456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181,-0.500)
!       call sum12478356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181, 0.500)
!       call sum13478256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181,-0.500)
!       call sum12368457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181, 0.500)
!       call sum12468357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181,-0.500)
!       call sum13468257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181, 0.500)
!       call sum12367458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181,-0.500)
!       call sum12467358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181, 0.500)
!       call sum13467258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z181,-0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z181(d,c,a,j,i,b,l,k)      ! 12478356 (+0.500) top two switched
!     & -z181(d,c,b,j,i,a,l,k)       ! 12378456 (-0.500)
!     & -z181(d,b,a,j,i,c,l,k)       ! 13478256 (-0.500)
!     & +z181(d,c,b,k,i,a,l,j)       ! 12368457 (+0.500)
!     & -z181(d,c,a,k,i,b,l,j)       ! 12468357 (-0.500)
!     & +z181(d,b,a,k,i,c,l,j)       ! 13468257 (+0.500)
!     & -z181(d,c,b,k,j,a,l,i)       ! 12367458 (-0.500)
!     & +z181(d,c,a,k,j,b,l,i)       ! 12467358 (+0.500)
!     & -z181(d,b,a,k,j,c,l,i))/2.0d0! 13467258 (-0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z181)
       deallocate(s88)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u54(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,n0+1:n2))
       i1=k2*k4
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u54)
       deallocate(d1)
       deallocate(f2)
c
       call sum345621(n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,x53,u54,1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder412356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
!       allocate(z172(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3*k3
!       i2=k1*k2*k3*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x53,f2,z172)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do f=n2+1,n3;do n=n0+1,n2
             sum=sum
     &     + x53(n,f,c,b,j,i)*t3c(f,d,a,n,l,k)      !nfcbjifdanlk    (+1.000)
     &     - x53(n,f,c,a,j,i)*t3c(f,d,b,n,l,k)      !nfcajifdbnlk    (-1.000)
     &     + x53(n,f,b,a,j,i)*t3c(f,d,c,n,l,k)      !nfbajifdcnlk    (+1.000)
     &     - x53(n,f,c,b,k,i)*t3c(f,d,a,n,l,j)      !nfcbkifdanlj    (-1.000)
     &     + x53(n,f,c,a,k,i)*t3c(f,d,b,n,l,j)      !nfcakifdbnlj    (+1.000)
     &     - x53(n,f,b,a,k,i)*t3c(f,d,c,n,l,j)      !nfbakifdcnlj    (-1.000)
     &     + x53(n,f,c,b,k,j)*t3c(f,d,a,n,l,i)      !nfcbkjfdanli    (+1.000)
     &     - x53(n,f,c,a,k,j)*t3c(f,d,b,n,l,i)      !nfcakjfdbnli    (-1.000)
     &     + x53(n,f,b,a,k,j)*t3c(f,d,c,n,l,i)      !nfbakjfdcnli    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14562378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172, 1.000)
!       call sum13562478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172,-1.000)
!       call sum12563478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172, 1.000)
!       call sum14572368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172,-1.000)
!       call sum13572468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172, 1.000)
!       call sum12573468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172,-1.000)
!       call sum14582367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172, 1.000)
!       call sum13582467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172,-1.000)
!       call sum12583467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z172, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z172(d,a,l,k,c,b,j,i)       ! 14562378 (+1.000)
!     & -z172(d,b,l,k,c,a,j,i)       ! 13562478 (-1.000)
!     & +z172(d,c,l,k,b,a,j,i)       ! 12563478 (+1.000)
!     & -z172(d,a,l,j,c,b,k,i)       ! 14572368 (-1.000)
!     & +z172(d,b,l,j,c,a,k,i)       ! 13572468 (+1.000)
!     & -z172(d,c,l,j,b,a,k,i)       ! 12573468 (-1.000)
!     & +z172(d,a,l,i,c,b,k,j)       ! 14582367 (+1.000)
!     & -z172(d,b,l,i,c,a,k,j)       ! 13582467 (-1.000)
!     & +z172(d,c,l,i,b,a,k,j)       ! 12583467 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z172)
       deallocate(x53)
c
       allocate(f1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder561234(n1,n3,n1,n3,n0,n1,n0,n1,n2,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,u54,f1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(u95(n0+1:n2,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,f1,b2,u95)
       deallocate(f1)
       deallocate(b2)
       deallocate(u54)
c
       call sum412356(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x44,u95,1.000)
       deallocate(u95)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z113(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x44,d2,z113)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x44(n,b,a,l,k,j)*t2b(d,c,n,i)          !nbalkjdcni      (-1.000)
     &     + x44(n,c,a,l,k,j)*t2b(d,b,n,i)          !ncalkjdbni      (+1.000)
     &     - x44(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (-1.000)
     &     + x44(n,b,a,l,k,i)*t2b(d,c,n,j)          !nbalkidcnj      (+1.000)
     &     - x44(n,c,a,l,k,i)*t2b(d,b,n,j)          !ncalkidbnj      (-1.000)
     &     + x44(n,c,b,l,k,i)*t2b(d,a,n,j)          !ncblkidanj      (+1.000)
     &     - x44(n,b,a,l,j,i)*t2b(d,c,n,k)          !nbaljidcnk      (-1.000)
     &     + x44(n,c,a,l,j,i)*t2b(d,b,n,k)          !ncaljidbnk      (+1.000)
     &     - x44(n,c,b,l,j,i)*t2b(d,a,n,k)          !ncbljidank      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12834567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113,-1.000)
!       call sum13824567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113, 1.000)
!       call sum14823567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113,-1.000)
!       call sum12734568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113, 1.000)
!       call sum13724568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113,-1.000)
!       call sum14723568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113, 1.000)
!       call sum12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113,-1.000)
!       call sum13624578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113, 1.000)
!       call sum14623578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z113,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z113(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z113(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z113(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & +z113(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & -z113(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z113(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & -z113(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z113(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z113(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z113)
       deallocate(x44)
c
       allocate(d1(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n1,n3,n0,n1,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s95(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s95)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n0,n1,s95,d1)
       allocate(f2(n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder251346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
       allocate(u66(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k2*k3*k4
       i3=k1*k3
       call egemm(i1,i2,i3,d1,f2,u66)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u66,1.000)
       deallocate(u66)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n0,n1,n0,n1,s95,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q21(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q21)
       deallocate(d1)
       deallocate(b2)
c
       call sum21(n0,n1,n0,n1,x8,q21,-1.000)
       deallocate(q21)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder3214(n0,n1,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n0,n1,n0,n1,s95,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s97(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       i1=k1*k1*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s97)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n1,n1,n3,n1,n3,n0,n1,x13,s97,-1.000)
c
       call sumx3142(n0,n3,n0,n1,n1,n3,n1,n3,n0,n1,x13,intr, 1.000)
c
!       allocate(h2(n0+1:n1,n1+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder62134578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4b,h2)
!       allocate(z13(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k1*k1*k2*k3*k3*k4
!       i3=k3*k1
!       call egemm(i1,i2,i3,x13,h2,z13)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x13(m,e,c,i)*t4b(d,e,b,a,l,m,k,j)      !mecidebalmkj    (+1.000)
     &     - x13(m,e,b,i)*t4b(d,e,c,a,l,m,k,j)      !mebidecalmkj    (-1.000)
     &     + x13(m,e,a,i)*t4b(d,e,c,b,l,m,k,j)      !meaidecblmkj    (+1.000)
     &     - x13(m,e,c,j)*t4b(d,e,b,a,l,m,k,i)      !mecjdebalmki    (-1.000)
     &     + x13(m,e,b,j)*t4b(d,e,c,a,l,m,k,i)      !mebjdecalmki    (+1.000)
     &     - x13(m,e,a,j)*t4b(d,e,c,b,l,m,k,i)      !meajdecblmki    (-1.000)
     &     + x13(m,e,c,k)*t4b(d,e,b,a,l,m,j,i)      !meckdebalmji    (+1.000)
     &     - x13(m,e,b,k)*t4b(d,e,c,a,l,m,j,i)      !mebkdecalmji    (-1.000)
     &     + x13(m,e,a,k)*t4b(d,e,c,b,l,m,j,i)      !meakdecblmji    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13456728(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13, 1.000)
!       call sum12456738(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13,-1.000)
!       call sum12356748(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13, 1.000)
!       call sum13456827(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13,-1.000)
!       call sum12456837(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13, 1.000)
!       call sum12356847(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13,-1.000)
!       call sum13457826(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13, 1.000)
!       call sum12457836(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13,-1.000)
!       call sum12357846(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z13, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z13(d,b,a,l,k,j,c,i)       ! 13456728 (+1.000)
!     & -z13(d,c,a,l,k,j,b,i)       ! 12456738 (-1.000)
!     & +z13(d,c,b,l,k,j,a,i)       ! 12356748 (+1.000)
!     & -z13(d,b,a,l,k,i,c,j)       ! 13456827 (-1.000)
!     & +z13(d,c,a,l,k,i,b,j)       ! 12456837 (+1.000)
!     & -z13(d,c,b,l,k,i,a,j)       ! 12356847 (-1.000)
!     & +z13(d,b,a,l,j,i,c,k)       ! 13457826 (+1.000)
!     & -z13(d,c,a,l,j,i,b,k)       ! 12457836 (-1.000)
!     & +z13(d,c,b,l,j,i,a,k)       ! 12357846 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z13)
       deallocate(x13)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,s97,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s156(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s156)
       deallocate(d1)
       deallocate(b2)
       deallocate(s97)
c
       call sum2134(n1,n3,n1,n3,n1,n3,n0,n1,x4,s156, 1.000)
       deallocate(s156)
c
       allocate(d1(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2314(n0,n1,n1,n3,n0,n1,n0,n1,
     & n1,n3,n0,n1,n0,n1,n0,n1,s95,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s96(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s96)
       deallocate(d1)
       deallocate(b2)
       deallocate(s95)
c
       call sum3241(n0,n1,n0,n1,n0,n1,n0,n1,x12,s96, 1.000)
c
       call sumx2143(n0,n3,n0,n1,n0,n1,n0,n1,n0,n1,x12,intr, 1.000)
c
!       allocate(h2(n0+1:n1,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       call reorder67123458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,t4b,h2)
!       allocate(z12(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1
!       i2=k1*k2*k3*k3*k3*k4
!       i3=k1*k1
!       call egemm(i1,i2,i3,x12,h2,z12)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n1
             sum=sum
     &     + (x12(n,m,j,i)*t4b(d,c,b,a,l,n,m,k)       !nmjidcbalnmk    (+0.500)
     &     - x12(n,m,k,i)*t4b(d,c,b,a,l,n,m,j)        !nmkidcbalnmj    (-0.500)
     &     + x12(n,m,k,j)*t4b(d,c,b,a,l,n,m,i))/2.0d0 !nmkjdcbalnmi    (+0.500)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4b=v4b+0.500*z12
!       call sum12345768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z12,-0.500)
!       call sum12345867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z12, 0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z12(d,c,b,a,l,k,j,i)      ! 12345678 (+0.500)
!     & -z12(d,c,b,a,l,j,k,i)       ! 12345768 (-0.500)
!     & +z12(d,c,b,a,l,i,k,j))/2.0d0! 12345867 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z12)
       deallocate(x12)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s96,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u112(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u112)
       deallocate(d1)
       deallocate(d2)
c
      call sum234156(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x31,u112,1.000)
       deallocate(u112)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z68(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x31,d2,z68)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n1
             sum=sum
     &     + x31(n,c,b,k,j,i)*t2b(d,a,l,n)          !ncbkjidaln      (+1.000)
     &     - x31(n,c,a,k,j,i)*t2b(d,b,l,n)          !ncakjidbln      (-1.000)
     &     + x31(n,b,a,k,j,i)*t2b(d,c,l,n)          !nbakjidcln      (+1.000)
     &     - x31(n,c,b,j,k,i)*t2b(d,a,l,n)          !ncbjkidaln      (-1.000)
     &     + x31(n,c,a,j,k,i)*t2b(d,b,l,n)          !ncajkidbln      (+1.000)
     &     - x31(n,b,a,j,k,i)*t2b(d,c,l,n)          !nbajkidcln      (-1.000)
     &     + x31(n,c,b,i,k,j)*t2b(d,a,l,n)          !ncbikjdaln      (+1.000)
     &     - x31(n,c,a,i,k,j)*t2b(d,b,l,n)          !ncaikjdbln      (-1.000)
     &     + x31(n,b,a,i,k,j)*t2b(d,c,l,n)          !nbaikjdcln      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14523678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68, 1.000)
!       call sum13524678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68,-1.000)
!       call sum12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68, 1.000)
!       call sum14523768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68,-1.000)
!       call sum13524768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68, 1.000)
!       call sum12534768(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68,-1.000)
!       call sum14523867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68, 1.000)
!       call sum13524867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68,-1.000)
!       call sum12534867(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z68, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z68(d,a,l,c,b,k,j,i)       ! 14523678 (+1.000)
!     & -z68(d,b,l,c,a,k,j,i)       ! 13524678 (-1.000)
!     & +z68(d,c,l,b,a,k,j,i)       ! 12534678 (+1.000)
!     & -z68(d,a,l,c,b,j,k,i)       ! 14523768 (-1.000)
!     & +z68(d,b,l,c,a,j,k,i)       ! 13524768 (+1.000)
!     & -z68(d,c,l,b,a,j,k,i)       ! 12534768 (-1.000)
!     & +z68(d,a,l,c,b,i,k,j)       ! 14523867 (+1.000)
!     & -z68(d,b,l,c,a,i,k,j)       ! 13524867 (-1.000)
!     & +z68(d,c,l,b,a,i,k,j)       ! 12534867 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z68)
       deallocate(x31)
c
       allocate(d1(n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       call reorder2413(n0,n1,n0,n1,n0,n1,n0,n1,
     & n0,n1,n0,n1,n0,n1,n0,n1,s96,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s155(n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1*k1
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s155)
       deallocate(d1)
       deallocate(b2)
       deallocate(s96)
c
       call sum2134(n0,n1,n1,n3,n0,n1,n0,n1,x3,s155,-1.000)
       deallocate(s155)
c
       call sumx2143(n0,n3,n0,n1,n1,n3,n0,n1,n0,n1,x3,intr, 1.000)
c
!       allocate(f2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
!       call reorder512346(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,t3b,f2)
!       allocate(z3(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k3
!       i2=k1*k2*k3*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x3,f2,z3)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x3(m,c,j,i)*t3b(d,b,a,l,m,k)           !mcjidbalmk      (+1.000)
     &     - x3(m,b,j,i)*t3b(d,c,a,l,m,k)           !mbjidcalmk      (-1.000)
     &     + x3(m,a,j,i)*t3b(d,c,b,l,m,k)           !majidcblmk      (+1.000)
     &     - x3(m,c,k,i)*t3b(d,b,a,l,m,j)           !mckidbalmj      (-1.000)
     &     + x3(m,b,k,i)*t3b(d,c,a,l,m,j)           !mbkidcalmj      (+1.000)
     &     - x3(m,a,k,i)*t3b(d,c,b,l,m,j)           !makidcblmj      (-1.000)
     &     + x3(m,c,k,j)*t3b(d,b,a,l,m,i)           !mckjdbalmi      (+1.000)
     &     - x3(m,b,k,j)*t3b(d,c,a,l,m,i)           !mbkjdcalmi      (-1.000)
     &     + x3(m,a,k,j)*t3b(d,c,b,l,m,i)           !makjdcblmi      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13456278(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3, 1.000)
!       call sum12456378(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3,-1.000)
!       call sum12356478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3, 1.000)
!       call sum13457268(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3,-1.000)
!       call sum12457368(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3, 1.000)
!       call sum12357468(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3,-1.000)
!       call sum13458267(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3, 1.000)
!       call sum12458367(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3,-1.000)
!       call sum12358467(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z3, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z3(d,b,a,l,k,c,j,i)       ! 13456278 (+1.000)
!     & -z3(d,c,a,l,k,b,j,i)       ! 12456378 (-1.000)
!     & +z3(d,c,b,l,k,a,j,i)       ! 12356478 (+1.000)
!     & -z3(d,b,a,l,j,c,k,i)       ! 13457268 (-1.000)
!     & +z3(d,c,a,l,j,b,k,i)       ! 12457368 (+1.000)
!     & -z3(d,c,b,l,j,a,k,i)       ! 12357468 (-1.000)
!     & +z3(d,b,a,l,i,c,k,j)       ! 13458267 (+1.000)
!     & -z3(d,c,a,l,i,b,k,j)       ! 12458367 (-1.000)
!     & +z3(d,c,b,l,i,a,k,j)       ! 12358467 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z3)
       deallocate(x3)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n1,n3,n1,n3,n0,n1,intr,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s98(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s98)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3))
       call reorder3421(n1,n3,n1,n3,n1,n3,n0,n1,
     & n1,n3,n0,n1,n1,n3,n1,n3,s98,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q22(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q22)
       deallocate(d1)
       deallocate(b2)
c
       x9=x9+q22
       deallocate(q22)
c
       allocate(d1(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3))
       call reorder4231(n1,n3,n1,n3,n1,n3,n0,n1,
     & n0,n1,n1,n3,n1,n3,n1,n3,s98,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s99(n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s99)
       deallocate(d1)
       deallocate(b2)
       deallocate(s98)
c
       call sum3124(n1,n3,n1,n3,n1,n3,n1,n3,x14,s99, 1.000)
       deallocate(s99)
c
       call sumx4321(n0,n3,n1,n3,n1,n3,n1,n3,n1,n3,x14,intr, 1.000)
c
!       allocate(h2(n1+1:n3,n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder23145678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z14(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,
!     & n1+1:n3,n1+1:n3))
!       i1=k3*k3
!       i2=k1*k1*k1*k2*k3*k4
!       i3=k3*k3
!       call egemm(i1,i2,i3,x14,h2,z14)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n1+1,n3
             sum=sum
     &     + (x14(f,e,c,b)*t4b(d,f,e,a,l,k,j,i)       !fecbdfealkji    (+0.500)
     &     - x14(f,e,c,a)*t4b(d,f,e,b,l,k,j,i)        !fecadfeblkji    (-0.500)
     &     + x14(f,e,b,a)*t4b(d,f,e,c,l,k,j,i))/2.0d0 !febadfeclkji    (+0.500)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14567823(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z14, 0.500)
!       call sum13567824(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z14,-0.500)
!       call sum12567834(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z14, 0.500)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +(z14(d,a,l,k,j,i,c,b)      ! 14567823 (+0.500)
!     & -z14(d,b,l,k,j,i,c,a)       ! 13567824 (-0.500)
!     & +z14(d,c,l,k,j,i,b,a))/2.0d0! 12567834 (+0.500)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z14)
       deallocate(x14)
c
       allocate(d1(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n2,n3,n0,n1,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(s100(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k1
       i3=k3
       call egemm(i1,i2,i3,d1,b2,s100)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s100,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder142356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n2,n3,n0,n2,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u79(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k1*k3*k3
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u79)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,x41,u79,1.000)
       deallocate(u79)
c
!       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
!       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
!       allocate(z108(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k1*k3*k3
!       i2=k2*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x41,d2,z108)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x41(m,b,a,k,j,i)*t2b(d,c,l,m)          !mbakjidclm      (-1.000)
     &     + x41(m,c,a,k,j,i)*t2b(d,b,l,m)          !mcakjidblm      (+1.000)
     &     - x41(m,c,b,k,j,i)*t2b(d,a,l,m)          !mcbkjidalm      (-1.000)
     &     + x41(m,b,a,k,i,j)*t2b(d,c,l,m)          !mbakijdclm      (+1.000)
     &     - x41(m,c,a,k,i,j)*t2b(d,b,l,m)          !mcakijdblm      (-1.000)
     &     + x41(m,c,b,k,i,j)*t2b(d,a,l,m)          !mcbkijdalm      (+1.000)
     &     - x41(m,b,a,j,i,k)*t2b(d,c,l,m)          !mbajikdclm      (-1.000)
     &     + x41(m,c,a,j,i,k)*t2b(d,b,l,m)          !mcajikdblm      (+1.000)
     &     - x41(m,c,b,j,i,k)*t2b(d,a,l,m)          !mcbjikdalm      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12534678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108,-1.000)
!       call sum13524678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108, 1.000)
!       call sum14523678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108,-1.000)
!       call sum12534687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108, 1.000)
!       call sum13524687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108,-1.000)
!       call sum14523687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108, 1.000)
!       call sum12534786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108,-1.000)
!       call sum13524786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108, 1.000)
!       call sum14523786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z108,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z108(d,c,l,b,a,k,j,i)       ! 12534678 (-1.000)
!     & +z108(d,b,l,c,a,k,j,i)       ! 13524678 (+1.000)
!     & -z108(d,a,l,c,b,k,j,i)       ! 14523678 (-1.000)
!     & +z108(d,c,l,b,a,k,i,j)       ! 12534687 (+1.000)
!     & -z108(d,b,l,c,a,k,i,j)       ! 13524687 (-1.000)
!     & +z108(d,a,l,c,b,k,i,j)       ! 14523687 (+1.000)
!     & -z108(d,c,l,b,a,j,i,k)       ! 12534786 (-1.000)
!     & +z108(d,b,l,c,a,j,i,k)       ! 13524786 (+1.000)
!     & -z108(d,a,l,c,b,j,i,k)       ! 14523786 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z108)
       deallocate(x41)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s100,d1)
       allocate(f2(n2+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1))
       call reorder142356(n2,n3,n2,n3,n1,n3,n0,n2,n0,n2,n0,n1,
     & n2,n3,n0,n2,n2,n3,n1,n3,n0,n2,n0,n1,t3c,f2)
       allocate(u69(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1))
       i1=k1*k1
       i2=k1*k2*k3*k4
       i3=k2*k4
       call egemm(i1,i2,i3,d1,f2,u69)
       deallocate(d1)
       deallocate(f2)
c
       call sum234516(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x26,u69,1.000)
       deallocate(u69)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z218(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x26,d2,z218)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     + x26(m,d,a,l,k,i)*t2a(c,b,m,j)          !mdalkicbmj      (+1.000)
     &     - x26(m,d,b,l,k,i)*t2a(c,a,m,j)          !mdblkicamj      (-1.000)
     &     + x26(m,d,c,l,k,i)*t2a(b,a,m,j)          !mdclkibamj      (+1.000)
     &     - x26(m,d,a,l,j,i)*t2a(c,b,m,k)          !mdaljicbmk      (-1.000)
     &     + x26(m,d,b,l,j,i)*t2a(c,a,m,k)          !mdbljicamk      (+1.000)
     &     - x26(m,d,c,l,j,i)*t2a(b,a,m,k)          !mdcljibamk      (-1.000)
     &     - x26(m,d,a,l,k,j)*t2a(c,b,m,i)          !mdalkjcbmi      (-1.000)
     &     + x26(m,d,b,l,k,j)*t2a(c,a,m,i)          !mdblkjcami      (+1.000)
     &     - x26(m,d,c,l,k,j)*t2a(b,a,m,i)          !mdclkjbami      (-1.000)
     &     + x26(m,d,a,l,j,k)*t2a(c,b,m,i)          !mdaljkcbmi      (+1.000)
     &     - x26(m,d,b,l,j,k)*t2a(c,a,m,i)          !mdbljkcami      (-1.000)
     &     + x26(m,d,c,l,j,k)*t2a(b,a,m,i)          !mdcljkbami      (+1.000)
     &     + x26(m,d,a,l,i,j)*t2a(c,b,m,k)          !mdalijcbmk      (+1.000)
     &     - x26(m,d,b,l,i,j)*t2a(c,a,m,k)          !mdblijcamk      (-1.000)
     &     + x26(m,d,c,l,i,j)*t2a(b,a,m,k)          !mdclijbamk      (+1.000)
     &     - x26(m,d,a,l,i,k)*t2a(c,b,m,j)          !mdalikcbmj      (-1.000)
     &     + x26(m,d,b,l,i,k)*t2a(c,a,m,j)          !mdblikcamj      (+1.000)
     &     - x26(m,d,c,l,i,k)*t2a(b,a,m,j)          !mdclikbamj      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23714568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum24713568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
!       call sum34712568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
!       call sum24613578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum34612578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
!       call sum23814567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
!       call sum24813567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum34812567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
!       call sum23814576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum24813576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
!       call sum34812576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum23614587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum24613587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
!       call sum34612587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum23714586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
!       call sum24713586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218, 1.000)
!       call sum34712586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z218,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z218(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z218(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z218(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z218(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z218(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z218(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!     & -z218(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z218(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z218(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z218(c,b,i,d,a,l,j,k)       ! 23814576 (+1.000)
!     & -z218(c,a,i,d,b,l,j,k)       ! 24813576 (-1.000)
!     & +z218(b,a,i,d,c,l,j,k)       ! 34812576 (+1.000)
!     & +z218(c,b,k,d,a,l,i,j)       ! 23614587 (+1.000)
!     & -z218(c,a,k,d,b,l,i,j)       ! 24613587 (-1.000)
!     & +z218(b,a,k,d,c,l,i,j)       ! 34612587 (+1.000)
!     & -z218(c,b,j,d,a,l,i,k)       ! 23714586 (-1.000)
!     & +z218(c,a,j,d,b,l,i,k)       ! 24713586 (+1.000)
!     & -z218(b,a,j,d,c,l,i,k)       ! 34712586 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z218)
       deallocate(x26)
c
       allocate(d1(n2+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
       call reorder2431(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n0,n1,n0,n1,s100,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q23(n0+1:n1,n0+1:n1))
       i1=k1*k1
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q23)
       deallocate(d1)
       deallocate(b2)
c
       x8=x8+q23
       deallocate(q23)
c
       call sumx12(0,n3,n0,n1,n0,n1,x8,fockr, 1.000)
c
!       allocate(h2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder61234578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4b,h2)
!       allocate(z8(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       i1=k1
!       i2=k1*k1*k2*k3*k3*k3*k4
!       i3=k1
!       call egemm(i1,i2,i3,x8,h2,z8)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x8(m,i)*t4b(d,c,b,a,l,m,k,j)           !midcbalmkj      (-1.000)
     &     + x8(m,j)*t4b(d,c,b,a,l,m,k,i)           !mjdcbalmki      (+1.000)
     &     - x8(m,k)*t4b(d,c,b,a,l,m,j,i)           !mkdcbalmji      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       v4b=v4b-z8
!       call sum12345687(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z8, 1.000)
!       call sum12345786(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z8,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z8(d,c,b,a,l,k,j,i)       ! 12345678 (+1.000)
!     & +z8(d,c,b,a,l,k,i,j)       ! 12345687 (+1.000)
!     & -z8(d,c,b,a,l,j,i,k)       ! 12345786 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z8)
       deallocate(x8)
c
       allocate(d1(n0+1:n2,n0+1:n1,n2+1:n3,n0+1:n1))
       call reorder4321(n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n2,n3,n0,n1,s100,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s111(n2+1:n3,n0+1:n1,n2+1:n3,n0+1:n1))
       i1=k1*k4*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s111)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n2,n3,n2,n3,n0,n1,x16,s111,-1.000)
       deallocate(s111)
c
       call sumx1342(n0,n3,n0,n1,n2,n3,n2,n3,n0,n1,x16,intm, 1.000)
c
!       allocate(h2(n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder61234578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4b,h2)
!       allocate(z16(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n0+1:n1))
!       i1=k1*k4
!       i2=k1*k1*k2*k3*k3*k3
!       i3=k4*k1
!       call egemm(i1,i2,i3,x16,h2,z16)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n1
             sum=sum
     &     - x16(m,e,d,i)*t4b(e,c,b,a,l,m,k,j)      !mediecbalmkj    (-1.000)
     &     + x16(m,e,d,j)*t4b(e,c,b,a,l,m,k,i)      !medjecbalmki    (+1.000)
     &     - x16(m,e,d,k)*t4b(e,c,b,a,l,m,j,i)      !medkecbalmji    (-1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456718(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z16,-1.000)
!       call sum23456817(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z16, 1.000)
!       call sum23457816(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z16,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z16(c,b,a,l,k,j,d,i)       ! 23456718 (-1.000)
!     & +z16(c,b,a,l,k,i,d,j)       ! 23456817 (+1.000)
!     & -z16(c,b,a,l,j,i,d,k)       ! 23457816 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z16)
       deallocate(x16)
c
       allocate(d1(n2+1:n3,n0+1:n1,n0+1:n1,n0+1:n2))
       call reorder2314(n0,n1,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n1,n0,n1,n0,n2,s100,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s110(n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1*k1
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s110)
       deallocate(d1)
       deallocate(b2)
c
       call sum3241(n0,n2,n0,n1,n0,n2,n0,n1,x15,s110, 1.000)
c
       call sumx2143(n0,n3,n0,n2,n0,n1,n0,n2,n0,n1,x15,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n0+1:n1,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,
!     & n0+1:n1,n0+1:n1))
!       call reorder56123478(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n2,n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t4b,h2)
!       allocate(z15(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2
!       i2=k1*k1*k3*k3*k3*k4
!       i3=k1*k2
!       call egemm(i1,i2,i3,x15,h2,z15)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1;do n=n0+1,n2
             sum=sum
     &     + x15(n,m,l,i)*t4b(d,c,b,a,n,m,k,j)      !nmlidcbanmkj    (+1.000)
     &     - x15(n,m,l,j)*t4b(d,c,b,a,n,m,k,i)      !nmljdcbanmki    (-1.000)
     &     + x15(n,m,l,k)*t4b(d,c,b,a,n,m,j,i)      !nmlkdcbanmji    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12346758(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z15, 1.000)
!       call sum12346857(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z15,-1.000)
!       call sum12347856(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z15, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z15(d,c,b,a,k,j,l,i)       ! 12346758 (+1.000)
!     & -z15(d,c,b,a,k,i,l,j)       ! 12346857 (-1.000)
!     & +z15(d,c,b,a,j,i,l,k)       ! 12347856 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z15)
       deallocate(x15)
c
       allocate(d1(n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       call reorder2413(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n1,n0,n2,n0,n2,n0,n1,s110,d1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(u117(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n2,n0+1:n2,n0+1:n1))
       i1=k1*k2*k2
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,d1,d2,u117)
       deallocate(d1)
       deallocate(d2)
c
      call sum235146(n0,n2,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,x25,u117,1.000)
       deallocate(u117)
c
!       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
!       allocate(z217(n2+1:n3,n1+1:n3,n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k3
!       i2=k1*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x25,d2,z217)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do n=n0+1,n2
             sum=sum
     &     - x25(n,c,b,l,j,i)*t2b(d,a,n,k)          !ncbljidank      (-1.000)
     &     + x25(n,c,a,l,j,i)*t2b(d,b,n,k)          !ncaljidbnk      (+1.000)
     &     - x25(n,b,a,l,j,i)*t2b(d,c,n,k)          !nbaljidcnk      (-1.000)
     &     + x25(n,c,b,l,k,i)*t2b(d,a,n,j)          !ncblkidanj      (+1.000)
     &     - x25(n,c,a,l,k,i)*t2b(d,b,n,j)          !ncalkidbnj      (-1.000)
     &     + x25(n,b,a,l,k,i)*t2b(d,c,n,j)          !nbalkidcnj      (+1.000)
     &     + x25(n,c,b,l,i,j)*t2b(d,a,n,k)          !ncblijdank      (+1.000)
     &     - x25(n,c,a,l,i,j)*t2b(d,b,n,k)          !ncalijdbnk      (-1.000)
     &     + x25(n,b,a,l,i,j)*t2b(d,c,n,k)          !nbalijdcnk      (+1.000)
     &     - x25(n,c,b,l,i,k)*t2b(d,a,n,j)          !ncblikdanj      (-1.000)
     &     + x25(n,c,a,l,i,k)*t2b(d,b,n,j)          !ncalikdbnj      (+1.000)
     &     - x25(n,b,a,l,i,k)*t2b(d,c,n,j)          !nbalikdcnj      (-1.000)
     &     - x25(n,c,b,l,k,j)*t2b(d,a,n,i)          !ncblkjdani      (-1.000)
     &     + x25(n,c,a,l,k,j)*t2b(d,b,n,i)          !ncalkjdbni      (+1.000)
     &     - x25(n,b,a,l,k,j)*t2b(d,c,n,i)          !nbalkjdcni      (-1.000)
     &     + x25(n,c,b,l,j,k)*t2b(d,a,n,i)          !ncbljkdani      (+1.000)
     &     - x25(n,c,a,l,j,k)*t2b(d,b,n,i)          !ncaljkdbni      (-1.000)
     &     + x25(n,b,a,l,j,k)*t2b(d,c,n,i)          !nbaljkdcni      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14623578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum13624578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
!       call sum12634578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum14723568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
!       call sum13724568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum12734568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
!       call sum14623587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
!       call sum13624587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum12634587(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
!       call sum14723586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum13724586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
!       call sum12734586(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum14823567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum13824567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
!       call sum12834567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum14823576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
!       call sum13824576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217,-1.000)
!       call sum12834576(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z217, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z217(d,a,k,c,b,l,j,i)       ! 14623578 (-1.000)
!     & +z217(d,b,k,c,a,l,j,i)       ! 13624578 (+1.000)
!     & -z217(d,c,k,b,a,l,j,i)       ! 12634578 (-1.000)
!     & +z217(d,a,j,c,b,l,k,i)       ! 14723568 (+1.000)
!     & -z217(d,b,j,c,a,l,k,i)       ! 13724568 (-1.000)
!     & +z217(d,c,j,b,a,l,k,i)       ! 12734568 (+1.000)
!     & +z217(d,a,k,c,b,l,i,j)       ! 14623587 (+1.000)
!     & -z217(d,b,k,c,a,l,i,j)       ! 13624587 (-1.000)
!     & +z217(d,c,k,b,a,l,i,j)       ! 12634587 (+1.000)
!     & -z217(d,a,j,c,b,l,i,k)       ! 14723586 (-1.000)
!     & +z217(d,b,j,c,a,l,i,k)       ! 13724586 (+1.000)
!     & -z217(d,c,j,b,a,l,i,k)       ! 12734586 (-1.000)
!     & -z217(d,a,i,c,b,l,k,j)       ! 14823567 (-1.000)
!     & +z217(d,b,i,c,a,l,k,j)       ! 13824567 (+1.000)
!     & -z217(d,c,i,b,a,l,k,j)       ! 12834567 (-1.000)
!     & +z217(d,a,i,c,b,l,j,k)       ! 14823576 (+1.000)
!     & -z217(d,b,i,c,a,l,j,k)       ! 13824576 (-1.000)
!     & +z217(d,c,i,b,a,l,j,k)       ! 12834576 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z217)
       deallocate(x25)
c
c
       allocate(d1(n0+1:n2,n0+1:n1,n0+1:n2,n0+1:n1))
       call reorder4213(n0,n2,n0,n1,n0,n1,n0,n2,
     & n0,n2,n0,n1,n0,n2,n0,n1,s110,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s159(n2+1:n3,n0+1:n1,n0+1:n2,n0+1:n1))
       i1=k1*k2*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s159)
       deallocate(d1)
       deallocate(b2)
       deallocate(s110)
c
       call sum2134(n0,n1,n2,n3,n0,n2,n0,n1,x1,s159,-1.000)
       deallocate(s159)
c
       call sumx1243(n0,n3,n0,n1,n2,n3,n0,n2,n0,n1,x1,intm, 1.000)
c
!       allocate(f2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3a,f2)
!       allocate(z1(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k4
!       i2=k1*k1*k3*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x1,f2,z1)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x1(m,d,l,i)*t3a(c,b,a,m,k,j)           !mdlicbamkj      (-1.000)
     &     + x1(m,d,l,j)*t3a(c,b,a,m,k,i)           !mdljcbamki      (+1.000)
     &     - x1(m,d,l,k)*t3a(c,b,a,m,j,i)           !mdlkcbamji      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23467158(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z1,-1.000)
!       call sum23468157(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z1, 1.000)
!       call sum23478156(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z1,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z1(c,b,a,k,j,d,l,i)       ! 23467158 (-1.000)
!     & +z1(c,b,a,k,i,d,l,j)       ! 23468157 (+1.000)
!     & -z1(c,b,a,j,i,d,l,k)       ! 23478156 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z1)
       deallocate(x1)
c
       allocate(d1(n0+1:n1,n2+1:n3,n0+1:n1,n0+1:n2))
       call reorder3214(n0,n1,n2,n3,n0,n1,n0,n2,
     & n0,n1,n2,n3,n0,n1,n0,n2,s100,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s101(n1+1:n3,n2+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s101)
       deallocate(d1)
       deallocate(b2)
       deallocate(s100)
c
       call sum3241(n0,n2,n2,n3,n1,n3,n0,n1,x20,s101,-1.000)
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n1,n3,n0,n1,x20,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n2,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t4c,h2)
!       allocate(z20(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3
!       i2=k1*k1*k2*k3*k3*k4
!       i3=k4*k2
!       call egemm(i1,i2,i3,x20,h2,z20)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x20(m,e,c,i)*t4c(e,d,b,a,m,l,k,j)      !meciedbamlkj    (+1.000)
     &     - x20(m,e,b,i)*t4c(e,d,c,a,m,l,k,j)      !mebiedcamlkj    (-1.000)
     &     + x20(m,e,a,i)*t4c(e,d,c,b,m,l,k,j)      !meaiedcbmlkj    (+1.000)
     &     - x20(m,e,c,j)*t4c(e,d,b,a,m,l,k,i)      !mecjedbamlki    (-1.000)
     &     + x20(m,e,b,j)*t4c(e,d,c,a,m,l,k,i)      !mebjedcamlki    (+1.000)
     &     - x20(m,e,a,j)*t4c(e,d,c,b,m,l,k,i)      !meajedcbmlki    (-1.000)
     &     + x20(m,e,c,k)*t4c(e,d,b,a,m,l,j,i)      !meckedbamlji    (+1.000)
     &     - x20(m,e,b,k)*t4c(e,d,c,a,m,l,j,i)      !mebkedcamlji    (-1.000)
     &     + x20(m,e,a,k)*t4c(e,d,c,b,m,l,j,i)      !meakedcbmlji    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13456728(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20, 1.000)
!       call sum12456738(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20,-1.000)
!       call sum12356748(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20, 1.000)
!       call sum13456827(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20,-1.000)
!       call sum12456837(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20, 1.000)
!       call sum12356847(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20,-1.000)
!       call sum13457826(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20, 1.000)
!       call sum12457836(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20,-1.000)
!       call sum12357846(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z20, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z20(d,b,a,l,k,j,c,i)       ! 13456728 (+1.000)
!     & -z20(d,c,a,l,k,j,b,i)       ! 12456738 (-1.000)
!     & +z20(d,c,b,l,k,j,a,i)       ! 12356748 (+1.000)
!     & -z20(d,b,a,l,k,i,c,j)       ! 13456827 (-1.000)
!     & +z20(d,c,a,l,k,i,b,j)       ! 12456837 (+1.000)
!     & -z20(d,c,b,l,k,i,a,j)       ! 12356847 (-1.000)
!     & +z20(d,b,a,l,j,i,c,k)       ! 13457826 (+1.000)
!     & -z20(d,c,a,l,j,i,b,k)       ! 12457836 (-1.000)
!     & +z20(d,c,b,l,j,i,a,k)       ! 12357846 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z20)
       deallocate(x20)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder4213(n1,n3,n2,n3,n0,n1,n0,n2,
     & n0,n2,n2,n3,n1,n3,n0,n1,s101,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s158(n2+1:n3,n2+1:n3,n1+1:n3,n0+1:n1))
       i1=k1*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s158)
       deallocate(d1)
       deallocate(b2)
c
       call sum2134(n2,n3,n2,n3,n1,n3,n0,n1,x6,s158, 1.000)
       deallocate(s158)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder2413(n1,n3,n2,n3,n0,n1,n0,n2,
     & n2,n3,n0,n2,n1,n3,n0,n1,s101,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s157(n0+1:n2,n0+1:n2,n1+1:n3,n0+1:n1))
       i1=k1*k3*k2
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s157)
       deallocate(d1)
       deallocate(b2)
       deallocate(s101)
c
       call sum3124(n0,n2,n1,n3,n0,n2,n0,n1,x5,s157,-1.000)
       deallocate(s157)
c
       call sumx2143(n0,n3,n0,n2,n1,n3,n0,n2,n0,n1,x5,intm, 1.000)
c
!       allocate(f2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
!       call reorder412356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n2,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
!       allocate(z5(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n1+1:n3,
!     & n0+1:n2,n0+1:n1))
!       i1=k1*k2*k3
!       i2=k1*k1*k3*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x5,f2,z5)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - x5(m,c,l,i)*t3b(d,b,a,m,k,j)           !mclidbamkj      (-1.000)
     &     + x5(m,b,l,i)*t3b(d,c,a,m,k,j)           !mblidcamkj      (+1.000)
     &     - x5(m,a,l,i)*t3b(d,c,b,m,k,j)           !malidcbmkj      (-1.000)
     &     + x5(m,c,l,j)*t3b(d,b,a,m,k,i)           !mcljdbamki      (+1.000)
     &     - x5(m,b,l,j)*t3b(d,c,a,m,k,i)           !mbljdcamki      (-1.000)
     &     + x5(m,a,l,j)*t3b(d,c,b,m,k,i)           !maljdcbmki      (+1.000)
     &     - x5(m,c,l,k)*t3b(d,b,a,m,j,i)           !mclkdbamji      (-1.000)
     &     + x5(m,b,l,k)*t3b(d,c,a,m,j,i)           !mblkdcamji      (+1.000)
     &     - x5(m,a,l,k)*t3b(d,c,b,m,j,i)           !malkdcbmji      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13467258(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5,-1.000)
!       call sum12467358(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5, 1.000)
!       call sum12367458(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5,-1.000)
!       call sum13468257(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5, 1.000)
!       call sum12468357(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5,-1.000)
!       call sum12368457(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5, 1.000)
!       call sum13478256(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5,-1.000)
!       call sum12478356(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5, 1.000)
!       call sum12378456(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z5,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z5(d,b,a,k,j,c,l,i)       ! 13467258 (-1.000)
!     & +z5(d,c,a,k,j,b,l,i)       ! 12467358 (+1.000)
!     & -z5(d,c,b,k,j,a,l,i)       ! 12367458 (-1.000)
!     & +z5(d,b,a,k,i,c,l,j)       ! 13468257 (+1.000)
!     & -z5(d,c,a,k,i,b,l,j)       ! 12468357 (-1.000)
!     & +z5(d,c,b,k,i,a,l,j)       ! 12368457 (+1.000)
!     & -z5(d,b,a,j,i,c,l,k)       ! 13478256 (-1.000)
!     & +z5(d,c,a,j,i,b,l,k)       ! 12478356 (+1.000)
!     & -z5(d,c,b,j,i,a,l,k)       ! 12378456 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z5)
       deallocate(x5)
c
       allocate(d1(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n2))
       call reorder4312(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n1,n3,n0,n1,n0,n2,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s112(n0+1:n2,n1+1:n3,n0+1:n1,n0+1:n2))
       i1=k2*k1*k3
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s112)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n2))
       call reorder2431(n0,n2,n1,n3,n0,n1,n0,n2,
     & n1,n3,n0,n2,n0,n1,n0,n2,s112,d1)
       allocate(f2(n1+1:n3,n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1))
       call reorder241356(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
     & n1,n3,n0,n2,n2,n3,n1,n3,n0,n1,n0,n1,t3b,f2)
       allocate(u91(n2+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n0+1:n2))
       i1=k2*k1
       i2=k1*k1*k3*k4
       i3=k2*k3
       call egemm(i1,i2,i3,d1,f2,u91)
       deallocate(d1)
       deallocate(f2)
c
       call sum235614(n0,n1,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,x37,u91,1.000)
       deallocate(u91)
c
!       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
!       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
!       allocate(z88(n1+1:n3,n1+1:n3,n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2,
!     & n0+1:n1,n0+1:n1))
!       i1=k1*k1*k2*k3*k4
!       i2=k1*k3*k3
!       i3=k1
!       call egemm(i1,i2,i3,x37,d2,z88)
!       deallocate(d2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n1
             sum=sum
     &     - x37(m,d,a,l,k,j)*t2a(c,b,m,i)          !mdalkjcbmi      (-1.000)
     &     + x37(m,d,b,l,k,j)*t2a(c,a,m,i)          !mdblkjcami      (+1.000)
     &     - x37(m,d,c,l,k,j)*t2a(b,a,m,i)          !mdclkjbami      (-1.000)
     &     + x37(m,d,a,l,k,i)*t2a(c,b,m,j)          !mdalkicbmj      (+1.000)
     &     - x37(m,d,b,l,k,i)*t2a(c,a,m,j)          !mdblkicamj      (-1.000)
     &     + x37(m,d,c,l,k,i)*t2a(b,a,m,j)          !mdclkibamj      (+1.000)
     &     - x37(m,d,a,l,j,i)*t2a(c,b,m,k)          !mdaljicbmk      (-1.000)
     &     + x37(m,d,b,l,j,i)*t2a(c,a,m,k)          !mdbljicamk      (+1.000)
     &     - x37(m,d,c,l,j,i)*t2a(b,a,m,k)          !mdcljibamk      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23814567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88,-1.000)
!       call sum24813567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88, 1.000)
!       call sum34812567(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88,-1.000)
!       call sum23714568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88, 1.000)
!       call sum24713568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88,-1.000)
!       call sum34712568(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88, 1.000)
!       call sum23614578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88,-1.000)
!       call sum24613578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88, 1.000)
!       call sum34612578(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z88,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z88(c,b,i,d,a,l,k,j)       ! 23814567 (-1.000)
!     & +z88(c,a,i,d,b,l,k,j)       ! 24813567 (+1.000)
!     & -z88(b,a,i,d,c,l,k,j)       ! 34812567 (-1.000)
!     & +z88(c,b,j,d,a,l,k,i)       ! 23714568 (+1.000)
!     & -z88(c,a,j,d,b,l,k,i)       ! 24713568 (-1.000)
!     & +z88(b,a,j,d,c,l,k,i)       ! 34712568 (+1.000)
!     & -z88(c,b,k,d,a,l,j,i)       ! 23614578 (-1.000)
!     & +z88(c,a,k,d,b,l,j,i)       ! 24613578 (+1.000)
!     & -z88(b,a,k,d,c,l,j,i)       ! 34612578 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z88)
       deallocate(x37)
c
       allocate(d1(n0+1:n2,n0+1:n1,n1+1:n3,n0+1:n2))
       call reorder4321(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n2,n0,n1,n1,n3,n0,n2,s112,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s138(n2+1:n3,n0+1:n1,n1+1:n3,n0+1:n2))
       i1=k2*k3*k1
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s138)
       deallocate(d1)
       deallocate(b2)
c
       call sum3124(n0,n1,n1,n3,n2,n3,n0,n2,x7,s138,-1.000)
       deallocate(s138)
c
       call sumx1324(n0,n3,n0,n1,n1,n3,n2,n3,n0,n2,x7,intm, 1.000)
c
!       allocate(h2(n0+1:n1,n1+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n0,n1,n0,n1,n1,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4a,h2)
!       allocate(z7(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4
!       i2=k1*k1*k1*k3*k3*k3
!       i3=k3*k1
!       call egemm(i1,i2,i3,x7,h2,z7)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n1
             sum=sum
     &     + x7(m,e,d,l)*t4a(e,c,b,a,m,k,j,i)       !medlecbamkji    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23467815(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z7, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z7(c,b,a,k,j,i,d,l)       ! 23467815 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z7)
       deallocate(x7)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n0,n2,n0,n2,s112,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(q24(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k3*k1
       call egemm1(i1,i3,d1,b2,q24)
       deallocate(d1)
       deallocate(b2)
c
       call sum21(n0,n2,n0,n2,x10,q24, 1.000)
       deallocate(q24)
c
       allocate(d1(n0+1:n1,n1+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n1,n3,n0,n1,n0,n2,
     & n0,n1,n1,n3,n0,n2,n0,n2,s112,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s113(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k3
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s113)
       deallocate(d1)
       deallocate(b2)
       deallocate(s112)
c
       call sum3241(n0,n2,n1,n3,n1,n3,n0,n2,x17,s113,-1.000)
c
       call sumx3124(n0,n3,n0,n2,n1,n3,n1,n3,n0,n2,x17,intm, 1.000)
c
!       allocate(h2(n0+1:n2,n1+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder52134678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n2,n1,n3,n2,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z17(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3
!       i2=k1*k1*k1*k3*k3*k4
!       i3=k3*k2
!       call egemm(i1,i2,i3,x17,h2,z17)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do m=n0+1,n2
             sum=sum
     &     - x17(m,e,c,l)*t4b(d,e,b,a,m,k,j,i)      !mecldebamkji    (-1.000)
     &     + x17(m,e,b,l)*t4b(d,e,c,a,m,k,j,i)      !mebldecamkji    (+1.000)
     &     - x17(m,e,a,l)*t4b(d,e,c,b,m,k,j,i)      !mealdecbmkji    (-1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13467825(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z17,-1.000)
!       call sum12467835(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z17, 1.000)
!       call sum12367845(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z17,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z17(d,b,a,k,j,i,c,l)       ! 13467825 (-1.000)
!     & +z17(d,c,a,k,j,i,b,l)       ! 12467835 (+1.000)
!     & -z17(d,c,b,k,j,i,a,l)       ! 12367845 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z17)
       deallocate(x17)
c
       allocate(d1(n0+1:n2,n1+1:n3,n1+1:n3,n0+1:n2))
       call reorder4213(n1,n3,n1,n3,n0,n2,n0,n2,
     & n0,n2,n1,n3,n1,n3,n0,n2,s113,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s160(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k3
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s160)
       deallocate(d1)
       deallocate(b2)
       deallocate(s113)
c
       call sum2134(n1,n3,n2,n3,n1,n3,n0,n2,x2,s160, 1.000)
       deallocate(s160)
c
       allocate(d1(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder1432(n0,n3,n0,n3,n0,n3,n0,n3,
     & n0,n1,n2,n3,n1,n3,n0,n2,intm,d1)
       allocate(b2(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,t1a,b2)
       allocate(s114(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2))
       i1=k2*k3*k4
       i2=k3
       i3=k1
       call egemm(i1,i2,i3,d1,b2,s114)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n1+1:n3))
       call reorder2431(n1,n3,n2,n3,n1,n3,n0,n2,
     & n2,n3,n0,n2,n1,n3,n1,n3,s114,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q27(n1+1:n3,n1+1:n3))
       i1=k3*k3
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q27)
       deallocate(d1)
       deallocate(b2)
c
       x9=x9-q27
       deallocate(q27)
c
       call sumx21(0,n3,n1,n3,n1,n3,x9,fockr, 1.000)
c
!       allocate(h2(n1+1:n3,n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder21345678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n1,n3,n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z9(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n0+1:n1,n1+1:n3))
!       i1=k3
!       i2=k1*k1*k1*k2*k3*k3*k4
!       i3=k3
!       call egemm(i1,i2,i3,x9,h2,z9)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x9(e,c)*t4b(d,e,b,a,l,k,j,i)           !ecdebalkji      (+1.000)
     &     - x9(e,b)*t4b(d,e,c,a,l,k,j,i)           !ebdecalkji      (-1.000)
     &     + x9(e,a)*t4b(d,e,c,b,l,k,j,i)           !eadecblkji      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum13456782(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z9, 1.000)
!       call sum12456783(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z9,-1.000)
!       call sum12356784(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z9, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z9(d,b,a,l,k,j,i,c)       ! 13456782 (+1.000)
!     & -z9(d,c,a,l,k,j,i,b)       ! 12456783 (-1.000)
!     & +z9(d,c,b,l,k,j,i,a)       ! 12356784 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z9)
       deallocate(x9)
c
       allocate(d1(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3))
       call reorder4231(n1,n3,n2,n3,n1,n3,n0,n2,
     & n0,n2,n2,n3,n1,n3,n1,n3,s114,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s115(n2+1:n3,n2+1:n3,n1+1:n3,n1+1:n3))
       i1=k3*k3*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s115)
       deallocate(d1)
       deallocate(b2)
       deallocate(s114)
c
       call sum3124(n2,n3,n1,n3,n2,n3,n1,n3,x18,s115, 1.000)
       deallocate(s115)
c
       call sumx4321(n0,n3,n2,n3,n1,n3,n2,n3,n1,n3,x18,intm, 1.000)
c
!       allocate(h2(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder12345678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z18(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n1+1:n3))
!       i1=k3*k4
!       i2=k1*k1*k1*k2*k3*k3
!       i3=k3*k4
!       call egemm(i1,i2,i3,x18,h2,z18)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3;do f=n2+1,n3
             sum=sum
     &     + x18(f,e,d,c)*t4b(f,e,b,a,l,k,j,i)      !fedcfebalkji    (+1.000)
     &     - x18(f,e,d,b)*t4b(f,e,c,a,l,k,j,i)      !fedbfecalkji    (-1.000)
     &     + x18(f,e,d,a)*t4b(f,e,c,b,l,k,j,i)      !fedafecblkji    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34567812(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z18, 1.000)
!       call sum24567813(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z18,-1.000)
!       call sum23567814(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z18, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z18(b,a,l,k,j,i,d,c)       ! 34567812 (+1.000)
!     & -z18(c,a,l,k,j,i,d,b)       ! 24567813 (-1.000)
!     & +z18(c,b,l,k,j,i,d,a)       ! 23567814 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z18)
       deallocate(x18)
c
       allocate(d1(n1+1:n3,n0+1:n1,n2+1:n3,n0+1:n2))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n2,n3,n0,n2,intm,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q25(n2+1:n3,n0+1:n2))
       i1=k2*k4
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q25)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q25,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s132(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s132)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s132,-1.000)
       deallocate(s132)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q25,b1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q26(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,b1,b2,q26)
       deallocate(b1)
       deallocate(b2)
       deallocate(q25)
c
       call sum21(n2,n3,n2,n3,x11,q26,-1.000)
       deallocate(q26)
c
       allocate(d1(n1+1:n3,n0+1:n1,n1+1:n3,n0+1:n1))
       call reorder3142(n0,n3,n0,n3,n0,n3,n0,n3,
     & n1,n3,n0,n1,n1,n3,n0,n1,intr,d1)
       allocate(b2(n1+1:n3,n0+1:n1))
       call reorder12(n1,n3,n0,n1,
     & n1,n3,n0,n1,t1a,b2)
       allocate(q28(n1+1:n3,n0+1:n1))
       i1=k1*k3
       i3=k1*k3
       call egemm1(i1,i3,d1,b2,q28)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q28,b1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s125(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s125)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s125,-1.000)
       deallocate(s125)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q28,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s121(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s121)
       deallocate(b1)
       deallocate(d2)
       deallocate(q28)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x4,s121,-1.000)
       deallocate(s121)
c
       allocate(d1(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3412(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n2,n3,n0,n2,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(s139(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k2
       i3=k4
       call egemm(i1,i2,i3,d1,b2,s139)
       deallocate(d1)
       deallocate(b2)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n0,n2,n0,n2,s139,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q29(n0+1:n2,n0+1:n2))
       i1=k2*k2
       i3=k4*k2
       call egemm1(i1,i3,d1,b2,q29)
       deallocate(d1)
       deallocate(b2)
c
       call sum21(n0,n2,n0,n2,x10,q29,-1.000)
       deallocate(q29)
c
       call sumx12(0,n3,n0,n2,n0,n2,x10,fockb, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z10(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,
!     & n0+1:n1,n0+1:n2))
!       i1=k2
!       i2=k1*k1*k1*k3*k3*k3*k4
!       i3=k2
!       call egemm(i1,i2,i3,x10,h2,z10)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do m=n0+1,n2
             sum=sum
     &     - x10(m,l)*t4b(d,c,b,a,m,k,j,i)          !mldcbamkji      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum12346785(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z10,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z10(d,c,b,a,k,j,i,l)       ! 12346785 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z10)
       deallocate(x10)
c
       allocate(d1(n0+1:n2,n2+1:n3,n0+1:n2,n0+1:n2))
       call reorder3214(n0,n2,n2,n3,n0,n2,n0,n2,
     & n0,n2,n2,n3,n0,n2,n0,n2,s139,d1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(s140(n2+1:n3,n2+1:n3,n0+1:n2,n0+1:n2))
       i1=k2*k2*k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,d1,b2,s140)
       deallocate(d1)
       deallocate(b2)
       deallocate(s139)
c
       call sum3241(n0,n2,n2,n3,n2,n3,n0,n2,x19,s140,-1.000)
       deallocate(s140)
c
       call sumx3142(n0,n3,n0,n2,n2,n3,n2,n3,n0,n2,x19,intb, 1.000)
c
!       allocate(h2(n0+1:n2,n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder51234678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n0,n2,n2,n3,n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z19(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,
!     & n2+1:n3,n0+1:n2))
!       i1=k2*k4
!       i2=k1*k1*k1*k3*k3*k3
!       i3=k4*k2
!       call egemm(i1,i2,i3,x19,h2,z19)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3;do m=n0+1,n2
             sum=sum
     &     + x19(m,e,d,l)*t4b(e,c,b,a,m,k,j,i)      !medlecbamkji    (+1.000)
             enddo;enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23467815(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z19, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z19(c,b,a,k,j,i,d,l)       ! 23467815 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z19)
       deallocate(x19)
c
       allocate(d1(n2+1:n3,n0+1:n2,n2+1:n3,n0+1:n2))
       call reorder3241(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n2,n3,n0,n2,intb,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q30(n2+1:n3,n0+1:n2))
       i1=k2*k4
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q30)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q30,b1)
       allocate(d2(n0+1:n2,n2+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n2,n2,n3,n1,n3,n0,n1,t2b,d2)
       allocate(s154(n2+1:n3,n1+1:n3,n0+1:n1,n2+1:n3))
       i1=k4
       i2=k1*k3*k4
       i3=k2
       call egemm(i1,i2,i3,b1,d2,s154)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n2,n3,n2,n3,n1,n3,n0,n1,x6,s154, 1.000)
       deallocate(s154)
c
       call sumx3241(n0,n3,n2,n3,n2,n3,n1,n3,n0,n1,x6,intm, 1.000)
c
!       allocate(f2(n2+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder123456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z6(n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k4
!       i2=k1*k1*k2*k3*k3
!       i3=k4
!       call egemm(i1,i2,i3,x6,f2,z6)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + x6(e,d,c,i)*t3b(e,b,a,l,k,j)           !edciebalkj      (+1.000)
     &     - x6(e,d,b,i)*t3b(e,c,a,l,k,j)           !edbiecalkj      (-1.000)
     &     + x6(e,d,a,i)*t3b(e,c,b,l,k,j)           !edaiecblkj      (+1.000)
     &     - x6(e,d,c,j)*t3b(e,b,a,l,k,i)           !edcjebalki      (-1.000)
     &     + x6(e,d,b,j)*t3b(e,c,a,l,k,i)           !edbjecalki      (+1.000)
     &     - x6(e,d,a,j)*t3b(e,c,b,l,k,i)           !edajecblki      (-1.000)
     &     + x6(e,d,c,k)*t3b(e,b,a,l,j,i)           !edckebalji      (+1.000)
     &     - x6(e,d,b,k)*t3b(e,c,a,l,j,i)           !edbkecalji      (-1.000)
     &     + x6(e,d,a,k)*t3b(e,c,b,l,j,i)           !edakecblji      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34567128(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6, 1.000)
!       call sum24567138(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6,-1.000)
!       call sum23567148(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6, 1.000)
!       call sum34568127(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6,-1.000)
!       call sum24568137(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6, 1.000)
!       call sum23568147(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6,-1.000)
!       call sum34578126(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6, 1.000)
!       call sum24578136(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6,-1.000)
!       call sum23578146(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z6, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z6(b,a,l,k,j,d,c,i)       ! 34567128 (+1.000)
!     & -z6(c,a,l,k,j,d,b,i)       ! 24567138 (-1.000)
!     & +z6(c,b,l,k,j,d,a,i)       ! 23567148 (+1.000)
!     & -z6(b,a,l,k,i,d,c,j)       ! 34568127 (-1.000)
!     & +z6(c,a,l,k,i,d,b,j)       ! 24568137 (+1.000)
!     & -z6(c,b,l,k,i,d,a,j)       ! 23568147 (-1.000)
!     & +z6(b,a,l,j,i,d,c,k)       ! 34578126 (+1.000)
!     & -z6(c,a,l,j,i,d,b,k)       ! 24578136 (-1.000)
!     & +z6(c,b,l,j,i,d,a,k)       ! 23578146 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z6)
       deallocate(x6)
c
       allocate(b1(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,q30,b1)
       allocate(b2(n0+1:n2,n2+1:n3))
       call reorder21(n2,n3,n0,n2,
     & n0,n2,n2,n3,t1b,b2)
       allocate(q31(n2+1:n3,n2+1:n3))
       i1=k4
       i2=k4
       i3=k2
       call egemm(i1,i2,i3,b1,b2,q31)
       deallocate(b1)
       deallocate(b2)
       deallocate(q30)
c
       call sum21(n2,n3,n2,n3,x11,q31, 1.000)
       deallocate(q31)
c
       call sumx21(0,n3,n2,n3,n2,n3,x11,fockb, 1.000)
c
!       allocate(h2(n2+1:n3,n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,
!     & n0+1:n1,n0+1:n1))
!       call reorder12345678(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n0,n1,n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,t4b,h2)
!       allocate(z11(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,
!     & n0+1:n1,n2+1:n3))
!       i1=k4
!       i2=k1*k1*k1*k2*k3*k3*k3
!       i3=k4
!       call egemm(i1,i2,i3,x11,h2,z11)
!       deallocate(h2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n2+1,n3
             sum=sum
     &     + x11(e,d)*t4b(e,c,b,a,l,k,j,i)          !edecbalkji      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum23456781(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z11, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z11(c,b,a,l,k,j,i,d)       ! 23456781 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z11)
       deallocate(x11)
c
       allocate(d1(n2+1:n3,n0+1:n2,n1+1:n3,n0+1:n1))
       call reorder4231(n0,n3,n0,n3,n0,n3,n0,n3,
     & n2,n3,n0,n2,n1,n3,n0,n1,intm,d1)
       allocate(b2(n2+1:n3,n0+1:n2))
       call reorder12(n2,n3,n0,n2,
     & n2,n3,n0,n2,t1b,b2)
       allocate(q32(n1+1:n3,n0+1:n1))
       i1=k1*k3
       i3=k2*k4
       call egemm1(i1,i3,d1,b2,q32)
       deallocate(d1)
       deallocate(b2)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q32,b1)
       allocate(d2(n0+1:n1,n2+1:n3,n1+1:n3,n0+1:n2))
       call reorder4123(n2,n3,n1,n3,n0,n2,n0,n1,
     & n0,n1,n2,n3,n1,n3,n0,n2,t2b,d2)
       allocate(s150(n2+1:n3,n1+1:n3,n0+1:n2,n1+1:n3))
       i1=k3
       i2=k2*k3*k4
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s150)
       deallocate(b1)
       deallocate(d2)
c
       call sum2341(n1,n3,n2,n3,n1,n3,n0,n2,x2,s150,-1.000)
       deallocate(s150)
c
       call sumx3214(n0,n3,n1,n3,n2,n3,n1,n3,n0,n2,x2,intm, 1.000)
c
!       allocate(f2(n1+1:n3,n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1))
!       call reorder123456(n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,
!     & n1,n3,n1,n3,n1,n3,n0,n1,n0,n1,n0,n1,t3a,f2)
!       allocate(z2(n1+1:n3,n1+1:n3,n0+1:n1,n0+1:n1,n0+1:n1,n2+1:n3,
!     & n1+1:n3,n0+1:n2))
!       i1=k2*k3*k4
!       i2=k1*k1*k1*k3*k3
!       i3=k3
!       call egemm(i1,i2,i3,x2,f2,z2)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     + x2(e,d,c,l)*t3a(e,b,a,k,j,i)           !edclebakji      (+1.000)
     &     - x2(e,d,b,l)*t3a(e,c,a,k,j,i)           !edblecakji      (-1.000)
     &     + x2(e,d,a,l)*t3a(e,c,b,k,j,i)           !edalecbkji      (+1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum34678125(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z2, 1.000)
!       call sum24678135(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z2,-1.000)
!       call sum23678145(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z2, 1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & +z2(b,a,k,j,i,d,c,l)       ! 34678125 (+1.000)
!     & -z2(c,a,k,j,i,d,b,l)       ! 24678135 (-1.000)
!     & +z2(c,b,k,j,i,d,a,l)       ! 23678145 (+1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z2)
       deallocate(x2)
c
       allocate(b1(n0+1:n1,n1+1:n3))
       call reorder21(n1,n3,n0,n1,
     & n0,n1,n1,n3,q32,b1)
       allocate(d2(n0+1:n1,n1+1:n3,n1+1:n3,n0+1:n1))
       call reorder3124(n1,n3,n1,n3,n0,n1,n0,n1,
     & n0,n1,n1,n3,n1,n3,n0,n1,t2a,d2)
       allocate(s144(n1+1:n3,n1+1:n3,n0+1:n1,n1+1:n3))
       i1=k3
       i2=k1*k3*k3
       i3=k1
       call egemm(i1,i2,i3,b1,d2,s144)
       deallocate(b1)
       deallocate(d2)
       deallocate(q32)
c
       call sum2341(n1,n3,n1,n3,n1,n3,n0,n1,x4,s144,-1.000)
       deallocate(s144)
c
       call sumx3241(n0,n3,n1,n3,n1,n3,n1,n3,n0,n1,x4,intr, 1.000)
c
!       allocate(f2(n1+1:n3,n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1))
!       call reorder213456(n2,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,
!     & n1,n3,n2,n3,n1,n3,n0,n2,n0,n1,n0,n1,t3b,f2)
!       allocate(z4(n2+1:n3,n1+1:n3,n0+1:n2,n0+1:n1,n0+1:n1,n1+1:n3,
!     & n1+1:n3,n0+1:n1))
!       i1=k1*k3*k3
!       i2=k1*k1*k2*k3*k4
!       i3=k3
!       call egemm(i1,i2,i3,x4,f2,z4)
!       deallocate(f2)
c
       do i=n0+1,n1-2;do j=i+1,n1-1;do k=j+1,n1;do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
          do a=n1+1,n3-2;do b=a+1,n3-1;do c=b+1,n3;do d=n2+1,n3
           if(indunocc(d,c,b,a).eq.1)cycle
             sum=0.0d0
             do e=n1+1,n3
             sum=sum
     &     - x4(e,c,b,i)*t3b(d,e,a,l,k,j)           !ecbidealkj      (-1.000)
     &     + x4(e,c,a,i)*t3b(d,e,b,l,k,j)           !ecaideblkj      (+1.000)
     &     - x4(e,b,a,i)*t3b(d,e,c,l,k,j)           !ebaideclkj      (-1.000)
     &     + x4(e,c,b,j)*t3b(d,e,a,l,k,i)           !ecbjdealki      (+1.000)
     &     - x4(e,c,a,j)*t3b(d,e,b,l,k,i)           !ecajdeblki      (-1.000)
     &     + x4(e,b,a,j)*t3b(d,e,c,l,k,i)           !ebajdeclki      (+1.000)
     &     - x4(e,c,b,k)*t3b(d,e,a,l,j,i)           !ecbkdealji      (-1.000)
     &     + x4(e,c,a,k)*t3b(d,e,b,l,j,i)           !ecakdeblji      (+1.000)
     &     - x4(e,b,a,k)*t3b(d,e,c,l,j,i)           !ebakdeclji      (-1.000)
             enddo
             v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)+sum
          enddo;enddo;enddo;enddo
       enddo;enddo;enddo;enddo
c
!       call sum14567238(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4,-1.000)
!       call sum13567248(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4, 1.000)
!       call sum12567348(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4,-1.000)
!       call sum14568237(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4, 1.000)
!       call sum13568247(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4,-1.000)
!       call sum12568347(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4, 1.000)
!       call sum14578236(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4,-1.000)
!       call sum13578246(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4, 1.000)
!       call sum12578346(n2,n3,n1,n3,n1,n3,n1,n3,n0,n2,n0,n1,n0,n1,n0,n1,v4b,z4,-1.000)
c
!       do i=n0+1,n1-2
!       do j=i+1,n1-1
!       do k=j+1,n1
!       do l=n0+1,n2
!       do a=n1+1,n3-2
!       do b=a+1,n3-1
!       do c=b+1,n3
!       do d=n2+1,n3
!c
!       v4b(d,c,b,a,l,k,j,i)=v4b(d,c,b,a,l,k,j,i)
!     & -z4(d,a,l,k,j,c,b,i)       ! 14567238 (-1.000)
!     & +z4(d,b,l,k,j,c,a,i)       ! 13567248 (+1.000)
!     & -z4(d,c,l,k,j,b,a,i)       ! 12567348 (-1.000)
!     & +z4(d,a,l,k,i,c,b,j)       ! 14568237 (+1.000)
!     & -z4(d,b,l,k,i,c,a,j)       ! 13568247 (-1.000)
!     & +z4(d,c,l,k,i,b,a,j)       ! 12568347 (+1.000)
!     & -z4(d,a,l,j,i,c,b,k)       ! 14578236 (-1.000)
!     & +z4(d,b,l,j,i,c,a,k)       ! 13578246 (+1.000)
!     & -z4(d,c,l,j,i,b,a,k)       ! 12578346 (-1.000)
!c
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       enddo
!       deallocate(z4)
       deallocate(x4)
c
c
       do i=n0+1,n1-2
       do j=i+1,n1-1
       do k=j+1,n1
       do l=n0+1,n2
        if(indocc(l,k,j,i).eq.1)cycle
       do a=n1+1,n3-2
       do b=a+1,n3-1
       do c=b+1,n3
       do d=n2+1,n3
          if(indunocc(d,c,b,a).eq.1)cycle
!
!        iocca=0
!        ioccb=0
!        iunoa=0
!        iunob=0
!        if(i.gt.(n1-iactocca))iocca=iocca+1
!        if(j.gt.(n1-iactocca))iocca=iocca+1
!        if(k.gt.(n1-iactocca))iocca=iocca+1
!        if(l.gt.(n2-iactoccb))ioccb=ioccb+1
!        if(iocca+ioccb.lt.iactindq)cycle
!        if(a.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(b.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(c.lt.(n1+iactunoa+1))iunoa=iunoa+1
!        if(d.lt.(n2+iactunob+1))iunob=iunob+1
!        if(iunoa+iunob.lt.iactindq)cycle
!
         coeleft=fockb(d,d)
     &          +fockr(c,c)
     &          +fockr(b,b)
     &          +fockr(a,a)
     &          -fockb(l,l)
     &          -fockr(k,k)
     &          -fockr(j,j)
     &          -fockr(i,i)
     &          +shift
         t4b(d,c,b,a,l,k,j,i)=
     &   t4b(d,c,b,a,l,k,j,i)-v4b(d,c,b,a,l,k,j,i)/coeleft
         t4b(d,c,b,a,l,k,i,j)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,b,a,l,i,j,k)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,b,a,l,i,k,j)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,b,a,l,j,k,i)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,b,a,l,j,i,k)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,a,b,l,k,j,i)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,a,b,l,k,i,j)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,a,b,l,i,j,k)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,a,b,l,i,k,j)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,a,b,l,j,k,i)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,c,a,b,l,j,i,k)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,b,c,l,k,j,i)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,b,c,l,k,i,j)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,b,c,l,i,j,k)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,b,c,l,i,k,j)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,b,c,l,j,k,i)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,b,c,l,j,i,k)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,c,b,l,k,j,i)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,c,b,l,k,i,j)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,c,b,l,i,j,k)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,c,b,l,i,k,j)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,c,b,l,j,k,i)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,a,c,b,l,j,i,k)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,c,a,l,k,j,i)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,c,a,l,k,i,j)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,c,a,l,i,j,k)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,c,a,l,i,k,j)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,c,a,l,j,k,i)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,c,a,l,j,i,k)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,a,c,l,k,j,i)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,a,c,l,k,i,j)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,a,c,l,i,j,k)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,a,c,l,i,k,j)= t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,a,c,l,j,k,i)=-t4b(d,c,b,a,l,k,j,i)
         t4b(d,b,a,c,l,j,i,k)= t4b(d,c,b,a,l,k,j,i)
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       enddo
       rewind(tb)
       write(tb)t4b
c
       end
